library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(readr)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Please install.packages('jsonlite') for affiliation geocoding.", call. = FALSE)
}

# Cache file lives next to the app (same working directory used by Shiny / Connect).
GEOCODE_CACHE_FILE <- Sys.getenv(
  "SHINY_GEOCODE_CACHE",
  unset = file.path(getwd(), "affiliation_geocode_cache.rds")
)

# Seconds between Photon requests (fair use).
GEOCODE_THROTTLE_SEC <- max(
  0.2,
  as.numeric(Sys.getenv("SHINY_GEOCODE_THROTTLE", unset = "0.35"))
)
GEOCODE_THROTTLE_MS <- max(100L, ceiling(GEOCODE_THROTTLE_SEC * 1000))
# Optional: synchronous geocode-before-app (slow).
GEOCODE_BLOCKING_AT_START <- isTRUE(as.logical(Sys.getenv(
  "SHINY_GEOCODE_BLOCKING_START",
  unset = "FALSE"
)))

# Google Sheet: Taiwanese Statistician in the US/Canada
#
# Unauthenticated CSV: `/export?format=csv` often returns 400/HTML ("cannot open connection")
# unless the file is Published to web. `/gviz/tq?tqx=out:csv` usually works when
# sharing is **Anyone with the link → Viewer**.
#
# Document ID + gid from `...spreadsheets/d/<ID>/edit#gid=<gid>`.
# `SHINY_SHEET_CSV_URL` forces one exact URL.

SHEET_DOCUMENT_ID <- Sys.getenv(
  "SHINY_GOOGLE_SHEET_ID",
  unset = "1eU9ZiaqseSyhwOJuRVyyFVA_yQmtrhtfysUyijPky30"
)
SHEET_GID <- Sys.getenv("SHINY_GOOGLE_SHEET_GID", unset = "0")

sheet_urls_composed <- function(id, gid) {
  paste0(
    "https://docs.google.com/spreadsheets/d/",
    id,
    c(
      "/gviz/tq?tqx=out:csv&gid=", # preferred for anonymous / Shiny-server reads
      "/export?format=csv&gid="
    ),
    gid
  )
}

read_sheet_from_google_or_stop <- function(urls_to_try = character()) {
  errs <- character()
  for (u in urls_to_try) {
    res <- tryCatch(
      suppressWarnings(readr::read_csv(u, show_col_types = FALSE)),
      error = function(e) e
    )
    if (!inherits(res, "error")) {
      attr(res, "sheet_url_used") <- u
      return(res)
    }
    errs <- c(errs, paste0(trimws(as.character(u)), " → ", conditionMessage(res)))
  }
  hint <- paste0(
    "Could not download the Google Sheet as CSV:\n\n",
    paste(errs, collapse = "\n\n"),
    "\n\nWhat to fix:\n",
    "- Share settings: give **Anyone with the link → Viewer**, or Publish to Web.\n",
    "- Wrong tab? Set **SHINY_GOOGLE_SHEET_GID** from the `#gid=` in the sheet URL.\n",
    "- Or provide a working CSV URL in **SHINY_SHEET_CSV_URL** (e.g. Publish-to-web CSV).\n"
  )
  stop(hint, call. = FALSE)
}

sheet_csv_override <- trimws(Sys.getenv("SHINY_SHEET_CSV_URL", unset = ""))
if (nzchar(sheet_csv_override)) {
  sheet_candidates <- sheet_csv_override
} else {
  sheet_candidates <- sheet_urls_composed(SHEET_DOCUMENT_ID, SHEET_GID)
}

# Optional spreadsheet columns Latitude / Longitude override geocoded coordinates.

safe_chr <- function(df, candidates, n, default = "") {
  nms <- names(df)
  nm <- intersect(candidates, nms)[1]
  if (is.na(nm)) {
    nms_l <- tolower(nms)
    for (cand in candidates) {
      hit <- match(tolower(cand), nms_l, nomatch = 0L)
      if (hit != 0L) {
        nm <- nms[hit]
        break
      }
    }
  }
  if (is.na(nm)) return(rep(default, n))
  trimws(as.character(df[[nm]]))
}

read_geocode_cache <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(
      affiliation = character(),
      geocode_lat = double(),
      geocode_lon = double()
    ))
  }
  x <- readRDS(path)
  if (!tibble::is_tibble(x)) x <- tibble::as_tibble(x)
  x |>
    dplyr::group_by(.data$affiliation) |>
    dplyr::slice_tail(n = 1L) |>
    dplyr::ungroup()
}

write_geocode_cache <- function(x, path) {
  x <- x |>
    dplyr::filter(nzchar(as.character(.data$affiliation))) |>
    dplyr::group_by(.data$affiliation) |>
    dplyr::slice_tail(n = 1L) |>
    dplyr::ungroup()
  saveRDS(x, path)
}

# Bias Photon toward Canada vs United States based on cues in affiliation text.
affiliation_geocode_query <- function(affiliation) {
  a <- trimws(as.character(affiliation))
  if (!nzchar(a)) return("")
  canada_hint <- grepl(
    paste(
      "\\bCanada\\b", "\\bCanadian\\b", "McGill", "Toronto\\b",
      "Montreal\\b", "\\bMcMaster\\b", "Waterloo\\b", "Western University",
      "\\bUBC\\b", "Simon\\s+Fraser", "\\bSFU\\b", "British Columbia",
      "Dalhousie", "\\bQuebec\\b", "Ottawa\\b",
      "Concordia\\b",
      sep = "|"
    ),
    x = a,
    ignore.case = TRUE
  )
  if (grepl("\\bCanada\\b|\\bCanadian\\b", a, ignore.case = TRUE) || canada_hint) {
    paste0(a, ", Canada")
  } else {
    paste0(a, ", United States")
  }
}

geocode_photon <- function(query) {
  query <- trimws(as.character(query))
  if (!nzchar(query)) {
    return(c(latitude = NA_real_, longitude = NA_real_))
  }
  u <- paste0(
    "https://photon.komoot.io/api?q=",
    utils::URLencode(query, reserved = TRUE),
    "&limit=1&lang=en"
  )
  parsed <- tryCatch(
    jsonlite::fromJSON(u, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) {
    return(c(latitude = NA_real_, longitude = NA_real_))
  }
  feats <- parsed$features
  if (is.null(feats) || length(feats) == 0L) {
    return(c(latitude = NA_real_, longitude = NA_real_))
  }
  geom <- feats[[1L]]$geometry
  if (is.null(geom)) {
    return(c(latitude = NA_real_, longitude = NA_real_))
  }
  coo <- geom$coordinates
  if (is.null(coo) || length(coo) < 2L) {
    return(c(latitude = NA_real_, longitude = NA_real_))
  }
  if (is.numeric(coo) && length(coo) >= 2L) {
    lon <- suppressWarnings(as.numeric(coo[1L]))
    lat <- suppressWarnings(as.numeric(coo[2L]))
  } else {
    lon <- suppressWarnings(as.numeric(coo[[1L]]))
    lat <- suppressWarnings(as.numeric(coo[[2L]]))
  }
  if (is.na(lat) || is.na(lon)) {
    return(c(latitude = NA_real_, longitude = NA_real_))
  }
  c(latitude = lat, longitude = lon)
}

affiliation_known_in_cache <- function(cache, affiliation) {
  aff <- trimws(as.character(affiliation))
  if (!nzchar(aff)) return(TRUE)
  any(cache$affiliation == aff, na.rm = TRUE)
}

# Join spreadsheet rows with RDS cache only (instant; safe for startup).
join_faculty_with_geocode_cache <- function(fac_tbl, cache_path) {
  ft <- fac_tbl |>
    dplyr::rename(lat_sheet = latitude, lon_sheet = longitude)

  cache <- read_geocode_cache(cache_path)

  ft |>
    dplyr::left_join(
      dplyr::transmute(
        cache,
        affiliation = .data$affiliation,
        latitude_geo = .data$geocode_lat,
        longitude_geo = .data$geocode_lon
      ),
      by = "affiliation"
    ) |>
    dplyr::mutate(
      latitude = dplyr::coalesce(.data$lat_sheet, .data$latitude_geo),
      longitude = dplyr::coalesce(.data$lon_sheet, .data$longitude_geo)
    ) |>
    dplyr::select(-dplyr::any_of(c(
      "lat_sheet", "lon_sheet", "latitude_geo", "longitude_geo"
    )))
}

# Affiliation keys that still need a Photon lookup (not yet in RDS).
pending_geocode_affiliations <- function(fac_tbl, cache_path) {
  ft <- fac_tbl |>
    dplyr::rename(lat_sheet = latitude, lon_sheet = longitude)

  cache <- read_geocode_cache(cache_path)

  uniq_aff <- ft |>
    dplyr::filter(nzchar(trimws(as.character(.data$affiliation)))) |>
    dplyr::distinct(.data$affiliation) |>
    dplyr::pull(.data$affiliation)

  still_need_geo <- ft |>
    dplyr::group_by(.data$affiliation) |>
    dplyr::summarise(
      gaps = any(is.na(.data$lat_sheet) | is.na(.data$lon_sheet), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$gaps) |>
    dplyr::pull(.data$affiliation)

  pending <- intersect(uniq_aff, unique(still_need_geo))
  pending[!vapply(
    pending,
    function(a) affiliation_known_in_cache(cache, a),
    logical(1L)
  )]
}

# One Photon lookup + append to RDS.
geocode_and_cache_one_affiliation <- function(aff, cache_path = GEOCODE_CACHE_FILE) {
  cache <- read_geocode_cache(cache_path)
  cache <- dplyr::filter(cache, .data$affiliation != aff)

  q1 <- affiliation_geocode_query(aff)
  res <- geocode_photon(q1)
  if (any(is.na(res))) {
    res <- geocode_photon(paste0(aff, ", North America"))
  }
  if (any(is.na(res))) {
    res <- geocode_photon(paste0(aff, " university"))
  }

  cache <- dplyr::bind_rows(
    cache,
    tibble::tibble(
      affiliation = aff,
      geocode_lat = unname(res["latitude"]),
      geocode_lon = unname(res["longitude"])
    )
  )
  write_geocode_cache(cache, cache_path)
  invisible(TRUE)
}

# Optional: blocking full queue (slow) — CLI / env SHINY_GEOCODE_BLOCKING_START=TRUE only.
enrich_faculty_coordinates_blocking <- function(
    fac_tbl,
    cache_path = GEOCODE_CACHE_FILE,
    throttle = GEOCODE_THROTTLE_SEC
) {
  pend <- pending_geocode_affiliations(fac_tbl, cache_path)
  if (length(pend)) {
    message(
      paste0(
        "(Geocoder/blocking) resolving ",
        length(pend),
        " affiliation(s); cache: ",
        cache_path,
        "; throttle=",
        throttle,
        "s/request."
      )
    )
    for (aff in pend) {
      geocode_and_cache_one_affiliation(aff, cache_path)
      base::Sys.sleep(throttle)
    }
  }
  join_faculty_with_geocode_cache(fac_tbl, cache_path)
}

fr <- read_sheet_from_google_or_stop(sheet_candidates)
src_url <- attr(fr, "sheet_url_used", exact = FALSE)
if (
  interactive() &&
    is.character(src_url) && length(src_url) == 1L &&
    nzchar(trimws(as.character(src_url)[1L]))
) {
  message("Google Sheet CSV source: ", trimws(src_url[1L]))
}
# Strip BOM/spaces so real headers match ("Affiliation" vs BOM-prefixed or typo columns).
names(fr) <- trimws(sub("^\ufeff", "", as.character(names(fr)), useBytes = FALSE))
n_fr <- nrow(fr)

# Static copy from CSV (reuse when refreshing coords after cache updates).
FACULTY_SHEET <- tibble(
  first_name = safe_chr(fr, c("First name", "First_name", "FIRST NAME"), n_fr),
  last_name = safe_chr(fr, c("Last Name", "Last_name", "LAST NAME"), n_fr),
  email = safe_chr(fr, "Email", n_fr),
  # Common typo: "Affliation" (missing i) — spellings & case-insensitive match in safe_chr()
  affiliation = safe_chr(fr, c("Affiliation", "Affliation", "Institution"), n_fr),
  title = safe_chr(fr, "Title", n_fr),
  dept_school = safe_chr(fr, c("Department/School", "Department"), n_fr),
  google_scholar = safe_chr(fr, c("Google Scholar", "Google scholar"), n_fr),
  website = safe_chr(fr, c("Website", "Personal website"), n_fr),
  latitude = suppressWarnings(as.numeric(safe_chr(fr, c("Latitude", "latitude"), n_fr))),
  longitude = suppressWarnings(as.numeric(safe_chr(fr, c("Longitude", "longitude"), n_fr)))
) |>
  mutate(
    title = ifelse(title == "", "Other", trimws(title))
  )

GEO_QUEUE_START <- pending_geocode_affiliations(FACULTY_SHEET, GEOCODE_CACHE_FILE)
# Session queue (avoid re-running Photon after synchronous blocking warmup).
GEO_QUEUE_BACKGROUND <- GEO_QUEUE_START

faculty_tbl_initial <- join_faculty_with_geocode_cache(FACULTY_SHEET, GEOCODE_CACHE_FILE)

if (GEOCODE_BLOCKING_AT_START) {
  faculty_tbl_initial <- enrich_faculty_coordinates_blocking(
    FACULTY_SHEET,
    cache_path = GEOCODE_CACHE_FILE,
    throttle = GEOCODE_THROTTLE_SEC
  )
  GEO_QUEUE_BACKGROUND <- character()
}

if (length(GEO_QUEUE_BACKGROUND) && !GEOCODE_BLOCKING_AT_START) {
  message(
    paste0(
      "(Geocoder) queued ",
      length(GEO_QUEUE_BACKGROUND),
      " affiliation(s) — updating in background after UI loads (≈ ",
      GEOCODE_THROTTLE_SEC,
      "s each). Env SHINY_GEOCODE_BLOCKING_START=TRUE waits before app opens."
    )
  )
}

rm(fr, n_fr)

# Cluster options
cluster_opts <- markerClusterOptions(
  maxClusterRadius = 45,
  showCoverageOnHover = FALSE,
  zoomToBoundsOnClick = TRUE,
  spiderfyOnMaxZoom = TRUE
)

# Title → marker color/icon (aligned with spreadsheet Title dropdown categories)
title_marker_spec <- list(
  "Professor" = list(markerColor = "red", icon = "graduation-cap"),
  "Associate Professor" = list(markerColor = "blue", icon = "graduation-cap"),
  "Assistant Professor" = list(markerColor = "cadetblue", icon = "graduation-cap"),
  "Emeritus" = list(markerColor = "gray", icon = "black-tie"),
  "In Memory" = list(markerColor = "darkpurple", icon = "star"),
  "Other" = list(markerColor = "beige", icon = "user")
)

icon_for_title <- function(t) {
  spec <- title_marker_spec[[t]]
  if (is.null(spec)) spec <- title_marker_spec[["Other"]]
  makeAwesomeIcon(spec$icon, markerColor = spec$markerColor, library = "fa")
}

titles_for_filter <- FACULTY_SHEET |>
  pull(title) |>
  unique() |>
  sort()

`%||%` <- function(a, b) if (length(a) == 0L || (length(a) == 1L && is.na(a))) b else a

escape_txt <- function(x) {
  x <- as.character(x %||% "")
  htmltools::htmlEscape(x, attribute = FALSE)
}

escape_attr <- function(x) {
  x <- as.character(x %||% "")
  htmltools::htmlEscape(x, attribute = TRUE)
}

href_if <- function(url, label) {
  u <- trimws(as.character(url %||% ""))
  if (u == "") return("")
  safe <- ifelse(grepl("^https?://", u, ignore.case = TRUE), u, paste0("https://", u))
  paste0(
    '<a href="', escape_attr(safe), '" target="_blank" rel="noopener">',
    escape_txt(label), "</a>"
  )
}

# Plain text cells for DT (NA → "" so DataTables JSON never receives NA/HTML bugs).
dt_chr <- function(x) {
  ifelse(is.na(x), "", trimws(as.character(x)))
}

# DT link columns — never yields NA (ifelse(..., nzchar(NA)) was breaking the widget).
dt_link_anchor <- function(x, anchor_label) {
  raw <- dt_chr(x)
  has <- nzchar(raw)
  href <- ifelse(
    has,
    ifelse(grepl("^https?://", raw, ignore.case = TRUE), raw, paste0("https://", raw)),
    ""
  )
  ifelse(
    has & nzchar(href),
    paste0(
      '<a href="',
      htmltools::htmlEscape(href, attribute = TRUE),
      '" target="_blank" rel="noopener">',
      htmltools::htmlEscape(anchor_label, attribute = FALSE),
      "</a>"
    ),
    ""
  )
}

popup_body <- Vectorize(
  function(first_name, last_name, email, affiliation, dept_school, title,
           google_scholar, website) {
    tn <- escape_txt(trimws(paste(first_name, last_name)))
    em_raw <- trimws(as.character(email %||% ""))
    mail <- ""
    if (em_raw != "")
      mail <- paste0(
        '<a href="mailto:', escape_attr(em_raw), '">',
        escape_txt(em_raw), "</a>"
      )
    scholar_l <- href_if(google_scholar, "Google Scholar")
    web_l <- href_if(website, "Website")
    links <- paste(collapse = " · ", Filter(nzchar, c(mail, scholar_l, web_l)))
    paste0(
      "<big><strong>", tn, "</strong></big>",
      "<br><span style=\"color:#374151;\"><strong>Title · 職位:</strong> ",
      escape_txt(title), "</span>",
      ifelse(nchar(escape_txt(dept_school)),
        paste0("<br>", escape_txt(dept_school)),
        ""
      ),
      "<br><strong>Affiliation · 機構:</strong> ",
      ifelse(
        nzchar(trimws(as.character(affiliation %||% ""))),
        paste0("<span style=\"color:#374151;font-style:italic\">", escape_txt(affiliation), "</span>"),
        "<span style=\"color:#9ca3af\">(no affiliation in sheet)</span>"
      ),
      ifelse(nchar(links), paste0("<br>", links), "")
    )
  },
  USE.NAMES = FALSE,
  vectorize.args = c(
    "first_name", "last_name", "email", "affiliation",
    "dept_school", "title", "google_scholar", "website"
  )
)

# UI
ui <- fluidPage(
  tags$style(HTML("
    #title_filt {
      font-size: 12px;
      padding: 4px;
      height: auto;
      min-width: 180px;
    }
  ")),
  titlePanel("North American Taiwanese Statistician Map"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        "title_filt",
        label = "Title:",
        choices = c("All", titles_for_filter),
        selected = "All"
      )
    ),
    mainPanel(
      width = 10,
      leafletOutput("map", height = 650),
      br(),
      DT::DTOutput("table"),
      br(),
      tags$div(
        style = "font-size: 12px; color: #555;",
        "Author: Chi-Kuang Yeh | Email: ",
        tags$a(href = "mailto:chi-kuang.yeh@mail.mcgill.ca", "chi-kuang.yeh@mail.mcgill.ca"),
        tags$br(),
        paste("Last updated:", format(Sys.Date(), "%B %d, %Y")),
        tags$hr(),
        "Data: ",
        tags$em("Taiwanese Statistician in the US/Canada"),
        " (Google Sheet). Map coordinates default to Photon geocode of ",
        tags$strong("Affiliation"),
        " (saved in ",
        tags$code(basename(GEOCODE_CACHE_FILE)),
        "). Optional spreadsheet ",
        tags$strong("Latitude"),
        " / ",
        tags$strong("Longitude"),
        " override pins."
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  faculty_data <- reactiveVal(faculty_tbl_initial)

  geo_remain <- reactiveVal(GEO_QUEUE_BACKGROUND)

  # Photon queue on Shiny’s own timer (`invalidateLater`), not `later::later`.
  # (`later` on shinyapps.io can desync WebSockets and disconnect sessions.)
  observe({
    isolate({
      if (GEOCODE_BLOCKING_AT_START) {
        return(invisible(NULL))
      }
      rem <- geo_remain()
      if (length(rem) == 0L) {
        return(invisible(NULL))
      }
      aff <- rem[[1L]]
      geo_remain(rem[-1L])

      ok <- TRUE
      tryCatch(
        geocode_and_cache_one_affiliation(aff, GEOCODE_CACHE_FILE),
        error = function(e) {
          ok <<- FALSE
          warning(
            "Photon geocode failed for ",
            encodeString(aff, quote = "\""),
            ": ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )

      faculty_data(join_faculty_with_geocode_cache(FACULTY_SHEET, GEOCODE_CACHE_FILE))

      if (ok &&
          isTRUE(as.logical(Sys.getenv("SHINY_GEOCODE_MESSAGES", unset = "FALSE")))) {
        message("(Geocoder) cached: ", encodeString(aff, quote = "\""))
      }

      rem_next <- geo_remain()
      if (length(rem_next) > 0L) {
        invalidateLater(GEOCODE_THROTTLE_MS, session)
      }
    })
  })

  filtered_data <- reactive({
    df <- faculty_data()
    if (!is.null(input$title_filt) && input$title_filt != "All") {
      df <- df |> dplyr::filter(title == input$title_filt)
    }
    df
  })

  map_points <- reactive({
    filtered_data() |>
      filter(!is.na(latitude), !is.na(longitude))
  })

  center_map <- reactive({
    mp <- map_points()
    if (nrow(mp) > 0) {
      list(
        lng = mean(mp$longitude, na.rm = TRUE),
        lat = mean(mp$latitude, na.rm = TRUE),
        zoom = 4L
      )
    } else {
      list(lng = -98, lat = 44, zoom = 3L)
    }
  })

  output$map <- renderLeaflet({
    c <- center_map()
    leaflet(options = leafletOptions(worldCopyJump = FALSE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = c$lng, lat = c$lat, zoom = c$zoom) |>
      setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
  })

  observe({
    df <- map_points()
    proxy <- leafletProxy("map")
    proxy |> clearMarkers() |> clearMarkerClusters() |> clearControls()

    if (nrow(df) > 0) {
      pops <- popup_body(
        df$first_name, df$last_name, df$email, df$affiliation,
        df$dept_school, df$title, df$google_scholar, df$website
      )
      labels_full <- paste0(
        trimws(paste(df$first_name, df$last_name)),
        "\n(",
        df$title,
        ")"
      )

      for (t in sort(unique(df$title))) {
        sub <- df[df$title == t, , drop = FALSE]
        ps <- pops[df$title == t]
        lb <- labels_full[df$title == t]

        proxy |> addAwesomeMarkers(
          data = sub,
          lng = sub$longitude,
          lat = sub$latitude,
          icon = icon_for_title(t),
          label = lb,
          popup = ps,
          group = t,
          clusterOptions = cluster_opts,
          labelOptions = labelOptions(
            textsize = "12px",
            direction = "auto",
            opacity = 0.95
          )
        )
      }

      # Legend (titles that appear at least once in current filter)
      leg_items <- ""
      titles_shown <- sort(unique(df$title))
      cols <- vapply(titles_shown, function(ti) {
        s <- title_marker_spec[[ti]]
        if (is.null(s)) s <- title_marker_spec[["Other"]]
        s$markerColor
      }, character(1))
      for (i in seq_along(titles_shown)) {
        leg_items <- paste0(
          leg_items,
          "<i class=\"fa fa-map-marker\" style=\"color:",
          cols[i],
          "; margin-right: 6px;\"></i> ",
          htmltools::htmlEscape(titles_shown[i]), "<br>"
        )
      }
      legend_html <- HTML(paste0(
        "<div style=\"background:white;padding:10px;border-radius:6px;font-size:13px;line-height:1.45;\"><b>Legend (Title)</b><br>",
        leg_items,
        "</div>"
      ))

      overlay <- unique(df$title)
      proxy |> addLayersControl(
        overlayGroups = overlay,
        options = layersControlOptions(collapsed = FALSE)
      )
      proxy |> addControl(legend_html, position = "bottomright")
    }
  })

  output$table <- DT::renderDT({
    tab <- filtered_data() |>
      dplyr::ungroup() |>
      dplyr::transmute(
        `Last Name` = dt_chr(.data$last_name),
        `First name` = dt_chr(.data$first_name),
        Email = dt_chr(.data$email),
        Affiliation = dt_chr(.data$affiliation),
        Title = dt_chr(.data$title),
        `Dept / School` = dt_chr(.data$dept_school),
        `Google Scholar` = dt_link_anchor(.data$google_scholar, "Scholar"),
        Website = dt_link_anchor(.data$website, "Link")
      ) |>
      as.data.frame(check.names = FALSE)
    DT::datatable(
      tab,
      rownames = FALSE,
      escape = 1L:6L,
      selection = "none",
      fillContainer = TRUE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = FALSE
      )
    )
  }, server = FALSE)
}

shinyApp(ui, server)
