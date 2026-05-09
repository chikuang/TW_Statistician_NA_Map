library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(readr)

# Cluster options
cluster_opts <- markerClusterOptions(
  maxClusterRadius = 10,
  showCoverageOnHover = FALSE,
  zoomToBoundsOnClick = TRUE,
  spiderfyOnMaxZoom = TRUE
)

# Read data from Google Sheets
sheet_url <- "https://docs.google.com/spreadsheets/d/1Qfcew4C7NDm0ycU7QpnaNZF6wwcijo48bmpi-GuKe6k/export?format=csv&gid=0"
faculty_data <- read_csv(sheet_url, show_col_types = FALSE) |>
  mutate(
    Latitude = as.numeric(trimws(as.character(Latitude))),
    Longitude = as.numeric(trimws(as.character(Longitude)))
  ) |>
  filter(!is.na(Latitude), !is.na(Longitude))

# Icons
professorIcon <- makeAwesomeIcon("graduation-cap", markerColor = "red", library = "fa")
studentIcon <- makeAwesomeIcon("user", markerColor = "gray", library = "fa")
alumniStatIcon <- makeAwesomeIcon("black-tie", markerColor = "blue", library = "fa")
alumniActSciIcon <- makeAwesomeIcon("black-tie", markerColor = "orange", library = "fa")
alumniPostdocIcon <- makeAwesomeIcon("black-tie", markerColor = "purple", library = "fa")

# UI
ui <- fluidPage(
  tags$style(HTML("
    #dept {
      font-size: 12px;
      padding: 4px;
      height: auto;
      width: 150px;
    }
  ")),
  titlePanel("UWaterloo Statistics and Actuarial Science Researcher Map"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("dept", "Area:",
                             choices = c("All", sort(unique(faculty_data$Area))),
                             selected = "All")
    ),
    mainPanel(width = 9,
              leafletOutput("map", height = 650),
              br(),
              DTOutput("table"),
              br(),
              tags$div(style = "font-size: 12px; color: #555;",
                       "Author: Chi-Kuang Yeh | Email: ",
                       tags$a(href = "mailto:chi-kuang.yeh@mail.mcgill.ca", "chi-kuang.yeh@mail.mcgill.ca"),
                       tags$br(),
                       paste("Last updated:", format(Sys.Date(), "%B %d, %Y"))
              )
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- faculty_data
    if (input$dept != "All") {
      df <- df |> filter(Area == input$dept)
    }
    df
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(worldCopyJump = FALSE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(
        lng = mean(faculty_data$Longitude, na.rm = TRUE),
        lat = mean(faculty_data$Latitude, na.rm = TRUE),
        zoom = 4
      ) |>
      setMaxBounds(
        lng1 = -180, lat1 = -85,
        lng2 = 180, lat2 = 85
      )
  })
  
  observe({
    df <- filtered_data()
    proxy <- leafletProxy("map", data = df)
    proxy |> clearMarkers() |> clearMarkerClusters()
    
    if (nrow(df) > 0) {
      professors <- df |> filter(Role == "Professor")
      students <- df |> filter(Role == "Student")
      
      alumni_roles <- list(
        list(role = "Alumni-Professor-Stat", icon = alumniStatIcon, group = "Alumni-Professor-Stat"),
        list(role = "Alumni-Professor-ActSci", icon = alumniActSciIcon, group = "Alumni-Professor-ActSci"),
        list(role = "Alumni-Postdoc", icon = alumniPostdocIcon, group = "Alumni-Postdoc")
      )
      
      if (nrow(professors) > 0) {
        proxy |> addAwesomeMarkers(
          data = professors,
          lng = ~Longitude, lat = ~Latitude,
          icon = professorIcon,
          label = ~paste(First_Name, Last_Name),
          popup = ~paste0("<b style='color:red;'>Professor</b><br>",
                          First_Name, " ", Last_Name, " (",
                          Graduation_School, " ", Graduation_Year, ")<br>",
                          Area, "<br>",
                          "<a href='", Profile, "' target='_blank'>Profile</a>"),
          group = "Professor",
          clusterOptions = cluster_opts
        )
      }
      
      if (nrow(students) > 0) {
        proxy |> addAwesomeMarkers(
          data = students,
          lng = ~Longitude, lat = ~Latitude,
          icon = studentIcon,
          label = ~paste(First_Name, Last_Name),
          popup = ~paste0("<b style='color:gray;'>Student</b><br>",
                          First_Name, " ", Last_Name, "<br>",
                          Area, "<br>",
                          Graduation_School, "<br>",
                          "<a href='", Profile, "' target='_blank'>Profile</a>"),
          group = "Student",
          clusterOptions = cluster_opts
        )
      }
      
      for (grp in alumni_roles) {
        alumni_df <- df |> filter(Role == grp$role)
        color_map <- list(
          "Alumni-Professor-Stat" = "blue",
          "Alumni-Professor-ActSci" = "orange",
          "Alumni-Postdoc" = "purple"
        )
        role_color <- color_map[[grp$role]]
        
        if (nrow(alumni_df) > 0) {
          proxy |> addAwesomeMarkers(
            data = alumni_df,
            lng = ~Longitude, lat = ~Latitude,
            icon = grp$icon,
            label = ~paste(First_Name, Last_Name),
            popup = ~paste0("<b style='color:", role_color, ";'>", grp$role, "</b><br>",
                            First_Name, " ", Last_Name, " (",
                            Graduation_School, " ", Graduation_Year, ")<br>",
                            Position, "<br>", Department_Current, "<br>",
                            Affiliation, "<br>",
                            "<a href='", Profile, "' target='_blank'>Profile</a>"),
            group = grp$group,
            clusterOptions = cluster_opts
          )
        }
      }
      
      legend_html <- HTML('
  <div style="background:white; padding:10px; border-radius:6px; font-size:14px;">
    <b>Legend</b><br>
    <i class="fa fa-graduation-cap" style="color:red"></i> <span style="color:red;">Professor</span><br>
    <i class="fa fa-user" style="color:gray"></i> <span style="color:gray;">Student</span><br>
    <i class="fa fa-black-tie" style="color:blue"></i> <span style="color:blue;">Alumni-Professor-Stat</span><br>
    <i class="fa fa-black-tie" style="color:orange"></i> <span style="color:orange;">Alumni-Professor-ActSci</span><br>
    <i class="fa fa-black-tie" style="color:purple"></i> <span style="color:purple;">Alumni-Postdoc</span>
  </div>
')
      
      # 加在這裡
      proxy |> clearControls()
      
      proxy |> addLayersControl(
        overlayGroups = c("Professor", "Student",
                          "Alumni-Professor-Stat", "Alumni-Professor-ActSci", "Alumni-Postdoc"),
        options = layersControlOptions(collapsed = FALSE)
      )
      
      proxy |> addControl(legend_html, position = "bottomright")
      
    }
  })
  
  output$table <- renderDT({
    filtered_data() |>
      select(
        First_Name, Last_Name, Area, Role,
        Graduation_School, Graduation_Year,
        Department_Current, Affiliation, Position,
        Profile
      )
  }, options = list(pageLength = 10))
}

shinyApp(ui, server)