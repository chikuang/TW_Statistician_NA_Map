# North American Taiwanese Statistician Map · 北美台灣統計學人地圖

May 9, 2026

- [Overview / 概要](#overview--概要)
- [Requirements / 環境需求](#requirements--環境需求)
- [Configuration (environment variables) /
  環境變數設定](#configuration-environment-variables--環境變數設定)
- [Data source / 資料來源](#data-source--資料來源)
- [Run locally / 本機執行](#run-locally--本機執行)
- [Deploy to a server / 部署到伺服器](#deploy-to-a-server--部署到伺服器)
  - [Posit Connect / Shiny Server](#posit-connect--shiny-server)
  - [shinyapps.io](#shinyappsio)
  - [Notes / 注意事項](#notes--注意事項)
- [Repository layout / 目錄結構](#repository-layout--目錄結構)
- [Maintainer / 維護者](#maintainer--維護者)
- [License / 授權](#license--授權)

[![](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

**English.** This repository is a [**Shiny**](https://shiny.posit.co/)
app for the **North American Taiwanese Statistician** community: it
places members on a world map and provides a filterable table. You can
run it locally, publish to
[**shinyapps.io**](https://www.shinyapps.io/), host it on **Shiny
Server**, or deploy via [**Posit
Connect**](https://posit.co/products/enterprise/connect/).

**中文。** 本存放庫是 **北美台灣統計學人（North American Taiwanese
Statistician）** 主題的 [**Shiny**](https://shiny.posit.co/)
網頁應用：在世界地圖上以標記顯示成員位置，並附可篩選的資料表。可於本機執行、發布至
[**shinyapps.io**](https://www.shinyapps.io/)、架在 **Shiny
Server**，或透過 [**Posit
Connect**](https://posit.co/products/enterprise/connect/) 部署。

## Overview / 概要

**English.**

- **Purpose:** Visualize where Taiwanese statisticians and related
  members in North America (and beyond) are located, with browsing,
  filtering by **Title**, and links to profiles and websites.
- **Implementation:** Single file [`app.R`](app.R): reads a **Google
  Sheet as CSV** (GViz URL first, then export URL), draws the map with
  **Leaflet** and **marker clustering**, and shows a sortable, paginated
  table with **DT**. Map coordinates default to **Photon** geocoding of
  **Affiliation** (cached in `affiliation_geocode_cache.rds`); optional
  **Latitude** / **Longitude** columns in the sheet override geocoded
  pins.
- **Legend:** Marker colors and icons follow the **Title** field (e.g.,
  faculty roles and alumni categories), as shown in the app legend.

**中文.**

- **用途：**
  視覺化北美等地台灣統計相關學人或成員的空間分布，支援瀏覽、依
  **職位（Title）** 篩選，以及連結到個人資料與網站。
- **實作：** 單一 [`app.R`](app.R)：讀取 **Google 試算表 CSV**（優先
  GViz 網址，再試 export）、以 **Leaflet** 繪製地圖並以 **marker
  clustering** 縮聚標記、以 **DT** 顯示可排序／分頁表格。地圖座標預設以
  **Photon** 對 **機構（Affiliation）** 地理編碼，結果快取於
  `affiliation_geocode_cache.rds`；試算表中若填 **緯度／經度**
  則會覆蓋編碼座標。
- **圖例：** 標記顏色與圖示依
  **Title**（例如教職、學生、校友類別等），與應用內圖例一致。

## Requirements / 環境需求

**English.** **R** (4.2+ recommended). Packages: `shiny`, `leaflet`,
`dplyr`, `DT`, `readr`, `jsonlite` (Photon responses). `tibble` is
pulled in via those dependencies.

**中文.** 建議 **R** 4.2 以上。**套件：**
`shiny`、`leaflet`、`dplyr`、`DT`、`readr`、`jsonlite`（解析
Photon）。`tibble` 會隨上述依賴一併安裝。

``` r
install.packages(c("shiny", "leaflet", "dplyr", "DT", "readr", "jsonlite"))
```

## Configuration (environment variables) / 環境變數設定

**English.** Optional settings for hosted or local runs (defaults are in
[`app.R`](app.R)).

**中文.** 下列為選用設定；預設值見 [`app.R`](app.R)。

| Variable | Role (EN) | 說明（中文） |
|----|----|----|
| `SHINY_GOOGLE_SHEET_ID` | Spreadsheet document ID | 試算表文件 ID |
| `SHINY_GOOGLE_SHEET_GID` | Tab ID (`#gid=` in sheet URL) | 工作表 gid（網址中 `#gid=`） |
| `SHINY_SHEET_CSV_URL` | Override: single CSV URL | 若設定則直接使用此 CSV 連結，略過自動組出的 Google 網址 |
| `SHINY_GEOCODE_CACHE` | Path to RDS geocode cache | 機構地理編碼快取 RDS 路徑 |
| `SHINY_GEOCODE_THROTTLE` | Seconds between Photon requests (~0.35) | Photon 請求間隔（秒） |
| `SHINY_GEOCODE_BLOCKING_START` | If `TRUE`, geocode all before UI (slow) | 若為 `TRUE`，載入 UI 前先跑完編碼（較慢） |
| `SHINY_GEOCODE_MESSAGES` | If `TRUE`, log each cache write to console | 若為 `TRUE`，於 R console 輸出每次寫入快取訊息 |

## Data source / 資料來源

**English.** At startup the app downloads the sheet as CSV. Column names
are matched flexibly (variants and leading BOM). Expected fields include
**First name**, **Last Name**, **Email**, **Affiliation**, **Title**,
**Department/School**, **Google Scholar**, **Website**, and optional
**Latitude** / **Longitude**. Use `SHINY_GOOGLE_SHEET_ID` /
`SHINY_GOOGLE_SHEET_GID` or `SHINY_SHEET_CSV_URL`. Ensure sharing allows
server access (**Anyone with the link → Viewer** usually works for GViz;
**Publish to web** may be needed for `/export?format=csv`).

**中文.** 應用啟動時自試算表下載 CSV。欄名可彈性對應（含常見別名與表頭
BOM）。預期欄位包含 **姓名、Email、機構、職位、系所／學院、Google
Scholar、網站** 以及選填 **緯度／經度**。請設定
`SHINY_GOOGLE_SHEET_ID`、`SHINY_GOOGLE_SHEET_GID` 或
`SHINY_SHEET_CSV_URL`，並確認分享權限讓伺服器可讀取（**具有連結的任何人均可檢視**
通常適用 GViz；**/export CSV** 有時需 **發布到網路**。）

## Run locally / 本機執行

**English.** From the project root (folder containing `app.R`), in R:

**中文.** 於專案根目錄（含 `app.R`），在 R 中：

``` r
shiny::runApp()
```

**English.** Or from a shell:

**中文.** 或由終端機：

``` r
Rscript -e "shiny::runApp('.', port = 3838)"
```

**English.** Open `http://127.0.0.1:3838` (change the port if needed).

**中文.** 瀏覽器開啟 `http://127.0.0.1:3838`（埠號依你的設定調整）。

## Deploy to a server / 部署到伺服器

**English.** Short summary—follow your organization’s IT and security
policies.

**中文.** 下列為常見流程摘要；請依機構資安與 IT 規範調整。

### Posit Connect / Shiny Server

**English.**

1.  Clone or `git pull` on the server.
2.  Install packages (prefer **renv** or a consistent library).
3.  Publish as a single-file Shiny app; Connect detects `app.R`.

**中文.**

1.  於伺服器 `git clone` 或 `git pull`。
2.  安裝上述套件（建議 **renv** 或固定版 R library）。
3.  以單檔 Shiny 應用發布至此目錄；Connect 通常會自動辨識 `app.R`。

### shinyapps.io

**English.** With **rsconnect** linked to your account:

**中文.** 已完成 **rsconnect** 綁定帳號後：

``` r
rsconnect::deployApp(
  appDir = ".",
  appName = "TW_Statistician_NA_Map" # EN: rename as needed · 中文：依帳號習慣命名
)
```

**English.** Verify the CSV URL is reachable from the cloud (sharing,
firewall).

**中文.** 請確認試算表 CSV 雲端仍可存取（權限、防火牆、Google 限制）。

### Notes / 注意事項

**English.**

- **HTTPS and privacy:** Map tiles and Photon use the network; for
  sensitive data, use private hosting or access control.
- **Google / limits:** Heavy refresh may trigger limits; consider
  scheduled CSV snapshots on the server.

**中文.**

- **HTTPS 與隱私：** 地圖圖磚與 Photon
  會連線外網；資料敏感請改私有託管或限制存取。
- **Google／流量：**
  頻繁重載可能碰到匯出或頻率限制；高流量可改為定期下載 CSV
  至伺服器本機再讀檔。

## Repository layout / 目錄結構

| Path | Description (EN) | 說明（中文） |
|----|----|----|
| [`app.R`](app.R) | UI, server, sheet ingest, geocode cache, Leaflet, DT | Shiny UI／server、試算表讀取、地理編碼快取、Leaflet／DT |
| `affiliation_geocode_cache.rds` | Optional RDS cache (bundled or at runtime) | 選用 RDS 快取（可連同專案佈署或於執行時建立） |
| `rsconnect/` | Prior shinyapps.io metadata (if present) | （若存在）先前 shinyapps.io 發布設定 |

## Maintainer / 維護者

**English.** **Chi-Kuang Yeh** — footer in the app (edit in
[`app.R`](app.R)).

**中文.** **Chi-Kuang Yeh** — 聯絡方式見應用程式頁尾（亦可於
[`app.R`](app.R) 調整信箱與署名）。

## License / 授權

**English.** Same as **`LICENSE`** in this repository: **GPL-3**.

**中文.** 與本存放庫 **`LICENSE`** 相同：**GPL-3**。
