# North American Taiwanese Statistician Map

May 9, 2026

- [Overview（專案說明）](#overview專案說明)
- [Requirements（環境）](#requirements環境)
- [Data source（資料來源）](#data-source資料來源)
- [Run locally（本機執行）](#run-locally本機執行)
- [Deploy to a server（部署到伺服器）](#deploy-to-a-server部署到伺服器)
  - [Posit Connect / Shiny Server](#posit-connect--shiny-server)
  - [shinyapps.io](#shinyappsio)
  - [注意事項](#注意事項)
- [Repository layout（目錄）](#repository-layout目錄)
- [Maintainer](#maintainer)
- [License](#license)

[![](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

這個存放庫是一段 **North American Taiwanese
Statistician（北美台灣統計學人）** 主題的
[**Shiny**](https://shiny.posit.co/) 網頁程式：在世界地圖上以 marker
標示會員／聯絡人地理位置，並附可篩選的資料表，適合發布到自己的 **Shiny
Server**、[**shinyapps.io**](https://www.shinyapps.io/)，或組織內的
[**Posit Connect**](https://posit.co/products/enterprise/connect/)。

## Overview（專案說明）

- **用途**：視覺化北美等地台灣統計相關學人或社群成員的空間分布，方便瀏覽、篩選與連結到個人檔案網頁。
- **實作**：單檔 [`app.R`](app.R)：讀取 **Google 試算表匯出的 CSV**，以
  **Leaflet** 繪製地圖、**marker cluster** 縮聚各點、**DT**
  顯示可排序／分頁表格。
- **角色圖例**（資料欄 `Role`）：例如 Professor／Student，以及數種
  Alumni 類別（顏色與圖示在介面上有說明）。

## Requirements（環境）

- **R**（建議 4.2+）。
- **R 套件**：`shiny`、`leaflet`、`dplyr`、`DT`、`readr`。

``` r
install.packages(c("shiny", "leaflet", "dplyr", "DT", "readr"))
```

## Data source（資料來源）

應用程式啟動時會從 `app.R` 內設定的 Google Sheets CSV
公開匯出網址讀取資料。欄位需包含至少 **`Latitude`、`Longitude`**，以及
UI 用到的 **`Area`、`Role`、`First_Name`、`Last_Name`、`Profile`**
等（與現有試算表結構對齊即可）。

若要改用你自己的試算表，請在 [`app.R`](app.R) 中更新 `sheet_url`，並確認
**「檔案 → 發布到網路」或同等共享設定**，讓 CSV 匯出連結可被伺服器抓取。

## Run locally（本機執行）

在專案根目錄（含 `app.R` 的那一層）於 R 內：

``` r
shiny::runApp()
```

或於終端機：

``` r
Rscript -e "shiny::runApp('.', port = 3838)"
```

然後瀏覽器開 `http://127.0.0.1:3838`（埠號以前述設定為準）。

## Deploy to a server（部署到伺服器）

以下為常見做法摘要；請依機構資安與 IT 規範調整。

### Posit Connect / Shiny Server

1.  將此存放庫 clone 或使用 `git pull` 到伺服器。
2.  在該目錄安裝上述套件（建議專用 **renv** 或系統 R library 一致化）。
3.  以 **Shiny Server** 或 **Posit Connect** 的「單一 `app.R` Shiny
    應用」方式掛載此目錄；Connect 通常會自動偵測 `app.R`。

### shinyapps.io

若已安裝 **rsconnect** 並完成帳號綁定：

``` r
rsconnect::deployApp(
  appDir = ".",
  appName = "TW_Statistician_NA_Map"  # 依帳號與命名習慣修改
)
```

部署前請確認 **`sheet_url`
在雲端環境仍可存取**（試算表權限／網路限制）。

### 注意事項

- **HTTPS
  與隱私**：地圖與外部圖磚會連到網路；若資料敏感，請改內部託管或限制存取。
- **Google
  配額**：大量重新整理可能觸發匯出頻率限制；高流量可考慮改為定期下載 CSV
  到伺服器本機再讀檔。

## Repository layout（目錄）

| 路徑         | 說明                                       |
|--------------|--------------------------------------------|
| `app.R`      | Shiny UI／server、資料讀取與 Leaflet／DT   |
| `rsconnect/` | （若存在）過去發布 shinyapps.io 的設定備份 |

## Maintainer

**Chi-Kuang Yeh** · 聯絡方式見應用程式頁尾（或可於 `app.R`
內調整信箱與署名）。

## License

與存放庫內 **`LICENSE`** 一致：**GPL-3**。
