# Load libraries
library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
library(reshape2)
library(dplyr)
library(viridis)
library(shinyjs)
library(tidyverse)
library(RColorBrewer)
library(tidytext)
library(wordcloud2)
library(plotly)
library(rsconnect)

# Source UI files
source("pages/auth/ui_login.R")
source("pages/sikd/ui_karakteristik.R")
source("pages/korelasi/ui_korelasi.R")
source("pages/sikd/ui_pendanaan.R")
source("pages/profil/ui_profilDesa.R")
source("pages/sikd/ui_peningkatanPAD.R")
source("pages/sikd/ui_peningkatanPerekonomian.R")
source("pages/sikd/ui_peningkatanProgramWisata.R")
source("pages/sikd/ui_potensiDesa.R")
source("pages/sikd/ui_tambah_data.R")
source("pages/aspek/ui_identitas.R")
source("pages/aspek/ui_aspekEkonomi.R")
source("pages/aspek/ui_aspekSosial.R")
source("pages/aspek/ui_aspekTemporal.R")
source("pages/aspek/ui_aspekSpatial.R")
source("pages/aspek/ui_tambah_hapus.R")

# UI
ui <- dashboardPage(
  title = "SISTEM INFORMASI KETERBUKAAN DESA BANTARKULON",
  skin = "green",
  dashboardHeader(
    title = tags$div(
      tags$div(
        "SISTEM INFORMASI KETERBUKAAN DESA BANTARKULON"
      ),
      tags$div(
        tags$img(src = "Pekalongan.png", height = "46px", style = "margin-right: 10px;"),
        tags$img(src = "SekolahVokasiIPB.png", height = "40px", style = "margin-right: 10px;"),
      ),
      style = "display: flex; justify-content: space-between"
    ),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Profil Desa", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analisis", tabName = "korelasi", icon = icon("chart-line")),
      menuItem("Data Model",
        icon = icon("database"),
        conditionalPanel(
          condition = "input.LOGINSTATUS == '1'",
          menuSubItem("Tambah Data", tabName = "tambahdata", icon = icon("file-circle-plus")),
        ),
        menuSubItem("Karakteristik Desa", tabName = "karakteristik", icon = icon("people-group")),
        menuSubItem("Potensi Desa", tabName = "potensiDesa", icon = icon("magnifying-glass-chart")),
        menuSubItem("Pendanaan Desa", tabName = "pendanaan", icon = icon("sack-dollar")),
        menuSubItem("Peningkatan PAD", tabName = "PeningkatanPAD", icon = icon("money-bill-trend-up")),
        menuSubItem("Peningkatan Perekonomian", tabName = "PeningkatanPerekonomian", icon = icon("arrow-up-right-dots")),
        menuSubItem("Peningkatan Program Wisata", tabName = "programwisata", icon = icon("arrow-up-from-water-pump"))
      ),
      menuItem("SIKD",
        icon = icon("list"),
        conditionalPanel(
          condition = "input.LOGINSTATUS == '1'",
          menuSubItem(
            "Tambah Data Aspek",
            tabName = "tambahdataAspek",
            icon = icon("file-circle-plus")
          ),
        ),
        menuSubItem(
          "Identitas",
          tabName = "identitas",
          icon = icon("users")
        ),
        menuSubItem(
          "Aspek Ekonomi",
          tabName = "aspekEkonomi",
          icon = icon("magnifying-glass-dollar")
        ),
        menuSubItem(
          "Aspek Sosial",
          tabName = "aspekSosial",
          icon = icon("user-group")
        ),
        menuSubItem(
          "Aspek Temporal",
          tabName = "aspekTemporal",
          icon = icon("user-clock")
        ),
        menuSubItem(
          "Aspek Spatial",
          tabName = "aspekSpatial",
          icon = icon("house-medical")
        )
      ),
      menuItem(
        div(class = "menu-login", tags$i(class = "fa-solid fa-right-to-bracket"), uiOutput("loginTextOutput")),
        tabName = "loginpage"
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItemTambahData,
      tabItemKarakteristik,
      tabItemKorelasi,
      tabItemPendanaan,
      tabItemProfilDesa,
      tabItemPeningkatanPAD,
      tabItemPeningkatanPerekonomian,
      tabItemPeningkatanProgramWisata,
      tabItemPotensiDesa,
      tabItemTambahHapusAspek,
      tabItemIdentitas,
      tabItemAspekEkonomi,
      tabItemAspekSosial,
      tabItemAspekTemporal,
      tabItemAspekSpatial,
      tabItemLogin
    ),
    tags$style(HTML("
    .main-sidebar{
      position: fixed;
    }
    .main-header{
      position: fixed;
      width: 100%;
    }
    @media only screen and (max-width: 600px) {
      .content-wrapper{
        min-height: 307px;
        margin-top: 98px !important;
      }
    }

    .content-wrapper{
    min-height: 307px;
    margin-top: 50px;
    }

    .center-box {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
    }
    .center-content {
      text-align: center;
    }
    .menu-login {
        display: flex;
        align-items: center;
        gap: 4px;
    }
    .sidebar-menu .treeview-menu .shiny-panel-conditional a {
        color: #b8c7ce;
        padding: 5px 5px 5px 15px;
        display: block;
        font-size: 14px;
    }

    #gallery{
      display: flex;
      flex-wrap: wrap;
      gap: 6px;
    }
    #gallery div{
      display: flex;
      flex-direction: column;
    }
    ")),
    shinyjs::useShinyjs(),
    tags$script(HTML("
    $(document).on('click', '.update-btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('update_id', id);
      Shiny.setInputValue('form_update', '1');
    });
    $(document).on('click', '.delete-btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_id', id);
    });

    $(document).on('click', '.update-btn-aspek', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('update_aspek_id', id);
      Shiny.setInputValue('form_update_aspek', '1');
    });
    $(document).on('click', '.delete-btn-aspek', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_aspek_id', id);
    });

    $(document).on('click', '.update-btn-galeri', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('update_galeri_id', id);
      Shiny.setInputValue('form_update_galeri', '1');
    });
    $(document).on('click', '.delete-btn-galeri', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_galeri_id', id);
    });

    Shiny.addCustomMessageHandler('form_update_false', function(message) {
      Shiny.setInputValue('form_update', '0');
    });

    // Check if the user is logged in when the app loads
    Shiny.addCustomMessageHandler('checkLogin', function(message) {
      var user = localStorage.getItem('user');
      if (user) {
        Shiny.setInputValue('stored_user', user);
        Shiny.setInputValue('LOGINSTATUS', '1');
      }else {
        Shiny.setInputValue('LOGINSTATUS', '0');
      }
    });

    // Store the user in local storage on login
    Shiny.addCustomMessageHandler('storeLogin', function(message) {
      localStorage.setItem('user', message);
    });

    // Clear the user from local storage on logout
    Shiny.addCustomMessageHandler('clearLogin', function(message) {
      localStorage.removeItem('user');
    });

    Shiny.addCustomMessageHandler('resetFileInput', function(elementId) {
        var el = document.getElementById(elementId);
        if (el) {
          el.value = '';
        }
    });
    "))
  )
)

# Server
server <- function(input, output, session) {
  showModal(modalDialog(
    title = "Loading...",
    "Memuat data aplikasi",
    easyClose = FALSE,
    footer = NULL
  ))

  checkLogin <- function() {
    session$sendCustomMessage("checkLogin", list())
  }
  checkLogin()

  # Load data
  potensi_desa <- read_csv("data/PotensiDesa.csv")
  karakteristik <- read_csv("data/Karakteristik.csv")
  peningkatan_perekonomian <- read_csv("data/PeningkatanPerekonomianDesa.csv")

  # Source server files
  source("pages/auth/server_login.R", local = TRUE)
  source("pages/profil/server_profilDesa.R", local = TRUE)
  source("pages/korelasi/server_korelasi.R", local = TRUE)
  source("pages/sikd/server_tambah_data.R", local = TRUE)
  source("pages/sikd/server_karakteristik.R", local = TRUE)
  source("pages/sikd/server_pendanaan.R", local = TRUE)
  source("pages/sikd/server_peningkatanPAD.R", local = TRUE)
  source("pages/sikd/server_peningkatanPerekonomian.R", local = TRUE)
  source("pages/sikd/server_peningkatanProgramWisata.R", local = TRUE)
  source("pages/sikd/server_potensiDesa.R", local = TRUE)
  source("pages/aspek/server_tambah_hapus.R", local = TRUE)
  source("pages/aspek/server_identitas.R", local = TRUE)
  source("pages/aspek/server_aspekEkonomi.R", local = TRUE)
  source("pages/aspek/server_aspekSosial.R", local = TRUE)
  source("pages/aspek/server_aspekTemporal.R", local = TRUE)
  source("pages/aspek/server_aspekSpatial.R", local = TRUE)

  removeModal()
}

# Run the application
shinyApp(ui, server)