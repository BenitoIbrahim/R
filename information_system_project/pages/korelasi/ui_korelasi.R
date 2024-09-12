# pages/korelasi/ui_korelasi.R

tabItemKorelasi <- tabItem(
  tabName = "korelasi",
  tags$style(HTML("
  #korelasi1Plot {
  height: auto !important;
  }
  ")),
  fluidRow(
    box(
      title = "Analisis Data",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tabsetPanel(
        tabPanel("Overview", 
                 h3("Korelasi 1: Potensi Desa dan Pembangunan Ekonomi"),
                 plotOutput("korelasi1Plot"),
                 fluidRow(
                   box(
                     width = 12,
                     h5("Analisis Korelasi 1"),
                     textOutput("korelasi1Explanation"),
                   )
                 ),
                 h3("Korelasi 2: Peluang Ekonomi dan Bantuan Modal"),
                 plotOutput("korelasi2Plot"),
                 fluidRow(
                   box(
                     width = 12,
                     h5("Analisis Korelasi 2"),
                     textOutput("korelasi2Explanation"),
                   )
                 ),
                 h3("Korelasi 3: Stabilitas Harga Pangan dan Pendapatan Masyarakat"),
                 plotOutput("korelasi3Plot"),
                 fluidRow(
                   box(
                     width = 12,
                     h5("Analisis Korelasi 3"),
                     textOutput("korelasi3Explanation"),
                   )
                 ),
                 h3("Korelasi 4: Ketersediaan Lapangan Pekerjaan dan Dana Pembangunan"),
                 plotOutput("korelasi4Plot"),
                 fluidRow(
                   box(
                     width = 12,
                     h5("Analisis Korelasi 4"),
                     textOutput("korelasi4Explanation"),
                   )
                 ),
                 h3("Korelasi 5: Peluang Ekonomi dan Bantuan Perusahaan"),
                 plotOutput("korelasi5Plot"),
                 fluidRow(
                   box(
                     width = 12,
                     h5("Analisis Korelasi 5"),
                     textOutput("korelasi5Explanation"),
                   )
                 ),
                 h3("Korelasi 6: Infrastruktur dan Aksesibilitas Fasilitas Umum"),
                 plotOutput("korelasi6Plot"),
                 fluidRow(
                   box(
                     width = 12,
                     h5("Analisis Korelasi 6"),
                     textOutput("korelasi6Explanation"),
                   )
                 ),
                 h3("Korelasi 7: Dampak Lingkungan dan Kualitas Hidup"),
                 plotOutput("korelasi7Plot"),
                 fluidRow(
                   box(
                     width = 12,
                     h5("Analisis Korelasi 7"),
                     textOutput("korelasi7Explanation"),
                   )
                 ),
        )
      )
    )
  )
)