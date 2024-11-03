tabItemAspekEkonomi <- tabItem(
  tabName = "aspekEkonomi",
  tags$script(HTML("
                  Shiny.addCustomMessageHandler('form_update_aspek', function(message) {
                    Shiny.setInputValue('form_update_aspek', '0');
                  });
                  Shiny.addCustomMessageHandler('selected_id_aspek', function(message) {
                    Shiny.setInputValue('selected_id_aspek', message);
                  });
                ")),
  conditionalPanel(
    condition = "input.form_update_aspek == '1'",
    fluidRow(
      box(
        title = h3(tags$b("Update Data")),
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        
        # Ekonomi
        box(
          status = "primary",
          width = 12,
          h4(tags$b("Aspek Ekonomi")),
          selectInput(
            "Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini.edit", 
            "Apakah Anda atau anggota keluarga Anda memiliki usaha sendiri di desa ini? ",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah.edit", 
            "Berapa pendapatan bulanan Anda atau anggota keluarga Anda secara keseluruhan? (Kisaran jumlah)",
            c(
              "500.000 - 1.500.000"="1",
              "1.500.001 - 3.000.000"="2",
              "3.000.001 - 4.500.000"="3",
              "4.500.001 - ke atas"="4"
            )
          )
          ,
          selectInput(
            "Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial.edit", 
            "Apakah Anda atau anggota keluarga Anda menerima bantuan sosial?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textInput(
            "Jika.ya.sebutkan.aspek.ekonomi.edit", 
            "Jika ya, sebutkan"
          ),
          selectInput(
            "Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini.edit", 
            "Bagaimana Anda menilai stabilitas harga pangan saat ini?",
            c(
              "Sangat stabil"="4",
              "Stabil"="3",
              "Tidak stabil"="2",
              "Sangat tidak stabil"="1"
            )
          ),
          selectInput(
            "Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda.edit", 
            "Apakah Anda memiliki simpanan atau investasi di luar penghasilan bulanan Anda?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini.edit", 
            "Apakah Anda merasa terdapat peluang ekonomi yang cukup di desa ini?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini.edit", 
            "Bagaimana Anda menilai aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini?",
            c(
              "Sangat terbuka"="4",
              "Terbuka"="3",
              "Tidak terbuka"="2",
              "Sangat tidak terbuka"="1"
            )
          ),
          selectInput(
            "Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini.edit", 
            "Apakah Anda merasa terdapat kesenjangan ekonomi di antara penduduk di desa ini?",
            c(
              "Sangat terasa"="4",
              "Terasa"="3",
              "Tidak terasa"="2",
              "Sangat tidak terasa"="1"
            )
          ),
          actionButton("updateAspekEkonomi", "Update"),
          actionButton("cancelAspekEkonomi", "Cancel")
        )
      )
    )
  ),
  fluidRow(
  conditionalPanel(
    condition = "input.memiliki_usaha == true || input.pendapatan_bulanan == true || input.LOGINSTATUS == '0'",
    box(title = "Usaha dan Pendapatan",
        width = 12,
        fluidRow(
          box(title = "Kepemilikan Usaha dan Pendapatan Perbulan", 
              status = "primary", solidHeader = TRUE, 
              width = 12,
              h5(tags$b("Apakah Anda atau anggota keluarga Anda memiliki usaha dan Berapa pendapatan bulanan Anda?")),
              plotOutput("barChartUsahaPendapatan"),
              box(
                title = "Hasil Analisis",
                width = 12,
                textOutput("analysisTextUsahaPendapatan")
              ),
          ),
        ),
    ),
  ),
  conditionalPanel(
    condition = "input.bantuan_sosial == true || input.LOGINSTATUS == '0' || input.sebutkan_bantuan == true",
    box(title = "Bantuan",
        width = 12,
        fluidRow(
          box(title = "Bantuan dan Jenis Bantuan", 
              status = "primary", solidHeader = TRUE, 
              width = 12,
              h5(tags$b("Apakah keluarga Anda menerima bantuan sosial? Jika ya, sebutkan")),
              plotlyOutput("barChartBantuanSosial"),
              box(
                title = "Hasil Analisis",
                width = 12,
                textOutput("analysisTextBantuanSosial")
              ),
          ),
        ),
    ),
  ),
  conditionalPanel(
    condition = "input.stabilitas_harga == true || input.LOGINSTATUS == '0' || input.simpanan_investasi == true",
    box(title = "Stabilitas Harga",
        width = 12,
        fluidRow(
          box(title = "Stabilitas dan Investasi", 
              status = "primary", solidHeader = TRUE, 
              width = 12,
              h5(tags$b("Bagaimana Anda menilai stabilitas harga dan Apakah Anda memiliki simpanan atau investasi?")),
              plotOutput("barChartStabilitasInvestasi"),
              box(
                title = "Hasil Analisis",
                width = 12,
                htmlOutput("analysisStabilitasInvestasi")
              ),
          ),
        ),
    ),
  ),
  conditionalPanel(
    condition = "input.peluang_ekonomi == true || input.LOGINSTATUS == '0'",
    box(title = "Peluang ekonomi di Desa Mekasari", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Apakah Anda merasa terdapat peluang ekonomi yang cukup di desa ini?"),
        plotOutput("pieChartPeluangEkonomi"),
        box(title = "Analisis Peluang Ekonomi", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisPeluangEkonomi"))),
  ),
  conditionalPanel(
    condition = "input.ketersedian_pekerjaan == true || input.LOGINSTATUS == '0'",
    box(title = "Ketersediaan Lapangan Pekerjaan", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Bagaimana Anda menilai aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini?"),
        plotOutput("pieChartLapanganPekerjaan"),
        box(title = "Analisis Lapangan Pekerjaan", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisLapanganPekerjaan"))),
  ),
  conditionalPanel(
    condition = "input.kesenjangan_ekonomi == true || input.LOGINSTATUS == '0'",
    box(title = "Kesenjangan Ekonomi", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Apakah Anda merasa terdapat kesenjangan ekonomi di antara penduduk di desa ini?"),
        plotOutput("pieChartKesenjanganEkonomiAspekEkonomi"),
        box(title = "Analisis Kesenjangan Ekonomi", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisKesenjanganEkonomiAspekEkonomi"))),
  ),
  
  ),
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
  fluidRow(
    box(title = "Aspek Ekonomi", width = 12, 
        DTOutput("data_table_apekEkonomi"))
  ),
  )
)