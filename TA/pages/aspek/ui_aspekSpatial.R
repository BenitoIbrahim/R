tabItemAspekSpatial <- tabItem(
  tabName = 'aspekSpatial',
  
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
        
        # Spatial
        box(
          status = "primary",
          width = 12,
          h4(tags$b("Aspek Spatial")),
          numericInput(
            'Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.KM.edit', 'Seberapa jauh rumah Anda dari pusat desa atau kota terdekat? .... KM',
            value = 0
          ),
          selectInput(
            'Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini.edit', 'Bagaimana Anda biasanya mengakses fasilitas umum seperti pasar, sekolah, dan rumah sakit di desa ini?',
            c(
              'Jalan Kaki'='1',
              'Sepeda'='2',
              'Sepeda Motor'='3',
              'Mobil'='4',
              'Angkutan Umum'='5',
              'Lainnya'='6'
            )
          ),
          selectInput(
            'Apakah.transportasi.umum.cukup.memadai.di.desa.ini.edit', 'Apakah transportasi umum cukup memadai di desa ini?',
            c(
              'Sangat memadai'='4',
              'Cukup memadai'='3',
              'Tidak cukup'='2',
              'Tidak ada'='1'
            )
          ),
          selectInput(
            'Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini.edit', 'Bagaimana kondisi infrastruktur jalan di desa ini? ',
            c(
              'Baik'='4',
              'Cukup baik'='3',
              'Tidak cukup baik'='2',
              'Tidak ada'='1'
            )
          ),
          selectInput(
            'Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah.edit', 'Apakah terdapat masalah terkait sanitasi di desa ini, seperti akses ke fasilitas toilet yang memadai atau masalah limbah?',
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Bagaimana.pasokan.air.bersih.di.desa.ini.edit', 'Bagaimana pasokan air bersih di desa ini? ',
            c(
              'Baik'='4',
              'Cukup baik'='3',
              'Tidak cukup baik'='2',
              'Tidak ada'='1'
            )
          ),
          selectInput(
            'Apakah.Anda.menghadapi.masalah.terkait.air.bersih.edit', 'Apakah Anda menghadapi masalah terkait air bersih?',
            c(
              'Sering'='4',
              'Kadang-kadang'='3',
              'Jarang'='2',
              'Tidak Pernah'='1'
            )
          ),
          selectInput(
            'Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya.edit', 'Apakah terdapat masalah terkait polusi di desa ini, seperti polusi udara atau pencemaran lingkungan lainnya?',
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk.edit', 'Apakah Anda merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai untuk memenuhi kebutuhan sehari-hari penduduk? ',
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          )
        ),
        actionButton("updateAspekSpatial", "Update"),
        actionButton("cancelAspekSpatial", "Cancel")
      )
    )
  ),
  fluidRow(
  conditionalPanel(
    condition = "input.jauh_rumah == true || input.LOGINSTATUS == '0'",
      box(title = "Jarak Rumah", 
          status = "primary", solidHeader = TRUE, 
          width = 12,
          h5(tags$b("Seberapa jauh rumah Anda dari pusat desa atau kota terdekat? .... KM")),
          plotOutput("barChartJarakRumah"),
      ),
  ),
  conditionalPanel(
    condition = "input.akses_fasilitas == true || input.LOGINSTATUS == '0'",
    box(title = "Akses Fasilitas", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Bagaimana Anda biasanya mengakses fasilitas umum seperti pasar, sekolah, dan rumah sakit di desa ini?"),
        plotOutput("pieChartAksesFasilitas"),
        box(title = "Analisis Akses Fasilitas", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisAksesFasilitas"))),
  ),
  conditionalPanel(
    condition = "input.transportasi_memadai == true || input.LOGINSTATUS == '0'",
    box(title = "Ketersedian Transportasi", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Apakah transportasi umum cukup memadai di desa ini?"),
        plotOutput("pieChartKetersediaanTransportasi"),
        box(title = "Analisis Mengikuti Pertemuan Desa", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisKetersediaanTransportasi"))),
  ),
  conditionalPanel(
    condition = "input.kondisi_infrastruktur == true || input.LOGINSTATUS == '0'",
    box(title = "Kondisi Infrastruktur", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Bagaimana kondisi infrastruktur jalan di desa ini?"),
        plotOutput("pieChartKondisiInfrastruktur"),
        box(title = "Analisis Kondisi Infrastruktur", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisKondisiInfrastruktur"))),
  ),
  conditionalPanel(
    condition = "input.pasokan_air == true || input.LOGINSTATUS == '0' || input.air_bersih == true",
    box(title = "Pasokan Air",
        width = 12,
        fluidRow(
          box(title = "Pasokan Air dan Kendala Air Bersih", 
              status = "primary", solidHeader = TRUE, 
              width = 12,
              h5(tags$b("Bagaimana pasokan air bersih di desa dan Apakah Anda menghadapi masalah terkait air bersih?")),
              plotOutput("barChartPasokanAir"),
              box(
                title = "Hasil Analisis",
                width = 12,
                textOutput("analysisTextPasokanAir")
              ),
          ),
        ),
    ),
  ),
  conditionalPanel(
    condition = "input.polusi == true || input.LOGINSTATUS == '0'",
    box(title = "Polusi", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Apakah terdapat masalah terkait polusi di desa ini, seperti polusi udara atau pencemaran lingkungan lainnya?"),
        plotOutput("pieChartPolusiAspek"),
        box(title = "Analisis Polusi", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisPolusiAspek"))),
  ),
  conditionalPanel(
    condition = "input.keberadaan_fasilitas == true || input.LOGINSTATUS == '0'",
    box(title = "Kebeeradaan Fasilitas", status = "danger", solidHeader = TRUE, 
        width = 4,
        h5("Apakah Anda merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai untuk memenuhi kebutuhan sehari-hari penduduk?"),
        plotOutput("pieChartBeradaanFasilitas"),
        box(title = "Analisis Kebeeradaan Fasilitas", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisBeradaanFasilitas"))),
  ),
  
  conditionalPanel(
    condition = "input.sanitasi_desa == true || input.LOGINSTATUS == '0'",
    box(title = "Sanitasi Desa", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Apakah terdapat masalah terkait sanitasi di desa ini, seperti akses ke fasilitas toilet yang memadai atau masalah limbah?"),
        plotOutput("pieChartSanitasiDesa"),
        box(title = "Analisis SanitasiDesa", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisSanitasiDesa"))),
  ),
  ),
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
  fluidRow(
    box(title = "Aspek Spatial", width = 12, 
        DTOutput("data_table_apekSpatial"))
  ),
  )
)