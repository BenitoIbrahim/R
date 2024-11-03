tabItemAspekSosial <- tabItem(
  tabName = "aspekSosial",
  tags$script(HTML("
                  Shiny.addCustomMessageHandler('form_update_aspek', function(message) {
                    Shiny.setInputValue('form_update_aspek', '0');
                  });
                  Shiny.addCustomMessageHandler('selected_id_aspek', function(message) {
                    Shiny.setInputValue('selected_id_aspek', message);
                  });
                ")),
  fluidRow(
  conditionalPanel(
    condition = "input.form_update_aspek == '1'",
    fluidRow(
      box(
        title = h3(tags$b("Update Data")),
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        
        # Sosial
        box(
          status = "primary",
          width = 12,
          h4(tags$b("Aspek Sosial")),
          selectInput(
            'Apakah.Anda.tinggal.di.desa.ini.sejak.lahir.edit', 
            'Apakah Anda tinggal di desa ini sejak lahir?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textInput(
            'Jika.tidak.sebutkan.asal.tempat.edit', 'Jika tidak, sebutkan asal tempat',
            
          ),
          selectInput(
            'Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini.edit', 'Apakah Anda aktif dalam kegiatan sosial kemasyarakatan di desa ini?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa.edit', 'Seberapa sering Anda berpartisipasi dalam pertemuan warga atau komunitas di desa?',
            
            c(
              'Sering'='4',
              'Kadang-kadang'='3',
              'Jarang'='2',
              'Tidak Pernah'='1'
            )
          ),
          selectInput(
            'Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini.edit', 'Apakah anda merasa adanya dukungan sosial yang memadai di desa ini?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
        ),
        actionButton("updateAspekSosial", "Update"),
        actionButton("cancelAspekSosial", "Cancel")
      ),
    )
  ),
  conditionalPanel(
    condition = "input.tinggal_didesa == true || input.LOGINSTATUS == '0' || input.asal_tempat == true",
    box(title = "Asal Tinggal",
        width = 12,
        fluidRow(
          box(title = "Tinggal di desa sejak kapan", 
              status = "primary", solidHeader = TRUE, 
              width = 12,
              h5(tags$b("Apakakah tinggal di desa ini sejak lahir? jika tidak sebutkan?")),
              plotlyOutput("barChartTinggalDidesa"),
              box(
                title = "Hasil Analisis",
                width = 12,
                textOutput("analysisTextTinggalDidesa")
              ),
          ),
        ),
    ),
  ),
  conditionalPanel(
    condition = "input.kegiatan_masyarakat == true || input.LOGINSTATUS == '0'",
    box(title = "Aktif Kegiatan Kemasyarakatan", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Apakah Anda aktif dalam kegiatan sosial kemasyarakatan di desa ini?"),
        plotOutput("pieChartKegiatanKemasyrakatan"),
        box(title = "Analisis Kegiatan Kemasyarakatan", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisKegiatanKemasyrakatan"))),
  ),
  conditionalPanel(
    condition = "input.pertemuan_masyarakat == true || input.LOGINSTATUS == '0'",
    box(title = "Mengikuti Pertemuan Desa", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Seberapa sering Anda berpartisipasi dalam pertemuan warga atau komunitas di desa?"),
        plotOutput("pieChartPertemuanDesa"),
        box(title = "Analisis Mengikuti Pertemuan Desa", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisPertemuanDesa"))),
  ),
  conditionalPanel(
    condition = "input.dukungan_sosial == true || input.LOGINSTATUS == '0'",
    box(title = "Dukungan Sosial", status = "primary", solidHeader = TRUE, 
        width = 4,
        h5("Apakah anda merasa adanya dukungan sosial yang memadai di desa ini?"),
        plotOutput("pieChartDukunganSOsial"),
        box(title = "Analisis Dukungan Sosial", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12, 
            textOutput("analysisDukunganSOsial"))),
  ),
  ),
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
  fluidRow(
    box(title = "Aspek Sosial", width = 12, 
        DTOutput("data_table_apekSosial"))
  ),
  )
)