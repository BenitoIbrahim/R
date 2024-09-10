tabItemIdentitas <- tabItem(
  tabName = "identitas",
  fluidRow(
    conditionalPanel(
      condition = "input.jenis_kelamin == true || input.LOGINSTATUS == '0'",
      box(title = "Jenis Kelamin", width = 6, 
          plotOutput("pie_chart_jenis_kelamin_identitas"))
    ),
    conditionalPanel(
      condition = "input.pendidikan == true || input.LOGINSTATUS == '0'",
      box( title = "Pendidikan", width = 6, 
           plotOutput("pie_chart_pendidikan_identitas"))
    ),
    conditionalPanel(
      condition = "input.usia == true || input.LOGINSTATUS == '0'",
      box(id = "age_bar_chart_box", title = "Distribusi Usia", width = 12, 
          plotOutput("usia_identitas_bar_chart"))
    ),
    conditionalPanel(
      condition = "input.status_perkawinan == true || input.LOGINSTATUS == '0' || input.apakah_memiliki_anak == true || input.jumlah_anak == true",
      box(title = "Status Perkawinan dan Kepemilikan Ana",
          width = 12,
          fluidRow(
            box(title = "Status Perkawinan dan Kepemilikan Anak", 
                status = "primary", solidHeader = TRUE, 
                width = 12,
                h5(tags$b("Status perkawinan dan Apakah memiliki anak?")),
                plotOutput("barChartPerkawinanKepemilikanAnak"),
                box(
                  title = "Hasil Analisis",
                  width = 12,
                  textOutput("analysisTextPerkawinanKepemilikanAnak")
                ),
            ),
          ),
      ),
    ),
    conditionalPanel(
      condition = "input.apakah_bekerja == true || input.LOGINSTATUS == '0' || input.pekerjaan == true",
      box(title = "Pekerjaan",
          width = 12,
          fluidRow(
            box(title = "Pekerjaan", 
                status = "primary", solidHeader = TRUE, 
                width = 12,
                h5(tags$b("Apakah anda bekerja saat ini? Jika bekerja, apa pekerjaan anda saat ini?")),
                plotOutput("barChartPekerjaanIdentitas"),
                box(
                  title = "Hasil Analisis",
                  width = 12,
                  textOutput("analysisTextPekerjaanIdentitas")
                ),
            ),
          ),
      ),
    )
  ),
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
        
        # Identitas
        box(
          status = "primary",
          width = 12,
          h4(tags$b("Identitas Responden")),
          textInput(
            "Nama_Identitas.edit", "Nama"
            
          ),
          selectInput(
            "Jenis.kelamin.edit", "Jenis kelamin",
            c(
              "Perempuan"="1",
              "Laki-laki"="2"
            ),
          ),
          numericInput(
            "Usia.tahun.edit", "Usia (tahun)",
            value = 0
            
          ),
          selectInput(
            "Status.perkawinan.edit", "Status perkawinan",
            c(
              'Belum Menikah'='1',
              'Menikah'='2',
              'Duda/ Janda'='3',
              'Lainnya'='4'
            )
          ),
          selectInput(
            "Apakah.memiliki.anak.edit", "Apakah memiliki anak?",
            c(
              'Ya'='1',
              'Tidak'='2',
              'Belum tahu'='3'
            )
          ),
          numericInput(
            "Jumlah.anak.orang.edit", "Jumlah anak (orang)",
            value = 0
            
          ),
          selectInput(
            "Tingkat.pendidikan.edit", "Tingkat pendidikan ",
            c(
              'Tidak lulus SD'='1',
              'SD'='2',
              'SMP'='3',
              'SMA'='4',
              'Diploma'='5',
              'Sarjana'='6',
              'Pasca Sarjana'='7',
              'Lainnya'='8'
            )
          ),
          selectInput(
            "Apakah.anda.bekerja.saat.ini.edit", "Apakah anda bekerja saat ini?",
            c(
              'Ya'='1',
              'Tidak'='2',
              'Belum tahu'='3'
            )
          ),
          textInput(
            "Jika.bekerja.apa.pekerjaan.anda.saat.ini.edit", "Jika bekerja, apa pekerjaan anda saat ini?"
            
          )
        ),
        actionButton("updateIdentitas", "Update"),
        actionButton("cancelIdentitas", "Cancel")
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
  fluidRow(
    box(title = "Identitas Responden", width = 12, 
        DTOutput("data_table_identitas"))
  ),
  )
)