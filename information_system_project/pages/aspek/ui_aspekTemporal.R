tabItemAspekTemporal <- tabItem(
  tabName = "aspekTemporal",
  
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
          h4(tags$b("Aspek Temporal")),
          numericInput(
            'Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun.edit', 'Berapa lama Anda sudah tinggal di desa ini? (tahun)',
            value = 0
          ),
          selectInput(
            'Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama.edit', 'Apakah Anda berencana untuk tinggal di desa ini dalam jangka waktu yang lama?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir.edit', 'Apakah Anda melihat perubahan signifikan dalam kondisi ekonomi dan sosial desa dalam beberapa tahun terakhir?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textAreaInput(
            'Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini.edit', 'Apakah Anda memiliki saran atau masukan untuk meningkatkan aspek sosial-ekonomi di desa ini?',
            
          )
        ),
        actionButton("updateAspekTemporal", "Update"),
        actionButton("cancelAspekTemporal", "Cancel")
      )
    )
  ),
  fluidRow(
  conditionalPanel(
    condition = "input.tahun_tinggal == true || input.LOGINSTATUS == '0'",
      box(title = "Tinggal Berapa Lama", 
          status = "primary", solidHeader = TRUE, 
          width = 12,
          h5(tags$b("Berapa lama Anda sudah tinggal di desa ini? (tahun)")),
          plotOutput("barChartTinggalBerapaLama"),
      ),
  ),
  
  conditionalPanel(
    condition = "input.pindah_tempat == true || input.LOGINSTATUS == '0'",
    box(title = "Pindah Tempat", status = "primary", solidHeader = TRUE, 
        width = 6,
        h5("Apakah Anda berencana untuk tinggal di desa ini dalam jangka waktu yang lama?"),
        plotOutput("pieChartPindahTempat"),
    ),
  ),
  conditionalPanel(
    condition = "input.perubahan_ekonomi == true || input.LOGINSTATUS == '0'",
    box(title = "Perubahan Ekonomi", status = "primary", solidHeader = TRUE, 
        width = 6,
        h5("Apakah Anda melihat perubahan signifikan dalam kondisi ekonomi dan sosial desa dalam beberapa tahun terakhir?"),
        plotOutput("pieChartPerubahanEkonomi"),
    ),
  ),
  ),
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
  fluidRow(
    box(title = "Aspek Sosial", width = 12, 
        DTOutput("data_table_apekTemporal"))
  ),
  )
)