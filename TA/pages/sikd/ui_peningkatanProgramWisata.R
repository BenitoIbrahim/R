tabItemPeningkatanProgramWisata <- tabItem(
  tabName = "programwisata",
  conditionalPanel(
    condition = "input.DesaWisata == true || input.CRSWisata == true || input.LOGINSTATUS == '0'",
    fluidRow(
      box(
        title = "Pengaruh Dana Desa dan CSR Pada Wisata",
        status="primary",
        width = 12,
        plotOutput("barPlotProgramWisata"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Terdapat tempat wisata yang dikelola dengan menggunakan dana desa'),
            width = 6,
            status="success",
            textOutput("analysisPeranDanaDesaPadaWisata"),
          ),
          
          box(
            title = h4('Terdapat tempat wisata yang dikelola dengan menggunakan dana CSR'),
            width = 6,
            status="warning",
            textOutput("analysisPeranDanaCSRPadaWisata"),
          ),
        )
      )
    )
  ),
  tags$script(
    HTML(
      "
      Shiny.addCustomMessageHandler('form_update_false', function(message) {
        Shiny.setInputValue('form_update', '0');
      });
      Shiny.addCustomMessageHandler('selected_id_handler', function(message) {
        Shiny.setInputValue('selected_id', message);
      });
    "
    )
  ), 
  conditionalPanel(
    condition = "input.form_update == '1'", 
    fluidRow(
      box(
        title = h3(tags$b("Update Data")),
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        box(
          status = "success",
          width = 12,
          h4(tags$b("Peningkatan Program Wisata")),
          
          selectInput(
            "Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa.edit",
            "Terdapat tempat wisata yang dikelola dengan menggunakan dana desa :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          ), selectInput(
            "Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR.edit",
            "Terdapat tempat wisata yang dikelola dengan menggunakan dana CSR :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          )
        ),
        actionButton("updatePeningkatanWisata", "Update"),
        actionButton("cancelPeningkatanWisata", "Cancel")
      )
    )
  ),
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
    fluidRow(box(
      title = "Peningkatan Program Wisata",
      width = 12,
      DTOutput("data_table_PeningkatanProgramWisata")
    )),
  )
)