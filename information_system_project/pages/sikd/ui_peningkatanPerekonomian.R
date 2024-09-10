tabItemPeningkatanPerekonomian <- tabItem(
  tabName = "PeningkatanPerekonomian",
  conditionalPanel(
    condition = "input.DesaEkonomi == true || input.DesaPengahasilan == true || input.DesaModal == true || input.LOGINSTATUS == '0'",
    fluidRow(
      box(
        title = "Pengaruh Dana Desa",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPerekonomianDanaDesa"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Terbukanya usaha ekonomi rakyat, karena adanya dana desa'),
            width = 4,
            status="success",
            textOutput("analysisPeranDanaDesaUntukUsahaRakyat"),
          ),
          
          box(
            title = h4('Dana desa menambah penghasilan masyarakat'),
            width = 4,
            status="warning",
            textOutput("analysisPeranDanaDesaDalamMenambahPenghasilanRakyat"),
          ),
          
          box(
            title = h4('Adanya Dana desa membantu mengembangkan modal untuk rakyat'),
            width = 4,
            status="danger",
            textOutput("analysisPeranDanaDesaUntukModalRakyat"),
          )
        )
      )
    )
  ),
  conditionalPanel(
    condition = "input.CSREkonomi == true || input.CSRPenghasilan == true || input.CSRModal == true || input.LOGINSTATUS == '0'",
    fluidRow(
      box(
        title = "Pengaruh Dana CSR",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPerekonomianCSR"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Terbukanya usaha ekonomi rakyat, karena adanya dana CSR'),
            width = 4,
            status="success",
            textOutput("analysisPeranDanaCSRUntukUsahaRakyat"),
          ),
          
          box(
            title = h4('Dana CSR menambah penghasilan masyarakat'),
            width = 4,
            status="warning",
            textOutput("analysisPeranDanaCSRDalamMenambahPenghasilanRakyat"),
          ),
          
          box(
            title = h4('Adanya Dana CSR membantu mengembangkan modal untuk rakyat'),
            width = 4,
            status="danger",
            textOutput("analysisPeranDanaCSRUntukModalRakyat"),
          )
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
          h4(tags$b("Peningkatan Perekonomian")),
          
          selectInput(
            "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa.edit",
            "Terbukanya usaha ekonomi rakyat, karena adanya dana desa :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          ), 
          selectInput(
            "Dana.desa.menambah.penghasilan.masyarakat.edit",
            "Dana desa menambah penghasilan masyarakat :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          ), 
          selectInput(
            "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat.edit",
            "Adanya Dana desa membantu mengembangkan modal untuk rakyat :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          ), 
          selectInput(
            "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR.edit",
            "Terbukanya usaha ekonomi rakyat, karena adanya dana CSR :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          ), 
          selectInput(
            "Dana.CSR.menambah.penghasilan.masyarakat.edit",
            "Dana CSR menambah penghasilan masyarakat :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          ), 
          selectInput(
            "Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat.edit",
            "Adanya Dana CSR membantu mengembangkan modal untuk rakyat :"
            ,
            c(
              "Berperan" = "3",
              "Cukup berperan" = "2",
              "Kurang berperan" = "1"
            ),
          )
        ),
        actionButton("updatePeningkatanPerekonomian", "Update"),
        actionButton("cancelPeningkatanPerekonomian", "Cancel")
      )
    )
  ),
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
    fluidRow(box(
      title = "Peningkatan Perekonomian Desa",
      width = 12,
      DTOutput("data_table_PeningkatanPerekonomian")
    )),
  )
)