tabItemPeningkatanPAD <- tabItem(
  tabName = "PeningkatanPAD",
  conditionalPanel(
    condition = "input.DesaPembagunan == true || input.LOGINSTATUS == '0' || input.DesaInfrastruktur == true || input.DesaBumdes == true",
    fluidRow(
      box(
        title = "Pengaruh Dana Desa",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPADDanaDesa"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Dana desa digunakan untuk membentuk kegiatan pembangunan desa (termasuk membangun usaha)'),
            width = 4,
            status="success",
            textOutput("analysisDanaKegiatanPembangunanDesa"),
          ),
          
          box(
            title = h4('Dana desa digunakan untuk membangun Infrastruktur desa (misalnya : jalan)'),
            width = 4,
            status="warning",
            textOutput("analysisDanaPembagunanInfrastruktur"),
          ),
          
          box(
            title = h4('Dana desa membantu permodalan bagi kegiatan BUMDes'),
            width = 4,
            status="danger",
            textOutput("analysisDanaPemodalanBUMDes"),
          )
        )
      )
    ),
  ),
  conditionalPanel(
    condition = "input.CSRPembangunan == true || input.LOGINSTATUS == '0' || input.CSRInfrastruktur == true || input.CSRBumdes == true",
    fluidRow(
      box(
        title = "Pengaruh Dana CSR",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPADCSR"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Dana CSR digunakan untuk membentuk kegiatan pembangunan desa (termasuk membangun usaha)'),
            width = 4,
            status="success",
            textOutput("analysisDanaCSRKegiatanPembangunanDesa"),
          ),
          
          box(
            title = h4('Dana CSR digunakan untuk membangun Infrastruktur desa (misalnya : jalan)'),
            width = 4,
            status="warning",
            textOutput("analysisDanaCSRPembagunanInfra"),
          ),
          
          box(
            title = h4('Dana CSR membantu permodalan bagi kegiatan BUMDes'),
            width = 4,
            status="danger",
            textOutput("analysisDanaCSRPemodalanBUMDes"),
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
          h4(tags$b("Peningkata PAD")),
          selectInput("Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha.edit", "Dana desa digunakan untuk membentuk kegiatan pembangunan desa (termasuk membangun usaha) :",
                      c(
                        "Berperan"="3",
                        "Cukup berperan"="2",
                        "Kurang berperan"="1"
                      ),
          ),
          selectInput("Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan.edit", "Dana desa digunakan untuk membangun Infrastruktur desa (misalnya : jalan)  :",
                      c(
                        "Berperan"="3",
                        "Cukup berperan"="2",
                        "Kurang berperan"="1"
                      ),
          ),
          selectInput("Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes.edit", "Dana desa membantu permodalan bagi kegiatan BUMDes :",
                      c(
                        "Berperan"="3",
                        "Cukup berperan"="2",
                        "Kurang berperan"="1"
                      ),
          ),
          selectInput("Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.edit", "Dana CSR digunakan untuk membentuk kegiatan pembangunan desa :",
                      c(
                        "Berperan"="3",
                        "Cukup berperan"="2",
                        "Kurang berperan"="1"
                      ),
          ),
          selectInput("Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan.edit", "Dana CSR digunakan untuk membangun Infrastruktur desa (misalnya : jalan)  :",
                      c(
                        "Berperan"="3",
                        "Cukup berperan"="2",
                        "Kurang berperan"="1"
                      ),
          ),
          selectInput("Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes.edit", "Dana CSR membantu permodalan bagi kegiatan BUMDes :",
                      c(
                        "Berperan"="3",
                        "Cukup berperan"="2",
                        "Kurang berperan"="1"
                      ),
          )
        ),
        actionButton("updatePeningkatanPAD", "Update"),
        actionButton("cancelPeningkatanPAD", "Cancel")
      )
    )
  ),
  conditionalPanel(
    condition = "input.LOGINSTATUS !== '0'",
    fluidRow(box(
      title = "Peningkatan PAD",
      width = 12,
      DTOutput("data_table_PeningkatanPAD")
    )),
  )
)