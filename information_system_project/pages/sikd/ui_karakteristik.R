tabItemKarakteristik <- tabItem(tabName = "karakteristik",
                                tags$script(HTML("
                                  Shiny.addCustomMessageHandler('form_update_false', function(message) {
                                    Shiny.setInputValue('form_update', '0');
                                  });
                                  Shiny.addCustomMessageHandler('selected_id_handler', function(message) {
                                    Shiny.setInputValue('selected_id', message);
                                  });
                                ")),
                                fluidRow(
                                  conditionalPanel(
                                    condition = "input.jenis_kelamin == true || input.LOGINSTATUS == '0'",
                                    box(title = "Jenis Kelamin", width = 6, 
                                        plotOutput("pie_chart_jenis_kelamin"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.pendidikan == true || input.LOGINSTATUS == '0' ",
                                    box(title = "Pendidikan", width = 6,
                                        plotOutput("pie_chart_pendidikan"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.show_age_chart == true || input.LOGINSTATUS == '0'",
                                    box(id = "age_bar_chart_box", title = "Distribusi Usia", width = 12, 
                                        plotOutput("age_bar_chart"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.pekerjaan_utama == true || input.LOGINSTATUS == '0'",
                                    box(title = "Pekerjaan Utama", width = 12, 
                                        plotOutput("bar_chart_pekerjaan_utama"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.pekerjaan_sampingan == true || input.LOGINSTATUS == '0'",
                                    box(title = "Pekerjaan Sampingan", width = 6, 
                                        plotOutput("bar_chart_pekerjaan_sampingan"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.memulai_usaha == true || input.LOGINSTATUS == '0'",
                                    box(title = "Memulai Usaha", width = 6, 
                                        plotOutput("bar_chart_memulai_usaha"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.jenis_usaha == true || input.LOGINSTATUS == '0'",
                                    box(title = "Jenis Usaha", width = 6, 
                                        plotOutput("bar_chart_jenis_usaha"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.skala_usaha == true || input.LOGINSTATUS == '0'",
                                    box(title = "Skala Usaha", width = 6, 
                                        plotOutput("pie_chart_skala_usaha"))
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
                                      
                                      # karakteristik
                                      box(
                                        status = "primary",
                                        width = 12,
                                        h4(tags$b("Karakteristik")),
                                        textInput("Nama.edit", "Nama"),
                                        selectInput("jenis.kelamin.edit", "Jenis Kelamin :",
                                                    c(
                                                      "Perempuan"="1",
                                                      "Laki-laki"="2"
                                                    ),
                                        ),
                                        textInput("Usia.edit", "Usia"),
                                        selectInput("Pendidikan.edit", "Pendidikan :",
                                                    c(
                                                      "SD"="1",
                                                      "SMP"="2",
                                                      "SMA"="3",
                                                      "Diploma"="4",
                                                      "S1"="5",
                                                      "S2"="6",
                                                      "S3"="7"
                                                    ),
                                        ),
                                        textInput("Pekerjaan.Utama.edit", "Pekerjaan Utama"),
                                        textInput("Pekerjaan.Sampingan.edit", "Pekerjaan Sampingan"),
                                        textInput("Memulai.Usaha.edit", "Memulai Usaha"),
                                        textInput("Jenis.Usaha.edit", "Jenis Usaha"),
                                        selectInput("skala.usaha.edit", "Skala Usaha :",
                                                    c(
                                                      "Mikro/home industry"="1",
                                                      "Kecil"="2",
                                                      "Menengah"="3",
                                                      "Besar"="4"
                                                    ),
                                        ),
                                      ),
                                      actionButton("updateKarakteristik", "Update"),
                                      actionButton("cancelKarakteristik", "Cancel")
                                    )
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.LOGINSTATUS !== '0'",
                                  fluidRow(
                                    box(title = "Karakteristik Desa", width = 12, 
                                        DTOutput("data_table")
                                    )
                                  )
                                )
)