tabItemProfilDesa <- tabItem(tabName = "dashboard",
                             tags$script(
                               HTML(
                                 "
      Shiny.addCustomMessageHandler('form_profil_desa_update', function(message) {
        Shiny.setInputValue('form_profil_desa_update', message);
      });
    "
                               )
                             ), 
                             conditionalPanel(
                               condition = "input.form_profil_desa_update == '1'", 
                               fluidRow(
                                 box(
                                   title = h3(tags$b("Update Profil Desa")),
                                   status = "primary",
                                   width = 12,
                                   solidHeader = TRUE,
                                   box(
                                     status = "success",
                                     width = 12,
                                     h4(tags$b("Potensi Desa")),
                                     
                                     textInput("nama", "Nama"),	
                                     textInput("kepala.desa", "Kepala Desa"),	
                                     fileInput("foto.kepala.desa", "Foto Kepala Desa", 
                                               accept = c("image/png", "image/jpeg", "image/jpg")),
                                     textInput("alamat", "Alamat"),
                                     textInput("utara", "Utara"),	
                                     textInput("timur", "Timur"),	
                                     textInput("selatan", "Selatan"),	
                                     textInput("barat", "Barat"),	
                                     numericInput("laki.laki", "Jumlah Laki-Laki", value = 0),	
                                     numericInput("perempuan", "Jumlah Perempuan", value = 0),	
                                     numericInput("kepala.keluarga", "Jumlah Kepala Keluarga", value = 0),	
                                     textInput("sejarah", "Sejarah")
                                     
                                   ),
                                   actionButton("updateProfilDesa", "Update"),
                                   actionButton("cancelProfilDesa", "Cancel")
                                 )
                               )
                             ),
                             actionButton("openFormProfilDesa", "Perbarui Profil Desa", style = "margin-bottom: 10px;display:none;"),
                             fluidRow(box(
                               solidHeader = TRUE,
                               status = "primary",
                               width = 12,
                               h2(tags$b("Selamat Datang")),
                               h2(tags$b(textOutput("selamatDatang"))),
                             ), ),
                             fluidRow(
                               box(
                                 solidHeader = TRUE,
                                 status = "warning",
                                 h3(tags$b("Sejarah Desa")),
                                 h4(tags$p(textOutput("sejarahDesa")))
                               ),
                               box(
                                 solidHeader = TRUE,
                                 status = "danger",
                                 h3(tags$b("Batas Wilayah Desa")),
                                 h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara"))),
                                 h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur"))),
                                 h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan"))),
                                 h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat"))),
                               )
                             ),
                             fluidRow(box(
                               title = h3(tags$b("Persebaran Laki-laki dan Perempuan")),
                               solidHeader = TRUE,
                               width = 12,
                               box(
                                 solidHeader = TRUE,
                                 width = 2,
                                 h4(
                                   tags$b("Total Kepala Keluarga : ") ,
                                   textOutput("kepalaKeluarga")
                                 )
                               ),
                               fluidRow(
                                 box(
                                   solidHeader = TRUE,
                                   width = 4,
                                   plotOutput("lakiPerempuan")
                                 ),
                                 box(
                                   solidHeader = TRUE,
                                   width = 4,
                                   h4(tags$b("Kepala Desa Saat Ini : ")),
                                   uiOutput("fotoKepalaDesa"),
                                   div(h4(tags$b(textOutput("kepalaDesa"))), style = "text-align: center;"),
                                   
                                 )
                               )
                             )))