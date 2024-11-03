render_server_profil_desa <- function(params) {
  pathTambahProfilDesa <- "data/profil_desa.csv"
  data_profil_desa <- read_csv("data/profil_desa.csv")
  print(data_profil_desa)
  
  output$selamatDatang <- renderText({
    selamatDatang <- paste("Di ", data_profil_desa$nama, ", ", data_profil_desa$alamat)
    selamatDatang
  })
  
  output$sejarahDesa <- renderText({
    sejarahDesa <- paste(data_profil_desa$sejarah)
    sejarahDesa
  })
  
  output$batasTimur <- renderText({
    batasTimur <- paste( data_profil_desa$timur)
    batasTimur
  })
  
  output$batasSelatan <- renderText({
    batasSelatan <- paste( data_profil_desa$selatan)
    batasSelatan
  })
  
  output$batasUtara <- renderText({
    batasUtara <- paste( data_profil_desa$utara)
    batasUtara
  })
  
  output$batasBarat <- renderText({
    batasBarat <- paste(data_profil_desa$barat)
    batasBarat
  })
  
  output$lakiPerempuan <- renderPlot({
    df <- data.frame(
      Gender = c("Laki-laki", "Perempuan"),
      Count = c(data_profil_desa$laki.laki, data_profil_desa$perempuan)
    )
    
    ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "red")) +
      theme_minimal() +
      labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
  })
  
  output$kepalaKeluarga <- renderText({
    kepalaKeluarga <- paste(data_profil_desa$kepala.keluarga, "Kepala Keluarga")
    kepalaKeluarga
  })
  
  output$kepalaDesa <- renderText({
    kepalaDesa <- paste(data_profil_desa$kepala.desa)
    kepalaDesa
  })
  
  output$fotoKepalaDesa <- renderUI({
    fotoKepalaDesa <- data_profil_desa$foto.kepala.desa
    img(src = fotoKepalaDesa, width = "100%")
  })
  
  
  resetInputs <- function() {
    updateTextInput(session,"nama", value="")	
    updateTextInput(session,"alamat", value="")	
    updateTextInput(session,"timur", value="")	
    updateTextInput(session,"selatan", value="")	
    updateTextInput(session,"barat", value="")	
    updateTextInput(session,"utara", value="")	
    updateNumericInput(session,"laki.laki", value=0)	
    updateNumericInput(session,"perempuan", value=0)	
    updateNumericInput(session,"kepala.keluarga", value=0)	
    updateTextInput(session,"sejarah", value="")	
    updateTextInput(session,"kepala.desa", value="")
  }
  
  loadDataProfilDesa <- function() {
    if (file.exists(pathTambahProfilDesa)) {
      read.csv(pathTambahProfilDesa)
    } else {
      data.frame(
        No = character(),
        nama=character(),	
        alamat=character(),	
        timur=character(),	
        selatan=character(),	
        barat=character(),	
        utara=character(),	
        laki.laki=character(),	
        perempuan=character(),	
        kepala.keluarga=character(),	
        sejarah=character(),	
        kepala.desa=character(),	
        foto.kepala.desa=character()
      )
    }
  }
  
  observeEvent(input$openFormProfilDesa, {
    id <- 1
    data_profil_desa <- loadDataProfilDesa()
    data_profil_desa <- data_profil_desa[data_profil_desa$No == id, ]
    updateTextInput(session,"nama", value=data_profil_desa$nama)	
    updateTextInput(session,"alamat", value=data_profil_desa$alamat)	
    updateTextInput(session,"timur", value=data_profil_desa$timur)	
    updateTextInput(session,"selatan", value=data_profil_desa$selatan)	
    updateTextInput(session,"barat", value=data_profil_desa$barat)	
    updateTextInput(session,"utara", value=data_profil_desa$utara)	
    updateNumericInput(session,"laki.laki", value=data_profil_desa$laki.laki)	
    updateNumericInput(session,"perempuan", value=data_profil_desa$perempuan)	
    updateNumericInput(session,"kepala.keluarga", value=data_profil_desa$kepala.keluarga)	
    updateTextInput(session,"sejarah", value=data_profil_desa$sejarah)	
    updateTextInput(session,"kepala.desa", value=data_profil_desa$kepala.desa)
    
    session$sendCustomMessage("form_profil_desa_update", "1")
    shinyjs::hide("openFormProfilDesa")
  })
  observeEvent(input$cancelProfilDesa, {
    session$sendCustomMessage("form_profil_desa_update", "0")
    shinyjs::show("openFormProfilDesa")
  })
  
  safeWriteCSV <- function(data, path) {
    tryCatch({
      write.csv(data, path, row.names = FALSE)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }
  
  observeEvent(input$updateProfilDesa, {
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data_profil_desa <- loadDataProfilDesa()
      
      file_info <- input$foto.kepala.desa
      file_name <- data_profil_desa$foto.kepala.desa
      if (!is.null(file_info)){
        file_path <- file_info$datapath
        file_name <- file_info$name
        
        save_path <- file.path("www", file_name)
        
        file.copy(file_path, save_path)
      }
      
      data_profil_desa[data_profil_desa$No == "1", ] <- data.frame(
        No = "1",
        nama=input$nama,	
        alamat=input$alamat,	
        timur=input$timur,	
        selatan=input$selatan,	
        barat=input$barat,	
        utara=input$utara,	
        laki.laki=input$laki.laki,	
        perempuan=input$perempuan,	
        kepala.keluarga=input$kepala.keluarga,	
        sejarah=input$sejarah,	
        kepala.desa=input$kepala.desa,	
        foto.kepala.desa=file_name,
        stringsAsFactors = FALSE
      )
      
      successProfilDesa <- safeWriteCSV(data_profil_desa, paste0(pathTambahProfilDesa, ".tmp"))
      
      if (successProfilDesa){
        file.rename(paste0(pathTambahProfilDesa, ".tmp"), pathTambahProfilDesa)
        resetInputs()
        
        session$sendCustomMessage("form_profil_desa_update", "0")
        shinyjs::show("openFormProfilDesa")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
        render_server_profil_desa(FALSE)
      }else {
        
        unlink(paste0(pathTambahProfilDesa, ".tmp"))
        
        removeModal()
        
        showModal(modalDialog(
          title = "Error",
          "Gagal menambahkan data.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    params = TRUE
  })
}

render_server_profil_desa(TRUE)