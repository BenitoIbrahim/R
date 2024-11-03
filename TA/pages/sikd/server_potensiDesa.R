render_server_potensi_desa <- function(params) {
  
  pathTambahPotensiDesa <- "data/PotensiDesa.csv"
  pathTambahGaleriPotensiDesa <- "data/PotensiDesaURL.csv"
  
  pathPotensiDesa <- read_csv("data/PotensiDesa.csv")
  output$data_table_PotensiDesa <- renderDT({
    data <- pathPotensiDesa%>%
      rename(
        `Jenis potensi` = Jenis.potensi,
        `Jumlah satuan` = Jumlah.satuan
      )
    
    action_buttons <- if (!is.null(input$stored_user)) {
      paste0(
        '<button class="update-btn" data-id="', data$No, '">Update</button>',
        '<button class="delete-btn" data-id="', data$No, '">Delete</button>'
      )
    } else {
      '<span></span>'
    }
    data$Actions <- action_buttons
    
    data <- data%>%
      rename(
        ID = No
      )
    
    datatable(data, selection = 'none', escape = FALSE, rownames = TRUE, colnames = c('No' = 1), options = list(
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  if (!$('#potensiDesa-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"potensiDesa-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"Bidang\">
        </th>
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>
      </tr>`);",
        "  $('#JenisPotensi').on('click', function(){",
        "    Shiny.setInputValue('JenisPotensi', this.checked);",
        "  });",
        "  $('#Bidang').on('click', function(){",
        "    Shiny.setInputValue('Bidang', this.checked);",
        "  });",
        "  $('#JumlahSatuan').on('click', function(){",
        "    Shiny.setInputValue('JumlahSatuan', this.checked);",
        "  });",
        "  }",
        "}"
      ),
      columnDefs = list(
        list(orderable = FALSE, className = 'select-checkbox-peningkatan-program-wisata', targets = 0),
        list(targets = 0, visible = TRUE),
        list(targets = ncol(data), orderable = FALSE, searchable = FALSE)
      ),
      select = list(style = 'multi', selector = 'td:first-child'),
      scrollX = TRUE
    ))%>%
      formatStyle(
        columns = c('Actions'),
        cursor = 'pointer'
      )
  })
  
  output$pieChartPotensiDesa <- renderPlotly({
    data <- pathPotensiDesa 
    
    data_summary <- data %>%
      group_by(Bidang) %>%
      summarise(count = n(),
                Jenis.potensi = paste(Jenis.potensi, collapse = ", "))
    
    plot_ly(data_summary, labels = ~Bidang, values = ~count, type = 'pie',
            hovertext = ~Jenis.potensi, hoverinfo = 'text+label+percent',
            textinfo = 'label+percent') %>%
      layout(title = 'Persentase Setiap Sektor Bidang',
             hovermode = 'closest')
  })
  
  observeEvent(input$update_id, {
    id <- as.integer(input$update_id)
    data_potensi_desa <- loadDataPotensiDesa()
    data_potensi_desa <- data_potensi_desa[data_potensi_desa$No == id, ]
    
    
    updateTextInput(session,"Jenis.potensi.edit", value = data_potensi_desa$Jenis.potensi)	
    updateTextInput(session,"Bidang.edit", value = data_potensi_desa$Bidang)	
    updateTextInput(session,"Jumlah.satuan.edit", value = data_potensi_desa$Jumlah.satuan)
    
    session$sendCustomMessage("selected_id_handler", id)
    
  })
  
  observeEvent(input$cancelPotensiDesa, {
    session$sendCustomMessage("form_update_false", "0")
  })
  
  observeEvent(input$updatePotensiDesa, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data_potensi_desa <- loadDataPotensiDesa()
      data_potensi_desa[data_potensi_desa$No == input$selected_id, ] <- data.frame(
        No = input$selected_id,
        Jenis.potensi=input$Jenis.potensi.edit,	
        Bidang=input$Bidang.edit,	
        Jumlah.satuan=input$Jumlah.satuan.edit,
        stringsAsFactors = FALSE
      )
      
      successPotensiDesa <- safeWriteCSV(data_potensi_desa, paste0(pathTambahPotensiDesa, ".tmp"))
      if (successPotensiDesa){
        file.rename(paste0(pathTambahPotensiDesa, ".tmp"), pathTambahPotensiDesa)
        resetInputs()
        render_server_potensi_desa(FALSE)
        
        session$sendCustomMessage("form_update_false", "0")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        
        unlink(paste0(pathTambahPotensiDesa, ".tmp"))
        
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
  
  observeEvent(input$cancelGaleriPotensiDesa, {
    session$sendCustomMessage("form_update_galeri", "0")
  })
  
  observeEvent(input$update_galeri_id, {
    id <- as.integer(input$update_galeri_id)
    data <- loadDataGaleriPotensiWisata()
    
    data <- data[data$No == id, ]
    session$sendCustomMessage("selected_id_galeri", id)
  })
  
  observeEvent(input$updateGaleriPotensiDesa, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataGaleriPotensiWisata()
      
      file_info <- input$Galeri.url.edit
      file_name <- data$Galeri.url
      if (!is.null(file_info)){
        file_path <- file_info$datapath
        file_name <- file_info$name
        
        save_path <- file.path("www", file_name)
        
        file.copy(file_path, save_path)
      }
      
      data <- loadDataGaleriPotensiWisata()
      data[data$No == input$selected_id_galeri, ] <- data.frame(
        No = input$selected_id_galeri,
        Galeri.url= file_name,
        stringsAsFactors = FALSE
      )
      
      successGaleriPotensiDesa <- safeWriteCSV(data, paste0(pathTambahGaleriPotensiDesa, ".tmp"))
      if (successGaleriPotensiDesa){
        file.rename(paste0(pathTambahGaleriPotensiDesa, ".tmp"), pathTambahGaleriPotensiDesa)
        resetInputs()
        render_server_potensi_desa(FALSE)
        
        session$sendCustomMessage("form_update_galeri", "0")
        session$sendCustomMessage("resetFileInput", "Galeri.url.edit")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        
        unlink(paste0(pathTambahGaleriPotensiDesa, ".tmp"))
        
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
  
  output$gallery <- renderUI({
    urls <- read.csv(pathTambahGaleriPotensiDesa)
    
    # Simpan tombol tindakan dalam variabel action_buttons
    action_buttons <- if (!is.null(input$stored_user)) {
      lapply(seq_along(urls$No), function(i) {
        paste0(
          '<button class="update-btn-galeri" data-id="', urls$No[i], '">Update</button>',
          '<button class="delete-btn-galeri" data-id="', urls$No[i], '">Delete</button>'
        )
      })
    } else {
      rep('<span></span>', length(urls$No))
    }
    
    img_tags <- lapply(seq_along(urls$Galeri.url), function(i) {
      tags$div(
        tags$img(src = urls$Galeri.url[i], style = "max-height:300px;margin:10px;"),
        HTML(action_buttons[[i]])
      )
    })
    
    do.call(tagList, img_tags)
  })
  
}

render_server_potensi_desa(TRUE)