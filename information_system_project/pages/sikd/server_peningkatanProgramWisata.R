render_server_peningkatan_wisata <- function(params) {
  pathTambahPeningkatanProgramWisata <- "data/PeningkatanProgramWisata.csv"
  pathPeningkatanProgramWisata <- read_csv("data/PeningkatanProgramWisata.csv")
  
  output$data_table_PeningkatanProgramWisata <- renderDT({
    data <- pathPeningkatanProgramWisata
    
    data <- data %>%
      mutate(
        Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa = recode(Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa,
                                                                                   `3`='Berperan',
                                                                                   `2`='Cukup Berperan',
                                                                                   `1`='Kurang Berperan',),
        Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR = recode(Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR, 
                                                                                  `3`='Berperan',
                                                                                  `2`='Cukup Berperan',
                                                                                  `1`='Kurang Berperan',)
      )
    
    data <- data %>%
      rename(
        `Terdapat tempat wisata yang dikelola dengan menggunakan dana desa` = Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa,
        `Terdapat tempat wisata yang dikelola dengan menggunakan dana CSR` = Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR
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
        "  if (!$('#peningkatanProgramWisata-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"peningkatanProgramWisata-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DesaWisata\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CRSWisata\">
        </th>
      </tr>`);",
        "  $('#DesaWisata').on('click', function(){",
        "    Shiny.setInputValue('DesaWisata', this.checked);",
        "  });",
        "  $('#CRSWisata').on('click', function(){",
        "    Shiny.setInputValue('CRSWisata', this.checked);",
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
  
  
  output$barPlotProgramWisata <- renderPlot({
    dana_data <- pathPeningkatanProgramWisata %>%
      rename(
        `Terdapat tempat wisata yang \ndikelola dengan menggunakan dana desa` = Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa,
        `Terdapat tempat wisata yang \ndikelola dengan menggunakan dana CSR` = Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR
      ) %>%
      mutate(
        `Terdapat tempat wisata yang \ndikelola dengan menggunakan dana desa` = recode(`Terdapat tempat wisata yang \ndikelola dengan menggunakan dana desa`,
                                                                                       `3` = "Berperan",
                                                                                       `2` = "Cukup Berperan",
                                                                                       `1` = "Kurang Berperan"),
        `Terdapat tempat wisata yang \ndikelola dengan menggunakan dana CSR` = recode(`Terdapat tempat wisata yang \ndikelola dengan menggunakan dana CSR`,
                                                                                      `3` = "Berperan",
                                                                                      `2` = "Cukup Berperan",
                                                                                      `1` = "Kurang Berperan")
      )
    
    # Convert to long format
    dana_long <- dana_data %>%
      pivot_longer(cols = c(`Terdapat tempat wisata yang \ndikelola dengan menggunakan dana desa`, `Terdapat tempat wisata yang \ndikelola dengan menggunakan dana CSR`),
                   names_to = "Variable",
                   values_to = "Score")
    
    # Group and count data
    dana_count <- dana_long %>%
      group_by(Variable, Score) %>%
      summarise(count = n(), .groups = 'drop')
    
    # Define number of bars and colors
    num_bars <- nrow(dana_count)
    colors <- brewer.pal(min(num_bars, 3), "Dark2")
    
    # Plot the data
    ggplot(dana_count, aes(x = Variable, y = count, fill = Score)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Pengaruh Dana Desa pada Kegiatan Pembangunan Desa",
           x = "Variabel",
           y = "Jumlah Respon",
           fill = "Pengaruh") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(dana_count$count, na.rm = TRUE), by = 1)) # Adding manual breaks for y-axis
  })
  
  output$analysisPeranDanaDesaPadaWisata <- renderText({
    # Membaca data
    dana_data <- pathPeningkatanProgramWisata %>%
      select(PeranDanaDesaPadaWisata = Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa) %>%
      mutate(PeranDanaDesaPadaWisata = recode(PeranDanaDesaPadaWisata,
                                              `3` = "Berperan",
                                              `2` = "Cukup Berperan",
                                              `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(PeranDanaDesaPadaWisata) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(PeranDanaDesaPadaWisata == category) %>%
        pull({{ column }})
      if (length(value) == 0) return(0)
      return(value)
    }
    
    # Mendapatkan jumlah dan persentase masing-masing kategori
    berperan_count <- get_value(analysis, "Berperan", n)
    cukup_berperan_count <- get_value(analysis, "Cukup Berperan", n)
    kurang_berperan_count <- get_value(analysis, "Kurang Berperan", n)
    
    berperan_percentage <- get_value(analysis, "Berperan", percentage)
    cukup_berperan_percentage <- get_value(analysis, "Cukup Berperan", percentage)
    kurang_berperan_percentage <- get_value(analysis, "Kurang Berperan", percentage)
    
    # Menentukan kategori dengan persentase tertinggi
    highest <- analysis %>%
      filter(percentage == max(percentage)) %>%
      slice(1) %>%
      pull(PeranDanaDesaPadaWisata)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa mengelola tempat wisata."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa mengelola tempat wisata."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa mengelola tempat wisata."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang Berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  output$analysisPeranDanaCSRPadaWisata <- renderText({
    # Membaca data
    dana_data <- pathPeningkatanProgramWisata %>%
      select(PeranDanaCSRPadaWisata = Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR) %>%
      mutate(PeranDanaCSRPadaWisata = recode(PeranDanaCSRPadaWisata,
                                             `3` = "Berperan",
                                             `2` = "Cukup Berperan",
                                             `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(PeranDanaCSRPadaWisata) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(PeranDanaCSRPadaWisata == category) %>%
        pull({{ column }})
      if (length(value) == 0) return(0)
      return(value)
    }
    
    # Mendapatkan jumlah dan persentase masing-masing kategori
    berperan_count <- get_value(analysis, "Berperan", n)
    cukup_berperan_count <- get_value(analysis, "Cukup Berperan", n)
    kurang_berperan_count <- get_value(analysis, "Kurang Berperan", n)
    
    berperan_percentage <- get_value(analysis, "Berperan", percentage)
    cukup_berperan_percentage <- get_value(analysis, "Cukup Berperan", percentage)
    kurang_berperan_percentage <- get_value(analysis, "Kurang Berperan", percentage)
    
    # Menentukan kategori dengan persentase tertinggi
    highest <- analysis %>%
      filter(percentage == max(percentage)) %>%
      slice(1) %>%
      pull(PeranDanaCSRPadaWisata)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana CSR untuk menglola tempat wisata."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana CSR untuk menglola tempat wisata."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana CSR untuk menglola tempat wisata."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang Berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  observeEvent(input$update_id, {
    id <- as.integer(input$update_id)
    data_peningkatanProgramWisata <- loadDataPeningkatanProgramWisata()
    data_peningkatanProgramWisata <- data_peningkatanProgramWisata[data_peningkatanProgramWisata$No == id, ]
    
    updateSelectInput(session,"Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa.edit", selected = data_peningkatanProgramWisata$Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa)
    updateSelectInput(session,"Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR.edit", selected = data_peningkatanProgramWisata$Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR)
    
    session$sendCustomMessage("selected_id_handler", id)
    
  })
  
  observeEvent(input$cancelPeningkatanWisata, {
    session$sendCustomMessage("form_update_false", "0")
  })
  
  observeEvent(input$updatePeningkatanWisata, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data_peningkatanProgramWisata <- loadDataPeningkatanProgramWisata()
      data_peningkatanProgramWisata[data_peningkatanProgramWisata$No == input$selected_id, ] <- data.frame(
        No = input$selected_id,
        Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa=input$Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa.edit,	
        Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR=input$Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR.edit,
        stringsAsFactors = FALSE
      )
      
      successPeningkatanProgramWisata <- safeWriteCSV(data_peningkatanProgramWisata, paste0(pathTambahPeningkatanProgramWisata, ".tmp"))
      
      if (successPeningkatanProgramWisata){
        file.rename(paste0(pathTambahPeningkatanProgramWisata, ".tmp"), pathTambahPeningkatanProgramWisata)
        
        resetInputs()
        render_server_peningkatan_wisata(FALSE)
        
        session$sendCustomMessage("form_update_false", "0")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        unlink(paste0(pathTambahPeningkatanProgramWisata, ".tmp"))
        
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

render_server_peningkatan_wisata(TRUE)