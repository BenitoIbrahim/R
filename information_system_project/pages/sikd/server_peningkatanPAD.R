render_server_peningkatan_PAD <- function(params) {
  pathTambahPeningkatanPAD <- "data/PeningkatanPAD.csv"
  pathPAD <- read_csv("data/PeningkatanPAD.csv")
  
  output$data_table_PeningkatanPAD <- renderDT({
    data <- pathPAD
    
    data <- data %>%
      mutate(
        Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha = recode(Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha, 
                                                                                                        `3`='Berperan',
                                                                                                        `2`='Cukup Berperan',
                                                                                                        `1`='Kurang Berperan',
        ),
        Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan = recode(Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan, 
                                                                                       `3`='Berperan',
                                                                                       `2`='Cukup Berperan',
                                                                                       `1`='Kurang Berperan',
        ),
        Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes = recode(Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes, 
                                                                    `3`='Berperan',
                                                                    `2`='Cukup Berperan',
                                                                    `1`='Kurang Berperan',
        ),
        Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa = recode(Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa, 
                                                                              `3`='Berperan',
                                                                              `2`='Cukup Berperan',
                                                                              `1`='Kurang Berperan',
        ),
        Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan = recode(Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan, 
                                                                                      `3`='Berperan',
                                                                                      `2`='Cukup Berperan',
                                                                                      `1`='Kurang Berperan',
        ),
        Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes = recode(Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes, 
                                                                   `3`='Berperan',
                                                                   `2`='Cukup Berperan',
                                                                   `1`='Kurang Berperan',
        ),
      )
    
    data <- data %>%
      rename(
        `Dana desa digunakan untuk membentuk kegiatan pembangunan desa (termasuk membangun usaha)` = Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha,
        `Dana desa digunakan untuk membangun Infrastruktur desa (misalnya : jalan) ` = Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan,
        `Dana desa membantu permodalan bagi kegiatan BUMDes` = Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes,
        `Dana CSR digunakan untuk membentuk kegiatan pembangunan desa` = Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa,
        `Dana CSR digunakan untuk membangun Infrastruktur desa (misalnya : jalan) ` = Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan,
        `Dana CSR membantu permodalan bagi kegiatan BUMDes` = Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes
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
    
    datatable(data, selection = 'none',escape = FALSE, rownames = TRUE, colnames = c('No' = 1), options = list(
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  if (!$('#peningkatanPAD-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"peningkatanPAD-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DesaPembagunan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DesaInfrastruktur\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DesaBumdes\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CSRPembangunan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CSRInfrastruktur\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CSRBumdes\">
        </th>
      </tr>`);",
        "  $('#DesaPembagunan').on('click', function(){",
        "    Shiny.setInputValue('DesaPembagunan', this.checked);",
        "  });",
        "  $('#DesaInfrastruktur').on('click', function(){",
        "    Shiny.setInputValue('DesaInfrastruktur', this.checked);",
        "  });",
        "  $('#DesaBumdes').on('click', function(){",
        "    Shiny.setInputValue('DesaBumdes', this.checked);",
        "  });",
        "  $('#CSRPembangunan').on('click', function(){",
        "    Shiny.setInputValue('CSRPembangunan', this.checked);",
        "  });",
        "  $('#CSRInfrastruktur').on('click', function(){",
        "    Shiny.setInputValue('CSRInfrastruktur', this.checked);",
        "  });",
        "  $('#CSRBumdes').on('click', function(){",
        "    Shiny.setInputValue('CSRBumdes', this.checked);",
        "  });",
        "  }",
        "}"
      ),
      columnDefs = list(
        list(orderable = FALSE, className = 'select-checkbox-peningkatanpad', targets = 0),
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
  
  # Dana Desa
  output$barPlotPeningkatanPADDanaDesa <- renderPlot({
    dana_data <- pathPAD %>%
      rename(
        `Dana Kegiatan Pembangunan Desa` = Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha,
        `Dana Pembagunan Infrastruktur` = Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan,
        `Dana desa membantu permodalan \nbagi kegiatan BUMDes` = Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes
      ) %>%
      mutate(
        `Dana Kegiatan Pembangunan Desa` = recode(`Dana Kegiatan Pembangunan Desa`,
                                                  `3` = "Berperan",
                                                  `2` = "Cukup Berperan",
                                                  `1` = "Kurang Berperan"),
        `Dana Pembagunan Infrastruktur` = recode(`Dana Pembagunan Infrastruktur`,
                                                 `3` = "Berperan",
                                                 `2` = "Cukup Berperan",
                                                 `1` = "Kurang Berperan"),
        `Dana desa membantu permodalan \nbagi kegiatan BUMDes` = recode(`Dana desa membantu permodalan \nbagi kegiatan BUMDes`,
                                                                        `3` = "Berperan",
                                                                        `2` = "Cukup Berperan",
                                                                        `1` = "Kurang Berperan")
      )
    
    # Convert to long format
    dana_long <- dana_data %>%
      pivot_longer(cols = c(`Dana Kegiatan Pembangunan Desa`, `Dana Pembagunan Infrastruktur`, `Dana desa membantu permodalan \nbagi kegiatan BUMDes`),
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
  
  output$analysisDanaKegiatanPembangunanDesa <- renderText({
    # Membaca data
    dana_data <- pathPAD %>%
      select(`Dana Kegiatan Pembangunan Desa` = Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha) %>%
      mutate(`Dana Kegiatan Pembangunan Desa` = recode(`Dana Kegiatan Pembangunan Desa`,
                                                       `3` = "Berperan",
                                                       `2` = "Cukup Berperan",
                                                       `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(`Dana Kegiatan Pembangunan Desa`) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(`Dana Kegiatan Pembangunan Desa` == category) %>%
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
      pull(`Dana Kegiatan Pembangunan Desa`)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa untuk membentuk kegiatan pembangunan desa."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa untuk membentuk kegiatan pembangunan desa."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa untuk membentuk kegiatan pembangunan desa."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  output$analysisDanaPembagunanInfrastruktur <- renderText({
    # Membaca data
    dana_data <- pathPAD %>%
      select(`Dana Pembagunan Infrastruktur` = Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan,) %>%
      mutate(`Dana Pembagunan Infrastruktur` = recode(`Dana Pembagunan Infrastruktur`,
                                                      `3` = "Berperan",
                                                      `2` = "Cukup Berperan",
                                                      `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(`Dana Pembagunan Infrastruktur`) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(`Dana Pembagunan Infrastruktur` == category) %>%
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
      pull(`Dana Pembagunan Infrastruktur`)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa untuk pembangunan infrastruktur desa"
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa untuk pembangunan infrastruktur desa"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa untuk pembangunan infrastruktur desa"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  output$analysisDanaPemodalanBUMDes <- renderText({
    # Membaca data
    dana_data <- pathPAD %>%
      select(`Dana desa membantu permodalan \nbagi kegiatan BUMDes` = Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes) %>%
      mutate(`Dana desa membantu permodalan \nbagi kegiatan BUMDes` = recode(`Dana desa membantu permodalan \nbagi kegiatan BUMDes`,
                                                                             `3` = "Berperan",
                                                                             `2` = "Cukup Berperan",
                                                                             `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(`Dana desa membantu permodalan \nbagi kegiatan BUMDes`) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(`Dana desa membantu permodalan \nbagi kegiatan BUMDes` == category) %>%
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
      pull(`Dana desa membantu permodalan \nbagi kegiatan BUMDes`)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    print(highest)
    
    # paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
    # paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
    # paste("Kurang Berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa untuk membantu permodalan bagi kegiatan BUMDes"
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa untuk membantu permodalan bagi kegiatan BUMDes"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa untuk membantu permodalan bagi kegiatan BUMDes"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  # CSR
  output$barPlotPeningkatanPADCSR <- renderPlot({
    dana_data <- pathPAD %>%
      rename(
        `Dana Kegiatan Pembangunan Desa` = Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa,
        `Dana Pembagunan Infrastruktur` = Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan,
        `Dana Untuk Membantu Permodalan BUMDes` = Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes
      ) %>%
      mutate(
        `Dana Kegiatan Pembangunan Desa` = recode(`Dana Kegiatan Pembangunan Desa`,
                                                  `3` = "Berperan",
                                                  `2` = "Cukup Berperan",
                                                  `1` = "Kurang Berperan"),
        `Dana Pembagunan Infrastruktur` = recode(`Dana Pembagunan Infrastruktur`,
                                                 `3` = "Berperan",
                                                 `2` = "Cukup Berperan",
                                                 `1` = "Kurang Berperan"),
        `Dana Untuk Membantu Permodalan BUMDes` = recode(`Dana Untuk Membantu Permodalan BUMDes`,
                                                         `3` = "Berperan",
                                                         `2` = "Cukup Berperan",
                                                         `1` = "Kurang Berperan")
      )
    
    # Convert to long format
    dana_long <- dana_data %>%
      pivot_longer(cols = c(`Dana Kegiatan Pembangunan Desa`, `Dana Pembagunan Infrastruktur`, `Dana Untuk Membantu Permodalan BUMDes`),
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
      labs(title = "Pengaruh Dana CSR pada Kegiatan Pembangunan Desa",
           x = "Variabel",
           y = "Jumlah Respon",
           fill = "Pengaruh") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(dana_count$count, na.rm = TRUE), by = 1)) # Adding manual breaks for y-axis
  })
  
  output$analysisDanaCSRKegiatanPembangunanDesa <- renderText({
    # Membaca data
    dana_data <- pathPAD %>%
      select(`Dana Kegiatan Pembangunan Desa` = Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa) %>%
      mutate(`Dana Kegiatan Pembangunan Desa` = recode(`Dana Kegiatan Pembangunan Desa`,
                                                       `3` = "Berperan",
                                                       `2` = "Cukup Berperan",
                                                       `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(`Dana Kegiatan Pembangunan Desa`) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(`Dana Kegiatan Pembangunan Desa` == category) %>%
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
      pull(`Dana Kegiatan Pembangunan Desa`)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana CSR untuk membentuk kegiatan pembangunan desa."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana CSR untuk membentuk kegiatan pembangunan desa."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana CSR untuk membentuk kegiatan pembangunan desa."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  output$analysisDanaCSRPembagunanInfra <- renderText({
    # Membaca data
    dana_data <- pathPAD %>%
      select(`Dana Pembagunan Infrastruktur` = Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan,) %>%
      mutate(`Dana Pembagunan Infrastruktur` = recode(`Dana Pembagunan Infrastruktur`,
                                                      `3` = "Berperan",
                                                      `2` = "Cukup Berperan",
                                                      `1` = "Kurang Berperan"))
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(`Dana Pembagunan Infrastruktur`) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(`Dana Pembagunan Infrastruktur` == category) %>%
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
      pull(`Dana Pembagunan Infrastruktur`)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa untuk pembangunan infrastruktur desa"
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa untuk pembangunan infrastruktur desa"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa untuk pembangunan infrastruktur desa"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  output$analysisDanaCSRPemodalanBUMDes <- renderText({
    # Membaca data
    dana_data <- pathPAD %>%
      select(`Dana desa membantu permodalan \nbagi kegiatan BUMDes` = Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes) %>%
      mutate(`Dana desa membantu permodalan \nbagi kegiatan BUMDes` = recode(`Dana desa membantu permodalan \nbagi kegiatan BUMDes`,
                                                                             `3` = "Berperan",
                                                                             `2` = "Cukup Berperan",
                                                                             `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(`Dana desa membantu permodalan \nbagi kegiatan BUMDes`) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(`Dana desa membantu permodalan \nbagi kegiatan BUMDes` == category) %>%
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
      pull(`Dana desa membantu permodalan \nbagi kegiatan BUMDes`)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa untuk membantu permodalan bagi kegiatan BUMDes"
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa untuk membantu permodalan bagi kegiatan BUMDes"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa untuk membantu permodalan bagi kegiatan BUMDes"
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    }
    
    # Menyusun hasil analisis
    analysis_text <- paste(
      paste("Berperan:", berperan_count, "orang (", round(berperan_percentage, 1), "%), "),
      paste("Cukup Berperan:", cukup_berperan_count, "orang (", round(cukup_berperan_percentage, 1), "%), "),
      paste("Kurang berperan:", kurang_berperan_count, "orang (", round(kurang_berperan_percentage, 1), "%)"),
      ".\n\n",
      conclusion,
      solution,
      sep = "\n"
    )
    
    return(analysis_text)
  })
  observeEvent(input$update_id, {
    id <- as.integer(input$update_id)
    data_peningkatanPAD <- loadDataPeningkatanPAD()
    data_peningkatanPAD <- data_peningkatanPAD[data_peningkatanPAD$No == id, ]
    
    updateSelectInput(session, "Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha.edit", selected = data_peningkatanPAD$Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha)
    updateSelectInput(session, "Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan.edit", selected = data_peningkatanPAD$Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan)
    updateSelectInput(session, "Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes.edit", selected = data_peningkatanPAD$Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes)
    updateSelectInput(session, "Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.edit", selected = data_peningkatanPAD$Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa)
    updateSelectInput(session, "Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan.edit", selected = data_peningkatanPAD$Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan)
    updateSelectInput(session, "Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes.edit", selected = data_peningkatanPAD$Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes)
    
    
    session$sendCustomMessage("selected_id_handler", id)
    
  })
  
  observeEvent(input$cancelPeningkatanPAD, {
    session$sendCustomMessage("form_update_false", "0")
  })
  
  observeEvent(input$updatePeningkatanPAD, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data_peningkatanPAD <- loadDataPeningkatanPAD()
      data_peningkatanPAD[data_peningkatanPAD$No == input$selected_id, ] <- data.frame(
        No = input$selected_id,
        Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha =
          input$Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha.edit, 
        Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan =
          input$Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan.edit, 
        Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes =
          input$Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes.edit, 
        Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa =
          input$Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.edit, 
        Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan =
          input$Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan.edit, 
        Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes =
          input$Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes.edit, 
        stringsAsFactors = FALSE
      )
      
      successPeningkatanPAD <- safeWriteCSV(data_peningkatanPAD, paste0(pathTambahPeningkatanPAD, ".tmp"))
      if (successPeningkatanPAD){
        file.rename(paste0(pathTambahPeningkatanPAD, ".tmp"), pathTambahPeningkatanPAD)
        
        resetInputs()
        render_server_peningkatan_PAD(FALSE)
        
        session$sendCustomMessage("form_update_false", "0")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {        
        unlink(paste0(pathTambahPeningkatanPAD, ".tmp"))

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

render_server_peningkatan_PAD(TRUE)