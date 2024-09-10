render_server_peningkatan_perekonomian <- function(params) {
  
  pathTambahPeningkatanPerekonomian <- "data/PeningkatanPerekonomianDesa.csv"
  pathPerekonomian <- read_csv("data/PeningkatanPerekonomianDesa.csv")
  
  output$data_table_PeningkatanPerekonomian <- renderDT({
    data <- pathPerekonomian
    
    data <- data %>%
      mutate(
        Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa = recode(Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa,
                                                                         `3`='Berperan',
                                                                         `2`='Cukup Berperan',
                                                                         `1`='Kurang Berperan',
        ),
        Dana.desa.menambah.penghasilan.masyarakat = recode(Dana.desa.menambah.penghasilan.masyarakat,
                                                           `3`='Berperan',
                                                           `2`='Cukup Berperan',
                                                           `1`='Kurang Berperan',
        ),
        Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat = recode(Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat,
                                                                            `3`='Berperan',
                                                                            `2`='Cukup Berperan',
                                                                            `1`='Kurang Berperan',
        ),
        Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR = recode(Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR,
                                                                        `3`='Berperan',
                                                                        `2`='Cukup Berperan',
                                                                        `1`='Kurang Berperan',
        ),
        Dana.CSR.menambah.penghasilan.masyarakat = recode(Dana.CSR.menambah.penghasilan.masyarakat,
                                                          `3`='Berperan',
                                                          `2`='Cukup Berperan',
                                                          `1`='Kurang Berperan',
        ),
        Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat = recode(Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat,
                                                                           `3`='Berperan',
                                                                           `2`='Cukup Berperan',
                                                                           `1`='Kurang Berperan',
        )
      )
    data <- data %>%
      rename(
        `Terbukanya usaha ekonomi rakyat, karena adanya dana desa`=Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa,
        `Dana desa menambah penghasilan masyarakat`=Dana.desa.menambah.penghasilan.masyarakat,
        `Adanya Dana desa membantu mengembangkan modal untuk rakyat`=Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat,
        `Terbukanya usaha ekonomi rakyat, karena adanya dana CSR`=Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR,
        `Dana CSR menambah penghasilan masyarakat`=Dana.CSR.menambah.penghasilan.masyarakat,
        `Adanya Dana CSR membantu mengembangkan modal untuk rakyat`=Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat,
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
        "  if (!$('#peningkatanPerekonomian-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"peningkatanPerekonomian-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DesaEkonomi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DesaPengahasilan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DesaModal\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CSREkonomi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CSRPenghasilan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CSRModal\">
        </th>
      </tr>`);",
        "  $('#DesaEkonomi').on('click', function(){",
        "    Shiny.setInputValue('DesaEkonomi', this.checked);",
        "  });",
        "  $('#DesaPengahasilan').on('click', function(){",
        "    Shiny.setInputValue('DesaPengahasilan', this.checked);",
        "  });",
        "  $('#DesaModal').on('click', function(){",
        "    Shiny.setInputValue('DesaModal', this.checked);",
        "  });",
        "  $('#CSREkonomi').on('click', function(){",
        "    Shiny.setInputValue('CSREkonomi', this.checked);",
        "  });",
        "  $('#CSRPenghasilan').on('click', function(){",
        "    Shiny.setInputValue('CSRPenghasilan', this.checked);",
        "  });",
        "  $('#CSRModal').on('click', function(){",
        "    Shiny.setInputValue('CSRModal', this.checked);",
        "  });",
        "  }",
        "}"
      ),
      columnDefs = list(
        list(orderable = FALSE, className = 'select-checkbox-peningkatan-perekonomian', targets = 0),
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
  output$barPlotPeningkatanPerekonomianDanaDesa <- renderPlot({
    dana_data <- pathPerekonomian %>%
      rename(
        `Peran Dana Desa Untuk Usaha Rakyat` = Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa,
        `Peran Dana Desa Dalam \nMenambah Penghasilan Rakyat` = Dana.desa.menambah.penghasilan.masyarakat,
        `Peran Dana Desa Untuk Modal Rakyat` = Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat
      ) %>%
      mutate(
        `Peran Dana Desa Untuk Usaha Rakyat` = recode(`Peran Dana Desa Untuk Usaha Rakyat`,
                                                      `3` = "Berperan",
                                                      `2` = "Cukup Berperan",
                                                      `1` = "Kurang Berperan"),
        `Peran Dana Desa Dalam \nMenambah Penghasilan Rakyat` = recode(`Peran Dana Desa Dalam \nMenambah Penghasilan Rakyat`,
                                                                       `3` = "Berperan",
                                                                       `2` = "Cukup Berperan",
                                                                       `1` = "Kurang Berperan"),
        `Peran Dana Desa Untuk Modal Rakyat` = recode(`Peran Dana Desa Untuk Modal Rakyat`,
                                                      `3` = "Berperan",
                                                      `2` = "Cukup Berperan",
                                                      `1` = "Kurang Berperan")
      )
    
    # Convert to long format
    dana_long <- dana_data %>%
      pivot_longer(cols = c(`Peran Dana Desa Untuk Usaha Rakyat`, `Peran Dana Desa Dalam \nMenambah Penghasilan Rakyat`, `Peran Dana Desa Untuk Modal Rakyat`),
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
  
  output$analysisPeranDanaDesaUntukUsahaRakyat <- renderText({
    # Membaca data
    dana_data <- pathPerekonomian %>%
      select(`Peran Dana Desa Untuk Usaha Rakyat` = Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa) %>%
      mutate(`Peran Dana Desa Untuk Usaha Rakyat` = recode(`Peran Dana Desa Untuk Usaha Rakyat`,
                                                           `3` = "Berperan",
                                                           `2` = "Cukup Berperan",
                                                           `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(`Peran Dana Desa Untuk Usaha Rakyat`) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(`Peran Dana Desa Untuk Usaha Rakyat` == category) %>%
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
      pull(`Peran Dana Desa Untuk Usaha Rakyat`)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa dalam mendukung terbuknya usaha ekonomi masyarakat."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa dalam mendukung terbuknya usaha ekonomi masyarakat."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa dalam mendukung terbuknya usaha ekonomi masyarakat."
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
  
  output$analysisPeranDanaDesaDalamMenambahPenghasilanRakyat <- renderText({
    # Membaca data
    dana_data <- pathPerekonomian %>%
      select(PeranDanaDesaDalamMenambahPenghasilanRakyat = Dana.desa.menambah.penghasilan.masyarakat) %>%
      mutate(PeranDanaDesaDalamMenambahPenghasilanRakyat = recode(PeranDanaDesaDalamMenambahPenghasilanRakyat,
                                                                  `3` = "Berperan",
                                                                  `2` = "Cukup Berperan",
                                                                  `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(PeranDanaDesaDalamMenambahPenghasilanRakyat) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(PeranDanaDesaDalamMenambahPenghasilanRakyat == category) %>%
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
      pull(PeranDanaDesaDalamMenambahPenghasilanRakyat)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa dalam menambah penghasilan masyarakat."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa dalam menambah penghasilan masyarakat."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa dalam menambah penghasilan masyarakat."
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
  
  output$analysisPeranDanaDesaUntukModalRakyat <- renderText({
    # Membaca data
    dana_data <- pathPerekonomian %>%
      select(PeranDanaDesaUntukModalRakyat = Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat) %>%
      mutate(PeranDanaDesaUntukModalRakyat = recode(PeranDanaDesaUntukModalRakyat,
                                                    `3` = "Berperan",
                                                    `2` = "Cukup Berperan",
                                                    `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(PeranDanaDesaUntukModalRakyat) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(PeranDanaDesaUntukModalRakyat == category) %>%
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
      pull(PeranDanaDesaUntukModalRakyat)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana desa untuk membantu mengembangkang modal untuk rakyat."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana desa untuk membantu mengembangkang modal untuk rakyat."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana desa untuk membantu mengembangkang modal untuk rakyat."
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
  output$barPlotPeningkatanPerekonomianCSR <- renderPlot({
    
    dana_data <- pathPerekonomian %>%
      rename(
        `Terbukanya usaha ekonomi rakyat, \nkarena adanya dana CSR` = Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR,
        `Dana CSR menambah penghasilan masyarakat` = Dana.CSR.menambah.penghasilan.masyarakat,
        `Adanya Dana CSR membantu \nmengembangkan modal untuk rakyat` = Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat,
      )%>%
      mutate(
        `Terbukanya usaha ekonomi rakyat, \nkarena adanya dana CSR` = recode(`Terbukanya usaha ekonomi rakyat, \nkarena adanya dana CSR`,
                                                                             `3`='Berperan',
                                                                             `2`='Cukup Berperan',
                                                                             `1`='Kurang Berperan',
        ),
        `Dana CSR menambah penghasilan masyarakat` = recode(`Dana CSR menambah penghasilan masyarakat`,
                                                            `3`='Berperan',
                                                            `2`='Cukup Berperan',
                                                            `1`='Kurang Berperan',
        ),
        `Adanya Dana CSR membantu \nmengembangkan modal untuk rakyat` = recode(`Adanya Dana CSR membantu \nmengembangkan modal untuk rakyat`,
                                                                               `3`='Berperan',
                                                                               `2`='Cukup Berperan',
                                                                               `1`='Kurang Berperan',
        ),
      )
    
    dana_long <- dana_data %>%
      pivot_longer(cols = c(`Terbukanya usaha ekonomi rakyat, \nkarena adanya dana CSR`, `Dana CSR menambah penghasilan masyarakat`, `Adanya Dana CSR membantu \nmengembangkan modal untuk rakyat`),
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
  
  output$analysisPeranDanaCSRUntukUsahaRakyat <- renderText({
    # Membaca data
    dana_data <- pathPerekonomian %>%
      select(PeranDanaCSRUntukUsahaRakyat = Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR) %>%
      mutate(PeranDanaCSRUntukUsahaRakyat = recode(PeranDanaCSRUntukUsahaRakyat,
                                                   `3` = "Berperan",
                                                   `2` = "Cukup Berperan",
                                                   `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(PeranDanaCSRUntukUsahaRakyat) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(PeranDanaCSRUntukUsahaRakyat == category) %>%
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
      pull(PeranDanaCSRUntukUsahaRakyat)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan danadana CSR dalam menambah penghasilan masyarakat."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan danadana CSR dalam menambah penghasilan masyarakat."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan danadana CSR dalam menambah penghasilan masyarakat."
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
  
  output$analysisPeranDanaCSRDalamMenambahPenghasilanRakyat <- renderText({
    # Membaca data
    dana_data <- pathPerekonomian %>%
      select(PeranDanaCSRDalamMenambahPenghasilanRakyat = Dana.CSR.menambah.penghasilan.masyarakat) %>%
      mutate(PeranDanaCSRDalamMenambahPenghasilanRakyat = recode(PeranDanaCSRDalamMenambahPenghasilanRakyat,
                                                                 `3` = "Berperan",
                                                                 `2` = "Cukup Berperan",
                                                                 `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(PeranDanaCSRDalamMenambahPenghasilanRakyat) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(PeranDanaCSRDalamMenambahPenghasilanRakyat == category) %>%
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
      pull(PeranDanaCSRDalamMenambahPenghasilanRakyat)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana CSR dalam menambah penghasilan masyarakat."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana CSR dalam menambah penghasilan masyarakat."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana CSR dalam menambah penghasilan masyarakat."
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
  
  output$analysisPeranDanaCSRUntukModalRakyat <- renderText({
    # Membaca data
    dana_data <- pathPerekonomian %>%
      select(PeranDanaCSRUntukModalRakyat = Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat) %>%
      mutate(PeranDanaCSRUntukModalRakyat = recode(PeranDanaCSRUntukModalRakyat,
                                                   `3` = "Berperan",
                                                   `2` = "Cukup Berperan",
                                                   `1` = "Kurang Berperan"))
    
    # Menghitung jumlah dan persentase
    analysis <- dana_data %>%
      count(PeranDanaCSRUntukModalRakyat) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Fungsi untuk mendapatkan nilai dengan penanganan NA
    get_value <- function(df, category, column) {
      value <- df %>%
        filter(PeranDanaCSRUntukModalRakyat == category) %>%
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
      pull(PeranDanaCSRUntukModalRakyat)
    
    highest_percentage <- max(berperan_percentage, cukup_berperan_percentage, kurang_berperan_percentage)
    
    # Menyusun kesimpulan dan saran
    if (highest == "Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Berperan\" tentang penggunaan dana CSR untuk membantu mengembangkang modal untuk rakyat."
      solution <- "Pemerintah desa dapat melanjutkan dan meningkatkan program agar hasilnya lebih maksimal."
      if(cukup_berperan_count > 0 || kurang_berperan_count > 0){
        solution <- paste(solution, " Karena masih terdapat masyarakat yang merespon Cukup Berperan ataupun Kurang Berperan")
      }
    } else if (highest == "Cukup Berperan") {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Cukup Berperan\" tentang penggunaan dana CSR untuk membantu mengembangkang modal untuk rakyat."
      solution <- "Pemerintah desa harus melakukan evaluasi dan meningkatkan program agar hasilnya lebih maksimal."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat memberikan respon \"Kurang Berperan\" tentang penggunaan dana CSR untuk membantu mengembangkang modal untuk rakyat."
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
    data_peningkatanPerekonomian <- loadDataPeningkatanPerekonomian()
    data_peningkatanPerekonomian <- data_peningkatanPerekonomian[data_peningkatanPerekonomian$No == id, ]
    
    updateSelectInput(session,"Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa.edit", selected = data_peningkatanPerekonomian$Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa)
    updateSelectInput(session,"Dana.desa.menambah.penghasilan.masyarakat.edit", selected = data_peningkatanPerekonomian$Dana.desa.menambah.penghasilan.masyarakat)	
    updateSelectInput(session,"Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat.edit", selected = data_peningkatanPerekonomian$Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat)
    updateSelectInput(session,"Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR.edit", selected = data_peningkatanPerekonomian$Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR)
    updateSelectInput(session,"Dana.CSR.menambah.penghasilan.masyarakat.edit", selected = data_peningkatanPerekonomian$Dana.CSR.menambah.penghasilan.masyarakat)	
    updateSelectInput(session,"Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat.edit", selected = data_peningkatanPerekonomian$Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat)
    
    session$sendCustomMessage("selected_id_handler", id)
    
  })
  
  observeEvent(input$cancelPeningkatanPerekonomian, {
    session$sendCustomMessage("form_update_false", "0")
  })
  
  observeEvent(input$updatePeningkatanPerekonomian, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data_peningkatanPerekonomian <- loadDataPeningkatanPerekonomian()
      data_peningkatanPerekonomian[data_peningkatanPerekonomian$No == input$selected_id, ] <- data.frame(
        No = input$selected_id,
        Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa = input$Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa.edit, 
        Dana.desa.menambah.penghasilan.masyarakat =
          input$Dana.desa.menambah.penghasilan.masyarakat.edit, 
        Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat =
          input$Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat.edit, 
        Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR =
          input$Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR.edit, 
        Dana.CSR.menambah.penghasilan.masyarakat =
          input$Dana.CSR.menambah.penghasilan.masyarakat.edit, 
        Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat =
          input$Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat.edit,
        stringsAsFactors = FALSE
      )
      
      successPeningkatanPerekonomian <- safeWriteCSV(data_peningkatanPerekonomian, paste0(pathTambahPeningkatanPerekonomian, ".tmp"))
      
      
      if (successPeningkatanPerekonomian){
        file.rename(paste0(pathTambahPeningkatanPerekonomian, ".tmp"), pathTambahPeningkatanPerekonomian)
        
        resetInputs()
        render_server_peningkatan_perekonomian(FALSE)
        
        session$sendCustomMessage("form_update_false", "0")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        unlink(paste0(pathTambahPeningkatanPerekonomian, ".tmp"))
        
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

render_server_peningkatan_perekonomian(TRUE)