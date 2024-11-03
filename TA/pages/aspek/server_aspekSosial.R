render_server_aspek_sosial <- function(params) {
  
  pathTambahAspekSosial <- 'data/AspekSosial.csv'
  
  output$data_table_apekSosial <- renderDT({
    data <- read_csv(pathTambahAspekSosial)%>%
      mutate(
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = recode(
          Apakah.Anda.tinggal.di.desa.ini.sejak.lahir, 
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini = recode(
          Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa = recode(
          Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa,
          `4`='Sering',
          `3`='Kadang-kadang',
          `2`='Jarang',
          `1`='Tidak Pernah'
        ), 
        Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini = recode(
          Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
      )
    
    action_buttons <- if (!is.null(input$stored_user)) {
      paste0(
        '<button class="update-btn-aspek" data-id="', data$No, '">Update</button>',
        '<button class="delete-btn-aspek" data-id="', data$No, '">Delete</button>'
      )
    } else {
      '<span></span>'
    }
    
    data$Actions <- action_buttons
    
    datatable(
      data,
      selection = 'none',
      escape = FALSE,
      rownames = TRUE,
      colnames = c(
        "No",
        "ID",
        'Apakah Anda tinggal di desa ini sejak lahir?', 
        'Jika tidak, sebutkan asal tempat', 
        'Apakah Anda aktif dalam kegiatan sosial kemasyarakatan di desa ini?',
        'Seberapa sering Anda berpartisipasi dalam pertemuan warga atau komunitas di desa?', 
        'Apakah anda merasa adanya dukungan sosial yang memadai di desa ini?', 
        "Actions"
      ),
      options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  if (!$('#aspekSosial-checkbox').length) {",
          "  $(thead).closest('thead').prepend(` 
      <tr id=\"aspekSosial-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"tinggal-didesa\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"kegiatan-masyarakat\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pertemuan-masyarakat\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"dukungan-sosial\">
        </th> 
      </tr>
          `);",
          "  $('#tinggal-didesa').on('click', function(){",
          "    Shiny.setInputValue('tinggal_didesa', this.checked);",
          "  });",
          "  $('#kegiatan-masyarakat').on('click', function(){",
          "    Shiny.setInputValue('kegiatan_masyarakat', this.checked);",
          "  });",
          "  $('#pertemuan-masyarakat').on('click', function(){",
          "    Shiny.setInputValue('pertemuan_masyarakat', this.checked);",
          "  });",
          "  $('#asal-tempat').on('click', function(){",
          "    Shiny.setInputValue('asal_tempat', this.checked);",
          "  });",
          "  $('#dukungan-sosial').on('click', function(){",
          "    Shiny.setInputValue('dukungan_sosial', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-aspek-sosial', targets = 0),
          list(targets = 0, visible = TRUE),
          list(targets = ncol(data), orderable = FALSE, searchable = FALSE)
        ),
        select = list(style = 'multi', selector = 'td:first-child')
      )
    )%>%
      formatStyle(
        columns = c('Actions'),
        cursor = 'pointer'
      )
  })
  
  # Update Sosial
  loadDataAspekSosial <- function() {
    if (file.exists(pathTambahAspekSosial)) {
      read.csv(pathTambahAspekSosial, stringsAsFactors = FALSE)
    } else {
      data.frame(
        No = character(),
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = character(), Jika.tidak.sebutkan.asal.tempat =
          character(), Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini =
          character(), Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa =
          character(), Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini =
          character(), stringsAsFactors = FALSE
      )
    }
  }
  
  observeEvent(input$cancelAspekSosial, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataAspekSosial()
    data <- data[data$No == id, ]
    
    updateSelectInput(
      session,
      'Apakah.Anda.tinggal.di.desa.ini.sejak.lahir.edit',
      selected = data$Apakah.Anda.tinggal.di.desa.ini.sejak.lahir
    )	
    updateTextInput(
      session,
      'Jika.tidak.sebutkan.asal.tempat.edit',
      value = data$Jika.tidak.sebutkan.asal.tempat
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini.edit',
      selected = data$Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa.edit',
      selected = data$Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa
    )	
    updateSelectInput(
      session,
      'Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini.edit',
      selected = data$Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini
    )
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateAspekSosial, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataAspekSosial()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = input$Apakah.Anda.tinggal.di.desa.ini.sejak.lahir.edit, 
        Jika.tidak.sebutkan.asal.tempat =
          input$Jika.tidak.sebutkan.asal.tempat.edit, 
        Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini =
          input$Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini.edit, 
        Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa =
          input$Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa.edit, 
        Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini =
          input$Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini.edit, 
        stringsAsFactors = FALSE
      )
      
      saveData <- function(data, path) {
        tryCatch({
          write.csv(data, path, row.names = FALSE)
          TRUE
        }, error = function(e) {
          FALSE
        })
      }
      
      temp <- paste0(pathTambahAspekSosial, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathTambahAspekSosial)
        render_server_aspek_sosial(FALSE)
        removeModal()
        
        session$sendCustomMessage("form_update_aspek", "0")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        unlink(temp)
        
        removeModal()
        
        showModal(modalDialog(
          title = "Error",
          "Gagal memperbrui data.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    params = TRUE
  })
  
  output$barChartTinggalDidesa <- renderPlotly({
    data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = recode(
          Apakah.Anda.tinggal.di.desa.ini.sejak.lahir,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    # Pisahkan data yang 'Ya' untuk mengelola NA di 'Jika.tidak.sebutkan.asal.tempat'
    data_ya <- data %>%
      filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir == "Tidak" & !is.na(Jika.tidak.sebutkan.asal.tempat))
    
    # Gabungkan kembali data yang sudah di-filter
    data <- bind_rows(
      data_ya,
      data %>% filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir != "TIdak")
    )
    
    # Custom function to collapse strings with <br> every two items
    collapse_with_br <- function(vec) {
      n <- length(vec)
      if (n == 0) return("")
      collapsed <- c()
      for (i in seq(1, n, by = 2)) {
        if (i + 1 <= n) {
          collapsed <- c(collapsed, paste(vec[i], vec[i + 1], sep = ", "))
        } else {
          collapsed <- c(collapsed, vec[i])
        }
      }
      return(paste(collapsed, collapse = "<br>"))
    }
    
    data_summary <- data %>%
      group_by(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir) %>%
      summarise(
        count = n(),
        Jika.tidak.sebutkan.asal.tempat = collapse_with_br(na.omit(Jika.tidak.sebutkan.asal.tempat))
      )
    
    plot_ly(data_summary, labels = ~Apakah.Anda.tinggal.di.desa.ini.sejak.lahir, values = ~count, type = 'pie',
            hovertext = ~Jika.tidak.sebutkan.asal.tempat, hoverinfo = 'text+label+percent',
            textinfo = 'label+percent') %>%
      layout(hovermode = 'closest')
  })
  
  output$analysisTextTinggalDidesa <- renderText({
    modal_combined_data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = recode(
          Apakah.Anda.tinggal.di.desa.ini.sejak.lahir,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    total_responden <- nrow(modal_combined_data)
    
    
    tinggalDidesa <- modal_combined_data %>%
      count(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir) %>%
      mutate(percentage = n / sum(n) * 100)
    
    ya_count <- tinggalDidesa %>%
      filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir == "Ya") %>%
      pull(n)
    
    tidak_count <- tinggalDidesa %>%
      filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir == "Tidak") %>%
      pull(n)
    
    ya_percentage <- tinggalDidesa %>%
      filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir == "Ya") %>%
      pull(percentage)
    
    tidak_percentage <- tinggalDidesa %>%
      filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir == "Tidak") %>%
      pull(percentage)
    
    Ya <- modal_combined_data %>%
      filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir == "Ya") %>%
      nrow()
    
    Tidak <- modal_combined_data %>%
      filter(Apakah.Anda.tinggal.di.desa.ini.sejak.lahir == "Tidak") %>%
      nrow()
    
    AsalDesa <- modal_combined_data %>%
      filter(!is.na(modal_combined_data$Jika.tidak.sebutkan.asal.tempat))
    
    AsalDesa <- paste(AsalDesa$Jika.tidak.sebutkan.asal.tempat, collapse = ", ")
    
    paste0("Dari total ", total_responden," responden, Terdapat ", Ya, " orang (", round(ya_percentage, 1), "%) berasal dari luar desa atau tidak tinggal di desa sejak lahir, sedangkan ", Tidak, " orang (", round(tidak_percentage, 1)," berasal dari desa ini atau tinggal sejak mereka lahir. Dari hasil analisis didapatkan asal dari warga yang bukan asli desa atau tidak tinggal di desa sejak lahir adalah ", AsalDesa)
  })
  
  output$pieChartKegiatanKemasyrakatan <- renderPlot({
    
    data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini = recode(
          Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )%>%
      drop_na(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini)
    
    data <- data %>%
      count(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisKegiatanKemasyrakatan <- renderText({
    data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini = recode(
          Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    ya_responden <- data_summary %>% filter(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini == "Ya") %>% pull(n)
    ya_percentage <- data_summary %>% filter(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini == "Ya") %>% pull(percentage)
    
    tidak_responden <- data_summary %>% filter(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini == "Tidak") %>% pull(n)
    tidak_percentage <- data_summary %>% filter(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini == "Tidak") %>% pull(percentage)
    
    belum_tahu_responden <- data_summary %>% filter(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini == "Belum tahu") %>% pull(n)
    belum_tahu_percentage <- data_summary %>% filter(Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini == "Belum tahu") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ",
      ya_responden, " orang (", round(ya_percentage, 1), "%) menyatakan aktif dalam kegiatan sosial kemasyarakatan di desa ini. ",
      tidak_responden, " orang (", round(tidak_percentage, 1), "%) menyatakan tidak aktif. ",
      belum_tahu_responden, " orang (", round(belum_tahu_percentage, 1), "%) menyatakan belum tahu. "
    )
    
    # Additional insights based on the data
    if (ya_percentage > 50) {
      analisis <- paste0(analisis, "Ini menunjukkan bahwa mayoritas penduduk desa terlibat dalam kegiatan sosial kemasyarakatan, yang mengindikasikan adanya rasa komunitas yang kuat.")
    } else if (tidak_percentage > 50) {
      analisis <- paste0(analisis, "Ini menunjukkan bahwa sebagian besar penduduk desa tidak terlibat dalam kegiatan sosial kemasyarakatan, yang mungkin menunjukkan kurangnya minat atau kesempatan untuk berpartisipasi dalam kegiatan tersebut.")
    } else if (belum_tahu_percentage > 50) {
      analisis <- paste0(analisis, "Sebagian besar responden menyatakan belum tahu tentang keterlibatan mereka dalam kegiatan sosial kemasyarakatan, yang mungkin menunjukkan kurangnya informasi atau ketidakjelasan mengenai kegiatan tersebut.")
    } else {
      analisis <- paste0(analisis, "Distribusi keterlibatan dalam kegiatan sosial kemasyarakatan cukup merata, menunjukkan adanya variasi dalam tingkat partisipasi di antara penduduk desa.")
    }
    
    analisis
  })
  
  output$pieChartPertemuanDesa <- renderPlot({
    
    data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa = recode(
          Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa,
          `4`='Sering',
          `3`='Kadang-kadang',
          `2`='Jarang',
          `1`='Tidak Pernah'
        )
      )%>%
      drop_na(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa)
    
    data <- data %>%
      count(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisPertemuanDesa <- renderText({
    data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa = recode(
          Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa,
          `4`='Sering',
          `3`='Kadang-kadang',
          `2`='Jarang',
          `1`='Tidak Pernah'
        )
      )
    
    data_summary <- data %>%
      count(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    sering_responden <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Sering") %>% pull(n)
    sering_percentage <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Sering") %>% pull(percentage)
    
    kadang_responden <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Kadang-kadang") %>% pull(n)
    kadang_percentage <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Kadang-kadang") %>% pull(percentage)
    
    jarang_responden <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Jarang") %>% pull(n)
    jarang_percentage <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Jarang") %>% pull(percentage)
    
    tidak_pernah_responden <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Tidak Pernah") %>% pull(n)
    tidak_pernah_percentage <- data_summary %>% filter(Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa == "Tidak Pernah") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ",
      sering_responden, " orang (", round(sering_percentage, 1), "%) sering berpartisipasi dalam pertemuan warga atau komunitas di desa. ",
      kadang_responden, " orang (", round(kadang_percentage, 1), "%) kadang-kadang berpartisipasi. ",
      jarang_responden, " orang (", round(jarang_percentage, 1), "%) jarang berpartisipasi. ",
      tidak_pernah_responden, " orang (", round(tidak_pernah_percentage, 1), "%) tidak pernah berpartisipasi."
    )
    
    # Additional insights based on the data
    if (sering_percentage > 50) {
      analisis <- paste0(analisis, " Ini menunjukkan bahwa mayoritas penduduk desa aktif berpartisipasi dalam pertemuan warga atau komunitas, yang mencerminkan adanya rasa komunitas yang kuat.")
    } else if (tidak_pernah_percentage > 50) {
      analisis <- paste0(analisis, " Ini menunjukkan bahwa sebagian besar penduduk desa tidak pernah berpartisipasi dalam pertemuan warga atau komunitas, yang mungkin menunjukkan kurangnya minat atau kesempatan untuk berpartisipasi dalam kegiatan tersebut.")
    } else if (kadang_percentage > 50) {
      analisis <- paste0(analisis, " Sebagian besar responden kadang-kadang berpartisipasi dalam pertemuan warga atau komunitas, yang menunjukkan bahwa meskipun ada keterlibatan, itu tidak konsisten.")
    } else {
      analisis <- paste0(analisis, " Distribusi partisipasi dalam pertemuan warga atau komunitas cukup merata, menunjukkan adanya variasi dalam tingkat keterlibatan di antara penduduk desa.")
    }
    
    analisis
  })
  
  output$pieChartDukunganSOsial <- renderPlot({
    
    data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini = recode(
          Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )%>%
      drop_na(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini)
    
    data <- data %>%
      count(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisDukunganSOsial <- renderText({
    data <- read_csv(pathTambahAspekSosial) %>%
      mutate(
        Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini = recode(
          Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    ya_responden <- data_summary %>% filter(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini == "Ya") %>% pull(n)
    ya_percentage <- data_summary %>% filter(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini == "Ya") %>% pull(percentage)
    
    tidak_responden <- data_summary %>% filter(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini == "Tidak") %>% pull(n)
    tidak_percentage <- data_summary %>% filter(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini == "Tidak") %>% pull(percentage)
    
    belum_tahu_responden <- data_summary %>% filter(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini == "Belum tahu") %>% pull(n)
    belum_tahu_percentage <- data_summary %>% filter(Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini == "Belum tahu") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ",
      ya_responden, " orang (", round(ya_percentage, 1), "%) merasa terdapat dukungan sosial yang memadai di desa ini. ",
      tidak_responden, " orang (", round(tidak_percentage, 1), "%) merasa tidak terdapat dukungan sosial yang memadai. ",
      belum_tahu_responden, " orang (", round(belum_tahu_percentage, 1), "%) belum tahu apakah terdapat dukungan sosial yang memadai atau tidak."
    )
    
    # Additional insights based on the data
    if (ya_percentage > 50) {
      analisis <- paste0(analisis, " Ini menunjukkan bahwa mayoritas penduduk desa merasa adanya dukungan sosial yang memadai, yang mencerminkan adanya ikatan sosial yang kuat dan kemungkinan keberhasilan program sosial di desa.")
    } else if (tidak_percentage > 50) {
      analisis <- paste0(analisis, " Ini menunjukkan bahwa sebagian besar penduduk desa merasa tidak adanya dukungan sosial yang memadai, yang mungkin menunjukkan kebutuhan untuk peningkatan program dan kegiatan sosial di desa.")
    } else if (belum_tahu_percentage > 50) {
      analisis <- paste0(analisis, " Sebagian besar responden belum tahu apakah terdapat dukungan sosial yang memadai atau tidak, yang menunjukkan kemungkinan kurangnya informasi atau kesadaran mengenai program sosial yang ada.")
    } else {
      analisis <- paste0(analisis, " Distribusi perasaan mengenai adanya dukungan sosial yang memadai cukup merata, menunjukkan adanya variasi dalam persepsi penduduk terhadap dukungan sosial di desa.")
    }
    
    analisis
  })
  
}

render_server_aspek_sosial(TRUE)