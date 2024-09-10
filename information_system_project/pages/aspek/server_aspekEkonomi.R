render_server_aspek_ekonomi <- function(params) {
  pathTambahAspekEkonomi <- "data/AspekEkonomi.csv"
  
  output$data_table_apekEkonomi <- renderDT({
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini= recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah= recode(
          Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah,`1`='500.000 - 1.500.000',
          `2`='1.500.001 - 3.000.000',
          `3`='3.000.001 - 4.500.000',
          `4`='4.500.001 - ke atas'
        )
        ,	
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial= recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini= recode(
          Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini,
          `4`='Sangat stabil',
          `3`='Stabil',
          `2`='Tidak stabil',
          `1`='Sangat tidak stabil'
        ),	
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda= recode(
          Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini= recode(
          Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini= recode(
          Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini,
          `4`='Sangat terbuka',
          `3`='Terbuka',
          `2`='Tidak terbuka',
          `1`='Sangat tidak terbuka'
        ),	
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini= recode(
          Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini,
          `4`='Sangat terasa',
          `3`='Terasa',
          `2`='Tidak terasa',
          `1`='Sangat tidak terasa'
        )
        
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
        'No',
        'ID',
        'Apakah Anda atau anggota keluarga Anda memiliki usaha sendiri di desa ini? ',	
        'Berapa pendapatan bulanan Anda atau anggota keluarga Anda secara keseluruhan? (Kisaran jumlah)',	
        'Apakah Anda atau anggota keluarga Anda menerima bantuan sosial?',	
        'Jika ya, sebutkan',	
        'Bagaimana Anda menilai stabilitas harga pangan saat ini?',	
        'Apakah Anda memiliki simpanan atau investasi di luar penghasilan bulanan Anda?',	
        'Apakah Anda merasa terdapat peluang ekonomi yang cukup di desa ini?',	
        'Bagaimana Anda menilai aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini?',	
        'Apakah Anda merasa terdapat kesenjangan ekonomi di antara penduduk di desa ini?',
        'Actions'
      ),
      options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  if (!$('#aspekEkonomi-checkbox').length) {",
          "  $(thead).closest('thead').prepend(` 
      <tr id=\"aspekEkonomi-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"memiliki-usaha\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pendapatan-bulanan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"bantuan-sosial\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"stabilitas-harga\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"simpanan-investasi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"peluang-ekonomi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"ketersedian-pekerjaan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"kesenjangan-ekonomi\">
        </th> 
      </tr>
          `);",
          "  $('#memiliki-usaha').on('click', function(){",
          "    Shiny.setInputValue('memiliki_usaha', this.checked);",
          "  });",
          "  $('#pendapatan-bulanan').on('click', function(){",
          "    Shiny.setInputValue('pendapatan_bulanan', this.checked);",
          "  });",
          "  $('#bantuan-sosial').on('click', function(){",
          "    Shiny.setInputValue('bantuan_sosial', this.checked);",
          "  });",
          "  $('#sebutkan-bantuan').on('click', function(){",
          "    Shiny.setInputValue('sebutkan_bantuan', this.checked);",
          "  });",
          "  $('#stabilitas-harga').on('click', function(){",
          "    Shiny.setInputValue('stabilitas_harga', this.checked);",
          "  });",
          "  $('#simpanan-investasi').on('click', function(){",
          "    Shiny.setInputValue('simpanan_investasi', this.checked);",
          "  });",
          "  $('#peluang-ekonomi').on('click', function(){",
          "    Shiny.setInputValue('peluang_ekonomi', this.checked);",
          "  });",
          "  $('#ketersedian-pekerjaan').on('click', function(){",
          "    Shiny.setInputValue('ketersedian_pekerjaan', this.checked);",
          "  });",
          "  $('#kesenjangan-ekonomi').on('click', function(){",
          "    Shiny.setInputValue('kesenjangan_ekonomi', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-aspek-ekonomi', targets = 0),
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
  
  
  # Update Ekonomi
  loadDataAspekEkonomi <- function() {
    if (file.exists(pathTambahAspekEkonomi)) {
      read.csv(pathTambahAspekEkonomi, stringsAsFactors = FALSE)
    } else {
      data.frame(
        No = character(),
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini =
          character(), 
        Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah =
          character(), 
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial =
          character(), 
        Jika.ya.sebutkan.aspek.ekonomi = character(), 
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini =
          character(), 
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda =
          character(), 
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini =
          character(), 
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini =
          character(), 
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini =
          character(),
        stringsAsFactors = FALSE
      )
    }
  }
  
  observeEvent(input$cancelAspekEkonomi, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataAspekEkonomi()
    data <- data[data$No == id, ]
    
    updateSelectInput(
      session,
      'Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini.edit',
      selected = data$Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah.edit',
      selected = data$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial.edit',
      selected = data$Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial
    )	
    updateTextInput(session, 'Jika.ya.sebutkan.aspek.ekonomi.edit', value = data$Jika.ya.sebutkan.aspek.ekonomi)	
    updateSelectInput(
      session,
      'Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini.edit',
      selected = data$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda.edit',
      selected = data$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini.edit',
      selected = data$Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini.edit',
      selected = data$Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini.edit',
      selected = data$Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini
    )
    
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateAspekEkonomi, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataAspekEkonomi()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini = input$Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini.edit, 
        Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah = input$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah.edit, 
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial = input$Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial.edit, 
        Jika.ya.sebutkan.aspek.ekonomi = input$Jika.ya.sebutkan.aspek.ekonomi.edit, 
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini = input$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini.edit, 
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda = input$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda.edit, 
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini = input$Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini.edit, 
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini = input$Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini.edit, 
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini = input$Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini.edit, 
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
      
      temp <- paste0(pathTambahAspekEkonomi, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathTambahAspekEkonomi)
        render_server_aspek_ekonomi(FALSE)
        
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
  
  output$barChartUsahaPendapatan <- renderPlot({
    modal_combined_data <- read_csv(pathTambahAspekEkonomi) %>%
      filter(!is.na(Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini) & !is.na(Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah)) %>%
      mutate(
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini= recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah= recode(
          Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah,`1`='500.000 - 1.500.000',
          `2`='1.500.001 - 3.000.000',
          `3`='3.000.001 - 4.500.000',
          `4`='4.500.001 - ke atas'
        )
      )
    
    modal_combined_data$Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini <- as.factor(modal_combined_data$Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini)
    modal_combined_data$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah <- as.factor(modal_combined_data$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah)
    
    modal_combined_count <- modal_combined_data %>%
      group_by(Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini, Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah) %>%
      summarise(count = n()) %>%
      ungroup()
    
    num_bars <- nrow(modal_combined_count)
    colors <- brewer.pal(min(num_bars, 4), "Dark2")
    
    ggplot(modal_combined_count, aes(x = Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini, y = count, fill = Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Kepemilikan Usaha dan Pendapatan",
           x = "Memiliki Usaha Sendiri?",
           y = "Jumlah Responden",
           fill = "Berapa \nPendapatan Perbulannya?") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(modal_combined_count$count), by = 1)) 
  })
  
  output$analysisTextUsahaPendapatan <- renderText({
    modal_combined_data <- read_csv(pathTambahAspekEkonomi) %>%
      filter(!is.na(Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini) & !is.na(Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah)) %>%
      mutate(
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini= recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    total_responden <- nrow(modal_combined_data)
    
    Ya <- modal_combined_data %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini == "Ya") %>%
      nrow()
    
    Tidak <- modal_combined_data %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini == "Tidak") %>%
      nrow()
    
    JmlhYa <- modal_combined_data %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini == "Ya")
    
    JmlhTidak <- modal_combined_data %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini == "Tidak")
    # Rata Rata Pendapatan
    mutate_rataRataPendapatan <- list(
      '500.000 - 1.500.000',
      '1.500.001 - 3.000.000',
      '3.000.001 - 4.500.000',
      '4.500.001 - ke atas'
    )
    
    rataRataPendapatanYa <- mean(JmlhYa$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah) 
    
    rataRataPendapatanTidak <- mean(JmlhTidak$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah) 
    
    # Mayoritas Pendapatan
    tertinggiPednapatanYa <- JmlhYa %>%
      count(Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah, sort = TRUE)
    
    tertinggiPednapatanYa <- tertinggiPednapatanYa$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah[1]
    
    tertinggiPednapatanTidak <- JmlhTidak %>%
      count(Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah, sort = TRUE)
    
    tertinggiPednapatanTidak <- tertinggiPednapatanTidak$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah[1]
    
    paste0("Dari total ", total_responden," responden, Terdapat : ", Ya, " orang yang memiliki usaha dan ", Tidak, " orang belum memiliki usaha. Dari hasil analisis di atas, ditemukan bahwa orang yang sudah memiliki usaha, rata rata memiliki pendapatan sekitar ", mutate_rataRataPendapatan[round(rataRataPendapatanYa)], " dengan pendapatan tertinggi yaitu ", mutate_rataRataPendapatan[tertinggiPednapatanYa], " dan yang belum memiliki usaha sendiri, rata rata memiliki pendapatan sekitar ", mutate_rataRataPendapatan[round(rataRataPendapatanTidak)], " dengan pendapatan tertinggi yaitu ", mutate_rataRataPendapatan[tertinggiPednapatanTidak])
  })
  
  output$barChartBantuanSosial <- renderPlotly({
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial = recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    # Pisahkan data yang 'Ya' untuk mengelola NA di 'Jika.ya.sebutkan.aspek.ekonomi'
    data_ya <- data %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial == "Ya" & !is.na(Jika.ya.sebutkan.aspek.ekonomi))
    
    # Gabungkan kembali data yang sudah di-filter
    data <- bind_rows(
      data_ya,
      data %>% filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial != "Ya")
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
      group_by(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial) %>%
      summarise(
        count = n(),
        Jika.ya.sebutkan.aspek.ekonomi = collapse_with_br(na.omit(Jika.ya.sebutkan.aspek.ekonomi))
      )
    
    plot_ly(data_summary, labels = ~Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial, values = ~count, type = 'pie',
            hovertext = ~Jika.ya.sebutkan.aspek.ekonomi, hoverinfo = 'text+label+percent',
            textinfo = 'label+percent') %>%
      layout(title = 'Persentase Setiap Sektor Bidang',
             hovermode = 'closest')
  })

  output$analysisTextBantuanSosial <- renderText({
    modal_combined_data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial= recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    total_responden <- nrow(modal_combined_data)
    
    
    terimaBantuan <- modal_combined_data %>%
      count(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial) %>%
      mutate(percentage = n / sum(n) * 100)
    
    ya_count <- terimaBantuan %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial == "Ya") %>%
      pull(n)
    
    tidak_count <- terimaBantuan %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial == "Tidak") %>%
      pull(n)
    
    ya_percentage <- terimaBantuan %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial == "Ya") %>%
      pull(percentage)
    
    tidak_percentage <- terimaBantuan %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial == "Tidak") %>%
      pull(percentage)
    
    Ya <- modal_combined_data %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial == "Ya") %>%
      nrow()
    
    Tidak <- modal_combined_data %>%
      filter(Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial == "Tidak") %>%
      nrow()
    
    BantuanDiterima <- modal_combined_data %>%
      filter(!is.na(modal_combined_data$Jika.ya.sebutkan.aspek.ekonomi))
    
    BantuanDiterima <- paste(BantuanDiterima$Jika.ya.sebutkan.aspek.ekonomi, collapse = ", ")
    
    paste0("Dari total ", total_responden," responden, Terdapat ", Ya, " orang (", round(ya_percentage, 1), "%) telah menerima bantuan sosial. Sementara itu, sebanyak ", Tidak, " orang (", round(tidak_percentage, 1)," belum menerima bantuan sosial. Dari hasil analisis didapatkan bantuan yang diterima masyarakat adalah ", BantuanDiterima)
  })
  
  output$barChartStabilitasInvestasi <- renderPlot({
    modal_combined_data <- read_csv(pathTambahAspekEkonomi) %>%
      filter(!is.na(Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini) & !is.na(Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda)) %>%
      mutate(
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini= recode(
          Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini,
          `4`='Sangat stabil',
          `3`='Stabil',
          `2`='Tidak stabil',
          `1`='Sangat tidak stabil'
        ),	
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda= recode(
          Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    modal_combined_data$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini <- as.factor(modal_combined_data$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini)
    modal_combined_data$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda <- as.factor(modal_combined_data$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda)
    
    modal_combined_count <- modal_combined_data %>%
      group_by(Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini, Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda) %>%
      summarise(count = n()) %>%
      ungroup()
    
    num_bars <- nrow(modal_combined_count)
    colors <- brewer.pal(min(num_bars, 4), "Dark2")
    
    ggplot(modal_combined_count, aes(x = Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini, y = count, fill = Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Kepemilikan Usaha dan Pendapatan",
           x = "Bagaimana Anda menilai stabilitas harga?",
           y = "Jumlah Responden",
           fill = "Apakah Anda memiliki\n simpanan atau investasi?") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(modal_combined_count$count), by = 1)) 
  })
  
  output$analysisStabilitasInvestasi <- renderUI({
    modal_combined_data <- read_csv(pathTambahAspekEkonomi) %>%
      filter(!is.na(Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini) & 
               !is.na(Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda)) %>%
      mutate(
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini= recode(
          Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini,
          `4`='Sangat stabil',
          `3`='Stabil',
          `2`='Tidak stabil',
          `1`='Sangat tidak stabil'
        ),  
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda= recode(
          Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    # Menghitung persentase terbesar untuk stabilitas harga
    stabilitas_count <- modal_combined_data %>%
      group_by(Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100) %>%
      arrange(desc(percentage))
    
    # Kategori stabilitas harga dengan persentase terbesar
    stabilitas_terbesar <- stabilitas_count[1, ]
    
    # Menganalisis kesadaran tentang stabilitas harga terkait dengan investasi
    investasi_count <- modal_combined_data %>%
      group_by(Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini, Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda) %>%
      summarise(count = n()) %>%
      group_by(Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini) %>%
      mutate(percentage = count / sum(count) * 100) %>%
      ungroup() %>%
      arrange(desc(percentage))
    
    analisis_string <- paste0(
      "Kategori stabilitas harga dengan persentase terbesar adalah '", stabilitas_terbesar$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini, 
      "' dengan ", round(stabilitas_terbesar$percentage, 1), "% responden. \n",
      "Berikut adalah analisis kesadaran tentang stabilitas harga terkait dengan investasi:\n"
    )
    
    for (kategori in unique(investasi_count$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini)) {
      analisis_string <- paste0(
        analisis_string, "\n",
        kategori, ":\n"
      )
      
      subdata <- investasi_count %>% filter(Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini == kategori)
      
      for (i in 1:nrow(subdata)) {
        analisis_string <- paste0(
          "<p>",analisis_string, "- ", subdata$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda[i], 
          ": ", round(subdata$percentage[i], 1), "%</p>"
        )
      }
    }
    
    HTML(analisis_string)
  })
  
  
  output$pieChartPeluangEkonomi <- renderPlot({
    
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini= recode(
          Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )%>%
      drop_na(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini)
    
    data <- data %>%
      count(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisPeluangEkonomi <- renderText({
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini = recode(
          Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Menyusun kalimat analisis
    total_responden <- sum(data_summary$n)
    ya_responden <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini == "Ya") %>% pull(n)
    ya_percentage <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini == "Ya") %>% pull(percentage)
    
    tidak_responden <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini == "Tidak") %>% pull(n)
    tidak_percentage <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini == "Tidak") %>% pull(percentage)
    
    belum_tahu_responden <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini == "Belum tahu") %>% pull(n)
    belum_tahu_percentage <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini == "Belum tahu") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ",
      ya_responden, " orang (", round(ya_percentage, 1), "%) merasa terdapat peluang ekonomi yang cukup di desa ini. ",
      tidak_responden, " orang (", round(tidak_percentage, 1), "%) merasa tidak terdapat peluang ekonomi yang cukup. ",
      "Selain itu, ada ", belum_tahu_responden, " orang (", round(belum_tahu_percentage, 1), "%) yang belum tahu atau tidak memiliki pendapat mengenai adanya peluang ekonomi di desa ini."
    )
    
    analisis_lanjutan <- paste0(
      "\n\nAnalisis ini menunjukkan bahwa sebagian besar penduduk desa memiliki pandangan yang beragam mengenai peluang ekonomi di wilayah mereka. ",
      "Dengan ", round(ya_percentage, 1), "% responden yang percaya adanya peluang ekonomi yang cukup, ini mencerminkan adanya optimisme dalam sebagian komunitas. ",
      "Namun, ", round(tidak_percentage, 1), "% responden merasa bahwa peluang ekonomi tersebut tidak cukup, yang mengindikasikan adanya kebutuhan untuk memperbaiki atau memperkenalkan inisiatif ekonomi yang lebih efektif."
    )
    
    paste0(analisis, analisis_lanjutan)
  })
  
  
  
  output$pieChartLapanganPekerjaan <- renderPlot({
    
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini= recode(
          Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini,
          `4`='Sangat terbuka',
          `3`='Terbuka',
          `2`='Tidak terbuka',
          `1`='Sangat tidak terbuka'
        )
      )%>%
      drop_na(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini)
    
    data <- data %>%
      count(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
 
  output$analysisLapanganPekerjaan <- renderText({
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini = recode(
          Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini,
          `4` = 'Sangat terbuka',
          `3` = 'Terbuka',
          `2` = 'Tidak terbuka',
          `1` = 'Sangat tidak terbuka'
        )
      )
    
    data_summary <- data %>%
      count(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Menyusun kalimat analisis
    total_responden <- sum(data_summary$n)
    sangat_terbuka_responden <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Sangat terbuka") %>% pull(n)
    sangat_terbuka_percentage <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Sangat terbuka") %>% pull(percentage)
    
    terbuka_responden <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Terbuka") %>% pull(n)
    terbuka_percentage <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Terbuka") %>% pull(percentage)
    
    tidak_terbuka_responden <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Tidak terbuka") %>% pull(n)
    tidak_terbuka_percentage <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Tidak terbuka") %>% pull(percentage)
    
    sangat_tidak_terbuka_responden <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Sangat tidak terbuka") %>% pull(n)
    sangat_tidak_terbuka_percentage <- data_summary %>% filter(Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini == "Sangat tidak terbuka") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ",
      sangat_terbuka_responden, " orang (", round(sangat_terbuka_percentage, 1), "%) merasa bahwa aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini sangat terbuka. ",
      terbuka_responden, " orang (", round(terbuka_percentage, 1), "%) menilai aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini terbuka. ",
      tidak_terbuka_responden, " orang (", round(tidak_terbuka_percentage, 1), "%) merasa aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini tidak terbuka. ",
      "Sebanyak ", sangat_tidak_terbuka_responden, " orang (", round(sangat_tidak_terbuka_percentage, 1), "%) merasa bahwa aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini sangat tidak terbuka."
    )
    
    analisis <- paste0(
      analisis, "\n\n",
      "Hasil survei ini menunjukkan bahwa mayoritas responden merasa bahwa aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini cukup baik, dengan ", 
      sangat_terbuka_responden + terbuka_responden, " orang (", round(sangat_terbuka_percentage + terbuka_percentage, 1), "%) yang menilai sangat terbuka atau terbuka. ",
      "Namun, masih ada ", tidak_terbuka_responden + sangat_tidak_terbuka_responden, " orang (", round(tidak_terbuka_percentage + sangat_tidak_terbuka_percentage, 1), "%) yang merasa bahwa aksesibilitas dan ketersediaan lapangan pekerjaan tidak terbuka atau sangat tidak terbuka. ",
      "Hal ini mengindikasikan bahwa meskipun ada pandangan positif mengenai ketersediaan lapangan pekerjaan, ada beberapa area yang mungkin perlu diperbaiki untuk meningkatkan aksesibilitas dan peluang pekerjaan di desa ini. ",
      "Upaya seperti pengembangan infrastruktur, program pelatihan keterampilan, dan peningkatan investasi di sektor lokal dapat menjadi langkah yang efektif untuk mengatasi kekurangan ini dan meningkatkan kesejahteraan ekonomi masyarakat desa."
    )
    
    analisis
  })
  
  output$pieChartKesenjanganEkonomiAspekEkonomi <- renderPlot({
    
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini= recode(
          Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini,
          `4`='Sangat terasa',
          `3`='Terasa',
          `2`='Tidak terasa',
          `1`='Sangat tidak terasa'
        )
      )%>%
      drop_na(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini)
    
    data <- data %>%
      count(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
   
  output$analysisKesenjanganEkonomiAspekEkonomi <- renderText({
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini = recode(
          Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini,
          `4` = 'Sangat terasa',
          `3` = 'Terasa',
          `2` = 'Tidak terasa',
          `1` = 'Sangat tidak terasa'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    sangat_terasa_responden <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Sangat terasa") %>% pull(n)
    sangat_terasa_percentage <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Sangat terasa") %>% pull(percentage)
    
    terasa_responden <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Terasa") %>% pull(n)
    terasa_percentage <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Terasa") %>% pull(percentage)
    
    tidak_terasa_responden <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Tidak terasa") %>% pull(n)
    tidak_terasa_percentage <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Tidak terasa") %>% pull(percentage)
    
    sangat_tidak_terasa_responden <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Sangat tidak terasa") %>% pull(n)
    sangat_tidak_terasa_percentage <- data_summary %>% filter(Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini == "Sangat tidak terasa") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ",
      sangat_terasa_responden, " orang (", round(sangat_terasa_percentage, 1), "%) merasa bahwa kesenjangan ekonomi di antara penduduk di desa ini sangat terasa. ",
      terasa_responden, " orang (", round(terasa_percentage, 1), "%) merasa kesenjangan ekonomi tersebut terasa. ",
      tidak_terasa_responden, " orang (", round(tidak_terasa_percentage, 1), "%) merasa bahwa kesenjangan ekonomi tidak terasa. ",
      "Sebanyak ", sangat_tidak_terasa_responden, " orang (", round(sangat_tidak_terasa_percentage, 1), "%) merasa bahwa kesenjangan ekonomi sangat tidak terasa."
    )
    
    analisis <- paste0(
      analisis, "\n\n",
      "Hasil survei ini menunjukkan bahwa mayoritas responden merasa adanya kesenjangan ekonomi di antara penduduk desa ini, dengan ",
      sangat_terasa_responden + terasa_responden, " orang (", round(sangat_terasa_percentage + terasa_percentage, 1), "%) yang menilai sangat terasa atau terasa. ",
      "Namun, masih ada ", tidak_terasa_responden + sangat_tidak_terasa_responden, " orang (", round(tidak_terasa_percentage + sangat_tidak_terasa_percentage, 1), "%) yang merasa bahwa kesenjangan ekonomi tidak terasa atau sangat tidak terasa. ",
      "Hal ini mengindikasikan bahwa meskipun ada pandangan yang kuat mengenai adanya kesenjangan ekonomi, ada juga sebagian penduduk yang merasa bahwa kesenjangan tersebut tidak signifikan. ",
      "Kesadaran tentang adanya kesenjangan ekonomi adalah langkah pertama yang penting untuk mengatasinya. ",
      "Untuk mengurangi kesenjangan ekonomi ini, beberapa langkah dapat diambil, seperti meningkatkan akses terhadap pendidikan dan pelatihan keterampilan, mendukung usaha kecil dan menengah, serta mengembangkan program bantuan sosial yang lebih merata. ",
      "Dengan demikian, diharapkan dapat tercipta kesejahteraan yang lebih merata di antara seluruh penduduk desa."
    )
    
    analisis
  })
}
render_server_aspek_ekonomi(TRUE)