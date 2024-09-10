render_server_aspek_spatial <- function(params){
  
  pathTambahAspekSpatial <- 'data/AspekSpatial.csv'
  
  
  output$data_table_apekSpatial <- renderDT({
    data <- read_csv(pathTambahAspekSpatial)%>%
      mutate(
        Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini = recode(
          Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini,
          `1`='Jalan Kaki',
          `2`='Sepeda',
          `3`='Sepeda Motor',
          `4`='Mobil',
          `5`='Angkutan Umum',
          `6`='Lainnya'
        ), 
        Apakah.transportasi.umum.cukup.memadai.di.desa.ini = recode(
          Apakah.transportasi.umum.cukup.memadai.di.desa.ini, 
          `4`='Sangat memadai',
          `3`='Cukup memadai',
          `2`='Tidak cukup',
          `1`='Tidak ada'
        ), 
        Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini = recode(
          Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini, 
          `4`='Baik',
          `3`='Cukup baik',
          `2`='Tidak cukup baik',
          `1`='Tidak ada'
        ), 
        Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah = recode(
          Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Bagaimana.pasokan.air.bersih.di.desa.ini = recode(
          Bagaimana.pasokan.air.bersih.di.desa.ini, 
          `4`='Baik',
          `3`='Cukup baik',
          `2`='Tidak cukup baik',
          `1`='Tidak ada'
        ), 
        Apakah.Anda.menghadapi.masalah.terkait.air.bersih = recode(
          Apakah.Anda.menghadapi.masalah.terkait.air.bersih, 
          `4`='Sering',
          `3`='Kadang-kadang',
          `2`='Jarang',
          `1`='Tidak Pernah'
        ), 
        Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya = recode(
          Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk = recode(
          Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk,
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
        'Seberapa jauh rumah Anda dari pusat desa atau kota terdekat? (Kec. Sagaranten) .... KM', 
        'Bagaimana Anda biasanya mengakses fasilitas umum seperti pasar, sekolah, dan rumah sakit di desa ini?', 
        'Apakah transportasi umum cukup memadai di desa ini?', 
        'Bagaimana kondisi infrastruktur jalan di desa ini? ', 
        'Apakah terdapat masalah terkait sanitasi di desa ini, seperti akses ke fasilitas toilet yang memadai atau masalah limbah?', 
        'Bagaimana pasokan air bersih di desa ini? ', 
        'Apakah Anda menghadapi masalah terkait air bersih?', 
        'Apakah terdapat masalah terkait polusi di desa ini, seperti polusi udara atau pencemaran lingkungan lainnya?', 
        'Apakah Anda merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai untuk memenuhi kebutuhan sehari-hari penduduk? ', 
        "Actions"
      ),
      options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  if (!$('#aspekSpatial-checkbox').length) {",
          "  $(thead).closest('thead').prepend(` 
      <tr id=\"aspekSpatial-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"jauh_rumah\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"akses_fasilitas\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"transportasi_memadai\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"kondisi_infrastruktur\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"sanitasi_desa\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pasokan_air\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"air_bersih\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"polusi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"keberadaan_fasilitas\">
        </th> 
      </tr>
          `);",
          "  $('#jauh_rumah').on('click', function(){",
          "    Shiny.setInputValue('jauh_rumah', this.checked);",
          "  });",
          "  $('#akses_fasilitas').on('click', function(){",
          "    Shiny.setInputValue('akses_fasilitas', this.checked);",
          "  });",
          "  $('#transportasi_memadai').on('click', function(){",
          "    Shiny.setInputValue('transportasi_memadai', this.checked);",
          "  });",
          "  $('#kondisi_infrastruktur').on('click', function(){",
          "    Shiny.setInputValue('kondisi_infrastruktur', this.checked);",
          "  });",
          "  $('#sanitasi_desa').on('click', function(){",
          "    Shiny.setInputValue('sanitasi_desa', this.checked);",
          "  });",
          "  $('#pasokan_air').on('click', function(){",
          "    Shiny.setInputValue('pasokan_air', this.checked);",
          "  });",
          "  $('#air_bersih').on('click', function(){",
          "    Shiny.setInputValue('air_bersih', this.checked);",
          "  });",
          "  $('#polusi').on('click', function(){",
          "    Shiny.setInputValue('polusi', this.checked);",
          "  });",
          "  $('#keberadaan_fasilitas').on('click', function(){",
          "    Shiny.setInputValue('keberadaan_fasilitas', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-aspek-spatial', targets = 0),
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
  loadDataAspekSpatial <- function() {
    if (file.exists(pathTambahAspekSpatial)) {
      read.csv(pathTambahAspekSpatial, stringsAsFactors = FALSE)
    } else {
      data.frame(
        No = character(),
        Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM =
          character(), Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini =
          character(), Apakah.transportasi.umum.cukup.memadai.di.desa.ini = character(), Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini =
          character(), Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah =
          character(), Bagaimana.pasokan.air.bersih.di.desa.ini = character(), Apakah.Anda.menghadapi.masalah.terkait.air.bersih =
          character(), Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya =
          character(), Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk =
          character(), 
        stringsAsFactors = FALSE
      )
    }
  }
  
  observeEvent(input$cancelAspekSpatial, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataAspekSpatial()
    data <- data[data$No == id, ]
    
    updateNumericInput(
      session,
      'Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM.edit',
      value = data$Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM
    )	
    updateSelectInput(
      session,
      'Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini.edit',
      selected = data$Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.transportasi.umum.cukup.memadai.di.desa.ini.edit',
      selected = data$Apakah.transportasi.umum.cukup.memadai.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini.edit',
      selected = data$Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah.edit',
      selected = data$Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah
    )	
    updateSelectInput(
      session,
      'Bagaimana.pasokan.air.bersih.di.desa.ini.edit',
      selected = data$Bagaimana.pasokan.air.bersih.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.menghadapi.masalah.terkait.air.bersih.edit',
      selected = data$Apakah.Anda.menghadapi.masalah.terkait.air.bersih
    )	
    updateSelectInput(
      session,
      'Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya.edit',
      selected = data$Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk.edit',
      selected = data$Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk
    )
    
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateAspekSpatial, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataAspekSpatial()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM =
          input$Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM.edit, 
        Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini =
          input$Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini.edit, 
        Apakah.transportasi.umum.cukup.memadai.di.desa.ini =
          input$Apakah.transportasi.umum.cukup.memadai.di.desa.ini.edit, 
        Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini =
          input$Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini.edit, 
        Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah =
          input$Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah.edit, 
        Bagaimana.pasokan.air.bersih.di.desa.ini =
          input$Bagaimana.pasokan.air.bersih.di.desa.ini.edit, 
        Apakah.Anda.menghadapi.masalah.terkait.air.bersih =
          input$Apakah.Anda.menghadapi.masalah.terkait.air.bersih.edit, 
        Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya =
          input$Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya.edit, 
        Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk =
          input$Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk.edit, 
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
      
      temp <- paste0(pathTambahAspekSpatial, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathTambahAspekSpatial)
        render_server_aspek_spatial(FALSE)
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
  
  
  output$barChartJarakRumah <- renderPlot({
    data <- read_csv(pathTambahAspekSpatial) %>%
      count(Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM)
    
    
    custom_colors <- c("red", "green", "blue", "orange", "purple", "yellow", "pink", "brown", "cyan", "magenta")
    
    set.seed(42) 
    shuffled_colors <- sample(custom_colors, length(unique(data$Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM)), replace = TRUE)
    
    ggplot(data, aes(x = factor(Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM), y = n, fill = factor(Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = shuffled_colors) + 
      scale_y_continuous(breaks = seq(0, max(data$n), by = 1)) + 
      theme_minimal() +
      labs(x = "Jarak (KM)", y = "Jumlah", title = "") +
      theme(legend.position = "none") 
  })

  output$pieChartAksesFasilitas <- renderPlot({
    
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini = recode(
          Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini,
          `1`='Jalan Kaki',
          `2`='Sepeda',
          `3`='Sepeda Motor',
          `4`='Mobil',
          `5`='Angkutan Umum',
          `6`='Lainnya'
        )
      )
    
    data <- data %>%
      count(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })

  output$analysisAksesFasilitas <- renderText({
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini = recode(
          Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini,
          `1` = 'Jalan Kaki',
          `2` = 'Sepeda',
          `3` = 'Sepeda Motor',
          `4` = 'Mobil',
          `5` = 'Angkutan Umum',
          `6` = 'Lainnya'
        )
      )
    
    data_summary <- data %>%
      count(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    jalan_kaki_responden <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Jalan Kaki") %>% pull(n)
    jalan_kaki_responden <- ifelse(length(jalan_kaki_responden) == 0, 0, jalan_kaki_responden)
    jalan_kaki_percentage <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Jalan Kaki") %>% pull(percentage)
    jalan_kaki_percentage <- ifelse(length(jalan_kaki_percentage) == 0, 0, jalan_kaki_percentage)
    
    sepeda_responden <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Sepeda") %>% pull(n)
    sepeda_responden <- ifelse(length(sepeda_responden) == 0, 0, sepeda_responden)
    sepeda_percentage <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Sepeda") %>% pull(percentage)
    sepeda_percentage <- ifelse(length(sepeda_percentage) == 0, 0, sepeda_percentage)
    
    sepeda_motor_responden <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Sepeda Motor") %>% pull(n)
    sepeda_motor_responden <- ifelse(length(sepeda_motor_responden) == 0, 0, sepeda_motor_responden)
    sepeda_motor_percentage <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Sepeda Motor") %>% pull(percentage)
    sepeda_motor_percentage <- ifelse(length(sepeda_motor_percentage) == 0, 0, sepeda_motor_percentage)
    
    mobil_responden <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Mobil") %>% pull(n)
    mobil_responden <- ifelse(length(mobil_responden) == 0, 0, mobil_responden)
    mobil_percentage <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Mobil") %>% pull(percentage)
    mobil_percentage <- ifelse(length(mobil_percentage) == 0, 0, mobil_percentage)
    
    angkutan_umum_responden <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Angkutan Umum") %>% pull(n)
    angkutan_umum_responden <- ifelse(length(angkutan_umum_responden) == 0, 0, angkutan_umum_responden)
    angkutan_umum_percentage <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Angkutan Umum") %>% pull(percentage)
    angkutan_umum_percentage <- ifelse(length(angkutan_umum_percentage) == 0, 0, angkutan_umum_percentage)
    
    lainnya_responden <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Lainnya") %>% pull(n)
    lainnya_responden <- ifelse(length(lainnya_responden) == 0, 0, lainnya_responden)
    lainnya_percentage <- data_summary %>% filter(Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini == "Lainnya") %>% pull(percentage)
    lainnya_percentage <- ifelse(length(lainnya_percentage) == 0, 0, lainnya_percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, cara yang paling umum untuk mengakses fasilitas umum di desa adalah menggunakan sepeda motor, yang dipilih oleh ", 
      sepeda_motor_responden, " orang (", round(sepeda_motor_percentage, 1), "%). "
    )
    
    if (jalan_kaki_percentage > 0) {
      analisis <- paste0(analisis, "Selain itu, ", jalan_kaki_responden, " orang (", round(jalan_kaki_percentage, 1), "%) biasanya mengakses fasilitas umum dengan berjalan kaki. ")
    }
    
    if (sepeda_percentage > 0) {
      analisis <- paste0(analisis, sepeda_responden, " orang (", round(sepeda_percentage, 1), "%) menggunakan sepeda. ")
    }
    
    if (mobil_percentage > 0) {
      analisis <- paste0(analisis, mobil_responden, " orang (", round(mobil_percentage, 1), "%) menggunakan mobil. ")
    }
    
    if (angkutan_umum_percentage > 0) {
      analisis <- paste0(analisis, angkutan_umum_responden, " orang (", round(angkutan_umum_percentage, 1), "%) menggunakan angkutan umum. ")
    }
    
    if (lainnya_percentage > 0) {
      analisis <- paste0(analisis, "dan ", lainnya_responden, " orang (", round(lainnya_percentage, 1), "%) menggunakan cara lainnya. ")
    }
    
    analisis <- paste0(analisis, "Hasil ini menunjukkan bahwa sepeda motor adalah moda transportasi yang dominan untuk mengakses fasilitas umum di desa, yang mencerminkan kemungkinan tingginya kepemilikan sepeda motor di daerah tersebut dan kemudahan penggunaannya. Moda transportasi lain seperti jalan kaki, sepeda, mobil, dan angkutan umum juga digunakan tetapi dalam persentase yang lebih kecil, yang mungkin mencerminkan preferensi pribadi, jarak, atau ketersediaan moda transportasi tersebut.")
    
    analisis
  })

  output$pieChartKetersediaanTransportasi <- renderPlot({
    
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.transportasi.umum.cukup.memadai.di.desa.ini = recode(
          Apakah.transportasi.umum.cukup.memadai.di.desa.ini, 
          `4`='Sangat memadai',
          `3`='Cukup memadai',
          `2`='Tidak cukup',
          `1`='Tidak ada'
        )
      )
    
    data <- data %>%
      count(Apakah.transportasi.umum.cukup.memadai.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.transportasi.umum.cukup.memadai.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.transportasi.umum.cukup.memadai.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisKetersediaanTransportasi <- renderText({
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.transportasi.umum.cukup.memadai.di.desa.ini = recode(
          Apakah.transportasi.umum.cukup.memadai.di.desa.ini, 
          `4` = 'Sangat memadai',
          `3` = 'Cukup memadai',
          `2` = 'Tidak cukup',
          `1` = 'Tidak ada'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.transportasi.umum.cukup.memadai.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    sangat_memadai_responden <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Sangat memadai") %>% pull(n)
    sangat_memadai_percentage <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Sangat memadai") %>% pull(percentage)
    
    cukup_memadai_responden <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Cukup memadai") %>% pull(n)
    cukup_memadai_percentage <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Cukup memadai") %>% pull(percentage)
    
    tidak_cukup_responden <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Tidak cukup") %>% pull(n)
    tidak_cukup_percentage <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Tidak cukup") %>% pull(percentage)
    
    tidak_ada_responden <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Tidak ada") %>% pull(n)
    tidak_ada_percentage <- data_summary %>% filter(Apakah.transportasi.umum.cukup.memadai.di.desa.ini == "Tidak ada") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, mayoritas merasa bahwa transportasi umum di desa cukup memadai. Secara rinci, ",
      sangat_memadai_responden, " orang (", round(sangat_memadai_percentage, 1), "%) menyatakan bahwa transportasi umum sangat memadai. ",
      cukup_memadai_responden, " orang (", round(cukup_memadai_percentage, 1), "%) merasa bahwa transportasi umum cukup memadai. "
    )
    
    if (tidak_cukup_responden > 0) {
      analisis <- paste0(analisis, "Namun, ", tidak_cukup_responden, " orang (", round(tidak_cukup_percentage, 1), "%) berpendapat bahwa transportasi umum tidak cukup memadai. ")
    }
    
    if (tidak_ada_responden > 0) {
      analisis <- paste0(analisis, "Sebanyak ", tidak_ada_responden, " orang (", round(tidak_ada_percentage, 1), "%) bahkan menyatakan bahwa tidak ada transportasi umum yang memadai. ")
    }
    
    analisis <- paste0(analisis, "Hasil ini menunjukkan bahwa meskipun sebagian besar penduduk merasa transportasi umum di desa ini cukup memadai, masih ada sejumlah responden yang merasa kebutuhan transportasi umum belum terpenuhi dengan baik. Ini dapat menjadi masukan penting bagi pihak terkait untuk meningkatkan ketersediaan dan kualitas transportasi umum di desa.")
    
    analisis
  })

  output$pieChartKondisiInfrastruktur <- renderPlot({
    
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini = recode(
          Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini, 
          `4`='Baik',
          `3`='Cukup baik',
          `2`='Tidak cukup baik',
          `1`='Tidak ada'
        )
      )%>%
      drop_na(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini)
    
    data <- data %>%
      count(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisKondisiInfrastruktur <- renderText({
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini = recode(
          Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini, 
          `4` = 'Baik',
          `3` = 'Cukup baik',
          `2` = 'Tidak cukup baik',
          `1` = 'Tidak ada'
        )
      )
    
    data_summary <- data %>%
      count(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    
    # Handling possible empty results with ifelse
    baik_responden <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Baik") %>% pull(n)
    baik_responden <- ifelse(length(baik_responden) == 0, 0, baik_responden)
    baik_percentage <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Baik") %>% pull(percentage)
    baik_percentage <- ifelse(length(baik_percentage) == 0, 0, baik_percentage)
    
    cukup_baik_responden <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Cukup baik") %>% pull(n)
    cukup_baik_responden <- ifelse(length(cukup_baik_responden) == 0, 0, cukup_baik_responden)
    cukup_baik_percentage <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Cukup baik") %>% pull(percentage)
    cukup_baik_percentage <- ifelse(length(cukup_baik_percentage) == 0, 0, cukup_baik_percentage)
    
    tidak_cukup_baik_responden <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Tidak cukup baik") %>% pull(n)
    tidak_cukup_baik_responden <- ifelse(length(tidak_cukup_baik_responden) == 0, 0, tidak_cukup_baik_responden)
    tidak_cukup_baik_percentage <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Tidak cukup baik") %>% pull(percentage)
    tidak_cukup_baik_percentage <- ifelse(length(tidak_cukup_baik_percentage) == 0, 0, tidak_cukup_baik_percentage)
    
    tidak_ada_responden <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Tidak ada") %>% pull(n)
    tidak_ada_responden <- ifelse(length(tidak_ada_responden) == 0, 0, tidak_ada_responden)
    tidak_ada_percentage <- data_summary %>% filter(Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini == "Tidak ada") %>% pull(percentage)
    tidak_ada_percentage <- ifelse(length(tidak_ada_percentage) == 0, 0, tidak_ada_percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, mayoritas memberikan penilaian yang positif terhadap kondisi infrastruktur jalan di desa ini. Secara rinci, ",
      baik_responden, " orang (", round(baik_percentage, 1), "%) menyatakan bahwa kondisi infrastruktur jalan baik. ",
      cukup_baik_responden, " orang (", round(cukup_baik_percentage, 1), "%) merasa bahwa kondisi infrastruktur jalan cukup baik. "
    )
    
    if (tidak_cukup_baik_responden > 0) {
      analisis <- paste0(analisis, "Namun, ada ", tidak_cukup_baik_responden, " orang (", round(tidak_cukup_baik_percentage, 1), "%) yang menilai bahwa kondisi infrastruktur jalan tidak cukup baik. ")
    }
    
    if (tidak_ada_responden > 0) {
      analisis <- paste0(analisis, "Sebanyak ", tidak_ada_responden, " orang (", round(tidak_ada_percentage, 1), "%) bahkan menyatakan bahwa tidak ada infrastruktur jalan yang memadai di desa ini. ")
    }
    
    analisis <- paste0(analisis, "Hasil ini menunjukkan bahwa meskipun sebagian besar penduduk merasa kondisi infrastruktur jalan di desa ini baik atau cukup baik, masih ada sejumlah responden yang merasa kebutuhan infrastruktur jalan belum terpenuhi dengan baik. Ini dapat menjadi masukan penting bagi pihak terkait untuk meningkatkan kualitas dan ketersediaan infrastruktur jalan di desa.")
    
    analisis
  })

  output$pieChartSanitasiDesa <- renderPlot({
    
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah = recode(
          Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )%>%
      drop_na(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah)
    
    data <- data %>%
      count(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisSanitasiDesa <- renderText({
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah = recode(
          Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    
    ya_responden <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah == "Ya") %>% pull(n)
    ya_responden <- ifelse(length(ya_responden) == 0, 0, ya_responden)
    ya_percentage <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah == "Ya") %>% pull(percentage)
    ya_percentage <- ifelse(length(ya_percentage) == 0, 0, ya_percentage)
    
    tidak_responden <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah == "Tidak") %>% pull(n)
    tidak_responden <- ifelse(length(tidak_responden) == 0, 0, tidak_responden)
    tidak_percentage <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah == "Tidak") %>% pull(percentage)
    tidak_percentage <- ifelse(length(tidak_percentage) == 0, 0, tidak_percentage)
    
    belum_tahu_responden <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah == "Belum tahu") %>% pull(n)
    belum_tahu_responden <- ifelse(length(belum_tahu_responden) == 0, 0, belum_tahu_responden)
    belum_tahu_percentage <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah == "Belum tahu") %>% pull(percentage)
    belum_tahu_percentage <- ifelse(length(belum_tahu_percentage) == 0, 0, belum_tahu_percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, sebanyak ", ya_responden, " orang (", round(ya_percentage, 1), "%) menyatakan bahwa terdapat masalah terkait sanitasi di desa ini. "
    )
    
    if (tidak_percentage > 0) {
      analisis <- paste0(analisis, tidak_responden, " orang (", round(tidak_percentage, 1), "%) menyatakan bahwa tidak terdapat masalah terkait sanitasi di desa ini. ")
    }
    
    if (belum_tahu_percentage > 0) {
      analisis <- paste0(analisis, "Sebanyak ", belum_tahu_responden, " orang (", round(belum_tahu_percentage, 1), "%) tidak tahu apakah terdapat masalah terkait sanitasi di desa ini atau tidak. ")
    }
    
    analisis <- paste0(analisis, "Hasil ini menunjukkan bahwa terdapat permasalahan sanitasi yang diakui oleh sebagian responden di desa ini. Hal ini dapat mencerminkan perlunya peningkatan akses terhadap fasilitas sanitasi yang memadai serta pengelolaan limbah yang lebih baik. Bagi pihak terkait, hasil ini bisa menjadi dasar untuk mengambil langkah-langkah perbaikan dalam bidang sanitasi demi meningkatkan kualitas hidup masyarakat di desa.")
    
    analisis
  })
  
  output$barChartPasokanAir <- renderPlot({
    modal_combined_data <- read_csv(pathTambahAspekSpatial) %>%
      filter(!is.na(Bagaimana.pasokan.air.bersih.di.desa.ini) & !is.na(Apakah.Anda.menghadapi.masalah.terkait.air.bersih)) %>%
      mutate(
        Bagaimana.pasokan.air.bersih.di.desa.ini = recode(
          Bagaimana.pasokan.air.bersih.di.desa.ini, 
          `4`='Baik',
          `3`='Cukup baik',
          `2`='Tidak cukup baik',
          `1`='Tidak ada'
        ), 
        Apakah.Anda.menghadapi.masalah.terkait.air.bersih = recode(
          Apakah.Anda.menghadapi.masalah.terkait.air.bersih, 
          `4`='Sering',
          `3`='Kadang-kadang',
          `2`='Jarang',
          `1`='Tidak Pernah'
        )
      )
    
    modal_combined_data$Bagaimana.pasokan.air.bersih.di.desa.ini <- as.factor(modal_combined_data$Bagaimana.pasokan.air.bersih.di.desa.ini)
    modal_combined_data$Apakah.Anda.menghadapi.masalah.terkait.air.bersih <- as.factor(modal_combined_data$Apakah.Anda.menghadapi.masalah.terkait.air.bersih)
    
    modal_combined_count <- modal_combined_data %>%
      group_by(Bagaimana.pasokan.air.bersih.di.desa.ini, Apakah.Anda.menghadapi.masalah.terkait.air.bersih) %>%
      summarise(count = n()) %>%
      ungroup()
    
    num_bars <- nrow(modal_combined_count)
    colors <- brewer.pal(min(num_bars, 4), "Dark2")
    
    ggplot(modal_combined_count, aes(x = Bagaimana.pasokan.air.bersih.di.desa.ini, y = count, fill = Apakah.Anda.menghadapi.masalah.terkait.air.bersih)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "",
           x = "Pasokan air bersih",
           y = "Jumlah Responden",
           fill = "Apakah menghadapi masalah air bersih?") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(modal_combined_count$count), by = 1)) 
  })
  
  output$analysisTextPasokanAir <- renderText({
    modal_combined_data <- read_csv(pathTambahAspekSpatial) %>%
      filter(!is.na(Bagaimana.pasokan.air.bersih.di.desa.ini) & !is.na(Apakah.Anda.menghadapi.masalah.terkait.air.bersih)) %>%
      mutate(
        Bagaimana.pasokan.air.bersih.di.desa.ini = recode(
          Bagaimana.pasokan.air.bersih.di.desa.ini, 
          `4` = 'Baik',
          `3` = 'Cukup baik',
          `2` = 'Tidak cukup baik',
          `1` = 'Tidak ada'
        ), 
        Apakah.Anda.menghadapi.masalah.terkait.air.bersih = recode(
          Apakah.Anda.menghadapi.masalah.terkait.air.bersih, 
          `4` = 'Sering',
          `3` = 'Kadang-kadang',
          `2` = 'Jarang',
          `1` = 'Tidak Pernah'
        )
      )
    
    modal_combined_count <- modal_combined_data %>%
      group_by(Bagaimana.pasokan.air.bersih.di.desa.ini, Apakah.Anda.menghadapi.masalah.terkait.air.bersih) %>%
      summarise(count = n()) %>%
      ungroup()
    
    total_responden <- sum(modal_combined_count$count)
    
    baik_responden <- modal_combined_count %>% filter(Bagaimana.pasokan.air.bersih.di.desa.ini == "Baik") %>% pull(count) %>% sum()
    cukup_baik_responden <- modal_combined_count %>% filter(Bagaimana.pasokan.air.bersih.di.desa.ini == "Cukup baik") %>% pull(count) %>% sum()
    tidak_cukup_baik_responden <- modal_combined_count %>% filter(Bagaimana.pasokan.air.bersih.di.desa.ini == "Tidak cukup baik") %>% pull(count) %>% sum()
    tidak_ada_responden <- modal_combined_count %>% filter(Bagaimana.pasokan.air.bersih.di.desa.ini == "Tidak ada") %>% pull(count) %>% sum()
    
    sering_masalah_responden <- modal_combined_count %>% filter(Apakah.Anda.menghadapi.masalah.terkait.air.bersih == "Sering") %>% pull(count) %>% sum()
    kadang_masalah_responden <- modal_combined_count %>% filter(Apakah.Anda.menghadapi.masalah.terkait.air.bersih == "Kadang-kadang") %>% pull(count) %>% sum()
    jarang_masalah_responden <- modal_combined_count %>% filter(Apakah.Anda.menghadapi.masalah.terkait.air.bersih == "Jarang") %>% pull(count) %>% sum()
    tidak_pernah_masalah_responden <- modal_combined_count %>% filter(Apakah.Anda.menghadapi.masalah.terkait.air.bersih == "Tidak Pernah") %>% pull(count) %>% sum()
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ", baik_responden, " orang (", round(baik_responden / total_responden * 100, 1), "%) menyatakan bahwa pasokan air bersih di desa ini baik, ",
      cukup_baik_responden, " orang (", round(cukup_baik_responden / total_responden * 100, 1), "%) menyatakan cukup baik, ",
      tidak_cukup_baik_responden, " orang (", round(tidak_cukup_baik_responden / total_responden * 100, 1), "%) menyatakan tidak cukup baik, dan ",
      tidak_ada_responden, " orang (", round(tidak_ada_responden / total_responden * 100, 1), "%) menyatakan tidak ada pasokan air bersih yang memadai."
    )
    
    analisis <- paste0(
      analisis, "\n\nTerkait masalah air bersih yang dihadapi, ", sering_masalah_responden, " orang (", round(sering_masalah_responden / total_responden * 100, 1), "%) sering menghadapi masalah air bersih, ",
      kadang_masalah_responden, " orang (", round(kadang_masalah_responden / total_responden * 100, 1), "%) kadang-kadang menghadapi masalah air bersih, ",
      jarang_masalah_responden, " orang (", round(jarang_masalah_responden / total_responden * 100, 1), "%) jarang menghadapi masalah air bersih, dan ",
      tidak_pernah_masalah_responden, " orang (", round(tidak_pernah_masalah_responden / total_responden * 100, 1), "%) tidak pernah menghadapi masalah air bersih."
    )
    
    analisis <- paste0(
      analisis, "\n\nHasil ini menunjukkan bahwa meskipun sebagian besar responden menilai pasokan air bersih di desa cukup baik atau baik, masih terdapat sejumlah responden yang menghadapi masalah air bersih secara berkala. Hal ini mengindikasikan bahwa perbaikan dan perhatian lebih terhadap distribusi dan kualitas pasokan air bersih di desa diperlukan untuk memastikan bahwa semua warga desa dapat menikmati akses air bersih yang memadai."
    )
    
    analisis
  })
  
  output$pieChartPolusiAspek <- renderPlot({
    
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya = recode(
          Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    data <- data %>%
      count(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisPolusiAspek <- renderText({
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya = recode(
          Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    ya_responden <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya == "Ya") %>% pull(n)
    ya_percentage <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya == "Ya") %>% pull(percentage)
    
    tidak_responden <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya == "Tidak") %>% pull(n)
    tidak_percentage <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya == "Tidak") %>% pull(percentage)
    
    belum_tahu_responden <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya == "Belum tahu") %>% pull(n)
    belum_tahu_percentage <- data_summary %>% filter(Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya == "Belum tahu") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ", ya_responden, " orang (", round(ya_percentage, 1), "%) menyatakan bahwa terdapat masalah terkait polusi di desa ini. ",
      tidak_responden, " orang (", round(tidak_percentage, 1), "%) menyatakan tidak ada masalah terkait polusi."
    )
    
    analisis <- paste0(
      analisis, "\n\nHasil ini menunjukkan bahwa mayoritas responden menyadari adanya masalah polusi di desa ini, yang mencakup polusi udara dan pencemaran lingkungan lainnya. Ini menunjukkan perlunya tindakan untuk mengatasi dan mengurangi polusi di desa, baik melalui program pengelolaan limbah, peningkatan kualitas udara, atau edukasi kepada masyarakat mengenai pentingnya menjaga kebersihan lingkungan. Di sisi lain, adanya responden yang menyatakan tidak ada masalah polusi dan yang belum tahu, menunjukkan perlunya informasi dan edukasi lebih lanjut mengenai dampak polusi dan bagaimana mengidentifikasinya di lingkungan sekitar."
    )
    
    analisis
  })
  
  output$pieChartBeradaanFasilitas <- renderPlot({
    
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk = recode(
          Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    data <- data %>%
      count(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk, "\n", round(percentage, 1), "%"))
    
    ggplot(data, aes(x = "", y = n, fill = Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisBeradaanFasilitas <- renderText({
    data <- read_csv(pathTambahAspekSpatial) %>%
      mutate(
        Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk = recode(
          Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk,
          `1` = 'Ya',
          `2` = 'Tidak',
          `3` = 'Belum tahu'
        )
      )
    
    data_summary <- data %>%
      count(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk) %>%
      mutate(percentage = n / sum(n) * 100)
    
    total_responden <- sum(data_summary$n)
    ya_responden <- data_summary %>% filter(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk == "Ya") %>% pull(n)
    ya_percentage <- data_summary %>% filter(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk == "Ya") %>% pull(percentage)
    
    tidak_responden <- data_summary %>% filter(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk == "Tidak") %>% pull(n)
    tidak_percentage <- data_summary %>% filter(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk == "Tidak") %>% pull(percentage)
    
    belum_tahu_responden <- data_summary %>% filter(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk == "Belum tahu") %>% pull(n)
    belum_tahu_percentage <- data_summary %>% filter(Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk == "Belum tahu") %>% pull(percentage)
    
    analisis <- paste0(
      "Dari total ", total_responden, " responden, ",
      ya_responden, " orang (", round(ya_percentage, 1), "%) merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai untuk memenuhi kebutuhan sehari-hari. ",
      tidak_responden, " orang (", round(tidak_percentage, 1), "%) merasa bahwa fasilitas dan infrastruktur tersebut tidak memadai."
    )
    
    analisis <- paste0(
      analisis, "\n\nHasil ini menunjukkan bahwa meskipun sebagian besar responden merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai, ada sejumlah responden yang merasa tidak puas dan yang belum tahu. Hal ini mengindikasikan adanya kebutuhan untuk meningkatkan dan memperbaiki infrastruktur serta fasilitas umum di desa ini, serta memberikan informasi yang lebih baik kepada penduduk mengenai fasilitas yang tersedia. Ini dapat menjadi masukan penting bagi pihak terkait untuk melakukan evaluasi dan perbaikan yang diperlukan agar seluruh penduduk dapat merasakan manfaat dari infrastruktur dan fasilitas umum yang ada."
    )
    
    analisis
  })
  
  
}

render_server_aspek_spatial(TRUE)