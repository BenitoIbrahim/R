render_server_identitas <- function (params) {
  
  pathIdentitasResponden <- 'data/IdentitasResponden.csv'
  
  output$data_table_identitas <- renderDT({
    data <- read_csv(pathIdentitasResponden)
    
    data <- data %>%
      mutate(
        Jenis.kelamin = recode(
          Jenis.kelamin,
          `1` = "Perempuan",
          `2` = "Laki - Laki"
        ),
        Status.perkawinan = recode(
          Status.perkawinan,
          `1`='Belum Menikah',
          `2`='Menikah',
          `3`='Duda/ Janda',
          `4`='Lainnya'
        ),
        Apakah.memiliki.anak = recode(
          Apakah.memiliki.anak,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),
        Tingkat.pendidikan = recode(
          Tingkat.pendidikan,
          `1`='Tidak lulus SD',
          `2`='SD',
          `3`='SMP',
          `4`='SMA',
          `5`='Diploma',
          `6`='Sarjana',
          `7`='Pasca Sarjana',
          `8`='Lainnya'
        ),
        Apakah.anda.bekerja.saat.ini = recode(
          Apakah.anda.bekerja.saat.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    data <- data %>% 
      select(-Nama)
    
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
        'Jenis kelamin',	
        'Usia (tahun)',	
        'Status perkawinan',	
        'Apakah memiliki anak?',	
        'Jumlah anak (orang)',	
        'Tingkat pendidikan ',	
        'Apakah anda bekerja saat ini?',	
        'Jika bekerja, apa pekerjaan anda saat ini?',
        'Actions'
        ),
      options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  if (!$('#identitas-checkbox').length) {",
          "  $(thead).closest('thead').prepend(` 
      <tr id=\"identitas-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"jenis-kelamin\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"usia\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"status-perkawinan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"apakah-memiliki-anak\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"jumlah-anak\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pendidikan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"apakah-bekerja\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pekerjaan\">
        </th> 
      </tr>
          `);",
          "  $('#jenis-kelamin').on('click', function(){",
          "    Shiny.setInputValue('jenis_kelamin', this.checked);",
          "  });",
          "  $('#usia').on('click', function(){",
          "    Shiny.setInputValue('usia', this.checked);",
          "  });",
          "  $('#status-perkawinan').on('click', function(){",
          "    Shiny.setInputValue('status_perkawinan', this.checked);",
          "  });",
          "  $('#apakah-memiliki-anak').on('click', function(){",
          "    Shiny.setInputValue('apakah_memiliki_anak', this.checked);",
          "  });",
          "  $('#jumlah-anak').on('click', function(){",
          "    Shiny.setInputValue('jumlah_anak', this.checked);",
          "  });",
          "  $('#pendidikan').on('click', function(){",
          "    Shiny.setInputValue('pendidikan', this.checked);",
          "  });",
          "  $('#apakah-bekerja').on('click', function(){",
          "    Shiny.setInputValue('apakah_bekerja', this.checked);",
          "  });",
          "  $('#pekerjaan').on('click', function(){",
          "    Shiny.setInputValue('pekerjaan', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-identitas-responden', targets = 0),
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
  
  # Update Identitas
  loadDataIdentitasResponden <- function() {
    if (file.exists(pathIdentitasResponden)) {
      read.csv(pathIdentitasResponden)
    } else {
      data.frame(
        No = character(),
        Nama=character(),	
        Jenis.kelamin=character(),	
        Usia.tahun=character(),	
        Status.perkawinan=character(),	
        Apakah.memiliki.anak=character(),	
        Jumlah.anak.orang=character(),	
        Tingkat.pendidikan=character(),	
        Apakah.anda.bekerja.saat.ini=character(),	
        Jika.bekerja.apa.pekerjaan.anda.saat.ini=character(),
      )
    }
  }
  
  observeEvent(input$cancelIdentitas, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataIdentitasResponden()
    data <- data[data$No == id, ]
    
    updateSelectInput(session, 'Jenis.kelamin.edit', selected =data$Jenis.kelamin)
    updateNumericInput(session, 'Usia.tahun.edit', value=data$Usia.tahun)
    updateSelectInput(session, 'Status.perkawinan.edit', selected=data$Status.perkawinan)
    updateSelectInput(session, 'Apakah.memiliki.anak.edit', selected=data$Apakah.memiliki.anak)
    updateNumericInput(session, 'Jumlah.anak.orang.edit', value=data$Jumlah.anak.orang)
    updateSelectInput(session, 'Tingkat.pendidikan.edit', selected=data$Tingkat.pendidikan)
    updateSelectInput(session, 'Apakah.anda.bekerja.saat.ini.edit', selected=data$Apakah.anda.bekerja.saat.ini)
    updateTextInput(session, 'Jika.bekerja.apa.pekerjaan.anda.saat.ini.edit', value=data$Jika.bekerja.apa.pekerjaan.anda.saat.ini)
    updateTextInput(session, 'Nama_Identitas.edit', value=data$Nama)
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateIdentitas, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataIdentitasResponden()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Nama=input$Nama_Identitas.edit,	
        Jenis.kelamin=input$Jenis.kelamin.edit,	
        Usia.tahun=input$Usia.tahun.edit,	
        Status.perkawinan=input$Status.perkawinan.edit,	
        Apakah.memiliki.anak=input$Apakah.memiliki.anak.edit,	
        Jumlah.anak.orang=input$Jumlah.anak.orang.edit,	
        Tingkat.pendidikan=input$Tingkat.pendidikan.edit,	
        Apakah.anda.bekerja.saat.ini=input$Apakah.anda.bekerja.saat.ini.edit,	
        Jika.bekerja.apa.pekerjaan.anda.saat.ini=input$Jika.bekerja.apa.pekerjaan.anda.saat.ini.edit,
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
      
      temp <- paste0(pathIdentitasResponden, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathIdentitasResponden)
        render_server_identitas(FALSE)
        
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
  
  # Jenis Kelamin
  output$pie_chart_jenis_kelamin_identitas <- renderPlot({
    gender_count <- read_csv(pathIdentitasResponden) %>%
      mutate(
        Jenis.kelamin = recode(
          Jenis.kelamin,
          `1` = "Perempuan",
          `2` = "Laki - Laki"
        )
      )%>%
      count(Jenis.kelamin) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Jenis.kelamin, "\n", round(percentage, 1), "%"))
    
    ggplot(gender_count, aes(x = "", y = n, fill = Jenis.kelamin)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      labs(fill = "Jenis Kelamin", title = "Distribusi Jenis Kelamin")
  })
  
  # Pendidikan
  output$pie_chart_pendidikan_identitas <- renderPlot({
    pendidikan_count <- read_csv(pathIdentitasResponden) %>%
      mutate(
        Tingkat.pendidikan = recode(
          Tingkat.pendidikan,
          `1`='Tidak lulus SD',
          `2`='SD',
          `3`='SMP',
          `4`='SMA',
          `5`='Diploma',
          `6`='Sarjana',
          `7`='Pasca Sarjana',
          `8`='Lainnya'
        )
      )%>%
      count(Tingkat.pendidikan) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Tingkat.pendidikan, "\n", round(percentage, 1), "%"))
    
    ggplot(pendidikan_count, aes(x = "", y = n, fill = Tingkat.pendidikan)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      labs(fill = "Pendidikan", title = "Distribusi Pendidikan")
  })
  
  # Usia
  output$usia_identitas_bar_chart <- renderPlot({
    age_count <- read_csv(pathIdentitasResponden) %>%
      count(Usia.tahun)
    
    # Define a custom color palette
    custom_colors <- c("red", "green", "blue", "orange", "purple", "yellow", "pink", "brown", "cyan", "magenta")
    # Shuffle the colors
    set.seed(42) # For reproducibility
    shuffled_colors <- sample(custom_colors, length(unique(age_count$Usia.tahun)), replace = TRUE)
    
    ggplot(age_count, aes(x = factor(Usia.tahun), y = n, fill = factor(Usia.tahun))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = shuffled_colors) + # Use custom shuffled colors
      scale_y_continuous(breaks = seq(0, max(age_count$n), by = 1)) + # Ensure integer breaks
      theme_minimal() +
      labs(x = "Usia", y = "Jumlah", title = "Distribusi Usia") +
      theme(legend.position = "none") # Remove legend
  })
  
  output$barChartPerkawinanKepemilikanAnak <- renderPlot({
    
    # Filter and recode data
    modal_combined_data <- read_csv(pathIdentitasResponden) %>%
      filter(!is.na(Status.perkawinan) & !is.na(Apakah.memiliki.anak)) %>%
      mutate(
        Status.perkawinan = recode(
          Status.perkawinan,
          `1` = "Belum Menikah",
          `2`='Menikah',
          `3`='Duda/ Janda',
          `4`='Lainnya'
        ),
        Apakah.memiliki.anak = recode(
          Apakah.memiliki.anak,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      ) %>%
      filter(Status.perkawinan != "Belum Menikah")
    
    # Convert to factors
    modal_combined_data$Status.perkawinan <- as.factor(modal_combined_data$Status.perkawinan)
    modal_combined_data$Apakah.memiliki.anak <- as.factor(modal_combined_data$Apakah.memiliki.anak)
    
    # Create data frame for plotting
    modal_combined_count <- modal_combined_data %>%
      group_by(Status.perkawinan, Apakah.memiliki.anak) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Define number of bars and colors
    num_bars <- nrow(modal_combined_count)
    colors <- brewer.pal(min(num_bars, 3), "Dark2")
    
    # Plot the data
    ggplot(modal_combined_count, aes(x = Status.perkawinan, y = count, fill = Apakah.memiliki.anak)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Status Perkawinan dan Kepemilikan Anak",
           x = "Status Perkawinan",
           y = "Jumlah Responden",
           fill = "Apakah memiliki anak?") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(modal_combined_count$count), by = 1)) # Adding manual breaks for y-axis
  })
  
  output$analysisTextPerkawinanKepemilikanAnak <- renderText({
    modal_combined_data <- read_csv(pathIdentitasResponden) %>%
      filter(!is.na(Status.perkawinan) & !is.na(Apakah.memiliki.anak)) %>%
      mutate(
        Status.perkawinan = recode(
          Status.perkawinan,
          `1` = "Belum Menikah",
          `2`='Menikah',
          `3`='Duda/ Janda',
          `4`='Lainnya'
        ),
        Apakah.memiliki.anak = recode(
          Apakah.memiliki.anak,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    
  total_responden <- nrow(modal_combined_data)
  
  belumMenikah <- modal_combined_data %>%
    filter(Status.perkawinan == "Belum Menikah") %>%
    nrow()
  
  menikah <- modal_combined_data %>%
    filter(Status.perkawinan == "Menikah") %>%
    nrow()
  
  dudaJanda <- modal_combined_data %>%
    filter(Status.perkawinan == "Duda/ Janda") %>%
    nrow()
  
  statusLainnya <- modal_combined_data %>%
    filter(Status.perkawinan == "Lainnya") %>%
    nrow()
  
  menikahPunyaAnak <- modal_combined_data %>%
    filter(Status.perkawinan == "Menikah" & Apakah.memiliki.anak == "Ya") %>%
    nrow()
  
  menikahBelumPunyaAnak <- modal_combined_data %>%
    filter(Status.perkawinan == "Menikah" & Apakah.memiliki.anak == "Tidak") %>%
    nrow()
  
  rataRataPunyaAnak <- mean(modal_combined_data$Jumlah.anak.orang)
    
  
  paste0("Dari total ", total_responden," responden, Terdapat : ", belumMenikah, "(Belum Menikah), ", menikah, "(Sudah Menikah), ", dudaJanda, "(Duda/Janda), dan ", statusLainnya, "(Status Perkawinan Lainnya). Dari hasil analisis terdapat masyrakat dengan status menikah yang sudah memiliki anak yaitu sebanyak ", menikahPunyaAnak , " pasangan. Sedangkan yang belum dikaruniai anak yaitu sebanyak ", menikahBelumPunyaAnak, " pasangan. Rata rata kepemilikan anak di desa ini yaitu sebanyak ", round(rataRataPunyaAnak), ".")
  })
  
  # Pekerjaan
  output$barChartPekerjaanIdentitas <- renderPlot({
    
    # Filter and recode data
    modal_combined_data <- read_csv(pathIdentitasResponden) %>%
      filter(!is.na(Apakah.anda.bekerja.saat.ini) & !is.na(Jika.bekerja.apa.pekerjaan.anda.saat.ini)) %>%
      mutate(
        Apakah.anda.bekerja.saat.ini = recode(
          Apakah.anda.bekerja.saat.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    # Convert to factors
    modal_combined_data$Apakah.anda.bekerja.saat.ini <- as.factor(modal_combined_data$Apakah.anda.bekerja.saat.ini)
    modal_combined_data$Jika.bekerja.apa.pekerjaan.anda.saat.ini <- as.factor(modal_combined_data$Jika.bekerja.apa.pekerjaan.anda.saat.ini)
    
    # Create data frame for plotting
    modal_combined_count <- modal_combined_data %>%
      group_by(Apakah.anda.bekerja.saat.ini, Jika.bekerja.apa.pekerjaan.anda.saat.ini) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Define number of bars and colors
    num_bars <- nrow(modal_combined_count)
    colors <- brewer.pal(min(num_bars, 3), "Dark2")
    
    # Plot the data
    ggplot(modal_combined_count, aes(x =Jika.bekerja.apa.pekerjaan.anda.saat.ini , y = count, fill = Apakah.anda.bekerja.saat.ini)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Pekerjaan",
           x = "Nama pekerjaan",
           y = "Jumlah Responden",
           fill = "Apakah Sedang bekerja?") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(modal_combined_count$count), by = 1)) # Adding manual breaks for y-axis
  })
  
  output$analysisTextPekerjaanIdentitas <- renderText({
    # Filter and recode data
    modal_combined_data <- read_csv(pathIdentitasResponden) %>%
      filter(!is.na(Apakah.anda.bekerja.saat.ini) & !is.na(Jika.bekerja.apa.pekerjaan.anda.saat.ini)) %>%
      mutate(
        Apakah.anda.bekerja.saat.ini = recode(
          Apakah.anda.bekerja.saat.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    total_responden <- nrow(modal_combined_data)
    
    sudahBejekerja <- modal_combined_data %>%
      filter(Apakah.anda.bekerja.saat.ini == "Ya")%>%
      nrow()
    
    belumBekerja <- modal_combined_data %>%
      filter(Apakah.anda.bekerja.saat.ini == "Tidak")%>%
      nrow()
    
    mayoritasPekerjaan <- modal_combined_data %>%
      count(Jika.bekerja.apa.pekerjaan.anda.saat.ini, sort = TRUE)
    
    mayoritasPekerjaan <- mayoritasPekerjaan$Jika.bekerja.apa.pekerjaan.anda.saat.ini[1]
    
    paste0("Dari total ", total_responden," responden, Terdapat : ", sudahBejekerja, " yang sudah bekerja, ", belumBekerja, " yang belum bekerja. Berdasarkan data yang ada, pekerjaan masyarakat saat ini adalah sebagai ", mayoritasPekerjaan)
  })
}

render_server_identitas(TRUE)