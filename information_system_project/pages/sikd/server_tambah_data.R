pathTambahProfilDesa <- "data/profil_desa.csv"
pathTambahKarakteristik <- "data/Karakteristik.csv"
pathTambahPotensiDesa <- "data/PotensiDesa.csv"
pathTambahPendanaan <- "data/Pendanaan.csv"
pathTambahPeningkatanPAD <- "data/PeningkatanPAD.csv"
pathTambahPeningkatanPerekonomian <- "data/PeningkatanPerekonomianDesa.csv"
pathTambahPeningkatanProgramWisata <- "data/PeningkatanProgramWisata.csv"
pathTambahGaleriPotensiDesa <- "data/PotensiDesaURL.csv"

# # Save data to CSV
# saveDataKarakteristik <- function(data) {
#   write.csv(data, pathTambahKarakteristik, row.names = FALSE)
# }
# 
# saveDataPotensiDesa <- function(data) {
#   write.csv(data, pathTambahPotensiDesa, row.names = FALSE)
# }
# 
# saveDataKaraPendanaan <- function(data) {
#   write.csv(data, pathTambahPendanaan, row.names = FALSE)
# }
# 
# saveDataPeningkatanPAD <- function(data) {
#   write.csv(data, pathTambahPeningkatanPAD, row.names = FALSE)
# }
# 
# saveDataPeningkatanPerekonomian <- function(data) {
#   write.csv(data, pathTambahPeningkatanPerekonomian, row.names = FALSE)
# }
# 
# saveDataPeningkatanProgramWisata <- function(data) {
#   write.csv(data, pathTambahPeningkatanProgramWisata, row.names = FALSE)
# }

# Function to write data to a CSV file and check for errors
safeWriteCSV <- function(data, path) {
  tryCatch({
    write.csv(data, path, row.names = FALSE)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

resetInputs <- function() {
  updateTextInput(session, "Nama", value = "")
  updateSelectInput(session, "jenis.kelamin")
  updateTextInput(session, "Usia", value = "")
  updateSelectInput(session, "Pendidikan")
  updateTextInput(session, "Pekerjaan.Utama", value = "")
  updateTextInput(session, "Pekerjaan.Sampingan", value = "")
  updateTextInput(session, "Memulai.Usaha", value = "")
  updateTextInput(session, "Jenis.Usaha", value = "")
  updateSelectInput(session, "skala.usaha")
  
  updateTextInput(session,"Jenis.potensi", value = "")	
  updateTextInput(session,"Bidang", value = "")	
  updateTextInput(session,"Jumlah.satuan", value = "")
  
  updateSelectInput(session,"Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha")
  updateSelectInput(session,"Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa")
  updateSelectInput(session,"Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility")
  updateSelectInput(session, "Modal.Usaha.Bapak.Ibu.diperoleh.dari")
  updateTextInput(session, "Modal.awal", value = "")
  updateSelectInput(session,"Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik")
  updateTextInput(session,"Jika.tahu.sudah.berapa.lama.perusahaan.beraktifitas.tahun",value = "")
  updateSelectInput(session,"Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa")
  updateSelectInput(session, "dalam.bentuk")
  updateSelectInput(session,"Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat")
  updateSelectInput(session, "Jika.ya.dalam.bentuk")
  
  
  updateSelectInput(session, "Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha")
  updateSelectInput(session, "Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan")
  updateSelectInput(session, "Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes")
  updateSelectInput(session, "Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa")
  updateSelectInput(session, "Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan")
  updateSelectInput(session, "Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes")
  
  
  
  updateSelectInput(session,"Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa")
  updateSelectInput(session,"Dana.desa.menambah.penghasilan.masyarakat")	
  updateSelectInput(session,"Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat")
  updateSelectInput(session,"Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR")
  updateSelectInput(session,"Dana.CSR.menambah.penghasilan.masyarakat")	
  updateSelectInput(session,"Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat")
  
  updateSelectInput(session,"Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa")
  updateSelectInput(session,"Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR")
  
  session$sendCustomMessage("resetFileInput", "Galeri.url")
  
  
  shinyjs::hide("update")
  shinyjs::hide("cancel")
  shinyjs::show("add")
}

# Read CSV file
# Karakteristik
loadDataKarakteristik <- function() {
    if (file.exists(pathTambahKarakteristik)) {
      read.csv(pathTambahKarakteristik)
    } else {
      data.frame(
        No = character(),
        Nama = character(),
        jenis.kelamin = character(),
        Usia = character(),
        Pendidikan = character(),
        Pekerjaan.Utama = character(),
        Pekerjaan.Sampingan = character(),
        Memulai.Usaha = character(),
        Jenis.Usaha = character(),
        skala.usaha = character()
      )
    }
}

# Potensi Desa
loadDataPotensiDesa <- function(){
    if (file.exists(pathTambahPotensiDesa)) {
      read.csv(pathTambahPotensiDesa)
    } else {
      data.frame(
        No = integer(),
        Jenis.potensi=character(),
        Bidang=character(),
        Jumlah.satuan=character()
      )
    }
}

# Pendanaan
loadDataPendanaan <- function(){
  if (file.exists(pathTambahPendanaan)) {
    read.csv(pathTambahPendanaan)
  } else {
    data.frame(
      No = integer(),
      Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha =
        character(),
      Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa = character(),
      Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility =
        character(),
      Modal.Usaha.Bapak.Ibu.diperoleh.dari = character(),
      Modal.awal = character(),
      Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik = character(),
      Jika.tahu.sudah.berapa.lama.perusahaan.beraktifitas.tahun = character(),
      Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa = character(),
      dalam.bentuk = character(),
      Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat = character(),
      Jika.ya.dalam.bentuk = character()
    )
  }
}

# Peningkatan PAD
loadDataPeningkatanPAD <- function(){
  if (file.exists(pathTambahPeningkatanPAD)) {
    read.csv(pathTambahPeningkatanPAD)
  } else {
    data.frame(
      No = integer(),
      Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha =
        character(), 
      Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan =
        character(), 
      Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes = character(), 
      Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa =
        character(), 
      Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan =
        character(), 
      Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes = character()
    )
  }
}

# Peningkatan Perekonomian
loadDataPeningkatanPerekonomian <- function(){
  if (file.exists(pathTambahPeningkatanPerekonomian)) {
    read.csv(pathTambahPeningkatanPerekonomian)
  } else {
    data.frame(
      No = integer(),
      Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa = character(), Dana.desa.menambah.penghasilan.masyarakat =
        character(), 
      Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat =
        character(), 
      Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR = character(), 
      Dana.CSR.menambah.penghasilan.masyarakat =
        character(), 
      Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat =
        character()
    )
  }
}

# Peningkatan Program Wisata
loadDataPeningkatanProgramWisata <- function(){
  if (file.exists(pathTambahPeningkatanProgramWisata)) {
    read.csv(pathTambahPeningkatanProgramWisata)
  } else {
    data.frame(
      Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa=character(),	
      Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR=character(),
    )
  }
}

# galeri Potensi Wisata
loadDataGaleriPotensiWisata <- function(){
  if (file.exists(pathTambahGaleriPotensiDesa)) {
    read.csv(pathTambahGaleriPotensiDesa, stringsAsFactors = FALSE)
  } else {
    data.frame(
      Galeri.url=character(),
      stringsAsFactors = FALSE
    )
  }
}

# Generate new ID
# Karakteristik
generateIDKarakteristik <- reactive({
  data <- loadDataKarakteristik()
  if (nrow(data) == 0) {
    return(1)
  } else {
    return(max(data$No) + 1)
  }
})

# Potensi Desa
generateIDPotensiDesa <- reactive({
  data <- loadDataPotensiDesa()
  if (nrow(data) == 0) {
    return(1)
  } else {
    return(max(data$No) + 1)
  }
})

# Pendanaan
generateIDPendanaan <- reactive({
  data <- loadDataPendanaan()
  if (nrow(data) == 0) {
    return(1)
  } else {
    return(max(data$No) + 1)
  }
})

# Peningkatan PAD
generateIDPeningkatanPAD <- reactive({
  data <- loadDataPeningkatanPAD()
  if (nrow(data) == 0) {
    return(1)
  } else {
    return(max(data$No) + 1)
  }
})

# Peningkatan Perekonomian
generateIDPeningkatanPerekonomian <- reactive({
  data <- loadDataPeningkatanPerekonomian()
  if (nrow(data) == 0) {
    return(1)
  } else {
    return(max(data$No) + 1)
  }
})

# Peningkatan Program Wista
generateIDPeningkatanProgramWisata <- reactive({
  data <- loadDataPeningkatanProgramWisata()
  if (nrow(data) == 0) {
    return(1)
  } else {
    return(max(data$No) + 1)
  }
})

# Add data
observeEvent(input$add, {
  
  showModal(modalDialog(
    title = "Loading...",
    "Proses Penambahan Data",
    easyClose = FALSE,
    footer = NULL
  ))
  
  # Karakteristik
  new_data_karakteristik <- data.frame(
    No = ifelse(nrow(loadDataKarakteristik()) == 0, "1", max(loadDataKarakteristik()$No) + 1),
    Nama = input$Nama,
    jenis.kelamin = input$jenis.kelamin,
    Usia = input$Usia,
    Pendidikan = input$Pendidikan,
    Pekerjaan.Utama = input$Pekerjaan.Utama,
    Pekerjaan.Sampingan = input$Pekerjaan.Sampingan,
    Memulai.Usaha = input$Memulai.Usaha,
    Jenis.Usaha = input$Jenis.Usaha,
    skala.usaha = input$skala.usaha,
    stringsAsFactors = FALSE
  )
  
  data_karakteristik <- loadDataKarakteristik()
  data_karakteristik <- rbind(data_karakteristik, new_data_karakteristik)
  successKarakteristik <- safeWriteCSV(data_karakteristik, paste0(pathTambahKarakteristik, ".tmp"))
  
  # Potensi Desa
  new_data_potensi_desa <- data.frame(
    No = ifelse(is.na(max(as.integer(loadDataPotensiDesa()$No), na.rm = TRUE)), 1, max(as.integer(loadDataPotensiDesa()$No), na.rm = TRUE) + 1),
    Jenis.potensi=input$Jenis.potensi,	
    Bidang=input$Bidang,	
    Jumlah.satuan=input$Jumlah.satuan,
    stringsAsFactors = FALSE
  )
  
  data_potensi_desa <- loadDataPotensiDesa()
  data_potensi_desa <- rbind(data_potensi_desa, new_data_potensi_desa)
  successPotensiDesa <- safeWriteCSV(data_potensi_desa, paste0(pathTambahPotensiDesa, ".tmp"))
  
  # Pendanaan
  new_data_pendanaan <- data.frame(
    No = ifelse(nrow(loadDataPendanaan()) == 0, "1", max(loadDataPendanaan()$No) + 1),
    Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha = input$Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha,
    Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa = input$Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa,
    Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility =
      input$Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility,
    Modal.Usaha.Bapak.Ibu.diperoleh.dari = input$Modal.Usaha.Bapak.Ibu.diperoleh.dari,
    Modal.awal = input$Modal.awal,
    Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik = input$Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik,
    Jika.tahu.sudah.berapa.lama.perusahaan.beraktifitas.tahun = input$Jika.tahu.sudah.berapa.lama.perusahaan.beraktifitas.tahun,
    Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa = input$Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa,
    dalam.bentuk = input$dalam.bentuk,
    Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat = input$Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat,
    Jika.ya.dalam.bentuk = input$Jika.ya.dalam.bentuk,
    stringsAsFactors = FALSE
  )
  
  data_pendanaan <- loadDataPendanaan()
  data_pendanaan <- rbind(data_pendanaan, new_data_pendanaan)
  successPendanaan <- safeWriteCSV(data_pendanaan, paste0(pathTambahPendanaan, ".tmp"))
  
  # Peningkatan PAD
  new_data_peningkatan_PAD <- data.frame(
    No = ifelse(nrow(loadDataPeningkatanPAD()) == 0, "1", max(loadDataPeningkatanPAD()$No) + 1),
    Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha =
      input$Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha, Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan =
      input$Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan, Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes =
      input$Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes, Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa =
      input$Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa, Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan =
      input$Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan, Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes =
      input$Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes, 
    stringsAsFactors = FALSE
  )
  
  
  data_peningkatanPAD <- loadDataPeningkatanPAD()
  data_peningkatanPAD <- rbind(data_peningkatanPAD, new_data_peningkatan_PAD)
  successPeningkatanPAD <- safeWriteCSV(data_peningkatanPAD, paste0(pathTambahPeningkatanPAD, ".tmp"))
  
  # Peningkatan Perekonomian
  new_data_peningkatan_Perekonomian <- data.frame(
    No = ifelse(nrow(loadDataPeningkatanPerekonomian()) == 0, "1", max(loadDataPeningkatanPerekonomian()$No) + 1),
    Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa = input$Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa, 
    Dana.desa.menambah.penghasilan.masyarakat =
      input$Dana.desa.menambah.penghasilan.masyarakat, 
    Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat =
      input$Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat, 
    Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR =
      input$Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR, 
    Dana.CSR.menambah.penghasilan.masyarakat =
      input$Dana.CSR.menambah.penghasilan.masyarakat, 
    Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat =
      input$Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat,
    stringsAsFactors = FALSE
  )
  
  
  data_peningkatanPerekonomian <- loadDataPeningkatanPerekonomian()
  data_peningkatanPerekonomian <- rbind(data_peningkatanPerekonomian, new_data_peningkatan_Perekonomian)
  successPeningkatanPerekonomian <- safeWriteCSV(data_peningkatanPerekonomian, paste0(pathTambahPeningkatanPerekonomian, ".tmp"))
  
  
  # Peningkatan Program Wista
  new_data_peningkatan_program_wisata <- data.frame(
    No = ifelse(nrow(loadDataPeningkatanProgramWisata()) == 0, "1", max(loadDataPeningkatanProgramWisata()$No) + 1),
    Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa=input$Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa,	
    Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR=input$Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR,
    stringsAsFactors = FALSE
  )
  
  
  data_peningkatanProgramWisata <- loadDataPeningkatanProgramWisata()
  data_peningkatanProgramWisata <- rbind(data_peningkatanProgramWisata, new_data_peningkatan_program_wisata)
  successPeningkatanProgramWisata <- safeWriteCSV(data_peningkatanProgramWisata, paste0(pathTambahPeningkatanProgramWisata, ".tmp"))
  
  file_info <- input$Galeri.url
  file_name <- ""
  if (!is.null(file_info)){
    file_path <- file_info$datapath
    file_name <- file_info$name
    
    save_path <- file.path("www", file_name)
    
    file.copy(file_path, save_path)
  }
  
  # Potensi Wisata
  new_data_galeri_potensi_wisata <- data.frame(
    No = ifelse(nrow(loadDataGaleriPotensiWisata()) == 0, "1", max(loadDataGaleriPotensiWisata()$No) + 1),
    Galeri.url = file_name
  )
  
  data_galeri_potensi_wisata <- loadDataGaleriPotensiWisata()
  data_galeri_potensi_wisata <- rbind(data_galeri_potensi_wisata, new_data_galeri_potensi_wisata)
  succes_galeri_potensi_wisata <- safeWriteCSV(data_galeri_potensi_wisata, paste0(pathTambahGaleriPotensiDesa, ".tmp"))
  
  if (successKarakteristik && successPotensiDesa && successPendanaan && successPeningkatanPAD && successPeningkatanPerekonomian && successPeningkatanProgramWisata && succes_galeri_potensi_wisata) {
    
    file.rename(paste0(pathTambahKarakteristik, ".tmp"), pathTambahKarakteristik)
    file.rename(paste0(pathTambahPotensiDesa, ".tmp"), pathTambahPotensiDesa)
    file.rename(paste0(pathTambahPendanaan, ".tmp"), pathTambahPendanaan)
    file.rename(paste0(pathTambahPeningkatanPAD, ".tmp"), pathTambahPeningkatanPAD)
    file.rename(paste0(pathTambahPeningkatanPerekonomian, ".tmp"), pathTambahPeningkatanPerekonomian)
    file.rename(paste0(pathTambahPeningkatanProgramWisata, ".tmp"), pathTambahPeningkatanProgramWisata)
    file.rename(paste0(pathTambahGaleriPotensiDesa, ".tmp"), pathTambahGaleriPotensiDesa)
    
    resetInputs()
    render_server_karakteristik(TRUE)
    render_server_pendanaan(TRUE)
    render_server_peningkatan_PAD(TRUE)
    render_server_peningkatan_perekonomian(TRUE)
    render_server_peningkatan_wisata(TRUE)
    render_server_potensi_desa(TRUE)
    
    removeModal()
    
    showModal(modalDialog(
      title = "Success",
      "Data berhasil ditambahkan.",
      easyClose = TRUE,
      footer = NULL
    ))
    # session$reload()
  } else {
    unlink(paste0(pathTambahKarakteristik, ".tmp"))
    unlink(paste0(pathTambahPotensiDesa, ".tmp"))
    unlink(paste0(pathTambahPendanaan, ".tmp"))
    unlink(paste0(pathTambahPeningkatanPAD, ".tmp"))
    unlink(paste0(pathTambahPeningkatanPerekonomian, ".tmp"))
    unlink(paste0(pathTambahPeningkatanProgramWisata, ".tmp"))
    unlink(paste0(pathTambahGaleriPotensiDesa, ".tmp"))
    
    removeModal()
    
    showModal(modalDialog(
      title = "Error",
      "Gagal menambahkan data.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
  
})

# Delete data
observeEvent(input$delete_id, {
  
  showModal(modalDialog(
    title = "Loading...",
    "Proses Penghapusan Data",
    easyClose = FALSE,
    footer = NULL
  ))
  
  No <- as.integer(input$delete_id)

  data_karakteristik <- loadDataKarakteristik()
  data_potensi_desa <- loadDataPotensiDesa()
  data_pendanaan <- loadDataPendanaan()
  data_peningkatan_PAD <- loadDataPeningkatanPAD()
  data_peningkatan_perekonomian <- loadDataPeningkatanPerekonomian()
  data_peningkatan_program_wisata <- loadDataPeningkatanProgramWisata()

  data_karakteristik <- data_karakteristik[data_karakteristik$No != No, ]
  data_potensi_desa <- data_potensi_desa[data_potensi_desa$No != No, ]
  data_pendanaan <- data_pendanaan[data_pendanaan$No != No, ]
  data_peningkatan_PAD <- data_peningkatan_PAD[data_peningkatan_PAD$No != No, ]
  data_peningkatan_perekonomian <- data_peningkatan_perekonomian[data_peningkatan_perekonomian$No != No, ]
  data_peningkatan_program_wisata <- data_peningkatan_program_wisata[data_peningkatan_program_wisata$No != No, ]


  successKarakteristik <- safeWriteCSV(data_karakteristik, paste0(pathTambahKarakteristik, ".tmp"))
  successPotensiDesa <- safeWriteCSV(data_potensi_desa, paste0(pathTambahPotensiDesa, ".tmp"))
  successPendanaan <- safeWriteCSV(data_pendanaan, paste0(pathTambahPendanaan, ".tmp"))
  successPeningkatanPAD <- safeWriteCSV(data_peningkatan_PAD, paste0(pathTambahPeningkatanPAD, ".tmp"))
  successPeningkatanPerekonomian <- safeWriteCSV(data_peningkatan_perekonomian, paste0(pathTambahPeningkatanPerekonomian, ".tmp"))
  successPeningkatanProgramWisata <- safeWriteCSV(data_peningkatan_program_wisata, paste0(pathTambahPeningkatanProgramWisata, ".tmp"))

  if (successKarakteristik && successPotensiDesa && successPendanaan && successPeningkatanPAD && successPeningkatanPerekonomian && successPeningkatanProgramWisata) {

    file.rename(paste0(pathTambahKarakteristik, ".tmp"), pathTambahKarakteristik)
    file.rename(paste0(pathTambahPotensiDesa, ".tmp"), pathTambahPotensiDesa)
    file.rename(paste0(pathTambahPendanaan, ".tmp"), pathTambahPendanaan)
    file.rename(paste0(pathTambahPeningkatanPAD, ".tmp"), pathTambahPeningkatanPAD)
    file.rename(paste0(pathTambahPeningkatanPerekonomian, ".tmp"), pathTambahPeningkatanPerekonomian)
    file.rename(paste0(pathTambahPeningkatanProgramWisata, ".tmp"), pathTambahPeningkatanProgramWisata)
    
    render_server_karakteristik(TRUE)
    render_server_pendanaan(TRUE)
    render_server_peningkatan_PAD(TRUE)
    render_server_peningkatan_perekonomian(TRUE)
    render_server_peningkatan_wisata(TRUE)
    render_server_potensi_desa(TRUE)
    
    removeModal()
    
    showModal(modalDialog(
      title = "Success",
      "Data berhasil dihapus.",
      easyClose = TRUE,
      footer = NULL
    ))

  } else {
    unlink(paste0(pathTambahKarakteristik, ".tmp"))
    unlink(paste0(pathTambahPotensiDesa, ".tmp"))
    unlink(paste0(pathTambahPendanaan, ".tmp"))
    unlink(paste0(pathTambahPeningkatanPAD, ".tmp"))
    unlink(paste0(pathTambahPeningkatanPerekonomian, ".tmp"))
    unlink(paste0(pathTambahPeningkatanProgramWisata, ".tmp"))

    showModal(modalDialog(
      title = "Error",
      "Gagal menghapus data.",
      easyClose = TRUE,
      footer = NULL
    ))
  }

})

observeEvent(input$delete_galeri_id, {
  
  showModal(modalDialog(
    title = "Loading...",
    "Proses Penghapusan Data",
    easyClose = FALSE,
    footer = NULL
  ))
  
  No <- as.integer(input$delete_galeri_id)
  data <- loadDataGaleriPotensiWisata()
  
  data <- data[data$No != No, ]
  
  succesGaleri <- safeWriteCSV(data, paste0(pathTambahGaleriPotensiDesa, ".tmp"))
  
  if (succesGaleri ) {
    
    file.rename(paste0(pathTambahGaleriPotensiDesa, ".tmp"), pathTambahGaleriPotensiDesa)
    
    render_server_potensi_desa(TRUE)
    
    removeModal()
    
    showModal(modalDialog(
      title = "Success",
      "Data berhasil dihapus.",
      easyClose = TRUE,
      footer = NULL
    ))
    
  } else {
    unlink(paste0(pathTambahGaleriPotensiDesa, ".tmp"))
    
    showModal(modalDialog(
      title = "Error",
      "Gagal menghapus data.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
})
