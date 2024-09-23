
pathIdentitasResponden <- 'data/IdentitasResponden.csv'
pathTambahAspekEkonomi <- "data/AspekEkonomi.csv"
pathTambahAspekSosial <- 'data/AspekSosial.csv'
pathTambahAspekTemporal <- 'data/AspekTemporal.csv'
pathTambahAspekSpatial <- 'data/AspekSpatial.csv'

safeWriteCSV <- function(data, path) {
  tryCatch({
    write.csv(data, path, row.names = FALSE)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

resetInputsAspek <- function() {
  
  updateSelectInput(session, 'Jenis.kelamin')
  updateNumericInput(session, 'Usia.tahun', value=0)
  updateSelectInput(session, 'Status.perkawinan')
  updateSelectInput(session, 'Apakah.memiliki.anak')
  updateNumericInput(session, 'Jumlah.anak.orang', value=0)
  updateSelectInput(session, 'Tingkat.pendidikan')
  updateSelectInput(session, 'Apakah.anda.bekerja.saat.ini')
  updateTextInput(session, 'Jika.bekerja.apa.pekerjaan.anda.saat.ini', value="")
  
  updateSelectInput(
    session,
    'Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah'
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial'
  )	
  updateTextInput(session, 'Jika.ya.sebutkan.aspek.ekonomi', value = "")	
  updateSelectInput(
    session,
    'Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini'
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda'
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini'
  )
  
  
  updateSelectInput(
    session,
    'Apakah.Anda.tinggal.di.desa.ini.sejak.lahir'
  )	
  updateTextInput(
    session,
    'Jika.tidak.sebutkan.asal.tempat',
    value = ""
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa'
  )	
  updateSelectInput(
    session,
    'Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini'
  )
  
  
  updateTextInput(
    session,
    'Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun',
    value = ""
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama'
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir'
  )	
  updateTextAreaInput(
    session,
    'Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini',
    value = ""
  )
  
  
  updateNumericInput(
    session,
    'Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM'
  )	
  updateSelectInput(
    session,
    'Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Apakah.transportasi.umum.cukup.memadai.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah'
  )	
  updateSelectInput(
    session,
    'Bagaimana.pasokan.air.bersih.di.desa.ini'
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.menghadapi.masalah.terkait.air.bersih'
  )	
  updateSelectInput(
    session,
    'Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya'
  )	
  updateSelectInput(
    session,
    'Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk'
  )
}

# Update Identitas
loadDataIdentitasResponden <- function() {
  if (file.exists(pathIdentitasResponden)) {
    read.csv(pathIdentitasResponden, stringsAsFactors = FALSE)
  } else {
    data.frame(
      No = character(),
      Jenis.kelamin=character(),	
      Usia.tahun=character(),	
      Status.perkawinan=character(),	
      Apakah.memiliki.anak=character(),	
      Jumlah.anak.orang=character(),	
      Tingkat.pendidikan=character(),	
      Apakah.anda.bekerja.saat.ini=character(),	
      Jika.bekerja.apa.pekerjaan.anda.saat.ini=character(), stringsAsFactors = FALSE
    )
  }
}

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

# Update Temporal
loadDataAspekTemporal <- function() {
  if (file.exists(pathTambahAspekTemporal)) {
    read.csv(pathTambahAspekTemporal, stringsAsFactors = FALSE)
  } else {
    data.frame(
      No = character(),
      Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun = character(), Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama =
        character(), Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir =
        character(), Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini = character(), stringsAsFactors = FALSE
    )
  }
}

# Update Spatial
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
        character(), stringsAsFactors = FALSE
    )
  }
}

observeEvent(input$addAspek, {
  
  showModal(modalDialog(
    title = "Loading...",
    "Proses Penambahan Data",
    easyClose = FALSE,
    footer = NULL
  ))
  
  dataIdentitasResponden <- loadDataIdentitasResponden()
  dataAspekEkonomi <- loadDataAspekEkonomi()
  dataAspekSosial <- loadDataAspekSosial()
  dataAspekTemporal <- loadDataAspekTemporal()
  dataAspekSpatial <- loadDataAspekSpatial()
  
  new_dataIdentitasResponden <- data.frame(
    No = ifelse(nrow(loadDataIdentitasResponden()) == 0, "1", max(loadDataIdentitasResponden()$No) + 1),
    Jenis.kelamin=input$Jenis.kelamin,	
    Usia.tahun=input$Usia.tahun,	
    Status.perkawinan=input$Status.perkawinan,	
    Apakah.memiliki.anak=input$Apakah.memiliki.anak,	
    Jumlah.anak.orang=input$Jumlah.anak.orang,	
    Tingkat.pendidikan=input$Tingkat.pendidikan,	
    Apakah.anda.bekerja.saat.ini=input$Apakah.anda.bekerja.saat.ini,	
    Jika.bekerja.apa.pekerjaan.anda.saat.ini=input$Jika.bekerja.apa.pekerjaan.anda.saat.ini,
    stringsAsFactors = FALSE
  )
  
  new_dataAspekEkonomi <- data.frame(
    No = ifelse(nrow(loadDataIdentitasResponden()) == 0, "1", max(loadDataIdentitasResponden()$No) + 1),
    Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini = input$Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini, 
    Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah = input$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah, 
    Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial = input$Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial, 
    Jika.ya.sebutkan.aspek.ekonomi = input$Jika.ya.sebutkan.aspek.ekonomi, 
    Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini = input$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini, 
    Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda = input$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda, 
    Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini = input$Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini, 
    Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini = input$Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini, 
    Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini = input$Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini, 
    stringsAsFactors = FALSE
  )
  
  new_dataAspekSosial <- data.frame(
    No = ifelse(nrow(loadDataAspekSosial()) == 0, "1", max(loadDataAspekSosial()$No) + 1),
    Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = input$Apakah.Anda.tinggal.di.desa.ini.sejak.lahir, 
    Jika.tidak.sebutkan.asal.tempat =
      input$Jika.tidak.sebutkan.asal.tempat, 
    Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini =
      input$Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini, 
    Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa =
      input$Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa, 
    Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini =
      input$Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini, 
    stringsAsFactors = FALSE
  )
  
  new_dataAspekTemporal <- data.frame(
    No = ifelse(nrow(loadDataAspekTemporal()) == 0, "1", max(loadDataAspekTemporal()$No) + 1),
    Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun = input$Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun, 
    Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama =
      input$Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama, 
    Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir =
      input$Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir, 
    Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini = input$Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini, 
    stringsAsFactors = FALSE
  )
  
  new_dataAspekSpatial <- data.frame(
    No = ifelse(nrow(loadDataAspekSpatial()) == 0, "1", max(loadDataAspekSpatial()$No) + 1),
    Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM =
      input$Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM, 
    Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini =
      input$Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini,
    Apakah.transportasi.umum.cukup.memadai.di.desa.ini =
      input$Apakah.transportasi.umum.cukup.memadai.di.desa.ini, 
    Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini =
      input$Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini, 
    Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah =
      input$Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah, 
    Bagaimana.pasokan.air.bersih.di.desa.ini =
      input$Bagaimana.pasokan.air.bersih.di.desa.ini, 
    Apakah.Anda.menghadapi.masalah.terkait.air.bersih =
      input$Apakah.Anda.menghadapi.masalah.terkait.air.bersih, 
    Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya =
      input$Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya, 
    Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk =
      input$Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk, 
    
    stringsAsFactors = FALSE
  )
  
  saveIdentitas <- safeWriteCSV(rbind(dataIdentitasResponden, new_dataIdentitasResponden), paste0(pathIdentitasResponden, ".tmp"))
  saveEkonomi <- safeWriteCSV(rbind(dataAspekEkonomi, new_dataAspekEkonomi), paste0(pathTambahAspekEkonomi, ".tmp"))
  saveSosial <- safeWriteCSV(rbind(dataAspekSosial, new_dataAspekSosial), paste0(pathTambahAspekSosial, ".tmp"))
  saveTemporal <- safeWriteCSV(rbind(dataAspekTemporal, new_dataAspekTemporal), paste0(pathTambahAspekTemporal, ".tmp"))
  saveSpatial <- safeWriteCSV(rbind(dataAspekSpatial, new_dataAspekSpatial), paste0(pathTambahAspekSpatial, ".tmp"))
  

  if(saveIdentitas && saveEkonomi && saveSosial && saveTemporal && saveSpatial){
    
    file.rename(paste0(pathIdentitasResponden, ".tmp"), pathIdentitasResponden)
    file.rename(paste0(pathTambahAspekEkonomi, ".tmp"), pathTambahAspekEkonomi)
    file.rename(paste0(pathTambahAspekSosial, ".tmp"), pathTambahAspekSosial)
    file.rename(paste0(pathTambahAspekTemporal, ".tmp"), pathTambahAspekTemporal)
    file.rename(paste0(pathTambahAspekSpatial, ".tmp"), pathTambahAspekSpatial)
    
    resetInputsAspek()
    
    render_server_identitas(TRUE)
    render_server_aspek_ekonomi(TRUE)
    render_server_aspek_sosial(TRUE)
    render_server_aspek_temporal(TRUE)
    render_server_aspek_spatial(TRUE)
    
    removeModal()
    
    showModal(modalDialog(
      title = "Success",
      "Data berhasil ditambahkan.",
      easyClose = TRUE,
      footer = NULL
    ))
    # 
    # session$reload()
  } else {
    unlink(paste0(pathIdentitasResponden, ".tmp"))
    unlink(paste0(pathTambahAspekEkonomi, ".tmp"))
    unlink(paste0(pathTambahAspekSosial, ".tmp"))
    unlink(paste0(pathTambahAspekTemporal, ".tmp"))
    unlink(paste0(pathTambahAspekSpatial, ".tmp"))
    
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
observeEvent(input$delete_aspek_id, {
  
  showModal(modalDialog(
    title = "Loading...",
    "Proses Penghapusan Data",
    easyClose = FALSE,
    footer = NULL
  ))
  
  No <- as.integer(input$delete_aspek_id)
  
  dataIdentitasResponden <- loadDataIdentitasResponden()
  dataAspekEkonomi <- loadDataAspekEkonomi()
  dataAspekSosial <- loadDataAspekSosial()
  dataAspekTemporal <- loadDataAspekTemporal()
  dataAspekSpatial <- loadDataAspekSpatial()
  
  dataIdentitasResponden <- dataIdentitasResponden[dataIdentitasResponden$No != No, ]
  dataAspekEkonomi <- dataAspekEkonomi[dataAspekEkonomi$No != No, ]
  dataAspekSosial <- dataAspekSosial[dataAspekSosial$No != No, ]
  dataAspekTemporal <- dataAspekTemporal[dataAspekTemporal$No != No, ]
  dataAspekSpatial <- dataAspekSpatial[dataAspekSpatial$No != No, ]
  
  
  saveIdentitas <- safeWriteCSV(dataIdentitasResponden, paste0(pathIdentitasResponden, ".tmp"))
  saveEkonomi <- safeWriteCSV(dataAspekEkonomi, paste0(pathTambahAspekEkonomi, ".tmp"))
  saveSosial <- safeWriteCSV(dataAspekSosial, paste0(pathTambahAspekSosial, ".tmp"))
  saveTemporal <- safeWriteCSV(dataAspekTemporal, paste0(pathTambahAspekTemporal, ".tmp"))
  saveSpatial <- safeWriteCSV(dataAspekSpatial, paste0(pathTambahAspekSpatial, ".tmp"))
  
  if(saveIdentitas && saveEkonomi && saveSosial && saveTemporal && saveSpatial){
    
    file.rename(paste0(pathIdentitasResponden, ".tmp"), pathIdentitasResponden)
    file.rename(paste0(pathTambahAspekEkonomi, ".tmp"), pathTambahAspekEkonomi)
    file.rename(paste0(pathTambahAspekSosial, ".tmp"), pathTambahAspekSosial)
    file.rename(paste0(pathTambahAspekTemporal, ".tmp"), pathTambahAspekTemporal)
    file.rename(paste0(pathTambahAspekSpatial, ".tmp"), pathTambahAspekSpatial)
    
    render_server_identitas(TRUE)
    render_server_aspek_ekonomi(TRUE)
    render_server_aspek_sosial(TRUE)
    render_server_aspek_temporal(TRUE)
    render_server_aspek_spatial(TRUE)
    
    removeModal()
    
    showModal(modalDialog(
      title = "Success",
      "Data berhasil dihapus",
      easyClose = TRUE,
      footer = NULL
    ))
    
  } else {
    unlink(paste0(pathIdentitasResponden, ".tmp"))
    unlink(paste0(pathTambahAspekEkonomi, ".tmp"))
    unlink(paste0(pathTambahAspekSosial, ".tmp"))
    unlink(paste0(pathTambahAspekTemporal, ".tmp"))
    unlink(paste0(pathTambahAspekSpatial, ".tmp"))
    
    removeModal()
    
    showModal(modalDialog(
      title = "Error",
      "Gagal menghapus data.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
})