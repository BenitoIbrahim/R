tabItemTambahData <- tabItem(
  tabName = "tambahdata",
  fluidRow(
    box(
      title = h3(tags$b("Tambah Data")),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      
      # karakteristik
      box(
        status = "primary",
        width = 12,
        h4(tags$b("Karakteristik")),
        textInput("Nama", "Nama"),
        selectInput("jenis.kelamin", "Jenis Kelamin :",
                    c(
                      "Perempuan"="1",
                      "Laki-laki"="2"
                    ),
        ),
        textInput("Usia", "Usia"),
        selectInput("Pendidikan", "Pendidikan :",
                    c(
                      "SD"="1",
                      "SMP"="2",
                      "SMA"="3",
                      "Diploma"="4",
                      "S1"="5",
                      "S2"="6",
                      "S3"="7"
                    ),
        ),
        textInput("Pekerjaan.Utama", "Pekerjaan Utama"),
        textInput("Pekerjaan.Sampingan", "Pekerjaan Sampingan"),
        textInput("Memulai.Usaha", "Memulai Usaha"),
        textInput("Jenis.Usaha", "Jenis Usaha"),
        selectInput("skala.usaha", "Skala Usaha :",
                    c(
                      "Mikro/home industry"="1",
                      "Kecil"="2",
                      "Menengah"="3",
                      "Besar"="4"
                    ),
        ),
      ),
      
      # potensi desa
      box(
        status = "success",
        width = 12,
        h4(tags$b("Potensi Desa")),

        textInput("Jenis.potensi", "Jenis potensi"),	
        textInput("Bidang", "Bidang"),	
        textInput("Jumlah.satuan", "Jumlah satuan")
      ),
      
      # pendanan
      box(
        status = "warning",
        width = 12,h4(tags$b("Pendanaan Desa")),
        selectInput("Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha", "Apakah Bapak/Ibu tahu mengenai pendanaan untuk mengembangkan usaha? :",
                    c(
                      "Tahu"="1",
                      "Tidak Tahu"="2"
                    ),
        ),
        selectInput("Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa", "Apakah Bapak/Ibu tahu yang dimaksud dengan dana desa? :",
                    c(
                      "Tahu"="1",
                      "Tidak Tahu"="2"
                    ),
        ),
        selectInput("Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility", "Apakah Bapak/Ibu tahu yang dimaksud dengan dana CSR (Coorporate Social Responsibility)? :",
                    c(
                      "Tahu"="1",
                      "Tidak Tahu"="2"
                    ),
        ),
        selectInput("Modal.Usaha.Bapak.Ibu.diperoleh.dari", "Modal Usaha Bapak/Ibu diperoleh dari :",
                    c(
                      "Modal sendiri"="1",
                      "Pinjam saudara"="2",
                      "Pinjam bank"="3",
                      "Bantuan desa"="4",
                      "Bantuan dana CSR"="5",
                      "Lainnya"="6"
                    ),
        ),
        textInput("Modal.awal", "Modal awal (Rp)"),
        selectInput("Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik", "Apakah Bapak/Ibu mengetahui adanya perusahaan listrik? :",
                    c(
                      "Tahu"="1",
                      "Tidak Tahu"="2"
                    ),
        ),
        textInput("Jika.tahu.sudah.berapa.lama.perusahaan.beraktifitas.tahun", "Jika tahu, sudah berapa lama perusahaan beraktifitas? (tahun)"),
        selectInput("Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa", "Apakah perusahaan memberikan bantuan buat masyarakat desa? :",
                    c(
                      "Tahu"="1",
                      "Tidak Tahu"="2"
                    ),
        ),
        selectInput("dalam.bentuk", "Jika ya, dalam bentuk :",
                    c(
                      "Modal sendiri"="1",
                      "Pinjam saudara"="2",
                      "Pinjam bank"="3",
                      "Bantuan desa"="4",
                      "Bantuan dana CSR"="5",
                      "Lainnya"="6"
                    ),
        ),
        selectInput("Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat", "Apakah pemerintah desa memberikan bantuan buat masyarakat? :",
                    c(
                      "Tahu"="1",
                      "Tidak Tahu"="2"
                    ),
        ),
        selectInput("Jika.ya.dalam.bentuk", "Jika ya, dalam bentuk :",
                    c(
                      "Modal sendiri"="1",
                      "Pinjam saudara"="2",
                      "Pinjam bank"="3",
                      "Bantuan desa"="4",
                      "Bantuan dana CSR"="5",
                      "Lainnya"="6"
                    ),
        ),
      ),
      
      # peningkatan PAD
      box(
        status = "danger",
        width = 12,h4(tags$b("Peningkatan PAD")),
        selectInput("Dana.desa.digunakan.untuk.membentuk.kegiatan.pembangunan.desa.termasuk.membangun.usaha", "Dana desa digunakan untuk membentuk kegiatan pembangunan desa (termasuk membangun usaha) :",
                    c(
                      "Berperan"="3",
                      "Cukup berperan"="2",
                      "Kurang berperan"="1"
                    ),
        ),
        selectInput("Dana.desa.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan", "Dana desa digunakan untuk membangun Infrastruktur desa (misalnya : jalan)  :",
                    c(
                      "Berperan"="3",
                      "Cukup berperan"="2",
                      "Kurang berperan"="1"
                    ),
        ),
        selectInput("Dana.desa.membantu.permodalan.bagi.kegiatan.BUMDes", "Dana desa membantu permodalan bagi kegiatan BUMDes :",
                    c(
                      "Berperan"="3",
                      "Cukup berperan"="2",
                      "Kurang berperan"="1"
                    ),
        ),
        selectInput("Dana.CSR.digunakan.untuk.membentuk.kegiatan.pembangunan.desa", "Dana CSR digunakan untuk membentuk kegiatan pembangunan desa :",
                    c(
                      "Berperan"="3",
                      "Cukup berperan"="2",
                      "Kurang berperan"="1"
                    ),
        ),
        selectInput("Dana.CSR.digunakan.untuk.membangun.Infrastruktur.desa.misalnya.jalan", "Dana CSR digunakan untuk membangun Infrastruktur desa (misalnya : jalan)  :",
                    c(
                      "Berperan"="3",
                      "Cukup berperan"="2",
                      "Kurang berperan"="1"
                    ),
        ),
        selectInput("Dana.CSR.membantu.permodalan.bagi.kegiatan.BUMDes", "Dana CSR membantu permodalan bagi kegiatan BUMDes :",
                    c(
                      "Berperan"="3",
                      "Cukup berperan"="2",
                      "Kurang berperan"="1"
                    ),
        )
      ),
      
      # peningkatan Perekonomian
      box(
        status = "danger",
        width = 12,h4(tags$b("Peningkatan Perekonomian")),
        selectInput(
          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa",
          "Terbukanya usaha ekonomi rakyat, karena adanya dana desa :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        ), 
        selectInput(
          "Dana.desa.menambah.penghasilan.masyarakat",
          "Dana desa menambah penghasilan masyarakat :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        ), 
        selectInput(
          "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat",
          "Adanya Dana desa membantu mengembangkan modal untuk rakyat :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        ), 
        selectInput(
          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR",
          "Terbukanya usaha ekonomi rakyat, karena adanya dana CSR :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        ), 
        selectInput(
          "Dana.CSR.menambah.penghasilan.masyarakat",
          "Dana CSR menambah penghasilan masyarakat :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        ), 
        selectInput(
          "Adanya.Dana.CSR.membantu.mengembangkan.modal.untuk.rakyat",
          "Adanya Dana CSR membantu mengembangkan modal untuk rakyat :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        )
      ),
      
      # peningkatan Program Wisata
      box(
        status = "danger",
        width = 12,h4(tags$b("Peningkatan Program Wisata")),
        selectInput(
          "Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.desa",
          "Terdapat tempat wisata yang dikelola dengan menggunakan dana desa :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        ), 
        selectInput(
          "Terdapat.tempat.wisata.yang.dikelola.dengan.menggunakan.dana.CSR",
          "Terdapat tempat wisata yang dikelola dengan menggunakan dana CSR :"
          ,
          c(
            "Berperan" = "3",
            "Cukup berperan" = "2",
            "Kurang berperan" = "1"
          ),
        ),
        fileInput("Galeri.url", "Foto Potensi Desa", 
                  accept = c("image/png", "image/jpeg", "image/jpg"))
      ),
      actionButton("add", "Tambah Data SIKD"),
      actionButton("update", "Update", style = "display: none;"),
      actionButton("cancel", "Cancel", style = "display: none;")
    )
  )
)