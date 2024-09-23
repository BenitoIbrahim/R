tabItemTambahHapusAspek <- tabItem(
  tabName = "tambahdataAspek",
  fluidRow(
    box(
      title = h3(tags$b("Tambah Data")),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
        # Identitas
      box(
        status = "primary",
        width = 12,
        h4(tags$b("Identitas Responden")),
        selectInput(
          "Jenis.kelamin", "Jenis kelamin",
          c(
            "Perempuan"="1",
            "Laki-laki"="2"
          ),
        ),
        numericInput(
          "Usia.tahun", "Usia (tahun)",
          value = 0
          
        ),
        selectInput(
          "Status.perkawinan", "Status perkawinan",
          c(
            'Belum Menikah'='1',
            'Menikah'='2',
            'Duda/ Janda'='3',
            'Lainnya'='4'
          )
        ),
        selectInput(
          "Apakah.memiliki.anak", "Apakah memiliki anak?",
          c(
            'Ya'='1',
            'Tidak'='2',
            'Belum tahu'='3'
          )
        ),
        numericInput(
          "Jumlah.anak.orang", "Jumlah anak (orang)",
          value = 0
          
        ),
        selectInput(
          "Tingkat.pendidikan", "Tingkat pendidikan ",
          c(
            'Tidak lulus SD'='1',
            'SD'='2',
            'SMP'='3',
            'SMA'='4',
            'Diploma'='5',
            'Sarjana'='6',
            'Pasca Sarjana'='7',
            'Lainnya'='8'
          )
        ),
        selectInput(
          "Apakah.anda.bekerja.saat.ini", "Apakah anda bekerja saat ini?",
          c(
            'Ya'='1',
            'Tidak'='2',
            'Belum tahu'='3'
          )
        ),
        textInput(
          "Jika.bekerja.apa.pekerjaan.anda.saat.ini", "Jika bekerja, apa pekerjaan anda saat ini?"
          
        )
      ),
        
        # Ekonomi
        box(
          status = "warning",
          width = 12,
          h4(tags$b("Aspek Ekonomi")),
          selectInput(
            "Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini", 
            "Apakah Anda atau anggota keluarga Anda memiliki usaha sendiri di desa ini? ",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah", 
            "Berapa pendapatan bulanan Anda atau anggota keluarga Anda secara keseluruhan? (Kisaran jumlah)",
            c(
              "500.000 - 1.500.000"="1",
              "1.500.001 - 3.000.000"="2",
              "3.000.001 - 4.500.000"="3",
              "4.500.001 - ke atas"="4"
            )
          )
          ,
          selectInput(
            "Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial", 
            "Apakah Anda atau anggota keluarga Anda menerima bantuan sosial?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textInput(
            "Jika.ya.sebutkan.aspek.ekonomi", 
            "Jika ya, sebutkan"
          ),
          selectInput(
            "Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini", 
            "Bagaimana Anda menilai stabilitas harga pangan saat ini?",
            c(
              "Sangat stabil"="4",
              "Stabil"="3",
              "Tidak stabil"="2",
              "Sangat tidak stabil"="1"
            )
          ),
          selectInput(
            "Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda", 
            "Apakah Anda memiliki simpanan atau investasi di luar penghasilan bulanan Anda?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini", 
            "Apakah Anda merasa terdapat peluang ekonomi yang cukup di desa ini?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini", 
            "Bagaimana Anda menilai aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini?",
            c(
              "Sangat terbuka"="4",
              "Terbuka"="3",
              "Tidak terbuka"="2",
              "Sangat tidak terbuka"="1"
            )
          ),
          selectInput(
            "Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini", 
            "Apakah Anda merasa terdapat kesenjangan ekonomi di antara penduduk di desa ini?",
            c(
              "Sangat terasa"="4",
              "Terasa"="3",
              "Tidak terasa"="2",
              "Sangat tidak terasa"="1"
            )
          )
        ),
        
        # Sosial
        box(
          status = "danger",
          width = 12,
          h4(tags$b("Aspek Sosial")),
          selectInput(
            'Apakah.Anda.tinggal.di.desa.ini.sejak.lahir', 
            'Apakah Anda tinggal di desa ini sejak lahir?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textInput(
            'Jika.tidak.sebutkan.asal.tempat', 'Jika tidak, sebutkan asal tempat',
            
          ),
          selectInput(
            'Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini', 'Apakah Anda aktif dalam kegiatan sosial kemasyarakatan di desa ini?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa', 'Seberapa sering Anda berpartisipasi dalam pertemuan warga atau komunitas di desa?',
            
            c(
              'Sering'='4',
              'Kadang-kadang'='3',
              'Jarang'='2',
              'Tidak Pernah'='1'
            )
          ),
          selectInput(
            'Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini', 'Apakah anda merasa adanya dukungan sosial yang memadai di desa ini?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
        ),
        
        
        # Temporal
        box(
          status = "primary",
          width = 12,
          h4(tags$b("Aspek Temporal")),
          numericInput(
            'Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun', 'Berapa lama Anda sudah tinggal di desa ini? (tahun)',
            value = 0
          ),
          selectInput(
            'Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama', 'Apakah Anda berencana untuk tinggal di desa ini dalam jangka waktu yang lama?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir', 'Apakah Anda melihat perubahan signifikan dalam kondisi ekonomi dan sosial desa dalam beberapa tahun terakhir?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textAreaInput(
            'Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini', 'Apakah Anda memiliki saran atau masukan untuk meningkatkan aspek sosial-ekonomi di desa ini?',
            
          )
        ),
        
      # Spatial
      box(
        status = "warning",
        width = 12,
        h4(tags$b("Aspek Spatial")),
        numericInput(
          'Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM', 'Seberapa jauh rumah Anda dari pusat desa atau kota terdekat? (Kec. Sagaranten) .... KM',
          value = 0
        ),
        selectInput(
          'Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini', 'Bagaimana Anda biasanya mengakses fasilitas umum seperti pasar, sekolah, dan rumah sakit di desa ini?',
          c(
            'Jalan Kaki'='1',
            'Sepeda'='2',
            'Sepeda Motor'='3',
            'Mobil'='4',
            'Angkutan Umum'='5',
            'Lainnya'='6'
          )
        ),
        selectInput(
          'Apakah.transportasi.umum.cukup.memadai.di.desa.ini', 'Apakah transportasi umum cukup memadai di desa ini?',
          c(
            'Sangat memadai'='4',
            'Cukup memadai'='3',
            'Tidak cukup'='2',
            'Tidak ada'='1'
          )
        ),
        selectInput(
          'Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini', 'Bagaimana kondisi infrastruktur jalan di desa ini? ',
          c(
            'Baik'='4',
            'Cukup baik'='3',
            'Tidak cukup baik'='2',
            'Tidak ada'='1'
          )
        ),
        selectInput(
          'Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah', 'Apakah terdapat masalah terkait sanitasi di desa ini, seperti akses ke fasilitas toilet yang memadai atau masalah limbah?',
          c(
            "Ya"="1",
            "Tidak"="2",
            "Belum tahu"="3"
          )
        ),
        selectInput(
          'Bagaimana.pasokan.air.bersih.di.desa.ini', 'Bagaimana pasokan air bersih di desa ini? ',
          c(
            'Baik'='4',
            'Cukup baik'='3',
            'Tidak cukup baik'='2',
            'Tidak ada'='1'
          )
        ),
        selectInput(
          'Apakah.Anda.menghadapi.masalah.terkait.air.bersih', 'Apakah Anda menghadapi masalah terkait air bersih?',
          c(
            'Sering'='4',
            'Kadang-kadang'='3',
            'Jarang'='2',
            'Tidak Pernah'='1'
          )
        ),
        selectInput(
          'Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya', 'Apakah terdapat masalah terkait polusi di desa ini, seperti polusi udara atau pencemaran lingkungan lainnya?',
          c(
            "Ya"="1",
            "Tidak"="2",
            "Belum tahu"="3"
          )
        ),
        selectInput(
          'Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk', 'Apakah Anda merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai untuk memenuhi kebutuhan sehari-hari penduduk? ',
          c(
            "Ya"="1",
            "Tidak"="2",
            "Belum tahu"="3"
          )
        )
      ),
      
      actionButton("addAspek", "Tambah Data Aspek"),
      ),
    )
  )