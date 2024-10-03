# pages/korelasi/server_korelasi.R

# Load required libraries
library(ggplot2)
library(dplyr)

# Fungsi untuk menghitung korelasi
calculate_correlation <- function(data, var1, var2) {
  cor(data[[var1]], data[[var2]], use = "pairwise.complete.obs")
}

# Fungsi untuk membuat plot korelasi
create_correlation_plot <- function(data, var1, var2, title) {
  ggplot(data, aes_string(x = var1, y = var2)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal()
}

# Fungsi untuk menghasilkan penjelasan dinamis
create_correlation_explanation <- function(data, var1, var2) {
  # Menghitung koefisien korelasi
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Buat penjelasan teks
  explanation <- paste(
    "Hasil analisis menunjukkan bahwa korelasi antara",
    var1,
    "dan",
    var2,
    "adalah",
    direction,
    if (strength == "lemah")
      ". Namun, ",
    "dengan kekuatan yang",
    strength,
    "(",
    round(correlation, 2),
    ").",
    "Ini menunjukkan bahwa adanya dana desa",
    if (direction == "positif")
      "berpotensi besar untuk membuka"
    else
      "mungkin kurang efektif dalam membuka",
    "peluang usaha ekonomi rakyat."
  )
  
  return(explanation)
}

# Server logic for correlation
output$korelasi1Plot <- renderPlot({
  # Korelasi 1: Potensi Desa dan Pembangunan Ekonomi
  # Karena data potensi desa bersifat kategorikal, kita akan menggunakan bar plot
  plot_width <- session$clientData$output_korelasi1Plot_width
  nrow_legend <- reactive({
    if (plot_width > 1000) {
      return(15)  
    } else if (plot_width > 800) {
      return(35)  
    } else {
      return(50)  
    }
  })
  ggplot(potensi_desa, aes(x = Bidang, fill = Jenis.potensi)) +
    geom_bar() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")+
    guides(fill = guide_legend(nrow = nrow_legend()))
}, height = reactive({
  plot_width <- session$clientData$output_korelasi1Plot_width
  
  if (plot_width > 1000) {
    return(800)  
  } else if (plot_width > 800) {
    return(1000)  
  } else {
    return(1500)  
  }
}))

output$korelasi1Explanation <- renderText({
  Jenis.potensi <- "Jenis.potensi"
  Bidang <- "Bidang"
  summary <- potensi_desa %>%
    group_by(Bidang, Jenis.potensi) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  # Membuat penjelasan teks
  explanation <- paste(
    "Distribusi potensi desa berdasarkan Bidang dan Jenis potensi menunjukkan bahwa kategori yang paling dominan adalah",
    summary[[Jenis.potensi]][1], "di bidang", summary[[Bidang]][1], 
    "dengan jumlah", summary$count[1], "."
  )
})

output$korelasi2Plot <- renderPlot({
  # Korelasi 2: Peluang Ekonomi dan Bantuan Modal
  create_correlation_plot(peningkatan_perekonomian, 
                          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa", 
                          "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat",
                          "Korelasi 2: Peluang Ekonomi dan Bantuan Modal")
})

output$korelasi2Explanation <- renderText({
  var1 <- "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa"
  var2 <- "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat"
  data <- peningkatan_perekonomian
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Buat penjelasan teks
  explanation <- paste(
    "Hasil analisis menunjukkan bahwa korelasi antara",
    var1,
    "dan",
    var2,
    "adalah",
    direction,
    if (strength == "lemah")
      ". Namun, ",
    "dengan kekuatan yang",
    strength,
    "(",
    round(correlation, 2),
    ").",
    "Ini menunjukkan bahwa adanya dana desa",
    if (direction == "positif")
      "berpotensi besar untuk membuka"
    else
      "mungkin kurang efektif dalam membuka",
    "peluang usaha ekonomi rakyat."
  )
})

output$korelasi3Plot <- renderPlot({
  # Korelasi 3: Stabilitas Harga Pangan dan Pendapatan Masyarakat
  # Karena kita tidak memiliki data tentang stabilitas harga pangan, kita akan menggunakan data yang tersedia
  create_correlation_plot(peningkatan_perekonomian, 
                          "Dana.desa.menambah.penghasilan.masyarakat", 
                          "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat",
                          "Korelasi 3: Penghasilan dan Modal Masyarakat")
})

output$korelasi3Explanation <- renderText({
  var1 <- "Dana.desa.menambah.penghasilan.masyarakat"
  var2 <- "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat"
  data <- peningkatan_perekonomian
  
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Membuat penjelasan teks yang lebih mendalam
  explanation <- paste(
    "Analisis korelasi antara variabel 'Dana Desa Menambah Penghasilan Masyarakat' dan",
    "'Adanya Dana Desa Membantu Mengembangkan Modal untuk Rakyat' menunjukkan adanya korelasi yang",
    direction,
    "dengan kekuatan yang",
    strength,
    "(nilai korelasi:",
    round(correlation, 2),
    ").",
    "Hal ini mengindikasikan bahwa",
    if (direction == "positif") {
      "peningkatan modal yang diperoleh melalui dana desa berhubungan erat dengan peningkatan penghasilan masyarakat. Ini menunjukkan bahwa ketika masyarakat memiliki akses lebih besar terhadap modal, mereka cenderung meningkatkan pendapatan mereka melalui berbagai usaha ekonomi."
    } else if (direction == "negatif") {
      "meskipun masyarakat menerima modal tambahan melalui dana desa, hal tersebut tidak serta-merta meningkatkan penghasilan mereka. Mungkin ada faktor lain, seperti kemampuan pengelolaan modal atau kondisi pasar lokal, yang mempengaruhi hasil tersebut secara negatif."
    } else {
      "tidak ada hubungan yang signifikan antara dana desa yang digunakan untuk modal dan peningkatan penghasilan masyarakat. Ini mungkin menunjukkan bahwa faktor-faktor lain, seperti pendidikan atau akses pasar, memiliki peran yang lebih dominan dalam menentukan penghasilan masyarakat."
    },
    "Penemuan ini memberikan wawasan yang penting bagi pembuat kebijakan untuk menilai bagaimana dana desa dapat lebih efektif digunakan untuk meningkatkan kesejahteraan masyarakat melalui pengembangan modal."
  )
})

output$korelasi4Plot <- renderPlot({
  # Korelasi 4: Ketersediaan Lapangan Pekerjaan dan Dana Pembangunan
  create_correlation_plot(peningkatan_perekonomian, 
                          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa", 
                          "Dana.desa.menambah.penghasilan.masyarakat",
                          "Korelasi 4: Ketersediaan Lapangan Pekerjaan dan Dana Pembangunan")
})

output$korelasi4Explanation <- renderText({
  var1 <- "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa"
  var2 <- "Dana.desa.menambah.penghasilan.masyarakat"
  data <- peningkatan_perekonomian
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Membuat penjelasan teks yang lebih mendalam
  explanation <- paste(
    "Analisis korelasi antara variabel 'Terbukanya Usaha Ekonomi Rakyat karena Adanya Dana Desa' dan",
    "'Dana Desa Menambah Penghasilan Masyarakat' menunjukkan bahwa hubungan antara kedua variabel ini",
    direction,
    "dengan kekuatan yang",
    strength,
    "(nilai korelasi:",
    round(correlation, 2),
    ").",
    "Ini mengindikasikan bahwa",
    if (direction == "positif") {
      "dana pembangunan desa yang efektif dapat membuka lapangan pekerjaan baru dan meningkatkan penghasilan masyarakat secara signifikan. Dengan dana yang tersedia, masyarakat memiliki kesempatan lebih besar untuk mengembangkan usaha ekonomi mereka, yang pada gilirannya berkontribusi pada peningkatan kesejahteraan."
    } else if (direction == "negatif") {
      "meskipun ada dana pembangunan, hal itu tidak selalu berkontribusi pada peningkatan ketersediaan lapangan pekerjaan atau penghasilan. Ini bisa disebabkan oleh faktor lain seperti distribusi dana yang tidak merata atau kurangnya keterampilan dan pengetahuan dalam mengelola usaha ekonomi."
    } else {
      "tidak ada hubungan yang signifikan antara ketersediaan dana pembangunan dan terbukanya lapangan pekerjaan atau peningkatan penghasilan masyarakat. Faktor-faktor lain mungkin memiliki pengaruh yang lebih besar, seperti kondisi ekonomi lokal atau tingkat pendidikan masyarakat."
    },
    "Penemuan ini penting untuk mengevaluasi efektivitas program pembangunan desa dan memastikan bahwa alokasi dana benar-benar digunakan untuk meningkatkan ketersediaan lapangan pekerjaan dan penghasilan masyarakat."
  )
})

output$korelasi5Plot <- renderPlot({
  # Korelasi 5: Peluang Ekonomi dan Bantuan Perusahaan
  create_correlation_plot(peningkatan_perekonomian, 
                          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR", 
                          "Dana.CSR.menambah.penghasilan.masyarakat",
                          "Korelasi 5: Peluang Ekonomi dan Bantuan Perusahaan")
})

output$korelasi5Explanation <- renderText({
  var1 <- "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR"
  var2 <- "Dana.CSR.menambah.penghasilan.masyarakat"
  data <- peningkatan_perekonomian
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Membuat penjelasan teks yang lebih mendalam
  explanation <- paste(
    "Analisis korelasi antara variabel 'Terbukanya Usaha Ekonomi Rakyat karena Adanya Dana CSR' dan",
    "'Dana CSR Menambah Penghasilan Masyarakat' menunjukkan hubungan yang",
    direction,
    "dengan kekuatan yang",
    strength,
    "(nilai korelasi:",
    round(correlation, 2),
    ").",
    "Ini mengindikasikan bahwa",
    if (direction == "positif") {
      "bantuan perusahaan melalui dana CSR dapat membuka peluang ekonomi baru yang pada akhirnya meningkatkan penghasilan masyarakat. Dana CSR yang efektif bisa menjadi katalisator penting dalam mengembangkan usaha-usaha lokal, yang berkontribusi pada kesejahteraan ekonomi masyarakat."
    } else if (direction == "negatif") {
      "meskipun perusahaan menyediakan dana CSR, hal tersebut tidak serta-merta meningkatkan penghasilan masyarakat. Ini bisa disebabkan oleh distribusi dana yang tidak tepat sasaran atau kurangnya kesiapan masyarakat dalam memanfaatkan bantuan tersebut untuk usaha ekonomi mereka."
    } else {
      "tidak ada hubungan yang signifikan antara bantuan perusahaan melalui dana CSR dan peningkatan penghasilan masyarakat. Hal ini menunjukkan bahwa faktor-faktor lain seperti dukungan teknis, pelatihan, atau akses ke pasar mungkin lebih penting dalam mendukung usaha ekonomi rakyat."
    },
    "Penemuan ini memberikan wawasan penting bagi perusahaan dan pembuat kebijakan dalam mengevaluasi efektivitas program CSR untuk memberdayakan masyarakat lokal dan meningkatkan ekonomi mereka."
  )
})

output$korelasi6Plot <- renderPlot({
  # Korelasi 6: Infrastruktur dan Aksesibilitas Fasilitas Umum
  # Karena kita tidak memiliki data spesifik tentang infrastruktur, kita akan menggunakan visualisasi alternatif
  ggplot(karakteristik, aes(x = Pekerjaan.Utama, y = Usia)) +
    geom_boxplot() +
    ggtitle("Korelasi 6: Distribusi Usia berdasarkan Pekerjaan Utama") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$korelasi6Explanation <- renderText({
  
  data <- karakteristik
  x_var <- "Pekerjaan.Utama"
  y_var <- "Usia"
  
  summary_stats <- data %>%
    group_by(!!sym(x_var)) %>%
    summarise(
      min_age = min(!!sym(y_var), na.rm = TRUE),
      q1_age = quantile(!!sym(y_var), 0.25, na.rm = TRUE),
      median_age = median(!!sym(y_var), na.rm = TRUE),
      q3_age = quantile(!!sym(y_var), 0.75, na.rm = TRUE),
      max_age = max(!!sym(y_var), na.rm = TRUE)
    )
  
  explanation <- paste(
    "Visualisasi distribusi usia berdasarkan pekerjaan utama menunjukkan perbedaan yang signifikan di antara berbagai kelompok pekerjaan. Berikut adalah ringkasan statistik untuk masing-masing kelompok:",
    paste0(
      "1. ",
      x_var,
      " '",
      summary_stats[[x_var]][1],
      "': Usia berkisar dari ",
      summary_stats$min_age[1],
      " hingga ",
      summary_stats$max_age[1],
      " dengan median usia ",
      summary_stats$median_age[1],
      "."
    ),
    paste0(
      "2. ",
      x_var,
      " '",
      summary_stats[[x_var]][2],
      "': Usia berkisar dari ",
      summary_stats$min_age[2],
      " hingga ",
      summary_stats$max_age[2],
      " dengan median usia ",
      summary_stats$median_age[2],
      "."
    ),
    paste0(
      "3. ",
      x_var,
      " '",
      summary_stats[[x_var]][3],
      "': Usia berkisar dari ",
      summary_stats$min_age[3],
      " hingga ",
      summary_stats$max_age[3],
      " dengan median usia ",
      summary_stats$median_age[3],
      "."
    ),
    "Dari hasil ini, kita dapat mengamati bahwa median usia dalam setiap kelompok pekerjaan utama berbeda, yang menunjukkan bahwa beberapa pekerjaan mungkin lebih banyak diambil oleh kelompok usia tertentu. Ini dapat memberikan wawasan tentang bagaimana distribusi usia dapat mempengaruhi aksesibilitas ke fasilitas umum, serta kebutuhan akan infrastruktur yang sesuai dengan kelompok usia tersebut.",
    sep = "\n"
  )
})

output$korelasi7Plot <- renderPlot({
  # Korelasi 7: Dampak Lingkungan dan Kualitas Hidup
  # Karena kita tidak memiliki data spesifik tentang dampak lingkungan, kita akan menggunakan visualisasi alternatif
  
  data_filtered <- karakteristik %>%
    filter(!is.na(Pendidikan), !is.na(Usia), !is.na(Jenis.Usaha))
  
  ggplot(data_filtered, aes(x = Pendidikan, y = Usia, color = Jenis.Usaha)) +
    geom_point() +
    theme_minimal()
})


output$korelasi7Explanation <- renderText({
  data_filtered <- karakteristik %>%
    mutate(
      Pendidikan = as.numeric(Pendidikan),
      Usia = as.numeric(Usia)
    )
  
  # Hitung korelasi dengan mengabaikan pasangan yang memiliki NA
  correlation_value <- cor(data_filtered$Pendidikan, data_filtered$Usia, method = "pearson", use = "pairwise.complete.obs")
  
  if (is.na(correlation_value)) {
    explanation <- "Berdasarkan data yang ada, tidak dapat dihitung korelasi yang valid antara tingkat pendidikan dan usia. Hal ini kemungkinan disebabkan oleh jumlah data yang tidak mencukupi atau karena adanya terlalu banyak nilai yang hilang (NA). Untuk mendapatkan hasil yang lebih akurat, pertimbangkan untuk melengkapi data yang hilang atau menggunakan metode imputasi untuk memperkirakan nilai yang hilang."
  } else if (correlation_value > 0.5) {
    explanation <- paste(
      "Terdapat korelasi positif yang kuat antara tingkat pendidikan dan usia, dengan nilai korelasi sebesar", 
      round(correlation_value, 2), ".",
      "Artinya, secara umum, semakin tinggi tingkat pendidikan seseorang, semakin tua usianya. Ini mungkin menunjukkan bahwa dalam data yang dianalisis, individu yang lebih tua cenderung memiliki pendidikan yang lebih tinggi, mungkin karena mereka telah menyelesaikan pendidikan pada tahap yang lebih lanjut dalam hidup mereka."
    )
  } else if (correlation_value < -0.5) {
    explanation <- paste(
      "Terdapat korelasi negatif yang kuat antara tingkat pendidikan dan usia, dengan nilai korelasi sebesar", 
      round(correlation_value, 2), ".",
      "Ini berarti bahwa seiring bertambahnya usia, tingkat pendidikan cenderung menurun dalam populasi yang dianalisis. Fenomena ini bisa terjadi dalam situasi di mana individu yang lebih tua mungkin tidak memiliki akses atau kesempatan untuk melanjutkan pendidikan yang lebih tinggi, berbeda dengan generasi yang lebih muda."
    )
  } else {
    explanation <- paste(
      "Korelasi antara tingkat pendidikan dan usia tidak signifikan, dengan nilai korelasi sebesar", 
      round(correlation_value, 2), ".",
      "Ini menunjukkan bahwa dalam populasi yang dianalisis, tidak ada hubungan yang jelas antara usia dan tingkat pendidikan. Artinya, tingkat pendidikan seseorang tidak dapat diprediksi berdasarkan usia mereka, dan faktor lain mungkin lebih berpengaruh dalam menentukan tingkat pendidikan."
    )
  }
  
  paste(explanation)
})

