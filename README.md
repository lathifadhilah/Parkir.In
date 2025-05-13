# SmartParking
SmartParking merupakan sebuah aplikasi yang memudahkan pengguna dalam mencari parkir di sebuah gedung bertingkat
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(magrittr)
library(dplyr)

camera_urls <- c(
  "http://192.168.249.112:8080/video",
  "http://192.168.249.113:8080/video",
  "http://192.168.249.114:8080/video",
  "http://192.168.249.115:8080/video",
  "http://192.168.249.116:8080/video"
)

calculate_cost <- function(duration, type) {
  rate <- ifelse(type == "motor", 2000, 5000)
  ceiling(duration) * rate
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel(" Smart Parking System - Gedung Multilantai"),
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .parking-card {
        border-radius: 15px; 
        padding: 15px; 
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .camera-feed {
        border: 2px solid #ddd; 
        border-radius: 10px; 
        overflow: hidden;
        margin-bottom: 15px;
      }
      .status-icon { font-size: 24px; 
        vertical-align: middle; 
        margin-right: 8px;
      }
      .progress-group {
        margin: 10px 0;
      }
      .bttn-lg {
        width: 100%;
        margin: 5px 0;
      }
    "))
  ),
  
  tabsetPanel(
    tabPanel(icon("sign-in-alt"), "Masuk Parkir",
             fluidRow(
               column(8,
                      h4("🎥 Monitoring Parkir Real-time", style = "color: #2c3e50;"),
                      tabsetPanel(
                        id = "camera_tabs",
                        tabPanel("Lantai 1", tags$div(class = "camera-feed", tags$iframe(src = camera_urls[1], width = "100%", height = "400px"))),
                        tabPanel("Lantai 2", tags$div(class = "camera-feed", tags$iframe(src = camera_urls[2], width = "100%", height = "400px"))),
                        tabPanel("Lantai 3", tags$div(class = "camera-feed", tags$iframe(src = camera_urls[3], width = "100%", height = "400px"))),
                        tabPanel("Lantai 4", tags$div(class = "camera-feed", tags$iframe(src = camera_urls[4], width = "100%", height = "400px"))),
                        tabPanel("Lantai 5", tags$div(class = "camera-feed", tags$iframe(src = camera_urls[5], width = "100%", height = "400px")))
                      )
               ),
               column(4,
                      tags$div(class = "parking-card", style = "background-color: #f8f9fa;",
                               h4("🚦 Status Kapasitas", style = "margin-top: 0;"),
                               lapply(1:5, function(i) {
                                 tagList(
                                   tags$div(class = "parking-card", style = "background-color: #e9ecef;",
                                            h5(paste("🏢 Lantai", i), style = "margin: 5px 0;"),
                                            uiOutput(paste0("motor_bar_", i)),
                                            uiOutput(paste0("car_bar_", i))
                                   )
                                 )
                               })
                      ),
                      tags$div(class = "parking-card", style = "background-color: #f8f9fa; margin-top: 20px;",
                               h4("🚘 Aksi Masuk Kendaraan", style = "margin-top: 0;"),
                               actionBttn("motor_in", "Motor Masuk", style = "stretch", color = "primary", icon = icon("motorcycle"), size = "lg"),
                               actionBttn("mobil_in", "Mobil Masuk", style = "stretch", color = "success", icon = icon("car"), size = "lg")
                      )
               )
             )
    ),
    
    tabPanel(icon("sign-out-alt"), "Keluar Parkir",
             fluidRow(
               column(6, offset = 3,
                      tags$div(class = "parking-card", style = "text-align: center;",
                               h4("🚗 Proses Keluar Parkir", style = "margin-top: 0;"),
                               actionBttn("motor_out", "Motor Keluar", style = "stretch", color = "royal", icon = icon("motorcycle"), size = "lg"),
                               actionBttn("mobil_out", "Mobil Keluar", style = "stretch", color = "danger", icon = icon("car"), size = "lg"),
                               hr(),
                               h4("📝 Transaksi Terakhir"),
                               verbatimTextOutput("last_action") %>% 
                                 tagAppendAttributes(style = "white-space: pre-wrap; padding: 10px; background: #f8f9fa;")
                      )
               )
             )
    ),
    
    tabPanel(icon("cog"), "Pengaturan",
             fluidRow(
               column(4, offset = 4,
                      tags$div(class = "parking-card",
                               h4("⚙️ Pengaturan Sistem", style = "margin-top: 0;"),
                               numericInputIcon("max_motor", "Slot Motor/Lantai:", value = 5, min = 1, max = 50, icon = list(icon("motorcycle"))),
                               numericInputIcon("max_mobil", "Slot Mobil/Lantai:", value = 5, min = 1, max = 50, icon = list(icon("car"))),
                               hr(),
                               actionBttn("apply_settings", "Simpan Pengaturan", style = "stretch", color = "warning")
                      )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    max_motor_slots = 5,
    max_car_slots = 5,
    motor_slots = rep(5, 5),
    car_slots = rep(5, 5),
    motor_entry = list(),
    car_entry = list(),
    last_action_text = ""
  )
  
  capacity_bar <- function(pct, type, id) {
    color <- case_when(
      pct < 70 ~ "success",
      pct < 90 ~ "warning",
      TRUE ~ "danger"
    )
    progressBar(id = id, value = pct, status = color, striped = TRUE, display_pct = TRUE)
  }
  
  lapply(1:5, function(i) {
    output[[paste0("motor_bar_", i)]] <- renderUI({
      pct <- (1 - rv$motor_slots[i]/rv$max_motor_slots) * 100
      title <- paste0("🛵 Motor (", rv$max_motor_slots - rv$motor_slots[i], "/", rv$max_motor_slots, ")")
      tagList(tags$div(style = "margin-bottom: 15px;",
                       tags$span(title, style = "font-weight: bold;"),
                       capacity_bar(pct, "motor", id = paste0("motorbar", i))))
    })
    output[[paste0("car_bar_", i)]] <- renderUI({
      pct <- (1 - rv$car_slots[i]/rv$max_car_slots) * 100
      title <- paste0("🚗 Mobil (", rv$max_car_slots - rv$car_slots[i], "/", rv$max_car_slots, ")")
      tagList(tags$div(style = "margin-bottom: 15px;",
                       tags$span(title, style = "font-weight: bold;"),
                       capacity_bar(pct, "car", id = paste0("carbar", i))))
    })
  })
  
  observeEvent(input$apply_settings, {
    rv$max_motor_slots <- input$max_motor
    rv$max_car_slots <- input$max_mobil
    rv$motor_slots <- rep(rv$max_motor_slots, 5)
    rv$car_slots <- rep(rv$max_car_slots, 5)
    rv$motor_entry <- list()
    rv$car_entry <- list()
    showModal(modalDialog("Pengaturan diperbarui. Semua slot di-reset ke kosong.", easyClose = TRUE))
  })
  
  observeEvent(input$motor_in, {
    lantai <- which(rv$motor_slots > 0)[1]
    if (is.na(lantai)) {
      showModal(modalDialog("Parkir motor penuh!"))
      return()
    }
    rv$motor_slots[lantai] <- rv$motor_slots[lantai] - 1
    rv$motor_entry <- append(rv$motor_entry, list(list(waktu = Sys.time(), lantai = lantai)))
    showModal(modalDialog(paste("Motor masuk di lantai", lantai), easyClose = TRUE))
  })
  
  observeEvent(input$mobil_in, {
    lantai <- which(rv$car_slots > 0)[1]
    if (is.na(lantai)) {
      showModal(modalDialog("Parkir mobil penuh!"))
      return()
    }
    rv$car_slots[lantai] <- rv$car_slots[lantai] - 1
    rv$car_entry <- append(rv$car_entry, list(list(waktu = Sys.time(), lantai = lantai)))
    showModal(modalDialog(paste("Mobil masuk di lantai", lantai), easyClose = TRUE))
  })
  
  show_payment_modal <- function(jenis, lantai, durasi, biaya) {
    showModal(modalDialog(
      title = paste("Pembayaran", toupper(jenis)),
      tagList(
        tags$p(style = "font-size: 16px;",
               tags$b(icon("clock"), "Durasi Parkir:"), round(durasi, 2), "jam"),
        tags$p(style = "font-size: 16px;",
               tags$b(icon("coins"), "Total Biaya:"), tags$span(paste0("Rp", format(biaya, big.mark = ".", decimal.mark = ",")),
                                                                 style = "color: #27ae60; font-weight: bold;")),
        radioGroupButtons("payment_method", "Metode Pembayaran:",
                          choices = c("E-Wallet", "Tunai"), status = "primary"),
        conditionalPanel(
          condition = "input.payment_method == 'E-Wallet'",
          tagList(
            pickerInput("ewallet_option", "Pilih E-Wallet:",
                        choices = c("QRIS M-Banking", "GoPay", "OVO", "ShopeePay")),
            uiOutput("ewallet_qr")
          )
        )
      ),
      footer = tagList(
        modalButton("Batal"),
        actionBttn("confirm_payment", "Konfirmasi Pembayaran", color = "success", style = "material-flat")
      ),
      size = "m"
    ))
  }
  
  observeEvent(input$motor_out, {
    if (length(rv$motor_entry) == 0) {
      showModal(modalDialog("Tidak ada motor terdaftar."))
      return()
    }
    entry <- rv$motor_entry[[1]]
    rv$motor_entry <- rv$motor_entry[-1]
    durasi <- difftime(Sys.time(), entry$waktu, units = "hours")
    biaya <- calculate_cost(durasi, "motor")
    rv$motor_slots[entry$lantai] <- rv$motor_slots[entry$lantai] + 1
    rv$last_action_text <- paste0("Motor keluar dari lantai ", entry$lantai,
                                  "\nDurasi: ", round(durasi, 2), " jam",
                                  "\nBiaya: Rp ", format(biaya, big.mark = ".", decimal.mark = ","))
    show_payment_modal("motor", entry$lantai, durasi, biaya)
  })
  
  observeEvent(input$mobil_out, {
    if (length(rv$car_entry) == 0) {
      showModal(modalDialog("Tidak ada mobil terdaftar."))
      return()
    }
    entry <- rv$car_entry[[1]]
    rv$car_entry <- rv$car_entry[-1]
    durasi <- difftime(Sys.time(), entry$waktu, units = "hours")
    biaya <- calculate_cost(durasi, "mobil")
    rv$car_slots[entry$lantai] <- rv$car_slots[entry$lantai] + 1
    rv$last_action_text <- paste0("Mobil keluar dari lantai ", entry$lantai,
                                  "\nDurasi: ", round(durasi, 2), " jam",
                                  "\nBiaya: Rp ", format(biaya, big.mark = ".", decimal.mark = ","))
    show_payment_modal("mobil", entry$lantai, durasi, biaya)
  })

  output$ewallet_qr <- renderUI({
    req(input$ewallet_option)
    qr_url <- switch(input$ewallet_option,
                     "QRIS M-Banking" = "https://api.qrserver.com/v1/create-qr-code/?data=QRIS-MBANKING&size=200x200",
                     "GoPay" = "https://api.qrserver.com/v1/create-qr-code/?data=GOPAY-123456&size=200x200",
                     "OVO" = "https://api.qrserver.com/v1/create-qr-code/?data=OVO-987654&size=200x200",
                     "ShopeePay" = "https://api.qrserver.com/v1/create-qr-code/?data=SHOPEEPAY-54321&size=200x200")
    tags$div(style = "text-align: center;",
             tags$img(src = qr_url, height = "200px",
                      style = "border: 2px solid #ddd; border-radius: 10px;"))
  })

  # ✅ Observer untuk tombol konfirmasi pembayaran
  observeEvent(input$confirm_payment, {
    removeModal()
    showModal(modalDialog("✅ Pembayaran berhasil! Terima kasih.", easyClose = TRUE))
  })

  output$last_action <- renderText(rv$last_action_text)
}

shinyApp(ui, server)

