# -*- coding: utf-8 -*-
library(shiny)
library(neuralnet)
library(shinythemes)
library(ggplot2)

# UI definition
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title - K??????LT??LD??
  titlePanel(
    div(
      style = "text-align: center; padding: 10px;",
      h2(HTML("&#127794; Tree Volume Prediction System"), style = "color: #2C3E50; margin: 5px 0;"),
      h5("Volume Estimation with Artificial Neural Network", style = "color: #7F8C8D; margin: 5px 0;")
    )
  ),
  
  # Ana layout
  sidebarLayout(
    # Left panel - Model Loading - K??????LT??LD??
    sidebarPanel(
      style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px;",
      
      h4(HTML("&#128202; Model Loading"), style = "color: #2C3E50; margin-top: 0;"),
      
      # SCOTS PINE MODEL LOADING - K??????LT??LD??
      div(
        style = "background-color: #FFF3CD; padding: 10px; border-radius: 8px; margin-bottom: 10px; border-left: 4px solid #FFC107;",
        h5(HTML("&#127810; Scots Pine Model"), style = "color: #856404; margin: 0 0 8px 0;"),
        fileInput(
          "saricam_model",
          NULL,
          accept = c(".RData", ".rda"),
          buttonLabel = "Select File...",
          placeholder = "No file selected"
        ),
        uiOutput("saricam_durum")
      ),
      
      # BLACK PINE MODEL LOADING - K??????LT??LD??
      div(
        style = "background-color: #D1ECF1; padding: 10px; border-radius: 8px; margin-bottom: 10px; border-left: 4px solid #17A2B8;",
        h5(HTML("&#127809; Black Pine Model"), style = "color: #0C5460; margin: 0 0 8px 0;"),
        fileInput(
          "karacam_model",
          NULL,
          accept = c(".RData", ".rda"),
          buttonLabel = "Select File...",
          placeholder = "No file selected"
        ),
        uiOutput("karacam_durum")
      ),
      
      hr(style = "margin: 10px 0;"),
      
      # Bilgi kutusu - K??????LT??LD??
      div(
        style = "background-color: #3498DB; color: white; padding: 10px; border-radius: 5px;",
        h6(HTML("&#8505;&#65039; Information"), style = "margin: 0 0 5px 0;"),
        p("Load separate models for both species.", style = "font-size: 12px; margin: 0 0 3px 0;"),
        p("Select species from right panel to predict.", style = "font-size: 12px; margin: 0;")
      )
    ),
    
    # Right panel - Prediction and Results
    mainPanel(
      # SPECIES SELECTION - K??????LT??LD??
      div(
        style = "background-color: #E8F5E9; padding: 12px; border-radius: 8px; margin-bottom: 15px; border: 2px solid #4CAF50;",
        fluidRow(
          column(
            12,
            h4(HTML("&#127795; Tree Species Selection"), style = "color: #2E7D32; margin: 0 0 8px 0;"),
            radioButtons(
              "tur_secim",
              NULL,
              choices = c("Scots Pine (Pinus sylvestris)" = "saricam",
                          "Black Pine (Pinus nigra)" = "karacam"),
              selected = character(0),
              inline = TRUE
            )
          )
        )
      ),
      
      tabsetPanel(
        type = "tabs",
        
        # Tahmin sonucu sekmesi
        tabPanel(
          "Prediction Result",
          icon = icon("chart-line"),
          br(),
          
          # Selected model info - K??????LT??LD??
          uiOutput("secili_model_info"),
          
          # Input parameters - K??????LT??LD?? VE YUKARI ??EKILDI
          wellPanel(
            style = "background-color: #F8F9FA; padding: 12px; margin-top: 10px;",
            h5(HTML("&#127919; Prediction Parameters"), style = "color: #2C3E50; margin: 0 0 10px 0;"),
            
            fluidRow(
              column(
                6,
                sliderInput(
                  "cap_input",
                  HTML("DBH (d<sub>1.30</sub> - cm):"),
                  value = 30,
                  min = 1,
                  max = 100,
                  step = 0.5,
                  width = "100%"
                )
              ),
              column(
                6,
                sliderInput(
                  "boy_input",
                  "Height (m):",
                  value = 20,
                  min = 1,
                  max = 40,
                  step = 0.5,
                  width = "100%"
                )
              )
            ),
            
            # Tahmin butonu
            actionButton(
              "tahmin_yap",
              "Predict Volume",
              icon = icon("calculator"),
              class = "btn-success btn-lg btn-block",
              style = "margin-top: 5px;"
            )
          ),
          
          # Result card - YUKARI ??EKILDI
          uiOutput("sonuc_karti"),
          
          br(),
          
          # Visualization
          plotOutput("gorsel_sonuc", height = "400px")
        ),
        
        # Model details tab
        tabPanel(
          "Model Details",
          icon = icon("info-circle"),
          br(),
          
          h3(HTML("&#128300; Model Characteristics")),
          verbatimTextOutput("model_ozet"),
          
          br(),
          
          h3(HTML("&#128200; Model Structure")),
          plotOutput("model_yapisi", height = "500px")
        ),
        
        # Batch prediction tab
        tabPanel(
          "Batch Prediction",
          icon = icon("table"),
          br(),
          
          h3(HTML("&#128203; Prediction for Multiple Values")),
          
          wellPanel(
            fluidRow(
              column(
                4,
                numericInput("cap_min", "Min DBH (cm):", value = 20, min = 1, max = 100),
                numericInput("cap_max", "Max DBH (cm):", value = 50, min = 1, max = 100)
              ),
              column(
                4,
                numericInput("boy_min", "Min Height (m):", value = 15, min = 1, max = 40),
                numericInput("boy_max", "Max Height (m):", value = 25, min = 1, max = 40)
              ),
              column(
                4,
                numericInput("cap_artis", "DBH Increment (cm):", value = 5, min = 1, max = 10),
                numericInput("boy_artis", "Height Increment (m):", value = 2, min = 0.5, max = 5)
              )
            ),
            
            actionButton("toplu_tahmin", "Run Batch Prediction", icon = icon("play"), class = "btn-info btn-block")
          ),
          
          br(),
          
          # Simple table display
          tableOutput("toplu_sonuc_tablo"),
          
          br(),
          
          downloadButton("indir_csv", "Download CSV", class = "btn-warning")
        ),
        
        # Comparison tab
        tabPanel(
          "Species Comparison",
          icon = icon("balance-scale"),
          br(),
          
          h3(HTML("&#128202; Scots Pine vs Black Pine Comparison")),
          
          wellPanel(
            fluidRow(
              column(6, numericInput("kars_cap", "DBH (cm):", value = 30, min = 1, max = 100)),
              column(6, numericInput("kars_boy", "Height (m):", value = 20, min = 1, max = 40))
            ),
            actionButton("karsilastir", "Compare", icon = icon("exchange-alt"), class = "btn-primary btn-block")
          ),
          
          br(),
          
          uiOutput("karsilastirma_sonuc"),
          
          br(),
          
          plotOutput("karsilastirma_grafik", height = "400px")
        )
      )
    )
  ),
  
  # Footer - K??????LT??LD??
  hr(style = "margin: 10px 0;"),
  div(
    style = "text-align: center; color: #7F8C8D; padding: 10px;",
    p(HTML("&copy; 2024 Tree Volume Prediction System | Cankiri Karatekin University"), 
      style = "font-size: 12px; margin: 0;"),
    p("Developer: Prof. Dr. Ilker Ercanli", style = "font-size: 11px; margin: 0;")
  )
)

# Server function
server <- function(input, output, session) {
  
  # Reactive values - SCOTS PINE
  saricam_data <- reactiveValues(
    model = NULL,
    d130_min = NULL,
    d130_max = NULL,
    boy_min = NULL,
    boy_max = NULL,
    vtoplam_min = NULL,
    vtoplam_max = NULL,
    yuklendi = FALSE,
    dosya_adi = NULL,
    model_tipi = NULL
  )
  
  # Reactive values - BLACK PINE
  karacam_data <- reactiveValues(
    model = NULL,
    d130_min = NULL,
    d130_max = NULL,
    boy_min = NULL,
    boy_max = NULL,
    vtoplam_min = NULL,
    vtoplam_max = NULL,
    yuklendi = FALSE,
    dosya_adi = NULL,
    model_tipi = NULL
  )
  
  # General reactive values
  genel_data <- reactiveValues(
    tahmin = NULL,
    toplu_tahmin_df = NULL,
    karsilastirma = NULL
  )
  
  # Function that returns active model
  aktif_model <- reactive({
    if (length(input$tur_secim) == 0) return(NULL)
    
    if (input$tur_secim == "saricam") {
      return(saricam_data)
    } else {
      return(karacam_data)
    }
  })
  
  # SCOTS PINE MODEL LOADING
  observeEvent(input$saricam_model, {
    req(input$saricam_model)
    model_yukle(input$saricam_model, saricam_data, "Scots Pine")
  })
  
  # BLACK PINE MODEL LOADING
  observeEvent(input$karacam_model, {
    req(input$karacam_model)
    model_yukle(input$karacam_model, karacam_data, "Black Pine")
  })
  
  # Model loading function
  model_yukle <- function(dosya_input, veri_nesnesi, tur_adi) {
    dosya <- dosya_input$datapath
    
    if (!is.null(dosya) && file.exists(dosya)) {
      tryCatch({
        veri_nesnesi$dosya_adi <- dosya_input$name
        
        # Load model file
        yuklu_nesneler <- load(dosya, envir = environment())
        
        cat("\n=== ", tur_adi, " MODEL LOADING ===\n")
        cat("Loaded objects:", paste(yuklu_nesneler, collapse = ", "), "\n")
        
        # Find model object
        model_nesne <- NULL
        for (isim in c("nn_model", "nn_modeltek", "nn_modelcift")) {
          if (isim %in% yuklu_nesneler) {
            model_nesne <- get(isim)
            break
          }
        }
        
        # If not found, search for neuralnet class
        if (is.null(model_nesne)) {
          for (nesne_adi in yuklu_nesneler) {
            nesne <- get(nesne_adi)
            if ("neuralnet" %in% class(nesne)) {
              model_nesne <- nesne
              break
            }
          }
        }
        
        if (is.null(model_nesne)) {
          stop("Model object not found!")
        }
        
        # Find parameters
        d_min <- if ("d130_min" %in% yuklu_nesneler) get("d130_min") else NULL
        d_max <- if ("d130_max" %in% yuklu_nesneler) get("d130_max") else NULL
        v_min <- if ("vtoplam_min" %in% yuklu_nesneler) get("vtoplam_min") else NULL
        v_max <- if ("vtoplam_max" %in% yuklu_nesneler) get("vtoplam_max") else NULL
        b_min <- if ("boy_min" %in% yuklu_nesneler) get("boy_min") else NULL
        b_max <- if ("boy_max" %in% yuklu_nesneler) get("boy_max") else NULL
        
        if (is.null(d_min) || is.null(d_max) || is.null(v_min) || is.null(v_max)) {
          stop("Required normalization parameters not found!")
        }
        
        # Determine model type
        if (!is.null(model_nesne$model.list)) {
          input_vars <- model_nesne$model.list$variables
          if (length(input_vars) == 2) {
            veri_nesnesi$model_tipi <- "cift"
          } else {
            veri_nesnesi$model_tipi <- "tek"
          }
        }
        
        # Save data
        veri_nesnesi$model <- model_nesne
        veri_nesnesi$d130_min <- as.numeric(d_min)
        veri_nesnesi$d130_max <- as.numeric(d_max)
        veri_nesnesi$vtoplam_min <- as.numeric(v_min)
        veri_nesnesi$vtoplam_max <- as.numeric(v_max)
        veri_nesnesi$boy_min <- if (!is.null(b_min)) as.numeric(b_min) else NA
        veri_nesnesi$boy_max <- if (!is.null(b_max)) as.numeric(b_max) else NA
        veri_nesnesi$yuklendi <- TRUE
        
        showNotification(
          HTML(paste0("&#9989; ", tur_adi, " model loaded successfully!")),
          type = "message",
          duration = 3
        )
        
        cat(tur_adi, "model type:", veri_nesnesi$model_tipi, "\n")
        cat("DBH range:", veri_nesnesi$d130_min, "-", veri_nesnesi$d130_max, "\n\n")
        
      }, error = function(e) {
        cat("ERROR (", tur_adi, "):", e$message, "\n")
        showNotification(
          paste(tur_adi, "model loading error:", e$message),
          type = "error",
          duration = 10
        )
        veri_nesnesi$yuklendi <- FALSE
      })
    }
  }
  
  # SCOTS PINE status indicator - K??????LT??LD??
  output$saricam_durum <- renderUI({
    if (saricam_data$yuklendi) {
      tip_mesaj <- if (saricam_data$model_tipi == "cift") "2 variables" else "1 variable"
      div(
        style = "background-color: #D4EDDA; color: #155724; padding: 8px; border-radius: 5px; margin-top: 8px; border: 1px solid #C3E6CB;",
        p(HTML(paste0("<strong>&#10004; Loaded</strong><br>", 
                      "Type: ", tip_mesaj, "<br>",
                      "DBH: ", round(saricam_data$d130_min, 1), "-", round(saricam_data$d130_max, 1), " cm")), 
          style = "margin: 0; font-size: 11px;")
      )
    } else {
      div(
        style = "background-color: #F8D7DA; color: #721C24; padding: 8px; border-radius: 5px; margin-top: 8px; border: 1px solid #F5C6CB;",
        p(HTML("<strong>&#10060; Not Loaded</strong>"), style = "margin: 0; font-size: 11px;")
      )
    }
  })
  
  # BLACK PINE status indicator - K??????LT??LD??
  output$karacam_durum <- renderUI({
    if (karacam_data$yuklendi) {
      tip_mesaj <- if (karacam_data$model_tipi == "cift") "2 variables" else "1 variable"
      div(
        style = "background-color: #D1ECF1; color: #0C5460; padding: 8px; border-radius: 5px; margin-top: 8px; border: 1px solid #BEE5EB;",
        p(HTML(paste0("<strong>&#10004; Loaded</strong><br>", 
                      "Type: ", tip_mesaj, "<br>",
                      "DBH: ", round(karacam_data$d130_min, 1), "-", round(karacam_data$d130_max, 1), " cm")), 
          style = "margin: 0; font-size: 11px;")
      )
    } else {
      div(
        style = "background-color: #F8D7DA; color: #721C24; padding: 8px; border-radius: 5px; margin-top: 8px; border: 1px solid #F5C6CB;",
        p(HTML("<strong>&#10060; Not Loaded</strong>"), style = "margin: 0; font-size: 11px;")
      )
    }
  })
  
  # Selected model info - K??????LT??LD??
  output$secili_model_info <- renderUI({
    model_data <- aktif_model()
    
    if (is.null(model_data)) {
      div(
        style = "background-color: #FFF3CD; padding: 10px; border-radius: 8px; border-left: 4px solid #FFC107;",
        h5(HTML("&#9888;&#65039; Please Select Species"), style = "color: #856404; margin: 0 0 5px 0;"),
        p("Select a tree species from above to make predictions.", style = "margin: 0; font-size: 13px;")
      )
    } else if (!model_data$yuklendi) {
      tur_adi <- if (input$tur_secim == "saricam") "Scots Pine" else "Black Pine"
      div(
        style = "background-color: #F8D7DA; padding: 10px; border-radius: 8px; border-left: 4px solid #DC3545;",
        h5(HTML(paste0("&#10060; ", tur_adi, " Model Not Loaded")), style = "color: #721C24; margin: 0 0 5px 0;"),
        p("Please load the model from the left panel.", style = "margin: 0; font-size: 13px;")
      )
    } else {
      tur_adi <- if (input$tur_secim == "saricam") "Scots Pine" else "Black Pine"
      tur_renk <- if (input$tur_secim == "saricam") "#FFC107" else "#17A2B8"
      tip_mesaj <- if (model_data$model_tipi == "cift") "Two-Input (DBH + Height)" else "Single-Input (DBH)"
      
      div(
        style = paste0("background-color: white; padding: 10px; border-radius: 8px; border-left: 4px solid ", tur_renk, "; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
        h5(HTML(paste0("&#9989; Active Model: ", tur_adi)), style = paste0("color: ", tur_renk, "; margin: 0 0 3px 0;")),
        p(paste("Type:", tip_mesaj, "| DBH:", round(model_data$d130_min, 1), "-", round(model_data$d130_max, 1), "cm"), 
          style = "margin: 0; font-size: 12px;")
      )
    }
  })
  
  # Prediction function
  ysa_tahmin <- function(cap_cm, boy_m = NULL, model_data = NULL) {
    if (is.null(model_data) || !model_data$yuklendi) return(NULL)
    
    tryCatch({
      # Normalize DBH
      cap_numeric <- as.numeric(cap_cm)
      d130_norm <- (cap_numeric - model_data$d130_min) / (model_data$d130_max - model_data$d130_min)
      
      # If model has 2 variables, normalize Height as well
      if (model_data$model_tipi == "cift" && !is.null(boy_m)) {
        boy_numeric <- as.numeric(boy_m)
        
        if (is.na(model_data$boy_min) || is.na(model_data$boy_max)) {
          boy_norm <- (boy_numeric - 5) / (40 - 5)
        } else {
          boy_norm <- (boy_numeric - model_data$boy_min) / (model_data$boy_max - model_data$boy_min)
        }
        
        tahmin_input <- data.frame(d130 = d130_norm, Boy = boy_norm)
      } else {
        tahmin_input <- data.frame(d130 = d130_norm)
      }
      
      # Prediction
      tahmin_norm <- compute(model_data$model, tahmin_input)
      
      # Denormalize
      tahmin <- as.numeric(tahmin_norm$net.result) * 
        (model_data$vtoplam_max - model_data$vtoplam_min) + 
        model_data$vtoplam_min
      
      return(as.numeric(tahmin))
    }, error = function(e) {
      cat("Prediction error:", e$message, "\n")
      return(NULL)
    })
  }
  
  # Make prediction
  observeEvent(input$tahmin_yap, {
    model_data <- aktif_model()
    
    if (is.null(model_data)) {
      showNotification("Please select a species!", type = "warning", duration = 3)
      return()
    }
    
    if (!model_data$yuklendi) {
      tur_adi <- if (input$tur_secim == "saricam") "Scots Pine" else "Black Pine"
      showNotification(paste(tur_adi, "model not loaded!"), type = "warning", duration = 3)
      return()
    }
    
    tahmin_sonuc <- ysa_tahmin(input$cap_input, input$boy_input, model_data)
    
    if (is.null(tahmin_sonuc) || is.na(tahmin_sonuc)) {
      showNotification("Prediction failed!", type = "error", duration = 5)
      return()
    }
    
    genel_data$tahmin <- tahmin_sonuc
    
    showNotification(HTML("&#9989; Prediction completed!"), type = "message", duration = 2)
  })
  
  # Result card - DAHA KOMPAKT
  output$sonuc_karti <- renderUI({
    if (is.null(genel_data$tahmin)) {
      div(
        style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px; text-align: center; border: 2px dashed #BDC3C7; margin-top: 10px;",
        icon("tree", style = "font-size: 36px; color: #95A5A6;"),
        h4("No prediction made yet", style = "color: #7F8C8D; margin-top: 15px; margin-bottom: 5px;"),
        p("Enter parameters and click 'Predict Volume'", style = "font-size: 13px; margin: 0;")
      )
    } else {
      tur_adi <- if (input$tur_secim == "saricam") "Scots Pine" else "Black Pine"
      tur_renk <- if (input$tur_secim == "saricam") "#F39C12" else "#2C3E50"
      
      div(
        style = paste0("background-color: ", tur_renk, "; color: white; padding: 20px; border-radius: 10px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); margin-top: 10px;"),
        h3(paste(tur_adi, "Prediction Result"), style = "margin: 0 0 15px 0;"),
        div(
          style = "background-color: rgba(255,255,255,0.2); padding: 15px; border-radius: 5px;",
          h5(paste("DBH:", input$cap_input, "cm | Height:", input$boy_input, "m"), style = "margin: 0 0 10px 0;"),
          h1(HTML(paste(round(genel_data$tahmin, 4), "m&sup3;")), style = "font-size: 42px; margin: 0;"),
          h6("Estimated Tree Volume", style = "margin: 8px 0 0 0;")
        ),
        p(paste("Date:", format(Sys.time(), "%d.%m.%Y %H:%M")), 
          style = "font-size: 11px; opacity: 0.8; margin: 10px 0 0 0;")
      )
    }
  })
  
  # Visual result
  output$gorsel_sonuc <- renderPlot({
    if (is.null(genel_data$tahmin)) return(NULL)
    
    model_data <- aktif_model()
    if (is.null(model_data) || !model_data$yuklendi) return(NULL)
    
    tryCatch({
      cap_aralik <- seq(model_data$d130_min, model_data$d130_max, length.out = 50)
      hacim_aralik <- sapply(cap_aralik, function(c) ysa_tahmin(c, input$boy_input, model_data))
      
      df_plot <- data.frame(Cap = cap_aralik, Hacim = hacim_aralik)
      df_nokta <- data.frame(Cap = input$cap_input, Hacim = genel_data$tahmin)
      
      tur_adi <- if (input$tur_secim == "saricam") "Scots Pine" else "Black Pine"
      tur_renk <- if (input$tur_secim == "saricam") "#F39C12" else "#2C3E50"
      
      ggplot(df_plot, aes(x = Cap, y = Hacim)) +
        geom_line(color = tur_renk, linewidth = 1.5) +
        geom_point(data = df_nokta, aes(x = Cap, y = Hacim), 
                   color = "#E74C3C", size = 5, shape = 19) +
        geom_vline(xintercept = input$cap_input, linetype = "dashed", color = "#E74C3C", alpha = 0.5) +
        labs(
          title = paste(tur_adi, "- DBH-Volume Relationship (Height =", input$boy_input, "m)"),
          subtitle = "Artificial Neural Network Prediction Curve",
          x = "Diameter at Breast Height (cm)",
          y = "Volume (m3)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D")
        )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Could not create plot", cex = 1.2)
    })
  })
  
  # Model summary
  output$model_ozet <- renderPrint({
    model_data <- aktif_model()
    
    if (is.null(model_data) || !model_data$yuklendi) {
      cat("Please select a species and load the model.\n")
    } else {
      tur_adi <- if (input$tur_secim == "saricam") "SCOTS PINE" else "BLACK PINE"
      
      cat("=== ", tur_adi, " ARTIFICIAL NEURAL NETWORK MODEL ===\n\n")
      cat("Model Type:", model_data$model_tipi, "variable(s)\n")
      cat("Input Variables: DBH (Diameter at Breast Height)")
      if (model_data$model_tipi == "cift") cat(" + Height")
      cat("\n")
      cat("Output Variable: Volume (m3)\n\n")
      cat("Data Ranges:\n")
      cat(sprintf("  DBH: %.2f - %.2f cm\n", model_data$d130_min, model_data$d130_max))
      cat(sprintf("  Volume: %.4f - %.4f m3\n", model_data$vtoplam_min, model_data$vtoplam_max))
      cat("\nModel Structure:\n")
      print(model_data$model)
    }
  })
  
  # Model structure plot
  output$model_yapisi <- renderPlot({
    model_data <- aktif_model()
    
    if (is.null(model_data) || !model_data$yuklendi) return(NULL)
    
    tryCatch({
      tur_adi <- if (input$tur_secim == "saricam") "Scots Pine" else "Black Pine"
      
      plot(model_data$model, rep = "best", 
           information = FALSE,
           col.entry = "#3498DB",
           col.out = "#27AE60",
           col.hidden = "#E67E22",
           main = paste(tur_adi, "Artificial Neural Network Architecture"))
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Could not create plot", cex = 1.2)
    })
  })
  
  # Batch prediction
  observeEvent(input$toplu_tahmin, {
    model_data <- aktif_model()
    
    if (is.null(model_data) || !model_data$yuklendi) {
      showNotification("Please load a model!", type = "warning")
      return()
    }
    
    tryCatch({
      cap_degerler <- seq(input$cap_min, input$cap_max, by = input$cap_artis)
      boy_degerler <- seq(input$boy_min, input$boy_max, by = input$boy_artis)
      
      kombinasyonlar <- expand.grid(Cap = cap_degerler, Boy = boy_degerler)
      
      hacim_tahminler <- rep(NA, nrow(kombinasyonlar))
      
      for (i in 1:nrow(kombinasyonlar)) {
        hacim_tahminler[i] <- ysa_tahmin(kombinasyonlar$Cap[i], kombinasyonlar$Boy[i], model_data)
      }
      
      tur_adi <- if (input$tur_secim == "saricam") "Scots Pine" else "Black Pine"
      
      genel_data$toplu_tahmin_df <- data.frame(
        SiraNo = 1:nrow(kombinasyonlar),
        Tur = tur_adi,
        Cap_cm = round(kombinasyonlar$Cap, 2),
        Boy_m = round(kombinasyonlar$Boy, 2),
        Hacim_m3 = round(hacim_tahminler, 4),
        stringsAsFactors = FALSE
      )
      
      basarili <- sum(!is.na(hacim_tahminler))
      showNotification(
        paste("Batch prediction completed for", tur_adi, "!", basarili, "predictions successful."),
        type = "message", 
        duration = 3
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })
  
  # Batch result table
  output$toplu_sonuc_tablo <- renderTable({
    if (is.null(genel_data$toplu_tahmin_df)) {
      return(data.frame(Message = "No batch prediction made yet."))
    }
    
    df <- genel_data$toplu_tahmin_df
    colnames(df) <- c("No", "Species", "DBH (cm)", "Height (m)", "Volume (m3)")
    return(df)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # CSV download
  output$indir_csv <- downloadHandler(
    filename = function() {
      tur_adi <- if (input$tur_secim == "saricam") "scots_pine" else "black_pine"
      paste("tree_volume_prediction_", tur_adi, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(genel_data$toplu_tahmin_df)) {
        df <- genel_data$toplu_tahmin_df
        colnames(df) <- c("No", "Species", "DBH (cm)", "Height (m)", "Volume (m3)")
        write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  # Comparison
  observeEvent(input$karsilastir, {
    if (!saricam_data$yuklendi || !karacam_data$yuklendi) {
      showNotification("Both models must be loaded!", type = "warning", duration = 3)
      return()
    }
    
    saricam_hacim <- ysa_tahmin(input$kars_cap, input$kars_boy, saricam_data)
    karacam_hacim <- ysa_tahmin(input$kars_cap, input$kars_boy, karacam_data)
    
    genel_data$karsilastirma <- data.frame(
      Tur = c("Scots Pine", "Black Pine"),
      Hacim = c(saricam_hacim, karacam_hacim),
      stringsAsFactors = FALSE
    )
    
    showNotification("Comparison completed!", type = "message", duration = 2)
  })
  
  # Comparison result
  output$karsilastirma_sonuc <- renderUI({
    if (is.null(genel_data$karsilastirma)) return(NULL)
    
    df <- genel_data$karsilastirma
    
    fluidRow(
      column(
        6,
        div(
          style = "background-color: #FFF3CD; padding: 20px; border-radius: 10px; text-align: center; border: 3px solid #FFC107;",
          h3("Scots Pine", style = "color: #856404; margin-top: 0;"),
          h1(paste(round(df$Hacim[1], 4), "m3"), style = "color: #856404;")
        )
      ),
      column(
        6,
        div(
          style = "background-color: #D1ECF1; padding: 20px; border-radius: 10px; text-align: center; border: 3px solid #17A2B8;",
          h3("Black Pine", style = "color: #0C5460; margin-top: 0;"),
          h1(paste(round(df$Hacim[2], 4), "m3"), style = "color: #0C5460;")
        )
      )
    )
  })
  
  # Comparison plot
  output$karsilastirma_grafik <- renderPlot({
    if (is.null(genel_data$karsilastirma)) return(NULL)
    
    df <- genel_data$karsilastirma
    
    ggplot(df, aes(x = Tur, y = Hacim, fill = Tur)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste(round(Hacim, 4), "m3")), 
                vjust = -0.5, size = 6, fontface = "bold") +
      scale_fill_manual(values = c("Scots Pine" = "#FFC107", "Black Pine" = "#17A2B8")) +
      labs(
        title = paste("Volume Comparison (DBH:", input$kars_cap, "cm, Height:", input$kars_boy, "m)"),
        x = "Tree Species",
        y = "Volume (m3)"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
        axis.text = element_text(size = 14)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
