library(shiny)
library(shinydashboard)
library(DT) 
library(dplyr)
library(plotly)
library(ggplot2)
library(ggtext)
library(maps)
library(ggcorrplot)
library(shinycssloaders)
library(RColorBrewer)
require(scatterplot3d)
library(reshape2)
library(readr)
library(tidyverse)
library(factoextra)
library(modeest)
library(bslib)
library(outliers)
library(EnvStats)

Data = read.csv("C:/Users/Riska Lathifah/OneDrive/Documents/DOKUMEN!/SEMESTER 3/EVD/shiny cuy/final project/datafinal.csv", sep = ",", fileEncoding = "ISO-8859-1", header = TRUE, quote='"')
Dataawal = read.csv("C:/Users/Riska Lathifah/OneDrive/Documents/DOKUMEN!/SEMESTER 3/EVD/shiny cuy/final project/dataPopulasi_k6FIXX.csv", sep = ",", fileEncoding = "ISO-8859-1", header=TRUE)
Datamodel= read.csv("C:/Users/Riska Lathifah/OneDrive/Documents/DOKUMEN!/SEMESTER 3/EVD/shiny cuy/final project/datakita.csv", sep = ",", fileEncoding = "ISO-8859-1", header=TRUE)
Data <- subset(Data, select = -X)

c3 = Data %>% 
  select(-"Nama.Produk",-"Nama.Prosesor",-"Nama.GPU",-"Gambar",-"Resolusi",-"Jenis.Layar",-"Kecepatan.CPU",-"Harga",-"Berat",-"Volume",-"Lebar",-"Ketinggian",-"Ketebalan",-"Ukuran.Layar",-"Kerapatan.Piksel",-"RAM",-"Kecepatan.RAM",-"Penyimpanan.Internal",-"Thread.CPU") %>% 
  names()

decsBox1 <- function(rata){
  renderInfoBox({
    infoBox(
      "Rata Rata", rata, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}

decsBox2 <- function(median){
  renderInfoBox({
    infoBox(
      "Median", median, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}
decsBox3 <- function(modus){
  renderInfoBox({
    infoBox(
      "Modus", paste(modus, collapse = ", "), icon = icon("uncharted"),
      color = "navy", fill = F
    )
  })
}
decsBox4 <- function(min){
  renderInfoBox({
    infoBox(
      "Minimal", min, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}
decsBox5 <- function(max){
  renderInfoBox({
    infoBox(
      "Maksimal", max, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}
decsBox6 <- function(quant1){
  renderInfoBox({
    infoBox(
      "Quartile 1", quant1, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}
decsBox7 <- function(quant3){
  renderInfoBox({
    infoBox(
      "Quartile 3", quant3, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}
decsBox8 <- function(varian1){
  renderInfoBox({
    infoBox(
      "Varians", varian1, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}
decsBox9 <- function(std1){
  renderInfoBox({
    infoBox(
      "Standar deviasi", std1, icon = icon("uncharted"),
      color = "navy", fill=F
    )
  })
}

sampledat <- sample(c(TRUE, FALSE), nrow(Datamodel), replace = TRUE, prob = c(0.7, 0.3))
datatrain <- Datamodel[sampledat, ]
datatests <- Datamodel[!sampledat, ]
numericdata_train <- datatrain %>% select(Harga, Berat, Volume, Lebar, Ketinggian, Ketebalan, Ukuran.Layar, Kerapatan.Piksel, Kecepatan.RAM, RAM, Penyimpanan.Internal, Thread.CPU,
                                          Kipas, Backlight.Keyboard, Layar.Sentuh, Anti.Pantul, Penyimpanan.Flash, SSD.NVME, HDMI, USB.Daya, Adaptor.Magsafe, Suara.Stereo, Dolby.Atmos, Stylus, Pemindai.Sidikjari, Jumlah.Mikrofon, Pengenal.Wajah.3D)
numericdata_train <- as.data.frame(numericdata_train)

numericdata_tests <- datatests %>% select(Harga, Berat, Volume, Lebar, Ketinggian, Ketebalan, Ukuran.Layar, Kerapatan.Piksel, Kecepatan.RAM, RAM, Penyimpanan.Internal, Thread.CPU,
                                          Kipas, Backlight.Keyboard, Layar.Sentuh, Anti.Pantul, Penyimpanan.Flash, SSD.NVME, HDMI, USB.Daya, Adaptor.Magsafe, Suara.Stereo, Dolby.Atmos, Stylus, Pemindai.Sidikjari, Jumlah.Mikrofon, Pengenal.Wajah.3D)
numericdata_tests <- as.data.frame(numericdata_tests)

forward_selection<- function(datatrain, response_var, criterion) {
  candidate_vars <- setdiff(names(datatrain), response_var)
  selected_vars <- c()
  best_model <- lm(as.formula(paste(response_var, "~ 1")), datatrain)
  best_criterion <- criterion(best_model)
  
  while(length(candidate_vars) > 0) {
    next_var <- NULL
    for (var in candidate_vars) {
      model <- lm(as.formula(paste(response_var, "~", paste(selected_vars, collapse = " + "), "+", var)), datatrain)
      current_criterion <- criterion(model)
      if (current_criterion > best_criterion) {
        best_criterion <- current_criterion
        next_var <- var
      }
    }
    
    if (!is.null(next_var)) {
      selected_vars <- c(selected_vars, next_var)
      candidate_vars <- setdiff(candidate_vars, next_var)
    } else {
      break
    }
  }
  final_model <- lm(as.formula(paste(response_var, "~", paste(selected_vars, collapse = " + "))), datatrain)
  return(final_model)
}



response_var <- "Harga"

# Define UI for application that draws a histogram
ui <- dashboardPage(dashboardHeader(title = "Kelompok 6"),skin= "purple",
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "Home", icon = icon("house")),
                        menuItem("Introduction", tabName = "intro", icon = icon("circle-info")),
                        menuItem("Dataset", tabName = "data_tab", icon = icon("database")),
                        menuItem("Data Prepocessing", tabName = "prepo", icon = icon("gears")),
                        menuItem("Visualization", tabName = "vis_tab", icon = icon("chart-column")),
                        menuItem("Compare two laptop", tabName = "compare", icon=icon("code-compare")),
                        menuItem("Predictive analysis", tabName = "analysis", icon=icon("arrow-up-right-dots"))
                      )
                      
                    ),
                    dashboardBody(theme=bs_theme(bootswatch="quartz"),
                                  tabItems(
                                    tabItem(tabName = "Home", 
                                            
                                            icon = icon("house"),
                                            fluidRow(
                                              column(12, 
                                                     tags$img(src = "welcome.jpg", width = "100%", height = "auto")
                                                     
                                              ))), 
                                    tabItem(tabName = "intro", 
                                            tabBox(id="t1", width = 12,  
                                                   tabPanel(
                                                     "Profile",
                                                     icon = icon("address-card"),
                                                     fluidRow(
                                                       column(12, 
                                                              tags$img(src = "team.jpg", width = "100%", height = "auto")
                                                       )
                                                     )
                                                   ), 
                                                   tabPanel("About Data", icon=icon("circle-info"),
                                                            fluidRow(
                                                              column(width = 8, tags$img(src="versus.jpg", width =600 , height = 300),
                                                                     tags$br() , 
                                                                     tags$a("website versus yang dilakukan scraping", style = "font-weight:bold; color:#3c8dbc;"), align = "center"),
                                                              column(width = 4, tags$br() ,
                                                                     tags$p("Data yang digunakan diperoleh melalui scraping menggunakan 
                                                                Versus API (application programming interface) dengan menggunakan 
                                                                Python. Data ini memiliki 795 data laptop dengan 35 variabel.")
                                                              ),
                                                            )
                                                   ),
                                                   tabPanel("Pendahuluan", icon=icon("lightbulb"),
                                                            fluidRow(
                                                              column(12, align="center",
                                                                     h2("Latar Belakang"),
                                                                     p("Dalam era digital saat ini, teknologi telah menjadi bagian yang tidak terpisahkan dalam kehidupan sehari-hari, termasuk dalam dunia pendidikan. Mahasiswa sebagai aktor kunci dalam konteks ini, seringkali dihadapkan pada kebutuhan untuk memilih perangkat teknologi yang sesuai dengan kebutuhan akademik dan aktivitas sehari-hari. Perangkat yang sangat penting bagi mahasiswa saat ini adalah laptop. Perangkat ini memungkinkan mahasiswa untuk mengakses sumber belajar online, menyelesaikan tugas, dan mengikuti kegiatan perkuliahan. Pada program studi Teknologi Sains Data Universitas Airlangga, laptop menjadi perangkat penting untuk membantu perkuliahan pada hampir seluruh mata kuliah yang ditempuh selama pendidikan."),
                                                                     p("Saat mahasiswa mencari laptop baru, seringkali mereka dihadapkan dengan banyaknya pilihan dengan spesifikasi yang berbeda-beda. Memilih laptop tertentu bisa jadi merupakan pilihan yang sulit. Oleh karena itu, kami merancang sebuah website yang memungkinkan mahasiswa membandingkan spesifikasi laptop secara efisien dan dapat memberikan informasi yang relevan untuk mendukung pengambilan keputusan. Website yang kami rancang tidak hanya memberikan solusi atas kebutuhan tersebut, namun juga sebagai sarana untuk menunjang pembelajaran dan meningkatkan produktivitas mahasiswa dalam masa pendidikan."),
                                                                     h2("Rumusan Masalah"),
                                                                     p("Bagaimana variabel yang dapat dipertimbangkan mahasiswa dalam memilih laptop?"),
                                                                     p("Bagaimana faktor atau variabel yang membentuk variasi harga laptop?"),
                                                                     h2("Tujuan Penelitian"),
                                                                     p("Mengetahui variabel yang dapat dipertimbangkan mahasiswa dalam memilih laptop."),
                                                                     p("Mengetahui faktor atau variabel yang membentuk variasi harga laptop.")
                                                              )
                                                            )
                                                   )
                                                   
                                                   
                                            )
                                    ),
                                    tabItem("data_tab",
                                            tabsetPanel(
                                              tabPanel("List Data",icon=icon("rectangle-list"),
                                                       DTOutput("dataset")),
                                              tabPanel("Structure",icon=icon("codepen"),
                                                       verbatimTextOutput("structure")),
                                              tabPanel("Summary",icon=icon("newspaper"),
                                                       selectInput(
                                                         "intColumn",
                                                         "Pilih Variabel:",
                                                         c(
                                                           "Harga ($)" = "Harga",
                                                           "Berat (kg)" = "Berat",
                                                           "Volume (cm^3)" = "Volume",
                                                           "Tinggi (mm)" = "Ketinggian",
                                                           "Lebar(mm)" = "Lebar",
                                                           "Tebal(mm)" = "Ketebalan",
                                                           "Ukuran Layar (inch)" = "ukuran.layar",
                                                           "Kerapatan Pixel(ppi)"="Kerapatan.Piksel",
                                                           "RAM (GB)"="RAM",
                                                           "Kecepatan RAM (MHz)"="Kecepatan.RAM",
                                                           "Penyimpanan Internal(GB)"="Penyimpanan.Internal",
                                                           "Thread CPU"="Thread.CPU"
                                                         )
                                                       ),
                                                       infoBoxOutput("descrip"),
                                                       infoBoxOutput("descrip1"),
                                                       infoBoxOutput("descrip2"),
                                                       infoBoxOutput("descrip3"),
                                                       infoBoxOutput("descrip4"),
                                                       infoBoxOutput("descrip5"),
                                                       infoBoxOutput("descrip6"),
                                                       infoBoxOutput("descrip7"),
                                                       infoBoxOutput("descrip8")
                                              ))
                                    ),
                                    tabItem("prepo",
                                            tabsetPanel(
                                              tabPanel("Dataset Awal",icon=icon("firstdraft"),
                                                       DTOutput("dataawal")),
                                              tabPanel("transformasi", icon=icon("shuffle"),
                                                       DTOutput('tabelTransformasi')),
                                              tabPanel("Missing Value", icon=icon("cubes-stacked"),
                                                       div(style = "margin-bottom: 1cm;",  # Menambahkan spasi 1 cm
                                                           fluidRow(
                                                             plotOutput("missingValueBarChart"),
                                                             textOutput("missingValueMessage")
                                                           )
                                                       ),
                                                       div(style = "margin-bottom: 1cm;",  # Menambahkan spasi 1 cm
                                                           fluidRow(
                                                             box(
                                                               width = 12,
                                                               "Pada barchart terlihat terdapat nilai yang hilang pada variabel Nama.Produk dan Nama Processor."
                                                             )
                                                           )
                                                       ),
                                                       div(style = "margin-bottom: 1cm;",  # Menambahkan spasi 1 cm
                                                           fluidRow(
                                                             plotOutput("missingValueBarChart2")
                                                           )
                                                       ),
                                                       fluidRow(
                                                         box(width = 12, "Pada barchart diatas terlihat bahwa data yang telah dilakukan imputasi sudah tidak memiliki nilai yang hilang.")
                                                       )
                                              ),
                                              tabPanel("Outliers", icon = icon("ellipsis"),
                                                       tabsetPanel(
                                                         tabPanel("Data Awal",
                                                                  selectInput("outlierOptionAwal", 
                                                                              "Choose an option:", 
                                                                              choices = list("Harga" = "Harga",
                                                                                             "Berat" = "Berat",
                                                                                             "Volume" = "Volume",
                                                                                             "Lebar" = "Lebar",
                                                                                             "Ketebalan" = "Ketebalan",
                                                                                             "Ketinggian" = "Ketinggian",
                                                                                             "Ukuran.Layar" = "Ukuran.Layar",
                                                                                             "Kerapatan.Piksel" = "Kerapatan.Piksel",
                                                                                             "Kecepatan.RAM" = "Kecepatan.RAM",
                                                                                             "RAM" = "RAM",
                                                                                             "Thread.CPU" = "Thread.CPU")),
                                                                  fluidRow(
                                                                    box(
                                                                      title = "Rosner Test",
                                                                      status = "primary",
                                                                      solidHeader = TRUE,
                                                                      width = 12,
                                                                      tableOutput("rosnerTestTableAwal")  # Tabel Rosner Test
                                                                    )
                                                                  )
                                                         ),
                                                         tabPanel("Data Akhir",
                                                                  selectInput("outlierOptionAkhir", 
                                                                              "Choose an option:", 
                                                                              choices = list("Harga" = "Harga",
                                                                                             "Berat" = "Berat",
                                                                                             "Volume" = "Volume",
                                                                                             "Lebar" = "Lebar",
                                                                                             "Ketebalan" = "Ketebalan",
                                                                                             "Ketinggian" = "Ketinggian",
                                                                                             "Ukuran.Layar" = "Ukuran.Layar",
                                                                                             "Kerapatan.Piksel" = "Kerapatan.Piksel",
                                                                                             "Kecepatan.RAM" = "Kecepatan.RAM",
                                                                                             "RAM" = "RAM",
                                                                                             "Thread.CPU" = "Thread.CPU")),
                                                                  fluidRow(
                                                                    box(
                                                                      title = "Rosner Test",
                                                                      status = "primary",
                                                                      solidHeader = TRUE,
                                                                      width = 12,
                                                                      tableOutput("rosnerTestTableAkhir")  # Tabel Rosner Test
                                                                    )
                                                                  )
                                                         )
                                                       )
                                              ),
                                              tabPanel("Dimensional Reduction", icon=icon("laptop-code"),
                                                       verbatimTextOutput("reduksi"))
                                              
                                            )
                                    ),
                                    tabItem(tabName = "vis_tab",
                                            tabsetPanel(
                                              id = "vis_tab",  
                                              tabPanel(
                                                "Trend", 
                                                value="trends",
                                                selectInput(inputId = "var2", 
                                                            label ="Select the Variable", 
                                                            choices = c(
                                                              "Harga ($)" = "Harga",
                                                              "Berat (kg)" = "Berat",
                                                              "Volume (cm^3)" = "Volume",
                                                              "Tinggi (mm)" = "Ketinggian",
                                                              "Lebar(mm)" = "Lebar",
                                                              "Tebal(mm)" = "Ketebalan",
                                                              "Ukuran Layar (inch)" = "Ukuran.Layar",
                                                              "Kerapatan Pixel(ppi)"="Kerapatan.Piksel",
                                                              "RAM (GB)"="RAM",
                                                              "Kecepatan RAM (MHz)"="Kecepatan.RAM",
                                                              "Penyimpanan Internal(GB)"="Penyimpanan.Internal",
                                                              "Thread CPU"="Thread.CPU"
                                                            )),
                                                fluidRow(
                                                  tags$div(align="center", box(tableOutput("top10"), title = textOutput("head1"), collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE)),
                                                  tags$div(align="center", box(tableOutput("low10"), title = textOutput("head2"), collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE))
                                                ),
                                                withSpinner(plotlyOutput("bar"))
                                              ),
                                              tabPanel(
                                                "Distribution", 
                                                value="distro",
                                                selectInput(inputId = "var1", 
                                                            label ="Select the Variable", 
                                                            choices = c(
                                                              "Harga ($)" = "Harga",
                                                              "Berat (kg)" = "Berat",
                                                              "Volume (cm^3)" = "Volume",
                                                              "Tinggi (mm)" = "Ketinggian",
                                                              "Lebar(mm)" = "Lebar",
                                                              "Tebal(mm)" = "Ketebalan",
                                                              "Ukuran Layar (inch)" = "Ukuran.Layar",
                                                              "Kerapatan Pixel(ppi)"="Kerapatan.Piksel",
                                                              "RAM (GB)"="RAM",
                                                              "Kecepatan RAM (MHz)"="Kecepatan.RAM",
                                                              "Penyimpanan Internal(GB)"="Penyimpanan.Internal",
                                                              "Thread CPU"="Thread.CPU"
                                                            ))
                                                ,
                                                withSpinner(plotlyOutput("histplot", height = "420px")),
                                                h3(" "),
                                                fluidRow(box(
                                                  textOutput("distExplanation"),
                                                  style = "background-color: #ffffff; border-radius: 3px; padding: 10px; margin-top: 10px; width:100%"
                                                ))
                                              ),
                                              tabPanel(
                                                "Charts", 
                                                value="Data Composition",
                                                selectInput(inputId = "var3", label ="Select the Variable", choices = c3),
                                                fluidRow(
                                                  column(6, withSpinner(plotlyOutput("pieplot"))),
                                                  column(6, withSpinner(plotlyOutput("barplot")))
                                                ),
                                                h3(" "),
                                                fluidRow(box(
                                                  textOutput("chartExplanation"),
                                                  style = "background-color: #ffffff; border-radius: 3px; padding: 10px; margin-top: 20px; width:100%"
                                                ))
                                              ),
                                              
                                              tabPanel("Correlation Matrix", id = "corr",
                                                       tabsetPanel(
                                                         tabPanel("Heatmap",
                                                                  withSpinner(plotlyOutput("cor"))
                                                         ),
                                                         tabPanel("Explanation",
                                                                  box(
                                                                    textOutput("heatmapExplanation"),
                                                                    style = "background-color: #ffffff; border-radius: 3px; padding: 10px; margin-top: 10px; width: 100%;"
                                                                  )
                                                         )
                                                       )
                                              )
                                              ,
                                              tabPanel("Scatter plot",
                                                       div(
                                                         fluidPage(
                                                           h2(tags$b(textOutput("captionscat")), align="center"),
                                                           box(plotOutput("scatter")),
                                                           box(
                                                             selectInput(
                                                               "variabel3", "variabel x :",
                                                               c(
                                                                 "Berat (kg)" = "Berat",
                                                                 "Volume (cm^3)" = "Volume",
                                                                 "Tinggi (mm)" = "Ketinggian",
                                                                 "Lebar(mm)" = "Lebar",
                                                                 "Tebal(mm)" = "Ketebalan",
                                                                 "Ukuran Layar (inch)" = "Ukuran.Layar",
                                                                 "Kerapatan Pixel(ppi)"="Kerapatan.Piksel",
                                                                 "RAM (GB)"="RAM",
                                                                 "Kecepatan RAM (MHz)"="Kecepatan.RAM",
                                                                 "Penyimpanan Internal(GB)"="Penyimpanan.Internal",
                                                                 "Thread CPU"="Thread.CPU"
                                                               )
                                                             ),
                                                             selectInput(
                                                               "variabel4", "variabel y :",
                                                               c(
                                                                 "Harga ($)" = "Harga"
                                                               )
                                                             )
                                                           ),
                                                           box(selectInput("colorsc", "Pilih Warna :",
                                                                           c("Blue" = "#7AC5CD",
                                                                             "Green" = "#556B2F",
                                                                             "Purple" = "#68228B",
                                                                             "Red" = "#8B1A1A"))),
                                                           box(
                                                             textOutput("scatterExplanation"),
                                                             style = "background-color: #ffffff; border-radius: 3px; padding: 10px; margin-top: 10px;"
                                                           )
                                                         )
                                                       )),
                                            )
                                    ),
                                    tabItem(tabName = "compare",
                                            fluidPage(
                                              titlePanel("Perbandingan Laptop"),
                                              fluidRow(
                                                column(6, selectInput("laptop1", "Pilih Laptop 1:", choices = unique(Data$Nama.Produk))),
                                                column(6, selectInput("laptop2", "Pilih Laptop 2:", choices = unique(Data$Nama.Produk)))
                                              ),
                                              fluidRow(
                                                column(6, uiOutput("comparisonBox1")),
                                                column(6, uiOutput("comparisonBox2"))
                                              )
                                            )
                                    ),
                                    tabItem(
                                      tabName = "analysis",
                                      fluidPage(
                                        headerPanel("Prediksi Harga Laptop"),
                                        sidebarPanel(
                                          textInput("a", "Masukkan kerapatan piksel :- ", ""),
                                          textInput("b", "Masukkan penyimpanan internal :- ", ""),
                                          textInput("c", "Masukkan RAM :-", ""),
                                          textInput("d", "Thread.CPU :- ", ""),
                                          textInput("e", "Ketinggian :- ", ""),
                                          actionButton('go', "Predict")
                                        ),
                                        mainPanel(
                                          box(
                                            title = "Informasi variabel",
                                            solidHeader = TRUE,
                                            column(
                                              12,
                                              align = "left",
                                              p("KerapatanPiksel:Jumlah piksel per inci pada layar laptop."),
                                              p("Contoh: Jika sebuah laptop memiliki kerapatan piksel sebesar 200 piksel per inci, ini berarti terdapat 200 piksel dalam satu inci panjang layar."),
                                              p(" "),
                                              p("PenyimpananInternal:Kapasitas penyimpanan internal laptop dalam gigabyte (GB)."),
                                              p("Contoh: Jika kapasitas penyimpanan internal sebuah laptop adalah 256 GB, ini menunjukkan bahwa laptop tersebut memiliki ruang penyimpanan sebesar 256 gigabyte."),
                                              p(" "),
                                              p("RAM:Kapasitas RAM laptop dalam gigabyte (GB)."),
                                              p("Contoh: Jika kapasitas RAM sebuah laptop adalah 8 GB, ini berarti laptop tersebut memiliki 8 gigabyte RAM untuk menjalankan aplikasi dan tugas komputasi."),
                                              p(" "),
                                              p("ThreadCPU:Jumlah thread pada unit pemrosesan pusat (CPU) laptop."),
                                              p("Contoh: Jika sebuah CPU laptop memiliki 4 thread, ini menunjukkan bahwa CPU tersebut dapat mengeksekusi empat tugas komputasi secara simultan."),
                                              p(" "),
                                              p("Ketinggian:Dimensi ketinggian laptop dalam milimeter (mm)."),
                                              p("Contoh: Jika ketinggian fisik laptop adalah 200 mm, ini berarti laptop tersebut memiliki tinggi sebesar 200 milimeter atau 20 centimeter."),
                                              p(" ")
                                              )),
                                          box(
                                            textOutput("Petunjuk"),
                                            title = "Petunjuk pengisian",
                                            solidHeader = TRUE
                                          ),
                                          box(
                                            h4("Prediksi harga laptop anda adalah"),
                                            textOutput("value")
                                          )
                                        )
                                      )
                                    )
                                    
                                      )
                                    )
                                  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #data table output
  model_dataset <- reactive({
    input$dts
  })
  output$dataset <-renderDT({datatable(Data, options=list(scrollX=TRUE))
  })
  
  #data table output
  model_dataawal <- reactive({
    input$dts
  })
  output$dataawal <-renderDT({datatable(Dataawal, options=list(scrollX=TRUE))
  })
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("10", input$var2, "Tertinggi")
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("10", input$var2, "Terendah")
  )
  
  output$top10 <- renderTable({
    
    Data %>% 
      select(Nama.Produk, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(10)
    
  })
  
  output$low10 <- renderTable({
    
    Data %>% 
      select(Nama.Produk, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(10)
  })
  #structure
  output$structure<-renderPrint({
    Data %>%
      str
  })
  #summary
  output$descrip <- renderUI({
    rata2 <- mean(Data[, c(input$intColumn)])
    rataRound <- round(rata2, 2)
    decsBox1(rataRound)
  })
  
  output$descrip1 <- renderUI({
    median1 <- median(Data[, c(input$intColumn)])
    medianRound <- round(median1, 2)
    decsBox2(medianRound)
  })
  find_mode <- function(x) {
    tbl <- table(x, useNA = "ifany")
    modes <- as.numeric(names(tbl[tbl == max(tbl)]))
    return(modes)
  }
  output$descrip2 <- renderUI({
    # Pastikan input$intColumn sesuai dengan nama kolom yang benar
    modus1 <- find_mode(Data[, c(input$intColumn)])
    modusRound <- round(modus1, 2)
    decsBox3(modusRound)
  })
  
  output$descrip3 <- renderUI({
    min1 <- min(Data[, c(input$intColumn)])
    minRound <- round(min1, 2)
    decsBox4(minRound)
  })
  
  output$descrip4 <- renderUI({
    max1 <- max(Data[, c(input$intColumn)])
    maxRound <- round(max1, 2)
    decsBox5(maxRound)
  })
  
  output$descrip5 <- renderUI({
    quantil1 <- quantile(Data[, c(input$intColumn)],prob=c(.25))
    quantil1Round <- round(quantil1, 2)
    decsBox6(quantil1Round)
  })
  
  output$descrip6 <- renderUI({
    quantil3 <- quantile(Data[, c(input$intColumn)],prob=c(.75))
    quantil3Round <- round(quantil3, 2)
    decsBox7(quantil3Round)
  })
  
  output$descrip7 <- renderUI({
    varian <- var(Data[, c(input$intColumn)])
    varianRound <- round(varian, 2)
    decsBox8(varianRound)
  })
  
  output$descrip8 <- renderUI({
    std2 <- sd(Data[, c(input$intColumn)])
    std2Round <- round(std2, 2)
    decsBox9(std2Round)
  })
  #Preprocessing
  # Add server code for "transformasi" here
  output$tabelTransformasi <- renderDT({ 
    # Fungsi untuk mengubah unit
    conv_unit <- function(value, from, to) {
      # Kode untuk mengubah unit di sini
    }
    
    # Transformasi data
    Dataawal <- Dataawal %>%
      mutate(Nama.Prosesor = str_extract(Nama.Produk, "(Intel|AMD|Apple M1|Qualcomm)\\s.*?(?=\\s/\\s|\\s\\d+GB|$)"))
    Dataawal <- Dataawal %>%
      mutate(Nama.GPU = str_extract(Nama.Produk, "Nvidia\\s[^/]+"))
    Dataawal <- Dataawal %>%
      mutate(Nama.Produk = trimws(str_extract(Nama.Produk, "^[A-Za-z0-9\\s\"'()-.]+(?=\\s(Intel|AMD|Apple\\sM1|Qualcomm))")))
    Dataawal <- Dataawal %>%
      mutate(Harga = ifelse(Harga > 10, Harga / 1000, Harga))
    
    # Mengubah unit
    Dataawal$Lebar <- conv_unit(Dataawal$Lebar, from = "mm", to = "cm")
    Dataawal$Ketinggian <- conv_unit(Dataawal$Ketinggian, from = "mm", to = "cm")
    Dataawal$Ketebalan <- conv_unit(Dataawal$Ketebalan, from = "mm", to = "cm")
    Dataawal$Ukuran.Layar <- conv_unit(Dataawal$Ukuran.Layar, from = "in", to = "cm")
    
    # Mengisi nilai yang hilang
    Dataawal$Nama.GPU <- ifelse(is.na(Dataawal$Nama.GPU), "Integrated", Dataawal$Nama.GPU)
    
    # Menyusun ulang kolom
    Dataawal <- Dataawal %>%
      select(Nama.Produk, Nama.Prosesor, Nama.GPU, everything())
    # Menghitung missing value
    missing_count <- colSums(is.na(Dataawal))
    # Membuat grafik batang untuk missing value
    create_bar_chart <- function(missing_count) {
      barplot(missing_count, col = "skyblue", main = "Jumlah Missing Value Setelah Transformasi",
              xlab = " ", ylab = "Jumlah Missing Value", las = 2)
    }
    # Membuat pesan untuk missing value
    no_missing_message <- function(missing_count) {
      if (sum(missing_count) == 0) {
        return("Tidak ada missing value pada data")
      } else {
        return(NULL)
      }
    }
    
    # Menampilkan grafik batang dan pesan
    output$missingValueBarChart <- renderPlot({
      create_bar_chart(missing_count)
    })
    output$missingValueMessage <- renderText({
      no_missing_message(missing_count)
    })
    
    return(Dataawal)
  })
  
  output$missingValueBarChart2 <- renderPlot({  # Fungsi renderPlot untuk grafik baru
    create_bar_chart(missing_count)
  })
  
  output$missingValueMessage <- renderText({
    no_missing_message(missing_count)
  })
  
  missing_count <- colSums(is.na(Data))
  
  create_bar_chart <- function(missing_count) {
    barplot(missing_count, col = "skyblue", main = "Jumlah Missing Value Setelah Imputasi",
            xlab = " ", ylab = "Jumlah Missing Value",las = 2)
  }
  
  no_missing_message <- function(missing_count) {
    if (sum(missing_count) == 0) {
      return("Tidak ada missing value pada data")
    } else {
      return(NULL)
    }
  }  
  
  # Untuk Data Awal
  observeEvent(input$outlierOptionAwal, {
    
    # Your k values for each option
    k_values <- list("Harga" = 4, "Berat" = 8, "Volume" = 35, "Lebar" = 1, 
                     "Ketebalan" = 4, "Ketinggian" = 1, "Ukuran.Layar" = 1, 
                     "Kerapatan.Piksel" = 1, "Kecepatan.RAM" = 1, "RAM" = 1, 
                     "Thread.CPU" = 3)
    
    # Perform Rosner Test
    test <- rosnerTest(Dataawal[[input$outlierOptionAwal]], k = k_values[[input$outlierOptionAwal]])
    
    # Assign the result to a reactive value
    output$rosnerTestTableAwal <- renderTable({
      test$all.stats
    })
  })
  
  # Untuk Data Akhir
  observeEvent(input$outlierOptionAkhir, {
    
    # Your k values for each option
    k_values <- list("Harga" = 4, "Berat" = 8, "Volume" = 35, "Lebar" = 1, 
                     "Ketebalan" = 4, "Ketinggian" = 1, "Ukuran.Layar" = 1, 
                     "Kerapatan.Piksel" = 1, "Kecepatan.RAM" = 1, "RAM" = 1, 
                     "Thread.CPU" = 3)
    
    # Perform Rosner Test
    test <- rosnerTest(Data[[input$outlierOptionAkhir]], k = k_values[[input$outlierOptionAkhir]])
    
    # Assign the result to a reactive value
    output$rosnerTestTableAkhir <- renderTable({
      test$all.stats
    })
  })
  output$reduksi <- renderPrint({
    final_model <- forward_selection(numericdata_train, response_var, function(model) summary(model)$r.squared)
    summary_model<-summary(final_model)
    return(summary_model)
  })
  
  # For histogram - distribution charts
  output$histplot <- renderPlotly({
    p1 = Data %>% 
      plot_ly() %>% 
      add_histogram(x=~get(input$var1), marker = list(color = 'blue'))%>% 
      layout(xaxis = list(title = paste(input$var1)))
    
    p2 = Data %>%
      plot_ly() %>%
      add_boxplot(x=~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = F))
    
    # stacking the plots on top of each other
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
      hide_legend() %>% 
      layout(title = "<b>Distribution chart - Boxplot and Histogram</b>",
             yaxis = list(title="<b>Frequency</b>"))
  })
  output$distExplanation <- renderText({
    switch(
      input$var1,
      "Harga"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 886. Masih terdapat outlier pada variabel harga. Selain itu, data terlihat right skewed.",
      "Berat"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 1.84. Masih terdapat outlier pada variabel berat. Selain itu, data terlihat right skewed.",
      "Volume"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 1592.563. Masih terdapat outlier pada variabel volume. Selain itu, data terlihat right skewed.",
      "Ketinggian"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 242. Masih terdapat outlier pada variabel tinggi. Selain itu, data cenderung berdistribusi normal.",
      "Lebar"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 355.6. Masih terdapat outlier pada variabel lebar. Selain itu, data cenderung berdistribusi normal.",
      "Tebal"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 19. Masih terdapat outlier pada variabel tebal. Selain itu, data cenderung berdistribusi normal.",
      "Ukuran.Layar"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 15.6. Masih terdapat outlier pada variabel ukuran layar. Selain itu, data cenderung berdistribusi normal.",
      "Kerapatan.Piksel"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 157. Masih terdapat outlier pada variabel kerapatan piksel. Selain itu, data terlihat right skewed",
      "RAM"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 16. Masih terdapat outlier pada variabel RAM. Selain itu, data terlihat right skewed",
      "Kecepatan.RAM"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 2933. Masih terdapat outlier pada variabel Kecepatan RAM. Selain itu, data terlihat right skewed",
      "Penyimpanan.Internal"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 512. Masih terdapat outlier pada variabel penyimpanan internal. Selain itu, data terlihat right skewed",
      "Thread.CPU"="Berdasarkan visualisasi di atas, data cenderung berada di dalam kotak dengan median 8. Masih terdapat outlier pada variabel Thread CPU. Selain itu, data terlihat right skewed"
    )
  })
  
  ### Bar Charts
  output$bar <- renderPlotly({
    Data %>% 
      plot_ly() %>% 
      add_bars(x=~Nama.Produk, y=~get(input$var2), marker = list(color = 'light blue')) %>% 
      layout(title = paste("Trend", input$var2),
             xaxis= list(showticklabels = FALSE),
             xaxis = list(title = "Book"),
             yaxis = list(title = paste(input$var2)))
    
  })
  
  # For pie chart
  output$pieplot <- renderPlotly({
    formulaText <- reactive({
      paste("Pie Chart of", input$var3)
    })
    Data %>% 
      plot_ly(labels = ~get(input$var3), type = "pie") %>% 
      layout(title = formulaText())
  })
  output$barplot <- renderPlotly({
    formulaText <- reactive({
      paste("Bar Chart of", input$var3)
    })
    
    # Mengubah data menjadi tabel frekuensi
    freq_table <- table(Data[[input$var3]])
    
    # Membuat bar chart
    plot_ly(x = ~names(freq_table), y = ~freq_table, type = "bar") %>% 
      layout(title = formulaText(), xaxis = list(title = input$var3), yaxis = list(title = "Frequency"))
  })
  
  output$chartExplanation <- renderText({
    switch(
      input$var3,
      "Kipas" = "Hanya ada sembilan laptop atau sekitar 0.956% yang menggunakan desain tanpa kipas. Sedangkan Sebagian besar laptop menggunakan desain dengan kipas sebanyak 932 atau 99%.",
      "Backlight.Keyboard"="Sebagian besar laptop menggunakan desain backlight pada keyboard sebanyak 755. Sedangkan laptop yang tidak memiliki desain ini hanya 19.8% yaitu 186 laptop.",
      "Layar.Sentuh"="Sebagian besar laptop tidak memiliki kemampuan layar sentuh atau touchscreen yaitu sebanyak 67.4% (634 laptop), sedangkan laptop yang memiliki kemampuan layar sentuh lebih sedikit yaitu sekitar 32.6%(307 laptop).",
      "Anti.Pantul"="Laptop yang mempunyai lapisan anti pantul dan tanpa anti pantul hampir seimbang. Sekitar 51.1% atau 481 untuk laptop yang mempunyai lapisan anti pantul. Sedangkan yang tidak memiliki lapisan anti pantul sekitar 48.9% atau 460 laptop.",
      "Penyimpanan.Flash"="Laptop yang menggunakan penyimpanan flash lebih banyak daripada yang tidak memiliki penyimpanan flash. Sekitar 86% atau 809 dari total laptop memiliki penyimpanan flash. Sedangkan 14% atau 132 dari total laptop tidak memiliki penyimpanan flash.",
      "SSD.NVME"="Sebagian besar laptop tidak memiliki SSD.NVME yaitu sebesar 71% atau 668 dari total laptop. Sedangkan sisanya sebesar 29% atau 273 dari total laptop memiliki SSD.NVME.",
      "HDMI"="Sebagian besar laptop memiliki HDMI yaitu sebesar 82.9% atau 780 dari total laptop. Sedangkan sisanya sebesar 17.1% atau 161 dari total laptop tidak memiliki SSD.NVME.",
      "USB.Daya"="Sebagian besar laptop memiliki port USB pengisi daya dalam mode tidur yaitu sebesar 82.4% atau 775 dari total laptop. Sedangkan sisanya sebesar 17.6% atau 166 dari total laptop tidak memiliki port USB pengisi daya dalam mode tidur.",
      "Adaptor.Magsafe"="Sebagian besar laptop tidak memiliki adaptor daya Magsafe yaitu sebesar 98.8% atau 930 dari total laptop. Sedangkan sisanya sebesar 1.17% atau 11 dari total laptop memiliki adaptor daya Magsafe.",
      "Suara.Stereo"="Sebagian besar laptop memiliki pengeras suara stereo terpasang yaitu sebesar 99.8% atau 939 dari total laptop. Sedangkan sisanya sebesar 0.213% atau 2 dari total laptop tidak memiliki pengeras suara stereo terpasang.",
      "Konektor.Suara.3.5mm"="Sebagian besar laptop memiliki soket untuk konektor audio 3,5 mm yaitu sebesar 97.3% atau 916 dari total laptop. Sedangkan sisanya sebesar 2.66% atau 25 dari total laptop tidak memiliki soket untuk konektor audio 3,5 mm.",
      "Dolby.Atmos"="Sebagian besar laptop tidak mempunyai Dolby Atmos yaitu sebesar 91.9% atau 916 dari total laptop. Sedangkan sisanya sebesar 8.08% atau 76 dari total laptop tidak mempunyai Dolby Atmos.",
      "Stylus"="Sebagian besar laptop tidak mempunyai stylus pen yaitu sebesar 88% atau 828 dari total laptop. Sedangkan sisanya sebesar 12% atau 113 dari total laptop mempunyai stylus pen.",
      "Pemindai.Sidikjari"="Sebagian besar laptop tidak mempunyai pemindai sidik jari yaitu sebesar 64.2% atau 604 dari total laptop. Sedangkan sisanya sebesar 35.8% atau 337 dari total laptop mempunyai pemindai sidik jari.",
      "Jumlah.Mikrofon"="Sebagian besar laptop hanya memiliki 1 mikrofon yaitu sekitar 51.9%. Kemudian urutan kedua terbanyak ada laptop yang memiliki 2 mikrofon dengan persentase 38.9%. Laptop yang memiliki 3 mikrofon hanya sekitar 5.21% yaitu 49 laptop. Sedangkan sisanya ada laptop yang memiliki 4 mikrofon yaitu sekitar 4.04% atau 38 dari total laptop.",
      "Pengenal.Wajah.3D"="Sebagian besar laptop tidak mempunyai pengenal wajah 3D yaitu sebesar 85.5% atau 805 dari total laptop. Sedangkan sisanya sebesar 14.5% atau 136 dari total laptop mempunyai pengenal wajah 3D."
    )
  })
  
  # Correlation plot 
  output$cor <- renderPlotly({
    
    cols_to_use <- setdiff(1:33, c(1,2,3,5,7,8,14,16,17,18,21,23,25,26,27,28,29,30,31,32,33,34,35))
    my_df <- Data[, cols_to_use]
    
    # Calculate correlation matrix
    cor_matrix <- cor(my_df, use = "complete.obs")
    
    # Convert to dataframe for plotly
    cor_df <- reshape2::melt(cor_matrix)
    names(cor_df) <- c("Var1", "Var2", "value")
    
    # Create correlation plot
    p <- plot_ly(cor_df, x = ~Var1, y = ~Var2, z = ~value, type = "heatmap", zmin = -1, zmax = 1, colors = colorRamp(c("blue", "white", "red"))) %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""),
             autosize = F, 
             width = 700, 
             height = 500)
    
    return(p)
  })
  output$heatmapExplanation <- renderText({"Berdasarkan Heatmap Corelation,terdapat beberapa variabel independen yang memiliki korelasi signifikan. 
    Variabel yang berkaitan dengan body laptop seperti Ukuran Layar, Ketebalan, Ketinggian, Lebar, Volume dan Berat saling berkorelasi posifit satu sama lain. Selain itu, variabel lain yang berkaitan dengan 
    kinerja laptop seperti RAM, Kecepatan RAM, Penyimpanan Internal dan Thread CPU juga saling berkorelasi yang signifikan"})
  
  
  # scatter plot
  output$scatter <- renderPlot({
    formulaText <- reactive({
      paste("Scatterplot ", input$variabel3 , "dengan variabel respon")
    })
    output$captionscat <- renderText({
      formulaText()
    })
    
    datascat1 = Data[[input$variabel3]]
    datascat2 = Data[[input$variabel4]]
    plot(datascat1, datascat2, main= formulaText() ,xlab= paste(input$variabel3), ylab= paste(input$variabel4), col = input$colorsc, pch=19)
    
  })
  
  output$scatterExplanation <- renderText({
    switch(
      input$variabel3,
      "Berat" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "Volume" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "Ketinggian" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "Lebar" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "Ketebalan" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "ukuran.layar"="Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "Kerapatan.Piksel" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "RAM" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak namun terdapat kecenderungan korelasi positif antara variabel X dan Y",
      "Kecepatan.RAM" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "Penyimpanan.Internal" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak menunjukkan bahwa tidak ada pola yang dapat diidentifikasi secara visual.
      Hal ini mengindikasikan tidak ada korelasi yang kuat antara variabel X dan Y",
      "Thread.CPU" = "Berdasarkan scatter plot tersebut terdapat persebaran titik yang acak namun terdapat kecenderungan korelasi positif antara variabel X dan Y"
      # Tambahkan opsi lain sesuai variabel yang ada
    )
  })
  
  # Comparison plot output
  variables_to_change <- c("Kipas", "Backlight.Keyboard", "Penyimpanan.Flash", "SSD.NVME", "HDMI", "USB.Daya", "Adaptor.Magsafe", "Suara.Stereo", "Konektor.Suara.3,5mm", "Dolby.Atmos", "Stylus", "Pemindai.Sidikjari", "Pengenal.Wajah.3D")
  
  output$comparisonBox1 <- renderUI({
    # Filter data based on selected laptop
    laptop1_data <- Data[Data$Nama.Produk == input$laptop1, ]
    
    # Create a list of tags for each column
    tags_list <- lapply(names(laptop1_data), function(column_name) {
      if (column_name == "Gambar") {
        tags$img(src = laptop1_data[[column_name]], height = "200px")
      } else {
        # Change the output based on the value for certain variables
        if (column_name %in% variables_to_change) {
          output_value <- ifelse(laptop1_data[[column_name]] == 0, "Tidak Ada", "Ada")
        } else {
          output_value <- laptop1_data[[column_name]]
        }
        tags$p(paste(column_name, ": ", output_value))
      }
    })
    
    # Create a box with all the tags
    box(do.call(tagList, tags_list),width=12)
  })
  
  #predictive analysis
  data2 = reactiveValues()
  observeEvent(input$go, {
    # Pastikan nilai variabel telah terdefinisi
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    c <- as.numeric(input$c)
    d <- as.numeric(input$d)
    e <- as.numeric(input$e)
    
    # Lakukan transformasi variabel jika diperlukan
    a2 <- log(log(a))
    b2 <- log(b)
    c2 <- log(c)
    d2 <- log(d)
    e2 <- sqrt(e)
    
  newPredict <- data.frame(
    Kerapatan.Piksel = a2,
    Penyimpanan.Internal = b2,
    RAM = c2,
    Thread.CPU = d2,
    Ketinggian = e2
  )
  final_model <- lm(Harga ~ Kerapatan.Piksel + Penyimpanan.Internal + RAM + Thread.CPU + Ketinggian, data=Datamodel)
  # Lakukan prediksi
  data2$op <- predict(final_model, newPredict)*100
  })
  output$value <- renderPrint({data2$op})

  output$Petunjuk<- renderText({
    "Isi setiap variabel tanpa satuannya. Contoh: RAM 32 GB diisi 32"
  })
}

# Run the application 
shinyApp(ui = ui, server = server)