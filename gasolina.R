#Library ----

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(magrittr)
library(xtable)
library(plotly)
library(utf8)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(ggbiplot)
library(forecast)
library(shinymanager)

#Contraseñas ----

  contraseñas <- data.frame(
    user = c("SBR-Gasolina"),
    password = c("SBR333"),
    stringsAsFactors = FALSE)

#Error General ----

  options(shiny.sanitize.errors = TRUE)  

#Base de Datos ----
  
  datos <- data.frame(readxl::read_excel("C:/Users/0303u/Google Drive/Trabajo/Trabajo SBR/Shell/Dashboard Gasolina Dinamico/DatosGasolina.xlsx", sheet = "DatosShell"))
  
  datos$Fecha <- as.Date(datos$Fecha)

#Datos Fijos ----

  datosFijos <- dplyr::filter(.data = datos, 
                              Fecha >= "2020-01-01" & Fecha <= "2020-12-31")
  
  datosFijosA <- dplyr::select(.data = datosFijos, 
                               Región, Fecha, Producto, Margen, Pesos, Costo, Litros, Importe, PrecioLitro) %>% 
    dplyr::filter(Región=="A") %>%
    dplyr::summarise(Utilidad = round(sum(Margen),0),
                     Ventas = round(sum(Pesos),0),
                     Costos = round(sum(Costo),0),
                     Litros = round(sum(Litros),0),
                     Despachos = round(sum(Importe),0),
                     Precio = round(mean(PrecioLitro),2))
  
  datosFijosB <- dplyr::select(.data = datosFijos, 
                               Región, Fecha, Producto, Margen, Pesos, Costo, Litros, Importe, PrecioLitro) %>% 
    dplyr::filter(Región=="B") %>%
    dplyr::summarise(Utilidad = round(sum(Margen),0),
                     Ventas = round(sum(Pesos),0),
                     Costos = round(sum(Costo),0),
                     Litros = round(sum(Litros),0),
                     Despachos = round(sum(Importe),0),
                     Precio = round(mean(PrecioLitro),2))
  
  datosFijosC <- dplyr::select(.data = datosFijos, 
                               Región, Fecha, Producto, Margen, Pesos, Costo, Litros, Importe, PrecioLitro) %>% 
    dplyr::filter(Región=="C") %>%
    dplyr::summarise(Utilidad = round(sum(Margen),0),
                     Ventas = round(sum(Pesos),0),
                     Costos = round(sum(Costo),0),
                     Litros = round(sum(Litros),0),
                     Despachos = round(sum(Importe),0),
                     Precio = round(mean(PrecioLitro),2))
  
  datosFijosA <- format(datosFijosA, big.mark = ",")
  datosFijosB <- format(datosFijosB, big.mark = ",")
  datosFijosC <- format(datosFijosC, big.mark = ",")
  
  datosFijosA <- data.frame(datosFijosA)
  datosFijosB <- data.frame(datosFijosB)
  datosFijosC <- data.frame(datosFijosC)

#header ----
  
  header <- dashboardHeaderPlus(title = "Gasolinera", 
                                enable_rightsidebar = TRUE,
                                rightSidebarIcon = "sliders")
#sidebar ----  
  
  sidebar <- dashboardSidebar({
    sidebarMenu(
      menuItem("Principal", tabName = "Principal", icon = icon("chart-line"),
               menuSubItem("Región A", tabName = "RegiónA", icon = shiny::icon("stream")),
               menuSubItem("Región B", tabName = "RegiónB", icon = shiny::icon("stream")),
               menuSubItem("Región C", tabName = "RegiónC", icon = shiny::icon("stream"))),
      menuItem("Avanzado", tabName = "Avanzado", icon = icon("laptop-code"),
               menuSubItem("PCA", tabName = "PCA", icon = shiny::icon("stream")),
               menuSubItem("Pronostico", tabName = "Pronostico", icon = shiny::icon("stream")),
               menuSubItem("Ventas-vs-Precio", tabName = "Ventas-vs-Precio", icon = shiny::icon("stream")))
               )
                             })

#rightSidebar ----
  
  rightsidebar <- rightSidebar(
    rightSidebarTabContent(
      id = 2,
      active = TRUE,
      title = "Periodo",
      icon = "calendar-alt",
      sliderInput(inputId = "filtro",
                  label = "Periodo:",
                  animate = TRUE,
                  min = min(datos$Fecha),
                  max = Sys.Date(),
                  value = as.Date(c("2020-01-01", "2020-12-31"))),
                  uiOutput("filtro2")),
    rightSidebarTabContent(
      id = 1,
      title = "Periodicidad",
      icon = "calendar-check",
      radioButtons("periodo",
                   "Periodicidad:",
                   c("Diario", "Semanal", "Mensual"))))
  
#body ----

  body <- dashboardBody({
    verbatimTextOutput("usuario")
    tabItems(
      tabItem(tabName = "RegiónA",
              fluidRow(
                gradientBox(
                  title = "Utilidad",
                  icon = "fa fa-hand-holding-usd",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("UtilidadA"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[1], "   Anual")
                ),
                gradientBox(
                  title = "Ventas",
                  icon = "fa fa-money-check-alt",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("VentasA"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[2], "   Anual")
                ),
                gradientBox(
                  title = "Costos",
                  icon = "fa fa-search-dollar",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("CostosA"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[3], "   Anual")
                ),
                gradientBox(
                  title = "Litros",
                  icon = "fa fa-gas-pump",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("LitrosA"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[4], "   Anual")
                ),
                gradientBox(
                  title = "Despacho",
                  icon = "fa fa-oil-can",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("DespachoA"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[5], "   Anual")
                ),
                gradientBox(
                  title = "Precio",
                  icon = "fa fa-dollar-sign",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("PrecioA"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[6], "   Anual")
                ),
                boxPlus(plotlyOutput("treemapA",
                                     height = "300px"),
                        title = "Ventas de las Sucursales",
                        width = 8,
                        collapsible = TRUE,
                        closable = FALSE,
                        status = "primary"
                ),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Super", 
                           status = "primary", 
                           div(tableOutput("SucursalAAs"),
                               style="font-size:90%")),
                  tabPanel(title = "VPower", 
                           status = "primary", 
                           div(tableOutput("SucursalAAv"),
                               style="font-size:90%")),
                  tabPanel(title = "Diesel", 
                           status = "primary", 
                           div(tableOutput("SucursalAAd"),
                               style="font-size:90%"))),
                  title = "Sucursales con mayores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Super", 
                           status = "primary", 
                           div(tableOutput("SucursalABs"),
                               style="font-size:90%")),
                  tabPanel(title = "VPower",
                           status = "primary", 
                           div(tableOutput("SucursalABv"),
                               style="font-size:90%")),
                  tabPanel(title = "Diesel", 
                           status = "primary", 
                           div(tableOutput("SucursalABd"),
                               style="font-size:90%"))),
                  title = "Sucursales con menores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE,
                  collapsed = TRUE),
                boxPlus(plotlyOutput("GraficaSerieTiempoA",
                                     height = "300px"),
                        title = "Utilidad Diaria",
                        width = 12,
                        collapsible = TRUE,
                        closable = FALSE,
                        status = "primary"
                )
              )
      ),
      tabItem(tabName = "RegiónB",
              fluidRow(
                gradientBox(
                  title = "Utilidad",
                  icon = "fa fa-hand-holding-usd",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput(outputId = "UtilidadB"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[1], "   Anual")
                ),
                gradientBox(
                  title = "Ventas",
                  icon = "fa fa-money-check-alt",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("VentasB"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[2], "   Anual")
                ),
                gradientBox(
                  title = "Costos",
                  icon = "fa fa-search-dollar",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("CostosB"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[3], "   Anual")
                ),
                gradientBox(
                  title = "Litros",
                  icon = "fa fa-gas-pump",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("LitrosB"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[4], "   Anual")
                ),
                gradientBox(
                  title = "Despacho",
                  icon = "fa fa-oil-can",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("DespachoB"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[5], "   Anual")
                ),
                gradientBox(
                  title = "Precio",
                  icon = "fa fa-dollar-sign",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("PrecioB"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[6], "   Anual")
                ),
                boxPlus(plotlyOutput("treemapB",
                                     height = "300px"),
                        title = "Ventas de las Sucursales",
                        width = 8,
                        collapsible = TRUE,
                        closable = FALSE,
                        status = "primary"
                ),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Super", 
                           status = "primary",
                           div(tableOutput("SucursalBAs"),
                               style="font-size:90%")),
                  tabPanel(title = "VPower", 
                           status = "primary", 
                           div(tableOutput("SucursalBAv"),
                               style="font-size:90%")),
                  tabPanel(title = "Diesel", 
                           status = "primary", 
                           div(tableOutput("SucursalBAd"),
                               style="font-size:90%"))),
                  title = "Sucursales con mayores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Super", 
                           status = "primary", 
                           div(tableOutput("SucursalBBs"),
                               style="font-size:90%")),
                  tabPanel(title = "VPower",
                           status = "primary", 
                           div(tableOutput("SucursalBBv"),
                               style="font-size:90%")),
                  tabPanel(title = "Diesel", 
                           status = "primary", 
                           div(tableOutput("SucursalBBd"),
                               style="font-size:90%"))),
                  title = "Sucursales con menores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE,
                  collapsed = TRUE),
                boxPlus(plotlyOutput("GraficaSerieTiempoB",
                                     height = "300px"),
                        title = "Utilidad Diaria",
                        width = 12,
                        collapsible = TRUE,
                        closable = FALSE,
                        status = "primary"
                )
              )
      ),
      tabItem(tabName = "RegiónC",
              fluidRow(
                gradientBox(
                  title = "Utilidad",
                  icon = "fa fa-hand-holding-usd",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("UtilidadC"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[1], "   Anual")
                ),
                gradientBox(
                  title = "Ventas",
                  icon = "fa fa-money-check-alt",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("VentasC"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[2], "   Anual")
                ),
                gradientBox(
                  title = "Costos",
                  icon = "fa fa-search-dollar",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("CostosC"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[3], "   Anual")
                ),
                gradientBox(
                  title = "Litros",
                  icon = "fa fa-gas-pump",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("LitrosC"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[4], "   Anual")
                ),
                gradientBox(
                  title = "Despacho",
                  icon = "fa fa-oil-can",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("DespachoC"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[5], "   Anual")
                ),
                gradientBox(
                  title = "Precio",
                  icon = "fa fa-dollar-sign",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("PrecioC"),
                               style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[6], "   Anual")
                ),
                boxPlus(plotlyOutput("treemapC",
                                     height = "300px"),
                        title = "Ventas de las Sucursales",
                        width = 8,
                        collapsible = TRUE,
                        closable = FALSE,
                        status = "primary"
                ),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Super", 
                           status = "primary", 
                           div(tableOutput("SucursalCAs"),
                               style="font-size:90%")),
                  tabPanel(title = "VPower", 
                           status = "primary", 
                           div(tableOutput("SucursalCAv"),
                               style="font-size:90%")),
                  tabPanel(title = "Diesel", 
                           status = "primary", 
                           div(tableOutput("SucursalCAd"),
                               style="font-size:90%"))),
                  title = "Sucursales con mayores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Super", 
                           status = "primary", 
                           div(tableOutput("SucursalCBs"),
                               style="font-size:90%")),
                  tabPanel(title = "VPower", 
                           status = "primary", 
                           div(tableOutput("SucursalCBv"),
                               style="font-size:90%")),
                  tabPanel(title = "Diesel", 
                           status = "primary", 
                           div(tableOutput("SucursalCBd"),
                               style="font-size:90%"))),
                  title = "Sucursales con menores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE,
                  collapsed = TRUE),
                boxPlus(plotlyOutput("GraficaSerieTiempoC",
                                     height = "300px"),
                        title = "Utilidad Diaria",
                        width = 12,
                        collapsible = TRUE,
                        closable = FALSE,
                        status = "primary"
                )
              )
      ),
      tabItem(tabName = "PCA",
              fluidRow(
                boxPlus(plotlyOutput("PCA",
                                     height = "500px"),
                        title = "Analisis de Correspondencia Multiple",
                        width = 12,
                        collapsible = TRUE,
                        closable = FALSE,
                        status = "primary"
                )
                      )
              ),
      tabItem(tabName = "Pronostico",
              fluidRow(
                boxPlus(
                  title = "Región",
                  width = 2,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE,
                  tags$style("#select2 {background-color:blue;}"),
                  selectInput(inputId = "Región",
                              label = "",
                              choices = c("A","B","C","Total"))),
                  boxPlus(
                    title = "Sucursal",
                    width = 4,
                    collapsible = TRUE,
                    closable = FALSE,
                    status = "primary",
                    footer_padding = FALSE,
                    uiOutput("Estación")),
                  boxPlus(
                    title = "Producto",
                    width = 2,
                    collapsible = TRUE,
                    closable = FALSE,
                    status = "primary",
                    footer_padding = FALSE,
                    uiOutput("Producto")),
                  boxPlus(
                    title = "# de Pronosticos",
                    width = 2,
                    collapsible = TRUE,
                    closable = FALSE,
                    status = "primary",
                    footer_padding = FALSE,
                    numericInput("pronosticos",
                                 label = "", 
                                 value = 7, 
                                 min = 1, 
                                 max = 31)),
                boxPlus(
                  title = "Descarga Datos",
                  width = 2,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE,
                  downloadButton("Descarga", 
                                 label = "Descarga")),
                boxPlus(
                  plotlyOutput("Arima",
                               height = "350px"),
                  title = "Pronostico",
                  width = 9,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "maroon",
                  footer_padding = FALSE),
                boxPlus(
                  tableOutput("tablaPronostico"),
                  title = "Datos",
                  width = 3,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "maroon",
                  footer_padding = FALSE),
                boxPlus(title = "Modelo",
                    width = 3,
                    status = "maroon",
                    textOutput("modelo"))
              ))
    )
  })
  
#ui ----
  
  ui <- dashboardPagePlus(header, sidebar, rightsidebar = rightsidebar, body, skin = "midnight")
  
  ui <- secure_app(ui)
  
#server ----
  
  server <- function(input, output) {
    
    #usuario
    
    usuario <- secure_server(
      check_credentials = check_credentials(contraseñas))
    
    output$usuario <- renderPrint({
      reactiveValuesToList(usuario)})
    
    #Segundo filtro dinamico
    
    output$filtro2 <- renderUI(dateRangeInput(inputId = "filtro2", 
                                              label = "Periodo:",
                                              min = min(datos$Fecha),
                                              max = Sys.Date(),
                                              start = input$filtro[1],
                                              end = input$filtro[2],
                                              language = "es",
                                              separator = "A",
                                              format = "dd-mm-yyyy"))
    
    #Datos
    
    data <- reactive({
      
      datos <- dplyr::filter(.data = datos, Fecha >= input$filtro2[1] & Fecha <= input$filtro2[2]) 
      
    })
    
    #KPI región A
    
    dataUtilidadA <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Utilidad = round(sum(Margen),0)) %>% 
        dplyr::ungroup() 
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "TOTAL", Utilidad = sum(dataA[2]))
      
      dataA <- data.frame(dataA)
      
      dataA$Utilidad <- format(dataA$Utilidad, big.mark = ",")
      
      UtilidadA <-  dataA[1:2]
      
    }) 
    
    dataVentasA <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Pesos) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0)) %>% 
        dplyr::ungroup()
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "TOTAL", Ventas = sum(dataA[2]))
      
      dataA <- data.frame(dataA)
      
      dataA$Ventas <- format(dataA$Ventas, big.mark = ",")
      
      VentasA <-  dataA[1:2]
      
    }) 
    
    dataCostosA <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Costo) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Costos = round(sum(Costo),0)) %>% 
        dplyr::ungroup()
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "TOTAL", Costos = sum(dataA[2]))
      
      dataA <- data.frame(dataA)
      
      dataA$Costos <- format(dataA$Costos, big.mark = ",")
      
      CostosA <-  dataA[1:2]
      
    }) 
    
    dataLitrosA <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Litros) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Litros = round(sum(Litros),0)) %>% 
        dplyr::ungroup()
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "TOTAL", Litros = sum(dataA[2]))
      
      dataA <- data.frame(dataA)
      
      dataA$Litros <- format(dataA$Litros, big.mark = ",")
      
      LitrosA <-  dataA[1:2]
      
    }) 
    
    dataDespachosA <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Importe) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Despachos = round(sum(Importe),0)) %>% 
        dplyr::ungroup()
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "TOTAL", Despachos = sum(dataA[2]))
      
      dataA <- data.frame(dataA)
      
      dataA$Despachos <- format(dataA$Despachos, big.mark = ",")
      
      Despachos <-  dataA[1:2]
      
    }) 
    
    dataPrecioLitroA <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, PrecioLitro) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(PrecioLitro = round(mean(PrecioLitro),2)) %>% 
        dplyr::ungroup()
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "PROMEDIO", PrecioLitro = round(mean(dataA$PrecioLitro),2))
      
      dataA <- data.frame(dataA)
      
      dataA$PrecioLitro <- format(dataA$PrecioLitro, big.mark = ",")
      
      LitrosA <-  dataA[1:2]
      
    }) 
    
    #KPI región B
    
    dataUtilidadB <- reactive({
      
      dataB <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Utilidad = round(sum(Margen),0)) %>% 
        dplyr::ungroup()
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "TOTAL", Utilidad = sum(dataB[2]))
      
      dataB <- data.frame(dataB)
      
      dataB$Utilidad <- format(dataB$Utilidad, big.mark = ",")
      
      UtilidadB <-  dataB[1:2]
      
    }) 
    
    dataVentasB <- reactive({
      
      dataB <- dplyr::select(.data = data(), Región, Fecha, Producto, Pesos) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0)) %>% 
        dplyr::ungroup()
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "TOTAL", Ventas = sum(dataB[2]))
      
      dataB <- data.frame(dataB)
      
      dataB$Ventas <- format(dataB$Ventas, big.mark = ",")
      
      VentasB <-  dataB[1:2]
      
    }) 
    
    dataCostosB <- reactive({
      
      dataB <- dplyr::select(.data = data(), Región, Fecha, Producto, Costo) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Costos = round(sum(Costo),0)) %>% 
        dplyr::ungroup()
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "TOTAL", Costos = sum(dataB[2]))
      
      dataB <- data.frame(dataB)
      
      dataB$Costos <- format(dataB$Costos, big.mark = ",")
      
      CostosB <-  dataB[1:2]
      
    }) 
    
    dataLitrosB <- reactive({
      
      dataB <- dplyr::select(.data = data(), Región, Fecha, Producto, Litros) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Litros = round(sum(Litros),0)) %>% 
        dplyr::ungroup()
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "TOTAL", Litros = sum(dataB[2]))
      
      dataB <- data.frame(dataB)
      
      dataB$Litros <- format(dataB$Litros, big.mark = ",")
      
      LitrosB <-  dataB[1:2]
      
    }) 
    
    dataDespachosB <- reactive({
      
      dataB <- dplyr::select(.data = data(), Región, Fecha, Producto, Importe) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Despachos = round(sum(Importe),0)) %>% 
        dplyr::ungroup()
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "TOTAL", Despachos = sum(dataB[2]))
      
      dataB <- data.frame(dataB)
      
      dataB$Despachos <- format(dataB$Despachos, big.mark = ",")
      
      DespachosB <-  dataB[1:2]
      
    }) 
    
    dataPrecioLitroB <- reactive({
      
      dataB <- dplyr::select(.data = data(), Región, Fecha, Producto, PrecioLitro) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(PrecioLitro = round(mean(PrecioLitro),2)) %>% 
        dplyr::ungroup()
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "PROMEDIO", PrecioLitro = mean((dataB$PrecioLitro),2))
      
      dataB <- data.frame(dataB)
      
      dataB$PrecioLitro <- format(dataB$PrecioLitro, big.mark = ",")
      
      LitrosB <-  dataB[1:2]
      
    }) 
    
    #KPI región C
    
    dataUtilidadC <- reactive({
      
      dataC <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Utilidad = round(sum(Margen),0)) %>% 
        dplyr::ungroup()
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "TOTAL", Utilidad = sum(dataC[2]))
      
      dataC <- data.frame(dataC)
      
      dataC$Utilidad <- format(dataC$Utilidad, big.mark = ",")
      
      UtilidadC <-  dataC[1:2]
      
    }) 
    
    dataVentasC <- reactive({
      
      dataC <- dplyr::select(.data = data(), Región, Fecha, Producto, Pesos) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0)) %>% 
        dplyr::ungroup()
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "TOTAL", Ventas = sum(dataC[2]))
      
      dataC <- data.frame(dataC)
      
      dataC$Ventas <- format(dataC$Ventas, big.mark = ",")
      
      VentasC <-  dataC[1:2]
      
    }) 
    
    dataCostosC <- reactive({
      
      dataC <- dplyr::select(.data = data(), Región, Fecha, Producto, Costo) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Costos = round(sum(Costo),0)) %>% 
        dplyr::ungroup()
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "TOTAL", Costos = sum(dataC[2]))
      
      dataC <- data.frame(dataC)
      
      dataC$Costos <- format(dataC$Costos, big.mark = ",")
      
      CostosC <-  dataC[1:2]
      
    }) 
    
    dataLitrosC <- reactive({
      
      dataC <- dplyr::select(.data = data(), Región, Fecha, Producto, Litros) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Litros = round(sum(Litros),0)) %>% 
        dplyr::ungroup()
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "TOTAL", Litros = sum(dataC[2]))
      
      dataC <- data.frame(dataC)
      
      dataC$Litros <- format(dataC$Litros, big.mark = ",")
      
      LitrosC <-  dataC[1:2]
      
    }) 
    
    dataDespachosC <- reactive({
      
      dataC <- dplyr::select(.data = data(), Región, Fecha, Producto, Importe) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Despachos = round(sum(Importe),0)) %>% 
        dplyr::ungroup()
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "TOTAL", Despachos = sum(dataC[2]))
      
      dataC <- data.frame(dataC)
      
      dataC$Despachos <- format(dataC$Despachos, big.mark = ",")
      
      DespachosC <-  dataC[1:2]
      
    }) 
    
    dataPrecioLitroC <- reactive({
      
      dataC <- dplyr::select(.data = data(), Región, Fecha, Producto, PrecioLitro) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(PrecioLitro = round(mean(PrecioLitro),2)) %>% 
        dplyr::ungroup()
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "PROMEDIO", PrecioLitro = mean((dataC$PrecioLitro),2))
      
      dataC <- data.frame(dataC)
      
      dataC$PrecioLitro <- format(dataC$PrecioLitro, big.mark = ",")
      
      LitrosC <-  dataC[1:2]
      
    }) 
    
    #Graficas serie de tiempo
    
    grafica1 <- reactive({
      
      datosGrafica <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen ) %>% 
        dplyr::filter(Región == "A") %>% 
        dplyr::group_by(Producto, Fecha) %>% 
        dplyr::summarise(Utilidad = sum(Margen))
      
      
      plot_ly(data=datosGrafica, x=~Fecha,  y = ~Utilidad,
              type = 'scatter', mode = 'lines',
              color = ~Producto , colors = c("#e33575", "#075383", "#b4b4b4")) %>%
        layout( plot_bgcolor = "#272c30", paper_bgcolor = "#272c30")  
      
      
    })
    
    grafica2 <- reactive({
      
      datosGrafica <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen ) %>% 
        dplyr::filter(Región == "B") %>% 
        dplyr::group_by(Producto, Fecha) %>% 
        dplyr::summarise(Utilidad = sum(Margen))
      
      
      plot_ly(data=datosGrafica, x=~Fecha,  y = ~Utilidad,
              type = 'scatter', mode = 'lines',
              color = ~Producto , colors = c("#e33575", "#075383", "#b4b4b4")) %>%
        layout( plot_bgcolor = "#272c30", paper_bgcolor = "#272c30")  
      
      
    })
    
    grafica3 <- reactive({
      
      datosGrafica <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen ) %>% 
        dplyr::filter(Región == "C") %>% 
        dplyr::group_by(Producto, Fecha) %>% 
        dplyr::summarise(Utilidad = sum(Margen))
      
      
      plot_ly(data=datosGrafica, x=~Fecha,  y = ~Utilidad,
              type = 'scatter', mode = 'lines',
              color = ~Producto , colors = c("#e33575", "#075383", "#b4b4b4")) %>%
        layout( plot_bgcolor = "#272c30", paper_bgcolor = "#272c30")  
      
      
    })
    
    #Grafica treemap
    
    grafica4 <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Estación, Fecha, Producto, Pesos, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::group_by(Producto, Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0)) %>% 
        dplyr::arrange(Producto)
      
      conteo <- dplyr::select(.data = dataA, Producto) %>% 
        dplyr::count()
      
      totales <- dplyr::select(.data = dataA, Producto, Estación, Ventas, Utilidad) %>% 
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(TotalVentas = sum(Ventas),
                         TotalUtlidad = sum(Utilidad))
      
      dataA <- data.frame(dataA)
      conteo <- data.frame(conteo)
      totales <- data.frame(totales)
      
      labels <- c("Diesel", 
                  dataA[1:conteo[1,2],2],
                  "Super",
                  dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),2],
                  "VPower",
                  dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),2])
      
      diesel <-c(" ", rep("Diesel", conteo[1,2]))
      super <- c(" ", rep("Super", conteo[2,2]))
      vpower <- c(" ", rep("VPower", conteo[3,2]))
      parents <- c(diesel,super,vpower)
      
      valores <- c(totales[1,2], 
                   dataA[1:conteo[1,2],3], 
                   totales[2,2], 
                   dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),3], 
                   totales[3,2], 
                   dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),3])
      
      Utilidad <- c(totales[1,3], 
                   dataA[1:conteo[1,2],4], 
                   totales[2,3], 
                   dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),4], 
                   totales[3,3], 
                   dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),4])
      
      ids <- paste0(parents, "-" , labels)
      
      ids[1] <- "Diesel"
      
      ids[conteo[1,2]+2] <- "Super"
      
      ids[conteo[1,2]+conteo[2,2]+3] <- "VPower"
      
      dataMap <- cbind(ids, labels,parents,valores,Utilidad)
      
      dataMap <- data.frame(dataMap)
      
      grafico <- plot_ly(data = dataMap, 
                         type = 'treemap',
                         ids = dataMap$ids,
                         labels = dataMap$labels,
                         parents = dataMap$parents,
                         values = dataMap$valores,
                         textposition = "top center",
                         branchvalues = "total") %>% 
        layout(plot_bgcolor = "#272c30",
               paper_bgcolor = "#272c30",
               margin=list(l=0, r=0, b=0, t=0),
               treemapcolorway=c("#075383", "#e33575", "#b4b4b4"))
      
      
      grafico
      
    })
    
    grafica5 <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Estación, Fecha, Producto, Pesos) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::group_by(Producto, Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0)) %>% 
        dplyr::arrange(Producto)
      
      conteo <- dplyr::select(.data = dataA, Producto) %>% 
        dplyr::count()
      
      totales <- dplyr::select(.data = dataA, Producto, Estación, Ventas) %>% 
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(Total = sum(Ventas))
      
      dataA <- data.frame(dataA)
      conteo <- data.frame(conteo)
      totales <- data.frame(totales)
      
      labels <- c("Diesel", 
                  dataA[1:conteo[1,2],2],
                  "Super",
                  dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),2],
                  "VPower",
                  dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),2])
      
      diesel <-c(" ", rep("Diesel", conteo[1,2]))
      super <- c(" ", rep("Super", conteo[2,2]))
      vpower <- c(" ", rep("VPower", conteo[3,2]))
      parents <- c(diesel,super,vpower)
      
      valores <- c(totales[1,2], 
                   dataA[1:conteo[1,2],3], 
                   totales[2,2], 
                   dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),3], 
                   totales[3,2], 
                   dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),3])
      
      Utilidad <- c(totales[1,3], 
                    dataA[1:conteo[1,2],4], 
                    totales[2,3], 
                    dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),4], 
                    totales[3,3], 
                    dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),4])
      
      ids <- paste0(parents, "-" , labels)
      
      ids[1] <- "Diesel"
      
      ids[conteo[1,2]+2] <- "Super"
      
      ids[conteo[1,2]+conteo[2,2]+3] <- "VPower"
      
      dataMap <- cbind(ids, labels,parents,valores,Utilidad)
      
      dataMap <- data.frame(dataMap)
      
      grafico <- plot_ly(data = dataMap, 
                         type = 'treemap',
                         ids = dataMap$ids,
                         labels=dataMap$labels,
                         parents= dataMap$parents,
                         values = dataMap$valores,
                         textposition = "top center",
                         branchvalues = "total") %>% 
        layout(plot_bgcolor = "#272c30",
               paper_bgcolor = "#272c30",
               margin=list(l=0, r=0, b=0, t=0),
               treemapcolorway=c("#075383", "#e33575", "#b4b4b4"))
      
      grafico
      
    })
    
    grafica6 <- reactive({
      
      dataA <- dplyr::select(.data = datos, Región, Estación, Fecha, Producto, Pesos, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::group_by(Producto, Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0)) %>% 
        dplyr::arrange(Producto)
      
      conteo <- dplyr::select(.data = dataA, Producto) %>% 
        dplyr::count()
      
      totales <- dplyr::select(.data = dataA, Producto, Estación, Ventas, Utilidad) %>% 
        dplyr::group_by(Producto) %>% 
        dplyr::summarise(TotalVentas = sum(Ventas),
                         TotalUtilidad = sum(Utilidad))
      
      dataA <- data.frame(dataA)
      conteo <- data.frame(conteo)
      totales <- data.frame(totales)
      
      labels <- c("Diesel", 
                  dataA[1:conteo[1,2],2],
                  "Super",
                  dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),2],
                  "VPower",
                  dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),2])
      
      diesel <-c(" ", rep("Diesel", conteo[1,2]))
      super <- c(" ", rep("Super", conteo[2,2]))
      vpower <- c(" ", rep("VPower", conteo[3,2]))
      parents <- c(diesel,super,vpower)
      
      valores <- c(totales[1,2], 
                   dataA[1:conteo[1,2],3], 
                   totales[2,2], 
                   dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),3], 
                   totales[3,2], 
                   dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),3])
      
      Utilidad <- c(totales[1,3], 
                    dataA[1:conteo[1,2],4], 
                    totales[2,3], 
                    dataA[c(conteo[1,2]+1):c(conteo[1,2]+conteo[2,2]),4], 
                    totales[3,3], 
                    dataA[c(conteo[1,2]+conteo[2,2]+1):c(conteo[1,2]+conteo[2,2]+conteo[3,2]),4])
      
      ids <- paste0(parents, "-" , labels)
      
      ids[1] <- "Diesel"
      
      ids[conteo[1,2]+2] <- "Super"
      
      ids[conteo[1,2]+conteo[2,2]+3] <- "VPower"
      
      dataMap <- cbind(ids, labels,parents,valores,Utilidad)
      
      dataMap <- data.frame(dataMap)
      
      grafico <- plot_ly(data = dataMap, 
                         type = 'treemap',
                         ids = dataMap$ids,
                         labels=dataMap$labels,
                         parents= dataMap$parents,
                         values = dataMap$valores,
                         textposition = "top center",
                         branchvalues = "total") %>% 
        layout(plot_bgcolor = "#272c30",
               paper_bgcolor = "#272c30",
               margin=list(l=0, r=0, b=0, t=0),
               treemapcolorway=c("#075383", "#e33575", "#b4b4b4"))
      
      grafico
      
    })
    
    #Región A: Sucursales
    
    dataSucursalAAd <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::filter(Producto=="Diesel") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalAAd <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalAAs <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::filter(Producto=="Super") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalAAs <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalAAv <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::filter(Producto=="VPower") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalAAv <-  dataA[c(1,2,4)]
      
    })
    
    dataSucursalABd <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::filter(Producto=="Diesel") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalABd <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalABs <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::filter(Producto=="Super") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalABs <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalABv <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="A") %>%
        dplyr::filter(Producto=="VPower") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalABv <-  dataA[c(1,2,4)]
      
    })
    
    #Región B: Sucursales
    
    dataSucursalBAd <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::filter(Producto=="Diesel") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalBAd <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalBAs <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::filter(Producto=="Super") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalBAs <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalBAv <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::filter(Producto=="VPower") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalBAv <-  dataA[c(1,2,4)]
      
    })
    
    dataSucursalBBd <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::filter(Producto=="Diesel") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalBBd <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalBBs <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::filter(Producto=="Super") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalABs <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalBBv <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="B") %>%
        dplyr::filter(Producto=="VPower") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalBBv <-  dataA[c(1,2,4)]
      
    })
    
    #Región C: Sucursales
    
    dataSucursalCAd <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::filter(Producto=="Diesel") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalCAd <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalCAs <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::filter(Producto=="Super") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalCAs <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalCAv <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::filter(Producto=="VPower") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(desc(Ventas)) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalCAv <-  dataA[c(1,2,4)]
      
    })
    
    dataSucursalCBd <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::filter(Producto=="Diesel") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalCBd <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalCBs <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::filter(Producto=="Super") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalCBs <-  dataA[c(1,2,4)]
      
    }) 
    
    dataSucursalCBv <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos, Margen) %>% 
        dplyr::filter(Región=="C") %>%
        dplyr::filter(Producto=="VPower") %>%
        dplyr::group_by(Estación) %>% 
        dplyr::summarise(Ventas = round(sum(Pesos),0),
                         Utilidad = round(sum(Margen),0),
                         Ganancia = paste0(round((sum(Margen)/sum(Pesos))*100,2),"%"))%>% 
        dplyr::arrange(Ventas) %>% 
        head(3)
      
      dataA <- data.frame(dataA)
      
      dataA[2:3] <- format(dataA[2:3], big.mark = ",")
      
      SucursalCBv <-  dataA[c(1,2,4)]
      
    })
    
    #Grafica ACP
    
    PCA <- reactive({
      
      dataA <- dplyr::select(.data = datos, Región, Fecha, Producto, Estación, Pesos) %>% 
        dplyr::group_by(Estación, Región, Producto) %>% 
        dplyr::summarise(Pesos = sum(Pesos))
      
      dataA <- tidyr::spread(data = dataA, Producto, Pesos, fill=0)
      
      dataA <- data.frame(dataA)
      
      rownames(dataA) <- dataA$Estación
      
      dataA <- data.frame(dataA)
      
      PCA <- FactoMineR::PCA(dataA, graph= FALSE, quali.sup = 1:2)
      
      dataA[3:5] <- format(dataA[3:5], big.mark = ",")
      
      dataA <- data.frame(dataA)
      
      grafica <- ggbiplot::ggbiplot(pcobj = PCA) +
        geom_point(size = 3 ,
                   aes(color = dataA$Región,
                       text = paste("Sucursal:" , dataA$Estación,
                                    '</br> Diesel:' ,dataA$Diesel,
                                    '</br> Super:', dataA$Super,
                                    '</br> VPower:', dataA$VPower)))+
        guides(color = guide_legend(title = "Región"))+
        ggthemes::theme_fivethirtyeight(base_size = 15) +
        ggplot2::geom_vline(xintercept = 0, colour= "white", linetype = "dotted") +
        ggplot2::geom_hline(yintercept = 0, colour= "white", linetype = "dotted") +
        ggplot2::xlab("")+
        ggplot2::labs(title = "Analisis de Componentes Principales", subtitle = "Biplot")+
        ggplot2::ylab("") +
        ggplot2::xlim(c(-4,4)) +
        ggplot2::ylim(c(-3,3)) +
        ggplot2::scale_colour_manual(values =  c("#e33575", "#075383", "#b4b4b4"))+
        ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "black"),
                       panel.grid.minor = ggplot2::element_blank(), 
                       panel.border = ggplot2::element_blank(),
                       plot.background = ggplot2::element_rect(fill = "#272c30", colour = "#272c30"), 
                       panel.background = ggplot2::element_rect(fill = "#272c30", colour = "#272c30"), 
                       plot.title = ggplot2::element_text(colour = 'white'),
                       plot.subtitle = ggplot2::element_text(colour = 'white'),
                       plot.caption = ggplot2::element_text(color = "white"),
                       axis.text = ggplot2::element_text(colour = "white"),
                       axis.title.y.left = ggplot2::element_text(colour = "white"),
                       axis.title.x.bottom = ggplot2::element_text(colour = "white"),
                       legend.position =  "bottom",
                       legend.text = element_text(colour = "white"),
                       legend.title = element_text(colour="white"),
                       legend.background = element_rect(fill="#272c30"),
                       axis.title.y  = ggplot2::element_text(angle = 90, colour = "white"))
      
      grafica <- ggplotly(grafica)
    })
    
    
    #Output 
    
    output$UtilidadA <- renderTable(dataUtilidadA(), colnames = FALSE, align = "l",spacing = "xs", digits = 0)  
    output$VentasA <- renderTable(dataVentasA(), colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$CostosA <- renderTable(dataCostosA(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$LitrosA <- renderTable(dataLitrosA(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$DespachoA <- renderTable(dataDespachosA(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$PrecioA <- renderTable(dataPrecioLitroA(), colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$UtilidadB <- renderTable(dataUtilidadB(), colnames = FALSE, align = "l",spacing = "xs", digits = 0)  
    output$VentasB <- renderTable(dataVentasB(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$CostosB <- renderTable(dataCostosB(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$LitrosB <- renderTable(dataLitrosB(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$DespachoB <- renderTable(dataDespachosB(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$PrecioB <- renderTable(dataPrecioLitroB(), colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$UtilidadC <- renderTable(dataUtilidadC(), colnames = FALSE, align = "l",spacing = "xs", digits = 0)  
    output$VentasC <- renderTable(dataVentasC(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$CostosC <- renderTable(dataCostosC(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$LitrosC <- renderTable(dataLitrosC(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$DespachoC <- renderTable(dataDespachosC(),colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$PrecioC <- renderTable(dataPrecioLitroC(), colnames = FALSE, align = "l",spacing = "xs", digits = 0)
    output$GraficaSerieTiempoA <- renderPlotly(grafica1())
    output$GraficaSerieTiempoB <- renderPlotly(grafica2())
    output$GraficaSerieTiempoC <- renderPlotly(grafica3())
    output$treemapA <- renderPlotly(grafica4())
    output$treemapB <- renderPlotly(grafica5())
    output$treemapC <- renderPlotly(grafica6())
    output$SucursalAAd <- renderTable(dataSucursalAAd(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalAAs <- renderTable(dataSucursalAAs(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalAAv <- renderTable(dataSucursalAAv(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalABd <- renderTable(dataSucursalABd(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalABs <- renderTable(dataSucursalABs(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalABv <- renderTable(dataSucursalABv(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalBAd <- renderTable(dataSucursalBAd(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalBAs <- renderTable(dataSucursalBAs(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalBAv <- renderTable(dataSucursalBAv(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalBBd <- renderTable(dataSucursalBBd(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalBBs <- renderTable(dataSucursalBBs(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalBBv <- renderTable(dataSucursalBBv(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalCAd <- renderTable(dataSucursalCAd(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalCAs <- renderTable(dataSucursalCAs(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalCAv <- renderTable(dataSucursalCAv(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalCBd <- renderTable(dataSucursalCBd(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalCBs <- renderTable(dataSucursalCBs(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$SucursalCBv <- renderTable(dataSucursalCBv(), colnames = TRUE, align = "l",spacing = "xs", digits = 0)
    output$PCA <- renderPlotly(PCA())
    output$Estación <- renderUI(selectInput(inputId = "Estación", 
                                           label = "", 
                                           choices = if(input$Región == "Total"){
                                                        c("Total")
                                                     }else{
                                                      data.frame(rbind("Total", dplyr::filter(data(), Región == input$Región) %>% 
                                                                                dplyr::distinct(Estación)))   
                                                               }))
    output$Producto <- renderUI(selectInput(inputId = "Producto", 
                                            label = "", 
                                            choices = if(input$Estación =="Total"){
                                                         data.frame(rbind("Total", dplyr::distinct(data(), Producto)))
                                            }else{
                                              data.frame(rbind("Total", dplyr::filter(data(), Estación == input$Estación) %>% 
                                                                        dplyr::distinct(Producto)))
                                            }))
    
    DatosArima <- reactive({
      
      if(input$Región == "Total"){
        if(input$Producto == "Total"){
          
          datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
            dplyr::group_by(Fecha) %>% 
            dplyr::summarise(Ventas = sum(Pesos))
        }else{
          
          datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos)%>% 
            dplyr::filter(Producto == input$Producto) %>% 
            dplyr::group_by(Fecha) %>% 
            dplyr::summarise(Ventas = sum(Pesos))
        }
      }else{
        
        if(input$Estación == "Total"){
          if(input$Producto == "Total"){
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
              dplyr::filter(Región == input$Región) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
            
          }else{
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
              dplyr::filter(Región == input$Región, Producto == input$Producto) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
          }
        }else{
          
          if(input$Producto == "Total"){
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
              dplyr::filter(Región == input$Región, Estación == input$Estación) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
          }else{
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>%
              dplyr::filter(Región == input$Región, Estación == input$Estación, Producto == input$Producto) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
            
          }
        }
      }
      
      datosModeloF <- dplyr::filter(datosModelo, Fecha >= max(datosModelo$Fecha)-90) 
      datosModeloF <- data.frame(datosModeloF)
      rownames(datosModeloF) <- datosModeloF$Fecha
      datosModelo <- datosModeloF[2]
      
    })
    DatosArimaP <- reactive({
      
      if(input$Región == "Total"){
        if(input$Producto == "Total"){
          
          datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
            dplyr::group_by(Fecha) %>% 
            dplyr::summarise(Ventas = sum(Pesos))
        }else{
          
          datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos)%>% 
            dplyr::filter(Producto == input$Producto) %>% 
            dplyr::group_by(Fecha) %>% 
            dplyr::summarise(Ventas = sum(Pesos))
        }
      }else{
        
        if(input$Estación == "Total"){
          if(input$Producto == "Total"){
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
              dplyr::filter(Región == input$Región) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
            
          }else{
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
              dplyr::filter(Región == input$Región, Producto == input$Producto) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
          }
        }else{
          
          if(input$Producto == "Total"){
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>% 
              dplyr::filter(Región == input$Región, Estación == input$Estación) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
          }else{
            
            datosModelo <- dplyr::select(.data = datos, Región, Estación, Producto, Fecha, Pesos) %>%
              dplyr::filter(Región == input$Región, Estación == input$Estación, Producto == input$Producto) %>% 
              dplyr::group_by(Fecha) %>% 
              dplyr::summarise(Ventas = sum(Pesos))
            
          }
        }
      }
      
      datosModeloF <- dplyr::filter(datosModelo, Fecha >= max(datosModelo$Fecha)-90) 
      datosModeloF <- data.frame(datosModeloF)
      datosModeloF <- datosModeloF$Fecha
      
    })
    Arima <- reactive({
    
      semana <- as.numeric(strftime( min(DatosArimaP()), format = "%V"))
      dia <-  as.numeric(strftime( min(DatosArimaP()), format = "%u"))
      
      datosModelo1 <- ts(DatosArima(), frequency = 7, start = c(semana,dia))
      modeloP <- forecast::auto.arima(DatosArima(), stepwise = FALSE)
      modelo <- forecast::auto.arima(datosModelo1, stepwise = FALSE)
      pronosticos <- input$pronosticos
      pronostico95 <- forecast::forecast(modelo, pronosticos, level=95)
      pronostico80 <- forecast::forecast(modelo, pronosticos, level=80)
      dias <- c(1:pronosticos , pronosticos:1)
      x <- c(as.Date(max(rownames(DatosArima()))) + dias)
      
      Fecha <- as.data.frame.Date(x[1:(length(x)/2)])
      colnames(Fecha) <- "Fecha"
      Pronostico <- pronostico95$mean
      Pronostico <- format(Pronostico, big.mark = ",")
      datosTabla <- data.frame(cbind(Fecha, Pronostico))
      
      
      parte1 <- list(
        line = list(
          color = "#075383",
          fillcolor = "#075383"),
        mode = "lines",
        name = "observados",
        type = "scatter",
        x= rownames(datosModelo),
        y= modeloP$x,
        xaxis = "x", 
        yaxis = "y")
      
      parte2 <- list(
        fill="toself",
        line = list(
          color = "#DADADA", 
          fillcolor = "#DADADA"),
        mode = "lines", 
        name = "95% de confianza", 
        type = "scatter",
        x = x,
        y = c(pronostico95$upper, pronostico95$lower[pronosticos:1]),
        xaxis = "x", 
        yaxis = "y", 
        hoveron = "points")
      
      parte3 <- list(
        fill="toself",
        line = list(
          color = "#b4b4b4", 
          fillcolor = "#b4b4b4"),
        mode = "lines", 
        name = "80% de confianza", 
        type = "scatter",
        x = x,
        y = c(pronostico80$upper, pronostico80$lower[pronosticos:1]),
        xaxis = "x", 
        yaxis = "y", 
        hoveron = "points")
      
      parte4 <- list(
        line = list(
          color = "#e33575",
          fillcolor = "#e33575"),
        mode = "lines",
        name = "predicción",
        type = "scatter",
        x= x[1:pronosticos],
        y= pronostico95$mean,
        xaxis = "x", 
        yaxis = "y")
      
      data <- list(parte1, parte2, parte3, parte4)
      
      layout <- list(
        title = "Pronostico de Ventas", 
        xaxis = list(
          title = "Tiempo", 
          domain = c(0, 1)
        ), 
        yaxis = list(
          title = "Ventas", 
          domain = c(0, 1)
        ), 
        margin = list(
          b = 40, 
          l = 60, 
          r = 10, 
          t = 25
        ))
      
      p <- plot_ly()
      p <- add_trace(p, line=parte1$line, mode=parte1$mode, name=parte1$name, type=parte1$type, x=parte1$x, y=parte1$y, xaxis=parte1$xaxis, yaxis=parte1$yaxis)
      p <- add_trace(p, fill=parte2$fill, fillcolor=parte2$line, line=parte2$line, mode=parte2$mode, name=parte2$name, type=parte2$type, x=parte2$x, y=parte2$y, xaxis=parte2$xaxis, yaxis=parte2$yaxis, hoveron=parte2$hoveron)
      p <- add_trace(p, fill=parte3$fill, fillcolor=parte2$line, line=parte3$line, mode=parte3$mode, name=parte3$name, type=parte3$type, x=parte3$x, y=parte3$y, xaxis=parte3$xaxis, yaxis=parte3$yaxis, hoveron=parte3$hoveron)
      p <- add_trace(p, line=parte4$line, mode=parte4$mode, name=parte4$name, type=parte4$type, x=parte4$x, y=parte4$y, xaxis=parte4$xaxis, yaxis=parte4$yaxis)
      p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin)
      p <- layout(p, plot_bgcolor = "#272c30", paper_bgcolor = "#272c30")
      
      p
         
    })
    TablaArima <- reactive({
      
      dataModelo <- ts(DatosArima(), frequency = 7, start = c(semana,dia))
      modelo <- forecast::auto.arima(dataModelo, stepwise = FALSE)
      pronosticos <- input$pronosticos
      pronostico95 <- forecast::forecast(modelo, pronosticos, level=95)
      dias <- c(1:pronosticos , pronosticos:1)
      x <- c(as.Date(max(rownames(DatosArima()))) + dias)
      
      Fecha <- as.data.frame.Date(x[1:(length(x)/2)])
      colnames(Fecha) <- "Fecha"
      Pronostico <- pronostico95$mean
      Pronostico <- format(Pronostico, big.mark = ",")
      datosTabla <- data.frame(cbind(Fecha, Pronostico))
      rownames(datosTabla) <- datosTabla$Fecha
      datosTabla <- datosTabla[2]
      
    })
    Modelo <- reactive({
      modelo <- forecast::auto.arima(DatosArima(), stepwise = FALSE)
      modelo <- modelo$arma
    })
    
    
    output$Arima <- renderPlotly(Arima())
    output$tablaPronostico <- renderTable(TablaArima(), rownames = TRUE)
    output$Descarga <- downloadHandler(
      filename = function(){
        paste("PronosticoVentas-(",input$Región, input$Producto, input$Estación, ")", ".csv", sep = "")},
      content = function(file) {
        write.csv(TablaArima(), file)})
    output$modelo <- renderText(Modelo())
 
  }
  
#ShinyApp ----
  
  shinyApp(ui = ui, server = server)


