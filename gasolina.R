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



options(scipen=999)

datos <- data.frame(readxl::read_excel("C:/Users/0303u/Google Drive/Trabajo/Trabajo SBR/Shell/Dashboard Gasolina Dinamico/DatosGasolina.xlsx", sheet = "DatosShell"))

datos$Fecha <- as.Date(datos$Fecha)

if(interactive()){
  
  header <- dashboardHeaderPlus(title = "Shell")
  
  sidebar <- dashboardSidebar({
    sidebarMenu(
      menuItem("Principal", tabName = "Principal", icon = icon("chart-line"),
               menuSubItem("Región A", tabName = "RegiónA", icon = shiny::icon("stream")),
               menuSubItem("Región B", tabName = "RegiónB", icon = shiny::icon("stream")),
               menuSubItem("Región C", tabName = "RegiónC", icon = shiny::icon("stream"))
      ),
      menuItem("Avanzado", tabName = "Avanzado", icon = icon("laptop-code"),
               menuSubItem("PCA", tabName = "PCA", icon = shiny::icon("stream")),
               menuSubItem("Pronostico", tabName = "Pronostico", icon = shiny::icon("stream")),
               menuSubItem("Ventas-vs-Precio", tabName = "Ventas-vs-Precio", icon = shiny::icon("stream"))),
      dateRangeInput(inputId = "filtro", 
                     label = "Periodo:",
                     min = min(datos$Fecha),
                     max = Sys.Date(),
                     start = "2020-01-01",
                     end = "2020-12-31",
                     language = "es",
                     separator = "A",
                     format = "dd-mm-yyyy"
                     
      ),
      sliderInput(inputId = "filtro2",
                  label = "Periodo",
                  min = as.Date(min(datos$Fecha)),
                  max = Sys.Date(),
                  value = as.Date(c("2020-01-01","2020-12-31")),
                  animate = TRUE
      )
    )})
  
    datosFijos <- dplyr::filter(.data = datos, Fecha >= "2020-01-01" & Fecha <= "2020-12-31")
  
  datosFijosA <- dplyr::select(.data = datosFijos, Región, Fecha, Producto, Margen, Pesos, Costo, Litros, Importe, PrecioLitro) %>% 
    dplyr::filter(Región=="A") %>%
    dplyr::summarise(Utilidad = round(sum(Margen),0),
                     Ventas = round(sum(Pesos),0),
                     Costos = round(sum(Costo),0),
                     Litros = round(sum(Litros),0),
                     Despachos = round(sum(Importe),0),
                     Precio = round(mean(PrecioLitro),2))
  
  datosFijosB <- dplyr::select(.data = datosFijos, Región, Fecha, Producto, Margen, Pesos, Costo, Litros, Importe, PrecioLitro) %>% 
    dplyr::filter(Región=="B") %>%
    dplyr::summarise(Utilidad = round(sum(Margen),0),
                     Ventas = round(sum(Pesos),0),
                     Costos = round(sum(Costo),0),
                     Litros = round(sum(Litros),0),
                     Despachos = round(sum(Importe),0),
                     Precio = round(mean(PrecioLitro),2))
  
  datosFijosC <- dplyr::select(.data = datosFijos, Región, Fecha, Producto, Margen, Pesos, Costo, Litros, Importe, PrecioLitro) %>% 
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
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "RegiónA",
              fluidRow(
                gradientBox(
                  title = "Utilidad",
                  icon = "fa fa-hand-holding-usd",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("UtilidadA"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[1], "   Anual")
                ),
                gradientBox(
                  title = "Ventas",
                  icon = "fa fa-money-check-alt",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("VentasA"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[2], "   Anual")
                ),
                gradientBox(
                  title = "Costos",
                  icon = "fa fa-search-dollar",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("CostosA"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[3], "   Anual")
                ),
                gradientBox(
                  title = "Litros",
                  icon = "fa fa-gas-pump",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("LitrosA"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[4], "   Anual")
                ),
                gradientBox(
                  title = "Despacho",
                  icon = "fa fa-oil-can",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("DespachoA"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosA[5], "   Anual")
                ),
                gradientBox(
                  title = "Precio",
                  icon = "fa fa-dollar-sign",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("PrecioA"),style="font-size:80%"),
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
                  tabPanel(title = "Diesel", status = "primary", div(tableOutput("SucursalAAd"),style="font-size:90%")),
                  tabPanel(title = "Super", status = "primary", div(tableOutput("SucursalAAs"),style="font-size:90%")),
                  tabPanel(title = "VPower", status = "primary", div(tableOutput("SucursalAAv"),style="font-size:90%"))),
                  title = "Sucursales con mayores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Diesel", status = "primary", div(tableOutput("SucursalABd"),style="font-size:90%")),
                  tabPanel(title = "Super", status = "primary", div(tableOutput("SucursalABs"),style="font-size:90%")),
                  tabPanel(title = "VPower", status = "primary", div(tableOutput("SucursalABv"),style="font-size:90%"))),
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
                  footer = div(tableOutput(outputId = "UtilidadB"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[1], "   Anual")
                ),
                gradientBox(
                  title = "Ventas",
                  icon = "fa fa-money-check-alt",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("VentasB"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[2], "   Anual")
                ),
                gradientBox(
                  title = "Costos",
                  icon = "fa fa-search-dollar",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("CostosB"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[3], "   Anual")
                ),
                gradientBox(
                  title = "Litros",
                  icon = "fa fa-gas-pump",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("LitrosB"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[4], "   Anual")
                ),
                gradientBox(
                  title = "Despacho",
                  icon = "fa fa-oil-can",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("DespachoB"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosB[5], "   Anual")
                ),
                gradientBox(
                  title = "Precio",
                  icon = "fa fa-dollar-sign",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("PrecioB"),style="font-size:80%"),
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
                  tabPanel(title = "Diesel", status = "primary", div(tableOutput("SucursalBAd"),style="font-size:90%")),
                  tabPanel(title = "Super", status = "primary", div(tableOutput("SucursalBAs"),style="font-size:90%")),
                  tabPanel(title = "VPower", status = "primary", div(tableOutput("SucursalBAv"),style="font-size:90%"))),
                  title = "Sucursales con mayores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Diesel", status = "primary", div(tableOutput("SucursalBBd"),style="font-size:90%")),
                  tabPanel(title = "Super", status = "primary", div(tableOutput("SucursalBBs"),style="font-size:90%")),
                  tabPanel(title = "VPower", status = "primary", div(tableOutput("SucursalBBv"),style="font-size:90%"))),
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
                  footer = div(tableOutput("UtilidadC"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[1], "   Anual")
                ),
                gradientBox(
                  title = "Ventas",
                  icon = "fa fa-money-check-alt",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("VentasC"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[2], "   Anual")
                ),
                gradientBox(
                  title = "Costos",
                  icon = "fa fa-search-dollar",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("CostosC"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[3], "   Anual")
                ),
                gradientBox(
                  title = "Litros",
                  icon = "fa fa-gas-pump",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("LitrosC"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[4], "   Anual")
                ),
                gradientBox(
                  title = "Despacho",
                  icon = "fa fa-oil-can",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("DespachoC"),style="font-size:80%"),
                  footer_padding = FALSE,
                  paste0(datosFijosC[5], "   Anual")
                ),
                gradientBox(
                  title = "Precio",
                  icon = "fa fa-dollar-sign",
                  gradientColor = "maroon", 
                  boxToolSize = "xs",
                  width = 2,
                  footer = div(tableOutput("PrecioC"),style="font-size:80%"),
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
                  tabPanel(title = "Diesel", status = "primary", div(tableOutput("SucursalCAd"),style="font-size:90%")),
                  tabPanel(title = "Super", status = "primary", div(tableOutput("SucursalCAs"),style="font-size:90%")),
                  tabPanel(title = "VPower", status = "primary", div(tableOutput("SucursalCAv"),style="font-size:90%"))),
                  title = "Sucursales con mayores ventas",
                  width = 4,
                  collapsible = TRUE,
                  closable = FALSE,
                  status = "primary",
                  footer_padding = FALSE),
                boxPlus(tabBox(
                  width = 15,
                  tabPanel(title = "Diesel", status = "primary", div(tableOutput("SucursalCBd"),style="font-size:90%")),
                  tabPanel(title = "Super", status = "primary", div(tableOutput("SucursalCBs"),style="font-size:90%")),
                  tabPanel(title = "VPower", status = "primary", div(tableOutput("SucursalCBv"),style="font-size:90%"))),
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
              ))
    )
  )
  
  
  
  
  ui <- dashboardPagePlus(header, sidebar, body, skin = "midnight")
  
  
  server <- function(input, output) {
    
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
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "Total", Utilidad = sum(dataA[2]))
      
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
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "Total", Ventas = sum(dataA[2]))
      
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
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "Total", Costos = sum(dataA[2]))
      
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
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "Total", Litros = sum(dataA[2]))
      
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
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "Total", Despachos = sum(dataA[2]))
      
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
      
      dataA <- dplyr::add_row(.data = dataA, Producto = "Promedio", PrecioLitro = round(mean(dataA$PrecioLitro),2))
      
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
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "Total", Utilidad = sum(dataB[2]))
      
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
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "Total", Ventas = sum(dataB[2]))
      
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
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "Total", Costos = sum(dataB[2]))
      
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
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "Total", Litros = sum(dataB[2]))
      
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
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "Total", Despachos = sum(dataB[2]))
      
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
      
      dataB <- dplyr::add_row(.data = dataB, Producto = "Promedio", PrecioLitro = mean((dataB$PrecioLitro),2))
      
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
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "Total", Utilidad = sum(dataC[2]))
      
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
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "Total", Ventas = sum(dataC[2]))
      
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
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "Total", Costos = sum(dataC[2]))
      
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
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "Total", Litros = sum(dataC[2]))
      
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
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "Total", Despachos = sum(dataC[2]))
      
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
      
      dataC <- dplyr::add_row(.data = dataC, Producto = "Promedio", PrecioLitro = mean((dataC$PrecioLitro),2))
      
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
      
      grafica <- ggplot2::ggplot(data = datosGrafica, ggplot2::aes(x=Fecha, y=Utilidad, colour=Producto)) +
        ggplot2::geom_line() +
        ggthemes::theme_fivethirtyeight(base_size = 8) +
        ggplot2::xlab("")+
        ggplot2::labs()+
        ggplot2::ylab("Utilidad") +
        ggplot2::scale_x_date(date_breaks = "1 months",
                              date_labels = "%b %y")+
        ggplot2::scale_colour_manual(values =  c("#e33575", "#075383", "#b4b4b4"))+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
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
                       axis.title.y  = ggplot2::element_text(angle = 90, colour = "white"),
                       axis.title.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(colour = "white", angle = 15))
      
      grafica <- ggplotly(grafica)
      
      grafica
      
    })
    
    grafica2 <- reactive({
      
      datosGrafica <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen ) %>% 
        dplyr::filter(Región == "B") %>% 
        dplyr::group_by(Producto, Fecha) %>% 
        dplyr::summarise(Utilidad = sum(Margen))
      
      grafica <- ggplot2::ggplot(data = datosGrafica, ggplot2::aes(x=Fecha, y=Utilidad, colour=Producto)) +
        ggplot2::geom_line() +
        ggthemes::theme_fivethirtyeight(base_size = 8) +
        ggplot2::xlab("")+
        ggplot2::labs()+
        ggplot2::ylab("Utilidad") +
        ggplot2::scale_x_date(date_breaks = "1 months",
                              date_labels = "%b %y")+
        ggplot2::scale_colour_manual(values =  c("#e33575", "#075383", "#b4b4b4"))+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
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
                       axis.title.y  = ggplot2::element_text(angle = 90, colour = "white"),
                       axis.title.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(colour = "white", angle = 15))
      
      grafica <- ggplotly(grafica)
      
      grafica
      
    })
    
    grafica3 <- reactive({
      
      datosGrafica <- dplyr::select(.data = data(), Región, Fecha, Producto, Margen ) %>% 
        dplyr::filter(Región == "C") %>% 
        dplyr::group_by(Producto, Fecha) %>% 
        dplyr::summarise(Utilidad = sum(Margen))
      
      grafica <- ggplot2::ggplot(data = datosGrafica, ggplot2::aes(x=Fecha, y=Utilidad, colour=Producto)) +
        ggplot2::geom_line() +
        ggthemes::theme_fivethirtyeight(base_size = 8) +
        ggplot2::xlab("")+
        ggplot2::labs()+
        ggplot2::ylab("Utilidad") +
        ggplot2::scale_x_date(date_breaks = "1 months",
                              date_labels = "%b %y")+
        ggplot2::scale_colour_manual(values =  c("#e33575", "#075383", "#b4b4b4"))+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
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
                       axis.title.y  = ggplot2::element_text(angle = 90, colour = "white"),
                       axis.title.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(colour = "white", angle = 15))
      
      grafica <- ggplotly(grafica)
      
      grafica
      
    })
    
    #Grafica treemap
    
    grafica4 <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Estación, Fecha, Producto, Pesos) %>% 
        dplyr::filter(Región=="A") %>%
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
      
      grafico <- plot_ly(data = dataA, 
                         type = "treemap",
                         labels=labels,
                         parents= parents,
                         values = valores,
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
      
      grafico <- plot_ly(data = dataA, 
                         type = "treemap",
                         labels=labels,
                         parents= parents,
                         values = valores,
                         textposition = "top center",
                         branchvalues = "total") %>% 
        layout(plot_bgcolor = "#272c30",
               paper_bgcolor = "#272c30",
               margin=list(l=0, r=0, b=0, t=0),
               treemapcolorway=c("#075383", "#e33575", "#b4b4b4"))
      
      
      grafico
      
    })
    
    grafica6 <- reactive({
      
      dataA <- dplyr::select(.data = data(), Región, Estación, Fecha, Producto, Pesos) %>% 
        dplyr::filter(Región=="C") %>%
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
      
      grafico <- plot_ly(data = dataA, 
                         type = "treemap",
                         labels=labels,
                         parents= parents,
                         values = valores,
                         textposition = "top center",
                         branchvalues = "total") %>% 
        layout(plot_bgcolor = "#272c30",
               paper_bgcolor = "#272c30",
               margin=list(l=0, r=0, b=0, t=0),
               treemapcolorway=c("#075383", "#e33575", "#b4b4b4"))
      
      
      grafico
      
    })
    
    #Región B: Sucursales
    
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
      
      dataA <- dplyr::select(.data = data(), Región, Fecha, Producto, Estación, Pesos) %>% 
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
        ggplot2::xlab("")+
        ggplot2::labs(title = "Analisis de Componentes Principales", subtitle = "Biplot")+
        ggplot2::ylab("") +
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
  }
  
  shinyApp(ui = ui, server = server)
  
}

