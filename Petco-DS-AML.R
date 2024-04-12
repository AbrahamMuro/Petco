##################################################################
###             DEVELOPER: ABRAHAM MURO LEZAMA                 ###
###       APLICACION PARA LA COMPRENSION DE DATOS PETCO        ###
###                     12 - ABRIL - 2024                      ###
##################################################################
rm(list = ls())
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(plotly)

options(scipen = 999)

petco <- "https://raw.githubusercontent.com/AbrahamMuro/Petco/main/BDDTest.txt"
data <- read.csv(petco,header = TRUE, sep = "\t", stringsAsFactors = FALSE)

data$Fecha.registro <- as.Date(data$Fecha.registro, format = "%d.%m.%Y")
data$Importe.netos.cupones <- as.numeric(gsub(",", "", data$`Importe.netos.cupones`))

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Análisis Petco"),
  tabsetPanel(
    tabPanel("Home",
             fluidRow(
               column(12, 
                      h2("Acerca de mí"),
                      p("¡Hola! Soy Abraham Muro y estoy encantado de presentarte este análisis."),
                      p("Esta aplicación analiza los datos de ventas y te permite visualizar diferentes aspectos de las ventas en Petco."),
                      p("¡Da click en Análisis de Ventas y descubre más sobre tus ventas!"),
                      p("GitHub:"),
                      a("https://github.com/AbrahamMuro", href = "https://github.com/AbrahamMuro"),
                      br(),
                      actionButton("url_btn", "Ver URLs")
               )
             )
    ),
    tabPanel("Análisis de Ventas",
             sidebarLayout(
               sidebarPanel(
                 selectInput("opcion", "Selecciona una opción:",
                             choices = c("Ventas por fecha", "Ventas por origen de e-commerce","Ventas por segmento de clientes", "Resumen de ventas", "Clientes recurrentes"))
               ),
               mainPanel(
                 plotOutput("plot"),
                 tableOutput("summary")
               )
             )
    ),
    modalDialog(
      id = "url_modal",
      title = "URLs",
      "Aquí puedes encontrar más información:",
      a("GitHub de Abraham Muro", href = "https://github.com/AbrahamMuro"),
      br(),
      footer = tagList(
        actionButton("close_modal", "Cerrar")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$url_btn, {
    showModal("url_modal")
  })
  
  observeEvent(input$close_modal,{
    removeModal()
  })
  
  output$plot <- renderPlot({
    switch(input$opcion,
           "Ventas por fecha" = {
             ventas_por_fecha <- aggregate(Importe.netos.cupones ~ Fecha.registro, data = data, sum)
             ggplot(ventas_por_fecha, aes(x = Fecha.registro, y = Importe.netos.cupones)) +
               geom_point(color = "blue") +
               geom_path(color = "blue", aes(group = 1)) +
               labs(x = "Fecha", y = "Ventas", title = "Ventas por fecha")
           },
           "Ventas por origen de e-commerce" = {
             ventas_por_origen <- aggregate(Importe.netos.cupones ~ Origen.e.commerce, data = data, sum)
             ventas_por_origen <- ventas_por_origen[order(ventas_por_origen$`Importe.netos.cupones`, decreasing = TRUE), ]
             colors1 <- viridisLite::viridis(nrow(ventas_por_origen), begin = 0, end = 1)
             ggplot(ventas_por_origen, aes(x = "", y = `Importe.netos.cupones`, fill = Origen.e.commerce)) +
               geom_bar(stat = "identity") +
               coord_polar(theta = "y") +
               scale_fill_manual(values = colors1) +
               labs(x = NULL, y = NULL, title = "Ventas por origen de e-commerce") +
               theme_void() +
               theme(legend.position = "bottom")
           },
           "Ventas por segmento de clientes" = {
             ventas_por_segmento <- aggregate(Importe.netos.cupones ~ Cliente..Segmento, data = data, sum)
             barplot(ventas_por_segmento$Importe.netos.cupones, names.arg = ventas_por_segmento$Cliente..Segmento, xlab = "Segmento de cliente", ylab = "Ventas", main = "Ventas por segmento de clientes", col = viridis(nrow(ventas_por_segmento)))
           },
           "Resumen de ventas" = {
             importe_total_ventas <- sum(data$Importe.netos.cupones)
             importe_por_categoria <- aggregate(Importe.netos.cupones ~ Categoria, data = data, sum)
             proporciones <- importe_por_categoria$Importe.netos.cupones / importe_total_ventas
             nombres <- importe_por_categoria$Categoria
             pie(proporciones, labels = nombres, main = "Proporción de Ventas por Categoría", col = viridis(length(proporciones)))
           },
           "Clientes recurrentes" = {
             clientes_recurrentes <- table(data$Cliente..Correo.electrónico)
             clientes_recurrentes_df <- data.frame(
               "Cliente" = names(clientes_recurrentes),
               "Cantidad" = as.numeric(clientes_recurrentes)
             )
             top_10_clientes <- head(arrange(filter(clientes_recurrentes_df, !grepl("#", Cliente)), desc(Cantidad)), 10)
             top_10_clientes <- top_10_clientes[order(top_10_clientes$Cliente), ]
             
             ggplot(top_10_clientes, aes(x = Cliente, y = Cantidad)) +
               geom_point(color = "red") +
               geom_path(color = "red", aes(group = 1)) +
               labs(x = "Cliente", y = "Cantidad de Transacciones", title = "Top 10 Clientes Recurrentes")
           }
    )
  })
}



shinyApp(ui = ui, server = server)
