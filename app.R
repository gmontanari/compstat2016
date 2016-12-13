## app.R ##
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(magrittr)

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Estad. Comp.2016 - Guillermina Montanari", titleWidth = 450),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Aprendiendo Shiny", tabName = "tarea1", icon = icon("trophy")),
                        menuItem("Función Inversa - Exponencial", tabName = "tarea2a", icon = icon("trophy")),
                        menuItem("Función Inversa - Weibull", tabName = "tarea2b", icon = icon("trophy")),
                        menuItem("Integración numérica - MCMC", tabName = "tarea3", icon = icon("trophy")),
                        menuItem("Tarea 4 - In Progress...", tabName = "tarea4", icon = icon("gear")),
                        menuItem("Tarea 5 - In Progress...", tabName = "tarea5", icon = icon("gear")),
                        menuItem("Tarea 6 - In Progress...", tabName = "tarea6", icon = icon("gear"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # Contenido de los tabs
                        tabItem(tabName = "tarea1",
                                fluidRow(
                                  box(
                                    title = "Datos",
                                    sliderInput("slider11", "Cantidad números a generar:", min= 1, max = 2000, value= 500),
                                    sliderInput("slider12", "Rango X:", min= -100, max = 100, value= c(-50,50)),
                                    sliderInput("slider13", "Rango Y:", min= -100, max = 100, value= c(-50,50)),
                                    height = 400
                                  ),
                                  box(plotOutput("plot1_2", height = 250)),
                                  submitButton("Ejecutar")
                                )
                        ),
                        tabItem(tabName = "tarea2a",
                                fluidRow(
                                  box(
                                    title = "Datos",
                                    sliderInput("slider2", "Cantidad simulaciones generar:", min=1, max=1000, value=200),
                                    numericInput("nLambda",label = "Densidad de la Exponencial (Lambda)", min = 0.001, max = 10, value = 0.1, step = 0.3),
                                    numericInput("nbins",label = "Bins del Histograma", min = 10, max = 80, value = 20, step = 1),
                                    height = 350
                                  ),
                                  box(
                                    selectInput("dataset", "Elegir Archivo:", choices = c("Datos Simulados")),
                                    radioButtons("filetype", "File type:",choices = c("csv")),
                                    downloadButton('downloadData', 'Download'),
                                    height = 350
                                  ),
                                  box(plotOutput("plot2_1", height = 250), title = "Histograma simulado uniformes vs Dist Proba Exponencial R"),
                                  box(plotOutput("plot2_2", height = 250), title = "Dist Exponencial Acumulada Simulada"),
                                  submitButton("Ejecutar")
                                )
                        ),
                        tabItem(tabName = "tarea2b",
                                fluidRow(
                                  box(
                                    title = "Datos",
                                    sliderInput("slider2b", "Cantidad simulaciones generar:", min=1, max=1000, value=200),
                                    numericInput("lambda",label = "Lambda (escala), mayor a cero", min = 1, max = 1, value = 1),
                                    numericInput("k",label = "K (shape), mayor a cero", min = 0.5, max = 5, value = 0.5, step = 0.5),
                                    numericInput("nbinsb",label = "Bins del Histograma", min = 10, max = 80, value = 20, step = 1),
                                    height = 400
                                  ),
                                  box(
                                    selectInput("datasetb", "Elegir Archivo:", choices = c("Datos Simulados")),
                                    radioButtons("filetypeb", "File type:",choices = c("csv")),
                                    downloadButton('downloadDatab', 'Download'),
                                    height = 400
                                  ),
                                  box(plotOutput("plot2_1b", height = 250), title = "Histograma simulado uniformes vs Dist Proba Weibull R"),
                                  box(plotOutput("plot2_2b", height = 250), title = "Dist Wiebull Acumulada Simulada"),
                                  submitButton("Ejecutar")
                                )
                        ),
                        tabItem(tabName = "tarea3",
                                fluidRow(
                                  box(
                                    title = "Datos Variables 3",
                                    textInput("textInput1", value = "function (x) sin(x)", width=NULL, label = "Ingresa una función"),
                                    numericInput("minimo",label = "Valor Minimo", min = NA, max = NA, value = 10),
                                    numericInput("maximo",label = "Valor Máximo", min = NA, max = NA, value = 20),
                                    numericInput("nsim",label = "Número simulaciones", min = NA, max = NA, value = 10),
                                    numericInput("npuntos",label = "Puntos en el intervalo", min = NA, max = NA, value = 20),
                                    numericInput("alpha",label = "Confianza", min = NA, max = NA, value = 0.05),
                                    height = 500
                                  ),
                                  box(plotOutput("plot3_1", height = 200)),
                                  box(plotOutput("plot3_2", height = 200)),
                                  submitButton("Ejecutar")
                                )
                        ),
                        tabItem(tabName = "tarea4"),
                        tabItem(tabName = "tarea5"),
                        tabItem(tabName = "tarea6")
                      )
                    )
)

# Inicio - Tarea1
# Funcion GCl para Generar números pesudoaleatorios
GCL <-function(nsim, semilla=101421, incremento=1, multiplicador=2456651, M=2^32){
  seq <- semilla
  for(i in 1:nsim){
    new <- (seq[i]*multiplicador +  incremento)%%M
    seq <- c(seq, new)
  }
  return(seq/M)
}
# Funcion GCl para Generar números pesudoaleatorios
# Fin - Tarea1
## Función Inversa - Tarea 2
FinvExp <- function(u, nLambda){return(-log(1-u)/nLambda)}
FinvWei <- function(u, shape, scala){return (scala*(-log(1-u))^(1/shape))}
## Función Inversa - Tarea 2

server <- function(input, output) {
  
  output$plot1_2 <- renderPlot({
    minX <- input$slider12[1]
    maxX <- input$slider12[2]
    minY <- input$slider13[1]
    maxY <- input$slider13[2]
    dataX <- runif(input$slider11, minX, maxX)
    dataY <- runif(input$slider11, minY, maxY) 
    xlab <- "Eje X"
    ylab <- "Eje Y"
    plot(dataX, dataY, xlab= xlab, ylab=ylab, main = "Aprendiendo Shiny", xlim = c(-100,100), ylim = c(-100,100), col = "dark green")
  })
  output$plot2_1 <- renderPlot({
    U <- runif(input$slider2)
    X <- FinvExp(U, input$nLambda)
    g <- X[seq_len(input$slider2)]
    h<-hist(g, breaks=input$nbins, density=10, col="lightgray", 
            xlab="Valores simulados de X", ylab="Frecuencias de X", main= "") 
    xfit<-seq(min(g),max(g),length=40) 
    yfit<-dexp(xfit, rate= input$nLambda) 
    yfit <- yfit*diff(h$mids[1:2])*length(g) 
    lines(xfit, yfit, col="blue", lwd=2)
    datasetInput <- reactive({switch(input$dataset,"Datos Simulados" = g)})
    output$table <- renderTable({ datasetInput()})
    output$downloadData <- downloadHandler( filename = function() { paste(input$dataset, input$filetype, sep = ".")},
    content = function(file) { 
        sep <- switch(input$filetype, "csv" = ",")
        write.table(datasetInput(), file, sep = sep, row.names = FALSE)}
    )})
  
  output$plot2_2 <- renderPlot({
    U <- runif(input$slider2)
    X <- FinvExp(U,input$nLambda)
    g <- X[seq_len(input$slider2)]
    exp_vector <- pexp(X, rate = input$nLambda)
    plot(X, exp_vector, pch=16, col= "red", type = "p", xlab="Valores simulados de X", ylab="Frecuencias acumuladas de X", main= "")
  })
  output$plot2_1b <- renderPlot({
    U <- runif(input$slider2b)
    X <- FinvWei(U,input$k,input$lambda)
    g <- X[seq_len(input$slider2b)]
    h<-hist(g, breaks=input$nbinsb, density=10, col="lightgray", xlab="Valores simulados de X", ylab="Frecuencias de X", main= "") 
    xfit<-seq(min(g),max(g),length=40) 
    yfit<-dweibull(xfit, shape=input$k, scale=input$lambda) 
    yfit <- yfit*diff(h$mids[1:2])*length(g) 
    lines(xfit, yfit, col="blue", lwd=2)
    datasetInput <- reactive({switch(input$datasetb,"Datos Simulados" = g)})
    output$table <- renderTable({ datasetInput()})
    output$downloadDatab <- downloadHandler( filename = function() { paste(input$datasetb, input$filetypeb, sep = ".")},
    content = function(file) { 
        sep <- switch(input$filetypeb, "csv" = ",")
        write.table(datasetInput(), file, sep = sep, row.names = FALSE)}
  )})
  
  output$plot2_2b <- renderPlot({
    U <- runif(input$slider2b)
    X <- FinvWei(U,input$lambda,input$k)
    g <- X[seq_len(input$slider2b)]
    exp_vector <- pweibull(X, shape=input$lambda, scale=input$k,lower.tail = TRUE, log.p = FALSE)
    plot(X, exp_vector, pch=16, col= "red", type = "p", xlab="Valores simulados de X", ylab="Frecuencias acumuladas de X", main= "")
  })
  output$plot3_1 <- renderPlot({
    X <- seq(input$minimo, input$maximo, length.out = 500)
    FUN1 <- reactive({
      texto <- paste("aux <-", input$textInput1)
      eval(parse(text = texto))
      aux
    })
    Y_x <- sapply(X, FUN1())
    plot(X, Y_x, pch=16)
    lines(X,Y_x,col="red")
  })
  output$plot3_2 <- renderPlot({
    #U <- runif(input$npuntos,input$minimo, input$maximo)
    FUN1 <- reactive({
      texto <- paste("aux <-", input$textInput1)
      eval(parse(text = texto))
      aux
    })
    #Y_u <- sapply(U, FUN1())
    S2=0
    Int=0
    int.upper=0
    int.lower=0
    for(i in 1:input$nsim){
      U <- runif(input$npuntos,input$minimo, input$maximo)
      Y_u <- sapply(U, FUN1())
      Int[i] = (input$maximo-input$minimo)*mean(Y_u)
      S2[i] = var(Y_u)
      quant <- qnorm(input$alpha/2, lower.tail=FALSE) 
      int.upper[i] <- Int[i] + sqrt(S2[i]/length(S2))*quant # Upper confidence interval
      int.lower[i] <- Int[i] - sqrt(S2[i]/length(S2))*quant # Lower 
    }
    Valor <- integrate(FUN1(), input$minimo,input$maximo)
    Valor_vec <- (rep.int(Valor[1],input$nsim))
    xs <- c(1:input$nsim)
    plot(xs, Valor_vec, type="l",ylim=c(min(int.lower),max(int.upper)),col=1)
    lines(xs,Int,type = "l",col=2)
    lines(xs,int.upper,type = "l",col=3)
    lines(xs,int.lower,type = "l",col=3)
  })

  }

shinyApp(ui, server)

