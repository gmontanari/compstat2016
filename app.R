## app.R ##
## Tareas de Estadística Computacional - Guillermina Montanari
## Otoño - Invierno 2016
##

library(dplyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(magrittr)
library(ADGofTest)
library(DT)

ui <- dashboardPage(
                    skin = "purple",
                    dashboardHeader(title = "Estad. Comp.2016 - Guillermina Montanari", titleWidth = 450), #DashbardHeader
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Aprendiendo Shiny", tabName = "tarea1", icon = icon("trophy")),
                        menuItem("Función Inversa - Exponencial", tabName = "tarea2a", icon = icon("trophy")),
                        menuItem("Función Inversa - Weibull", tabName = "tarea2b", icon = icon("trophy")),
                        menuItem("Integración numérica - MCMC", tabName = "tarea3", icon = icon("trophy")),
                        menuItem("Tarea 4 - In Progress...", tabName = "tarea4", icon = icon("gear")),
                        menuItem("Tarea 5 - In Progress...", tabName = "tarea5", icon = icon("gear")),
                        menuItem("Tarea 6 - In Progress...", tabName = "tarea6", icon = icon("gear"))
                      )#SidebarMenu
                    ),#DashboardSidebar
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "tarea1",
                                fluidRow(
                                  box(
                                    title = "Datos", status = "primary",solidHeader = TRUE,collapsible = TRUE,
                                    sliderInput("slider11", "Cantidad números a generar:", min= 1, max = 2000, value= 500),
                                    sliderInput("nbins1", "Intervalos:", min= 1, max = 100, value= 10),
                                    submitButton("Ejecutar"),
                                    height = 300
                                  ),
                                  box(
                                    plotOutput("plot1_1", height = 250),title = "Histograma",status = "primary",solidHeader = TRUE,collapsible = TRUE
                                  ),
                                  box(
                                    plotOutput("plot1_2", height = 250),title = "Plot",status = "primary",solidHeader = TRUE,collapsible = TRUE
                                  )
                                ) #fluidrow
                        ), #tabitem1
                        tabItem(tabName = "tarea2a",
                                fluidRow(
                                  box(
                                    title = "Datos",status = "success",solidHeader = TRUE,collapsible = TRUE,
                                    sliderInput("slider2", "Cantidad simulaciones generar:", min=1, max=1000, value=200),
                                    numericInput("nLambda",label = "Densidad de la Exponencial (Lambda)", min = 0.001, max = 10, value = 0.1, step = 0.3),
                                    numericInput("nbins",label = "Bins del Histograma", min = 10, max = 80, value = 20, step = 1),
                                    height = 350,
                                    submitButton("Ejecutar")
                                  ),
                                  box(
                                    title = "Descarga",status = "success",solidHeader = TRUE,collapsible = TRUE,
                                    selectInput("dataset", "Elegir Archivo:", choices = c("Datos Simulados")),
                                    radioButtons("filetype", "File type:",choices = c("csv")),
                                    downloadButton('downloadData', 'Download'),
                                    height = 350
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title = "Histograma simulado uniformes vs Dist Proba Exponencial R",status = "success",solidHeader = TRUE,collapsible = TRUE,
                                    plotOutput("plot2_1", height = 200), 
                                    height = 300
                                  ),
                                  box(
                                    title = "Funcion Acumulada Simulada", status="success",solidHeader = TRUE,
                                    plotOutput("plot2_2", height = 200),
                                    height = 300
                                  )
                                ),
                                fluidRow(
                                  box(
                                      title = "Prueba Ajuste", status="success",solidHeader = TRUE,
                                      plotOutput("plot2_3", height = 200),
                                      height = 300
                                  ),
                                  valueBoxOutput("progressBox", width=5)
                                ) #fluidrow
                        ), #tabitem2a
                        
                        tabItem(tabName = "tarea2b",
                                fluidRow(
                                  box(
                                    title = "Datos", status="danger",solidHeader = TRUE,
                                    sliderInput("slider2b", "Cantidad simulaciones generar:", min=1, max=1000, value=200),
                                    numericInput("lambda",label = "Lambda (escala), mayor a cero", min = 1, max = 1, value = 1),
                                    numericInput("k",label = "K (shape), mayor a cero", min = 0.5, max = 5, value = 0.5, step = 0.5),
                                    numericInput("nbinsb",label = "Bins del Histograma", min = 10, max = 80, value = 20, step = 1),
                                    submitButton("Ejecutar"),
                                    height = 440
                                  ),
                                  box(
                                    title = "Download datos Simulación", status="danger",solidHeader = TRUE,
                                    selectInput("datasetb", "Archivo:", choices = c("Datos Simulados")),
                                    radioButtons("filetypeb", "File type:",choices = c("csv")),
                                    downloadButton('downloadDatab', 'Download'),
                                    height = 440
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title = "Hist UNI simulada vs Dist Proba Weibull R", status="danger",solidHeader = TRUE,
                                    plotOutput("plot2_1b", height = 250), height = 400),
                                  box(
                                    title = "Dist Wiebull Acumulada Simulada", status="danger",solidHeader = TRUE,
                                    plotOutput("plot2_2b", height = 250), height = 400)
  
                          ),#fluidrow
                          fluidRow(
                            box(
                              title = "Prueba Ajuste", status="danger",solidHeader = TRUE,
                              plotOutput("plot2_3b", height = 200),
                              height = 300
                            ),
                            valueBoxOutput("progressBoxb", width=5)
                          ) #fluidrow
                        ), #tabitem2b
                        tabItem(tabName = "tarea3",
                                fluidRow(
                                  box(
                                    title = "Datos Función", status="info",solidHeader = TRUE,
                                    textInput("textInput1", value = "function (x) sin(x)", width=NULL, label = "Ingresa una función"),
                                    numericInput("minimo",label = "Valor Minimo", min = NA, max = NA, value = 10),
                                    numericInput("maximo",label = "Valor Máximo", min = NA, max = NA, value = 20),
                                    #value = 0.05),
                                    height = 350
                                  ),
                                  box(
                                    title = "Datos Integración", status="info",solidHeader = TRUE,
                                    numericInput("nsim",label = "Número simulaciones", min = NA, max = NA, value = 10),
                                    numericInput("npuntos",label = "Puntos en el intervalo", min = NA, max = NA, value = 20),
                                    numericInput("alpha",label = "Confianza", min = 0.001, max = 1, value = 0.05, step = 0.01),
                                    submitButton("Ejecutar"),
                                    height = 350
                                  )
                                ),
                              fluidRow(
                                  box(
                                    title = "Función a Integrar", status="info",solidHeader = TRUE,
                                    plotOutput("plot3_1", height = 200)
                                    ),
                                  box(
                                    title = "Ajuste", status="info",solidHeader = TRUE,
                                    plotOutput("plot3_2", height = 200)
                                    )
                                )
                        ), #tabitem3
                        tabItem(tabName = "tarea4"),
                        tabItem(tabName = "tarea5"),
                        tabItem(tabName = "tarea6")
                      )
                    )
)# DashboardPage

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
# Fin - Tarea1

## Función Inversa - Tarea 2
FinvExp <- function(u, nLambda){return(-log(1-u)/nLambda)}
FinvWei <- function(u, shape, scala){return (scala*(-log(1-u))^(1/shape))}
## Función Inversa - Tarea 2

server <- function(input, output) {

  datos1<-reactive({
    input$slider11
  })  
  
  output$plot1_1 <- renderPlot({
    data <- GCL(datos1())
    ggplot()+aes(data)+ geom_histogram(bins=input$nbins1,col="blue",fill="grey",alpha=.2)
  })
  
  output$plot1_2 <- renderPlot({
    data <- GCL(datos1())
    x <- data[-length(data)]
    y <- data[-1]
    ggplot()+aes(x,y)+ geom_point(bins=input$nbins1,col="blue",fill="grey",alpha=.2)
  })
  
  output$plot2_1 <- renderPlot({
    U <- runif(input$slider2)
    X <- FinvExp(U, input$nLambda)
    g <- X[seq_len(input$slider2)]
    h<-hist(g, breaks=input$nbins, density=10, col="green", 
            xlab="Valores simulados de X", ylab="Frecuencias de X", main= "") 
    xfit<-seq(min(g),max(g),length=40) 
    yfit<-dexp(xfit, rate= input$nLambda) 
    yfit <- yfit*diff(h$mids[1:2])*length(g) 
    lines(xfit, yfit, col="green", lwd=1)
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
    plot(X, exp_vector, pch=10, col= "green", type = "p", xlab="Valores simulados de X", ylab="Frecuencias acumuladas de X", main= "")
  })
  
  output$plot2_3 <- renderPlot({
    U <- runif(input$slider2)
    X <- FinvExp(U, input$nLambda)
    y <- quantile(X,c(0.25,0.75))
    Xe<-qexp(c(0.25,0.75))
    slope<-diff(y)/diff(Xe)
    int <- y[1]-slope*Xe[1]
    ggplot()+aes(sample=X)+stat_qq(distribution=qexp)+geom_abline(intercept = int,slope=slope,col="green")+
      xlab("X")+ ylab("Y")
  })
  
  output$progressBox <- renderValueBox({
    U <- runif(input$slider2)
    X <- FinvExp(U, input$nLambda)
    ks<- as.numeric(ks.test(X, "pexp", rate =input$nLambda))
    ks<-round(ks,3)
    valueBox(
      paste0("p-value=",ks[2]),"Prueba de ajuste Kolmogorov-Smirnov",icon = icon("list"),
      color = "green"
    )
  })
  
  output$plot2_1b <- renderPlot({
    U <- runif(input$slider2b)
    X <- FinvWei(U,input$k,input$lambda)
    g <- X[seq_len(input$slider2b)]
    h<-hist(g, breaks=input$nbinsb, density=4, col="lightgray", xlab="Valores simulados de X", ylab="Frecuencias de X", main= "") 
    xfit<-seq(min(g),max(g),length=40) 
    yfit<-dweibull(xfit, shape=input$k, scale=input$lambda) 
    yfit <- yfit*diff(h$mids[1:2])*length(g) 
    lines(xfit, yfit, col="orange", lwd=2)
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
    plot(X, exp_vector, pch=16, col= "orange", type = "p", xlab="Valores simulados de X", ylab="Frecuencias acumuladas de X", main= "")
  })
  
  output$plot2_3b <- renderPlot({
    U <- runif(input$slider2)
    X <- FinvWei(U,input$lambda,input$k)
    y <- quantile(X,c(0.25,0.75))
    Xe<-qexp(c(0.25,0.75))
    slope<-diff(y)/diff(Xe)
    int <- y[1]-slope*Xe[1]
    ggplot()+aes(sample=X)+stat_qq(distribution=qexp)+geom_abline(intercept = int,slope=slope,col="orange")+
      xlab("X")+ ylab("Y")
  })
  
  output$progressBoxb <- renderValueBox({
    U <- runif(input$slider2b)
    X <- FinvWei(U,input$lambda,input$k)
    ks<- as.numeric(ks.test(X, "pweibull",shape=input$lambda, scale=input$k,lower.tail = TRUE, log.p = FALSE))
    ks<-round(ks,3)
    valueBox(
      paste0("p-value=",ks[2]),"Prueba de ajuste Kolmogorov-Smirnov",icon = icon("list"),
      color = "red"
    )
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

