#### ENOTRNO DE R ####

require(cowplot)
require(ggplot2)
require(gridExtra)
require(kableExtra)
require(knitr)
require(leaflet)
require(raster)
require(RColorBrewer)
require(reshape2)
require(rgdal)
require(rgeos)
require(shiny)
require(sp)
require(stringr)

#### LECTURA DE LOS MODELOS ####

data.models <- "./data/table.rds"
data.dic <- "./data/dictionary.rds"
#data.spatial <- "./data/spatial.rds"

#### UI ####

ui <- fluidPage(
    #titulo
    titlePanel("Visualizador de modelos predictivos sobre el rendimiento educativo en el Ecuador"),
    h5("Este aplicación resume los resultados de un proyecto de investigación enfocado en el análisis de los impulsores territoriales del rendimiento académico para explicar las brechas educativas en el contexto urbano-rural del país. Para mayor información, por favor revise la publicación en este vínculo:"),
    a(href="https://www.mdpi.com/2220-9964/10/12/830", "Santos-García, F., Valdivieso, K. D., Rienow, A., & Gairín, J. (2021). Urban-Rural Gradients Predict Educational Gaps: Evidence from a Machine Learning Approach Involving Academic Performance and Impervious Surfaces in Ecuador. ISPRS International Journal of Geo-Information, 10(12)."),
    #filtro para datos
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="group", label=h5("Seleccione el grupo temático:"), 
                        choices = list("Entorno académico (AE)"=1,"Aspectos socio-ecónomicos y culturales (SC)"=2,"Habilidades cognitivas (CS)"=3), 
                        selected = 1),
            
            selectInput(inputId="spatial", label=h5("Seleccione un nivel:"),
                        choices = list("Regiones naturales"=1,"Provincias"=2,"Cantones"=3)),
            
            selectInput(inputId="unit", label=h5("Elija una unidad espacial:"),
                        choices = NULL),
            
            img(src = "LOGO-indoamerica.png", height = 72*1.6, width = 72*2.5),
            h5("Autor: Fabián Santos, PhD."),
        width=3),
        #paneles de datos
        mainPanel(
            tabsetPanel(
                tabPanel(h5("Resumen estadístico"),
                         htmlOutput("est")
                ),
                tabPanel(h5("Descripción de las muestras"),
                         plotOutput("des")
                ),
                tabPanel(h5("Sobre las temáticas"),
                         plotOutput("them")
                ),
                tabPanel(h5("Sobre la preguntas - Alto puntaje (>7)"),
                         plotOutput("pregAlto")
                ),
                tabPanel(h5("Sobre la preguntas - Bajo puntaje (<7)"),
                         plotOutput("pregBajo")
                ),
                tabPanel(h5("Precisión de los modelos"),
                         plotOutput("prec")
                )
            )
        )
    )
)

#### SERVER ####

server <- function(input, output, session){
    #seleccion grupo
    group.data <- reactive({
        group.data <- readRDS(data.models)
        group.data <- group.data[[as.numeric(input$group)]]
        group.data
    })
    #actualizar opciones
    observeEvent(input$spatial,{
        if(input$spatial==1){
            choices <- unique(group.data()[,"REGION"])
            choices <- choices[order(choices)]
            updateSelectInput(session,inputId = "unit", choices = choices)
        }else if(input$spatial==2){
            choices <- unique(group.data()[,"PROV"])
            choices <- choices[order(choices)]
            updateSelectInput(session,inputId = "unit", choices = choices)
        }else{
          choices <- unique(group.data()[,"CANT"])
          choices <- choices[order(choices)]
          updateSelectInput(session,inputId = "unit", choices = choices)
        }
    })
    #filtrar datos
    filtered.data <- reactive({
        input.data <- group.data()
        if(input$spatial==1){
            output.data <- list()
            output.data$base <- input.data[input.data$REGION==input$unit,]
        }else if(input$spatial==2){
            output.data <- list()
            output.data$base <- input.data[input.data$PROV==input$unit,]
        }else{
          output.data <- list()
          output.data$base <- input.data[input.data$CANT==input$unit,]
        }
        output.data
    })
    #resumen estadistico
    output$est <- renderText({
        est.vars <- c("AP","APclass","acc","kappa","naPerc")
        est.data <- filtered.data()$base[,est.vars]
        est.data <- base::split(est.data,est.data$APclass)
        est.data <- est.data[sapply(est.data,nrow)!=0]
        est.data <- lapply(est.data,function(x){
          x.df <- as.data.frame(t(as.matrix(
            round(apply(x[,c("AP","acc","kappa","naPerc")],2,mean,na.rm = T),2)
          )))
          x.sd <- round(apply(x[,c("AP","acc","kappa","naPerc")],2,sd,na.rm = T),2)
          x.df[1,] <- paste0(x.df,"±",x.sd)
          x.df$class <- unique(as.character(x$APclass))
          x.df$n <- nrow(x)
          return(x.df)
        })
        est.data <- do.call("rbind.data.frame",est.data)
        rownames(est.data) <- NULL
        est.data <- est.data[,c(6,1,2,3,4,5)]
        names(est.data) <- c("Centros educativos","Puntaje","Precisión global","Índice Kappa","Valores ausentes (%)","Clase")
        #tabla
        if(nrow(est.data)==2){
          kableExtra::kable(est.data[,1:5],"html") %>%
            kable_styling() %>%
            pack_rows("Alto (>7)",1,1,label_row_css = "background-color: #ff8080; color: black;") %>%
            pack_rows("Bajo (<7)",2,2,label_row_css = "background-color: #00bfc2; color: black;") %>%
            add_header_above(c("Promedios ± SD" = 5))
        }else{
          if(est.data$Clase=="Alto (>7)"){
            kableExtra::kable(est.data[,1:5],"html") %>%
              kable_styling() %>%
              pack_rows("Alto (>7)",1,1,label_row_css = "background-color: #ff8080; color: black;") %>%
              add_header_above(c("Promedios ± SD" = 5))
          }else{
            kableExtra::kable(est.data[,1:5],"html") %>%
              kable_styling() %>%
              pack_rows("Bajo (<7)",1,1,label_row_css = "background-color: #00bfc2; color: black;") %>%
              add_header_above(c("Promedios ± SD" = 5))
          }
        }
    })
    #descripcion
    output$des <- renderPlot({
        des.vars <- c("class","qui","fin","int","sex","etn","han","IMP")
        vars.names <- c("Tamaño del grupo","Estrato socio-económico","Administración","Celulares con internet","Género dominante","Etnia dominante","Personas con discapacidad","Gradiente urbano-rural")
        des.data <- filtered.data()$base[,c("APclass",des.vars)]
        des.data <- as.data.frame(lapply(des.data,droplevels))
        des.plot <- list()
        for(i in 1:length(des.vars)){
            des.plot[[i]] <- as.data.frame.matrix(table(des.data$APclass,des.data[,des.vars[i]]))
            des.plot[[i]]$Clase <- rownames(des.plot[[i]])
            rownames(des.plot[[i]]) <- NULL
            des.plot[[i]] <- reshape2::melt(des.plot[[i]],id.vars="Clase")
            des.plot[[i]]$concept <- vars.names[i]
        }
        des.plot[[9]] <- as.data.frame(table(des.data$APclass))
        des.plot[[9]]$variable <- des.plot[[9]]$Var1
        des.plot[[9]]$concept <- "Puntaje"
        names(des.plot[[9]])[c(1,2)] <- c("Clase","value")
        des.plot[[9]] <- des.plot[[9]][,c("Clase","variable","value","concept")]
        des.plot <- do.call("rbind.data.frame",des.plot)
        names(des.plot)[1] <- "Puntaje"
        text.size <- 10
        p <- ggplot(des.plot, aes(x=reorder(variable,-value),y=value,fill=Puntaje)) +
            theme_bw() +
            theme(plot.title = element_text(size = text.size),
                  legend.position="bottom",
                  axis.title=element_text(size=text.size),
                  axis.text.x=element_text(hjust = 1,size=text.size,angle=45),
                  axis.text.y=element_text(size=text.size),
                  legend.title=element_text(size=text.size),
                  legend.text=element_text(size=text.size),
                  strip.text.y = element_text(size=text.size),
                  strip.text.x = element_text(size=text.size),
                  panel.grid.minor = element_blank(),
                  legend.key = element_rect(colour = "black")) +
            geom_bar(stat="identity",position="stack",color="black") +
            scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
            coord_flip() +
            facet_wrap(concept~.,ncol=3,scales = "free") +
            xlab("Categorías") +
            ylab("Instituciones educativas (número)")
        p
    })
    #temáticas frecuentes
    output$them <- renderPlot({
        them.data <- filtered.data()$base[,c("APclass","THEM")]
        them.names <- unique(them.data$THEM)
        them.data <- base::split(them.data,them.data$APclass)
        them.data <- lapply(them.data,function(x){
            x$APclass <- as.character(x$APclass)
            x <- as.data.frame.matrix(table(x$APclass,x$THEM))
            x.names <- names(x)
            x.class <- rownames(x)
            x <- apply(x,2,function(y){
                y <- round((y*100)/sum(x),2)
            })
            x <- as.data.frame(x)
            x$THEM <- rownames(x)
            rownames(x) <- NULL
            names(x)[1] <- "value"
            x$APclass <- x.class
            if(!all(them.names %in% x$THEM)){
                them.miss <- them.names[!them.names %in% x$THEM]
                them.miss <- data.frame(value=0,THEM=them.miss,APclass=x.class)
                x <- rbind(x,them.miss)
            }
            return(x)
        })
        them.data <- do.call("rbind.data.frame",them.data)
        rownames(them.data) <- NULL
        them.data$value <- as.numeric(them.data$value)
        them.xlines <- length(unique(them.data$THEM))
        them.ylines <- range(them.data$value)
        them.ylines <- c(floor(them.ylines[1]),ceiling(them.ylines[2]))
        y.step <- floor(diff(them.ylines)/3)
        them.ylines <- c(y.step,y.step*2,them.ylines[2])
        them.data$concept <- "Temáticas más frecuentes en los modelos"
        names(them.data)[3] <- "Puntaje"
        
        dic.names <- readRDS(data.dic)
        them.data$THEMNAMES <- NA
        them.data$THEMCONCEPT <- NA
        for(i in 1:nrow(them.data)){
            them.data[i,"THEMNAMES"] <- dic.names[dic.names$prefijo %in% them.data[i,"THEM"],2]
            them.data[i,"THEMCONCEPT"] <- dic.names[dic.names$prefijo %in% them.data[i,"THEM"],4]
        }
        text.size <- 10
        p <- ggplot(them.data[order(them.data$THEM),],aes(x=THEM,y=value,color=Puntaje,fill=Puntaje,group=Puntaje)) +
            theme_bw() +
            theme(plot.title = element_text(size = text.size),
                  legend.position="bottom",
                  axis.title=element_text(size=text.size),
                  axis.text.x=element_text(hjust = 1,size=text.size,angle=0),
                  axis.text.y=element_blank(),
                  legend.title=element_text(size=text.size),
                  legend.text=element_text(size=text.size),
                  strip.text.y = element_text(size=text.size),
                  strip.text.x = element_text(size=text.size),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.key = element_rect(colour = "black")) +
            geom_polygon(size = 1.5,alpha=0.1) +
            geom_vline(xintercept = 1:them.xlines,linetype="dashed",size=1,alpha=0.5) +
            geom_hline(yintercept = them.ylines,linetype="dashed",size=1,alpha=0.5) +
            annotate(geom="text", x=0.5, y=them.ylines+2, size=5, label=paste0(them.ylines,"%"),color="gray30") +
            coord_polar() +
            xlab("Temáticas") +
            ylab("Instituciones educativas (%)") +
            facet_wrap(.~concept)
        mytheme <- gridExtra::ttheme_default(core = list(padding = unit(c(2.5, 2.5), "mm")))
        them.data <- unique(them.data[,c("THEM","THEMNAMES")])
        names(them.data) <- c("Prefijo","Temática")
        q <- gridExtra::tableGrob(them.data, theme = mytheme, rows = NULL)
        plot_grid(p,q,ncol = 2)
    })
    #preguntas frecuentes (alto puntaje)
    output$pregAlto <- renderPlot({
      preg.data <- filtered.data()$base[,c("APclass","QUES")]
      preg.data <- base::split(preg.data,preg.data$APclass)
      preg.data <- lapply(preg.data,function(x){
        x.class <- unique(x$APclass)
        x <- as.data.frame(table(x$QUES))
        x <- x[order(x$Freq,decreasing=T),]
        x$APclass <- x.class
        x$perc <- round((x$Freq*100) / sum(x$Freq),2)
        x$Var1 <- as.character(x$Var1)
        return(x)
      })
      preg.data <- preg.data[[1]][1:10,] #ALTO
      preg.data$APclass <- droplevels(preg.data$APclass)
      names(preg.data)[3] <- "Puntaje"
      preg.data$concept <- "Preguntas más frecuentes en los modelos"
      text.size <- 10
      num.breaks <- c(0,unique(preg.data$Freq))
      p <- ggplot(preg.data,aes(x=Freq,y=reorder(Var1,Freq),fill=Puntaje,group=Puntaje)) +
        theme_bw() +
        theme(plot.title = element_text(size = text.size),
              legend.position="bottom",
              axis.title=element_text(size=text.size),
              axis.text.y=element_text(size=text.size),
              axis.text.x=element_text(hjust = 1,size=text.size,angle=45),
              panel.grid.minor = element_blank(), 
              legend.title=element_text(size=text.size),
              legend.text=element_text(size=text.size),
              strip.text.y = element_text(size=text.size),
              legend.key = element_rect(colour = "black")) +
        geom_bar(stat="identity",position="dodge2",color="black") +
        scale_fill_manual(values="#F8766D") + #c("#00BFC4","#F8766D")
        scale_y_discrete(labels = function(x) str_wrap(x, width = 90)) +
        scale_x_continuous(breaks = num.breaks, labels = scales::label_number(accuracy = 1)) +
        xlab("Instituciones educativas (conteo)") +
        ylab("Preguntas") +
        facet_wrap(.~concept)
      p
    })
    #preguntas frecuentes (bajo puntaje)
    output$pregBajo <- renderPlot({
      preg.data <- filtered.data()$base[,c("APclass","QUES")]
      preg.data <- base::split(preg.data,preg.data$APclass)
      preg.data <- lapply(preg.data,function(x){
        x.class <- unique(x$APclass)
        x <- as.data.frame(table(x$QUES))
        x <- x[order(x$Freq,decreasing=T),]
        x$APclass <- x.class
        x$perc <- round((x$Freq*100) / sum(x$Freq),2)
        x$Var1 <- as.character(x$Var1)
        return(x)
      })
      preg.data <- preg.data[[2]][1:10,] #BAJO
      preg.data$APclass <- droplevels(preg.data$APclass)
      names(preg.data)[3] <- "Puntaje"
      preg.data$concept <- "Preguntas más frecuentes en los modelos"
      text.size <- 10
      num.breaks <- c(0,unique(preg.data$Freq))
      p <- ggplot(preg.data,aes(x=Freq,y=reorder(Var1,Freq),fill=Puntaje,group=Puntaje)) +
        theme_bw() +
        theme(plot.title = element_text(size = text.size),
              legend.position="bottom",
              axis.title=element_text(size=text.size),
              axis.text.y=element_text(size=text.size),
              axis.text.x=element_text(hjust = 1,size=text.size,angle=45),
              panel.grid.minor = element_blank(), 
              legend.title=element_text(size=text.size),
              legend.text=element_text(size=text.size),
              strip.text.y = element_text(size=text.size),
              legend.key = element_rect(colour = "black")) +
        geom_bar(stat="identity",position="dodge2",color="black") +
        scale_fill_manual(values="#00BFC4") + #c("#00BFC4","#F8766D")
        scale_y_discrete(labels = function(x) str_wrap(x, width = 90)) +
        scale_x_continuous(breaks = num.breaks, labels = scales::label_number(accuracy = 1)) +
        xlab("Instituciones educativas (conteo)") +
        ylab("Preguntas") +
        facet_wrap(.~concept)
      p
    })
    #precisión modelos
    output$prec <- renderPlot({
      prec.vars <- c("acc","kappa","brier","naPerc","linkObs","linkDist")
      vars.names <- c("Puntaje","Precisión general (0 - 1)","Coeficiente Kappa (0 - 1)","Puntaje de Brier (-∞ - 1)","Valores faltantes (%)","Muestras vinculadas (Conteo)","Distancia entre muestras vinculadas (Metros)")
      prec.data <- filtered.data()$base[,c("APclass",prec.vars)]
      names(prec.data) <- vars.names
      prec.data <- reshape2::melt(prec.data,id.vars="Puntaje")
      text.size <- 10
      q <- ggplot(prec.data, aes(y=value,fill=Puntaje)) +
        theme_bw() +
        theme(plot.title = element_text(size = text.size),
              legend.position="bottom",
              axis.title=element_text(size=text.size),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_text(size=text.size),
              panel.grid.major=element_blank(),
              panel.grid.minor = element_blank(), 
              legend.title=element_text(size=text.size),
              legend.text=element_text(size=text.size),
              strip.text.x = element_text(size=text.size),
              strip.text.y = element_text(size=text.size),
              legend.key = element_rect(colour = "black")) +
        geom_boxplot(coef=1) +
        facet_wrap(variable~.,ncol=3,scales = "free") +
        xlab("Categorías") +
        ylab("Valores")
      q
    })
    #fin funcion server
}

# Run the application 
shinyApp(ui = ui, server = server)
