library(shiny)
library(tidyverse) 
library(lubridate)
library(ggpmisc)
library(data.table)

datos <- read_csv2("datos.csv")
datos <- as.data.table(datos)
datos <- datos[, lapply(.SD, iconv, "latin1", "UTF-8")]

datos <- datos %>%
    
    rename(fecha_incautacion = TIEM_DIA_SK_INCAUTACION ,
           aduana = ADUA_ADUA_DESCC , 
           infraccion = INFR_INFR_DESCC ,
           pais_procedencia = PAIS_PAIS_DESCC_PAIS_PROCEDENC ,
           unidad = UCOM_UCOM_DESCC ,
           estado_incautacion = ESTINC_ESTINC_DESCC ,
           tipo = TINC_TINC_DESCC ,
           cantidad = INC_CANTIDAD ,
           valor = INC_VALOR_MN) %>%
    
    
    select(fecha_incautacion,
           aduana,
           infraccion,
           pais_procedencia,
           unidad,
           estado_incautacion,
           tipo,
           cantidad,
           valor)
    
    
    

ui <- fluidPage(
    
    navlistPanel(id = "tabset",
                 
                 "Incautaciones",
                 
        tabPanel('Introduccion' , 'Este proyecto nace como un instrumento para poder aplicar los contenidos 
                 aprendidos durante el semestre en un problema real. Se tendrá que demostrar los conocimentos adquiridos 
                 al realizar un analisis exploratorio sobre datos de interes nacional, poniendo especial énfasis en la 
                 correcta visualización de los resultados obtenidos, realizando una correcta interpretación de la misma. 
                 En el caso de este grupo en cuestión, se analizarán datos de incautaciones de aduana presentados por 
                 la Dirección Nacional de Aduanas, relacionando tipos y el valor las de inacutaciones, cantidades, procedencia, 
                 estos fueron elegidos  para llevar a cabo estos objetivos, se utilizará 
                 exclusivamente el lenguaje de programación R, y sus respectivas expansiones, como Markdown, Shiny y RStudio.'),
        
        tabPanel("Tipo de incautacion",
                 selectInput("artefacto","Tipo de incautación",c('ANTEOJOS', 'APARATOS PARA TELEFONIA Y ACCESORIOS ' ,
                                                                 'ARMAS Y SUS ACCESORIOS' , 'ARTICULOS DE PIROTECNIA' , 
                                                                 'ARTICULOS DE ELECTRICIDAD' , 'ARTICULOS DE MOBILIARIO' ,
                                                                 'ARTICULOS DE OFICINA Y MANUFACTURAS DE PAPEL Y CARTON' ,
                                                                 'ARTICULOS DE OPTICA' , 'ARTICULOS PARA EL HOGAR' , 
                                                                 'BALANZAS ELECTRONICAS' , 'BEBIDAS' , 'BIJOU' ,
                                                                 'BILLETERAS Y MONEDEROS' , 'CALZADOS' , 'CAMPING' , 'CARBON' ,
                                                                 'CD' , 'CIGARROS' , 'COMBUSTIBLES' , 'COMESTIBLES' , 
                                                                 'COMUNICACIONES' , 'CONSTRUCCION' , 
                                                                 'COSMETICOS Y PRODUCTOS DE LIMIPIEZA' , 
                                                                 'COTILLON, ACCESORIOS PARA FIESTAS Y CUMPLEANIOS',
                                                                 'CUERO Y SUS DERIVADOS' , 'DINERO' , 'DROGAS' ,
                                                                 'ELECTRODOMESTICOS Y APARATOS OPTICOS' ,
                                                                 'ENVASES DE PLASTICO, LATA, VIDRIO, DIVERSOS MATERIALES' ,
                                                                 'ESPEJOS' , 'HERRAMIENTAS' , 'INFORMATICO' ,
                                                                 'INSECTICIDAS' , 'INSTRUMENTOS MUSICALES' , 
                                                                 'JUGUETES EROTICOS' , 'JUGUETES VARIOS' , 'LINTERNAS' ,
                                                                 'LUBRICANTES' , 'MAQUINARIA' , 'MATERIAL ODONTOLOGICO' , 
                                                                 'MEDICAMENTOS' , 'PARLANTES' , 'PEGAMENTOS, SELLADORES, SILICONAS' ,
                                                                 'PRECURSORES' , 'PRODUCTOS AGRICOLAS' , 'PRODUCTOS MINERALES' ,
                                                                 'PRODUCTOS Y ACCESORIOS PARA LABORATORIO' , 
                                                                 'PROTEINAS Y SUPLEMENTOS DEPORTIVOS' , 'QUEMADORES A GAS' , 
                                                                 'RELOJES' , 'REPUESTOS' , 'TABACO' , 'VALORES' , 'VEHICULOS' ,
                                                                 'VESTIMENTA' , 'VIDEOJUEGOS, APARATOS Y ACCESORIOS')),

                 plotOutput("linea"),
                 plotOutput("caja")),
        
        tabPanel("Tipo de infracción",
                 selectInput("crimen","Tipo de incautación",c("Abandono","Contrabando","Falsificacion","Receptacion")),
                 plotOutput("hist2")),
        
        tabPanel("Valor de incautación",
                 numericInput("min","Valor mínimo",value=500),
                 numericInput("max","Valor máximo",value=5000),
                 "Los valores oscilan entre 0 y 148000000",
                 plotOutput("hist")),
        
        tabPanel("Relación cantidad y valor",
                 selectInput("color","Variable en color",c("aduana","infraccion","estado_incautacion")),
                 plotOutput("puntos"))
    )
)

server <- function(input, output){
    
    output$hist <- renderPlot({
        
        datos %>% 
            filter(valor>input$min) %>% 
            filter(valor<input$max) %>%
            select(aduana)%>%
            mutate(aduana = recode(aduana, `SEDE REGIONAL SUROESTE(BAJA)` = "SEDE SUDOESTE",
                                   `SEDE REGIONAL LITORAL NORESTE(BAJA)`= "SEDE NORESTE",`SEDE REGIONAL OESTE(ANTES NOROESTE)` = "SEDE OESTE")) %>%
            group_by(aduana) %>% 
            summarise(conteo=n()) %>% 
            ggplot(aes(x=fct_reorder(aduana,conteo),y=conteo))+geom_col()+labs(x="Aduana",y="Cantidad")+
            theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8))
    })
    
    output$hist2 <- renderPlot({
        
        datos %>% 
            separate(fecha_incautacion , into=c('anio','mes'), sep=4) %>%
            group_by(infraccion,anio) %>% 
            summarise(conteo=n()) %>% 
            subset(subset=grepl(input$crimen,infraccion)) %>% 
            ggplot(aes(x=anio,y=conteo))+geom_col(fill="blue")+labs(x="Año",y="Cantidad")
    })
    
    output$linea<-renderPlot({
        
        datos %>% 
            mutate(fecha_incautacion=ym(fecha_incautacion)) %>%
            group_by(tipo,fecha_incautacion) %>% 
            summarise(conteo=n()) %>% 
            subset(subset=grepl(input$artefacto,tipo)) %>% 
            ggplot(aes(x=fecha_incautacion ,y=conteo))+geom_line(color="purple")+labs(x="Fecha de incautación",y="Cantidad")+stat_peaks(color="purple")+stat_peaks(geom="text",color="purple",vjust=-0.3)
            
            })
    
    output$caja<-renderPlot({
        
        datos %>% 
            mutate(pais_procedencia = recode(pais_procedencia, `CHINA, REPUBLICA POPULAR DE` = "CHINA",
                                   `UNITED STATES MINOR OUTLYING ISLANDS`= "USA",`RUSIA(FEDERACION RUSA)`="RUSIA")) %>%
            group_by(tipo,fecha_incautacion,pais_procedencia) %>% 
            summarise(conteo=n()) %>% 
            subset(subset=grepl(input$artefacto,tipo)) %>% 
            ggplot()+geom_boxplot(aes(x=reorder(pais_procedencia,conteo,median),y=conteo))+labs(x="País de procedencia",y="Cantidad")+theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8))
    })
    
    output$puntos<-renderPlot({
        atipicovalor<-quantile(as.numeric(datos$valor),probs=0.75)+1.5*(quantile(as.numeric(datos$valor),probs=0.75)-quantile(as.numeric(datos$valor),probs=0.25))
        
        atipicocantidad<-quantile(as.numeric(datos$cantidad),probs=0.75)+1.5*(quantile(as.numeric(datos$cantidad),probs=0.75)-quantile(as.numeric(datos$cantidad),probs=0.25))
        
        datos %>% mutate(INC_CANTIDAD=round(as.numeric(cantidad),0),INC_VALOR_MN=round(as.numeric(valor),0))%>% filter(INC_VALOR_MN<atipicovalor)%>% filter(INC_CANTIDAD<atipicocantidad)%>% ggplot(aes(x=INC_VALOR_MN,y=INC_CANTIDAD,colour=.data[[input$color]]))+geom_point()+theme(aspect.ratio = 1)
    })
}

shinyApp(ui = ui, server = server)
