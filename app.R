library(shiny)
library(tidyverse) 
library(lubridate)

datos<-read_delim("datos.csv",delim=";")

ui <- fluidPage(
    navlistPanel(id = "tabset",
        "Exploración",
        tabPanel("Tipo de incautacion",
                 selectInput("artefacto","Tipo de incautación",c("ARMAS Y SUS ACCESORIOS")),
                 plotOutput("linea"),
                 plotOutput("caja")),
        tabPanel("Tipo de infracción",
                 selectInput("crimen","Tipo de incautación",c("Abandono","Contrabando","Falsificacion","Receptacion")),
                 plotOutput("hist2")),
        tabPanel("Valor de incautación",
                 numericInput("min","Valor mínimo",value=500),
                 numericInput("max","Valor mínimo",value=5000),
                 plotOutput("hist")),
        "Datos relacionados",
        tabPanel("Recaudación DNA","Muestra el Panel 2"),
        tabPanel("Cargas en arribo DNA","Muestra Panel 3")
    )
)
server <- function(input, output){
    output$hist<-renderPlot({
        datos %>% filter(INC_VALOR_MN>input$min) %>% filter(INC_VALOR_MN<input$max)%>%group_by(ADUA_ADUA_DESCC) %>% summarise(conteo=n()) %>% ggplot(aes(x=ADUA_ADUA_DESCC,y=conteo))+geom_col()
    })
    output$hist2<-renderPlot({
        datos %>% separate(TIEM_DIA_SK_INCAUTACION, into=c("anio","mes"),sep=4) %>%group_by(INFR_INFR_DESCC,anio) %>% summarise(conteo=n()) %>% subset(subset=grepl(input$crimen,INFR_INFR_DESCC)) %>% ggplot(aes(x=anio,y=conteo))+geom_col()
    })
    output$linea<-renderPlot({
        datos%>% mutate(fecha=ym(TIEM_DIA_SK_INCAUTACION)) %>% group_by(TINC_TINC_DESCC,fecha) %>% summarise(conteo=n())  %>% subset(subset=grepl(input$artefacto,TINC_TINC_DESCC)) %>% ggplot(aes(x=fecha,y=conteo))+geom_line()
    })
    output$caja<-renderPlot({
        datos %>% mutate(fecha=ym(TIEM_DIA_SK_INCAUTACION)) %>% 
            group_by(TINC_TINC_DESCC,fecha,PAIS_PAIS_DESCC_PAIS_PROCEDENC) %>% summarise(conteo=n()) %>% subset(subset=grepl(input$artefacto,TINC_TINC_DESCC)) %>% ggplot()+geom_boxplot(aes(x=PAIS_PAIS_DESCC_PAIS_PROCEDENC,y=conteo))
    })
}
shinyApp(ui = ui, server = server)
