#NUEVA
# cargamos librerías
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(leafpop)
library(grid)

# leemos datos
data <- read.csv("https://raw.githubusercontent.com/lau-cloud/BCN_OPEN_DATA/master/2019_vehicles.csv", 
                 encoding = "UTF-8", stringsAsFactors = TRUE) %>% 
    filter(Descripcio_tipus_vehicle != "Desconegut", 
           Nom_barri != "Desconegut",
           !is.na(Descripcio_torn)) %>%
  mutate(Descripcio_tipus_vehicle=fct_recode(Descripcio_tipus_vehicle, "Patinet elèctric"="Veh. mobilitat personal amb motor"))

# vehículos top
vehicles_top <- c("Turisme", 
                  "Motocicleta",
                  "Furgoneta", 
                  "Ciclomotor", 
                  "Bicicleta",
                  "Taxi", 
                  "Patinet elèctric")

`%notin%` <- Negate(`%in%`)

# categorias con pocos accidentes en "Altres"
levels(data$Descripcio_tipus_vehicle)[which(levels(data$Descripcio_tipus_vehicle) %notin% 
                                                vehicles_top)] <- "Altres"

# insertamos iconos
emovehicles <- data.frame(Descripcio_tipus_vehicle=c("Turisme", "Motocicleta", "Furgoneta",
                                                     "Ciclomotor", "Bicicleta", "Taxi",
                                                     "Patinet elèctric",
                                                     "Altres"),
                          emovehicle=c("🚗 Turisme", "🏍️ Motocicleta", "🚚 Furgoneta",
                                       "🛵 Ciclomotor", "🚲 Bicicleta", "🚕 Taxi", 
                                       "🛴 Patinet elèctric", "Altres"),
                          emo=c("🚗", "🏍", "🚚", "🛵", "🚲", "🚕", "🛴", "Altres"))
data <- plyr::join(data, emovehicles, by="Descripcio_tipus_vehicle") 


# leemos GeoJSON
bcn_map <- geojson_read("https://raw.githubusercontent.com/lau-cloud/BCN_OPEN_DATA/master/neighbourhoods.geojson", 
                        what = "sp")


# orden factores
data$Descripcio_dia_setmana <- factor(data$Descripcio_dia_setmana, levels = c("Dilluns", "Dimarts", "Dimecres",
                                                                              "Dijous", "Divendres", "Dissabte", 
                                                                              "Diumenge"))
data$Descripcio_torn <- factor(data$Descripcio_torn, levels = c("Matí", "Tarda", "Nit"))
data$emovehicle <- factor(data$emovehicle, levels = c("🚗 Turisme", "🏍️ Motocicleta", "🚚 Furgoneta",
                                                      "🛵 Ciclomotor", "🚲 Bicicleta", "🚕 Taxi", 
                                                      "🛴 Patinet elèctric", "Altres"))
data$emo <- factor(data$emo, levels = c("🚗", "🏍", "🚚","🛵", "🚲", "🚕","🛴", "Altres"))

###### Shiny ###### 

###### UI ###### 
ui <- fluidPage(useShinyjs(),
    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),
    tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')),
    fluidRow(
        column(10, wellPanel(p(
               HTML(paste(h2("Accidents a Barcelona"),"<br>","<p><span style = 'font-size:110%'>Durant el 2019, més de 19.000 vehicles es van veure implicats en accidents a la ciutat de Barcelona.", "<br>",
                          "Els accidents amb <b>motocicletes i bicicletes</b> creixen exponencialment, i els <b>patinets de mobilitat personal</b>","<br>", "amb o sense motor, es veuen implicats cada vegada en més incidents.",
                          "<br>", "<br>",
                          "Saps quins són els barris on hi ha més accidents de cotxe, bicicleta o patinet elèctric?",
                          "En quin moment del dia hi ha més accidents pel teu barri? N'hi ha més dilluns o dissabte?", "<br>",
                          "<b>Navega i descobreix quins riscos tens quan condueixes o transites per la ciutat!</b></span></p>")))))),

                   sidebarLayout(
                     sidebarPanel(id="sidebar",tags$head(tags$script("
      Shiny.addCustomMessageHandler('background-color', function(color) {
        document.body.style.backgroundColor = color;
      });
    ")),
                                  theme = shinytheme("superhero"),

                       tabsetPanel(type="pills", id="tabs",

                                             tabPanel("Accidents", icon=icon("car-crash"),
                                
                                div(br(), id="inputs", prettyRadioButtons("vehicle", label = "Vaig amb...",
                                                   choices = c(levels(data$emovehicle), "Tots"), selected = "Tots",
                                                   status = "default", animation = "tada", icon = icon("check")),
                                selectInput('dset', 'Dia de la setmana', c("Tots", levels(data$Descripcio_dia_setmana))),
                                selectInput('mdia', 'Moment del dia', c("Tots", levels(data$Descripcio_torn))),
                                selectInput('districte', 'Districte', c("Tots", unique(as.character(data$Nom_districte)))),
                                uiOutput("selecciobarri"),
                                actionButton("resetAll", "Reset", icon=icon("filter")), br(),br(),
                                plotOutput(outputId="rosa"))
                                ),
                     tabPanel("Info", icon=icon("info"), 
                              p(
                                HTML(paste(h3("Com navegar per l'app"), 
                                           "<li> Els valors del mapa varien en funció de les opcions triades</li>","<br>", 
                                           "<li> Clica a 'Reset' per tornar a la vista per defecte</li>", "<br>", 
                                           "<li> Col·loca el cursor a sobre dels barris per consultar el número d'accidents</li>", "<br>",
                                           "<li> Clicant, podràs veure el percentatge d'accidents del barri seleccionat respecte al total 
                                           de Barcelona, i el número de vehicles implicats (desglossat per tipus de vehicle)
                                           </li>", "<br>",
                                           "<li> El mapa varia aplicant qualsevol filtre. El gràfic radial només es filtra per tipus de vehicle</li>", "<br>",
                                           
                                           h3("Dades"), 
                                           "<li> Accidents atesos per la Guàrdia Urbana a la ciutat de Barcelona 2019. Disponible <a href='https://opendata-ajuntament.barcelona.cat/data/ca/dataset/accidents-gu-bcn'><b>aquí</b></a></p></li>","<br>", 
                                           "<li> Vehicles implicats en accidents gestionats per la Guàrdia Urbana a la ciutat de Barcelona 2019. Disponible <a href='https://opendata-ajuntament.barcelona.cat/data/ca/dataset/accidents-vehicles-gu-bcn'><b>aquí</b></a></p></li></li>","<br>", 
                                           "<li> Mapa GeoJSON de Barcelona</li>","<br>",
                                           
                                           h3("Codi"), 
                                           "Aquesta eina està desenvolupada íntegrament en R, especialment amb les llibreries 'shiny',
                                           'leaflet' i 'ggplot2'"
                                           )))),
                     tabPanel("Qui som?", icon=icon("user-friends"), p(
                       HTML(paste(
                         "<br>",
                         "<li> <b>Laura Navarro Soler</b>. Periodista de dades, graduada en Periodisme per la Universidad Miguel Hernández de Elche (2017) i postgrau en Periodisme de Dades i Visualització per la Universitat Blanquerna (2019).
                         Treballo com a <i>freelance</i> amb interés en temes socials, ambientals i científics.</li>","<br>",
                         "<li> <b>Xavier Vivancos García</b>. Enginyer de Telecomunicacions per la Universitat Pompeu Fabra i Màster en Business Intelligence i Big Data (Universitat Oberta de Catalunya). Treballo com a consultor BI, 
                         utilizant majorment Power BI per elaborar i mantenir <i>dashboards</i>. Apassionat del llenguatge R i Kaggle.</li>"
                       ))))
                              
                     )),
                   
                   mainPanel(tags$style(type="text/css",
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }"),
                             leafletOutput("mymap", width="100%", height="900")
                   )
    ))

###### UI ###### 



###### Server ###### 
server <- function(input, output, session) {
    
    # output para que cuando seleccionamos un distrito, sólo nos aparezcan barrios pertenecientes a ese distrito
    output$selecciobarri <- renderUI({
        conditionalPanel(condition = "input.districte != 'Tots'", 
                         selectInput("barri", "Pel barri de:", 
                                     choices = c("Tots", unique(as.character(data[data$Nom_districte == input$districte, ]$Nom_barri)))), )
    })
    
    #cambiar background 
    observeEvent(input$mdia, {
      if(input$mdia == "Matí"){
        session$sendCustomMessage("background-color", "white")
      } 
      if(input$mdia == "Tarda"){
        session$sendCustomMessage("background-color", "grey")
      } 
      if(input$mdia == "Nit"){
        session$sendCustomMessage("background-color", "#333333")
      } 
      if(input$mdia == "Tots"){
        session$sendCustomMessage("background-color", "white")
      }
    })
    
    
    # reset filtros
    observeEvent(input$resetAll, {
      reset("inputs")
    })
    
    
    # reset filtros
    observeEvent(input$resetAll, {
      reset("inputs")
    })
    
    
    # output rosa (diagrama radial)
    output$rosa <- renderPlot({
      
      if (input$vehicle != "Tots") {
        data <- data[data$emovehicle == input$vehicle,]

      }

      data %>% 
        filter(!is.na(Descripcio_torn)) %>% 
        group_by(Descripcio_dia_setmana, Descripcio_torn) %>%
        summarise(count=n_distinct(Numero_expedient)) %>% 
        ggplot(aes(x = Descripcio_dia_setmana,  y = count, fill = Descripcio_torn)) +
        coord_polar(theta = "x", start = -pi/45) +
        geom_bar(stat = "identity", alpha = 0.9) +
        scale_fill_manual( values = c("#ABDDA4", "#FEE08B", "#9E0142")) +
        scale_y_continuous(label=function(x) format(x, big.mark=",")) +
        xlab("") +
        ylab("") +
        theme_minimal() +
        theme(legend.text = element_text(size=10.1, color = "#575757"),
              axis.text = element_text(color = "#575757", size = 9.4),
              axis.text.y = element_text(size = 9),
              plot.title = element_text(color = "#575757", size = 18, hjust = 0.5),
              panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              plot.background = element_rect(fill = "white", color="white"),
              plot.margin = margin(0,0,0,0, "cm"),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-20,0,0,0)

              
        ) + ggtitle(paste0("Accidents de ",ifelse(input$vehicle %notin% c("Tots", "Altres"),
                                                substring(input$vehicle, 2),input$vehicle))) 
      
    })


    
    # output mapa leaflet
    output$mymap <- renderLeaflet({
        if (input$vehicle != "Tots") {
            data <- data %>% filter(emovehicle == input$vehicle)
        }
        if (input$dset != "Tots") {
            data <- data %>% filter(Descripcio_dia_setmana == input$dset)
        }
        if (input$mdia != "Tots") {
            data <- data %>% filter(Descripcio_torn == input$mdia)
        }
        if (input$districte != "Tots") {
            data <- data %>% filter(Nom_districte == input$districte)
        }
        if (input$barri != "Tots") {
            data <- data %>% filter(Nom_barri == input$barri)
        }       
      
        # número de accidentes por barrio
        n <-  data %>%
          group_by(Nom_barri,  .drop = FALSE) %>%
            summarise(accidentes=n_distinct(Numero_expedient)) %>%
            rename(neighbourhood=Nom_barri) 
        
        # número de vehículos implicados por barrio
        nvehiculos <-  data %>%
          count(Nom_barri) %>%
          rename(neighbourhood=Nom_barri)
        
        # tipo de vehículo implicado
        vehiculos <- data %>%
          filter(Descripcio_tipus_vehicle %in% c(vehicles_top, "Altres")) %>%
          select(Nom_barri, emo) %>%
          group_by(Nom_barri, emo, .drop = FALSE) %>%
          summarize(n = n()) %>%
          mutate(Total=sum(n)) %>%
                   #paste0(n, "  (",round(ifelse(is.nan(n/sum(n)), 0, n/sum(n))*100,1), "%)")) %>%
          ungroup() %>% 
          mutate(Vehicles= Total,
                   #paste0(Total, "  (", round(ifelse(is.nan(Total/18946), 0, Total/18946)*100,1), "% de BCN)" ),
                 "    "="    ") %>%
               
        
          spread(emo, n) %>%
          rename(neighbourhood=Nom_barri) 
        
        vehiculos <- merge(n, vehiculos, all.x=TRUE, all.y = TRUE) %>%
          mutate(Accidents=paste0(accidentes,  "  (", round(ifelse(is.nan(accidentes/9989), 0, accidentes/9989)*100,2), "%)" ))
        
       # vehiculos2 <- vehiculos[, c(1, 15, 5, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
        vehiculos2 <- vehiculos[, c(1, 14, 5, 4, 6, 7, 8, 9, 10, 11, 12, 13)]
        
        colnames(vehiculos2)[5:12] <- c("- 🚗", "- 🏍", "- 🚚", "- 🛵",
                                      "- 🚲", "- 🚕", "- 🛴", "- Altres")
        
        vehiculos2 <- vehiculos2[, colSums(vehiculos2 != 0) > 0]
        
        # introducimos accidentes por vehículo en el archivo GeoJSON
        bcn_map@data <- plyr::join(bcn_map@data, vehiculos2, by="neighbourhood") 
        
        
        # introducimos el número de accidentes por barrio en el archivo GeoJSON
        bcn_map@data <- plyr::join(bcn_map@data, n, by="neighbourhood") 
        bcn_map@data$accidentes <- ifelse(is.na(bcn_map@data[["accidentes"]]), 0, bcn_map@data[["accidentes"]])
        bcn_map@data$nlog <- ifelse(bcn_map@data[["accidentes"]]!=0, log10(bcn_map@data[["accidentes"]]), bcn_map@data[["accidentes"]])

        
        # escala de color viridis
        pal <- colorNumeric("Spectral", c(0, log10(1174)), reverse = TRUE)
  
        # visualización
        leaflet(bcn_map) %>%
            addProviderTiles(ifelse(input$mdia == "Nit",providers$CartoDB.DarkMatter,
                                    providers$CartoDB.Positron)) %>%       
            addPolygons(color="black", weight=0.5, fillColor=~pal(bcn_map@data$nlog),
                        fillOpacity=0.8, popup = popupTable(bcn_map, row.numbers = FALSE,
                                                            feature.id = FALSE,
                                                            zcol= 3:(ncol(bcn_map@data)-2)),
                        label=~paste0(bcn_map@data[["neighbourhood"]], ": ", bcn_map@data[["accidentes"]]), highlightOptions=highlightOptions(weight=4)) %>%
            addResetMapButton() %>%
            addLegend(pal=pal, values=~bcn_map@data[["nlog"]], 
                      opacity=0.5, title="Nº accidents", position="bottomright",
                      labFormat=labelFormat(transform=function(x) round(10^x)))
 

    })
    
}
###### Server ###### 


# ejecutamos aplicación
shinyApp(ui = ui, server = server)
