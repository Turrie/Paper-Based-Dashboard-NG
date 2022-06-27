library(shinydashboard)
library(dplyr)
library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
library(shinyalert)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
library(grid)
require(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
library(formattable)
library(shinycssloaders)
library(shinybusy)


### SHINY UI ###
ui <- bootstrapPage(
  # tags$head(includeHTML("gtag.html")),

  navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,inverse = TRUE,

             #darkly,cyborg.flatly, slate, cosmo
             "Nigeria Paper Based Tools Annex", id="nav",
             # tags$style(type = 'text/css', '.navbar { background-color: #262626;
             #                                   font-family: Arial;
             #                                   font-size: 23px;
             #                                   color: #FF0000; }'),
             #useShinyalert(),
             
    # navbarPage(title = div("Paper Based Tools Annex"), 
    #                                               theme = shinytheme("sandstone"),inverse = TRUE,
             
             tabPanel("Use case mapper",
                      shinyalert::useShinyalert(), 
                      
                      
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          #leafletOutput("mymap", width="100%", height="100%"),
                          #shinyjs::useShinyjs(),
                          #withSpinner(tmapOutput(outputId = "ureaplot", width="100%", height="100%"), type = 2),
                          tmapOutput(outputId = "tmapplot", width="100%", height="100%"),
                          # tmapOutput(outputId = "npkplot", width="100%", height="100%"),
                          # tmapOutput(outputId = "yieldplot", width="100%", height="100%"),

                          absolutePanel(id = "controls", class = "panel panel-default",
                                        tags$head(includeCSS("styles.css")),
                                        top = 75, left = 70, width = 320, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                                   
                                       

                      # pickerInput("understood",  span(tags$i(h4("This tool contains tables and maps with advice on application rates of urea, DAP &
                      # NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer depends on soil
                      # conditions and the time of planting.")), style="color:#045a8d"),
                      #             choices = c("Yes", "No"),
                      #             selected = NULL,
                      #             multiple = TRUE,
                      #             options = pickerOptions(maxOptions = 1)), align = "right",class = "mylabel",
                      #span(tags$i(h4("Please fill out information for all fields accurately in order to get correct output.")), style="color:#045a8d"),
                      
                      uiOutput("country"),
                                     
                   
                      conditionalPanel(
                        condition = "input.country == 'Nigeria'",
                        uiOutput("lga_Groups")
                        
                      ),
                      
                                       
                      uiOutput("usecase"),
                      
                     
                      conditionalPanel(
                        condition = "input.country == 'Nigeria'",
                        uiOutput("unit_loc")
                        
                      ),
                      
                      conditionalPanel(
                        condition = "input.country == 'Nigeria' & input.unit_loc == 'ha'",
                        uiOutput("FCY_ha")),
                      
                      conditionalPanel(
                        condition = "input.country == 'Nigeria' & input.unit_loc == 'acre'",
                        uiOutput("FCY_acre")
                        
                      ),
                      
                      uiOutput("plntmth"),
                      
                      conditionalPanel(
                        condition = "input.country == 'Nigeria'",
                        uiOutput("selection")),
                      
                      uiOutput("costs")
  
                      ),
                      
                      #span(tags$i(h4("Give price information here.")), style="color:#045a8d"),
                      conditionalPanel(
                      condition =  "input.costs == 'Yes' ",
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    bottom = 75, left = 405, width = 250, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                                    
                                    #"input.country == 'Nigeria'" &
                                    # conditionalPanel(
                                    # condition =  "input.costs == 'Yes' ",
                                    # paste(span(tags$i(h4("Give price information here (Naira)")), style="color:##008000")))
                                    # 
                                    # conditionalPanel(
                                    #   condition = "input.country == 'Tanzania'",
                                    #   span(tags$i(h4("Give price information here (TZS)")), style="color:##008000")),
                                    
                                    uiOutput("CassavaPrice"),
                                    
                                    conditionalPanel(
                                      condition = "input.country == 'Nigeria'",
                                      uiOutput("NPK151515Price")),
                                    
                                
                                    uiOutput("UreaPrice"),
                                    
                                    shinyalert::useShinyalert(),                 
                                    
                                    
                      )),
                    
                                    
                      # absolutePanel(id = "controls", class = "panel panel-default",
                      #               bottom = 10, left = 10, width = 150, 
                      #               draggable = TRUE, height = "50px",
                      #               shinyalert::useShinyalert(),                 
                      #         uiOutput("btn_go")
                      #         
                      #                  
                      # ),           
                          
                      
                          
                          absolutePanel(id = "logo", class = "card", bottom = 100, right = 200, width = 200, fixed=TRUE, draggable = FALSE, 
                                        tags$img(src='pics/akilimo4.jpg',height = 200, width = 300
                          ),
                          tags$a(href="https://akilimo.org", "Learn more...")
                          
                          ),
                     
                      absolutePanel(id = "go", class = "panel panel-default",
                                    bottom = 5, left = 405, width = 150, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                      uiOutput("btn_go")
                      )),
                      
                      #conditionalPanel(condition = "input.buton > 0", p("I'm a dashboard"))
                      conditionalPanel(condition = ("input.btn_go > 0"),
                      absolutePanel(id = "fr", class = "panel panel-default",
                                    bottom = 5, right = 100, width = 200, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                                    
                                    downloadButton("downloadDatafr", "Download printable guide")
                                    #uiOutput("downloadDatafr")
                      ),
                      
                     
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "100",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')",
                                                                       "https://twitter.com/ACAI_IITA")))
                          #absolutePanel(uiOutput("btn_go"))https://twitter.com/ACAI_IITA

                      ),
                      
             ),

             tabPanel("View maps side by side",width = "100%",
                    
                      textOutput("sidetext"),
                      
                      
                      br(),
                      
                      fluidRow(
                        column(6,
                               downloadButton("downloadData", "Download pdf of maps")
                               
                        ),
                        column(6,
                               #downloadButton("downloadDatafr", "Download printable guide")
                        )
                      ),
                      
                     
                      fluidRow(
                        shinydashboard::box(width = 4,title = "Urea",withSpinner(tmapOutput(outputId = "ureaplot2", width = 450, height = 600))
                        ),
                        
                        conditionalPanel(condition="input.country == 'Nigeria'",
                        shinydashboard::box(width = 4,title = "NPK 15:15:15",withSpinner(tmapOutput(outputId = "npkplot", width = 450, height = 600)))),
                        
                        shinydashboard::box(width = 4,title = "Yield",withSpinner(tmapOutput(outputId = "yieldplot", width = 450, height = 600)))
                        
              

                      )
             ),

             
            
             tabPanel("View Table",
                    
                    #downloadButton("downloadcsv", "Download csv"),
                  
                    conditionalPanel(condition="input.country == 'Nigeria'",
                                     textOutput("tabletext_naira")),
                    
                  
                    #h3('The table below specifies the recommended fertilizer application rates by LGA and month of planting, as well as the expected root yield response. '),
                    conditionalPanel(condition="input.costs == 'No'",
                                     box(class = "mybg",
                                         br(),
                                         width = NULL, status = 'primary',
                                         DT::dataTableOutput('mytable', width = "100%")
                                         
                                     )
                    ),
                    conditionalPanel(condition="input.costs == 'Yes'",
                                     box(class = "mybg",
                                         br(),
                                         width = NULL, status = 'primary',
                                         DT::dataTableOutput('mytable2', width = "100%")
                                         
                                     )
                    )
                    
                    
                      ),
             
             tabPanel(
               uiOutput("tabers")
             ),
           
    # tags$style(type = 'text/css', '.navbar { background-color: #262626;
    #                                   font-family: Arial;
    #                                   font-size: 23px;
    #                                   color: #FF0000; }'),  
    tags$style(HTML(".navbar-header { width:45% }
                   .navbar-brand { width: 45%; font-size: 23px}")) # cen    
                      
             
  )          
)


