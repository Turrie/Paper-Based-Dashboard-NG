#' Paper based Annex dashboard
#' Authors : Meklit Chernet, Turry Ouma, IITA
#' Last updated on : November 2021 (to include GH)
#' 
#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/paper based/PaperbasedDashboard_NG")

#C:\Users\User\Documents\ACAI\paper based\PaperbasedDashboard -v4 - Copy
#C:\Users\User\Documents\ACAI\paper based\PaperBasedAnnex - RW
library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(shinyalert)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
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
library(shinybusy)
library(DT)


### SHINY SERVER ###

server = function(input, output, session) {
  #.............................................................................  
  # Show a modal on initiation of tool
  #.............................................................................
      shinyalert("Paper Based Tools Annex", "This tool contains tables and maps with advice on application rates of urea,
                         NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer depends on soil
                         conditions and the time of planting.
                     
                 
                 
                 ", type = "info", timer = 9000, size = 'm',
                 closeOnClickOutside = FALSE,
                 closeOnEsc = FALSE,
                 animation = TRUE,
                 html = TRUE,

                 showConfirmButton = FALSE,
                 showCancelButton = FALSE,
                 confirmButtonText = "OK",
                 confirmButtonCol = "#AEDEF4")
 
  #............................................................................. 
  #spinner before maps are displayed
  #.............................................................................
  observeEvent(input$btn_go, {
  
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      #spin = "fading-circle",
      #spin = "fading-circle",
      
      color = 	"#228B22",
      #00FF00
      text = "Please wait while the map is being generated..."
    )
    Sys.sleep(6)
    remove_modal_spinner()
  })
  
  #.............................................................................
  #render select input options
  #.............................................................................
  output$country <- renderUI({

    pickerInput("country", "Country:",
                choices = c("Nigeria"),
                selected = "Nigeria",
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
      })
#})

  observeEvent(input$country, {
    if(input$country == "Nigeria")  {
 
      output$usecase <- renderUI({
       
        pickerInput("usecase", "Select use case",
                    choices = c("Fertilizer Recommendation", "Scheduled Planting"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1),
                    )
            })
    }
  })
    

   
      
  output$lga_Groups <- renderUI({
        
        pickerInput("lga_Groups", "Select state",
                    choices = c("Abia", "Akwa Ibom","Anambra", "Benue", "Cross River", "Delta", "Ebonyi","Edo", "Ekiti", 
                                "Enugu","Imo", "Kogi", "Kwara","Ogun", "Ondo", "Osun",  "Oyo","Taraba"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
 
  
  observeEvent(input$usecase, {
    
    if(!is.null(input$region) | !is.null(input$lga_Groups) | !is.null(input$state) | !is.null(input$reg_ghana))  {
  output$plntmth <- renderUI({
        
      pickerInput("plntmth", "Select planting month",
                  choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                              "October", "November", "December"),
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1))
  })
    }
     
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth)) {
      output$costs <- renderUI({
        
        pickerInput("costs", "Would you like to specify your prices for cassava and fertilizers?",
                    choices = c("Yes", "No"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
  
    }
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth))  {
  output$selection <- renderUI({
    
    pickerInput("selection", "Select variable to view",
                              choices = c("NPK 15:15:15 rate", "Expected yield response", "Urea rate"),
                              selected = NULL,
                              multiple = TRUE,
                              options = pickerOptions(maxOptions = 1))
  })
  
  
 
  
    }
  })

  observeEvent(input$usecase, {
    if(!is.null(input$usecase))  {
      
      output$unit_loc <- renderUI({
        
        selectInput("unit_loc", "Select unit of land",
                    choices = c("acre", "ha"))
                  
        
        
        
      })
    }
  }) 
  
  observeEvent(input$usecase, {
    if(!is.null(input$usecase))  {
      
      output$unit_loc_rw <- renderUI({
        
        selectInput("unit_loc_rw", "Select unit of land",
                    choices = c("are", "ha"))
        
        
        
        
      })
    }
  }) 
  
  observeEvent(input$unit_loc, {
    if(!is.null(input$unit_loc))  {
      output$FCY_ha <- renderUI({

        selectInput("FCY_ha", "Select Your Current Yield (Tonnes)",
                                  choices = c("0-7.5 t/ha", "7.5-15 t/ha", "15-22.5 t/ha", "22.5-30 t/ha", ">30 t/ha",  ""),
                    selected = "")            
      })
    

      output$FCY_acre <- renderUI({

        selectInput("FCY_acre", "Select Your Current Yield (Tonnes)",
                    choices = c("0-3 t/acre", "3-6 t/acre", "6-9 t/acre", "9-12 t/acre", ">12 t/acre", ""),
                    selected = "")
      })
    }
      
    })  
  

  observeEvent(input$costs, {
    if(input$costs == "Yes" ) {
   
  output$CassavaPrice <- renderUI({
    
    textInput("CassavaPrice", "Price of cassava per ton")
  })
    }
  })

  observeEvent(input$costs, {
    if(input$costs == "Yes")  {
 
       output$NPK151515Price <- renderUI({
    
    textInput("NPK151515Price", "Cost of NPK:15:15:15 per 50Kg bag")
  }) 
  

    }
  })
  

    observeEvent(input$costs, {
      if(input$costs == "Yes") {
      output$UreaPrice <- renderUI({
        
        textInput("UreaPrice", "Cost of Urea per 50Kg bag")
      })
    }
    })

  
  observeEvent(input$costs, {
    if(input$costs == "Yes") {  
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }else if(input$costs == "No"){
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }
      
    
  })
  
  
 #.................................................................................................................
  

  
        #######################################################################################
      ## Read the GIS layers
      #######################################################################################

  
  TownsNG <- readOGR(dsn = ".", layer = "Places_towns")
  
  RiversNG <- readOGR(dsn = ".", layer = "Rivers")
      
  boundaryNG <- readOGR(dsn=getwd(), layer="gadm36_NGA_1")
      
       
  ngstate <- readOGR(dsn=getwd(), layer="gadm36_NGA_2")
      
      
      ###################################################################################################
      ## NG fertilizer recom for FCY 1:5
      ###################################################################################################
      FR_NG_FCY1 <- readRDS("FRrecom_lga_level1_NG_2020.RDS")
      FR_NG_FCY2 <- readRDS("FRrecom_lga_level2_NG_2020.RDS")
      FR_NG_FCY3 <- readRDS("FRrecom_lga_level3_NG_2020.RDS")
      FR_NG_FCY4 <- readRDS("FRrecom_lga_level4_NG_2020.RDS")
      FR_NG_FCY5 <- readRDS("FRrecom_lga_level5_NG_2020.RDS")
      
   
      ###########################################################################
      ##  adding planting month
      ###########################################################################
      addplm <- function(ds, country){
        ds$respY <- ds$TargetY - ds$CurrentY
        ds$groRev <- ds$NR + ds$TC
        ds$plm <- as.factor(ds$plw)
        levels(ds$plm)[levels(ds$plm) %in% 1:4]   <- "January"
        levels(ds$plm)[levels(ds$plm) %in% 5:8]   <- "February"
        levels(ds$plm)[levels(ds$plm) %in% 9:13]  <- "March"
        levels(ds$plm)[levels(ds$plm) %in% 14:17] <- "April"
        levels(ds$plm)[levels(ds$plm) %in% 18:22] <- "May"
        levels(ds$plm)[levels(ds$plm) %in% 23:26] <- "June"
        levels(ds$plm)[levels(ds$plm) %in% 27:30] <- "July"
        levels(ds$plm)[levels(ds$plm) %in% 31:35] <- "August"
        levels(ds$plm)[levels(ds$plm) %in% 36:39] <- "September"
        levels(ds$plm)[levels(ds$plm) %in% 40:43] <- "October"
        levels(ds$plm)[levels(ds$plm) %in% 44:48] <- "November"
        levels(ds$plm)[levels(ds$plm) %in% 49:53] <- "December"
        
        if(country=="NG"){
          ds$rateUrea <- ds$urea
          ds$rateNPK151515 <- ds$NPK15_15_15
        }else{
          ds$rateUrea <- ds$urea
          ds$rateNPK171717 <- ds$NPK17_17_17
          ds$rateDAP <- ds$DAP
        }
        return(ds)
        }
 
      FR_NG_FCY1_plm <- addplm(ds=FR_NG_FCY1, country = "NG") ## NG if user current yield is level 1
      FR_NG_FCY2_plm <- addplm(ds=FR_NG_FCY2, country = "NG") ## NG if user current yield is level 2
      FR_NG_FCY3_plm <- addplm(ds=FR_NG_FCY3, country = "NG") ## NG if user current yield is level 3
      FR_NG_FCY4_plm <- addplm(ds=FR_NG_FCY4, country = "NG") ## NG if user current yield is level 4
      FR_NG_FCY5_plm <- addplm(ds=FR_NG_FCY5, country = "NG") ## NG if user current yield is level 5
      
     
      ###########################################################################
      ## select FCY and read the corresponding file 
      ## NG: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
      ###########################################################################
      
      #.................................................................................................................
      #Dashboard activity starts here
      #............................................................................. 
      ## Determine platform type and set working directory accordingly
   
      
      
    observeEvent(input$btn_go, {
    
    #define reactive values
    country <- input$country
    FCY_ha <- input$FCY_ha
    print(FCY_ha)
    FCY_acre <- input$FCY_acre

    print(FCY_acre)
    Selection <- input$selection
    
    usecase <- input$usecase
    plantMonth <- input$plntmth
  
    lga_Groups <- input$lga_Groups
 
    plantMonth <- input$plntmth
    cities <- input$city
    unit <- input$unit_loc

    UreaPrice <- as.numeric(input$UreaPrice)
 
    NPK151515Price <- as.numeric(input$NPK151515Price)
 
   
    CassavaPrice <- as.numeric(input$CassavaPrice)
    
    costs <- input$costs

    print(unit)
   
    print(plantMonth)
    
    #specify yield categories
 
    if(unit == 'ha'){
      yield_level <- ifelse( FCY_ha == "0-7.5 t/ha", "a low yield level",
                             ifelse( FCY_ha == "7.5-15 t/ha","a normal yield level",
                                     ifelse( FCY_ha == "15-22.5 t/ha","a medium yield level", 
                                             ifelse( FCY_ha == "22.5-30 t/ha","a high yield level",
                                                     ifelse( FCY_ha == ">30 t/ha","a very high yield level"
                                                     )))))
    }else if(unit == 'acre'){ 
      yield_level <- ifelse( FCY_acre == "0-3 t/acre","a low yield level",
                             ifelse( FCY_acre == "3-6 t/acre","a normal yield level",
                                     ifelse( FCY_acre == "6-9 t/acre","a medium yield level",
                                             ifelse( FCY_acre == "9-12 t/acre","a high yield level",
                                                     ifelse( FCY_acre == ">12 t/acre","a very high yield level")
                                             ))))
    }
    
    
    #lga_Groups = "Abia"    
  
    lgaGroups <- input$lga_Groups
    lgaGroups2 <- input$lga_Groups
    
   
      if (unit == "ha"){
       FCY <- FCY_ha
        
        if(FCY == "7.5-15 t/ha" ){
          ds <- FR_NG_FCY2_plm
        }else if(FCY == "0-7.5 t/ha" ){
          ds <- FR_NG_FCY1_plm
        }else if(FCY == "15-22.5 t/ha" ){
          ds <- FR_NG_FCY3_plm
        }else if(FCY == "22.5-30 t/ha"){
          ds <- FR_NG_FCY4_plm
        }else if(FCY == ">30 t/ha" ){
          ds <- FR_NG_FCY5_plm
        }
      }else if(unit == "acre"){
        FCY <- FCY_acre
        
        if(FCY == "3-6 t/acre" ){
          ds <- FR_NG_FCY2_plm
        }else if(FCY == "0-3 t/acre" ){
          ds <- FR_NG_FCY1_plm
        }else if(FCY == "6-9 t/acre" ){
          ds <- FR_NG_FCY3_plm
        }else if(FCY == "9-12 t/acre" ){
          ds <- FR_NG_FCY4_plm
        }else if(FCY == ">12 t/acre" ){
          ds <- FR_NG_FCY5_plm
          
        }
      }
    
      ####################################################################################################### 
     
      #Subset by state for every filter option presented by users
      Oyo <- droplevels(ds[ds$STATE == "Oyo", ])
      Oyolabel <- data.frame(state= c("Oyo"), lon=c(3.3), lat=c(9))
      
      Ogun <- droplevels(ds[ds$STATE == "Ogun", ])
      Ogunlabel <- data.frame(state= c("Ogun"), lon=c(3.4), lat=c(7.65))
      
      Kogi <- droplevels(ds[ds$STATE == "Kogi", ])
      Kogilabel <- data.frame(state= c("Kogi"), lon=c(6.63), lat=c(8.56))
      
      Kwara <- droplevels(ds[ds$STATE %in% c("Kwara"), ])
      Kwaralabel <- data.frame(state= c( "Kwara"), lon=c(4.9), lat=c(9.5))
      
      Taraba <- droplevels(ds[ds$STATE %in% c("Taraba"), ])
      Tarabalabel <- data.frame(state= c( "Taraba"), lon=c(10.2), lat=c(9.05))
      
      CrossRiver <- droplevels(ds[ds$STATE %in% c("Cross River"), ])
      Crossriver_label <- data.frame(state= c("Cross River"), lon=c(8), lat=c(8.2) )
      
      Benue <- droplevels(ds[ds$STATE %in% c("Benue"), ])
      Benue_label <- data.frame(state= c("Benue"), lon=c(9.5), lat=c(8))
      
      Edo <- droplevels(ds[ds$STATE %in% c("Edo"), ])
      Edolabel <- data.frame(state= c("Edo"), lon=c(5.3), lat=c(7))
      
      Delta <- droplevels(ds[ds$STATE %in% c("Delta"), ])
      Deltalabel <- data.frame(state= c("Delta"), lon=c(6), lat=c(5))
      
      Akwa_Ibom <- droplevels(ds[ds$STATE %in% c("Akwa Ibom"), ])
      Akwa_Ibomlabel <- data.frame(state= c( "Akwa Ibom"), lon=c(8), lat=c(5.45))
      
      Imo <- droplevels(ds[ds$STATE %in% c("Imo"), ])
      Imolabel <- data.frame(state= c( "Imo"), lon=c(6.9), lat=c(5.9))
      
      Abia <- droplevels(ds[ds$STATE %in% c("Abia"), ])
      Abialabel <- data.frame(state= c( "Abia"), lon=c(7.7), lat=c(5.9))
      
      
      Ondo <- droplevels(ds[ds$STATE %in% c("Ondo"), ])
      Ondolabel <- data.frame(state= c( "Ondo"), lon=c(5.3), lat=c(6.6))
      
      Ekiti <- droplevels(ds[ds$STATE %in% c("Ekiti"), ])
      Ekitilabel <- data.frame(state= c( "Ekiti"), lon=c(5.3), lat=c(8.1))
      
      Osun <- droplevels(ds[ds$STATE == "Osun", ])
      Osunlabel <- data.frame(state= c("Osun"), lon=c(4.2), lat=c(8.05))
      
       
      Anambra <- droplevels(ds[ds$STATE %in% c("Anambra"), ])
      Anambralabel <- data.frame(state= c( "Anambra"), lon=c(7.15), lat=c(6.45))
      
      Ebonyi <- droplevels(ds[ds$STATE %in% c("Ebonyi"), ])
      Ebonyilabel <- data.frame(state= c( "Ebonyi"), lon=c(7.83), lat=c(6.67))
      
      Anambra <- droplevels(ds[ds$STATE %in% c("Anambra"), ])
      Anambra_label <- data.frame(state= c("Anambra"), lon=c(6.7), lat=c(5.9))
      
      Enugu <- droplevels(ds[ds$STATE %in% c("Enugu"), ])
      Enugulabel <- data.frame(state= c("Enugu"), lon=c(7), lat=c(7.1))
      
      Ebonyi <- droplevels(ds[ds$STATE %in% c("Ebonyi"), ])
      Ebonyilabel <- data.frame(state= c("Ebonyi"), lon=c(8.25), lat=c(6.9))
      
      #specify other key values for getting recommendations  
      
      if(lgaGroups == "Benue"){
        cities <- c("Makurdi")
        LGApoints <- Benue 
        stateLabel <- Benue_label
        textangle <- 0 
        couple = "One"
      }else if(lgaGroups =="Cross River"){
        cities <- c("Calabar")
        LGApoints <- Benue 
        stateLabel <- Benue_label
        textangle <- 0 
        couple = "One"
      }else if(lgaGroups =="Enugu"){
        cities <- c("Enugu")
        LGApoints <- Enugu 
        stateLabel <- Enugulabel
        textangle <- 0 
        couple = "One"
      }else if(lgaGroups =="Delta"){
        cities = c("Asaba")
        LGApoints <- Delta 
        stateLabel <- Deltalabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Edo"){
        cities = c("Benin City")
        LGApoints  <-  Edo 
        stateLabel  <-  Edolabel
        textangle <- 0 
        couple  <-  "One"
      }  else if(lgaGroups == "Imo"){
        cities = c("Owerri")
        LGApoints  <-  Imo 
        stateLabel  <-  Imolabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Abia"){
        cities = c("Umuahia")
        LGApoints  <-  Abia 
        stateLabel  <-  Abialabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Akwa Ibom"){
        cities = c("Uyo")
        LGApoints  <-  Akwa_Ibom 
        stateLabel  <-  Akwa_Ibomlabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ekiti"){
        cities = c("Ado Ekiti")
        LGApoints  <-  Ekiti 
        stateLabel  <-  Ekitilabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ondo"){ 
        cities = c("Akure")
        LGApoints  <-  Ondo
        stateLabel  <-  Ondolabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Osun"){ 
        cities = c("Osogbo")
        LGApoints  <-  Osun
        stateLabel  <-  Osunlabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Anambra"){ 
        cities = c("Awka")
        LGApoints  <-  Anambra
        stateLabel  <-  Anambralabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ebonyi"){ 
        cities = c("Ebonyi")
        LGApoints  <-  Ebonyi
        stateLabel  <-  Ebonyilabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Taraba"){ 
        cities = "Taraba"
        LGApoints  <-  Taraba
        stateLabel  <-  Tarabalabel
        textangle <- 0 
        couple  <-  "One"
        
      }else if(lgaGroups == "Kogi"){ 
        cities = "Kogi"
        LGApoints  <-  Taraba
        stateLabel  <-  Tarabalabel
        textangle <- 0 
        couple  <-  "One"
        
      }else if(lgaGroups == "Kwara"){
        cities = "Kwara"
        LGApoints  <-  Kwara
        stateLabel  <-  Kwaralabel
        textangle <- 0 
        couple  <-  "One" 
        
      }else if(lgaGroups == "Oyo"){ 
        cities = "Oyo"
        LGApoints  <-  Oyo
        stateLabel  <-  Oyolabel
        textangle <- 0 
        couple  <-  "One" 
        
      }else if(lgaGroups == "Ogun"){ 
        cities = "Abeokuta"
        LGApoints == Ogun
        stateLabel  <-  Ogunlabel
        textangle <- 0 
        couple  <-  "One"
        
      }
      
      
      #filter by month and couple and state
      #plantMonth = "January"
      plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
      
      if(couple == "Two"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
      }
      
      if(couple == "Three"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
      }
      
      plotData <- droplevels(plotData[plotData$STATE %in% lgaGroups, ])
      
      #incorporate GIS layers
      AOI <- lgaGroups
      AOIMapS <- subset(boundaryNG, NAME_1 %in% AOI ) 
      
      AOIMap <- subset(ngstate, NAME_1 %in% AOI )
      AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
      LGAnames <- as.data.frame(AOIMap)
      LGAnames <- cbind(LGAnames, coordinates(AOIMap))
      colnames(LGAnames) <- c("STATE","LGA","long","lat"  )
      LGAnames <- LGAnames[!LGAnames$LGA %in% c("IbadanNorth-West","IbadanNorth-East","IbadanSouth-West", "IbadanSouth-East"),]
      LGAnames$LGA <- gsub("Egbado /", "", LGAnames$LGA )
      
      
      crop_ngstate <- subset(ngstate, NAME_1 %in% AOI )
      towns <- as.data.frame(TownsNG)
      towns <- towns[towns$name %in% cities & towns$fclass %in% c("town", "city"),]
      crop_RiversNG <-  crop(RiversNG, extent(crop_ngstate))
      crop_RiversNG <- crop_RiversNG[crop_RiversNG$fclass == "river", ]
      
      
      LGAaverage <- ddply(plotData, .(LGA, STATE), summarize,
                          LGAUrea = round(mean(rateUrea), digits=0),
                          LGANPK151515 = round(mean(rateNPK151515), digits=0),
                          LGAdY = round(mean(respY), digits=0))
      
      dss <- LGAaverage
      dss$LGAUrea <- dss$LGAUrea / 2.47105
      dss$LGANPK151515 <- dss$LGANPK151515 / 2.47105
      dss$LGAdY <- dss$LGAdY / 2.47105
      
      if(unit == 'acre'){
        LGAaverage <- dss
      }
      
      plotData <- merge(plotData, LGAaverage, by=c("LGA", "STATE"))
      
      if(unit == "ha"){
        plotData$Urea <- round(plotData$LGAUrea/25)*25
        plotData$NPK15_15_15 <- round(plotData$LGANPK151515/50)*50
        plotData$dY <- round(plotData$LGAdY/2)*2
      }else{
        plotData$Urea <- round(plotData$LGAUrea/10)*10
        plotData$NPK15_15_15 <- round(plotData$LGANPK151515/20)*20
        plotData$dY <- round(plotData$LGAdY/1)*1
      }
      
      #csv tables naming
      fileNameCsv <- paste("tables", ".csv", sep="")
      
      AOIMap2 <- merge(AOIMap, unique(plotData[, c("LGA", "Urea", "NPK15_15_15","dY", "LGAdY")]),by.x="NAME_2" ,by.y="LGA")
      AOIMap2$month <- plantMonth
      AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
      plotData$month <- plantMonth
      
   
      
      #generate table
      Currency <- "Naira"
      tt <- unique(as.data.frame(plotData[, c("STATE","LGA", "Urea", "NPK15_15_15", "LGAdY", "month")]))
      tt$LGAdY <- round(tt$LGAdY, digits = 1)
      tt <- tt[order(tt$STATE, tt$LGA, tt$month), ]
      
      tt2 <- dplyr::select(tt, c(STATE, LGA, Urea, NPK15_15_15,LGAdY))
      colnames(tt2) <- c("State","LGA", "Urea (kg/ha)", "NPK 15:15:15 (kg/ha)", "Expected yield increase (t)")
     
      
      #subset by cost information in a reactive environment
     
      if(costs == "No"){
        
        output$tabletext_naira <- renderText({
          
          
          paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
          
        })
        
        output$mytable <- renderDT({tt2},
                                        rownames = FALSE, 
                                        extensions = c('Buttons','FixedColumns'), 
                                        options = list(dom = 'Bfrtip',
                                                       pageLength = nrow(tt2),
                                                       initComplete = DT::JS(
                                                         "function(settings, json) {",
                                                         "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                         "}"),
                                                       
                                                       buttons = list(
                                                         list(extend = 'excel', 
                                                              filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                              title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY, ".", sep="")),
                                                         list(extend = 'pdf',
                                                              filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                              title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  ".", sep=""),
                                                              header = TRUE)
                                                       )
                                                       
                                        )
        )
        
        # data_output <- function(df) {
        #   DT::datatable(df, rownames= FALSE, options = list( dom = 'Bfrtip', buttons = c('excel','pdf','print','colvis'), pageLength = nrow(df), initComplete = DT::JS(
        #     "function(settings, json) {",
        #     "$(this.api().table().header()).css({'background-color': '#369BE9', 'color': '#fff'});",
        #     "}") ), list(extend = 'pdf',
        #                  filename = 'CurrentTable',
        #                  title = paste("My chTitle"),
        #                  header = FALSE),
        #     extensions = c('Buttons','FixedColumns'))
        # }
      
      }else if (costs == "Yes"){
      #tt$totalCost = tt[(UreaPrice * UreaRate) + (NPK171717Price * NPK171717Rate)+(DAPPrice * DAPRate)]
    
      tt_dataframe <- reactive({
        
        df_tt <- data.frame(UreaPrice=input$UreaPrice,NPK151515Price=input$NPK151515Price,CassavaPrice=input$CassavaPrice,
                            STATE=input$lga_Groups)
        
        return(df_tt)
      })
      
  
      #incorporate price information in csv table for display and download by the user
      tt_merge <- merge(tt, tt_dataframe(),by="STATE")
      
      tt_merge$totalSalePrice = as.numeric(tt_merge$LGAdY)  * as.numeric(tt_merge$CassavaPrice)
      tt_merge$totalCost = (as.numeric(tt_merge$UreaPrice)/50 * as.numeric(tt_merge$Urea)) + (as.numeric(tt_merge$NPK151515Price)/50 * as.numeric(tt_merge$NPK15_15_15))
      tt_merge$NetRevenue = as.numeric(tt_merge$totalSalePrice) - as.numeric(tt_merge$totalCost)
      head(tt_merge)
      #tt_merge2 <- dplyr::select(tt_merge, c(STATE, LGA,month, Urea, NPK15_15_15,LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
      
      tt_merge2 <- dplyr::select(tt_merge, c(STATE, LGA, Urea, NPK15_15_15,LGAdY, totalSalePrice, totalCost, NetRevenue))
      colnames(tt_merge2) <- c("State","LGA", "Urea (kg/ha)", "NPK 15:15:15 (kg/ha)", "Expected yield increase (t)", 
                                "Total sale (Naira)", "Fertilizer cost (Naira)", "Profit (Naira)")
      output$tabletext_naira <- renderText({
        
        
        paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
        
      })
      
      output$mytable2 <- renderDT({tt_merge2},
                                 rownames = FALSE, 
                                 extensions = c('Buttons','FixedColumns'), 
                                 options = list(dom = 'Bfrtip',
                                                pageLength = nrow(tt_merge2),
                                                initComplete = DT::JS(
                                                  "function(settings, json) {",
                                                  "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                  "}"),
                                                
                                                buttons = list(
                                                  list(extend = 'excel', 
                                                       filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                       title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")),
                                                  list(extend = 'pdf',
                                                       filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                       title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
                                                       header = TRUE)
                                                )
                                                
                                 )
      )
      # output$mytable2 = DT::renderDataTable({
      #   data_output(tt_merge2)
      # }, caption=paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
      # )
    }
      
      # --------------------------------------------------------------------------
      #side by side maps
      # --------------------------------------------------------------------------
      AOIMap3 <- st_as_sf(AOIMap2)
      #urea plot
      ################################################
      
      #reactive  title based on unit of land
     
      filt_select <- reactive({
        print(Selection)
        if (Selection == "Urea rate"){
          
          filt_select <- "Urea rate"
        }else if (Selection == "Expected yield response"){
          filt_select <- "Expected yield response"
        }else{
          filt_select <- "NPK 15:15:15 rate"
        }
        
      })
      
      tturea <- reactive({
        
        if(unit == "ha"){
          
          tturea <- paste("Recommended urea rate (kg/ha)")
        }else {
          
          tturea <- paste("Recommended urea rate (kg/acre)")
        }
      })
       
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      #tmap output using reactive values
      require(ggrepel)
      library(tmap)
   
      observeEvent(tturea(),
                   {
                     
                     output$ureaplot2 <- renderTmap({
                       
                       
                       sm1 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "Urea",
                           title = tturea(),
                           palette = "Greens")+
                         tm_text(text = "NAME_2")
                       sm1
                       
                       
                       
                     })
                   })
      #NPK plot
      ############################################################################   
    
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk <- reactive({
        
        if(unit == "ha"){
          
          
          ttnpk <- paste("Recommended NPK 15:15:15 rate (kg/ha)")
        }else {
          
          ttnpk <- paste("Recommended NPK 15:15:15 rate (kg/acre)") 
        }
      })
      
      
      
      mopsclae <- unique(AOIMap3$NPK15_15_15)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
      levels(AOIMap3$NPK15_15_15) <- kev
      
      #tmap output using reactive values 
      observeEvent(ttnpk(),
                   {
                     
                     output$npkplot <- renderTmap({
                      
                       sm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK15_15_15",
                           title = ttnpk(),
                    
                           palette = "YlOrBr")+
                         tm_text(text = "NAME_2")
            
                         
                     })
                   })
      
      ############################################################################   
      #yield plot
      #############################################################################
      #reactive title based on unit of land
      
       ttha <- reactive({
        
        if(unit == "ha"){
          
          ttha <- paste("Recommended yield response (t/ha)")
        }else {
          
          ttha <- paste("Recommended yield response (t/acre)")
        }
      })
      
    
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      #tmap output using reactive values
      observeEvent(ttha(),
                   {
                     
                     
                     output$yieldplot <- renderTmap({
                       
                       
                       sm3 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "dY",
                           title = ttha(),
                           #breaks = c(3, 4, 5, 6),
                           #labels = c("Low", "Medium", "High"),
                           palette = "YlGnBu")+
                         tm_text(text = "NAME_2")
                       
                       
                       
                     })
                   })
      

      
      #-------------------------------------------------------------------------
      #front page dynamic tmap
      #this map changes based on input at the filter level
      #-------------------------------------------------------------------------
      
      #reactive selection of variable to view
      observeEvent(filt_select(), {
        if (filt_select() == "Urea rate"){
          
          ureacols <- reactive({
            
            if(unit == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/acre)"
            }
          })
          
          tturea <- reactive({
            
            if(unit == "ha"){
              
              tturea <- paste("Recommended urea rate(kg/ha)")
            }else {
              
              tturea <- paste("Recommended urea rate (kg/acre)")
            }
          })
          
          
          
          ureasclae <- unique(AOIMap3$Urea)
          keU <- as.character(ureasclae[order(ureasclae)])
          AOIMap3$Urea <- factor(AOIMap3$Urea)
          levels(AOIMap3$Urea) <- keU
          
          require(ggrepel)
          library(tmap)
          
          #tmap output using reactive values 
          #urea
          observeEvent(tturea(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           sm1 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "Urea",
                               title = tturea(),
                           
                               
                               #breaks = c(200, 175, 150, 125,100),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Greens")+
                             tm_legend(legend.position = c("right", "top"))+
                             tm_text(text = "NAME_2")
                           sm1
                           
                           
                           
                         })
                       })
        }else if(filt_select() == "NPK 15:15:15 rate"){
          
          ttnpk <- reactive({
            
            if(unit == "ha"){
              
              ttnpk <- paste("NPK 15:15:15 rate (kg/ha)")
            }else {
              
              ttnpk <- paste("NPK 15:15:15 rate (kg/acre)")
            }
          })
          
          mopsclae <- unique(AOIMap3$NPK15_15_15)
          kev <- as.character(mopsclae[order(mopsclae)])
          AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
          levels(AOIMap3$NPK15_15_15) <- kev
          
          #tmap output using reactive values 
          #npk
          observeEvent(ttnpk(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           sm2 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK15_15_15",
                               title = ttnpk(),
                               palette = "Oranges")+
                             tm_text(text = "NAME_2")
                         
                           sm2
                           
                         }) 
                       })
          
        }else if(filt_select() == "Expected yield response"){
          ttha <- reactive({
            
            if(unit == "ha"){
              
              ttha <- paste("Recommended yield response (t/ha)")
            }else {
              
              ttha <- paste("Recommended yield response (t/acre)")
            }
          })
          
          
          Ysclae <- unique(AOIMap3$dY)
          keY <- as.factor(Ysclae[order(Ysclae)])
          AOIMap3$dY <- factor(AOIMap3$dY)
          levels(AOIMap3$dY) <- keY
          
          #tmap output using reactive values 
          #yield
          observeEvent(ttha(),
                       {
                         
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           sm3 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "dY",
                               title = ttha(),
                               #breaks = c(3, 4, 5, 6),
                               #labels = c("Low", "Medium", "High"),
                               palette = "YlGnBu")+
                             tm_text(text = "NAME_2")
                           
                           sm3
                           
                         })
                       })
          
        }
        
        
        #generate static maps
 
       
        #color pallette
         if(unit == "ha"){
          ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                        "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
          tt <- "Urea (kg/ha)"
        }else {
          ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                        "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
          tt <- "Urea (kg/acre)"
        }
        ureasclae <- unique(AOIMap3$Urea)
        keU <- as.character(ureasclae[order(ureasclae)])
        AOIMap3$Urea <- factor(AOIMap3$Urea)
        levels(AOIMap3$Urea) <- keU
        
        require(ggrepel) 
        
        #ggplot urea
        ggUrea <- ggplot(AOIMap3) +
          geom_sf(aes(fill=Urea), col="grey30") +
          scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          geom_text(data=stateLabel, aes(lon, lat, label=state, fontface=2), col='black', size=6)+
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          annotation_scale(location = "br", width_hint = 0.3, line_width = 0.4) +
          annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                                 style = north_arrow_fancy_orienteering) +
          # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
          #                        style = north_arrow_fancy_orienteering) +
          # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8)) 
        
        
        ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
        if(unit == "ha"){
          NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                       "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
          tt <- "NPK 15-15-15 (kg/ha)"
        }else{
          NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
                       "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
          tt <- "NPK 15-15-15 (kg/acre)"
        }
        
        
        mopsclae <- unique(AOIMap3$NPK15_15_15)
        kev <- as.character(mopsclae[order(mopsclae)])
        AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
        levels(AOIMap3$NPK15_15_15) <- kev
        
        
        #npk ggplot
        require(plotly)
        ggNPK <- ggplot(AOIMap3) +
          geom_sf(aes(fill=NPK15_15_15), col="grey30") +
          scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
          #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
        
        #yield color pallette
        if(unit == "ha"){
          tt <- "Yield increase (t/ha)"
          Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
                      "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
                      "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
        }else{
          tt <- "Yield increase (t/acre)"
          Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
                      "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
                      "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
        }
        
        Ysclae <- unique(AOIMap3$dY)
        keY <- as.factor(Ysclae[order(Ysclae)])
        AOIMap3$dY <- factor(AOIMap3$dY)
        levels(AOIMap3$dY) <- keY
        
        #yield ggplot
        ggYield <- ggplot(AOIMap3) +
          geom_sf(aes(fill=dY), col="grey30") +
          scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
          #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
        
        #put the maps together in pdf format
        fileName <- paste("maps", ".pdf", sep="")
        pdf(fileName, onefile = TRUE, height = 14, width=12)
        #pdf.options(paper = "a4")
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5,0.8), "null"))))   
        grid.text(paste("Planting in", plantMonth, "at", yield_level,  sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
        print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
        #print(ggMOP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
        print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
        dev.off()
        
      })  
      
      #.............................................................................................................
  
      
      #.............................................................................................................
      #download buttons for printablse guides based on unit of land and use case selection
      if (usecase == "Fertilizer Recommendation" & unit == "acre"){
        
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("FR Printable guides (acre)", "pdf", sep=".")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Nigeria Acre latest.pdf", file)
          },
          contentType = "application/pdf"
        )
        

      }else if(usecase == "Fertilizer Recommendation" & unit == "ha"){
        
        #download hectare printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("FR Printable guides (ha)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Nigeria Hectare latest.pdf", file)
          },
          contentType = "application/pdf"
        ) 
        
        
      }else if (usecase == "Scheduled Planting" & unit == "acre"){
        #download acre printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("SP Printable guides (acre)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Acre latest.pdf", file)
          },
          contentType = "application/pdf"
        )
      }else if(usecase == "Scheduled Planting" & unit == "ha"){
        
        #download hectare printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("SP Printable guides (ha)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Hectare latest.pdf", file)
          },
          contentType = "application/pdf"
        )  
        
      } 
      
      output$sidetext <- renderText({
        
        
        # paste0('<span style=\"background-color:', "color", '\ ">',text,' #<span style=\"font-size:8px;font-weight:bold;background-color:white;">',"ent_type",'</span></span>')
        paste("Maps and tables below present fertilizer recommendations for cassava planted in", plantMonth, "in", lgaGroups2, "in a field with", yield_level,
              ". Recommendations are optimized to obtain a maximal return on investment, assuming cassava will be harvested after 12 months.
              ")
        
      })
      
       #download maps
      output$downloadData <- downloadHandler(
        filename <- function() {
          paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
        },
        
        content <- function(file) {
          file.copy("maps.pdf", file)
        },
        contentType = "application/pdf"
      )
      
      #download tables
      output$downloadcsv <- downloadHandler(
        filename <- function() {
          paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
        },
        
        content <- function(file) {
          file.copy("tables.csv", file)
        },
        contentType = "application/csv"
      )
      
      
  
      })
      
  }



#runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
