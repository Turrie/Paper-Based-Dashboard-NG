setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard_GH")
source("SP_functions.R")


require(gtools)
library(rgdal)
library(raster)
library(dismo)
library(rgeos)
library(ggspatial)
require(plyr)
library(gtable)
require(qpdf)

library(grid)
require(tidyr)

require(plyr)



library(maptools)

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

#######################################################################################
## Read the GIS layers
#######################################################################################


TownsNG <- readOGR(dsn = ".", layer = "Places_towns")

RiversNG <- readOGR(dsn = ".", layer = "Rivers")


boundaryNG <- readOGR(dsn=getwd(), layer="gadm36_NGA_1")


ngstate <- readOGR(dsn=getwd(), layer="gadm36_NGA_2")


boundaryTZ <- readOGR(dsn=getwd(), layer="gadm36_TZA_1")


tzRegion <- readOGR(dsn=getwd(), layer="gadm36_TZA_2")

boundaryRW <- readOGR(dsn=getwd(), layer="gadm36_RWA_1")


rwState <- readOGR(dsn=getwd(), layer="gadm36_RWA_2")

boundaryGH <- readOGR(dsn=getwd(), layer="gadm36_GHA_1")


ghRegions <- readOGR(dsn=getwd(), layer="gadm36_GHA_2")


###################################################################################################
## NG fertilizer recom for FCY 1:5
###################################################################################################
FR_NG_FCY1 <- readRDS("FRrecom_lga_level1_NG_2020.RDS")
FR_NG_FCY2 <- readRDS("FRrecom_lga_level2_NG_2020.RDS")
FR_NG_FCY3 <- readRDS("FRrecom_lga_level3_NG_2020.RDS")
FR_NG_FCY4 <- readRDS("FRrecom_lga_level4_NG_2020.RDS")
FR_NG_FCY5 <- readRDS("FRrecom_lga_level5_NG_2020.RDS")


###########################################################################
##  TZ fertilizer recom for FCY 1:5
###########################################################################
FR_TZ_FCY1 <- readRDS("FRrecom_lga_level1_TZ_2020.RDS")
FR_TZ_FCY2 <- readRDS("FRrecom_lga_level2_TZ_2020.RDS")
FR_TZ_FCY3 <- readRDS("FRrecom_lga_level3_TZ_2020.RDS")
FR_TZ_FCY4 <- readRDS("FRrecom_lga_level4_TZ_2020.RDS")
FR_TZ_FCY5 <- readRDS("FRrecom_lga_level5_TZ_2020.RDS")


###########################################################################
## RW fertilizer recom for FCY 1:5
###########################################################################
SP_FCY <- readRDS("Ghana_SP_annexData.RDS")
SP_FCY_FCY1 <- SP_FCY[SP_FCY$FCY == "level1", ]
FR_RW_FCY2 <- FR_RW_FCY[FR_RW_FCY$FCY == "level2", ]
FR_RW_FCY3 <- FR_RW_FCY[FR_RW_FCY$FCY == "level3", ]
FR_RW_FCY4 <- FR_RW_FCY[FR_RW_FCY$FCY == "level4", ]
FR_RW_FCY5 <- FR_RW_FCY[FR_RW_FCY$FCY == "level5", ]


###########################################################################
## GH fertilizer recom for FCY 1:5
###########################################################################

FR_GH_FCY <- readRDS("GH_FR_CassavaPaperBased.RDS")
FR_GH_FCY1 <- FR_GH_FCY[FR_GH_FCY$FCY == "level1", ]
FR_GH_FCY2 <- FR_GH_FCY[FR_GH_FCY$FCY == "level2", ]
FR_GH_FCY3 <- FR_GH_FCY[FR_GH_FCY$FCY == "level3", ]
FR_GH_FCY4 <- FR_GH_FCY[FR_GH_FCY$FCY == "level4", ]
FR_GH_FCY5 <- FR_GH_FCY[FR_GH_FCY$FCY == "level5", ]

FR_NG_FCY1_plm <- Agg_plantingMonth_SP(ds=FR_NG_FCY1, country = "NG") ## NG if user current yield is level 1
FR_NG_FCY2_plm <- Agg_plantingMonth_SP(ds=FR_NG_FCY2, country = "NG") ## NG if user current yield is level 2
FR_NG_FCY3_plm <- Agg_plantingMonth_SP(ds=FR_NG_FCY3, country = "NG") ## NG if user current yield is level 3
FR_NG_FCY4_plm <- Agg_plantingMonth_SP(ds=FR_NG_FCY4, country = "NG") ## NG if user current yield is level 4
FR_NG_FCY5_plm <- Agg_plantingMonth_SP(ds=FR_NG_FCY5, country = "NG") ## NG if user current yield is level 5


FR_TZ_FCY1_plm <- Agg_plantingMonth_SP(ds=FR_TZ_FCY1, country = "TZ") ## TZ if user current yield is level 1
FR_TZ_FCY2_plm <- Agg_plantingMonth_SP(ds=FR_TZ_FCY2, country = "TZ") ## TZ if user current yield is level 2
FR_TZ_FCY3_plm <- Agg_plantingMonth_SP(ds=FR_TZ_FCY3, country = "TZ") ## TZ if user current yield is level 3
FR_TZ_FCY4_plm <- Agg_plantingMonth_SP(ds=FR_TZ_FCY4, country = "TZ") ## TZ if user current yield is level 4
FR_TZ_FCY5_plm <- Agg_plantingMonth_SP(ds=FR_TZ_FCY5, country = "TZ") ## TZ if user current yield is level 5


FR_RW_FCY1_plm <- Agg_plantingMonth_SP(ds=FR_RW_FCY1, country = "RW") ## RW if user current yield is level 1
FR_RW_FCY2_plm <- Agg_plantingMonth_SP(ds=FR_RW_FCY2, country = "RW") ## RW if user current yield is level 2
FR_RW_FCY3_plm <- Agg_plantingMonth_SP(ds=FR_RW_FCY3, country = "RW") ## RW if user current yield is level 3
FR_RW_FCY4_plm <- Agg_plantingMonth_SP(ds=FR_RW_FCY4, country = "RW") ## RW if user current yield is level 4
FR_RW_FCY5_plm <- Agg_plantingMonth_SP(ds=FR_RW_FCY5, country = "RW") ## RW if user current yield is level 5

FR_GH_FCY1_plm <- Agg_plantingMonth_SP(ds=FR_GH_FCY1, country = "GH") ## GH if user current yield is level 1
FR_GH_FCY2_plm <- Agg_plantingMonth_SP(ds=FR_GH_FCY2, country = "GH") ## GH if user current yield is level 2
FR_GH_FCY3_plm <- Agg_plantingMonth_SP(ds=FR_GH_FCY3, country = "GH") ## GH if user current yield is level 3
FR_GH_FCY4_plm <- Agg_plantingMonth_SP(ds=FR_GH_FCY4, country = "GH") ## GH if user current yield is level 4
FR_GH_FCY5_plm <- Agg_plantingMonth_SP(ds=FR_GH_FCY5, country = "GH") ## GH if user current yield is level 5