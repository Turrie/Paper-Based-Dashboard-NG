Paper based tools annex dashboard
About
In ACAI we have recommendation made for fertilizer application and schedule planting for cassava growers in Nigeria and Tanzania. Currently we have fertilizer recommendations based on farmers current yield, planting month by state and LGA for Nigeria and by region and district for Tanzania. For every farmer’s current yield and planting month we have fertilizer rates and expected yield increase which is presented both in tables and maps. The scheduled planting (SP) gives recommendation on how much root yield can be obtained given a planting month and farmers current yield defined by user and just like the fertilizer recommendation, the SP is also working at LGA/district level.
The paper-based tools annex dashboard is an interative shiny app with maps, tables and some graphical output to show difference based on either planting month or farmers current yield, spatial variation, etc.
The interactive dashboard provides a medium where users:
-	Can indicate their current yield, location and planting month and be provided by recommendation both in table and map format
-	Can indicate their location and be provided with recommendations for several planting months together with expected yield increase

The steps to follow 
You need shape files for the country you want to work with. These should be saved under this link /home/akilimo/projects/AKILIMO_Dashboards/PaperBasedAnnex/Input/readGISlayers You can follow the naming to suit the country.
You also need some precalculated FCY values for the specific country. These can be provided by Meklit, and need to be pasted here: /home/akilimo/projects/AKILIMO_Dashboards/PaperBasedAnnex/Input/readFCY 
Please create a folder with the country abbreviations.
You also need printable guides for the country you work in, to be stored here: /home/akilimo/projects/AKILIMO_Dashboards/PaperBasedAnnex/Input/PrintableGuides
In the script, the country info will need to reworked, afterwhich the dashboard should go live just after running the server.R file ((press Run App).
Tabular data with location, planting month, current yield, target yield, cost and expected net revenue, and rate of fertilizers.


The dashboard has 3 panels:
The use case mapper 
The use case mapper offers an interactive map with a floating filter that allows selection of maps by country, state and use case, unit of land, current yield and planting month. The tool also allows selection of variables to be viewed by:
1.	NPK 15:15:15 rate
2.	Expected yield response
3.	Urea rate
4.	DAP (TZ)
The user can also opt to specify their prices for cassava and fertilizers in order to get more accurate results. After pressing the icon ‘GET MAPS & TABLES’, the user gets an interactive map that allows the user to zoom in and out in order to have a better view of the geographical areas presented and gives a legend to explain the color coding. 
On this panel also, a user can download a printable guide which will be specific to filter values specified by the user.
View maps side by side
This offers side by side viewing of the maps by all the variables and allows the same level of zoom and user color coding. The maps can also be downloaded as required by the user.
View table panel
This panel offfers a table that gives a detailed summary of the information gathered from the filter and shared on the map. The table can be downloaded in two formats: excel and PDF. It also has a ‘send to printer’ option. The columns can also be toggled using the ‘column visibility’ tab at the top of the table to offer better visibility of the select columns.


