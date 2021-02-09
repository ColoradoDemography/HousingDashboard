# SDO Housing Dashboars
# Adam Bickford January 2021
# 
library(plotly)
library(shiny)

source("setup.R")

function(req) {
  htmlTemplate("index.html",
                level=selectInput("level","Select Data Level:", choices=c("Select a Data Level","Counties","Municipalities")),  
                geog=selectInput("geog","Select a Location:",choices= ""), # Build this from data set
                goBtn = actionButton("goButton","Generate Charts"),
                line_chart = plotlyOutput("LINE", width = "1000px", height = "475px"),
                yoy_chart = plotlyOutput("YOY", width = "1000px", height = "475px"),
                bar_chart = plotlyOutput("BARCH", width = "1000px", height = "475px"),
                dlBtn = downloadButton("CHDATA","Download Data (CSV)"))
 }



 
