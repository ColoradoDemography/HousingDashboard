# Unemployment Rate Dashboard  Support functions
# Adam Bickford January 2020
# 

library(tidyverse, quietly=TRUE)
library(stringr)
library(readr)
library(readxl, quietly=TRUE)
library(RPostgreSQL)
library(blsAPI)
library(plotly)
library(scales, quietly=TRUE)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(RColorBrewer)


# Additions for Database pool
library('pool') 
library('DBI')
library('stringr')
library('config')

# Set up database pool 
config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)



onStop(function(){
  poolClose(DOLAPool)
})


# Support Functions
# NumFmt formats a numberic variable to a whold number, comma separated value
#
NumFmt <- function(inval){
  outval <- format(round(inval ,digits=0),  big.mark=",")
  return(outval)
}

# Percent returns a percentage value
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC( x, format = format, digits = digits, ...), "%")
}

# simpleCap produces string in Proper case
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}


# popPlace list of county names
popPlace <- function(DBPool) {

  # Create Connection Strings
  clookupStr <- paste0("SELECT countyfips, placefips, municipalityname, year FROM estimates.county_muni_timeseries WHERE year = 2019 and placefips = 0;")
  plookupStr <- paste0("SELECT countyfips, placefips, municipalityname, year FROM estimates.county_muni_timeseries WHERE year = 2019;")
 
  
  # f.cLookup contains the county records
  f.cLookup <- dbGetQuery(DBPool, clookupStr)
  
  f.pLookup <- dbGetQuery(DBPool, plookupStr)
  
  # County   
  
  f.cLookup[,3] <- sapply(f.cLookup[,3], function(x) simpleCap(x))
  
  
  # Municialities
  
  
  #removing errant records...
  f.pLookup <- f.pLookup[which(f.pLookup$placefips != 0),] #remove State Records
  f.pLookup <- f.pLookup[which(f.pLookup$countyfips != 999),] # County total records for multiple places
  f.pLookup <- f.pLookup[which(f.pLookup$placefips != 99990),] #Remove Unincoprpoated Areas
  f.pLookup <- f.pLookup[which(!is.na(f.pLookup$placefips)),] #Remove Disbanded Areas
  
  f.pLookup$municipalityname <- gsub(' \\(Part\\)','',f.pLookup$municipalityname)
  f.pLookup$municipalityname <- gsub(' \\(part\\)','',f.pLookup$municipalityname)
  f.pLookup$municipalityname <- gsub('Sprgs','Springs',f.pLookup$municipalityname)
  f.pLookup$municipalityname <- gsub('/G','',f.pLookup$municipalityname)
  

  
  # merging County and municipals
  f.cty <- f.cLookup[,c(1,3)] %>% 
          mutate(municipalityname = ifelse(municipalityname == "Colorado State","Colorado", municipalityname))
  
  f.plac <- left_join(f.pLookup,f.cty,by="countyfips")
  names(f.plac)[3] <- "municipalityname"
  names(f.plac)[5] <- "countyname"
  f.plac <- f.plac[,c(2,1,3:5)]
  f.plac <- f.plac[order(f.plac$municipalityname),]   
  
  outlist <- list("cty" = f.cty, "place" = f.plac)
   
  return(outlist)
}

#listToFips retuns a fips code from a county name

listTofips <- function(df, inList1, lvl){

  # Function to produce a vector of FIPS codes from an input list of names and codes
  if(lvl == "Counties") {
    f.outdata <- df$cty
    outfips <- f.outdata[which(f.outdata$municipalityname == inList1),1]
  } else {
    f.outdata <- df$place  # Create County Place 
    fipsl <- f.outdata[which(f.outdata$municipalityname == inList1),1:2]
    if (nrow(fipsl) == 1) {
      outfips <- paste0(fipsl[1,2], str_pad(fipsl[1,1],5,pad = "0"))
    } else {
      outfips <- paste0("999", str_pad(fipsl[1,1],5,pad = "0"))
    }
  }
  
  return(outfips)
} #end listTofips

# Substring Right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# GenPlot returns the Plots
GenPlot <- function(DBPool, geogfips, geogname, datlevel) {

geogsel <- listTofips(geogfips,geogname, datlevel)

# Generating the data
if(datlevel == "Counties") {
  sqlLookup = paste0("SELECT year, countyfips, totalhousingunits, households, vacanthousingunits, vacancyrate FROM estimates.county_profiles WHERE countyfips = ", geogsel,";")
  if(geogsel == 14) {   # Fix for Broomfield county
    f.rawData <- dbGetQuery(DBPool, sqlLookup) %>% filter(year >= 2000)
  } else {
    f.rawData <- dbGetQuery(DBPool, sqlLookup)
  }
  
  f.chartData <- f.rawData %>%
    mutate(occupancyrate = 100 - vacancyrate,
           occupiedhousingunits = households,
           yoy_total = totalhousingunits - lag(totalhousingunits),
           yoy_occ = occupiedhousingunits - lag(occupiedhousingunits),
           yoy_vacant = vacanthousingunits - lag(vacanthousingunits))
  }
if(datlevel == "Municipalities") {
  sqlLookup = paste0("SELECT year,  countyplace, totalhousingunits, occupiedhousingunits, vacanthousingunits, vacancyrate FROM estimates.muni_pop_housing WHERE countyplace = '", geogsel,"';")
  
  f.chartData <- dbGetQuery(DBPool, sqlLookup) 
  
  f.chartData <- f.chartData %>%
    mutate(occupancyrate = 100 - vacancyrate,
           yoy_total = totalhousingunits - lag(totalhousingunits),
           yoy_occ = occupiedhousingunits - lag(occupiedhousingunits),
           yoy_vacant = vacanthousingunits - lag(vacanthousingunits))
  }



f.chartDataout <- f.chartData

# Chart Titles, legend entries and caption
total_tit <- paste0("Total and Occupied Housing Units, ",geogname)
yoy_tit <- paste0("Year to Year Difference, ",geogname)
bar_tit <- paste0("Occupied and Vacant Housing Units, ",geogname)

totStr <- "Total Housing Units"
occStr <- "Occupied Housing Units"
vacStr <- "Vacant Housing Units"

captionSTR <- paste0("Data and Visualization by the<br>State Demography Office,<br>Print Date: ", format(Sys.Date(), "%m/%d/%Y"))

# tool tip text
f.chartData$totHU_Text <- paste0("Total Housing Units,", f.chartData$year,": ",NumFmt(f.chartData$totalhousingunits))
f.chartData$occHU_Text <- paste0("Occupied Housing Units,", f.chartData$year,": ",NumFmt(f.chartData$occupiedhousingunits))
f.chartData$yoytot_Text <- paste0("Year to Year Difference<br>Total Housing Units,", f.chartData$year-1, " to ",f.chartData$year,": ",NumFmt(f.chartData$yoy_total))
f.chartData$yoyocc_Text <- paste0("Year to Year Difference<br>Occupied Housing Units,", f.chartData$year-1, "to ",f.chartData$year,": ",NumFmt(f.chartData$yoy_occ))
f.chartData$vacR_Text <- paste0("Vacancy Rate, ",f.chartData$year,": ",percent(f.chartData$vacancyrate),"<br>Vacant Housing Units: ",NumFmt(f.chartData$vacanthousingunits))
f.chartData$occR_Text <- paste0("Occupancy Rate, ",f.chartData$year,": ",percent(f.chartData$occupancyrate),"<br>Occupied Housing Units: ",NumFmt(f.chartData$occupiedhousingunits))


# Line Chart
lineCh <- plot_ly(f.chartData, x = ~year, y = ~totalhousingunits, type = 'scatter', mode = 'lines+markers',
               line = list(color = 'rgb(0,76,153)'),
               marker = list(color = 'rgb(0,76,153)'),
               name = totStr, text = ~totHU_Text, hoverinfo = 'text')
lineCh <- lineCh %>% add_trace(y = ~occupiedhousingunits, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(255,128,0)'),
                               marker = list(color = 'rgb(255,128,0)'),
                               name = occStr, text = ~occHU_Text, hoverinfo = 'text')
# Annotation
lineCh <- lineCh %>% add_annotations(text=captionSTR, 
                                     xref = 'paper', yref = 'paper',  
                                     x = 1.2,  y = 0,
                   align='left', showarrow=FALSE,
                   font=list(size=10))

lineCh <- lineCh %>% layout(autosize = T,
                      title = total_tit,
                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      hoverlabel = "right",
                      xaxis = list(title = "Year",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Housing Units",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      legend = list(legend = list(x = 100, y = 0.5)))


# Year to Year  Chart

yoyCh <- plot_ly(f.chartData, x = ~year, y = ~yoy_total, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'rgb(0,76,153)'),
                  marker = list(color = 'rgb(0,76,153)'),
                  name = totStr, text = ~yoytot_Text, hoverinfo = 'text')
yoyCh <- yoyCh %>% add_trace(y = ~yoy_occ, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(255,128,0)'),
                               marker = list(color = 'rgb(255,128,0)'),
                               name = occStr, text = ~yoyocc_Text, hoverinfo = 'text')
# Annotation
yoyCh <- yoyCh %>% add_annotations(text=captionSTR, 
                                     xref = 'paper', yref = 'paper',  
                                     x = 1.2,  y = 0,
                                     align='left', showarrow=FALSE,
                                     font=list(size=10))


yoyCh <- yoyCh %>% layout(autosize = T,
                            title = yoy_tit,
                            paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                            hoverlabel = "right",
                            xaxis = list(title = "Year",
                                         gridcolor = 'rgb(255,255,255)',
                                         showgrid = TRUE,
                                         showline = FALSE,
                                         showticklabels = TRUE,
                                         tickcolor = 'rgb(127,127,127)',
                                         ticks = 'outside',
                                         zeroline = TRUE),
                            yaxis = list(title = "Housing Units",
                                         gridcolor = 'rgb(255,255,255)',
                                         showgrid = TRUE,
                                         showline = FALSE,
                                         showticklabels = TRUE,
                                         tickcolor = 'rgb(127,127,127)',
                                         ticks = 'outside',
                                         zeroline = FALSE),
                            legend = list(legend = list(x = 100, y = 0.5)))

# Stacked Bar Chart for Vacancy Rate 
barCh <-  plot_ly(f.chartData, x = ~year, y = ~occupiedhousingunits, type = 'bar', 
                          marker = list(color = 'rgb(255,128,0)'),
                          name = occStr, text = ~occR_Text, hoverinfo = 'text')
barCh <- barCh %>% add_trace(y = ~vacanthousingunits, type = 'bar', 
                             marker = list(color = 'rgb(96,96,96)'),
                             name = vacStr, text = ~vacR_Text, hoverinfo = 'text')

# Annotation
barCh <- barCh %>% add_annotations(text=captionSTR, 
                                     xref = 'paper', yref = 'paper',  
                                     x = 1.2,  y = 0,
                                     align='left', showarrow=FALSE,
                                     font=list(size=10))

barCh <- barCh %>% layout(autosize = T,
                          title = bar_tit,
                          paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                          hoverlabel = "right",
                          barmode = 'stack',
                          xaxis = list(title = "Year",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE),
                          yaxis = list(title = "Housing Units",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE),
                          legend = list(legend = list(x = 100, y = 0.5)))


#Final Formatting of  f.chartDataOUt

  f.chartDataout <- f.chartDataout %>%
    mutate(place = geogname) %>%
    select(place, year, totalhousingunits, occupiedhousingunits, vacanthousingunits, vacancyrate, occupancyrate) 


f.chartDataout[,3:5] <- sapply(f.chartDataout[,3:5],function(x) NumFmt(x))
f.chartDataout[,6:7] <- sapply(f.chartDataout[,6:7],function(x) percent(x))

 
names(f.chartDataout) <- c("Place","Year","Total Housing Units","Occupied Housing Units","Vacant Housing Units",
                           "Vacancy Rate", "Occupancy Rate")


outlist <- list("LINE" = lineCh, "YOY" = yoyCh, "BARCH" = barCh, "CHDATA" = f.chartDataout)
return(outlist)
}


