source("setup.R")

function(input, output, session) {
  geogList <- popPlace(DOLAPool)
  ctyList <- geogList$cty
  placeList <- geogList$place
  
observeEvent(input$level, ({

  if(input$level == "Select a Data Level") { #the initial state of the dropdowns
    outUnit <- ""
  }
  
  if(input$level == "Counties") {
    outUnit <- unique(as.list(ctyList$municipalityname))
  }
  if(input$level == "Municipalities") {  
    outUnit <- unique(as.list(placeList$municipalityname))
  }
  
  updateSelectInput(session, "geog", choices = outUnit)
}))

observeEvent( input$goButton,{
    geogList <- popPlace(DOLAPool)
    sellevel <- input$level
    selgeog <- input$geog
    OutPlot <- GenPlot(DBPool = DOLAPool, geogfips=geogList, geogname= selgeog, datlevel= sellevel)



output$LINE  <-  renderPlotly({OutPlot[["LINE"]]})
output$YOY   <-  renderPlotly({OutPlot[["YOY"]]})
output$BARCH <-  renderPlotly({OutPlot[["BARCH"]]})

# Add Download Image...

output$CHDATA=downloadHandler(
    filename= function(){
      paste0("Housing Unit Data ",selgeog,".csv")
    },
    content= function(file){
      write.csv(OutPlot[["CHDATA"]], file, row.names=FALSE)
    }
 )
})
}
