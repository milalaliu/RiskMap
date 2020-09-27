library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(jsonlite)
library(viridis)

shinyServer(function(input, output) {
  
  df_history <- fromJSON("data/history.json")
  df_history$city = substr(df_history$belongCity,1,3)
  tp_history = df_history[df_history$city=='臺北市',]
  tp_history_df = tp_history[,c("caseId","caseName","belongAddress","belongCity","longitude","latitude")]
  tp_history_df$risk <- as.factor(sample(1:10, nrow(tp_history_df), replace=T))
  tp_history_df$warter_risk <- as.factor(sample(1:10, nrow(tp_history_df), replace=T))
  tp_history_df$heat_risk <- 10-as.numeric(tp_history_df$warter_risk)
  tp_history_df$heat_risk=as.factor(tp_history_df$heat_risk)
  tp_history_df$color = heat.colors(10)[tp_history_df$risk]
  
  # new column for the popup label
  
  bb_data <- mutate(tp_history_df, cntnt=paste0('<strong>名稱: </strong>',caseName,
                                                '<br><strong>所在地理區域:</strong> ', belongCity,
                                                '<br><strong>地址或位置:</strong> ', belongAddress,
                                                '<br><strong>經度:</strong> ',longitude,
                                                '<br><strong>緯度:</strong> ',latitude,
                                                '<br><strong>天災風險值:</strong> ',risk,
                                                '<br><strong>水災害風險值:</strong> ',warter_risk,
                                                '<br><strong>溫度災害風險值:</strong> ',heat_risk)) 
  
  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(palette = viridis(10) ,bb_data$risk)
  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) %>% 
      addTiles() %>%
      setView(lng = 121.51111, lat = 25.04001, zoom = 15) %>% 
      #addCircles(lng = ~longitude, lat = ~latitude,color = ~color) %>% 
      addCircleMarkers(#data = bb_data, lat =  ~longitude, lng =~latitude, 
        radius = 8, popup = ~as.character(cntnt), 
        color = ~pal(risk),
        stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=bb_data$risk,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  
  
  
})