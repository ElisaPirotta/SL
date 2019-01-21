## ----
#
## ----

library(maps)
library(leaflet)
library(dplyr)
library(htmltools)

# ---- world map 

worldCities <- world.cities[, c(-3)]
capitalCities <- worldCities[which(worldCities$capital == 1),]
capitalCities <- capitalCities[-c(180,137),] # duplicate capital cities 

helped <- capitalCities[c(4,144,60,163,112,6,132,64,90,13,85,40,103,39,115,156),]
notYetHelped <- capitalCities[c(118,110,80,134,84,137,35,44,143,18,127,78),] 
helped2 <- helped[,-1]
notYetHelped2 <- notYetHelped[,-1]


helped2$label <- rep("NA", length(nrow(helped2)))
helped2$label[1] <- "We planted trees in here!"
helped2$label[2] <- "We planted trees in here!"
helped2$label[3] <- "We planted trees in here!"
helped2$label[4] <- "We planted trees in here!"
helped2$label[5] <- "We planted trees in here!"
helped2$label[6] <- "We planted trees in here!"
helped2$label[7] <- "We planted trees in here!"
helped2$label[8] <- "We planted trees in here!"
helped2$label[9] <- "We planted trees in here!"
helped2$label[10] <- "We planted trees in here!"
helped2$label[11] <- "We planted trees in here!"
helped2$label[12] <- "We planted trees in here!"
helped2$label[13] <- "We planted trees in here!"
helped2$label[14] <- "We planted trees in here!"
helped2$label[15] <- "We planted trees in here!"
helped2$label[16] <- "We planted trees in here!"


helped2$pop <- rep("NA", length(nrow(helped2)))
helped2$pop[1] <- paste(sep = "<br/>",
                          "<b>262.652 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/BoTnTWnjs6K/'>Gahna</a>")
helped2$pop[2] <- paste(sep = "<br/>",
                          "<b>1.736.880 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/BqW_hH7AjV1/'>Burkina Faso</a>")

helped2$pop[3] <- paste(sep = "<br/>",
                          "<b>73.833 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/BqNA_Vegbd6/'>Senegal</a>")

helped2$pop[4] <- paste(sep = "<br/>",
                          "<b>248.777 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/BYv_wrAnQcD/'>Morocco</a>")

helped2$pop[5] <- paste(sep = "<br/>",
                          "<b>27.000 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/Bqcv5uoFPmp/'>Spain</a>")

helped2$pop[6] <- paste(sep = "<br/>",
                          "<b>574.351 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/BhoIzeSDMUF/'>Ethiopia</a>")

helped2$pop[7] <- paste(sep = "<br/>",
                          "<b>288.989 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/BoTnTWnjs6K/'>Kenya</a>")

helped2$pop[8] <- paste(sep = "<br/>",
                          "<b>915.414 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/Bmkr2GKD8YU/'>Tanzania</a>")

helped2$pop[9] <- paste(sep = "<br/>",
                          "<b>156.531 trees planted in</b>",
                          "<a href='https://www.instagram.com/p/Bh_f2g3AtmI/'>Uganda</a>")

helped2$pop[10] <- paste(sep = "<br/>",
                           "<b>1.026.951 trees planted in</b>",
                           "<a href='https://www.instagram.com/p/Bm5GpYUjUlg/'>Madagascar</a>")

helped2$pop[11] <- paste(sep = "<br/>",
                           "<b>222.595 trees planted in</b>",
                           "<a href='https://www.instagram.com/p/Bq-GZLcA9eW/'>Indonesia</a>")

helped2$pop[12] <- paste(sep = "<br/>",
                           "<b>789.345 trees planted in</b>",
                           "<a href='https://www.instagram.com/p/Br-AVVAD2mc/'>Brazil</a>")

helped2$pop[13] <- paste(sep = "<br/>",
                           "<b>361.717 trees planted in</b>",
                           "<a href='https://www.instagram.com/p/BMGj_F0geP3//'>Peru</a>")

helped2$pop[14] <- paste(sep = "<br/>",
                           "<b>437.473 trees planted in</b>",
                           "<a>Colombia</a>")

helped2$pop[15] <- paste(sep = "<br/>",
                           "<b>192.231 trees planted in</b>",
                           "<a href='https://blog.ecosia.org/tag/nicaragua/'>Nicaragua</a>")

helped2$pop[16] <- paste(sep = "<br/>",
                           "<b>72.157 trees planted in</b>",
                           "<a>Haiti</a>")


notYetHelped2$col <- rep("NA", length(nrow(notYetHelped2)))
notYetHelped2$col[1] <- "#99CC66"
notYetHelped2$col[2] <- "#99CC66"
notYetHelped2$col[3] <- "#99CC66"
notYetHelped2$col[4] <- "#66CC00"
notYetHelped2$col[5] <- "#66CC00"
notYetHelped2$col[6] <- "#99CC66"
notYetHelped2$col[7] <- "#00CC00"
notYetHelped2$col[8] <- "#00CC00"
notYetHelped2$col[9] <- "#33CC33"
notYetHelped2$col[10] <- "#33CC33"
notYetHelped2$col[11] <- "#33CC33"
notYetHelped2$col[12] <- "#66CC00"

notYetHelped2$popu <- rep("NA", length(nrow(notYetHelped2)))
notYetHelped2$popu[1] <- paste(sep = "<br/>",
                               "<b>We planned to plant trees here in the near future!</b>",
                               "<a>Mozambique</a>")
notYetHelped2$popu[2] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>Zambia</a>")
notYetHelped2$popu[3] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>Zimbawe</a>")
notYetHelped2$popu[4] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>India</a>")
notYetHelped2$popu[5] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>Pakistan</a>")
notYetHelped2$popu[6] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>Mauritania</a>")
notYetHelped2$popu[7] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>Germany</a>")
notYetHelped2$popu[8] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>Belgium</a>")
notYetHelped2$popu[9] <-paste(sep = "<br/>",
                                "<b>We planned to plant trees here in the near future!</b>",
                                "<a>Canada</a>")
notYetHelped2$popu[10] <-paste(sep = "<br/>",
                                 "<b>We planned to plant trees here in the near future!</b>",
                                 "<a>Paraguay</a>")
notYetHelped2$popu[11] <-paste(sep = "<br/>",
                                 "<b>We planned to plant trees here in the near future!</b>",
                                 "<a>Uruguay</a>")
notYetHelped2$popu[12] <-paste(sep = "<br/>",
                                 "<b>We planned to plant trees here in the near future!</b>",
                                 "<a>Vietnam</a>")


mapworld = map("world", fill = T, plot = F)

greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 35, iconHeight = 58,
  iconAnchorX = 20, iconAnchorY = 55
)


leaflet(helped2) %>%
  fitBounds(-140, -3, 160, 70) %>%
  addPolygons(data = mapworld , stroke = FALSE, fillCol = "#FFFFFF") %>% 
  
  addTiles(group = "Default") %>% 
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$Stamen.Watercolor, group = "Water Color") %>%
  
  
  addMarkers(~long, ~lat, group = "Helped Countries", icon = greenLeafIcon, popup = ~pop, label = ~label, 
             labelOptions = labelOptions(noHide = F, style = list("box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                  "font-size" = "12px",
                                                                  "border-color" = "rgba(0,0,0,0.3)"))) %>%
             
  addCircleMarkers(~long, ~lat, group = "To be helped Countries", data = notYetHelped2, color = ~col, popup = ~popu) %>%
  
  addLayersControl(
    baseGroups = c("Default", "Terrain", "Toner Lite", "Water Color"),
    overlayGroups = c("Helped Countries", "To Be Helped Countries"),
    options = layersControlOptions(collapsed = FALSE)
  )


## ----
# Shiny
## ----

library(shiny)

ui <- fluidPage(
  leafletOutput("mymap", height = 1000)
)


server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    mapworld = map("world", fill = T, plot = F)
    leaflet(helped2) %>%
      #fitBounds(-140, -3, 160, 70) %>%
      addPolygons(data = mapworld , stroke = FALSE, fillCol = "#FFFFFF") %>% 
      
      addTiles(group = "Default") %>% 
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addProviderTiles(providers$Stamen.Watercolor, group = "Water Color") %>%
      
      
      addMarkers(~long, ~lat, group = "Helped Countries", icon = greenLeafIcon, popup = ~pop, label = ~label, 
                 labelOptions = labelOptions(noHide = F, style = list("box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                      "font-size" = "12px",
                                                                      "border-color" = "rgba(0,0,0,0.3)"))) %>%
      
      addCircleMarkers(~long, ~lat, group = "To be helped Countries", data = notYetHelped2, color = ~col, popup = ~popu) %>%
      
      addLayersControl(
        baseGroups = c("Default", "Terrain", "Toner Lite", "Water Color"),
        overlayGroups = c("Helped Countries", "To Be Helped Countries"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
}

shinyApp(ui, server)

