
##################all india - if I have the processing power


###trying something new - make all years before 2006 -1 if they are lit and 1 if they are lit, then sum them.
##result gives an indication of whether or not the area has got brighter or darker since construction

# Create a raster brick representing the years 1992-2005
pre_2006 <- All_deblur_india[[1:14]]

# Create a raster brick representing the years 2006-2013
post_2006 <- All_deblur_india[[15:21]]

# Reclassify 0 to -1 in each layer of the pre_2006 raster brick
pre_2006_reclass <- calc(pre_2006, function(x) {
  ifelse(x == 1, -1, x)
}, return = "RasterBrick")

beep(4)
summary(values(pre_2006_reclass))
# Sum the cells in each layer of the reclassified pre_2006 raster brick
pre_2006_sum <- calc(pre_2006_reclass, sum)
summary(values(pre_2006_sum))
# Sum the cells in each layer of the reclassified pre_2006 raster brick
post_2006_sum <- calc(post_2006, sum)

summary(values(pre_2006_sum))



# Create a color palette
pal2 <- colorNumeric(palette = "viridis", domain = c(0, 1), n = 100)

#bright areas got bright  
m <- leaflet() %>% 
  
  setView(lng = 78.33545 , lat = 30.35355, zoom = 10)  %>% 
  addTiles() %>% 
  addRasterImage(post_2006_sum, colors =  pal2, opacity = .51) %>%
  addPolygons(data = areas15km_india_NJ, color = "#253494", fillOpacity = 0, weight = 3)  

m
post_2006_sum

# Normalize the values in post_2006_sum to the 0-1 range
post_2006_norm <- post_2006_sum
post_2006_norm[post_2006_norm != 0] <- scale(post_2006_norm[post_2006_norm != 0], 
                                             center = min(post_2006_norm[post_2006_norm != 0]), 
                                             scale = max(post_2006_norm[post_2006_norm != 0]))


summary(values(post_2006_norm))
pre_2006_sum
# Normalize the values in pre_2006_sum to the 0-1 range
# Normalize values of pre_2006_sum raster layer between 0 and 1
# Get the minimum and maximum values, ignoring the 0s
# Extract the minimum value of a raster object ignoring zeros
pre_2006_norm <- pre_2006_sum 

pre_2006_min <- min(pre_2006_sum[pre_2006_sum != 0])
pre_2006_max <- max(pre_2006_sum[pre_2006_norm != 0])

pre_2006_norm[pre_2006_norm != 0] <- scale(pre_2006_norm[pre_2006_norm != 0], 
                                           center = pre_2006_max, 
                                           scale = pre_2006_min)

#is the change postive or negative after construction?
difference_Tehri <- post_2006_norm - pre_2006_norm
summary(values(difference_Tehri))

pal2 <- colorNumeric(palette = c("orange", "purple"), domain = c(min(values(difference_Tehri)), max(values(difference_Tehri))))

# Define the color palette for negative values (black to white)
pal_neg <- colorNumeric(palette = c("#000000", "grey"), domain = c(-1, 0), na.color = "transparent")

# Define the color palette for positive values (neon green to neon blue)
pal_pos <- colorNumeric(palette = c("#FFFFFF", "#0000FF"), domain = c(0, 1), na.color = "transparent")

# Combine the two palettes into a single palette for the entire range
pal2 <- colorNumeric(palette = c("#000000", "grey","white", "#31f7f6" ), domain = c(-1, 0, 0, 1), na.color = "transparent")

difference_Tehri[difference_Tehri == 0] <- NA


# Define the color palette for negative values (black to white)
pal_neg <- colorNumeric(palette = c("#000000", "grey"), domain = c(-1, 0), na.color = "transparent")

# Define the color palette for positive values (neon green to neon blue)
pal_pos <- colorNumeric(palette = c("#FFFFFF", "#0000FF"), domain = c(0, 1), na.color = "transparent")

# Combine the two palettes into a single palette for the entire range
pal2 <- colorNumeric(palette = c("#000000", "grey", "white", "#31f7f6"), domain = c(-1, 0, 0, 1), na.color = "transparent")

# Define a separate palette for the legend
pal3 <- colorNumeric(palette = c("#000000", "grey", "white", "#31f7f6"), domain = c(-1, -0.5, 0.5, 1))


m <- leaflet() %>% 
  setView(lng = 78.33545, lat = 30.35355, zoom = 10) %>% 
  addTiles() %>% 
  addRasterImage(difference_Tehri, colors = pal2, opacity = 0.7) %>%
  addPolygons(data = areas15km_india_NJ, color = "#253494", fillOpacity = 0, weight = 3)  %>%
  addControl(
    html = tags$div(
      style = "background-color: rgba(255, 255, 255, 0.5); padding: 1px; font-family: Calibri",
      tags$div(
        style = "display: flex; align-items: center; color: black; font-size: 20px;",
        tags$div(
          style = "display: flex; align-items: center; color: black; font-size: 15px; margin-left: 0px;margin-bottom: 5px;",
          "more activity"
        )
      ),
      tags$div(
        style = "display: flex; align-items: center; color: black; font-size: 20px;",
        tags$div(
          style = "width: 20px; height: 30px; background: linear-gradient(0deg, white, #31f7f6); margin-left: 30px;"
        ),
        ""
      ),
      tags$div(
        style = "display: flex; align-items: center; color: black; font-size: 14px; margin-left: 5px; margin-top: 1px; margin-bottom: 1px;margin-left: 36px",
        "\u2195   2006"
      ),
      tags$div(
        style = "display: flex; align-items: center; color: #31f7f6;",
        tags$div(
          style = "width: 20px; height: 30px; background: linear-gradient(0deg,#000000,grey); margin-left: 30px;"
        ),
        ""
      ),
      tags$div(
        style = "display: flex; align-items: center; color: black; font-size: 15px; margin-left: 0px; margin-top: 5px;",
        "less activity"
      )
    ),
    position = "bottomright"
  ) %>%
  addControl(
    html = tags$div(
      style = "background-color: rgba(255, 255, 255, 0.5); padding: 1px; font-family: Calibri",
      tags$div(
        style = "color: black; font-size: 15px; margin-bottom: 0px; text-align: left",
        htmltools::HTML("Socio-economic activity <br> comparison before and <br> after Tehri construction")
      ),
      position = "topright"
    ))  

m