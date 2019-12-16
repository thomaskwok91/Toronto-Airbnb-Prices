setwd("C:/Users/thoma/Desktop/Spatial HW/Project/")
airbnb <- read.csv("Project-Airbnb_Toronto.csv")
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(GWmodel)
library(ModelMap)
library(gstat)
library(tidyverse)
library(ggplot2)
library(ggfittext)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
library(corrplot)

summary(airbnb)

#summary of prices
summary(airbnb$price)
quantile(airbnb$price, seq(0,1,by=0.1))
quantile(airbnb$price, seq(.9,1,by=0.01))

#histogram
hist(airbnb$price)

#Prices filtered
airbnb_filter <- airbnb %>%
  filter(price <=180)
Price <- airbnb_filter$price

#Under $180 per night
hist(Price)

#Neighborhood filters
airbnb_filter3 <- airbnb %>% 
  group_by(neighborhood) %>%
  filter(n() >= 100)

#ggplot of 9 most available neighborhood
ggplot(data=airbnb_filter3, aes(x=neighborhood)) +
  geom_bar() +
  theme_tufte()+
  coord_flip()

#comparing average cost of airbnb and neighborhood
price_lattice <- airbnb_filter3 %>%
  group_by(neighborhood) %>%
  summarise(Avg_Price=mean(price, na.rm = TRUE))
ggplot(data=price_lattice, aes(y=Avg_Price, x=neighborhood))+geom_bar(stat="identity")+coord_flip()

#using leaflet

#leaflet map
leaflet(airbnb) %>% 
  addTiles() %>%
  setView(-79.4, 43.7, zoom = 11) %>% 
  addHeatmap(lng = ~longitude, lat = ~latitude, intensity=~price,
             blur=20, max=1, radius = 10)

#heatmap of bedrooms
pal_bed <- colorBin("viridis", domain=airbnb$bedrooms, bins = 5, pretty = TRUE,
                na.color = "#808080", alpha = FALSE, reverse = TRUE,
                right = FALSE)

leaflet(data = airbnb) %>% 
  addProviderTiles("CartoDB.Positron") %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal_bed(bedrooms), weight = 1, radius=10, fillOpacity = 1, opacity = 1,
                   label = paste("Name:", airbnb$bedrooms)) %>% 
  addLegend("bottomright", pal = pal_bed, values = ~bedrooms,
            title = "Airbnb Bedrooms",
            opacity = 1
  )

#heatmap accommodates
pal_accommodates <- colorBin("viridis", domain=airbnb$accommodates, bins = 10, pretty = TRUE,
                    na.color = "#808080", alpha = FALSE, reverse = TRUE,
                    right = FALSE)

leaflet(data = airbnb) %>% 
  addProviderTiles("CartoDB.Positron") %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal_accommodates(accommodates), weight = 1, radius=10, fillOpacity = 1, opacity = 1,
                   label = paste("Name:", airbnb$accommodates)) %>% 
  addLegend("bottomright", pal = pal_accommodates, values = ~accommodates,
            title = "Airbnb Accommodates",
            opacity = 1
  )

#heatmap price
pal <- colorBin("viridis", domain=airbnb$price, bins = 12, pretty = TRUE,
                na.color = "#808080", alpha = FALSE, reverse = TRUE,
                right = FALSE)

leaflet(data = airbnb) %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal(price), weight = 1, radius=10, fillOpacity = 1, opacity = 1,
                   label = paste("Name:", airbnb$price)) %>% 
  addLegend("bottomright", pal = pal, values = ~price,
            title = "Airbnb Price",
            opacity = 1
  )

################### Price Categories

#mutate price
airbnb_data <- airbnb %>% 
  mutate(price_categories = cut(price,
                                breaks = c(0, 200, 400, 600, 2000),
                                labels = c('$0-199', '$200-399', '$400-599', '$600-2000'), 
                                include.lowest = TRUE, 
                                right = FALSE))

#plot price category by bedroom and accommodates
ggplot(data = subset(airbnb_data)) +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = bedrooms,
                 alpha = 0.01)) +
  scale_size(range = c(0,3)) + 
  scale_color_gradient(low = "red", high = "purple") +
  facet_wrap(vars(price_categories),
             nrow = 2, 
             ncol = 2) +
  labs(title = 'Toronto Area Airbnb Bedrooms by Prices', 
       x = 'Latitude', 
       y = 'Longitude',
       color = 'bedrooms') +
  guides(alpha = "none") + 
  theme_bw()

ggplot(data = subset(airbnb_data)) +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = accommodates,
                 alpha = 0.01)) +
  scale_size(range = c(0,3)) + 
  scale_color_gradient(low = "red", high = "purple") +
  facet_wrap(vars(price_categories),
             nrow = 2, 
             ncol = 2) +
  labs(title = 'Toronto Area Airbnb Accommodates by Prices', 
       x = 'Latitude', 
       y = 'Longitude',
       color = 'accommodates') +
  guides(alpha = "none") + 
  theme_bw()

ggplot(data = subset(airbnb_data)) +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = overall_satisfaction,
                 alpha = 0.01)) +
  scale_size(range = c(0,3)) + 
  scale_color_gradient(low = "red", high = "purple") +
  facet_wrap(vars(price_categories),
             nrow = 2, 
             ncol = 2) +
  labs(title = 'Toronto Area Airbnb Satisfaction by Prices', 
       x = 'Latitude', 
       y = 'Longitude',
       color = 'overall_satisfaction') +
  guides(alpha = "none") + 
  theme_bw()

#heatmap categories
grp1 <- airbnb_data %>%
  filter(price_categories == '$0-199')
grp2 <- airbnb_data %>%
  filter(price_categories == '$200-399')
grp3 <- airbnb_data %>%
  filter(price_categories == '$400-599')
grp4 <- airbnb_data %>%
  filter(price_categories == '$600-2000')

#grp1
pal1 <- colorBin("viridis", domain=grp1$price, bins = 4, pretty = TRUE,
                 na.color = "#808080", alpha = FALSE, reverse = TRUE,
                 right = FALSE)

leaflet(data = grp1) %>% 
  addProviderTiles("CartoDB.Positron") %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal1(price), weight = 1, radius=10, fillOpacity = 1, opacity = 1,
                   label = paste("Name:", grp1$price)) %>% 
  addLegend("bottomright", pal = pal1, values = ~price,
            title = "Airbnb Price",
            opacity = 1
  )

#grp2
pal2 <- colorBin("viridis", domain=grp2$price, bins = 4, pretty = TRUE,
                 na.color = "#808080", alpha = FALSE, reverse = TRUE,
                 right = FALSE)

leaflet(data = grp2) %>% 
  addProviderTiles("CartoDB.Positron") %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal2(price), weight = 1, radius=10, fillOpacity = 1, opacity = 1,
                   label = paste("Name:", grp2$price)) %>% 
  addLegend("bottomright", pal = pal2, values = ~price,
            title = "Airbnb Price",
            opacity = 1
  )

#grp3
pal3 <- colorBin("viridis", domain=grp3$price, bins = 4, pretty = TRUE,
                 na.color = "#808080", alpha = FALSE, reverse = TRUE,
                 right = FALSE)

leaflet(data = grp3) %>% 
  addProviderTiles("CartoDB.Positron") %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal3(price), weight = 1, radius=10, fillOpacity = 1, opacity = 1,
                   label = paste("Name:", grp3$price)) %>% 
  addLegend("bottomright", pal = pal3, values = ~price,
            title = "Airbnb Price",
            opacity = 1
  )

#grp4
pal4 <- colorBin("viridis", domain=grp4$price, bins = 4, pretty = TRUE,
                 na.color = "#808080", alpha = FALSE, reverse = TRUE,
                 right = FALSE)

leaflet(data = grp4) %>% 
  addProviderTiles("CartoDB.Positron") %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal4(price), weight = 1, radius=10, fillOpacity = 1, opacity = 1,
                   label = paste("Name:", grp4$price)) %>% 
  addLegend("bottomright", pal = pal4, values = ~price,
            title = "Airbnb Price",
            opacity = 1
  )

######################################################################3

#select only important info in airbnb
airbnb_df <- airbnb %>%
  dplyr::select(price, bedrooms, accommodates, reviews, overall_satisfaction, longitude, latitude)

#global regression = All dataset
global <- lm(price~., data=airbnb_df)
summary(global)

##model selection: See the importance of each variable and the model
library("GWmodel")
library(spgwr)
airbnb.spdf <- SpatialPointsDataFrame(airbnb[, 13:14], airbnb)

DeVar <- "price"
InDeVars <- c("bedrooms", "accommodates", "reviews", "overall_satisfaction", "longitude", "latitude")
model.sel <- model.selection.gwr(DeVar, InDeVars, data = airbnb.spdf,
                                 kernel = "bisquare", adaptive = TRUE, bw = 41)
sorted.models <- model.sort.gwr(model.sel, numVars = length(InDeVars),
                                ruler.vector = model.sel[[2]][,2])
model.list <- sorted.models[[1]]
model.view.gwr(DeVar, InDeVars, model.list = model.list)
plot(sorted.models[[2]][,2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection procedure",
     ylab = "AICc", xlab = "Model number", type = "b")

#correlation between variables of interest
cplot <- cor(airbnb_df) #this tells me bedroom and accommodates is biggest factor for price. Interesting overall satisfaction isn't big factor
corrplot(cplot, method="circle")

#Create distance matrix
library("GWmodel")
airbnb.spdf <- SpatialPointsDataFrame(airbnb[, 13:14], airbnb)
DM <- gw.dist(dp.locat = coordinates(airbnb.spdf))

#global regression = No spatial data, R-square .2856
global2 <- lm(price~bedrooms, data=airbnb.spdf)
summary(global2)

#gaussian (n=24) R-square .445
bw.gwr1 <- bw.gwr(price~bedrooms, data=airbnb.spdf, approach="AICc", kernel="gaussian", adaptive=TRUE)
bgwr.res <- gwr.basic(price~bedrooms, data=airbnb.spdf, bw=bw.gwr1, kernel="gaussian", adaptive=TRUE)
print(bgwr.res)

#bisquare (n=41), R-square .646
bw.gwr2 <- bw.gwr(price~bedrooms, data=airbnb.spdf, approach="AICc", kernel="bisquare", adaptive=TRUE)
bgwr.res2 <- gwr.basic(price~bedrooms, data=airbnb.spdf, bw=bw.gwr2, kernel="bisquare", adaptive=TRUE)
print(bgwr.res2)

#boxcar (n=19) R-square = .597
bw.gwr3 <- bw.gwr(price~bedrooms, data=airbnb.spdf, approach="AICc", kernel="boxcar", adaptive=TRUE)
bgwr.res3 <- gwr.basic(price~bedrooms, data=airbnb.spdf, bw=17, kernel="boxcar", adaptive=TRUE)
print(bgwr.res3)

#tri-cube (n=41) R-square = .635
bw.gwr5 <- bw.gwr(price~bedrooms, data=airbnb.spdf, approach="AICc", kernel="tricube", adaptive=TRUE)
bgwr.res5 <- gwr.basic(price~bedrooms, data=airbnb.spdf, bw=bw.gwr5, kernel="tricube", adaptive=TRUE)
print(bgwr.res5)

#Log-Price to deal with skew and remove negative prices = Bandwidth = 131 but R-square is .518
bw.gwr6 <- bw.gwr(log(price)~bedrooms, data=airbnb.spdf, approach="AICc", kernel="bisquare", adaptive=TRUE)
bgwr.res6 <- gwr.basic(log(price)~bedrooms, data=airbnb.spdf, bw=bw.gwr6, kernel="bisquare", adaptive=TRUE)
print(bgwr.res6)

###########################################################################3

#coeff between price and bedrooms
colours = c("green", "blue", "red", "yellow")
spplot(bgwr.res2$SDF, "bedrooms", cuts=quantile(bgwr.res2$SDF$bedrooms),
       col.regions=colours,
       main = "Basic GW regression coefficient estimates for price and bedroom")

#statistical significance
t = bgwr.res2$SDF$bedrooms / bgwr.res2$SDF$bedrooms_SE
sig.map = SpatialPointsDataFrame(airbnb.spdf, data.frame(t))
colours=c("green","red","green")
breaks=c(min(t),-4,4,max(t))
spplot(sig.map, cuts=breaks, col.regions=colours, main = "Statistical Significance of bedroom to Price")

############################################################################3

#log Price to combat negatives that don't make sense

#coeff between price and bedrooms
colours = c("green", "blue", "red", "yellow")
spplot(bgwr.res6$SDF, "bedrooms", cuts=quantile(bgwr.res6$SDF$bedrooms),
       col.regions=colours,
      main = "Basic GW regression coefficient estimates for log(price) and bedroom")

#statistical significance
t = bgwr.res6$SDF$bedrooms / bgwr.res6$SDF$bedrooms_SE
sig.map = SpatialPointsDataFrame(airbnb.spdf, data.frame(t))
colours=c("green","red","green")
breaks=c(min(t),-4
         ,4,max(t))
spplot(sig.map, cuts=breaks, col.regions=colours, main = "Statistical Significance of bedroom to log(Price)")
