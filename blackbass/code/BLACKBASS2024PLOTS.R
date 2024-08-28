leaflet() %>%
  # Base group
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
addCircles(lng = station_coords$Longitude, 
           lat = station_coords$Latitude, 
           fill=TRUE, color="white", weight=10) 

lg22 <- filtered_df2 %>% 
  subset(species == "lg22")

leaflet() %>%
  # Base group
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addCircles(lng = station_coords$Longitude, 
             lat = station_coords$Latitude, 
             fill=TRUE, color="white", weight=10) %>%
addCircleMarkers(lng= lg22$long, lat= lg22$lat, weight= 2, radius= 4, color= "red", stroke= FALSE, fillOpacity = 1, group = "lg22")

st_count <- filtered_df2 %>% count(Station.Name)
st_count %>% plot(st_count$Station.Name, st_count$n)

library(ggplot2)

st_count <- filtered_df2 %>% count(Station.Name)

# Plotting with ggplot2
ggplot(st_count, aes(x = Station.Name, y = n, group=1)) +
  geom_point(stat = "identity") +
  xlab("Station Name") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotates x-axis labels for better readability
  geom_line()

filtered_df2 <- filtered_df2 %>% 
  arrange(species, Station.Name, datetimeEST)

data.frame(station_coords) %>%
  ggplot(mapping = aes(x = Longitude, y = Latitude)) + 
  xlab("Longitude") + ylab("Latitude") +
  geom_point()

##############################################################
################residence per species#########################
# Replace 'species_name' with the actual species name you're interested in
lf5 <- "lf5"

lf5_df <- filtered_df2 %>%
  filter(species == lf5)

# Calculate the mean residence time per station for the specific species
mean_residence_lf5 <- lf5_df %>%
  group_by(Station.Name) %>%
  summarise(Mean_Residence_Time = mean(Time_Diff, na.rm = TRUE))

# Create a barplot for the mean residence time for the specific species
barplot(mean_residence_lf5$Mean_Residence_Time,
        names.arg = mean_residence_lf5$Station.Name,
        las = 2,  # Rotate the station names for better readability
        cex.names = 0.7,  # Adjust the size of the station names
        main = paste("Mean Residence Time per Station for", lf5),
        xlab = "Station Name",
        ylab = "Mean Residence Time (hours)")
######################################################################
############detection count per species##############
detection_count_lg1 <- lg1_df %>%
  group_by(Station.Name) %>%
  summarise(Number_of_Detections = n())

barplot(detection_count_lg1$Number_of_Detections,
        names.arg = detection_count_lg1$Station.Name,
        las = 2,  # Rotate the station names for better readability
        cex.names = 0.7,  # Adjust the size of the station names
        main = paste("Number of Detections per Station for", lg1),
        xlab = "Station Name",
        ylab = "Number of Detections")
#####################################################################
###############BOXPLOTS##############################################

# Count the number of detections per station for each species
detection_count_per_species_station <- filtered_df2 %>%
  group_by(species, Station.Name) %>%
  summarise(Number_of_Detections = n()) %>%
  ungroup()

# Create boxplots for the number of detections for each species
boxplot(Number_of_Detections ~ species, 
        data = detection_count_per_species_station,
        main = "Boxplot of Number of Detections per Species",
        xlab = "Species",
        ylab = "Number of Detections",
        las = 2,  # Rotate species labels for better readability
        cex.axis = 0.7)  # Adjust axis label size
#######################################################################
#DAILY DETECTIONS###
filtered_df2 %>%
  group_by(species, date) %>%
  summarise(DailyDetections = n()) %>%
  ggplot(mapping= aes(x= species, y= DailyDetections)) +
  xlab("Species") + ylab("No of detections/day") +
  geom_boxplot()

# PLOTS ACROSS TIME AND RECEIVER
lg1_df %>% ggplot(mapping= aes(x= datetimeEST, y= as.factor(Station.Name))) +
  xlab("date") + ylab("receiver") + geom_point() + scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d")

#use ggmap function to overlay receiver sites on a google map

register_google(key='AIzaSyA3i5Enp3gOTjgh8XzqxlcO6zeIXiOxHJo')
pll_bbox <- make_bbox(lon= filtered_df2$long, lat= filtered_df2$lat, f=0.4)
pll_map <- get_map(location = pll_bbox, source= "google", maptype = "satellite")
ggmap(pll_map) + geom_point(data= data.frame(station_coords), mapping= aes(x= Longitude, y= Latitude), colour= "red") + xlab("Longitude") + ylab("Latitude")

ggmap(pll_map) +
  geom_point(data = station_coords, 
             mapping = aes(x = Longitude, y = Latitude), 
             colour = "red") +
  geom_text(data = station_coords, 
            mapping = aes(x = Longitude, y = Latitude, label = Station.Name), 
            vjust = -1,  # Adjust vertical position of the text
            hjust = 0.5,  # Adjust horizontal position of the text
            size = 2,     # Size of the text
            colour = "red") +  # Color of the text
  xlab("Longitude") + 
  ylab("Latitude")

#############################LEAFLET PLOTS#####################################################################

the_df <- filtered_df2 %>% #remove NAs from latlong columns in filtered_df2 to create a new dataframe called the_df
  filter(!is.na(lat) & !is.na(long)) 

coordinates(the_df) <- c("long", "lat") # convert the lat-longs to spatial objects

lg23 <- the_df %>% subset(species == "lg23") %>% remove.duplicates() 

baia_map %>%  #add markers to the circles in which the tagged fish was recorded 
  addCircleMarkers(lng= lg23$long, lat= lg23$lat, weight=2, radius=4, color="red", stroke=FALSE, fillOpacity = 1, group= "lg23") %>%
  addLayersControl(baseGroups = c("Satellite", "Map"), overlayGroups = c("lg23"), options = layersControlOptions(collapsed= FALSE))

#################################################################################################################

COA(lg23)
setupData(filtered_df2, fishtags, station_coords, source = "VEMCO", tz = "Australia/Brisbane")
