#setup
library(tidyverse)
library(sf)
library(tmap)
library(stringr)

bmps <- read.csv("./BMPreport2016.csv")

counties <- sf::read_sf("./data/County_Boundaries/County_Boundaries.shp")%>%
  sf::st_make_valid()
dams <- sf::read_sf("./data/Dam_or_Other_Blockage_Removed/Dam_or_Other_Blockage_Removed_2012_2017.shp")%>%
  sf::st_make_valid()
streams <- sf::read_sf("./data/Streams_Opened_by_Dam_Removal/Streams_Opened_by_Dam_Removal_2012_2017.shp")%>%
  sf::st_make_valid()

bmps <- bmps %>% mutate(., FIPS.trimmed = stringr::str_sub(GeographyName, 1, 5))

#1.1

summary_stats <- bmps %>%
  group_by(StateAbbreviation) %>%
  summarise(
    Mean_Cost = mean(Cost, na.rm = TRUE),
    Median_Cost = median(Cost, na.rm = TRUE),
    SD_Cost = sd(Cost, na.rm = TRUE),
    Min_Cost = min(Cost, na.rm = TRUE),
    Max_Cost = max(Cost, na.rm = TRUE)
  )

print(summary_stats)+
  
 
#1.2 - i may have subsetted this TOO much

bmps_filtered_acres <- bmps %>%
  filter(Unit == "Acres") %>%  
  filter(!is.na(Cost)) %>%     
  filter(Cost > 10 & Cost < 10000 ) %>%
  filter(!is.na(TotalAmountCredited)) %>%
  filter(TotalAmountCredited > 1500)

bmps_filtered_acres %>%
  ggplot(mapping = aes(x = Cost, y = TotalAmountCredited)) +
  geom_point(na.rm = TRUE)

#1.3 - Make a boxplot with “StateAbbreviation” on the x-axis and “TotalAmountCredited” on the y-axis.
#HOWEVER, the only data I want plotted are for cover crop BMPs. Note, there are many types of cover
#crops in this dataset, and I want you to include them ALL. There are handy functions within the stringr
#package that can help you here. Use the help function ?stringr or read the documentations/vignettes found
#online. A Google search for “r stringr vignettes documentation” would be a good place to start if you’re
#having trouble.

glimpse(bmps)

cover_crop_bmps <- bmps %>%
  filter(str_detect(BMP, regex("cover crop", ignore_case = TRUE)))

ggplot(cover_crop_bmps, aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation)) +
  scale_y_log10() +
  labs(y = "log10(TotalAmountCredited)") 


#1.4 - make a scatterplot of the dam dataset, this time with “YEAR” on the x-axis and “STATE” on y-axis
#(think of it like a timeline). Assume no dams were built in year 0, so you’ll need to remove those data points.
#You know how to remove observations based on a given criteria.

dams_filtered <- dams %>%
  filter(YEAR != 0)

ggplot(dams_filtered, aes(x = YEAR, y = STATE)) +
  geom_point() 

#1.5 -  make one last (aspatial) visualization. But this time, it’s your choice what data and plots to use. The
#only requirement is that you link two of the datasets together in some manner. Be creative. Make it look
#nice (e.g., use proper labels, interesting colors/shading/size).

streams_counties <- st_join(streams, counties, join = st_within)

streams_counties <- streams_counties %>%
  mutate(stream_length = as.numeric(st_length(geometry)))

streams_counties_cleaned <- streams_counties %>%
  filter(!is.na(stream_length) & !is.na(STATEFP10) & !is.na(COUNTYFP10))

county_stream_length <- streams_counties_cleaned %>%
  group_by(STATEFP10, COUNTYFP10) %>%  
  summarize(total_length = sum(stream_length, na.rm = TRUE)) %>%
  st_drop_geometry() 

state_county_stream_length <- county_stream_length %>%
  group_by(STATEFP10, COUNTYFP10) %>%
  summarize(total_length = sum(total_length, na.rm = TRUE))

ggplot(state_county_stream_length, aes(x = STATEFP10, y = total_length, fill = COUNTYFP10)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Stream Length by State (Stacked by County FP)",
       x = "State FP",
       y = "Total Stream Length",
       fill = "County FP")   

#2.1 - Find the 5 longest streams in the ‘streams opened by dam removal’ dataset

streams <- streams %>%
  mutate(Length = as.numeric(st_length(geometry)))  

longest_streams <- streams %>%
  arrange(desc(Length)) %>%  
  slice(1:5) 

print(longest_streams)

#2.2 - Find the three counties with the greatest TOTAL length of streams (opened by dam removal) in them

county_stream_length2.0 <- streams_counties_cleaned %>%
  group_by(COUNTYFP10) %>%
  summarize(total_length = sum(stream_length, na.rm = TRUE)) %>%
  st_drop_geometry()

top_counties <- county_stream_length2.0 %>%
  arrange(desc(total_length)) %>%
  slice(1:3)

print(top_counties)

#2.3 - Make a map of the counties, shading each county by the total cost of BMPs funded/implemented in that
#county. This will require you to join multiple datasets together

bmps_counties <- bmps %>%
  left_join(counties, by = c("FIPS.trimmed" = "GEOID10"))

county_bmp_cost <- bmps_counties %>%
  group_by(FIPS.trimmed) %>%
  summarize(total_cost = sum(Cost, na.rm = TRUE))

counties_with_cost <- counties_with_cost %>%
  filter(total_cost > 0)

counties_with_cost <- counties %>%
  left_join(county_bmp_cost, by = c("GEOID10" = "FIPS.trimmed"))

tm_shape(counties_with_cost) + tm_polygons("total_cost")

#2.4

closest_stream_pt1 <- st_nearest_feature(dams, streams)

closest_streams <- streams[closest_stream_pt1, ]

distances <- st_distance(dams, closest_streams, by_element = TRUE)

dams_with_closest_streams <- dams %>%
  mutate(closest_stream = closest_streams$OBJECTID_1,
         distance_to_stream = distances)

head(dams_with_closest_streams)

#2.5 - Calculate how many removed dams are (or were) in each state

dams_by_state <- dams %>%
  group_by(STATE) %>%  
  summarize(num_dams = n())

print(dams_by_state)
