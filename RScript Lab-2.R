#setup
library(tidyverse)
library(sf)
library(tmap)
library(stringr)

#aspatial data
bmps<- read_csv("./data/BMPreport2016_landbmps.csv")

#spatial data
counties <- sf::read_sf("./data/County_Boundaries/County_Boundaries.shp")%>%
  sf::st_make_valid()
dams <- sf::read_sf("./data/Dam_or_Other_Blockage_Removed/Dam_or_Other_Blockage_Removed_2012_2017.shp")%>%
  sf::st_make_valid()
streams <- sf::read_sf("./data/Streams_Opened_by_Dam_Removal/Streams_Opened_by_Dam_Removal_2012_2017.shp")%>%
  sf::st_make_valid()

#data stuff
glimpse(bmps)

bmps <- bmps %>% mutate(., FIPS.trimmed = stringr::str_sub(GeographyName, 1, 5))

#Task 1
  #1.1
    #question - how to add two summary rows? cost by bmp type and total cost of all bmp types
summary(bmps$Cost)

st.bmpcost <- bmps%>%
  group_by(StateAbbreviation, BMPType) %>%
  summarise(BMPType.Cost = sum(Cost, na.rm = T)) %>%
  ungroup(BMPType) %>%
  summarise(Total.Cost = sum(Cost, na.rm=T))
#not working - 
  

print(st.bmpcost, n=21)

  #1.2


#Task2
  #2.1
glimpse(streams)


  