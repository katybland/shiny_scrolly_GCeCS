
# make pacfin public data map by port group 2010-2020

rm(list = ls())
setwd("~/Documents/GitHub/shiny_scrolly_GCeCS")



options(scipen = 999)  #to not use scientific notation

library(tidyverse)
library(sf)
library(spData)
library(leaflet)
library(DT)
library(gganimate)
library(gifski)
library(showtext)
library(RColorBrewer)
# font_add_google(name = "STIX Two Math", family = "STIX Two Math")



# read in state/port files from PACFIN ---------------------------------
WA <- read.csv("data/pacfin_public/ALL005-W-2010---2020.csv") %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB")
OR <- read.csv("data/pacfin_public/ALL005-O-2010---2020.csv") %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB")
CA <- read.csv("data/pacfin_public/ALL005-C-2010---2020.csv") %>% 
  filter(PACFIN_SPECIES_CODE == "DCRB")



# CA Coordinates -- (From Fisher et al. 2021) -------------------------------------
# the latitude is an average of all port of landing locations within a port group
#   the longitude was set manually to be a point along the coast at the given latitude
# manually added in OR, WA, and SoCA port group lat/long from google maps approximation
pcgroup_coords <- read.csv('data/pcgroup_mean_coordinates.csv')
colnames(pcgroup_coords)

  


# remove unnecessary columns
WA <- WA[-c(3,4,5,7,8,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28,29,30)]
OR <- OR[c(1,2,6,9,13,17,21,25,29,33)]
CA <- CA[c(1,2,6,9,13,17,21,25,29,33,37,41,45,49)]


#make in tidy format
WA <- WA %>%
  pivot_longer(!c(1,2,3), names_to = "port", values_to = "exvessel_revenue")
OR <- OR %>%
  pivot_longer(!c(1,2,3), names_to = "port", values_to = "exvessel_revenue")
CA <- CA %>%
  pivot_longer(!c(1,2,3), names_to = "port", values_to = "exvessel_revenue")


#bind datasets
all <- bind_rows(WA, OR)
all <- bind_rows(all, CA)


#convert ports to factors
all$port <- as.factor(all$port)


#take off unneccessary part of port name
levels(all$port) <- gsub('_EXVESSEL_REVENUE', '', levels(all$port))


#add in coordinates
all_spatial <- left_join(all, pcgroup_coords, by = c("port" = "port_group"))


# ggplot() +
#   geom_bar(data = all, aes(x = as.factor(LANDING_YEAR), y = exvessel_revenue, fill = port), stat = "identity") +
#   facet_wrap(~AGENCY_CODE)

#### Map transition across years into different plots for scrolly #####

#get basemap 
wc_geom <- us_states %>% 
  filter(NAME %in% c("Washington", "Oregon", "California"))


bb <- seq(from = 0, to = 40000000, length.out = 11)  #define size breaks
ll <- c("$0", 
        "$3 million", 
        "$6 million", 
        "$9 million", 
        "$12 million",
        "$15 million",
        "$18 million",
        "$21 million",
        "$24 million",
        "$27 million",
        "$30 million")

map_diff_years <- function(year) {
  scale = 1
  map <- ggplot() +
    geom_sf(data = wc_geom, fill = "grey",
            # color = "transparent", 
            alpha = 0.45) +
    geom_point(data = filter(all_spatial, LANDING_YEAR == year), 
               aes(x = Lon, y = Lat, 
                   size = exvessel_revenue, color = exvessel_revenue,
                   text = glue::glue('<span style = "font-size:1.5em">{port_group_name}</span>
                                                <i>Exvessel revenue</i>: ${exvessel_revenue}')),
               alpha = .9) +
    scale_size_continuous(range = c(1, 25),breaks = bb,labels = ll,
                          limits=c(0, 40000000),
                          guide = guide_legend(reverse = TRUE) ) +
    scale_colour_gradientn( breaks = bb,labels = ll,
                           colours = c("#ef8354", "#ccb7ae", "#565264")
                             # myPalette(100)
                           , limits=c(0, 40000000),
                           guide = guide_legend(reverse = TRUE) 
                            ) +
    labs(size = "Exvessel Revenue",
         color = "Exvessel Revenue"
         # subtitle = year,
         # caption = "*port landings representing <4 vessels in a year are not shown for confidentiality purposes"
         ) +
    theme_void() 
# theme(
    #   text = element_text(family = "STIX Two Math", size = 22*scale), 
    #   plot.title = element_text(size = 26*scale, hjust = 0.5),
    #   plot.subtitle = element_text(size = 14*scale, hjust = 0.5),
    #   plot.caption = element_text(size = 12*scale),
    #   legend.text = element_text(size = 16*scale))
  return(map)
}


# # make different maps for different years
# map2010 <- map_diff_years(2010)
# map2011 <- map_diff_years(2011)
# map2012 <- map_diff_years(2012)
# map2013 <- map_diff_years(2013)
# map2014 <- map_diff_years(2014)
# map2015 <- map_diff_years(2015)
# map2016 <- map_diff_years(2016)
# map2017 <- map_diff_years(2017)
# map2018 <- map_diff_years(2018)
# map2019 <- map_diff_years(2019)
# map2020 <- map_diff_years(2020)

# Convert into ggplotly
# ggplotly(map2010)
# 
# ggplotly(map2010, tooltip = 'text') %>%
#   layout(
#     x.axis = element_blank(),
#     # legend = list(x = 0.25, y = 0.525),
#     # font = list(family = 'Lato'),
#     margin = list(t=50),
#     autosize=F,margin=list(l=50,r=50,b=50,t=50,pad=5))


# map2010 
# map2011 
# map2012 
# map2013 
# map2014 
# map2015 
# map2016 
# map2017 
# map2018
# map2019
# map2020 

# ggsave("images/map2010.jpg", plot = map2010)
# ggsave("images/map2011.jpg", plot = map2011)
# ggsave("images/map2012.jpg", plot = map2012)
# ggsave("images/map2013.jpg", plot = map2013)
# ggsave("images/map2014.jpg", plot = map2014)
# ggsave("images/map2015.jpg", plot = map2015)
# ggsave("images/map2016.jpg", plot = map2016)
# ggsave("images/map2017.jpg", plot = map2017)
# ggsave("images/map2018.jpg", plot = map2018)
# ggsave("images/map2019.jpg", plot = map2019)
# ggsave("images/map2020.jpg", plot = map2020)


# plots <- ggplot() +
#   geom_sf(data = wc_geom, fill = "grey",
#           # color = "transparent", 
#           alpha = 0.45) +
#   geom_point(data = all_spatial, 
#              aes(x = Lon, y = Lat, size = exvessel_revenue),
#              alpha = .8, color = "chocolate2") +
#   scale_size_continuous(range = c(.5, 10),
#                         breaks = bb,
#                         labels = ll) +
#   labs(size = "Exvessel Revenue", title = 'Year: {frame_time}') +
#   theme_void() +
#   transition_time(LANDING_YEAR) +
#   ease_aes('sine-in-out')
# 
# animate(plots, fps = 1, end_pause = 20,renderer=gifski_renderer("test.gif"))
