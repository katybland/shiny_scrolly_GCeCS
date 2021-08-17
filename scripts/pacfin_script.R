
# make pacfin public data map by port group 2010-2020

rm(list = ls())
#setwd("~/Documents/GitHub/shiny_scrolly_GCeCS")



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
library(plotly)
library(ggrepel)
# font_add_google(name = "STIX Two Math", family = "STIX Two Math")



# read in state/port files from PACFIN ---------------------------------
# WA <- read.csv("data/pacfin_public/ALL005-W-2010---2020.csv") %>% 
#   filter(PACFIN_SPECIES_CODE == "DCRB")
# OR <- read.csv("data/pacfin_public/ALL005-O-2010---2020.csv") %>% 
#   filter(PACFIN_SPECIES_CODE == "DCRB")
# CA <- read.csv("data/pacfin_public/ALL005-C-2010---2020.csv") %>% 
#   filter(PACFIN_SPECIES_CODE == "DCRB")
tri_state <- read_csv("data/pacfin_public/CRAB001-W-O-C-A-2000---2021.csv")
tri_state_totals <- tri_state[c(1,2,51:54)] %>% 
  filter(AGENCY_CODE %in% c("O", "W", "C"))
  




# CA Coordinates -- (From Fisher et al. 2021) -------------------------------------
# the latitude is an average of all port of landing locations within a port group
#   the longitude was set manually to be a point along the coast at the given latitude
# manually added in OR, WA, and SoCA port group lat/long from google maps approximation
# pcgroup_coords <- read.csv('data/pcgroup_mean_coordinates.csv')
# colnames(pcgroup_coords)

  

# 
# # remove unnecessary columns
# WA <- WA[-c(3,4,5,7,8,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28,29,30)]
# OR <- OR[c(1,2,6,9,13,17,21,25,29,33)]
# CA <- CA[c(1,2,6,9,13,17,21,25,29,33,37,41,45,49)]


# #make in tidy format
# WA <- WA %>%
#   pivot_longer(!c(1,2,3), names_to = "port", values_to = "exvessel_revenue")
# OR <- OR %>%
#   pivot_longer(!c(1,2,3), names_to = "port", values_to = "exvessel_revenue")
# CA <- CA %>%
#   pivot_longer(!c(1,2,3), names_to = "port", values_to = "exvessel_revenue")


# #bind datasets
# all <- bind_rows(WA, OR)
# all <- bind_rows(all, CA)


# #convert ports to factors
# all$port <- as.factor(all$port)


# #take off unnecessary part of port name
# levels(all$port) <- gsub('_EXVESSEL_REVENUE', '', levels(all$port))


#add in coordinates
# all_spatial <- left_join(all, pcgroup_coords, by = c("port" = "port_group"))

#aggregate to state level for plot
# agg_spatial <- all_spatial %>% 
#   group_by(AGENCY_CODE, LANDING_YEAR) %>% 
#   summarize(total_evr = sum(exvessel_revenue, na.rm = TRUE)) 
  # mutate(reveal = case_when(
  #   LANDING_YEAR == 2010 ~ 1,
  #   LANDING_YEAR == 2011 ~ 2,
  #   LANDING_YEAR == 2012 ~ 3,
  #   LANDING_YEAR == 2013 ~ 4,
  #   LANDING_YEAR == 2014 ~ 5,
  #   LANDING_YEAR == 2015 ~ 6,
  #   LANDING_YEAR == 2016 ~ 7,
  #   LANDING_YEAR == 2017 ~ 8,
  #   LANDING_YEAR == 2018 ~ 9,
  #   LANDING_YEAR == 2019 ~ 10,
  #   LANDING_YEAR == 2020 ~ 11,
  # ))






plot_diff_years <- function(year) {
  plot <- ggplot(data = filter(tri_state_totals, AGENCY_CODE == "C" & CRAB_YEAR >2010),
                 aes(x = CRAB_YEAR, y = TOTAL_EXVESSEL_REVENUE)) +
    geom_rect(inherit.aes=FALSE, aes(xmin=2015.5, xmax=2016.5,
                                     ymin=-Inf,
                                     ymax=Inf),
              color="transparent", fill="gray94", alpha=0.3) +
    geom_line(aes(group = AGENCY_CODE, 
                  color = AGENCY_CODE
                  ,
                  alpha = ifelse(CRAB_YEAR+1 <= year, 1, 0)
                  ),
              size = 3) +
    geom_point(aes(color = AGENCY_CODE
                   ,
                  alpha = ifelse(CRAB_YEAR <= year, 1, 0)
                  # text = glue::glue('<span style = "font-size:1.5em">{AGENCY_CODE}</span>
                  #                               <i>Crab Season</i>: {CRAB_YEAR}
                  #                               <i>Total Revenue</i>: {TOTAL_EXVESSEL_REVENUE}'
                  ),
               size = 4) +

    geom_text_repel(aes(label=paste("$", round(TOTAL_EXVESSEL_REVENUE/1000000, digits = 1), "Million"),
                  alpha = ifelse(CRAB_YEAR == year, 1, 0),
                  ), force=1, point.padding=unit(1,'lines'),
                  vjust=0,
                  direction='y',
                  nudge_x=2,
                  segment.size=0.2,
              size = 5,
              # hjust= ifelse(year < 2020, -.2, 1.3), vjust= ifelse(year < 2020, 0, .2)
              ) +
    scale_color_manual(name = "",
                       labels = c("California", "Oregon", "Washington"),
                       values = c("#c97c5d","#ccb7ae", "#565264")) +
    scale_alpha(range = c(0, 1), guide = 'none') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          legend.position = "none", #c(.75, .15),
          text = element_text(size=22),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlab("") +
    ylab("") +
    scale_x_continuous(breaks = seq(from = 2011, to = 2020, by = 1)) +
    scale_y_continuous(limits = c(0, 110000000),
                       breaks=c(0, 30000000, 60000000, 90000000),
                       labels = c("0", "$30 Million", "$60 Million", "$90 Million")) 
  
  plot
}

plot_diff_years(2020)


# highlight_2015 <- function(year) {
#   plot <- ggplot() +
#     # geom_point(aes(x = as.factor(LANDING_YEAR), y = total_evr ,color = AGENCY_CODE)) +
#     geom_line(data = agg_spatial, aes(LANDING_YEAR, y = total_evr, group = AGENCY_CODE, 
#                   color = AGENCY_CODE),
#                   alpha =.5,
#               size = 3) +
#     geom_point(data = agg_spatial, aes(LANDING_YEAR, y = total_evr, color = AGENCY_CODE),
#                    alpha = .5,
#                size = 4) +
#     geom_point(data = filter(agg_spatial, LANDING_YEAR == 2015), aes(LANDING_YEAR, y = total_evr, color = AGENCY_CODE),
#                    alpha = 1 ,
#                size = 6) +
#     # geom_text(aes(label=paste("$", round(total_evr/1000000, digits = 1), "Million"), 
#     #               alpha = ifelse(LANDING_YEAR == year, 1, 0)),
#     #           size = 5,
#     #           hjust= ifelse(year < 2020, -.2, 1.3), vjust= ifelse(year < 2020, 0, .2)) +
#     scale_color_manual(name = "",
#                        labels = c("California", "Oregon", "Washington"),
#                        values = c("#c97c5d","#ccb7ae", "#565264")) +
#     scale_alpha(range = c(0, 1), guide = 'none') +
#     theme_bw() +
#     theme(legend.position = c(.75, .15),
#           # legend.position = "top",
#           text = element_text(size=22),
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank()) +
#     xlab("") +
#     ylab("") +
#     scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1)) +
#     scale_y_continuous(limits = c(0, 110000000),
#                        breaks=c(0, 30000000, 60000000, 90000000),
#                        labels = c("0", "$30 Million", "$60 Million", "$90 Million")) 
#   
#   plot
#   
# }
# 
# highlight_2015(2020)


  









# ggplot() +
#   geom_bar(data = all, aes(x = as.factor(LANDING_YEAR), y = exvessel_revenue, fill = port), stat = "identity") +
#   facet_wrap(~AGENCY_CODE)

#### Map transition across years into different plots for scrolly #####

#get basemap 
wc_geom <- us_states %>% 
  filter(NAME %in% c("Washington", "Oregon", "California"))

ca_geom <-  us_states %>% 
  filter(NAME == "California")


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

# map_diff_years <- function(year) {
#   scale = 1
#   map <- ggplot() +
#     geom_sf(data = wc_geom, fill = "grey",
#             # color = "transparent", 
#             alpha = 0.45) +
#     geom_point(data = filter(all_spatial, LANDING_YEAR == year), 
#                aes(x = Lon, y = Lat, 
#                    size = exvessel_revenue, color = exvessel_revenue,
#                    text = glue::glue('<span style = "font-size:1.5em">{port_group_name}</span>
#                                                 <i>Exvessel revenue</i>: ${exvessel_revenue}')),
#                alpha = .9) +
#     scale_size_continuous(range = c(1, 25),breaks = bb,labels = ll,
#                           limits=c(0, 40000000),
#                           guide = guide_legend(reverse = TRUE) ) +
#     scale_colour_gradientn( breaks = bb,labels = ll,
#                            colours = c("#ef8354", "#ccb7ae", "#565264")
#                              # myPalette(100)
#                            , limits=c(0, 40000000),
#                            guide = guide_legend(reverse = TRUE) 
#                             ) +
#     labs(size = "Exvessel Revenue",
#          color = "Exvessel Revenue"
#          # subtitle = year,
#          # caption = "*port landings representing <4 vessels in a year are not shown for confidentiality purposes"
#          ) +
#     theme_void() 
# # theme(
#     #   text = element_text(family = "STIX Two Math", size = 22*scale), 
#     #   plot.title = element_text(size = 26*scale, hjust = 0.5),
#     #   plot.subtitle = element_text(size = 14*scale, hjust = 0.5),
#     #   plot.caption = element_text(size = 12*scale),
#     #   legend.text = element_text(size = 16*scale))
#   return(map)
# }
# 
# map_diff_years(2010)

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
