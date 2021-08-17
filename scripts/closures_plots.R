

#CA historical closures 

# setwd("~/Documents/GitHub/shiny_scrolly_GCeCS")

library(tidyverse)
library(sf)
library(spData)
library(knitr)
library(viridis)
library(cowplot)


CA_closures <- read.csv("data/DCRB_Historic_Closures_CA_updated.csv")
pcgroup_coords <- read.csv('data/pcgroup_mean_coordinates.csv')


#add in coordinates
closures_sp <- left_join(CA_closures, pcgroup_coords, by = c("pcgroup" = "port_group"))

#get basemap
CA_geom <- us_states %>% 
  filter(NAME == "California")


g2 <- 
  ggplot() +
    geom_sf(data = ca_geom, fill = "grey",
            # color = "transparent", 
            alpha = 0.45) +
    geom_point(data = filter(pcgroup_coords, port_group %in% CA_closures$pcgroup),
               aes(x = Lon, y = Lat, color = port_group_name), size = 4) +
    geom_text(data = filter(pcgroup_coords,port_group %in% CA_closures$pcgroup),
              aes(x = Lon, y = Lat, label= port_group_name),
              hjust = -.1, 
              size = 5
    ) +
    scale_color_manual(values = rev(c( "#81171b", "#ef8354","#c97c5d","#ccb7ae","#a6808c", "#565264", "black"))) +
    theme_void() +
    theme(legend.position = "none")
g2



make_bar_closure <- function() {
   g1<- ggplot(closures_sp) +
    geom_bar(aes(x = y+1, y = days.closed, fill = port_group_name, 
                 text = glue::glue('<span style = "font-size:1.5em">{port_group_name}</span>
                                                <i>Year</i>: {y}
                                                <i>Days delayed</i>: {days.closed}'
    )), 
             position = "dodge", stat = "identity") +
    
    # geom_text(aes(x = y, y = days.closed, label = ifelse(days.closed >0, days.closed, "")), 
    #           nudge_x = -.3, nudge_y = 2.5) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
      legend.position = "none",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      # axis.text.x = element_text(vjust = 10),
      # axis.ticks.x = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      # axis.title.y = element_blank(),
      #panel.border = element_blank(),
      panel.background = element_blank(),
      text = element_text(size = 20))+
    xlab("") +
    scale_x_continuous(labels=as.character(closures_sp$y+1),breaks=closures_sp$y+1) +
    scale_y_continuous(name = "Days closed") +
    scale_fill_manual(values = rev(c( "#81171b", "#ef8354","#c97c5d","#ccb7ae","#a6808c", "#565264", "black")))

  
  ggplotly(g1, tooltip = 'text') 
}




ggplotly(make_bar_closure())

bar_closure_b <- ggplot() +
  geom_bar(data = filter(closures_sp, y == 2015), aes(x = Lat, y = days.closed , fill = days.closed), 
           position = "dodge", stat = "identity") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = c(.3, .7),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"),
        # panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("Dungeness crab season") +
  ylab("Number of days closed") +
  # scale_fill_discrete(name = "California Port Group")+
  facet_wrap(~y)



# ggsave("www/images/bar_CA_closures.png", plot = bar_closure, width = 8, height = 6) 

bbb <- as.integer(floor(seq(from = 1, to = 200, length.out = 11)))  #define size breaks
  

map_closure_length <- function(year) {
  map <- ggplot() + 
    geom_sf(data = CA_geom, fill = "grey",
            # color = "transparent", 
            alpha = 0.45) +
    geom_point(data = filter(closures_sp, y == year), 
               aes(x = Lon, y = Lat, size = days.closed, color = days.closed ),
               # text = glue::glue('<span style = "font-size:1.5em">{port_group_name}</span>
               #                                <i>Exvessel revenue</i>: ${days.closed}')),
               alpha = .9) +
    scale_size_continuous(range = c(1, 15),
                          breaks = bbb,
                          limits=c(0, 200)) +
    scale_colour_gradientn(guide = "legend",
                           breaks = bbb,
                           colours = c("burlywood1", "thistle", "navy"),
                           values = c(0, 0.65, 1),
                           limits=c(0, 200)) +
    labs(size = "Number of days closed",
         color = "Number of days closed"
         # subtitle = year,
         # caption = "*port landings representing <4 vessels in a year are not shown for confidentiality purposes"
    ) +
    theme_void ()

  return(map)
}

map_closure_length(2016)

table_closures_length <- function(year) {
  table <- closures_sp %>% 
    filter(y == year) %>% 
    select(port_group_name, days.closed)
  
  colnames(table) <- c("Port Group", "Days closed")
  return(table)
}

#table_closures_length(2015)


