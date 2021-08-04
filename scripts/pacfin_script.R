
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
                          limits=c(0, 40000000)) +
    scale_colour_gradientn(guide = "legend", breaks = bb,labels = ll,
                           colours = c("burlywood1", "navy")
                             # myPalette(100)
                           , limits=c(0, 40000000)) +
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


render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5,
           text6,
           text7,
           text8
    )
  )
}

text0 <- HTML("<span style='font-size:20px'> How do jobs differ in their susceptibility to automation? </span>
              <br><br> 
              <p> The topic of job automation has rapidly entered the national discourse. 
              Although it was once a topic reserved for policy wonks, college students, and Reddit Libertarians, it's now a serious talking point&mdash;from 
              the multitude of news articles documenting the risk of automation, to <a href='https://www.forbes.com/sites/quora/2019/09/27/automation-will-dramatically-change-the-workforce-andrew-yang-has-a-plan-to-bridge-the-gap/#52f0e5df204b' target='_blank'>Andrew Yang's presidential proposal</a> to solve job automation via a universal basic income. 
              <br> Experts have put forth a wide range of estimates for the true impact of job automization. No matter how severe it is, few argue that automation will have <i>no impact</i>.
              Most agree that automation will impact the way Americans work, and that it may put many workers out of a job (and many argue it already has).
              <br><br> But will different workers experience the impacts of automation in different ways? In this post, I combine data from two Oxford researchers, <a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Carl Benedikt Frey and Michael A. Osborne</a>,
              with employment statistics from the Bureau of Labor Statistics to answer the question: 
              <br><br>
              <span style='font-size:18px'> How do jobs differ in their susceptibility to automation? </span>
              <br> Specifically, how does a worker's <b>level of education</b> and <b>current income</b> influence their risk of job loss to the rise of the robots? <p>")

text1 <- HTML("<H2> 2015 </H2>
              <br> <p> this is going to be a paragraph on the fundamentals of climate change, 
              <br> and to the right there will be an infographic about climate change
              <br> and then there will be another paragraph on how climate change leads to 
              <br> ocean warming because ocean absorbs heat (and carbon dioxide etc)</right>")

text2 <- HTML("<H2> High school diplomas </H2>
              <br> <p>Workers with <font color='#F56C42'>high school diplomas</font> have a median income of $25,636.
              <br> On average, those occupations have a <b>60% chance</b> of job automation.
              <br><br> There are 33,129,910 workers with a <font color='#F56C42'>high school diploma</font>.<p>")

text3 <- HTML("<H2> Postsecondary nondegree awards </H2>
              <br> <p>Workers with <font color='#008640'>postsecondary nondegree awards</font> (e.g. actors) have a median income of $39,990.
              <br> On average, those occupations have a <b>52% chance</b> of job automation.
              <br><br> There are 5,904,150 workers with a <font color='#008640'>postsecondary nondegree award</font>.<p>")

text4 <- HTML("<H2> Associate's degrees </H2>
              <br> <p>Workers with <font color='#3487BD'>associate's degrees</font> have a median income of $41,496.
              <br> On average, those occupations have a <b>50% chance</b> of job automation.
              <br><br> There are 1,869,840 workers with an <font color='#3487BD'>associate's degree</font>.<p>")

text5 <- HTML("<H2> Bachelor's degrees </H2>
              <br> <p>Workers with <font color='#C71C7E'>bachelor's degrees</font> have a median income of $59,124.
              <br> On average, those occupations have a <b>20% chance</b> of job automation.
              <br><br> There are 18,399,270 workers with a <font color='#C71C7E'>bachelor's degree</font>.<p>")

text6 <- HTML("<H2> Master's degrees </H2>
              <br> <p>Workers with <font color='#5E4FA2'>master's degrees</font> have a median income of $69,732.
              <br> On average, those occupations have a <b>10% chance</b> of job automation.
              <br><br> There are 1,281,710 workers with a <font color='#5E4FA2'>master's degree</font>.<p>")

text7 <- HTML("<H2> Doctoral degrees </H2>
              <br> <p>Workers with <b>doctoral degrees</b> have a median income of $84,396.
              <br> On average, those occupations have a <b>3% chance</b> of job automation.
              <br><br> There are 1,386,850 workers with a <b>doctoral degree</b>.<p>")

text8 <- HTML("<H2> In Sum </H2>
              <br> <p>All things considered, the nominal median income of an average US worker is <b>$31,786</b>.
              <br>
              <br> 47% of jobs are expected to face a high risk of automatization in the near future.<sup>1</sup><p>
              <br><br><br>
              <span style='font-size:11px'><sup>1</sup><a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>
               write that 'associated occupations are potentially automatable over
              some unspecified number of years, <i>perhaps a decade or two.'</i></span>")

concludingtext <- HTML("<p><span style='font-size:24px'><b>The Risk of Automation</b></span>
                        <br>
                            <span style='font-size:18px'>This data led researchers Carl Frey and Michael Osborne to predict that 47% of jobs are at serious risk of automation over the next couple decades.
                        <br>
                            <br>The visuals above suggest that the ills of automation may not be evenly distributed across jobs.
                            Less educated workers are more likely to face job loss as a product of automation. Those with high school diplomas or less find themself concentrated near the top of the y-axis, while those with bachelor’s degrees or higher face a lower risk of automation.
                        <br>
                            <br>A job’s salary is also predictive of automation probability. As the median income of a profession increases, the likelihood of automation displacing its workers decreases.
                            This could suggest that automation will increasingly bifurcate the already divided labor market, making those at the top wealthier at the expense of the worse-off.
                        <br>
                            <br>Automation’s impact on work necessitates a policy response. The fact that automation will have different effects on different industries and different workers is a reminder that this public policy will have to be strategic and thoughtful.</span></p>")


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
