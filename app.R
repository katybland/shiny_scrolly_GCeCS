# This is the shiny app 
# code from: https://github.com/connorrothschild/shiny-scrollytell/blob/master/app.R
# start edits July 21, 2021
# last edit July 21, 2021
# edits by Katy Bland



# setwd("~/Documents/GitHub/shiny_scrolly_GCeCS")

library(scrollytell)
library(here)
library(shinyWidgets)
library(plotly)
library(ggvis)

source(here("scripts/pacfin_script.R"))
source(here("scripts/closures_plots.R"))



### USER INTERFACE ### ----------------------------------------------------------------------

ui <- fluidPage(
   
    title = "gcecs",
    
    # suppress warning messages while data is loading on-screen
    tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    #point to style sheet for document
    tags$head(includeCSS("www/style.css")),
    
    # article title & name
    fluidRow(style = "background-color: #FFFFFF;",
        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
        ),
   
    ggvisOutput("plot_test"),
     
    fluidRow(style = "background-color: #FFFFFF;",
             column(2),
             column(8,
                    h1("When a marine heatwave", br(), "leads to a fishery disaster", br()),
                    h2("How harmful algal blooms threaten the U.S. West Coast Dungeness crab fishery")
                   ),
             column(2)
    ),
    
    fluidRow(br(), br(), br(),
             style = "background-color: #FFFFFF;", 
             column(3),
             column(6, img(src = "images/IMG_3318_1080x.png", height = "100%", width = "100%")),
             column(3,br(), br(),br(), br(),br(), 
                    h5("scroll down to explore the story"))
    ),
    
    # 
    # fluidRow(style = "background-color: #FFFFFF;", align = "center", 
    #          ),
   
    fluidRow(style = "background-color: #FFFFFF;",
             br(),br()
             ),
    
    fluidRow(id = 'intro',
             column(3),
             column(6,
                    br(),
                    br(),
                    hr(),
                    br(),
                    "In January 2017, the U.S. Secretary of Commerce declared eight 'Federal Fishery Disasters' in U.S. waters.
                    Among these were the 2015/16 California Dungeness crab and rock crab fisheries
                    and the 2015/16 Quileute Dungeness crab fishery (a Tribal fishery on the Washington coast). 
                    Congress eventually provided $25.8 Million and $1.5 Million, respectively, 
                    of relief funding to those directly affected by these fishery disasters.", br(), 
                    br(), 
                    h2("Why were they 'federal fishery disasters'?") ,"Broadly, 
                       federal fishery disasters are major fishery disruptions 
                       arising from natural, undetermined, or in certain circumstances, man-made causes. ",
                    br(), br(),
                    "Scroll down to see the monetary value of 
                    the California 
                    Dungeness crab commercial fishery from 2011-2020. 
                    Keep an eye out for the revenue disruption in the 2015/16 season."
                  
                    # "Across the U.S. west coast, the Dungeness crab fishing industry
                    # is an economic and cultural lifeblood for coastal communities. So when the fishery 
                    
                    ),
             column(3)
    ),
                          
    br(),br(),br(),br(),
    
    
    #Scroll through plots of port exvessel revenue by year 
    scrolly_container("scr",
                      
                      scrolly_graph(
                          br(),br(),br(),br(),br(),br(),br(),
                          HTML('<center>'),
                          plotOutput("rev_plot",
                                     height = "500px",
                                    width = "900px"),
                          h4("*Dungeness crab seasons are identified here by the year they normally end.", br(),"
                             (2016 represents the season that started in November 2015 and ended in July 2016)."),
                          HTML('</center>')),
                      scrolly_sections(
                          
                          br(),br(),br(),br(),
                          
            # scrolly_section(id = 2010, fluidRow(id = 'text2010', class = "scrolly1", "2010"),
            #                 br(),br(),br(),br()),
            scrolly_section(id = 2011, fluidRow(id = 'text2011', class = "scrolly1", "2011"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2012, fluidRow(id = 'text2012', class = "scrolly1", "2012"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2013, fluidRow(id = 'text2013', class = "scrolly1", "2013"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2014, fluidRow(id = 'text2014', class = "scrolly1", "2014"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2015, fluidRow(id = 'text2015', class = "scrolly1", "2015"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2016, fluidRow(id = 'text2016', class = "scrolly1", "2016"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2017, fluidRow(id = 'text2017', class = "scrolly1", "2017"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2018, fluidRow(id = 'text2018', class = "scrolly1", "2018"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2019, fluidRow(id = 'text2019', class = "scrolly1", "2019"),
                            br(),br(),br(),br()),
            scrolly_section(id = 2020, fluidRow(id = 'text2020', class = "scrolly1", "2020"),
                            br(),br(),br(),br()),
            
            
            
            # add a scrolly_section with nothing in it;
            # this buffer prevents the plot from disappearing while reading last section
            scrolly_section(id = 2020, fluidRow(id = 'endscrolly1', class = "scrolly1", 
                                                h2("Why was there such as dramatic drop in revenue?"),
                                                   h3("In California, 
                                                   the Dungeness crab fishery usually opens in 
                                                   late November or early December (opening date varies by region). 
                                                   Instead of opening at the end of 2015, however, 
                                                  the Dungeness crab fishery was closed for up to five and a half months
                                                  in some ports because of a severe harmful algal bloom. 
                                                   The Oregon and Washington state Dungeness crab fisheries 
                                                      were delayed about one month.", br(),
                                                   br()
                                                   
                                                  
                                                   ))),
            
            scrolly_section(id = 2020, fluidRow(id = 'blank', class = "scrolly1", br())),
            hr()
            )
    ),
    
    
    fluidRow(br(),
             column(3),
             column(6, br(), br(), br(),br(), br(), br(),br(), br(), br(),br(), 
                    h2("Although season delays are common, this delay was unprecedented."),br(), 
             "The following plot visualizes the 2015/16 Dungeness crab season delay length 
             by port group in central and northern California, in comparison with other years. Hover over each bar to 
                    see the port group name and delay length.",
                    br(), h4("*Dungeness crab seasons are identified here by the year they normally end.", br(),"
                             (2016 represents the season that started in November 2015 and ended in July 2016).")),
             column(3)
    ),
    
    fluidRow(column(1),
             column(7, 
                    plotlyOutput("bar_closure_test", height = '600px'),
                    h4("*data from Fisher et al., 2021")),
             column(3, br(), br(), 
                    plotOutput("CA_map_stag")),
             column(1)
    ),
    
    fluidRow(
             column(3),
             column(6, 
                    br(), br(), br(),br(), br(), 
                    h1("Let's take a step back to look at... "),h2("why harmful algal blooms happen,", br(),
                       "why they become toxic,", br(), "and how they impact the fishery.")),
             column(3)
    ),
    
    fluidRow(br(),br(),br(),
             column(3,  align = "right", img(src = "images/pseudo-nitzschia-australis-diatom.png", height = "90%", width = "90%" )),
             column(6, 
                    # style = "background-color: #F4F1E2;" , 
                    em("Pseudo-nitzschia"), "is a genus of over 50 known species of diatoms
                             that sit at the bottom of marine food chains.",
                    em("Pseudo-nitzschia australis"), "is a species that is well-adapted to low-nutrient conditions and
                    extreme temperatures, responds quickly to nutrient inputs, and like 25 other", 
                    em("Pseudo-nitzschia"),"species, can produce toxins.", br(),
                    br(),
                    "When provided with warm water and nutrients,", em("Pseudo-nitzschia australis"), 
                    "quickly reproduces and creates a 'bloom'. Although these blooms are dramatic, 
                    they are only harmful if the bloom produces domoic acid, an amnesic shellfish toxin (AST).", br(),
                    br(),
                    "The conditions that enable domoic acid production are environment-specific 
                    and are still being studied. In lab experiments, iron limitation, low silicic acid concentrations, 
                    increased pH, increased salinity, the presence of predators, light availability, 
                    and sometimes even multiple, compounding factors have been linked to domoic acid production.",
                    br(), br(),
                    "By feeding on both", em("Pseudo-nitzschia"),"and other intermediate organisms (like zooplankton), 
                    filter feeders (like razor clams) and opportunistic feeders (like Dungeness crabs)
                               can quickly concentrate domoic acid in their digestive tracts.  
                               Because domoic acid does not permanently bioaccumulate in tissue, however,
                               it can be expelled from marine organisms over time via a process called depuration.  
                               Until clams and crabs are depurated of toxic levels of domoic acid,
                               these fisheries are shut down to avoid amnesic shellfish poisoning in humans."),
             
             column(3, br(), br(), br(), br(), br(), br(),br(), br(), br(),
                    img(src = "images/domoic_acid.png", height = "80%", width = "80%" ))
    ),
                    
    # fluidRow(id = 'intro',
    #          column(3),
    #          column(6, br(), br(), br(), br(),
    #                 h2("Why do", em("Pseudo-nitzschia"),"blooms happen on the U.S. West Coast?"),
    #                 
    #                 "One oscillation, ", br(),br(),
    #                 img(align = "center", src = "images/PDO_NOAA.png", height = "100%", width = "100%" ),
    #                 h4("Image: climate.gov"),
    #                 br(),br(),
    #                 "During a PDO warm (positive) phase,
    #                 .
    #                 
    #                ", br(), br()),
    #          column(3)
    # ),
    # br(),br(),
    # 
    # 
  
    
    
    
    #hold for an RRR picture?
    # fluidRow(id = 'intro',
    #          column(3),
    #          column(6, align = "right",
    #                 "sea surface temperature anomaly, May 2015", br(),
    #                 img(src = "images/0201NewsFeature_PacificBlob_Globe5.png"), br(),
    #                 h4("Image credit: Chelle Gentemann and JPL PO.DAAC: Charles Thompson and Jeffery R. Hall.")),
    #          column(3)
    # ),
    # br(), br(), br(), br(),
    # 
    
    fluidRow(id = 'intro',
             column(3),
             column(6, br(), br(), br(), br(), br(), 
                    h2("So what happened in 2015?"),
                    
                    "Since", em("Pseudo-nitzschia"), "needs warmth and nutrients to bloom,
                    the coupling of natural ocean-atmospheric oscillations in the Pacific Ocean, 
                    yearly upwelling conditions on the U.S. West Coast, and a marine heatwave 
                    set the stage for a harmful algal bloom.", br(), br(),
                    
                    "In late 2013, a patch of nutrient-poor warm water began to linger in the Gulf of Alaska, 
                    This warm water patch was soon termed the “Northern Pacific Marine Heatwave Anomaly”
                    or, colloquially, “The Blob”.", br(), br(), "Additionally, the Pacific Decadal Oscillation (PDO), 
                   an ocean-atmospheric pattern of variability in ocean 
                    surface temperatures, entered a positive phase in Spring 2014.
                    
                    During a positive phase, the eastern Pacific ocean warms and the western-central Pacific ocean cools.", br(), 
                    br(), 
                    "See the plot below, from McKibben et al. (2017), which shows the link between 
                    large scale ocean-atmospheric phenomenon,
                    sea surface temperature and domoic acid production in Oregon razor clams.", br(), br()),
                    
                    column(3)
             ),
    
    fluidRow(align = "center", 
                      img(src = "images/McKibben_et_al_2017_fig1.png", height = "70%", width = "70%")
             ),  
    
    fluidRow(
             column(3),
             column(6, br(), br(), br(), br(), br(), 
                    
             
                    "By late 2014, following the end of spring/summer 2014 coastal upwelling,
                    the Blob began to encroach upon coastal zones, raising temperatures
                    along the continental shelf. The Blob was 2.5°C warmer at the surface
                    than the long term mean and enabled warm-water species to move north.", br(), br(),
                    
                    "As spring upwelling conditions returned in early 2015, 
                    coastal areas were injected with upwelled nutrients. The image below shows this slightly cooler,
                    nutrient rich water along the U.S. West Coast (green) and the Blob offshore (red)."),
    column(3)
),
             
                    
     
    
    
    fluidRow(br(), br(),
             column(3),
             column(6, align = "right",
                    "sea surface temperature anomaly, May 2015", br(),
                    img(src = "images/0201NewsFeature_PacificBlob_Globe5.png"), br(),
                    h4("Image credit: Chelle Gentemann and JPL PO.DAAC: Charles Thompson and Jeffery R. Hall.")
                    ),
             column(3)
             ),
    
    
    fluidRow(id = 'end_blob',
             br(),br(),br(),br(),br(),br(),
             column(3,
                    fluidRow(align = "center", 
                             img(src = "images/domoic_acid.png", height = "50%", width = "50%"))),
             column(6, "Because of the unusually warm water and nutrient supply,", em("Pseudo-nitzschia australis"), 
                    "quickly bloomed and subsequently produced domoic acid.",
                    "By November 2015, however, strong winds from Alaska began to churn the Blob and dissipate the warm surface waters.
                    Although the Blob and the", em("Pseudo-nitzschia australis"), "bloom were diminished, 
                    domoic acid still remained in the food web.",
                    br(),
                    br()
                    # h2("Coastwide, the Blob indirectly led to unprecedented levels of domoic acid.") 
                    ),
             column(3,
                    fluidRow(align = "center", 
                             img(src = "images/domoic_acid.png", height = "50%", width = "50%")))
    ),
    
    fluidRow(br(), br(),
             column(12, br(), align = "center",
                    img(src = "images/DA_map_large.png",height = "80%", width = "80%"), br(),
                    fluidRow(column(8),
                             column(4, h4("Image: NOAA’s Northwest Fisheries Science Center.")))
                    )
               
    ),
   
    br(),br(),br(),br(),
    
    
    br(), br(), br(), br(),
    
    fluidRow(column(3, align = "right", br(), br(), br(), br(), br(),br(), br(), br(), br(), br(),
                    br(), br(), br(), br(), br(), br(), br(), br(),                 
                    img(src = "images/oregon_legend.png",height = "80%", width = "80%")),
             column(6, 
                    h2("What does this have to do with climate change?"),
                    
                    "Although natural variability can cause heatwaves, 
                    this devastating marine heatwave is very likely attributed to human-induced climate change.
                    The Intergovernmental Panel of Climate Change (IPCC) projects that the 
                    frequency, duration, spatial extent, and intensity of marine heatwaves 
                    will increase in the future. With that, the occurance and toxicity 
                    of HABs is also expected to increase. ",  br(),
                    br(),
                    
                    h2("What can be done?"), 
                    "The Dungeness crab industry is already feeling the devastating impacts of HABs, 
                    so mitigating these impacts will therefore be critical to the survival of the industry.", br(),
                    br(),
                    "In reaction to the prevalence of domoic acid in recent years, west coast states are now employing 
                    management strategies (namely evisceration orders) that may alleviate these Dungeness crab 
                    industry disruptions. Evisceration orders dictate that crab exceeding the action level 
                    in crab viscera (30 parts per million), but below the action level in crab meat (20 parts per million) 
                    must be gutted to be sold. Evisceration keeps the fishery open but limits crab product forms. 
                    Depuration of toxins, by holding crab in tanks until crab viscera and tissue is below the action level,
                    is another potential strategy for combating industry disruptions.", br(), 
                    br(),
                    "In the face of a warming ocean and more frequent domoic acid episodes, understanding the relative
                    benefit of each strategy will be vital to ensuring the wellbeing of the Dungeness crab industry
                    and coastal communities across the west coast."),
             column(3)
             ),
  
    
    br(),
    fluidRow(br(), br(), br(),
             style = "background-color: #FFFFFF;", 
             column(4),
             column(4, img(src = "images/sections_pacific_dreams.png", height = "100%", width = "100%"),
                    h4("Image: Pacific Dream Seafoods")),
             column(4
                    )
    ),
    
    br(),br(),br(),br(),br(),br(),
    
    
    


  

    
    ### Further Resources ----------------
    
    fluidRow(column(3),
             column(6, 
                    hr(),
                    h1("Looking for further detail?"), 
                       h2("Check out the following resources:"), 
                         
                    br(),
                    br(),
                    tags$ul(
                      tags$li(p("U.S. Federal Fishery Disasters"),
                              tags$ul(
                                tags$li(tags$a(href = "https://www.fisheries.noaa.gov/national/funding-and-financial-services/fishery-disaster-determinations",
                                                                              "Past fishery disaster determinations")),
                                tags$li(tags$a(href = "https://www.fisheries.noaa.gov/national/resources-fishing/frequent-questions-fishery-disaster-assistance", 
                                                                         "Federal Fishery Disaster FAQ"))))),
                    
                    tags$ul(
                      tags$li(p("U.S. West Coast Dungeness crab fishery management bodies"),
                              tags$ul(
                                tags$li("Coast-wide collaboration:", tags$a(href = "https://www.psmfc.org/program/tri-state-dungeness-crab-tsdc",
                                                                              "Tri-state Dungeness Crab program")),
                                tags$li("State of Oregon:", tags$a(href = "https://www.dfw.state.or.us/mrp/shellfish/commercial/crab/index.asp", 
                                                                         "ODFW Dungeness crab information")),
                                tags$li("State of California:", tags$a(href = "https://marinespecies.wildlife.ca.gov/dungeness-crab/", 
                                                                         "CDFW Dungeness crab enhanced status report")),
                                tags$li("Washington: Tribal and state Dungeness crab co-management"),
                                        tags$ul(
                                          tags$li(tags$a(href = "https://nwifc.org/",
                                                         "NorthWest Indian Fisheries Commission")),
                                          tags$li(tags$a(href = "https://wdfw.wa.gov/fishing/commercial/crab",
                                                           "WDFW Dungeness crab information"))))),
                                        
                      tags$li(p("HAB Information"),
                              tags$ul(
                                tags$li(tags$a(href = "https://coastalscience.noaa.gov/research/stressor-impacts-mitigation/hab-forecasts/",
                                                "National Centers for Coastal Ocean Science HAB Forecasts by Region")),
                                tags$li("U.S. West Coast HAB Bulletins",
                                      tags$ul(
                                        tags$li(tags$a(href = "http://www.nanoos.org/products/habs/forecasts/bulletins.php",
                                                         "Pacific Northwest HAB Bulletin")),
                                        tags$li(tags$a(href = "https://sccoos.org/california-hab-bulletin/archive-ca-hab-bulletins/",
                                                         "California HAB Bulletin")))),
                                tags$li("NOAA:", tags$a(href = "https://www.fisheries.noaa.gov/west-coast/science-data/effects-harmful-algal-blooms-west-coast-fishing-communities",
                                                      "Impacts of HABs on U.S. West Coast communities")))))),
             column(3)
    ),
    
    br(),br(),br(),br(),br(),
    
    
    hr(),
    
    
    ###  References  ------------------
    fluidRow(column(1),
             column(8, 
                    
                    h3("References"),
                    br(),
                    
                    h4("Anderson, D. M., Fensin, E., Gobler, C. J., Hoeglund, A. E., Hubbard, K. A.,
                    Kulis, D. M., Landsberg, J. H., Lefebvre, K. A., Provoost, P., Richlen, M. L., 
                    Smith, J. L., Solow, A. R., & Trainer, V. L. (2021). 
                    Marine harmful algal blooms (HABs) in the United States: History, current status and future trends. 
                    Harmful Algae, 102(January), 101975. https://doi.org/10.1016/j.hal.2021.101975"),
                    
                    h4("Bindoff, N.L., W.W.L. Cheung, J.G. Kairo, J. Arístegui, V.A. Guinder, R. Hallberg,
                       N. Hilmi, N. Jiao, M.S. Karim, L. Levin, S. O’Donoghue, S.R. Purca Cuicapusa, B. Rinkevich,
                       T. Suga, A. Tagliabue, and P. Williamson, 2019: Changing Ocean, Marine Ecosystems, 
                       and Dependent Communities. In: IPCC Special Report on the Ocean and Cryosphere in a 
                       Changing Climate [H.-O. Pörtner, D.C. Roberts, V. Masson-Delmotte, P. Zhai,
                       M. Tignor, E. Poloczanska, K. Mintenbeck, A. Alegría, M. Nicolai, A. Okem, J. Petzold
                       , B. Rama, N.M. Weyer (eds.)]. In press."),
                    
                    h4("Fisher, M. C., Moore, S. K., Jardine, S. L., Watson, J. R., & Samhouri, J. F. (2021).
                       Climate shock effects and mediation in fisheries.
                       Proceedings of the National Academy of Sciences, 118(2), e2014379117. 
                       https://doi.org/10.1073/pnas.2014379117"),
                    
                    h4("Lewitus, A. J., Horner, R. A., Caron, D. A., Garcia-Mendoza, E., Hickey, B. M., Hunter,
                    M., Huppert, D. D., Kudela, R. M., Langlois, G. W., Largier, J. L., Lessard, 
                    E. J., RaLonde, R., Jack Rensel, J. E., Strutton, P. G., Trainer, V. L., & Tweddle, J. F. (2012). 
                    Harmful algal blooms along the North American west coast region: History, trends, causes, and impacts. 
                    Harmful Algae, 19, 133–159. https://doi.org/10.1016/j.hal.2012.06.009"),
                    
                    h4("McCabe, R. M., Hickey, B. M., Kudela, R. M., Lefebvre, K. A.,
                    Adams, N. G., Bill, B. D., Gulland, F. M. D., Thomson, R. E., Cochlan, W. P., & Trainer, V. L. (2016). 
                    An unprecedented coastwide toxic algal bloom linked to anomalous ocean conditions. 
                    Geophysical Research Letters, 43(19), 10,366-10,376. https://doi.org/10.1002/2016GL070023"),
                    
                    h4("McKibben, S. M., Peterson, W., Wood, A. M., Trainer, V. L., Hunter, M., & White, A. E. (2017).
                       Climatic regulation of the neurotoxin domoic acid. 
                       Proceedings of the National Academy of Sciences of the United States of America, 114(2), 239–244.
                       https://doi.org/10.1073/pnas.1606798114"),
                    
                    h4("National Center for Biotechnology Information (2021). 
                    PubChem Compound Summary for CID 5282253, Domoic acid. 
                       Retrieved August 5, 2021 from https://pubchem.ncbi.nlm.nih.gov/compound/L-Domoic-acid."),
                  
                    h4("Trainer, V. L., Bates, S. S., Lundholm, N., Thessen, A. E., Cochlan, W. P., Adams, 
                    N. G., & Trick, C. G. (2012). Pseudo-nitzschia physiological ecology, phylogeny, 
                    toxicity, monitoring and impacts on ecosystem health. Harmful Algae, 14, 271–300. 
                    https://doi.org/10.1016/j.hal.2011.10.025)"),
                    
                    h4("Trainer, V. L., Kudela, R. M., Hunter, M. V., Adams, N. G., & McCabe, R. M. (2020). 
                    Climate Extreme Seeds a New Domoic Acid Hotspot on the US West Coast. Frontiers in Climate, 2(December), 1–11.
                    https://doi.org/10.3389/fclim.2020.571836")),
             column(3)
             ),
    
    
    #meta information ------------------
    
    fluidRow(column (1),
             column (10,
                     hr(),
                     h4("App created in RStudio by Katy Bland in 2021",  
                        br(),
                        "Find the R code on", tags$a(href = "https://github.com/katybland/shiny_scrolly_GCeCS", 
                                                           "Github."), 
                        br(),
                     "Have a question? Spot an error? ", 
                     tags$a(href = "mailto:katy.d.bland@gmail.com", 
                            "Send an email."), style = "font-size: 85%", 
                     br(),br(),
                     tags$em("Last updated: August 2021"), style = 'font-size:75%')),
             column (1)
    ),
    
    br(),br(),br(),br()
    
)




### SERVER ### ----------------------------------------------------------------------


server <- function(input, output){
    #updated new first plot
  output$rev_plot <- renderPlot({
    plot_diff_years(input$scr) 
  })
  
  # output$rev_plot_2015 <- renderPlot({
  #   highlight_2015(input$scr) 
  # })
  
  output$CA_map_stag <- renderPlot({
    g2
  })
  
  
  
  
    #for first scrolly container
    output$plot <- renderPlot({
        map_diff_years(input$scr)
    })
    
    
    # output$closures_map <- renderPlot({
    #     map_closure_length(input$integer)
    # })
    
    # output$closures_table <- renderPlot({
    #     table_closures_length(input$integer)
    # })
    
    # output$bar_closure_test <- renderPlot({
    #     make_bar_closure()
    # })
    
   
    output$bar_closure_test <- renderPlotly({
        ggplotly(make_bar_closure(), tooltip = 'text') %>%
            layout(
                title = list(element_blank()),
                legend = list(x = 0.1, y = 0.9),
                font = list(family = 'Euphemia UCAS'),
                margin = list(t = 50),
                hoverlabel = list(bgcolor = 'whitesmoke', color = 'darkGray')
            ) %>%
            config(
                displaylogo = F,
                showSendToCloud = F,
                displayModeBar = F
            )
    })
   
    
    output$scr <- renderScrollytell({scrollytell()})
    
    
    
}


# Run the application
shinyApp(ui = ui, server = server)


