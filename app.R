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
                    h1("THE PHYSICAL SCIENCE BASIS", br(), "OF MARINE HARMFUL ALGAL BLOOMS", br()),
                    h2("How Pseudo-nitzschia puts the U.S. West Coast Dungeness crab fishery in peril")
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
                    "Across the U.S. west coast, the Dungeness crab fishing industry
                    is an economic and cultural lifeblood of coastal communities. Scroll through the following map to visualize
                       how much revenue the fishery yields in exvessel value (the monetary worth of commercial landings)."
                    ),
             column(3)
    ),
                          
    br(),br(),br(),br(),
    
    
    #Scroll through plots of port exvessel revenue by year 
    scrolly_container("scr",
                      scrolly_graph(
                          br(),
                          HTML('<center>'),
                          plotOutput("plot", height = '800px'),
                          HTML('</center>')),
                      scrolly_sections(
                          
                          br(),br(),br(),br(),
                          
                          scrolly_section(id = 2010,
                                          fluidRow(id="howto", class = "instructions",
                                     h2("How to read this map:"),
                                     br(), 
                                     "The size and color of each", icon("circle"), "corresponds", br(),
                                     "to the total exvessel revenue at a group", br(),
                                     "of ports in a single calendar year.",br(),
                                      br(),
                                     "Port groups with landings that", br(),
                                     "represent less than four vessels in a year", br(),
                                     "are not shown for confidentiality purposes."
                                     )),
            scrolly_section(id = 2010, fluidRow(id = 'text2010', class = "scrolly1", "2010"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2011, fluidRow(id = 'text2011', class = "scrolly1", "2011"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2012, fluidRow(id = 'text2012', class = "scrolly1", "2012"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2013, fluidRow(id = 'text2013', class = "scrolly1", "2013"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2014, fluidRow(id = 'text2014', class = "scrolly1", "2014"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2015, fluidRow(id = 'text2015', class = "scrolly1", "2015"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2016, fluidRow(id = 'text2016', class = "scrolly1", "2016"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2017, fluidRow(id = 'text2017', class = "scrolly1", "2017"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2018, fluidRow(id = 'text2018', class = "scrolly1", "2018"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2019, fluidRow(id = 'text2019', class = "scrolly1", "2019"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            scrolly_section(id = 2020, fluidRow(id = 'text2020', class = "scrolly1", "2020"),
                            br(),br(),br(),br(),br(),br(),br(),br(),br()),
            
            # add a scrolly_section with nothing in it;
            # this buffer prevents the plot from disappearing while reading last section
            scrolly_section(id = 2015, fluidRow(id = 'endscrolly1', class = "scrolly1", 
                                                h2("Did you see the drop in 2015 landings?"),
                                                   h3("In California, Oregon and coastal Washington, the Dungeness crab fishery usually opens in 
                                                   late November or early/mid December (opening date varies by region). 
                                                   Instead of opening at the end of 2015, however, 
                                                   a harmful algal bloom forced the Dungeness crab fishery
                                                   to close for up to five and a half months in some ports. 
                                                   The usual early season pulse of landings didn't occur, 
                                                   and instead, this season's (reduced) landings were pushed into 2016.", br(),
                                                   br(),
                                                   h2("Let's take a step back to look at", em("why"), br(), "the 
                                                   toxic bloom happened in 2015.")
                                                   ))),
            
            scrolly_section(id = 2015, fluidRow(id = 'blank', class = "scrolly1", br()))
            ),
            hr()
    ),
    
    
    
    
    
    fluidRow(hr(),
             column(3),
             column(6, br(), br(), br(), br(), br(), br(),
                    "In late 2013, a patch of nutrient-poor warm water
                    began to linger in the Gulf of Alaska, due in part
                    to a “ridiculously resilient” atmospheric high pressure ridge.
                    This warm water patch was soon termed the
                    “Northern Pacific Marine Heatwave Anomaly”
                    or, colloquially, “The Blob”.", br(), br(),
             ),
             column(3)
    ),
    
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
             column(6,  "In Spring 2014, the Pacific Decadal Oscillation (PDO) entered a positive phase. 
                    The PDO is ocean-atmospheric pattern of variability in ocean 
                    surface temperatures in the mid-latitude Pacific. During a PDO warm (positive) phase,
                    the eastern Pacific ocean warms and the western-central Pacific ocean cools. At the same time,
                    spring upwelling conditions began and tempered the warm temperatures along the U.S. West Coast.",  br(),br(),
             
             "By late 2014, following the end of spring/summer 2014 upwelling,
                    the blob began to encroach upon coastal zones, raising temperatures
                    along the continental shelf. The blob was 2.5°C warmer at the surface
                    than the long term mean and enabled warm-water species to move north.", br(),
                    fluidRow(br(), br(), br(),
                             column(3, br(), img(src = "images/pseudo-nitzschia-australis-diatom.png", height = "90%", width = "90%" )),
                             column(9, style = "background-color: #F4F1E2;" , 
                             "One of the microscopic marine algae that extended 
                             their range and survived these anomalously warm conditions
                             along the entire west coast was", em("Pseudo-nitzschia australis."), br(),
                             br(), 
                             "It is one of over 50 known species in the", 
                             em("Pseudo-nitzschia"), "genus, which are diatoms
                             that sit at the bottom of marine food chains. Importantly,",
                             em("P. australis"), "is well-adapted to low-nutrient conditions and responds
                             quickly to nutrient inputs."))
                    ),
             column(3)
    ),
    
    
    
    
    br(), br(), br(), br(),
    
    fluidRow(id = 'intro',
             column(3),
             column(6, "As spring upwelling conditions returned in early 2015, 
                    coastal areas were injected with upwelled nutrients 
                    and relatively cold water from the deep ocean.", br(), br(),br()
             ),
             column(3)
    ),
    
    fluidRow(id = 'intro',
             column(3),
             column(6, align = "right",
                    "sea surface temperature anomaly, May 2015", br(),
                    img(src = "images/0201NewsFeature_PacificBlob_Globe5.png"), br(),
                    h4("Image credit: Chelle Gentemann and JPL PO.DAAC: Charles Thompson and Jeffery R. Hall.")
                    ),
             column(3)
             ),
    
    
    fluidRow(br(),br(),br(),br(),br(),br(),br(),br(),br(),
             column(3, align = "right",
                    img(src = "images/pseudo-nitzschia-australis-diatom.png", height = "30%", width = "30%" ),
                    img(src = "images/domoic_acid.png", height = "50%", width = "50%" )),
             column(6, 
                    column(3, img(src = "images/pseudo-nitzschia-australis-diatom.png", height = "90%", width = "90%" )),
                    column(9, style = "background-color: #F4F1E2;" , 
                    em("Pseudo-nitzschia"), "quickly bloomed and subsequently
                    produced domoic acid, an amnesic shellfish toxin (AST).",  
                    em("P. australis"), "is one of over 25 species in the genus known 
                    to produce ASTs, although the conditions that enable AST production 
                    are environment specific and are still being studied.")),
             column(3, 
                    img(src = "images/domoic_acid.png", height = "50%", width = "50%"),
                    img(src = "images/pseudo-nitzschia-australis-diatom.png", height = "30%", width = "30%" ))
             ),
    
    fluidRow(align = "center", 
             img(src = "images/domoic_acid.png", height = "50%", width = "50%")
             ),
    
    fluidRow(id = 'end_blob',
             br(),br(),br(),br(),br(),br(),
             column(3),
             column(6, "By November 2015, with the return of El Niño 
             (a cyclical oceanic-atmospheric phenomenon), strong winds from Alaska 
             began to churn the blob and dissipate the warm surface waters."),
             column(3)
    ),
    
    fluidRow(br(),br(),br(),br(),br(),
             column(3, align = "right",
                    img(src = "images/domoic_acid.png", height = "50%", width = "50%" ),
                    img(src = "images/domoic_acid.png", height = "30%", width = "30%" )),
             column(6, 
                    fluidRow(
                        column(3, img(src = "images/domoic_acid.png", height = "90%", width = "90%" )),
                        column(9, style = "background-color: #F4F1E2;" , 
                           "Although the Blob and the", em("P. australis"), "bloom were diminished, 
                           domoic acid still remained in seafloor sediments and in the 
                           digestive tracts of many marine organisms. DA concentrates 
                           in the digestive tracts of those that ingest it, 
                           with little movement to surrounding tissues.")),
                    fluidRow(br(),
                        column(3),
                        column(9, style = "background-color: #F4F1E2;" , 
                               "Filter feeders (like razor clams) and opportunistic feeders (like Dungeness crabs)
                               can quickly concentrate domoic acid in their digestive tracts.  
                               Because domoic acid does not bioaccumulate in tissue,
                               it can be expelled from marine organisms over time via a process called depuration.  
                               But until clams and crabs are depurated of toxic levels of domoic acid, 
                               these fisheries are shut down to avoid amnesic shellfish poisoning in humans. "))),
             column(3, 
                    img(src = "images/domoic_acid.png", height = "50%", width = "50%"),
                    img(src = "images/domoic_acid.png", height = "30%", width = "30%" ))
    ),

    
    br(),br(),br(),br(),br(),br(),br(),
    
    
    # fluidRow(column(2),
    #          column(6,
    #                 # Input: Simple integer interval ----
    #                 sliderInput("integer", "Pick a year to view the Dungeness crab fishery closure length:",
    #                             sep="", ticks = FALSE,  min = 2005, max = 2017, value = 2005),
    #                 plotOutput("closures_map", height = '600px')
    #                 ),
    #          
    #          column(2, plotOutput("closures_table")),
    #          column(2),
    #          
    # ),
    
    fluidRow(br(),
             column(3),
             column(6, "The following plot visualizes the 2015 Dungeness crab season delay length 
             by port group in central and northern California, in comparison with other years. 
             Click on the plot, then hover over each bar to 
                    see the port group name and delay length.",
                    br(), h4("*Dungeness crab seasons are identified here by the year they normally would have started 
                             (2015 represents the season that would have started in November 2015 and ended in July 2016).")),
             column(3)
             ),
    
    # fluidRow(column(3),
    #          column(6, h2("Califronia Dungeness crab season delay by port")
    #          ),
    #          column(3)
    # ),
    
    fluidRow(column(2),
             column(8, 
                        plotlyOutput("bar_closure_test", height = '600px'),
                    h4("*data from Fisher et al., 2021")),
             column(2)
             ),
    
    br(),br(),br(),br(),br(),br(),br(),
    
    fluidRow(column(3),
             column(6, "Although natural variability can cause heatwaves, 
                    this devastating marine heatwave was five times more likely
                    to occur because of human-induced climate change. The blob is one of many marine heatwaves 
                    over the last two decades that can likely be attributed to humans.", br(), br(), br(), br(),
                    img(src = "images/IPCC_fig6.3.png", height = "90%", width = "90%"),
                    h4(em("Image and caption adapted from Bindoff et al., 2019:"), "Examples of recent marine heatwaves (MHWs) and their observed impacts. 
                       (a) Examples of documented MHWs over the last two decades and their impacts on natural, 
                       physical and socioeconomic systems. The colour map shows the maximum sea surface
                       temperature (SST) anomaly during the MHW using the National Oceanic and Atmospheric Administration’s
                       (NOAA) daily Optimum Interpolation SST dataset (Reynolds et al. 2007; Banzon et al. 2016).
                       A MHW is defined here as a set of spatially and temporally coherent grid points 
                       exceeding the 99th percentile. The 99th percentile is calculated over the 1982–2011 
                       reference period after de-seasonalising the data. Red shading of the boxes indicates
                       if the likelihood of MHW occurrence has increased due to anthropogenic climate change, 
                       and symbols denote observed impacts on physical systems over land, marine ecosystems,
                       and socioeconomic and human systems. Figure is updated from Frölicher and Laufkötter (2018
                       ) and is not a complete compilation of all documented MHWs."),
                    br(), br(), br(), br(),
                    h2("But what about other instances of domoic acid presence on the U.S. West Coast, outside of the blob?"),
                    "The coupling of warm  phases of natural ocean-atmospheric oscillations with both warming oceans 
                    and local upwelling conditions set the stage for harmful algal blooms and domoic acid production.
                    See the plot below from McKibben et al. (2017), which shows the link between 
                    large scale ocean-atmospheric phenomenon (Pacific Decadal Oscillation and the El Niño Southern Oscillation)
                    and domoic acid production."),
             column(3)
    ),
    
    
      
    br(), br(), br(),
    
    fluidRow(align = "center", 
             img(src = "images/McKibben_et_al_2017_fig1.png", height = "60%", width = "60%")
    ),
    
    
    br(), br(), br(), br(),
    
    fluidRow(column(3),
             column(6, 
                    h2("What might be to come and what can be done?"),
                    "The frequency, duration, spatial extent, and intensity of marine heatwaves 
                    are projected to increase in the future. With that, the occurance and toxicity 
                    of HABs is also expected to increase. ", "The Dungeness crab industry is already feeling the devastating impacts of HABs, 
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
  
    
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    
    
    


  

    
    ### Further Resources ----------------
    
    fluidRow(column(3),
             column(6, 
                    hr(),
                    h1("Looking for further detail on anything mentioned above?"), 
                       h2("Check out the following resources:"), 
                         
                    br(),
                    br(),
                    tags$ul(
                        tags$li(p("Pacific Northwest HAB bulletin")),
                        tags$li(p("Washington: Tribal and state Dungeness crab co-management")),
                        tags$li(p("State of Oregon Dungeness crab management")),
                        tags$li(p("State of California Dungeness crab management")),
                        tags$li(p("Socioeconomic impacts of the 2015-2016 closure")),
                        tags$li(p("California Harmful Algae Risk Model"))
                    )),
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
                        "Find the R code on Github:", tags$a(href = "https://github.com/katybland/shiny_scrolly_GCeCS", 
                                                           tags$i(class = 'fa fa-github', style = 'color:#5000a5')), 
                        br(),
                     "Have a question? Spot an error? Send an email ", 
                     tags$a(href = "mailto:katy.d.bland@gmail.com", 
                            tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%", 
                     br(),br(),
                     tags$em("Last updated: August 2021"), style = 'font-size:75%')),
             column (1)
    ),
    
    br(),br(),br(),br()
    
)




### SERVER ### ----------------------------------------------------------------------


server <- function(input, output){
    
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


