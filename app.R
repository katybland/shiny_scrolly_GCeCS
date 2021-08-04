# This is the shiny app 
# code from: https://github.com/connorrothschild/shiny-scrollytell/blob/master/app.R
# start edits July 21, 2021
# last edit July 21, 2021
# edits by Katy Bland

setwd("~/Documents/GitHub/shiny_scrolly_GCeCS")

library(scrollytell)
library(here)

source(here("scripts/pacfin_script.R"))

top_left <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"
top_right <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"
bottom_left <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"
bottom_right <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"

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
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    fluidRow(column(2),
             column(8,
                    # align = "center",
                    h1("The climate science of"),
                    h1("marine harmful algal blooms"),
                    h2("How Pseudo-nitzschia puts the Dungeness crab fishery in peril"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br()
                    ),
             column(2)
    ),
    
    fluidRow(align = "center", h3("- scroll down to explore the story - ")),
   
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    fluidRow(id = 'intro',
             column(3),
             column(6,
                    br(),
                    hr(),
                    br(),
                    br(),
                    br(),
                    "Across the entire U.S. west coast, the Dungeness crab fishing industry
                    is a lifeblood of coastal communities. Scroll through the following map to visualize
                       how much revenue the fishery yeilds in exvessel value."
                    ),
             column(3)
    ),
                          
    br(),
    br(),
    br(),
    br(),
    
    
    #Scroll through plots of port exvessel revenue by year 
    scrolly_container("scr",
                      scrolly_graph(
                          br(),
                          HTML('<center>'),
                          plotOutput("plot", height = '900px'),
                          HTML('</center>')),
                      scrolly_sections(
                          br(),
                          br(),
                          br(),
                          br(),
                          scrolly_section(id = 2010,
                                          fluidRow(id="howto", class = "instructions",
                                     h2("How to read this map:"),
                                     br(), 
                                     "The size and color of each", icon("circle"), "corresponds", br(),
                                     "to the total exvessel revenue at a group", br(),
                                     "of ports in a single calendar year.",br(),
                                      br(),
                                     "Port groups with landings that", br(),
                                     "represent < 4 vessels in a year", br(),
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
            scrolly_section(id = 2020, fluidRow(id = 'endscrolly1', class = "scrolly1", h2("Did you see the drop in Dungeness crab landings in 2015?"))),
            scrolly_section(id = 2020, fluidRow(id = 'blank', class = "scrolly1", br())),
            scrolly_section(id = 2020, fluidRow(id = 'blank', class = "scrolly1", br()))
            )
    ),
    
    fluidRow(id = 'intro',
             column(3),
             column(6, "The Dungeness crab fishery was closed for up to 
             5.5 months in some Northern California ports because 
             of a harmful algal bloom and subsequent domoic acid
             toxicity. Let’s take a step back and look at why this happened.", br(),
             br(),
             "In late 2013, a patch of nutrient-poor warm water 
             (now termed the Northern Pacific Marine Heatwave Anomaly) 
             began to linger in the Gulf of Alaska"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             column(3)
    )
)





server <- function(input, output){
    output$plot <- renderPlot({
        map_diff_years(input$scr)
    })
    
    output$scr <- renderScrollytell({scrollytell()})
    #output$section <- renderText(paste0("Section: ", input$scr))

    # observe({cat("section:", input$scr, "\n")})
}





# Run the application
shinyApp(ui = ui, server = server)




############
#### unused, potentially useful code bits
############

# parent container
# tags$div(class="landing-wrapper",
#          # child element 1: images
#          # tags$div(class="landing-block background-content",
#          #          # top left
#          #          img(src= top_left),
#          #          # top right
#          #          img(src= top_right),
#          #          # bottom left
#          #          img(src=bottom_left),
#          #          # bottom right
#          #          img(src=bottom_right)
#          # ),
#          # 
#          # child element 2: content
#          tags$div(class="landing-block foreground-content",
#                   tags$div(class="foreground-text",
#                            tags$h1("The Climate Science of Marine HABs"),
#                            tags$h2(" "),
#                            br(),
#                            tags$h4(em("- scroll down to explore -"))
#                   )
#          ),
#          
# ),

