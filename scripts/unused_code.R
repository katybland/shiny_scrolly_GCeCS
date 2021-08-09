

# unused app.R code bits

############
#### unused, potentially useful code bits
############



# top_left <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"
# top_right <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"
# bottom_left <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"
# bottom_right <- "https://thisiswhidbey.com/wp-content/uploads/2019/08/Dungeness-crabs-Anacortes-commercial-fishery.jpg"


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




# in server
#output$section <- renderText(paste0("Section: ", input$scr))

# observe({cat("section:", input$scr, "\n")})

# #for second scrolly container
# output$plot2 <- renderPlot({
#   map_diff_years(input$scr2)
# })
# 
# output$scr2 <- renderScrollytell({scrollytell()})


# #test out different scrolly container
# scrolly_container("scr2",
#                   scrolly_graph(
#                       br(),
#                       HTML('<center>'),
#                       plotOutput("plot2", height = '900px'),
#                       HTML('</center>')),
#                   scrolly_sections(
#                       
#                       br(),br(),br(),br(),
#                       
#                       scrolly_section(id = 2010,
#                                       fluidRow(id="howto", class = "instructions",
#                                                h2("How to read this map:"),
#                                                br(), 
#                                                "The size and color of each", icon("circle"), "corresponds", br(),
#                                                "to the total exvessel revenue at a group", br(),
#                                                "of ports in a single calendar year.",br(),
#                                                br(),
#                                                "Port groups with landings that", br(),
#                                                "represent < 4 vessels in a year", br(),
#                                                "are not shown for confidentiality purposes."
#                                       )),
#                       scrolly_section(id = 2010, fluidRow(id = 'text2010', class = "scrolly1", "2010"),
#                                       br(),br(),br(),br(),br(),br(),br(),br(),br()),
#                       scrolly_section(id = 2011, fluidRow(id = 'text2011', class = "scrolly1", "2011"),
#                                       br(),br(),br(),br(),br(),br(),br(),br(),br()),
#                       scrolly_section(id = 2012, fluidRow(id = 'text2012', class = "scrolly1", "2012"),
#                                       br(),br(),br(),br(),br(),br(),br(),br(),br()),
#                      
#                       
#                       # add a scrolly_section with nothing in it;
#                       # this buffer prevents the plot from disappearing while reading last section
#                       scrolly_section(id = 2020, fluidRow(id = 'endscrolly1', class = "scrolly1", h2("Did you see the drop in Dungeness crab landings in 2015?"))),
#                       scrolly_section(id = 2020, fluidRow(id = 'blank', class = "scrolly1", br())),
#                       scrolly_section(id = 2020, fluidRow(id = 'blank', class = "scrolly1", br()))
#                   )
#                   
# ),
