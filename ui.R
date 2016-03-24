library(shiny)

load("data/box_profiles_20151217.Rds")

shinyUI(fluidPage(
  
  # suppresses spurious 
  # 'progress' error messages after all the debugging 
  # is done:
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  HTML("<hr>"),
  # the main stuff:
    fluidRow(
        column(width = 6,
               helpText("Functional view:"),
               plotOutput("funcplot", height=600,
                          brush = brushOpts(id = "func_plot_brush")
                          )
        ),
        column(width = 6,
               helpText("Communications view:"),
               plotOutput("commplot", height=600,
                          brush = brushOpts(id = "comm_plot_brush")
               )
        )
      ),
    #fluidRow(
    #  column(width=6,
    #         verbatimTextOutput("debug_func")
    #         ),
    #  column(width=6,
    #         verbatimTextOutput("debug_comm")
    #  )
    #),
    fluidRow(
      column(width = 3,
             selectInput("port", 
                         label = "Server port:",
                         choices = c("Any",
                                     as.character(ports)),
                         selected = "Any")
      ),
      column(width = 3,
             sliderInput("edgefrac",
                  label = "% edges to draw:", 
                  min = 0.0, max = 5.0, step=0.1,
                  value = 0.0)
      ),
      column(width = 3,
        selectInput("commcoords", 
                  label = "Main graph layout:",
                  choices = c("DRL",
                              "LINE 1st-order",
                              "LINE 2nd-order",
                              "LINE mixed",
                              "Fruchterman-Reingold"
                  ),
                  selected = "DRL")
      ),
      column(width = 3,
             selectInput("zoomcoords", 
                         label = "Zoom graph layout:",
                         choices = c("Communications layout",
                                     "Functional layout",
                                     "Auto adjust"
                                    ),
                         selected = "Communications layout")
      )
    ),
    sidebarLayout(
      sidebarPanel(
             checkboxInput("drawzoom", label = "Draw zoom graph? (All edges!)", value = FALSE),
             helpText("Protocol profile:"),
             plotOutput("plot_hover_proto", width="100%", height=250),
             sliderInput("proto_temp", 
                         label = "Protocol amplifier:",
                         min = 1, max = 10, value = 1),
             helpText("Port profile:"),
             plotOutput("plot_hover_port", width="100%", height=250),
             sliderInput("port_temp", 
                         label = "Port amplifier:",
                         min = 1, max = 10, value = 1)
      ),
      mainPanel( 
             plotOutput("subplot", height=800,
                        click = clickOpts(id="plot_click"),
                        hover = hoverOpts(id="plot_hover", delayType="throttle")
                        )
      )
    ),
    HTML("<hr>")
  )
)