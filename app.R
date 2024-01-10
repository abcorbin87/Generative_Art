# andrew.corbin.ngs@gmail.com
# Jan 8th, 2024

#install.packages("shiny", "aRtsy", "ggplot2", "magrittr", "shinythemes")

library(shiny)
library(aRtsy)
library(ggplot2)
library(magrittr)
library(shinythemes)
# 

# functions and needed objects
numbers <- seq(1, 100000)



# Define UI for application
ui <- fluidPage(theme = shinytheme("darkly"), title = "aRrtsy aRt geneRatoR",
                tabsetPanel(
                  tabPanel(
                    titlePanel("'Flow' Art Generator"),
                    
                    # Sidebar
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("seed1",
                                     "Enter Seed Number (1-100,000):",
                                     value = 88),
                        p("Suggested seeds for interesting plots to get started: 88, 3587, 846, 123, 245, 55555, ..."),
                        br(),
                        sliderInput("iter_in1",
                                    dragRange = FALSE,
                                    label = "Number of iterations",
                                    min = 1,
                                    max = 2000,
                                    value = 300,
                                    step = 1),
                        sliderInput("lines_in1",
                                    dragRange = FALSE,
                                    label = "Number of lines to draw",
                                    min = 1,
                                    max = 4000,
                                    value = 1000,
                                    step = 1),
                        sliderInput("lwd_in1",
                                    dragRange = FALSE,
                                    label = "Line weight (default = 0.05)",
                                    min = 0.01,
                                    max = 5,
                                    value = 0.05,
                                    step = 0.01),
                        sliderInput("input_col1",
                                    dragRange = FALSE,
                                    label = "How many random colors do you want to include?)",
                                    min = 2,
                                    max = 1024,
                                    value = 5,
                                    step = 1),
                        actionButton("flow_button", "Generate 'Flow' Art!", class = "btn-info", style="color: #fff; background-color: #bbbbbb; border-color: #2e6da4"),
                        br(),
                        br(),
                        downloadButton('down', "Download Random 'Flow' Plot!"),
                        radioButtons(inputId = "downtype", label = "Select the file type", choices = list("png", "pdf")),
                        width = 5),
                      
                      # Show a plot
                      mainPanel(plotOutput(outputId = "plot1"), width = 7)
                    )
                    # mainPanel(plotOutput(outputId = "plot1"), width = 7)
                ),
                    tabPanel(
                      titlePanel("'Smoke' Art Generator"),
                      
                      # Sidebar
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("seed2",
                                       "Enter Seed Number (1-100,000):",
                                       value = 245),
                          p("Suggested seeds for interesting plots to get started:"),
                          p("88, 3587, 846, 123, 245, 55555, ..."),
                          br(),
                          sliderInput("init2",
                                      dragRange = FALSE,
                                      label = "Number of initial starting points",
                                      min = 1,
                                      max = 100,
                                      value = 2,
                                      step = 1),
                          actionButton("smoke_button", "Generate 'Rainbow Smoke' Art!", class = "btn-info", style="color: #fff; background-color: #bbbbbb; border-color: #2e6da4"),
                        width = 5),
                      mainPanel(plotOutput(outputId = "plot2"), width = 7)
                    )
                   # mainPanel(plotOutput(outputId = "plot1"), width = 7)
                        # Show a plot
                  ),
                  tabPanel(
                    titlePanel("Maze Generator"),
                    
                    # Sidebar
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("seed3",
                                     "Enter Seed Number (1-100,000):",
                                     value = 123),
                        p("Suggested seeds for interesting plots to get started:"),
                        p("88, 3587, 846, 123, 245, 55555, ..."),
                        br(),
                        sliderInput("size3",
                                    dragRange = FALSE,
                                    label = "Size of maze",
                                    min = 6,
                                    max = 100,
                                    value = 40,
                                    step = 1),
                        # check box for polar or not to default not polar
                        checkboxInput("polar3", "Check if you want the maze to be polar"),
                        actionButton("maze_button", "Generate Maze!", class = "btn-info", style="color: #fff; background-color: #bbbbbb; border-color: #2e6da4"),
                        position = "left"),
      
                      # Show a plot
                    mainPanel(plotOutput(outputId = "plot3"), position = "right")
                )
                # mainPanel(plotOutput(outputId = "plot1"), width = 7)
              ),
              tabPanel(
                titlePanel("Mandelbrot Generator"),
                
                # Sidebar
                sidebarLayout(
                  sidebarPanel(position = "left",
                    numericInput("seed4",
                                 "Enter Seed Number (1-100,000):",
                                 value = 55555),
                    p("Seeds are from 1-100,000. Suggested seeds for interesting plots to get started: 88, 3587, 846, 123, 245, 55555, ..."),
                    br(),
                    textInput(inputId = "set_in4",
                              label = "Fractal Set (mandelbrot, multibrot, julia, or ship):",
                              value = "julia"),
                    sliderInput("zoom4",
                                dragRange = FALSE,
                                label = "Zoom Scale (default = 4)",
                                min = 0,
                                max = 20,
                                value = 4,
                                step = 1),
                    sliderInput("input_color4",
                                dragRange = FALSE,
                                label = "How many random colors do you want to include?)",
                                min = 2,
                                max = 1024,
                                value = 4,
                                step = 1),
                    sliderInput("res_in4",
                                dragRange = FALSE,
                                label = "Resolution",
                                min = 1,
                                max = 1500,
                                value = 300,
                                step = 1),
                    sliderInput("left_in4",
                                dragRange = FALSE,
                                label = "Left - def -2.16",
                                min = -5,
                                max = 5,
                                value = -2.16,
                                step = 0.01
                    ),
                    sliderInput("right_in4",
                                dragRange = FALSE,
                                label = "Right - def 1.16",
                                min = -5,
                                max = 5,
                                value = 1.16,
                                step = 0.01
                    ),
                    sliderInput("bottom_in4",
                                dragRange = FALSE,
                                label = "Bottom - def -1.66",
                                min = -5,
                                max = 5,
                                value = -1.66,
                                step = 0.01
                    ),
                    sliderInput("top_in4",
                                dragRange = FALSE,
                                label = "Top - def 1.66",
                                min = -5,
                                max = 5,
                                value = 1.66,
                                step = 0.01
                    ),
                    actionButton("mandelbrot_button", "Generate Mandelbrot!", class = "btn-info", style="color: #fff; background-color: #bbbbbb; border-color: #2e6da4"), 
                    ),
                  
                  # Show a plot
                  mainPanel(plotOutput(outputId = "plot4"), position = "right")
                )
            )
          )
)

# Define server logic required to print plot using random seed

server <- function(input, output) {
  observeEvent(input$flow_button, {
    output$plot1 <- renderPlot({
      seed1 <- reactive(input$seed1)
    set.seed(seed1())
    iter_in1 = reactive(input$iter_in1)
    lines_in1 = reactive(input$lines_in1)
    lwd_in1 = reactive(input$lwd_in1)
    input_col1 = reactive(input$input_col1)
    # angles <- matrix(0, 200, 200)
    # angles[1:100, ] <- seq(from = 0, to = 2 * pi, length = 100)
    # angles[101:200, ] <- seq(from = 2 * pi, to = 0, length = 100)
    # angles <- angles + rnorm(200 * 200, sd = 0.1)
    canvas_flow(lines = lines_in1(),
                iterations = iter_in1(), 
                lwd = lwd_in1(), 
                colors = colorPalette('random', n = input_col1()), 
                background = "#000000")
                # polar = TRUE,
                # angles = angles)
    }, height = 700, width = 900)
  })
  observeEvent(input$smoke_button, {
    output$plot2 <- renderPlot({
      input_init2 <- reactive(input$init2)
      set.seed(input$seed2)
      canvas_smoke(colors = colorPalette("random", 1024), 
                   resolution = 300, 
                   init = input_init2())
    }, height = 700, width = 900)
  })
    observeEvent(input$maze_button, {
      output$plot3 <- renderPlot({
        input_size3 = reactive(input$size3)
        set.seed(input$seed3)
        canvas_maze(color = "#fafafa", walls = "#000000", background = "#fafafa", resolution = input_size3())
      }, height = 700, width = 900)
  })
    observeEvent(input$mandelbrot_button, {
      output$plot4 <- renderPlot({
        set_name4 = reactive(input$set_in4)
        zoom_in4 = reactive(input$zoom4)
        res_in4 = reactive(input$res_in4)
        left_in4 = reactive(input$left_in4)
        right_in4 = reactive(input$right_in4)
        bottom_in4 = reactive(input$bottom_in4)
        top_in4 = reactive(input$top_in4)
        input_col4 = reactive(input$input_color4)

        set.seed(input$seed4)
        canvas_mandelbrot(colors = colorPalette("random", input_col4()), 
                          set = set_name4(), 
                          zoom = zoom_in4(), 
                          #resolution = 1000,
                          resolution = res_in4(),
                          left = left_in4(),
                          right = right_in4(),
                          bottom = bottom_in4(),
                          top = top_in4()
        )
                          # left = -2.16, 
                          # right = 1.16, 
                          # bottom = -1.66, 
                          # top = 1.66)
      }, height = 700, width = 900)
    })
    
    
    # download random plot
  output$down <- downloadHandler(
    filename =  function() {
      paste("Generative_Art_Image", input$downtype, sep=".")
    },
    content = function(file) {
      if(input$downtype == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(canvas_flow(lines = 1000, colors = colorPalette(name = 'random', n = 5), background = "#000000")) # random plot generated
      dev.off()  # turn the device off
      
    } 
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

# Idea: Make an art piece that moves chess pieces in a random way toward one part of the plot (a corner or one side - possible user decision after testing), with each piece a different color from a selection of user choices. Pieces are deleted if it reaches the end or cannot move further. plot ends when pieces cannot move anymore. user can select the chess piece, and the resolution.
