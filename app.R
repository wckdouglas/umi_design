#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("Designing UMI length for your TGIRT-seq "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "umi_length",
                        label = "Number of UMI base:",
                        min = 8,
                        max = 22,
                        value = 13),
            sliderInput(inputId = "seq_depth",
                    label = "Number of read depth (M):",
                    min = 1,
                    max = 1000,
                    value = 10),
            numericInput('n', 'Number of different fragments', 1000)
        ),
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("poisson_plot")
    ))
))



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    output$poisson_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- 0:20
        diversity <- 4^input$umi_length * input$n
        lambda <- input$seq_depth *1e6/ diversity
        p<-dpois(x, lambda)
        
        # draw the histogram with the specified number of bins
        qplot(x=x,y=p, geom='line') +
            geom_line(color = 'red', size = 2) +
            xlim(0,20) +
            labs(x = 'How many times would UMI collision occurs?',
                  y = 'Fraction of UMI')+
            theme(text = element_text(size = 20))
    }) 
})

# Run the application 
shinyApp(ui = ui, server = server)
