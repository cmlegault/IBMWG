#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# note: need to set working directory to source file location before running app

library(shiny)
library(tidyverse)

all_scores <- read.csv(file = "all_scores.csv")
all_resids <- read.csv(file = "all_resids.csv")
scaa_scores <- read.csv(file = "all_scores_scaa.csv")
scaa_resids <- read.csv(file = "all_resids_scaa.csv")

all_scores_long <- all_scores %>%
    pivot_longer(cols = -c(1, 2), names_to = "IBM", values_to = "value") %>%
    mutate(source = "Rank",
           scenset = "base")
all_resids_long <- all_resids %>%
    pivot_longer(cols = -c(1, 2), names_to = "IBM", values_to = "value") %>%
    mutate(source = "Resid",
           scenset = "base")
scaa_scores_long <- scaa_scores %>%
    pivot_longer(cols = -c(1, 2), names_to = "IBM", values_to = "value") %>%
    mutate(source = "Rank",
           scenset = "scaa")
scaa_resids_long <- scaa_resids %>%
    pivot_longer(cols = -c(1, 2), names_to = "IBM", values_to = "value") %>%
    mutate(source = "Resid",
           scenset = "scaa")

all_values <- rbind(all_scores_long, all_resids_long, 
                    scaa_scores_long, scaa_resids_long)

all_metrics <- all_scores$metric


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Scoring IBMWG Results"),

    # Pick which metrics to include 
    sidebarLayout(
        sidebarPanel(
            selectInput("metrics",
                        "Metrics:",
                        all_metrics,
                        multiple = TRUE),
            radioButtons("myset",
                         "Set:",
                         choices = c("base", "scaa"),
                         selected = "base"),
            radioButtons("showres",
                         "Plot:",
                         choices = c("Rank", "Resid", "Both"),
                         selected = "Both")
        ),
        
        # Show a plot of the scores
        mainPanel(
            plotOutput("scorePlot"),
            tableOutput("rankTable"),    
            tableOutput("residTable")    
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scorePlot <- renderPlot({
        if (is.null(input$metrics)){
            return(NULL)
        }
        
        mysource <-  c("Rank", "Resid")
        if (input$showres != "Both"){
            mysource <- input$showres
        }
        
        myvalues <- all_values %>%
            filter(scenset == input$myset) %>%
            filter(metric %in% input$metrics) %>%
            filter(source %in% mysource) %>%
            group_by(IBM, source) %>%
            summarise(meanvalue = mean(value)) 
        
        ggplot(myvalues, aes(x=reorder(IBM, meanvalue), y=meanvalue)) +
                   geom_bar(stat = "identity") +
                   coord_flip() +
                   facet_wrap(~source, scales = "free_x") +
                   labs(x="", y="Score (bigger is better)") +
                   theme_bw()
    })
    
    output$rankTable <- renderTable({
        if (is.null(input$metrics) | input$showres == "Resid"){
            return(NULL)
        }

        myscores <- all_scores
        if (input$myset == "scaa"){
            myscores <- scaa_scores
        }
        mytable <- myscores %>%
            filter(metric %in% input$metrics)
    })
    
    output$residTable <- renderTable({
        if (is.null(input$metrics) | input$showres == "Rank"){
            return(NULL)
        }
        
        myscores <- all_resids
        if (input$myset == "scaa"){
            myscores <- scaa_resids
        }
        mytable <- myscores %>%
            filter(metric %in% input$metrics)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
