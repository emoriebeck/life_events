#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  tags$head(tags$style(HTML("
    .shiny-text-output {
                            background-color:#FF0000;
                            } 
                            "))),
  
  a(h1(span("Life Events", style = "font-weight: 300"), 
       style = "font-family: 'Source Sans Pro';
       color: #fff; text-align: center;
       background-color:#6A51A3;
       padding: 20px"), href="http://pmdlab.wustl.edu/beck/projects/networks.html"),
  
  
  br(),
  # Application title
  #titlePanel("Idiographic Personality Networks"),
  # Sidebar with a slider input for number of bins 
  tabsetPanel(
    tabPanel("Raw Data", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("covs", label = "Choose Covariates",
                                choices = ""),
                 checkboxGroupInput("traits",
                                "Choose Trait:",
                                choices = c("E", "A", "C", "N", "O"),
                                selected = "E"),
                 checkboxGroupInput("events", label = "Choose Events", 
                                    choices = "")
               ),
               mainPanel(
                 plotOutput("corPlots")
               ))),
    tabPanel("Selection Effects", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("covs", label = "Choose Covariates",
                                choices = ""),
                 checkboxGroupInput("traits",
                                    "Choose Trait:",
                                    choices = c("E", "A", "C", "N", "O"),
                                    selected = "E"),
                 checkboxGroupInput("events", label = "Choose Events", 
                                    choices = "")
               ),
               mainPanel(
                 plotOutput("corPlots")
               ))),
    tabPanel("Socialization Effects", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("covs", label = "Choose Covariates",
                                choices = ""),
                 checkboxGroupInput("traits",
                                    "Choose Trait:",
                                    choices = c("E", "A", "C", "N", "O"),
                                    selected = "E"),
                 checkboxGroupInput("events", label = "Choose Events", 
                                    choices = "")
               ),
               mainPanel(
                 plotOutput("corPlots")
               ))),
    wellPanel(
      helpText(   a("Lab Website",     href="http://pmdlab.wustl.edu/beck/projects/networks.html")
      )
    )
)
)


library(psych)
library(brms)
library(bayesplot)
library(tidybayes)
library(tidyverse)

load(url("https://github.com/emoriebeck/life_events/raw/master/data.RData"))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  observe({
      events <- unique(le_dat$Event)
      updateCheckboxGroupInput(session, 'events', choices = c(events),
                               selected = events[1:2])
      covs <- unique(match.dat$new_name)
      updateSelectizeInput(session, 'covs', choices = c("", covs),
                           selected = covs[1])
  })
  
  
   output$corPlots <- renderPlot({
     df <- match.dat %>% filter(new_name %in% input$covs) %>% 
       full_join(le_dat %>% select(Event, PROC_SID, le.group) %>%
                   filter(Event %in% input$events)) %>%
       full_join(bfi_long %>% filter(Trait %in% input$traits)) %>%
       filter(!is.na(le.group) & !is.na(value) & !is.na(pers_value) & wave == "T1")
     
     samp <- sample(unique(df$PROC_SID),200)
     
     df %>% ggplot(aes(x = pers_value, y = value)) +
       geom_jitter(data = . %>% filter(PROC_SID %in% samp), aes(shape = factor(le.group))) +
       geom_smooth(aes(color = factor(le.group)), method = "lm", se = F) +
       labs(x = "Personality", y = input$covs, color = "Event",
            title = input$covs, shape = "Event") +
       facet_grid(Event~Trait)+
       theme_classic()
      # generate bins based on input$bins from ui.R
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

