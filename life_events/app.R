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
    tabPanel("Selection Effects", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("set", label = "Choose Matching Set",
                                choices = c("Unmatched", "Matched"),
                                selected = "Matched"),
                 checkboxGroupInput("traits2",
                                    "Choose Trait:",
                                    choices = c("E", "A", "C", "N", "O"),
                                    selected = c("E", "A", "C", "N", "O")),
                 checkboxGroupInput("events2", label = "Choose Events", 
                                    choices = "")
               ),
               mainPanel(
                 plotOutput("selPlots")
               ))),
    tabPanel("Socialization Effects", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("pars", label = "Choose Parameters",
                                choices = ""),
                 checkboxGroupInput("traits3",
                                    "Choose Trait:",
                                    choices = c("E", "A", "C", "N", "O"),
                                    selected = c("E", "A", "C", "N", "O")),
                 checkboxGroupInput("events3", label = "Choose Events", 
                                    choices = "")
               ),
               mainPanel(
                 plotOutput("socPlots")
               ))),
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
library(ggridges)
library(tidyverse)

load(url("https://github.com/emoriebeck/life_events/raw/master/data.RData"))
load(url("https://github.com/emoriebeck/life_events/raw/master/selection_samples.RData"))
load(url("https://github.com/emoriebeck/life_events/raw/master/growth_samples.RData"))

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
  
  observe({
    events <- unique(le_dat$Event)
    updateCheckboxGroupInput(session, 'events2', choices = c(events),
                             selected = events[1:2])
  })
  
  observe({
    events <- unique(le_dat$Event)
    updateCheckboxGroupInput(session, 'events3', choices = c(events),
                             selected = events[1:2])
    pars <- unique(growth_samples$term)
    updateSelectizeInput(session, 'pars', choices = c(pars),
                         selected = pars[1])
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

   output$selPlots <- renderPlot({
     df <- bfi_samples %>% 
       filter(Trait %in% input$traits2 & Event %in% input$events2 & match %in% input$set)
     
     df %>% 
       ggplot(aes(y = Event, x = estimate)) +
       geom_vline(aes(xintercept = 1), linetype = "dashed") +
       geom_density_ridges(aes(fill = match), alpha= .4, scale = .8, 
                           rel_min_height = 0.025) +
       stat_pointintervalh(aes(color = match), .prob = c(.66, .95)) +
       labs(x = "OR", y = NULL, fill = NULL, color = NULL) +
       facet_wrap(~Trait, nrow = 1) +
       theme_classic() +
       theme(legend.position = "bottom",
             axis.text = element_text(face = "bold", size = rel(1.2)),
             strip.text = element_text(face = "bold", size = rel(2)),
             axis.title = element_text(face = "bold", size = rel(1.2)),
             legend.text = element_text(face = "bold", size = rel(1.2)))
   })
      
   output$socPlots <- renderPlot({
     df <- growth_samples %>% 
       filter(Trait %in% input$traits3 & Event %in% input$events3 & term %in% input$pars)
     
     df %>% 
       ggplot(aes(y = Event, x = estimate)) +
       geom_density_ridges(aes(fill = Trait), alpha= .4, scale = .8, 
              rel_min_height = 0.025) +
       stat_pointintervalh(aes(color = Trait), .prob = c(.66, .95)) +
       theme_classic() +
       theme(legend.position = "bottom")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

