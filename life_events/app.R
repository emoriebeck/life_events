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
       padding: 20px"), href="http://pmdlab.wustl.edu/projects/networks.html"),
  
  
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
                 selectizeInput("plot", label = "Choose Plot Type:",
                                choices = c("Trajectories", "Posterior Distributions")),
                 
                 conditionalPanel(
                   condition = "input.plot != 'Trajectories'",
                   selectizeInput("pars", label = "Choose Parameters",
                                  choices = "")
                 ),
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
                 tabPanel("Matching", 
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("cat", label = "Choose Matching Category",
                                             choices = ""),
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

load(url("https://github.com/emoriebeck/life_events/blob/master/mean_diff.RData?raw=true"))
load(url("https://github.com/emoriebeck/life_events/raw/master/selection_samples.RData"))
load(url("https://github.com/emoriebeck/life_events/raw/master/growth_samples.RData"))
load(url("https://github.com/emoriebeck/life_events/raw/master/growth_pred.RData"))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  observe({
      events <- unique(diff$Event)
      updateCheckboxGroupInput(session, 'events', choices = c(events),
                               selected = events[1:5])
      cats <- unique(diff$Category)
      updateSelectizeInput(session, 'cat', choices = c("", cats),
                           selected = "BFI")
  })
  
  observe({
    events <- unique(diff$Event)
    updateCheckboxGroupInput(session, 'events2', choices = c(events),
                             selected = events[1:2])
  })
  
  observe({
    events <- unique(diff$Event)
    updateCheckboxGroupInput(session, 'events3', choices = c(events),
                             selected = events[1:2])
    pars <- unique(growth_samples$term)
    updateSelectizeInput(session, 'pars', choices = c(pars),
                         selected = pars[1])
  })
  
  
   output$corPlots <- renderPlot({
     df <- diff %>% unnest(d, .drop = T) %>% 
       filter(Category == input$cat & Event %in% input$events)
     
     df %>%
       group_by(Event, match_set, var) %>%
       summarize(d = mean(d)) %>% ungroup() %>%
       mutate(match_set = recode(match_set, `socialization` = "Matched")) %>%
       ggplot(aes(x = var, y = d, shape = match_set)) +
       scale_shape_manual(values = c(19,1)) +
       scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1, 1, 1)) +
       geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
       geom_point(size = 3) +
       labs(y = "Cohen's d", x = NULL, shape = NULL) +
       coord_flip() +
       facet_grid(.~Event) +
       theme_classic() +
       theme(legend.position = "bottom",
             axis.text.y = element_text(face = "bold"),
             axis.text.x = element_text(face = "bold", size = rel(1.2)),
             axis.title = element_text(face = "bold", size = rel(1.2)),
             strip.text = element_text(face = "bold"),
             legend.text = element_text(face = "bold"),
             legend.title = element_text(face = "bold", size = rel(1.2)))
      # generate bins based on input$bins from ui.R
      
   })

   output$selPlots <- renderPlot({
     df <- bfi_samples %>% 
       filter(Trait %in% input$traits2 & Event %in% input$events2 & match %in% input$set)
     
     df %>% 
       ggplot(aes(y = Event, x = estimate)) +
       geom_vline(aes(xintercept = 1), linetype = "dashed") +
       geom_density_ridges(aes(fill = match), alpha= .4, scale = .5, 
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
     if (input$plot != "Trajectories"){
       df <- growth_samples %>% 
         filter(Trait %in% input$traits3 & Event %in% input$events3 & term %in% input$pars)
       
       df %>% 
         ggplot(aes(y = Event, x = estimate)) +
         geom_density_ridges(aes(fill = Trait), alpha= .4, scale = .8, 
                rel_min_height = 0.025) +
         stat_pointintervalh(aes(color = Trait), .prob = c(.66, .95)) +
         theme_classic() +
         theme(legend.position = "bottom")
     } else{
       df <- growth_pred %>% filter(shrt_Trait %in% input$traits3 & shrt_Event %in% input$events3)
       rdf <- range_act %>% filter(shrt_Trait %in% input$traits3 & shrt_Event %in% input$events3)
       df %>%
         ggplot(aes(x = new.wave + 1, y = Estimate)) +
         scale_x_continuous(limits = c(1,3), breaks = seq(1,3,1)) +
         scale_color_manual(values = c("royalblue", "black")) +
         geom_ribbon(aes(ymin = `2.5%ile`, ymax = `97.5%ile`, group = le_value), fill = "lightblue", alpha = .25) +
         geom_line(aes(color = factor(le_value), linetype = factor(le_value)), size = 1) +
         geom_blank(data = rdf) +
         labs(x = "Wave", y = "Predicted Personality Rating",
              color = "Life Event", linetype = "Life Event") +
         facet_grid(Event ~ Trait, scales = "free") +
         theme_classic() +
         theme(axis.text = element_text(face = "bold"),
               axis.title = element_text(face = "bold", size = rel(1.2)),
               legend.position = "bottom",
               legend.text = element_text(face = "bold"),
               legend.title = element_text(face = "bold", size = rel(1.2)),
               strip.text = element_text(face = "bold", size = rel(.8)),
               plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5))
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

