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
    tabPanel("Matching", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("events", label = "Choose Event", selected = "Married",
                                choices = c("Married","MoveIn","ChldBrth","ChldMvOut","SepPart",
                                "Divorce","PartDied", "DadDied","MomDied","Unemploy","Retire",
                                "FrstJob","LeftPar","ParDied"))
               ),
               mainPanel(
                 plotOutput("corPlots")
               ))),
    # tabPanel("Selection Effects", 
    #          sidebarLayout(
    #            sidebarPanel(
    #              checkboxGroupInput("set", label = "Choose Matching Set",
    #                             choices = c("Unmatched", "Matched"),
    #                             selected = "Matched"),
    #              checkboxGroupInput("traits2",
    #                                 "Choose Trait:",
    #                                 choices = c("E", "A", "C", "N", "O"),
    #                                 selected = c("E", "A", "C", "N", "O")),
    #              checkboxGroupInput("events2", label = "Choose Events", 
    #                                 choices = "")
    #            ),
    #            mainPanel(
    #              plotOutput("selPlots")
    #            ))),
    tabPanel("Socialization Effects", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("plot", label = "Choose Plot Type:", selected = "Posterior Distributions",
                                choices = c("Trajectories", "Posterior Distributions")),#, "Group Differences")),
                 # conditionalPanel(
                 #   condition = "input.plot != 'Trace'",
                 
                 conditionalPanel(
                   condition = "input.plot == 'Posterior Distributions'",
                   selectizeInput("pars", label = "Choose Parameters", selected = "Intercept",
                                  choices = c("Intercept", "Slope", "Event Group", "Slope x Event Group",
                                              "Level 2 Intercept SD", "Level 2 Slope SD", "Level 2 Intercept-Slope r", "Sigma"))
                 ),
                 checkboxGroupInput("traits3",
                                    "Choose Trait:",
                                    choices = c("E", "A", "C", "N", "O"),
                                    selected = c("E", "A", "C", "N", "O")),
                 checkboxGroupInput("events3", label = "Choose Events", 
                                    choices = c("Married","MoveIn","ChldBrth","ChldMvOut","SepPart",
                                                "Divorce","PartDied", "DadDied","MomDied","Unemploy","Retire",
                                                "FrstJob","LeftPar","ParDied"),
                                    selected = c("Married","MoveIn","ChldBrth","ChldMvOut"))
                 # )#,
                 # conditionalPanel(
                 #   condition = "input.plot == 'Trace'",
                 #   selectizeInput("traits4", label = "Choose Trait:",
                 #                  choices = c("E", "A", "C", "N", "O")),
                 #   selectizeInput("event4", label = "Choose Events", 
                 #                  choices = "")
                 # )
               ),
               mainPanel(
                 plotOutput("socPlots"),
                 textOutput("size")
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

# load("mean_diff.RData")
# load("growth_samples.RData")
# load("plot_files.RData")
load(url("https://github.com/emoriebeck/life_events/blob/master/results/mean_diff.RData?raw=true"))
# load(url("https://github.com/emoriebeck/life_events/raw/master/results/selection_samples.RData"))
load(url("https://github.com/emoriebeck/life_events/raw/master/results/growth_samples.RData"))
# load(url("https://github.com/emoriebeck/life_events/raw/master/results/growth_pred.RData"))
load(url("https://github.com/emoriebeck/life_events/raw/master/results/plot_files.RData"))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
#   observe({
#       events <- unique(diff$Event)
#       updateSelectizeInput(session, 'events', choices = c(events),
#                                selected = events[1])
#   })
  
  # observe({
  #   events <- unique(diff$Event)
  #   updateCheckboxGroupInput(session, 'events2', choices = c(events),
  #                            selected = events[1:6])
  # })
  
  # observe({
  #   events <- unique(diff$Event)
  #   updateCheckboxGroupInput(session, 'events3', choices = c(events),
  #                            selected = events[1:6])
  #   updateSelectizeInput(session, 'events4', choices = c(events),
  #                            selected = events[1])
  #   # pars <- unique(growth_samples$term)
  #   # updateSelectizeInput(session, 'pars', choices = c(pars),
  #   #                      selected = pars[1])
  # })
  # 
  
   output$corPlots <- renderPlot({
     df <- diff %>% unnest(d, .drop = T) %>% 
       filter(Event %in% input$events)
     
     df %>%
       group_by(Event, match_set, var) %>%
       summarize(d = mean(d)) %>% ungroup() %>%
       mutate(match_set = recode(match_set, `socialization` = "Matched")) %>%
       ggplot(aes(x = var, y = d, shape = match_set)) +
       scale_shape_manual(values = c(19,1)) +
       scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1, 1, 1)) +
       geom_hline(aes(yintercept = 0), linetype = "dashed", size = .25) +
       geom_point(size = 1.5) +
       labs(y = "Cohen's d", x = NULL, shape = NULL) +
       # coord_flip() +
       facet_grid(.~Event) +
       theme_classic() +
       theme(legend.position = "bottom",
             axis.text.x = element_text(face = "bold", size = rel(.7), angle = 45, hjust = 1),
             axis.text.y = element_text(face = "bold", size = rel(1.2)),
             axis.title = element_text(face = "bold", size = rel(1.2)),
             strip.text = element_text(face = "bold"),
             plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5),
             legend.text = element_text(face = "bold"),
             legend.title = element_text(face = "bold", size = rel(1.2)))
      # generate bins based on input$bins from ui.R
      
   })

   # output$selPlots <- renderPlot({
   #   df <- bfi_samples %>% 
   #     filter(Trait %in% input$traits2 & Event %in% input$events2 & match %in% input$set)
   #   
   #   df %>% 
   #     ggplot(aes(y = match, x = estimate)) +
   #     scale_colour_manual(values = rep("black", length(unique(input$traits2)))) +
   #     scale_fill_manual(values = c("#ac95f6", "#fffb98")[1:length(unique(input$set))]) +
   #     geom_vline(aes(xintercept = 1), linetype = "dashed") +
   #     geom_density_ridges(aes(fill = match), #alpha= .4, #scale = .5, 
   #                         rel_min_height = 0.0025) +
   #     stat_pointintervalh(aes(color = match), .prob = c(.66, .95), size = 1) +
   #     labs(x = "OR", y = NULL, fill = NULL, color = NULL) +
   #     facet_grid(Event~Trait) +
   #     theme_classic() +
   #     theme(legend.position = "bottom",
   #           axis.text = element_text(face = "bold", size = rel(1.2)),
   #           axis.text.y = element_blank(),
   #           strip.text.x = element_text(face = "bold", size = rel(2)),
   #           strip.text.y = element_text(face = "bold", size = rel(.7)),
   #           axis.title = element_text(face = "bold", size = rel(1.2)),
   #           legend.text = element_text(face = "bold", size = rel(1.2)))
   # })
      
   output$socPlots <- renderPlot({
     if (input$plot == "Posterior Distributions"){
       df <- growth_samples %>% tbl_df %>%
         filter(Trait %in% input$traits3 & Event %in% input$events3 & term %in% input$pars)
       
       df %>% 
         ggplot(aes(y = Event, x = estimate)) +
         scale_colour_manual(values = rep("black", length(input$traits3))) +
         geom_density_ridges(aes(fill = Trait), #alpha= .4, #scale = .8,
                rel_min_height = 0.005) +
         stat_pointintervalh(aes(color = Trait), .prob = c(.66, .95)) +
         facet_grid(~Trait, scales = "free") +
         theme_classic() +
         theme(legend.position = "bottom")
     } else if (input$plot == "Trajectories"){
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
     # } else if (input$plot == "Trace"){
     #   df <- growth_samples %>% tbl_df %>%
     #     filter(Trait %in% input$traits4 & Event %in% input$events4) %>%
     #     spread(key = term, value = estimate) %>%
     #     mutate_if(is.factor, funs(as.integer))
     #   
     #   mcmc_trace(df, pars = unique(growth_samples$term),
     #              size = .25) +
     #     theme_classic() +
     #     theme(legend.position = c(.5, .15),
     #           legend.direction = "horizontal") +
     #     theme(axis.text = element_text(face = "bold"),
     #           axis.title = element_text(face = "bold", size = rel(1.2)),
     #           legend.text = element_text(face = "bold"),
     #           legend.title = element_text(face = "bold", size = rel(1.2)),
     #           strip.text = element_text(face = "bold", size = rel(.8)),
     #           plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5))
     } #else {
       # color = "gray"
       # df.sl <-  slopes %>% filter(shrt_Trait %in% input$traits3 & shrt_Event %in% input$events3) %>% 
       #   filter(!is.na(Event))
       # df.rs <- ranef_slopes %>% filter(shrt_Trait %in% input$traits3 & shrt_Event %in% input$events3) %>% 
       #   filter(!is.na(Event))
       # df.rects <- rects %>% filter(shrt_Trait %in% input$traits3 & shrt_Event %in% input$events3) %>% 
       #   filter(!is.na(Event))
       # 
       # p <- df.sl %>%
       #   ggplot(aes(x = term2, y = b)) +
       #   scale_y_continuous(limits = c(-.3,.3), breaks = seq(-.3,3,.3)) +
       #   geom_violin(data = df.rs %>% filter(sig == "ns" & term == "No Event"),
       #               aes(x = 0), color = NA, fill = color, alpha = .3) +
       #   geom_violin(data = df.rs %>% filter(sig == "ns" & term == "Event"),
       #               aes(x = 1), color = NA, fill = color, alpha = .3) +
       #   scale_shape_manual(values = c(15, 17)) +
       #   geom_errorbar(data = . %>% filter(sig == "ns"), aes(ymin = lower, ymax = upper), width = .2) +
       #   geom_point(data = . %>% filter(sig == "ns" ),
       #              aes(shape = term), color = color, size = 2) + #shape = 15, 
       #   geom_point(data = . %>% filter(sig == "ns" & term == "No Event"),
       #              aes(shape = term), color = "black", size = 2, shape = 2) +
       #   geom_point(data = . %>% filter(sig == "ns" & term == "Event"),
       #              aes(shape = term), color = "black", size = 2, shape = 0) +
       #   geom_label(data = . %>% filter(sig == "ns"),
       #              aes(y = -.27, label = ifelse(abs(b) < .001, round(b, 4), 
       #                                           ifelse(abs(b) < .01, round(b,3), round(b,2)))), 
       #              fill = color, color = "black", size = 3) +
       #   geom_label(data = . %>% filter(sig == "ns" & term == "Event"),
       #              aes(y = .27, label = paste("d =", ifelse(abs(d) < .001, round(d, 4), 
       #                                                       ifelse(abs(d) < .01, round(d,3), round(d,2))), sep = " ")), 
       #              fill = color, color = "black", size = 3, nudge_x = -.5) +
       #   labs(x = NULL, y = "Estimate", shape = NULL) +
       #   facet_grid(Event~Trait) +
       #   theme_classic() +
       #   theme(legend.position = "bottom",
       #         axis.text = element_text(face = "bold", size = rel(1.2)),
       #         axis.text.x = element_blank(),#element_text(face = "bold", size = rel(1.2), angle = 45, hjust = 1),
       #         axis.ticks.x = element_blank(),
       #         axis.title = element_text(face = "bold", size = rel(1.2)),
       #         strip.text = element_text(face = "bold", size = rel(.8)),
       #         legend.text = element_text(face = "bold"),
       #         legend.title = element_text(face = "bold", size = rel(1.2)),
       #         plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5))
       # if(any(df.sl$sig == "sig")){
       #   p+
       #     geom_rect(data = df.rects %>% filter(sig == "sig"), fill = "yellow", alpha = .5,
       #               aes(xmin = -.5, xmax = 1.5, ymin = -Inf, ymax = Inf)) +
       #     geom_violin(data = df.rs %>% filter(sig == "sig" & term == "No Event"), aes(x = 0),
       #                 fill = "royalblue", color = NA, alpha = .3) +
       #     geom_violin(data = df.rs %>% filter(sig == "sig" & term == "Event"), aes(x = 1),
       #                 fill = "royalblue", color = NA, alpha = .3) +
       #     geom_errorbar(data = . %>% filter(sig == "sig"), aes(ymin = lower, ymax = upper), width = .2) +
       #     geom_point(data = . %>% filter(sig == "sig"),
       #                aes(shape = term), color = "royalblue", size = 2) + #, shape = 15
       #     geom_point(data = . %>% filter(sig == "sig" & term == "No Event"),
       #                aes(shape = term), color = "black", size = 2, shape = 2) +
       #     geom_point(data = . %>% filter(sig == "sig" & term == "Event"),
       #                aes(shape = term), color = "black", size = 2, shape = 0) +
       #     geom_label(data = . %>% filter(sig == "sig"),
       #                aes(y = -.27, label = ifelse(abs(b) < .001, round(b, 4), 
       #                                             ifelse(abs(b) < .01, round(b,3), round(b,2)))), 
       #                fill = "royalblue", color = "white", size = 3) +
       #     geom_label(data = . %>% filter(sig == "sig" & term == "Event"),
       #                aes(y = .27, label = paste("d =", ifelse(abs(b) < .001, round(b, 4), 
       #                                                         ifelse(abs(b) < .01, round(b,3), round(b,2))), sep = " ")), 
       #                fill = "royalblue", color = "white", size = 3, nudge_x = -.5)
       # }
     }
   # }, height = reactive({ifelse(input$plot == "Group Differences", 150 * length(unique(input$events3)),
   #                       ifelse(input$plot == "Posterior Distributions", 50*length(unique(input$events3)),
   #                              100 * length(unique(input$events3))))[1] })
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

