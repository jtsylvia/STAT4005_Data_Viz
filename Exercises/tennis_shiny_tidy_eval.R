library(tidyverse)
library(here)

atp_df <- read_csv(here("data/atp_matches_2019.csv"))

wta_df <- read_csv(here("data/wta_matches_2019.csv"))

both_df <- bind_rows(atp_df, wta_df)

both_long <- both_df %>% pivot_longer(c(winner_name, loser_name))

both_n50 <- both_long %>% group_by(value) %>% count() %>%
  filter(n > 50)
major_tennis <- semi_join(both_long, both_n50, by = c("value"))

major_tennis <- major_tennis %>% mutate(w_svperc = 100 * w_1stIn / w_svpt,
                                        l_svperc = 100 * l_1stIn / l_svpt,
                                        w_firstwon = 100 * w_1stWon / w_1stIn,
                                        l_firstwon = 100 * l_1stWon / l_1stIn,
                                        w_secondwon = 100 * w_2ndWon / (w_svpt - w_1stIn),
                                        l_secondwon = 100 * l_2ndWon / (l_svpt - l_1stIn))

major_tennis_w <- major_tennis %>% filter(name == "winner_name")
major_tennis_l <- major_tennis %>% filter(name == "loser_name")

w_small <- major_tennis_w %>% select(value, winner_seed, w_ace, w_df, w_svperc,
                                     w_firstwon, w_secondwon) %>%
  rename(seed = winner_seed, ace = w_ace, df = w_df, svperc = w_svperc,
         firstwon = w_firstwon, secondwon = w_secondwon)

l_small <- major_tennis_l %>% select(value, loser_seed, l_ace, l_df, l_svperc, l_firstwon, l_secondwon)  %>%
  rename(seed = loser_seed, ace = l_ace, df = l_df, svperc = l_svperc,
         firstwon = l_firstwon, secondwon = l_secondwon)

df <- bind_rows(w_small, l_small) %>%
  rename(player = "value")

df_sub <- df %>% filter(player == "Daniil Medvedev")
ggplot(df_sub, aes(x = ace)) +
  geom_histogram(colour = "black", fill = "white", bins = 15)

var_choices <- names(df)[3:7]


library(shiny)

ui <- fluidPage(
  sidebarLayout(sidebarPanel(
    selectizeInput("playerchoice",
                   label = "Choose a Player", choices = levels(factor(df$player)),
                   selected = "Aryna Sabalenka"),
    radioButtons("varchoice", label = "Choose a Statistic",
                 choices = var_choices),
    sliderInput("binnumber", label = "Choose a Number of Bins", 
                min = 0, max = 100, value = 15, step = 5, animate = TRUE)),
    mainPanel(plotOutput("histgraph"))
  )
)

server <- function(input, output, session) {
  
  df_sub <- reactive({
    df %>% filter(player == input$playerchoice)
  })
  
  hist_plot <- reactive({
    # ggplot(df_sub(), aes_string(x = input$varchoice)) +
    # geom_histogram(colour = "black", fill = "white", bins = 15)
    ggplot(df_sub(), aes(x = .data[[input$varchoice]])) +
      geom_histogram(colour = "black", fill = "white", bins = input$binnumber) +
      theme_grey(base_size = 22)
  })
  
  output$histgraph <- renderPlot({
    hist_plot()
  })
  
  
}

shinyApp(ui, server)

##Did not figure out how to add secondary table

## advantages of static over interactive
## easier to present or to send to someone
## more user-friendly
## useful when trying to show one particular "thing"; like other manors of stat majors
## looking at one particular player of interest.

