library(tidyverse)
library(ggplot2)

HockeyData <- read.csv("https://raw.githubusercontent.com/packsportsanalytics/NC-State-Nova-Data-Viz-Challenge-2021/main/Clean%20PWHPA%20Secret%20Dream%20Gap%20Tour%20-%20SportLogiq%20-%20PWHPA%20Secret%20Dream%20Gap%20Tour.csv")

Colors <- read.csv('https://raw.githubusercontent.com/packsportsanalytics/NC-State-Nova-Data-Viz-Challenge-2021/main/PWHPA%20Team%20Colors.csv')

#create data frame that takes top 50 players in rush chances and group them by team

TopScorers <- HockeyData %>% 
               group_by(player, team) %>% 
              summarise(Goals = sum(goals_for, na.rm = T), 
                        RushChances = sum(rush_chances, na.rm = T)) %>% 
              ungroup

TopScorers <- merge(TopScorers, Colors, by = "team")

Plot <- ggplot(TopScorers,
               aes(x = RushChances,
                   y = Goals, 
                   color = team)) + 
  geom_point(position = "jitter") +
              labs(title = "Rush Efficiency by Player", 
                   x = "Rush Chances", 
                   y = "Goals", 
                   caption = "Data Viz by Brandon Wisniewski and Robbie Goss ~ Data from PWHPA and Sportlogiq") +
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    plot.caption = element_text(hjust = 0.5)) +
              scale_fill_identity() +
              scale_color_manual(values = c("MIN" = "#0a0f30",
                                            "NH" = "#141517",
                                            "MTL" = "#32364f",
                                            "TOR" = "#6dd5e1",
                                            "CAL" = "#ee0b19"))

print(Plot)

