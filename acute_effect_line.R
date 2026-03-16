library(ggplot2)
library(dplyr)

raw_med_stats_data <- clean_data %>%
  filter(
    Session_Type %in% c("MindWandering1", "Guided Breathing", "Open Monitoring", "MindWandering2"),
    Block %in% c(2,3),
    Dimension == "Worry",
    Participant_ID == 1
    
  ) %>%
  mutate(
    Session_Name = fct_collapse(Session_Type,
                                "MW1" = "MindWandering1",
                                "Meditation" = c("Guided Breathing", "Open Monitoring"),
                                "MW2" = "MindWandering2"
    ),
    Session_Name = fct_relevel(Session_Name, "MW1", "Meditation", "MW2"),
    #Participant_ID = as.factor(Participant_ID),
    Day_Factor = as.factor(Day_Num),
    Unique_Session_ID = as.factor(Unique_Session_ID)
  ) %>%
  arrange(Day_Factor, Unique_Session_ID, Time_Offset)

mean_med_stats_data <- raw_med_stats_data %>%
  group_by(Unique_Session_ID, Day_Num, Session_Name)%>%
  summarise(
    raw_mean = mean(Intensity, na.rm = TRUE),
    raw_sd = sd(Intensity, na.rm = TRUE),
    N = n(),
    raw_se = raw_sd/sqrt(N),
    .groups = "drop"
  )

clean_med_stats_data <- mean_med_stats_data %>%
  filter(Session_Name %in% c("MW1", "Meditation", "MW2")) %>%
  mutate(
    Session_Name = factor(Session_Name, levels = c("MW1", "Meditation", "MW2")),
    Day_Num = as.numeric(as.character(Day_Num)),
    Intervention_Half = ifelse(Day_Num <= 10, 
                               "Days 6-10 (Guided Breathing)", 
                               "Days 11-15 (Open Monitoring)")
  )%>%
  mutate(Intervention_Half = factor(Intervention_Half, levels = c("Days 6-10 (Guided Breathing)", "Days 11-15 (Open Monitoring)")))

ggplot(clean_med_stats_data, aes(x = Session_Name, y = raw_mean, group = as.factor(Day_Num), color = as.factor(Day_Num))) +
  
  geom_line(size = 0.5, alpha = 0.5 ) +
  geom_point(size = 1) +
  
  
  geom_errorbar(aes(ymin = raw_mean - raw_se, ymax = raw_mean + raw_se), width = 0.15, alpha = 0.3) +
  
  
  facet_wrap(~Intervention_Half, ncol=2) +
  
  
  labs(
    title = "Trajectory of Raw Worry Intensity Before, During, and After Meditation",
    subtitle = "Participant 1: Guided Breathing and Open Monitoring",
    x = "Session Type",
    y = "Raw Worry Intensity"
  ) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.background = element_rect(fill = "#ECF0F1"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )