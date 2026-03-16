#library(lme4)
library(nlme)
#library(lmerTest)
library(emmeans)
library(ggplot2)
library(dplyr)

med_stats_data <- clean_data %>%
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


dimension_model <- lme(
  fixed = Intensity ~ Session_Name * Day_Num,
  random = ~ 1 | Day_Factor/Unique_Session_ID,
  correlation = corAR1(form = ~ Time_Offset | Day_Factor/Unique_Session_ID),
  data = med_stats_data,
  na.action = na.omit
)


anova(dimension_model)


dimension_comparisons <- emmeans(dimension_model, pairwise ~ Session_Name | Day_Num, 
                                 at = list(Day_Num = 6:15),
                                 adjust = "bonferroni")


summary(dimension_comparisons)

dimension_comp_df <- as.data.frame(dimension_comparisons$emmeans)
clean_dimension_comp <- dimension_comp_df %>%
  filter(Session_Name %in% c("MW1", "Meditation", "MW2")) %>%
  mutate(
    Session_Name = factor(Session_Name, levels = c("MW1", "Meditation", "MW2")),
    Day_Num = as.numeric(as.character(Day_Num)),
    Intervention_Half = ifelse(Day_Num <= 10, 
                               "Days 6-10 (Guided Breathing)", 
                               "Days 11-15 (Open Monitoring)")
  )%>%
  mutate(Intervention_Half = factor(Intervention_Half, levels = c("Days 6-10 (Guided Breathing)", "Days 11-15 (Open Monitoring)")))


ggplot(clean_dimension_comp, aes(x = Session_Name, y = emmean, group = as.factor(Day_Num), color = as.factor(Day_Num))) +

  geom_line(size = 0.5, alpha = 0.5 ) +
  geom_point(size = 1) +
  

  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, alpha = 0.3) +
  

  facet_wrap(~Intervention_Half, ncol=2) +
  

  labs(
    title = "Trajectory of Estimated Worry Intensity Before, During, and After Meditation",
    subtitle = "Participant 1: Guided Breathing and Open Monitoring",
    x = "Session Type",
    y = "Estimated Worry Intensity"
  ) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.background = element_rect(fill = "#ECF0F1"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )