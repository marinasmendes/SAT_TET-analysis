library(lme4)
library(nlme)
library(lmerTest)
library(emmeans)

mw1_stats_data <- clean_data %>%
  filter(
    Session_Type == "EndOfDay",
    Dimension == "HRV_RMSSD",
    #Participant_ID %in% c(1,2)
  ) %>%
  mutate(
    Block_Name = factor(Block, levels = c(1, 2, 3, 4), 
                        labels = c("Pre", "Guided", "Open", "Post")),
    Participant_ID = as.factor(Participant_ID),
    Unique_Session_ID = as.factor(Unique_Session_ID)
  ) %>%
  arrange(Unique_Session_ID, Time_Offset)


dimension_model <- lme(
  fixed = Intensity ~ Block_Name,
  random = ~ 1 | Participant_ID/Unique_Session_ID,
  correlation = corAR1(form = ~ Time_Offset | Participant_ID/Unique_Session_ID),
  data = mw1_stats_data,
  na.action = na.omit
)


anova(dimension_model)


dimension_comparisons <- emmeans(dimension_model, pairwise ~ Block_Name, adjust = "bonferroni")


summary(dimension_comparisons)