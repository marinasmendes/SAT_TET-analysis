mw_comparison_boxplot <- clean_data %>%
  filter(
    Session_Type %in% c("MindWandering1", "Guided Breathing", "Open Monitoring", "MindWandering2"),
    Block %in% c(2, 3),
    Dimension %in% c("Worry", "Stress", "Rumination")
  )%>%
  mutate(
    Session_Name = fct_collapse(Session_Type,
      "MW1" = "MindWandering1",
      "Meditation" = c("Guided Breathing", "Open Monitoring"),
      "MW2" = "MindWandering2"
    ),
    Session_Name = fct_relevel(Session_Name, "MW1", "Meditation", "MW2"),
    Participant_ID = paste("Participant", Participant_ID),
  )

ggplot(data = mw_comparison_boxplot, aes(x = Session_Name, y = Intensity, fill = Session_Name)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "darkgray") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkred") +
  facet_grid(Dimension~Participant_ID) +
  theme_bw() +
  theme(
    legend.position = "none", # Hide legend since x-axis has the names
    strip.text = element_text(size = 12, face = "bold", background = element_rect(fill = "gray90")),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Distribution of Anxiety Across Intervention Blocks (MW1)",
    subtitle = "Boxes show median & quartiles | Red diamonds = Mean | Dots = Raw data points",
    x = "Intervention Phase",
    y = "Heart Rate Variability RMSSD"
  )