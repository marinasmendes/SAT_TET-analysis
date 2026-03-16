mw1_boxplot <- clean_data %>%
  filter(
    Session_Type == "MindWandering1",
    Dimension == "Rumination"
  )%>%
  mutate(
    Block_Name = factor(Block, levels = c(1, 2, 3, 4), 
                      labels = c("Pre", "Guided", "Open", "Post")),
    Participant_ID = paste("Participant", Participant_ID)
  )

ggplot(data = mw1_boxplot, aes(x = Block_Name, y = Intensity, fill = Block_Name)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "darkgray") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkred") +
  facet_wrap(~Participant_ID) +
  theme_bw() +
  theme(
    legend.position = "none", # Hide legend since x-axis has the names
    strip.text = element_text(size = 12, face = "bold", background = element_rect(fill = "gray90")),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Distribution of Rumination Across Intervention Blocks (MW1)",
    subtitle = "Boxes show median & quartiles | Red diamonds = Mean | Dots = Raw data points",
    x = "Intervention Phase",
    y = "Rumination Intensity (0-100)"
  )