matrix_data <- clean_data %>%
  filter(
    Participant_ID %in% c(1, 2, 3),
    Session_Type %in% c("MindWandering2")
  ) %>%
  select(Participant_ID, Unique_Session_ID, Time_Offset, Dimension, Intensity) %>%
  pivot_wider(
    names_from = Dimension,
    values_from = Intensity
  )

centred_data <- matrix_data %>%
  group_by(Unique_Session_ID) %>%
  mutate(across(-c(Time_Offset, Participant_ID),
                ~ .x - mean(.x, na.rm = TRUE))) %>%
  ungroup()

corr_matrix <- centred_data %>%
  select(-Unique_Session_ID, -Time_Offset, -Participant_ID) %>%
  cor(use = "pairwise.complete.obs")

corrplot(corr_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black", 
         number.cex = 0.5,
         tl.cex = 0.5,
         tl.srt = 45,
         tl.col = "black",
         diag = FALSE,
         title = "All Participants - MW2 only",
         mar = c(0,0,2,0))
  
