library(tidyverse)
library(lubridate)

raw_hr <- read.csv("raw_hr_hr.csv")
raw_rmssd <- read.csv("raw_heart_rate_variability_HR RMS SD.csv")
logbook <- read.csv("study_dates_log.csv")%>%
  mutate(Calendar_Date = as_date(Calendar_Date))

clean_hr <- raw_hr %>%
  mutate(
    Participant_ID = 1,
    DateTime = ymd_hms(start),
    Calendar_Date = as_date(DateTime),
    Day_Start = update(DateTime, hour = 6, minute = 0, second = 0),
    Minute_Progress = as.numeric(difftime(DateTime, Day_Start, units = "mins")),
    HR_bpm = as.numeric(str_remove_all(value, "\\[|\\]"))
  ) %>%
  filter(Minute_Progress >= 0, Minute_Progress <= 1020) %>%
  mutate(
    Time_Window_Mins = (floor(Minute_Progress/30)*30) + 12.5,
    Time_Window_Mins = ifelse(Time_Window_Mins > 1002.5, 1020, Time_Window_Mins)
  ) %>%
  # FIX: Kept Time_Window_Mins instead of DateTime!
  select(Participant_ID, Calendar_Date, Time_Window_Mins, HR_bpm)

clean_rmssd <- raw_rmssd %>%
  mutate(
    Participant_ID = 1,
    DateTime = ymd_hms(start),
    Calendar_Date = as_date(DateTime),
    Day_Start = update(DateTime, hour = 6, minute = 0, second = 0),
    Minute_Progress = as.numeric(difftime(DateTime, Day_Start, units = "mins")),
    RMSSD = as.numeric(str_remove_all(value, "\\[|\\]"))
  ) %>%
  filter(Minute_Progress >= 0, Minute_Progress <= 1020) %>%
  mutate(
    Time_Window_Mins = (floor(Minute_Progress/30)*30) + 12.5,
    Time_Window_Mins = ifelse(Time_Window_Mins > 1002.5, 1020, Time_Window_Mins)
  ) %>%
  # FIX: Kept Time_Window_Mins instead of DateTime!
  select(Participant_ID, Calendar_Date, Time_Window_Mins, RMSSD)

hr_summary <- clean_hr %>%
  inner_join(logbook, by = c("Participant_ID", "Calendar_Date")) %>%
  group_by(Participant_ID, Study_Day_Num, Time_Window_Mins) %>%
  summarize(Avg_HR = mean(HR_bpm, na.rm = TRUE), .groups = "drop")

rmssd_summary <- clean_rmssd %>%
  inner_join(logbook, by = c("Participant_ID", "Calendar_Date")) %>%
  group_by(Participant_ID, Study_Day_Num, Time_Window_Mins) %>%
  summarize(Avg_RMSSD = mean(RMSSD, na.rm = TRUE), .groups = "drop")

final_physio_data <- hr_summary %>%
  full_join(rmssd_summary, by = c("Participant_ID", "Study_Day_Num", "Time_Window_Mins")) %>%
  arrange(Study_Day_Num, Time_Window_Mins)

long_physio <- final_physio_data %>%
  rename(
    Time_Offset = Time_Window_Mins,
    Day_Num = Study_Day_Num
  ) %>%
  pivot_longer(
    cols = c(Avg_HR, Avg_RMSSD),   # The columns we want to stack
    names_to = "Dimension",        # The new category column
    values_to = "Intensity"        # The new numbers column
  ) %>%
  mutate(
    # Optional: Rename them so they look cleaner in your graphs
    Dimension = ifelse(Dimension == "Avg_HR", "Heart_Rate", "HRV_RMSSD"),
    
    # Assign them the "EndOfDay" session type so they match your traces
    Session_Type = "EndOfDay"
  )

session_map <- clean_data %>%
  filter(Session_Type == "EndOfDay") %>%
  select(Participant_ID, Day_Num, Block, Unique_Session_ID, Group_ID) %>%
  distinct()

mapped_physio <- long_physio %>%
  left_join(session_map, by = c("Participant_ID", "Day_Num")) %>%
  select(Unique_Session_ID, Group_ID, Participant_ID, Day_Num, Block, 
         Session_Type, Time_Offset, Dimension, Intensity)

final_master_dataset <- bind_rows(clean_data, mapped_physio)

write_csv(final_master_dataset, "all_data_master.csv")

clean_data <- final_master_dataset