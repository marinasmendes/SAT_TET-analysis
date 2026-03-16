# ==============================================================================
# STEP 1: IMPORT
# ==============================================================================

# 1. Find all files ending in "Aggregated.csv"
file_list <- list.files(pattern = "Aggregated\\.csv$")

if(length(file_list) == 0) {
  stop("No files found! Check your working directory.")
}

print(paste("Found", length(file_list), "files. Importing..."))

# 2. Read and combine files
data_list <- list()

for (file in file_list) {
  # Read the file
  temp <- read.csv(file, stringsAsFactors = FALSE)
  data_list[[file]] <- temp
}

# Combine into one master dataframe
all_data <- do.call(rbind, data_list)

print("Import successful!")
print(paste("Total rows:", nrow(all_data)))

# ==============================================================================
# STEP 2: CREATE IDENTIFIERS
# ==============================================================================

# 1. Extract Day Number from Session_ID (e.g., "003_Day10_GB" -> 10)
# We look for the digits strictly between "Day" and "_"
all_data$Day_Num <- as.numeric(gsub(".*Day(\\d+)_.*", "\\1", all_data$Session_ID))

# 2. Create 5-Day Block
all_data$Block <- ceiling(all_data$Day_Num / 5)

# 3. Create Group ID (Participant + Session Type + Block)
# This will look like: "3_Guided Breathing_Block_2"
all_data$Group_ID <- paste(all_data$Participant_ID, 
                           all_data$Session_Type, 
                           "Block", all_data$Block, 
                           sep="_")

# 4. Define Unique Session ID
# Your file already has a great one (Session_ID), but let's double-check duplicates
all_data$Unique_Session_ID <- all_data$Session_ID

# ==============================================================================
# STEP 3: CLEANUP & SAVE
# ==============================================================================

# 1. Remove rows with empty/NA Dimensions
all_data <- subset(all_data, !is.na(Dimension) & Dimension != "")

# 2. Safety Aggregation
# This removes any duplicate timestamps (if they exist) so the heatmap script doesn't crash
# We aggregate 'Intensity' by all the grouping columns
clean_data <- aggregate(Intensity ~ Unique_Session_ID + Group_ID + Participant_ID + 
                          Day_Num + Block + Session_Type + Time_Offset + Dimension, 
                        data = all_data, 
                        FUN = mean, na.rm = TRUE)

# 3. Save the Master File
write.csv(clean_data, "all_data_CLEAN_MASTER.csv", row.names = FALSE)

# 4. Set 'all_data' to the clean version for your next steps
all_data <- clean_data

print("------------------------------------------------")
print("SUCCESS! Data is compiled and cleaned.")
print(paste("Unique Sessions:", length(unique(all_data$Unique_Session_ID))))
print(paste("Unique Groups (Blocks):", length(unique(all_data$Group_ID))))
print("You are now ready to run the correlation loop!")
