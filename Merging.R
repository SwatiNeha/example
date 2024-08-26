library(dplyr)
library (ggplot2)
library(plotly)
library(dplyr)

process_svc_file <- function(file_path, file_id, det) {
  lines <- readLines(file_path)
  data_lines <- lines[-1]  # Remove the first line (header line)
  data <- read.table(text = data_lines, header = FALSE, col.names = c("x", "y", "timestamp", "pen_state", "azimuth", "altitude", "pressure"))
  data$ID <- paste0(det, file_id)
  
  return(data)
}

read_svc_files <- function(file_paths, det) {
  data_list <- list()
  for (i in seq_along(file_paths)) {
    data <- process_svc_file(file_paths[i], i, det)
    data_list[[i]] <- data
  }
  final_data <- do.call(rbind, data_list)
  
  # Sort the data by ID and timestamp
  sorted_data <- final_data %>%
    arrange(ID, timestamp)
  
  return(sorted_data)
}


# Function to remove outliers for each patient based on the timestamp
remove_outliers <- function(data) {
  data %>%
    group_by(ID) %>%
    mutate(
      Q1 = quantile(timestamp, 0.25),
      Q3 = quantile(timestamp, 0.75),
      IQR = Q3 - Q1,
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR
    ) %>%
    filter(timestamp >= Lower_Bound & timestamp <= Upper_Bound) %>%
    select(-Q1, -Q3, -IQR, -Lower_Bound, -Upper_Bound) %>%
    ungroup()
}


# Processing Ill patients' files
file_paths_Ill <- list.files(path = "./data/Ill/", pattern = "*.svc", full.names = TRUE)
Ill_data <- read_svc_files(file_paths_Ill, "Ill_")
write.csv(Ill_data, "Ill_patients.csv", row.names = FALSE)

# Processing Control patients' files
file_paths_Ctrl <- list.files(path = "./data/Control/", pattern = "*.svc", full.names = TRUE)
Control_data <- read_svc_files(file_paths_Ctrl, "Ctrl_")
write.csv(Control_data, "Control_patients.csv", row.names = FALSE)


# Read the CSV files
Ill_patients <- read.csv("Ill_patients.csv")
Ctrl_patients <- read.csv("Control_patients.csv")

# Remove outliers from Ill patients' data
Ill_patients_clean <- remove_outliers(Ill_patients)

Ctrl_patients_clean <- remove_outliers(Ctrl_patients)
# Save the cleaned data back to CSV
write.csv(Ill_patients_clean, "Ill_patients_clean.csv", row.names = FALSE)
write.csv(Ctrl_patients_clean, "Ill_patients_clean.csv", row.names = FALSE)

# Check the first few rows of the cleaned data
head(Ill_patients_clean)
head(Ctrl_patients_clean)


# Loop through each unique patient ID and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  plot_ill_patient <- ggplot(patient_data, aes(x = x, y = y)) +
    geom_point(size = 1.5) +
    labs(
      title = paste("Spiral for Ill Patient", id),
      x = "X Coordinate",
      y = "Y Coordinate"
    ) +
    theme_minimal()
  
  # Display the plot
  print(plot_ill_patient)
}


# Loop through each unique control patient ID and create a separate plot
for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  plot_ctrl_patient <- ggplot(patient_data, aes(x = x, y = y)) +
    geom_point(size = 1.5) +
    labs(
      title = paste("Spiral for Control Patient", id),
      x = "X Coordinate",
      y = "Y Coordinate"
    ) +
    theme_minimal()
  
  # Display the plot
  print(plot_ctrl_patient)
}


# Loop through each Ill Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$pressure,
    main = paste("Pressure vs. Timestamp for Ill Patient", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Pressure",
    cex= 0.1
  )
}


# Loop through each Ill Patient and create a separate plot
for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$pressure,
    main = paste("Pressure vs. Timestamp for Ctrl Patient", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Pressure",
    cex= 0.1
  )
}