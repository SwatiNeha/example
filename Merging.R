library(dplyr)
library (ggplot2)
library(plotly)
library(scatterplot3d)


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
write.csv(Ctrl_patients_clean, "Ctrl_patients_clean.csv", row.names = FALSE)

# Check the first few rows of the cleaned data
head(Ill_patients_clean)
head(Ctrl_patients_clean)

Ctrl_patients_clean$x <- as.numeric(Ctrl_patients_clean$x)
Ctrl_patients_clean$y <- as.numeric(Ctrl_patients_clean$y)
########################## Overall Plots ##############################

# Define a function to create all seven plots for a given patient ID
create_patient_plots <- function(id, data, type) {
  # Set up the 3x3 layout
  par(mfrow = c(3, 3))
  
  patient_data <- data %>% filter(ID == id)
  
  # Plot 1: Spirals for Ill Patients
  plot(
    patient_data$x, 
    patient_data$y,
    main = paste("Spirals for",type, id),
    xlab = "X",
    ylab = "Y",
    cex = 0.3
  )
  
  # Plot 2: X coordinate vs. Timestamp
  plot(
    patient_data$timestamp, 
    patient_data$x,
    main = paste("X coordinate vs. Timestamp for",type, id),
    xlab = "Timestamp (milliseconds)",
    ylab = "X",
    cex = 0.3
  )
  
  # Plot 3: Y coordinate vs. Timestamp
  plot(
    patient_data$timestamp, 
    patient_data$y,
    main = paste("Y coordinate vs. Timestamp for",type, id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Y",
    cex = 0.3
  )
  
  # Plot 4: Pressure vs. Timestamp
  plot(
    patient_data$timestamp, 
    patient_data$pressure,
    main = paste("Pressure vs. Timestamp for",type, id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Pressure",
    cex = 0.3
  )
  
  # Plot 5: Azimuth vs. Timestamp
  plot(
    patient_data$timestamp, 
    patient_data$azimuth,
    type = "p",
    main = paste("Azimuth vs. Timestamp for",type, id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Azimuth",
    cex = 0.3
  )
  
  # Plot 6: Altitude vs. Timestamp
  plot(
    patient_data$timestamp, 
    patient_data$altitude,
    main = paste("Altitude vs. Timestamp for",type, id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Altitude",
    cex = 0.3
  )
  
  # Plot 7: Azimuth vs. Altitude
  colors <- c("red", "blue") # Adjust the colors and add more if needed
  pen_states <- unique(patient_data$pen_state)
  col <- colors[match(patient_data$pen_state, pen_states)]
  
  plot(
    patient_data$altitude, 
    patient_data$azimuth,
    main = paste("Azimuth vs. Altitude for",type, id),
    xlab = "Altitude",
    ylab = "Azimuth",
    col = col
  )
  legend("topleft", legend = pen_states, col = colors, pch = 10, 
         title = "Pen State",
         cex = 0.8,           # Adjust text size (smaller)
         pt.cex = 0.7,        # Adjust point size (smaller)
         box.lwd = 1.5,       # Line width of the legend box
         box.lty = "solid",   # Line type of the legend box
         inset = 0.02)
  
  # Plot 8: 3D Time vs trace
  scatterplot3d(
    patient_data$x,              # x-axis: X coordinate
    patient_data$y,              # y-axis: Y coordinate
    patient_data$timestamp,      # z-axis: Timestamp
    xlab = "X coordinate",
    ylab = "Y coordinate",
    zlab = "Timestamp (milliseconds)",
    main = paste("3D Scatter Plot for",type, id),
    cex.axis = 0.7
  )
  
  # Plot 9: X vs Pressure
    plot(
      patient_data$x, 
      patient_data$pressure,
      main = paste("X vs Pressure for",type, id),
      xlab = "X",
      ylab = "Pressure",
      cex= 0.3
    )
}
 


# Loop through the first 10 unique patient IDs and create separate plots
patient_ids <- unique(Ill_patients_clean$ID)

for (id in patient_ids) {
  # Create a new plot window
  dev.new()
  
  # Generate all plots for the current patient ID
  create_patient_plots(id, Ill_patients_clean, type= "Ill Patients")
  readline(prompt = "Press [Enter] to continue to the next patient...")
}

ctrl_ids <- unique(Ctrl_patients_clean$ID)

for (id in ctrl_ids) {
  # Create a new plot window
  dev.new()
  
  # Generate all plots for the current patient ID
  create_patient_plots(id, Ctrl_patients_clean, type= "Control Subjects")
  readline(prompt = "Press [Enter] to continue to the next subject...")
}
