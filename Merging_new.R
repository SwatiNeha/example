library(dplyr)
library (ggplot2)
library(plotly)
library(scatterplot3d)
library(corrplot)
library(tidyr)
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

plot_box_plots <- function(data) {
  # Select only numeric columns
  numeric_data <- data  %>% select(where(is.numeric))
  
  # Reshape the data for ggplot2
  long_data <- numeric_data %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value")
  
  # Create box plots
  ggplot(long_data, aes(x = Variable, y = Value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Variable", y = "Value")
}

plot_box_plots2 <- function(data) {
  # Exclude timestamp column (assuming it's named "timestamp" - adjust if necessary)
  numeric_data <- data %>% select(-timestamp) %>% select(where(is.numeric))
  
  # Reshape the data for ggplot2
  long_data <- numeric_data %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value")
  
  # Create box plots
  ggplot(long_data, aes(x = Variable, y = Value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Variable", y = "Value")
}

plot_box_plots3 <- function(data) {
  # Select only numeric columns
  numeric_data <- data %>% select(timestamp) %>% select(where(is.numeric))
  
  # Reshape the data for ggplot2
  long_data <- numeric_data %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Values")
  
  # Create box plots
  ggplot(long_data, aes(x = Variable, y = Values)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(hjust = 1)) +
    labs(x = 'Variable', y = "Value (in millisec)")
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
Ill_patients <- read.csv("Ill_patients.csv", colClasses = c("pen_state" = "factor"))
Ctrl_patients <- read.csv("Control_patients.csv", colClasses = c("pen_state" = "factor"))

str(Ill_patients)
str(Ctrl_patients)
# Box plots for Ill patients
plot_box_plots(Ctrl_patients)

# Box plots for Control patients
plot_box_plots3(Ctrl_patients)

plot_box_plots3(Ill_patients)

# Box plots for Ill patients
plot_box_plots2(Ill_patients)

# Box plots for Control patients
plot_box_plots2(Ctrl_patients)


# Remove outliers from Ill patients' data
Ill_patients_clean <- remove_outliers(Ill_patients)

Ctrl_patients_clean <- remove_outliers(Ctrl_patients)

# Save the cleaned data back to CSV
write.csv(Ill_patients_clean, "Ill_patients_clean.csv", row.names = FALSE)
write.csv(Ctrl_patients_clean, "Ctrl_patients_clean.csv", row.names = FALSE)

# Check the first few rows of the cleaned data
head(Ill_patients_clean)
head(Ctrl_patients_clean)


plot_correlation_matrix <- function(data) {
  # Select only numeric columns
  numeric_data <- data %>% select(where(is.numeric))
  print(numeric_data)
  
  # Compute the correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Print correlation matrix
  print(cor_matrix)
  
  # Plot the correlation matrix without diagonal
  corrplot(cor_matrix, method = "color", type = "upper", 
           tl.cex = 0.8, tl.col = "black", 
           addCoef.col = "black", # Add correlation coefficients
           diag = FALSE,          # Remove the diagonal
           mar = c(0, 0, 1, 0))
}

# Correlation matrix and plot for Ill patients
plot_correlation_matrix(Ill_patients_clean)

# Correlation matrix and plot for Control patients
plot_correlation_matrix(Ctrl_patients_clean)


Ill_patients_clean$timestamp_new <- (Ill_patients_clean$timestamp/1000)

Ctrl_patients_clean$timestamp_new <- (Ctrl_patients_clean$timestamp/1000)

########################## Spirals ##############################

# Loop through each unique ill patient ID and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  plot(
    patient_data$x, 
    patient_data$y,
    #main = paste("Spirals for Ill Patient", id),
    xlab = "X",
    ylab = "Y",
    cex= 0.3
  )
}

# Loop through each unique control patient ID and create a separate plot
for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  plot(
    patient_data$x, 
    patient_data$y,
    #main = paste("Spirals for Control Subject", id),
    xlab = "X",
    ylab = "Y",
    cex = 0.3
  )
}

######################## X vs Timestamp ####################

# Loop through each Ill Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$x,
    #main = paste("X coordinate vs. Timestamp for Ill Patient", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "X",
    cex= 0.3
  )
}

# Loop through each Ctrl Patient and create a separate plot

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$x,
    #main = paste("X Coordinate vs. Timestamp for Ctrl Subject", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "X",
    cex= 0.3
  )
}

######################## Y vs Timestamp ####################

# Loop through each Ill Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$y,
    #main = paste("Y coordinate vs. Timestamp for Ill Patient", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Y",
    cex= 0.3
  )
}

# Loop through each Ctrl Patient and create a separate plot

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$y,
    #main = paste("Y Coordinate vs. Timestamp for Ctrl Subject", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Y",
    cex= 0.3
  )
}

######################## Pressure vs Timestamp ####################

# Loop through each Ill Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$pressure,
    #main = paste("Pressure vs. Timestamp for Ill Patient", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Pressure",
    cex= 0.3
  )
}

# Loop through each Ctrl Patient and create a separate plot

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$pressure,
    #main = paste("Pressure vs. Timestamp for Ctrl Subject", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Pressure",
    cex= 0.3
  )
}

######################## Azimuth vs Timestamp ####################

# Loop through each Control Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$azimuth,
    type="p",
    #main = paste("Azimuth vs. Timestamp for Ill Patient", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Azimuth",
    cex= 0.3
  )
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$azimuth,
    type="p",
    #main = paste("Azimuth vs. Timestamp for Ctrl Subject", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Azimuth",
    cex= 0.3
  )
}


######################## Altitude vs Timestamp ####################

# Loop through each Ill Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$altitude,
    #main = paste("Altitude vs. Timestamp for Ill Patient", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Altitude",
    cex= 0.3
  )
}

# Loop through each Ctrl Patient and create a separate plot
for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  # Create a base plot with points and lines
  plot(
    patient_data$timestamp, 
    patient_data$altitude,
    #main = paste("Altitude vs. Timestamp for Ctrl Subject", id),
    xlab = "Timestamp (milliseconds)",
    ylab = "Altitude",
    cex= 0.3
  )
}

######################## 3D Timestamp vs Trace ####################

# Loop through each Ill Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  scatterplot3d(
    patient_data$x,              # x-axis: X coordinate
    patient_data$y,              # y-axis: Y coordinate
    patient_data$timestamp_new,  # z-axis: Timestamp
    xlab = "X",
    ylab = "Y",
    zlab = "Timestamp (seconds)",
    #main = paste("3D Scatter Plot for Patient", id),
    cex.axis = 0.7
  )
}

# Loop through each Ctrl Patient and create a separate plot
for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  scatterplot3d(
    patient_data$x,              # x-axis: X coordinate
    patient_data$y,              # y-axis: Y coordinate
    patient_data$timestamp_new,  # z-axis: Timestamp
    xlab = "X",
    ylab = "Y",
    zlab = "Timestamp (seconds)",
    #main = paste("3D Scatter Plot for Patient", id),
    cex.axis = 0.7
  )
}


######################## X vs Pressure ####################

# Loop through each Ill Patient and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  plot(
    patient_data$x, 
    patient_data$pressure,
    #main = paste("X vs Pressure for",id),
    xlab = "X",
    ylab = "Pressure",
    cex= 0.3
  )
}

# Loop through each Ctrl Patient and create a separate plot
for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  plot(
    patient_data$x, 
    patient_data$pressure,
    #main = paste("X vs Pressure for",id),
    xlab = "X",
    ylab = "Pressure",
    cex= 0.3
  )
}


######################## Azimuth vs Altitude (across pen_state) ####################

# Loop through each Ill Patient and create a separate plot

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id, pen_state==1)
  
  # Create a base plot with points and lines
  plot(
    patient_data$altitude, 
    patient_data$azimuth,
    #main = paste("Azimuth vs. Altitude for Ill Patient", id),
    xlab = "Altitude",
    ylab = "Azimuth"
  )
  
}



# Loop through each Ctrl Patient and create a separate plot

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id, pen_state==1)
  
  # Create a base plot with points and lines
  plot(
    patient_data$altitude, 
    patient_data$azimuth,
    #main = paste("Azimuth vs. Altitude for Ctrl Subject", id),
    xlab = "Altitude",
    ylab = "Azimuth"
  )

}
