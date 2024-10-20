library(dplyr)
library (ggplot2)
library(plotly)
library(scatterplot3d)
library(corrplot)
library(tidyr)
library(ggthemes)
library(RColorBrewer)
library(reshape2)

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

color_palette <- brewer.pal(8, "RdPu")

plot_box_plots <- function(data) {
  # Select only numeric columns
  numeric_data <- data  %>% select(where(is.numeric))
  
  # Reshape the data for ggplot2
  long_data <- numeric_data %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value")
  
  # Create box plots
  ggplot(long_data, aes(x = Variable, y = Value)) +
    geom_boxplot() +
    theme_tufte(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "lightyellow", color = NA),
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
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
    theme_tufte(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "lightyellow", color = NA),
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    labs(x = "Variable", y = "Value")
}

plot_box_plots3 <- function(data) {
  # Select only numeric columns
  numeric_data <- data %>% select(timestamp) %>% select(where(is.numeric))
  
  # Reshape the data for ggplot2
  long_data <- numeric_data %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Values")
  
  y_limits <- range(long_data$Values, na.rm = TRUE)
  print(floor(y_limits[1]))
  
  ggplot(long_data, aes(x = Variable, y = Values)) +
    geom_boxplot() +
    theme_tufte(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "lightyellow", color = NA),
      axis.text.x = element_text(hjust = 1)
    ) +
    labs(x = "Variable", y = "Value (in millisec)")+
    scale_y_continuous(
      limits = y_limits,  # Set the y-axis limits
      breaks = seq(from = ceiling(y_limits[1]), to = ceiling(y_limits[2])+1, by = (ceiling(y_limits[2]) - floor(y_limits[1])) / 3),  # Set breaks dynamically
      expand = expansion(mult = c(0, 0.05))  # Add a small expansion on top of the scale
    )
}

# Processing Ill patients' files
file_paths_Ill <- list.files(path = "./data_new/Ill/", pattern = "*.svc", full.names = TRUE)
Ill_data <- read_svc_files(file_paths_Ill, "Ill_")
write.csv(Ill_data, "Ill_patients.csv", row.names = FALSE)

# Processing Control patients' files
file_paths_Ctrl <- list.files(path = "./data_new/Control/", pattern = "*.svc", full.names = TRUE)
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
  
  # Normalize (center and scale) the data
  normalized_data <- numeric_data %>% mutate(across(everything(), scale))
  
  # Compute the correlation matrix
  cor_matrix <- cor(normalized_data, use = "complete.obs")
  
  # Melt the correlation matrix into a long format
  cor_data <- melt(cor_matrix)
  colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
  
  # Filter out the lower triangle and diagonal
  cor_data <- cor_data %>% 
    filter(as.numeric(Variable1) > as.numeric(Variable2))
  
  # Create the correlation heatmap with ggplot2
  ggplot(cor_data, aes(x = Variable2, y = Variable1, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 3) +  # Add correlation values with 2 decimal points
    scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), name = "Correlation") +
    theme_tufte(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "lightyellow", color = NA),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text.y = element_text(hjust = 1),
      panel.background = element_blank()
    ) +
    labs(x = "", y = "", title = "Correlation Matrix (Upper Triangle, Normalized & Centered)")
}


# Correlation matrix and plot for Ill patients
plot_correlation_matrix(Ill_patients_clean)

# Correlation matrix and plot for Control patients
plot_correlation_matrix(Ctrl_patients_clean)


Ill_patients_clean$timestamp_new <- (Ill_patients_clean$timestamp/1000)

Ctrl_patients_clean$timestamp_new <- (Ctrl_patients_clean$timestamp/1000)

########################## Spirals ##############################

plot_patient_spiral <- function(patient_data, patient_id, patient_type) {
  ggplot(patient_data, aes(x = x, y = y)) +
    geom_point(color = "black", size = 0.3) +
    theme_tufte(base_size = 12) +
    labs(
      title = paste("Spirals for", patient_type, "Patient", patient_id),
      x = "X",
      y = "Y"
    ) +
    theme(
      plot.background = element_rect(fill = "lightyellow", color = NA),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(color = "black")
    )
}

# Loop through each unique ill patient ID and create a separate plot
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  print(plot_patient_spiral(patient_data, id, "Ill"))
}

# Loop through each unique control patient ID and create a separate plot
for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  print(plot_patient_spiral(patient_data, id, "Control"))
}




plot_with_tufte_theme <- function(data, x_col, y_col, x_label, y_label, plot_title) {
  # Get min and max for x and y columns
  x_min <- min(data[[x_col]], na.rm = TRUE)
  x_max <- max(data[[x_col]], na.rm = TRUE)
  y_min <- min(data[[y_col]], na.rm = TRUE)
  y_max <- max(data[[y_col]], na.rm = TRUE)
  
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(color = "black", size = 1.0) +
    theme_tufte(base_size = 12) +
    labs(
      x = x_label,
      y = y_label,
      title = plot_title
    ) +
    theme(
      plot.background = element_rect(fill = "lightyellow", color = NA),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(color = "black")
    ) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))  # Set limits
}

######################## X vs Timestamp ####################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "x", 
                              "Timestamp (milliseconds)", "X", 
                              paste("X vs Timestamp for Ill Patient", id)))
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "x", 
                              "Timestamp (milliseconds)", "X", 
                              paste("X vs Timestamp for Ctrl Patient", id)))
}

######################## Y vs Timestamp ####################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "y", 
                              "Timestamp (milliseconds)", "Y", 
                              paste("Y vs Timestamp for Ill Patient", id)))
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "y", 
                              "Timestamp (milliseconds)", "Y", 
                              paste("Y vs Timestamp for Ctrl Patient", id)))
}

######################## Pressure vs Timestamp ########################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "pressure", 
                              "Timestamp (milliseconds)", "Pressure", 
                              paste("Pressure vs Timestamp for Ill Patient", id)))
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "pressure", 
                              "Timestamp (milliseconds)", "Pressure", 
                              paste("Pressure vs Timestamp for Ctrl Patient", id)))
}

######################## Azimuth vs Timestamp ########################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "azimuth", 
                              "Timestamp (milliseconds)", "Azimuth", 
                              paste("Azimuth vs Timestamp for Ill Patient", id)))
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "azimuth", 
                              "Timestamp (milliseconds)", "Azimuth", 
                              paste("Azimuth vs Timestamp for Ctrl Patient", id)))
}

######################## Altitude vs Timestamp ########################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "altitude", 
                              "Timestamp (milliseconds)", "Altitude", 
                              paste("Altitude vs Timestamp for Ill Patient", id)))
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "timestamp", "altitude", 
                              "Timestamp (milliseconds)", "Altitude", 
                              paste("Altitude vs Timestamp for Ctrl Patient", id)))
}

######################## 3D Timestamp vs Trace ########################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  scatterplot3d(
    patient_data$x, patient_data$y, patient_data$timestamp_new,
    xlab = "X", ylab = "Y", zlab = "Timestamp (seconds)",
    main = paste("3D Scatter Plot for Ill Patient", id),
    cex.axis = 0.7, pch = 19
  )
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  scatterplot3d(
    patient_data$x, patient_data$y, patient_data$timestamp_new,
    xlab = "X", ylab = "Y", zlab = "Timestamp (seconds)",
    main = paste("3D Scatter Plot for Ctrl Patient", id),
    cex.axis = 0.7, pch = 19
  )
}

######################## X vs Pressure ########################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "x", "pressure", 
                              "X", "Pressure", 
                              paste("X vs Pressure for Ill Patient", id)))
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  print(plot_with_tufte_theme(patient_data, "x", "pressure", 
                              "X", "Pressure", 
                              paste("X vs Pressure for Ctrl Patient", id)))
}

######################## Azimuth vs Altitude (pen_state == 1) ########################

for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id, pen_state == 1)
  print(plot_with_tufte_theme(patient_data, "altitude", "azimuth", 
                              "Altitude", "Azimuth", 
                              paste("Azimuth vs Altitude for Ill Patient", id)))
}

for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id, pen_state == 1)
  print(plot_with_tufte_theme(patient_data, "altitude", "azimuth", 
                              "Altitude", "Azimuth", 
                              paste("Azimuth vs Altitude for Ctrl Patient", id)))
}


# Set background color, font family, and adjust text size globally
par(bg = "lightyellow", family = "serif", cex = 1.2, cex.lab = 1, cex.axis = 0.6)  # Smaller axis and label text

# 3D Scatter Plot for Ill patients with Tufte-inspired theme
for(id in unique(Ill_patients_clean$ID)) {
  patient_data <- Ill_patients_clean %>% filter(ID == id)
  
  scatterplot3d(
    patient_data$x, patient_data$y, patient_data$timestamp_new,
    xlab = "X", ylab = "Y", zlab = "Timestamp (seconds)",
    cex.axis = 0.6, size = 0.3, color = "black",  # Smaller axis text and colorblind-friendly black markers
    grid = TRUE, box = FALSE  # Tufte-inspired minimalist design
  )
}


for(id in unique(Ctrl_patients_clean$ID)) {
  patient_data <- Ctrl_patients_clean %>% filter(ID == id)
  
  scatterplot3d(
    patient_data$x, patient_data$y, patient_data$timestamp_new,
    xlab = "X", ylab = "Y", zlab = "Timestamp (seconds)",
    cex.axis = 0.6, size = 0.3, color = "black",  # Smaller axis text and colorblind-friendly black markers
    grid = TRUE, box = FALSE  # Tufte-inspired minimalist design
  )
}
