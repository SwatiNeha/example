library(dplyr)
library(plotly)
library(ggplot2)
library(corrplot)
library(scatterplot3d)
library(tidyr)
library(dbscan)
library(fpc)
library(clusterCrit)
library(mclust)
library(Rtsne)
library(cluster)
library(dendextend)
library(ggthemes)
library(RColorBrewer)
library(factoextra)
library(FNN)

par(bg = "#FFF9D9")

set.seed(123)

Ill_patients <- read.csv("Ill_patients_clean.csv", colClasses = c("pen_state" = "factor"))
Ctrl_patients <- read.csv("Ctrl_patients_clean.csv", colClasses = c("pen_state" = "factor"))

# Convert milliseconds to seconds
convert_to_seconds <- function(time_ms) {
  return(time_ms / 1000)  # Convert milliseconds to seconds
}

# Apply the conversion to the timestamp column in both datasets
Ill_patients$timestamp <- convert_to_seconds(Ill_patients$timestamp)
Ctrl_patients$timestamp <- convert_to_seconds(Ctrl_patients$timestamp)

# Calculate speed
calculate_speed <- function(x, y, time) {
  distances <- sqrt(diff(x)^2 + diff(y)^2)
  time_diffs <- diff(time)  # Already in seconds
  speed <- distances / time_diffs  # speed = distance / time
  return(c(0, speed))  # NA for the first point (no previous point to compare)
}

# Calculate acceleration
calculate_acceleration <- function(speed, time) {
  acceleration <- c(0, diff(speed) / diff(time))  # Change in speed over change in time
  return(acceleration)  # Return the acceleration values
}

# Calculate angles
calculate_angle <- function(x, y) {
  angles <- atan2(diff(y), diff(x)) * (180 / pi)  # Convert to degrees
  return(c(0, angles))  # NA for the first point
}

# Calculate pressure variation
calculate_pressure_variation <- function(pressure) {
  pressure_variation <- c(0, diff(pressure))  # Change in pressure
  return(pressure_variation)  # Return the variation values
}

# Apply speed calculation on both patient datasets
Ill_patients <- Ill_patients %>%
  arrange(timestamp) %>% 
  mutate(Speed = calculate_speed(x, y, timestamp))

Ctrl_patients <- Ctrl_patients %>%
  arrange(timestamp) %>% 
  mutate(Speed = calculate_speed(x, y, timestamp))

# Combine the datasets
patient_data <- rbind(Ill_patients, Ctrl_patients)

# Handle infinite speeds
patient_data$Speed[is.infinite(patient_data$Speed)] <- NA

# Calculate acceleration based on the newly adjusted speed and timestamp
patient_data <- patient_data %>%
  arrange(ID, timestamp) %>%
  mutate(Acceleration = calculate_acceleration(Speed, timestamp))

# Calculate angles and pressure variations remain the same
patient_data <- patient_data %>%
  arrange(ID, timestamp) %>%
  mutate(Direction = calculate_angle(x, y))

# Apply pressure variation function
patient_data <- patient_data %>%
  arrange(ID, timestamp) %>%
  mutate(Pressure_Variation = calculate_pressure_variation(pressure))

# Calculate total distance traveled
calculate_total_distance <- function(x, y) {
  distances <- sqrt(diff(x)^2 + diff(y)^2)
  return(c(0, cumsum(distances)))  # Cumulative distance
}

patient_data <- patient_data %>%
  arrange(ID, timestamp) %>%
  mutate(Total_Distance = calculate_total_distance(x, y))

# Calculate average speed during active movement
average_active_speed <- function(speed, threshold) {
  active_speeds <- speed[speed > threshold]
  return(mean(active_speeds, na.rm = TRUE))
}

# Assume a threshold for active speed
speed_threshold <- 1000  # Adjust this value as necessary
patient_data <- patient_data %>%
  group_by(ID) %>%
  mutate(Avg_Active_Speed = average_active_speed(Speed, speed_threshold))

# Calculate coefficient of variation for speed
patient_data <- patient_data %>%
  group_by(ID) %>%
  mutate(CV_Speed = sd(Speed, na.rm = TRUE) / mean(Speed, na.rm = TRUE))

# Calculate change in direction
calculate_direction_change <- function(direction) {
  return(c(0, diff(direction)))
}

patient_data <- patient_data %>%
  arrange(ID, timestamp) %>%
  mutate(Direction_Change = calculate_direction_change(Direction))

# Calculate peak speed
patient_data <- patient_data %>%
  group_by(ID) %>%
  mutate(Peak_Speed = max(Speed, na.rm = TRUE))

# Now, summarize the data with new features
patient_summary <- patient_data %>%
  group_by(ID) %>%
  summarize(
    Mean_Speed = mean(Speed, na.rm = TRUE),
    SD_Speed = sd(Speed, na.rm = TRUE),
    Mean_Acceleration = mean(Acceleration, na.rm = TRUE),
    SD_Acceleration = sd(Acceleration, na.rm = TRUE),
    Mean_Pressure_Variation = mean(Pressure_Variation, na.rm = TRUE),
    SD_Pressure_Variation = sd(Pressure_Variation, na.rm = TRUE),
    Total_Distance = max(Total_Distance, na.rm = TRUE),
    Avg_Active_Speed = mean(Avg_Active_Speed, na.rm = TRUE),
    CV_Speed = mean(CV_Speed, na.rm = TRUE),
    Mean_Direction_Change = mean(Direction_Change, na.rm = TRUE),
    Peak_Speed = max(Peak_Speed, na.rm = TRUE)
  )

# Write the summary to CSV
write.csv(patient_summary, "patient_summary.csv", row.names = FALSE)


# Normalize the summary
normalized_summary <- scale(patient_summary[, -1])


wss <- numeric(11)  # Store WSS for k from 1 to 10
for (k in 1:11) {
  kmeans_model <- kmeans(normalized_summary, centers = k, nstart = 10)
  wss[k] <- kmeans_model$tot.withinss  # Total within-cluster sum of squares
}

# Create a data frame for plotting
elbow_data <- data.frame(
  k = 1:11,
  WSS = wss
)


# Plot the elbow curve
ggplot(elbow_data, aes(x = k, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal k", 
       x = "Number of Clusters (k)", 
       y = "Total Within-Cluster Sum of Squares") +
  theme_tufte(base_size = 12) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(
    limits = c(1, 11),  # Set x-axis limits from 1 to 11
    breaks = seq(1, 11, 1)  # Set breaks for x-axis
  ) +
  scale_y_continuous(
    limits = c(floor(min(wss)), ceiling(max(wss))),  # Set y-axis limits using floor and ceiling
    breaks = seq(floor(min(wss)), ceiling(max(wss)), length.out = 5)  # Set y-axis breaks
  )+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA)
  )



patient_summary <- patient_summary %>% 
  mutate(
    Group = ifelse(ID %in% Ill_patients$ID, "Ill", "Control")
  )

kmeans_result <- kmeans(normalized_summary, centers = 2)
patient_summary$Cluster_kmeans <- kmeans_result$cluster

ggplot(patient_summary, aes(x = Mean_Speed, y = Mean_Pressure_Variation, color = factor(Cluster_kmeans))) +
  geom_point(size = 2, , alpha = 0.7) +
  labs(title = "K-Means Clustering of Patient Movement Patterns by Mean Speed", 
       x = "Mean Speed", 
       y = "Mean Pressure Variation") +
  scale_color_brewer(palette = "Dark2", name = "Cluster") +
  facet_wrap(~ Group) +
  theme_tufte(base_size = 12)+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right"  # Position the legend appropriately
  )


# Perform PCA on the numeric features in the patient_summary
pca_result <- prcomp(normalized_summary, center = TRUE, scale. = TRUE)  # Exclude ID column

# Create a data frame with PCA results and cluster assignments
pca_data <- data.frame(pca_result$x[, 1:2], Cluster = patient_summary$Cluster_kmeans)  # Assuming you have a 'Cluster' column

# Plot PCA results
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point(size = 2.5, alpha = 0.7) +
  labs(title = "PCA Plot of Clusters",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_tufte(base_size = 12) +
  scale_color_brewer(palette = "Dark2")


dist_matrix <- dist(patient_summary[,-c(1,13,14)])  # Exclude non-numeric columns
silhouette_kmeans <- silhouette(kmeans_result$cluster, dist_matrix)
kmean_mean_silhouette <- mean(silhouette_kmeans[, 3])  # Average silhouette width
print(paste("Average Silhouette Width for K-Means:", kmean_mean_silhouette))

dbi_kmeans <- intCriteria(as.matrix(normalized_summary), kmeans_result$cluster, crit = "davies_bouldin")
print(paste("Davies-Bouldin Index for K-Means:", dbi_kmeans))

dunn_kmeans <- intCriteria(as.matrix(normalized_summary), kmeans_result$cluster, crit = "dunn")
print(paste("Dunn Index for K-Means:", dunn_kmeans))


####################################################
#kNNdistplot(normalized_summary, k = 3)

knn_distances <- knn.dist(normalized_summary, k = 3)

knn_df <- data.frame(
  index = 1:nrow(normalized_summary),
  distance = sort(knn_distances[, 3])  # Sort by 3rd nearest neighbor
)

x_lim <- c(0, nrow(knn_df))  # Set the x-axis limits based on the number of data points
y_lim <- c(min(knn_df$distance), max(knn_df$distance))

# Plot using ggplot2 with Tufte's theme and color-blind accessible colors
ggplot(knn_df, aes(x = index, y = distance)) +
  geom_line() +  # Dark2 color-blind friendly
  geom_point() +  # Dark2 color-blind friendly
  labs(title = "kNN Distance Plot (k = 3)", 
       x = "Data Points", 
       y = "Distance to 3rd Nearest Neighbor") +
  theme_tufte(base_size = 12)+
  scale_x_continuous(limits = x_lim) +  # Apply x-axis limits
  scale_y_continuous(limits = y_lim)+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA)
  )  

dbscan_result <- dbscan(normalized_summary, eps = 2, MinPts = 3)

patient_summary$Cluster_DBSCAN <- as.factor(dbscan_result$cluster)

ggplot(patient_summary, aes(x = Mean_Speed, y = Mean_Pressure_Variation, color = factor(Cluster_DBSCAN))) +
  geom_point(size = 2, , alpha = 0.7) +
  labs(title = "DBSCAN Clustering of Patient Movement Patterns by Mean Speed", 
       x = "Mean Speed", 
       y = "Mean Pressure Variation") +
  scale_color_brewer(palette = "Dark2", name = "Cluster") +
  facet_wrap(~ Group)+
  theme_tufte(base_size = 12)+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right"  # Position the legend appropriately
  )

#ggplotly(plot_speed)


# Perform PCA on the numeric features in the patient_summary
pca_result <- prcomp(normalized_summary, center = TRUE, scale. = TRUE)  # Exclude ID column

# Create a data frame with PCA results and cluster assignments
pca_data <- data.frame(pca_result$x[, 1:2], Cluster = patient_summary$Cluster_DBSCAN)  # Assuming you have a 'Cluster' column

# Plot PCA results
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point(size = 2.5, alpha = 0.7) +
  labs(title = "PCA Plot of DBSCAN Clusters",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_tufte(base_size = 12) +
  scale_color_brewer(palette = "Dark2")+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA),
    text = element_text(family = "serif")
  )


dist_matrix <- dist(patient_summary[,-c(1,13,14,15)])  # Exclude non-numeric columns
silhouette_dbscan <- silhouette(dbscan_result$cluster, dist_matrix)
dbscan_mean_silhouette <- mean(silhouette_dbscan[, 3])  # Average silhouette width
print(paste("Average Silhouette Width for DBSCAN:", dbscan_mean_silhouette))

dbscan_clusters <- as.integer(dbscan_result$cluster)
dbi_dbscan <- intCriteria(as.matrix(normalized_summary), dbscan_clusters, crit = "davies_bouldin")
print(paste("Davies-Bouldin Index for DBSCAN:", dbi_dbscan))

dunn_dbscan <- intCriteria(as.matrix(normalized_summary), dbscan_clusters, crit = "dunn")
print(paste("Dunn Index for DBSCAN:", dunn_dbscan))

######################
dist_matrix <- dist(normalized_summary)  # Calculate distance matrix
ahc_result <- agnes(dist_matrix, method = "ward")  # Ward's method is commonly used


max_clusters=11
wss <- numeric(max_clusters)
for (k in 2:max_clusters) {
  cluster_cut <- cutree(agnes(dist_matrix, method = "ward"), k)
  wss[k] <- sum(sapply(1:k, function(i) sum(dist(normalized_summary[cluster_cut == i, ])^2)))
}

# Plot WSS to identify the "elbow"
wss_df <- data.frame(k = 2:max_clusters, WSS = wss[2:max_clusters])

ggplot(wss_df, aes(x = k, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares") +
  theme_tufte(base_size = 12) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(
    limits = c(min(wss_df$k), max(wss_df$k)),
    breaks = seq(min(wss_df$k), max(wss_df$k), by = 1)
  ) +
  scale_y_continuous(
    limits = c(floor(min(wss_df$WSS)), ceiling(max(wss_df$WSS))),
    breaks = seq(floor(min(wss_df$WSS)), ceiling(max(wss_df$WSS)), length.out = 5)
  )+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA)
  )

# Plot the dendrogram
dendrogram <- as.dendrogram(ahc_result)

dend_col <- brewer.pal(8, "Dark2")

plot_dendrogram <- function(dendrogram, k = 5, dend_col = brewer.pal(8, "Dark2")) {
  
  
  colored_dendrogram <- color_branches(dendrogram, k = k)
  
  # Plot the base dendrogram with custom settings
  plot(
    colored_dendrogram, 
    main = "Dendrogram of Agglomerative Hierarchical Clustering", 
    yaxt = "s", xaxt = "n",  # Show y-axis but hide x-axis ticks for a minimalistic look
    bty = "n",  # Remove the box around the plot
    sub = "", xlab = "Height", ylab = "Patients",  # Remove unnecessary labels
    cex.main = 1.2,  # Adjust main title size to match base_size = 12
    cex.axis = 0.9,  # Adjust axis text size
    cex.lab = 1.0,   # Adjust any label sizes
    font.main = 1,    # Make the main title non-bold (default plain)
    family = "serif"
  )

}

plot_dendrogram(dendrogram, k = 5)

# Cut the tree at a certain height to form clusters (let's say we want 3 clusters)
cluster_cut <- cutree(ahc_result, k = 5)  # Define number of clusters (e.g., k = 3)

# Add cluster assignments to the PCA data for visualization
patient_summary$Cluster_AHC <- factor(cluster_cut)

ggplot(patient_summary, aes(x = Mean_Speed, y = Mean_Pressure_Variation, color = factor(Cluster_AHC))) +
  geom_point(size = 2, , alpha = 0.7) +
  labs(title = "AHC Clustering of Patient Movement Patterns by Mean Speed", 
       x = "Mean Speed", 
       y = "Mean Pressure Variation") +
  scale_color_brewer(palette = "Dark2", name = "Cluster") +
  facet_wrap(~ Group)+
  theme_tufte(base_size = 12)+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    text = element_text(family = "serif")# Position the legend appropriately
  )

#ggplotly(plot_speed)

write.csv(patient_summary, "patient_summary_new.csv", row.names = FALSE)

pca_result <- prcomp(normalized_summary, center = TRUE, scale. = TRUE)  # Exclude ID column

# Create a data frame with PCA results and cluster assignments
pca_data <- data.frame(pca_result$x[, 1:2], Cluster = patient_summary$Cluster_AHC)  # Assuming you have a 'Cluster' column

# Plot PCA results
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point(size = 2.5, alpha = 0.7) +
  labs(title = "PCA Plot of AHC Clusters",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_tufte(base_size = 12) +
  scale_color_brewer(palette = "Dark2")+
  theme(
    panel.background = element_rect(fill = "#FFF9D9", color = NA)
  )

dist_matrix <- dist(patient_summary[,-c(1,13,14,15,16)]) 
ahc_result <- agnes(dist_matrix, method = "ward")
cluster_cut <- cutree(ahc_result, k = 5)
silhouette_ahc <- silhouette(cluster_cut, dist_matrix)
ahc_mean_silhouette <- mean(silhouette_ahc[, 3])  # Average silhouette width
print(paste("Average Silhouette Width for AHC:", ahc_mean_silhouette))

dbi_ahc <- intCriteria(as.matrix(normalized_summary), cluster_cut, crit = "davies_bouldin")
print(paste("Davies-Bouldin Index for AHC:", dbi_ahc))

dunn_ahc <- intCriteria(as.matrix(normalized_summary), cluster_cut, crit = "dunn")
print(paste("Dunn Index for AHC:", dunn_ahc))





# Compute ARI for each clustering method by comparing with the ground truth ("Group")
# Ground truth labels (Ill=1, Control=0)
true_labels <- as.integer(patient_summary$Group == "Ill")

print(true_labels)
# Adjusted Rand Index (ARI) for K-Means
ari_kmeans <- adjustedRandIndex(true_labels, patient_summary$Cluster_kmeans)
print(paste("ARI for K-Means:", ari_kmeans))

# ARI for DBSCAN
ari_dbscan <- adjustedRandIndex(true_labels, as.integer(patient_summary$Cluster_DBSCAN))
print(paste("ARI for DBSCAN:", ari_dbscan))

# ARI for AHC
ari_ahc <- adjustedRandIndex(true_labels, as.integer(patient_summary$Cluster_AHC))
print(paste("ARI for AHC:", ari_ahc))



# Compute t-SNE
tsne_result <- Rtsne(normalized_summary, dims = 2, perplexity = 5, verbose = TRUE, max_iter = 500)

plot_tsne <- function(tsne_result, cluster_labels, cluster_method_name) {
  
  # Create a t-SNE data frame
  tsne_data <- data.frame(
    X1 = tsne_result$Y[, 1], 
    X2 = tsne_result$Y[, 2], 
    Cluster = as.factor(cluster_labels)
  )
  
  # Generate t-SNE plot
  tsne_plot <- ggplot(tsne_data, aes(x = X1, y = X2, color = Cluster)) +
    geom_point(size = 2.5) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = paste("t-SNE Plot for", cluster_method_name, "Clustering"), 
      x = "t-SNE Dimension 1", 
      y = "t-SNE Dimension 2"
    ) +
    theme_tufte(base_size = 12) +  # Tufte theme for minimalistic styling
    theme(
      panel.background = element_rect(fill = "#FFF9D9", color = NA),  # Tufte-inspired background color
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
  
  return(tsne_plot)
}

# Call the function for each clustering method

# For K-Means clustering
kmeans_plot <- plot_tsne(tsne_result, patient_summary$Cluster_kmeans, "K-Means")
print(kmeans_plot)

# For DBSCAN clustering
dbscan_plot <- plot_tsne(tsne_result, patient_summary$Cluster_DBSCAN, "DBSCAN")
print(dbscan_plot)

# For AHC clustering
ahc_plot <- plot_tsne(tsne_result, patient_summary$Cluster_AHC, "AHC")
print(ahc_plot)


plot_silhouette <- function(silhouette_result, title) {
  
  # Define color palette (color-blind friendly)
  color_palette <- brewer.pal(n = 8, name = "Dark2")
  
  # Generate silhouette plot
  silhouette_plot <- fviz_silhouette(silhouette_result) +
    scale_fill_manual(values = color_palette) +  # Apply color-blind friendly palette
    theme_tufte(base_size = 12) +  # Apply Tufte's minimalist theme
    labs(title = title, 
         x = "Silhouette Width", y = "Cluster") +
    theme(
      panel.background = element_rect(fill = "#FFF9D9", color = NA),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 8),
      text = element_text(family = "serif") 
    )
  
  return(silhouette_plot)
}


# For K-Means clustering
silhouette_kmeans_plot <- plot_silhouette(silhouette_kmeans, "Silhouette Plot for K-Means Clustering")
print(silhouette_kmeans_plot)

# For DBSCAN clustering
silhouette_dbscan_plot <- plot_silhouette(silhouette_dbscan, "Silhouette Plot for DBSCAN Clustering")
print(silhouette_dbscan_plot)

# For AHC clustering
silhouette_ahc_plot <- plot_silhouette(silhouette_ahc, "Silhouette Plot for AHC Clustering")
print(silhouette_ahc_plot)

