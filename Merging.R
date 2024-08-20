library(dplyr)


process_svc_file <- function(file_path, file_id, det) {
  lines <- readLines(file_path)
  data_lines <- lines[-1]
  data <- read.table(text = data_lines, header = FALSE, col.names = c("x", "y", "timestamp", "pen_state", "azimuth", "altitude", "pressure"))
  data$ID <- paste0(det, file_id)
  
  return(data)
}

read_svc_files <- function(file_paths, det){
  data_list <- list()
  for (i in seq_along(file_paths)) {
    data <- process_svc_file(file_paths[i], i, det)
    data_list[[i]] <- data
  }
  final_data <- do.call(rbind, data_list)

  return(final_data)
}

file_paths_Ill <- list.files(path = "./Ill/", pattern = "*.svc", full.names = TRUE)
Ill_data <- read_svc_files(file_paths_Ill, "Ill_")
write.csv(Ill_data, "Ill_patients.csv", row.names = FALSE)

file_paths_Ctrl <- list.files(path = "./Control/", pattern = "*.svc", full.names = TRUE)
Control_data <- read_svc_files(file_paths_Ctrl, "Ctrl_")
write.csv(Control_data, "Control_patients.csv", row.names = FALSE)


Ill_patients <- read.csv("Ill_patients.csv")
Ctrl_patients <- read.csv("Control_patients.csv")

head(Ill_patients)
head(Ctrl_patients)
