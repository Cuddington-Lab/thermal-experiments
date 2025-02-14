library('readxl')

# Define URLs for downloading the files
urls <- c(
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e9inc2-summer-constant-24-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e9inc3-nw-27.5-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e9inc4-summer-constant-27.5-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e9inc6-summer-control-24-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e10inc2-summer-constant-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e10inc3-nw-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e10inc4-summer-constant-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e10inc6-summer-control-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e10inc6-summer-control-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e11inc2-summer-control-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e11inc3-summer-nw-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e11inc4-summer-constant-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e11inc6-summer-constant-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e12inc2-summer-control-24-2.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e12inc3-summer-constant-24-2.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e12inc4-summer-nw-27.5-2.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e12inc6-summer-constant-27.5-2.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e13inc2-summer-control-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e13inc3-summer-nw.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e13inc4-summer-constant-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e13inc6-summer-constant-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e14inc2-summer-nw-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e14inc3-summer-constant-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e14inc4-summer-control-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e14inc6-summer-constant-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e15inc2-summer-constant-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e15inc3-summer-constant-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e15inc4-summer-nw-27.5.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e15inc6-summer-control-24.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e16inc2-summer-all-day-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e16inc3-summer-random-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e17inc3-summer-random.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e17inc4-summer-all-day.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e19inc3-summer-random.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e19inc4-summer-all-day.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e20inc3-summer-all-day.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e20inc6-summer-random.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e21inc4-summer-random.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e21inc6-summer-all-day.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e22inc2-summer-random-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e22inc3-summer-all-day-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e22inc4-summer-random-joint.xls",
  "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/e22inc6-summer-all-day-joint.xls"
)

# Download and read files into a list of data frames
data_list <- lapply(urls, function(url) {
  temp_file <- tempfile(fileext = ".xls")
  download.file(url, temp_file, mode = "wb")
  read_excel(temp_file, skip = 10, col_names = FALSE) # Read the Excel file
})

# Initialize an empty dataframe for storing the results
lng <- length(data_list)
cat_stats <- setNames(data.frame(matrix(ncol = 5, nrow = lng)), c("fname", "program_mean", "obs_mean", "program_sd", "obs_sd"))

# Loop through each dataset in data_list
for (i in 1:lng) {
  # Get the current data frame
  my_data <- data_list[[i]]
  colnames(my_data) <- c("Date", "Time", "Obs", "Program")  # Rename columns
  
  # Convert missing data recorded as zeros to NAs
  my_data$Obs <- ifelse(my_data$Obs == 0, NA, my_data$Obs)
  
  # Convert Date and Time to DateTime format
  my_data$DateTime <- as.POSIXct(paste(my_data$Date, my_data$Time), format="%Y-%m-%d %H:%M")
  if (is.na(my_data$DateTime[1])) {
    my_data$DateTime <- as.POSIXct(paste(my_data$Date, my_data$Time), format="%d/%m/%Y %H:%M")
  }
  
  # Remove first hour (incubator is ramping up or down)
  my_data <- my_data[-c(1:61), ]
  
  # Store the filename (for reference)
  cat_stats$fname[i] <- urls[i]  
  # Subset data to initial length of 120 time steps
  index <- seq(from = 1, to = nrow(my_data), by = 60)
  subset <- data.frame(sub_time = my_data$Time[index], 
                       sub_obs_temp = my_data$Obs[index], 
                       sub_set_temp = my_data$Program[index])
  
  # Add summary stats
  cat_stats$program_mean[i] <- mean(subset$sub_set_temp, na.rm = TRUE)
  cat_stats$obs_mean[i] <- mean(subset$sub_obs_temp, na.rm = TRUE)
  cat_stats$program_sd[i] <- sd(subset$sub_set_temp, na.rm = TRUE)
  cat_stats$obs_sd[i] <- sd(subset$sub_obs_temp, na.rm = TRUE)
}

# Write the results to a CSV file
write.csv(cat_stats, file = "C:/Users/user/Desktop/cat_stats.csv")
