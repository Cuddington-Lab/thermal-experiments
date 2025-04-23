# Load required libraries
library(httr)
library(jsonlite)
library(readxl)
library(readr)

# GitHub API URL for the folder content
api_url <- "https://api.github.com/repos/Cuddington-Lab/thermal-experiments/contents/night_warming/night_warming_thermallog"

# Get the folder contents from GitHub API
response <- GET(api_url)

# Check if the request was successful
if (response$status_code != 200) {
  stop("Failed to retrieve folder contents from GitHub API")
}

# Parse the JSON response
files_info <- fromJSON(content(response, as = "text"))

# Filter for both .xls and .csv files
data_files <- files_info$name[grepl("\\.(xls|csv)$", files_info$name)]

# Construct the raw URLs for each file
base_raw_url <- "https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/night_warming/night_warming_thermallog/"
urls <- paste0(base_raw_url, data_files)

# Print URLs for verification
print(urls)

# Function to read files based on extension
read_file <- function(url) {
  temp_file <- tempfile(fileext = ifelse(grepl("\\.xls$", url), ".xls", ".csv"))
  download.file(url, temp_file, mode = "wb")
  
  if (grepl("\\.xls$", url)) {
    return(read_excel(temp_file, skip = 10, col_names = FALSE))
  } else if (grepl("\\.csv$", url)) {
    return(read_csv(temp_file, skip = 10, col_names = FALSE))
  }
}

# Read files into a list of data frames
data_list <- lapply(urls, read_file)

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

# Experiments with interruptions where we manually joined 2 protocols
target_fnames <- c("e4", "e9", "e12", "e16", "e22")

# Remove partial protocols for these experiments and keep joined file
cat_stats <- cat_stats[
  !grepl(paste(target_fnames, collapse = "|"), cat_stats$fname) |  # keep if fname doesn't contain target
    grepl("joint|merge", cat_stats$fname),                        # or if it does and has "joint" or "merge"
]


# Create a new column to check if program_mean differs from obs_mean by more than 1.1C
cat_stats$mean_diff_flag <- ifelse(abs(cat_stats$program_mean - cat_stats$obs_mean) > 1.1, "yes", "no")

# Create a new column to check if program_sd differs from obs_sd by more than 0.6C
cat_stats$sd_diff_flag <- ifelse(abs(cat_stats$program_sd - cat_stats$obs_sd) > 0.6, "yes", "no")

cat_stats$remarks <- ifelse(
  cat_stats$sd_diff_flag == "yes",
  "justification for keeping replicate outside SD validation: these are constant temperature treatments which had 24h interruption and remained at 20C during this period",
  ""
)

# Write the results to a CSV file
write.csv(cat_stats, file = "C:/Users/user/Desktop/cat_stats_night_warming.csv")