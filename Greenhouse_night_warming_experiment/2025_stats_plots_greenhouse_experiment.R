data <- read.csv("C:/Users/user/Desktop/Greenhouse_experiment/greenhouse_2025.csv", stringsAsFactors = FALSE)

data$treatment <- gsub("^nw$", "night-warming", data$treatment)
data$treatment <- gsub("^random$", "random warming", data$treatment)
data$treatment <- as.factor(data$treatment)

data$replicate <- factor(rep(1:9, length.out = nrow(data)))

clean_data <- subset(data, exclusion != "y")
clean_data$area <- clean_data$final_area - clean_data$initial_area

bar_cols <- c("ambient" = "#1f77b4", "night-warming" = "#ff7f0e", "random warming" = "darkred")

dates <- unique(clean_data$start_date)
n_trials <- length(dates)

biomass_ylim <- c(0, 0.2)
area_ylim <- c(0, 120)

par(mfrow = c(2, 3), mar = c(2, 6, 1, 0.5), oma = c(0, 0, 4, 0),
    cex.axis = 1.2, cex.lab = 1.3, cex.main = 1.4)

biomass_counter <- 0
for (i in seq_len(n_trials)) {
  d <- dates[i]
  subset_data <- subset(clean_data, start_date == d)
  subset_data <- subset_data[!is.na(subset_data$biomass), ]
  
  if (nrow(subset_data) == 0) next
  
  biomass_counter <- biomass_counter + 1
  
  mean_sd <- aggregate(biomass ~ treatment, data = subset_data,
                       FUN = function(x) c(mean = mean(x), sd = sd(x)))
  mean_sd <- do.call(data.frame, mean_sd)
  colnames(mean_sd) <- c("treatment", "mean", "sd")
  
  ylab <- if (biomass_counter == 1) "Mean biomass (cm²)" else ""
  yaxt <- if (biomass_counter == 1) "s" else "n"
  
  bar_centers <- barplot(mean_sd$mean, names.arg = FALSE,
                         ylim = biomass_ylim, yaxt = yaxt,
                         col = bar_cols[mean_sd$treatment],
                         ylab = ylab,
                         main = paste("Trial", biomass_counter),
                         cex.axis = 1.2, cex.lab = 1.3)
  
  arrows(x0 = bar_centers, y0 = mean_sd$mean - mean_sd$sd,
         x1 = bar_centers, y1 = mean_sd$mean + mean_sd$sd,
         angle = 90, code = 3, length = 0.05)
  
  if (biomass_counter == 1) {
    mtext("Trial 1, 2, 3", outer = TRUE, side = 3, line = 2, cex = 1.6)
  }
}

area_counter <- 0
for (i in seq_len(n_trials)) {
  d <- dates[i]
  subset_data <- subset(clean_data, start_date == d)
  subset_data <- subset_data[!is.na(subset_data$area), ]
  
  if (nrow(subset_data) == 0) next
  
  area_counter <- area_counter + 1
  
  mean_sd_area <- aggregate(area ~ treatment, data = subset_data,
                            FUN = function(x) c(mean = mean(x), sd = sd(x)))
  mean_sd_area <- do.call(data.frame, mean_sd_area)
  colnames(mean_sd_area) <- c("treatment", "mean", "sd")
  
  ylab <- if (area_counter == 1) "Mean area (cm²)" else ""
  yaxt <- if (area_counter == 1) "s" else "n"
  
  bar_centers <- barplot(mean_sd_area$mean, names.arg = FALSE,
                         ylim = area_ylim, yaxt = yaxt,
                         col = bar_cols[mean_sd_area$treatment],
                         ylab = ylab,
                         main = "",
                         cex.axis = 1.2, cex.lab = 1.3)
  
  arrows(x0 = bar_centers, y0 = mean_sd_area$mean - mean_sd_area$sd,
         x1 = bar_centers, y1 = mean_sd_area$mean + mean_sd_area$sd,
         angle = 90, code = 3, length = 0.05)
}

plot.new()

legend("center", legend = c("ambient", "night-warming", "random warming"),
       fill = c("#1f77b4", "#ff7f0e", "darkred"),
       border = NA,
       cex = 2,
       bty = "n",  # no box
       horiz = FALSE,  # vertical layout
       inset = 0.05)