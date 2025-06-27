
# Load and prepare data
data <- read.csv("C:/Users/user/Desktop/Greenhouse_experiment/greenhouse_2025_hourly_temps.csv", stringsAsFactors = FALSE)
data$datetime <- as.POSIXct(paste(data$Date, data$Time), format = "%m/%d/%Y %I:%M:%S %p")
cont_cols <- paste0("cont", 1:9)
data[cont_cols] <- lapply(data[cont_cols], as.numeric)
exps <- 2:5
data <- data[data$exp %in% exps, ]

library(dplyr)
library(tidyr)

long_data <- data %>%
  pivot_longer(cols = starts_with("cont"), names_to = "container", values_to = "temp") %>%
  mutate(treatment = case_when(
    container %in% c("cont1", "cont2", "cont3") ~ "night-warming",
    container %in% c("cont4", "cont5", "cont6") ~ "random warming",
    container %in% c("cont7", "cont8", "cont9") ~ "ambient"
  )) %>%
  group_by(exp, datetime, treatment) %>%
  summarise(temp = mean(temp, na.rm = TRUE), .groups = "drop")

# Treatment styles
colors <- c("ambient" = "#1f77b4", "night-warming" = "#ff7f0e", "random warming" = "darkred")
line_types <- c("ambient" = 1, "night-warming" = 1, "random warming" = 2)
treatment_labels <- names(colors)

# Plot layout with space for legend at bottom
par(mfrow = c(1, 4),
    mar = c(4, 1, 3, 0.5),
    oma = c(5, 5, 1, 1),
    cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6,
    xaxs = "i", yaxs = "i")

# Plot panels
for (i in seq_along(exps)) {
  e <- exps[i]
  subset <- long_data[long_data$exp == e, ]
  datetime_range <- range(subset$datetime)
  
  plot(NA, xlim = datetime_range, ylim = c(10, 35),
       xlab = "Date", ylab = "", main = paste("Experiment", e),
       axes = FALSE, xaxs = "i", yaxs = "i")
  
  axis.POSIXct(1, at = pretty(subset$datetime), format = "%b %d", cex.axis = 1)
  
  if (i == 1) {
    axis(2, las = 1)
    mtext("Temperature (°C)", side = 2, line = 3.5, cex = 1.4)
  }
  
  for (treat in treatment_labels) {
    sub_treat <- subset[subset$treatment == treat, ]
    if (nrow(sub_treat) > 0) {
      lines(sub_treat$datetime, sub_treat$temp,
            col = colors[treat], lty = line_types[treat], lwd = 2)
    }
  }
  
  box(lwd = 2.5)
}

par(xpd = NA)
legend("bottom", inset = c(0.85, -0.15),
       legend = treatment_labels,
       col = colors[treatment_labels],
       lty = line_types[treatment_labels],
       lwd = 3, horiz = TRUE, bty = "n",
       cex = 1.5, y.intersp = 1.2, x.intersp = 1.5)



par(mar = c(2, 2, 2, 2))  # add some margin around the plot
plot.new()
legend("center",
       legend = treatment_labels,
       col = colors[treatment_labels],
       lty = line_types[treatment_labels],
       lwd = 4,
       horiz = TRUE,
       bty = "n",
       cex = 1.4,
       x.intersp = 3,     # increase horizontal space between legend items
       y.intersp = 2,     # increase vertical space if multiline
       xjust = 0.5,      # center justification
       adj = 0.3          # center label alignment
)
