set.seed(1234)
library(MuMIn)
library(nls.multstart)

# Modified Ratkowsky model (sugested by Kim)
modified_ratkowsky <- function(temp, a, b, tmin, tmax) {
  a * (temp - tmin)^3 * (1 - exp(b * (temp - tmax)))^1
}

# Quadratic function
quadratic <- function(temp, a, b, c) {
  a * temp^2 + b * temp + c
}

# Hinshelwood model
hinshelwood <- function(temp, a, b, E, Eh, R = 8.314) {
  T_kelvin <- temp + 273.15
  a * exp(-E / (R * T_kelvin)) - b * exp(-Eh / (R * T_kelvin))
}

# Logistic1 Model
logistic1 <- function(temp, a, b, c, d) {
  a * exp(b * temp) - exp(c * (d - temp))
}

# Sharpe-Schoolfield Model
sharpe_schoolfield <- function(temp, k, c, a, b, R = 8.314) {
  k * (temp / 298.16) * exp(c / (R * 298.16) - c / (R * temp)) /
    (1 + exp(a / (R * (1 / temp - 1 / 298.16))) + exp(b / (R * (1 / temp - 1 / 298.16))))
}

# Ratkowsky Model
ratkowsky <- function(temp, a, b, tmin, tmax) {
  ((a * (temp - tmin) * (1 - exp(b * (temp - tmax))))^2)
}

# Briere 1 Model
briere1 <- function(temp, a, tmin, tmax) {
  a * temp * (temp - tmin) * sqrt(tmax - temp)
}

# Weibull Model
weibull <- function(temp, a, b, c, d) {
  a * ((d - 1) / d)^(1 - d) * exp(-((temp - b) / c) + ((d - 1) / d)^(1 / d))
}

# Eppley curve
eppley <- function(temp, a, b, z, w) {
  a * exp(b * temp) * (1 - ((temp - z) / (w / 2))^2)
}

# Read dataset
tpc <- read.csv("https://raw.githubusercontent.com/Cuddington-Lab/thermal-experiments/main/autocorrelation/autocorrelation_TPC.csv", 
                header=TRUE, stringsAsFactors=TRUE, fileEncoding="UTF-8-BOM")

tpc$total_living_fronds <- tpc$Frond_count_1 + tpc$Frond_count_2 + tpc$Frond_count_3
tpc$total_living_fronds[tpc$total_living_fronds == 0] <- 0.00001
tpc$r <- (log(tpc$total_living_fronds) - log(12)) / 5

# Colors for treatments
colors <- c("#0072B2","#D55E00", "grey37")
color_index <- 1
fit_results <- list()
fit_models <- list()
aicc <- list()

layout(matrix(c(1, 1, 2), nrow = 3, byrow = TRUE), heights = c(3, 1))
par(mar = c(4, 5, 1, 1) + 0.1,
    mgp = c(4, 0.5, 0))  # Adjusted margins for better spacing

# First plot
plot(
  "",  
  xlim = c(min(tpc$Mean_temperature)-2, max(tpc$Mean_temperature)+5), ylim = c(-0.5, 0.5),
  xaxt = "n", yaxt = "n", frame.plot = FALSE,
  xlab = "", ylab = ""
)

# Manually add the y-axis for the first plot
axis(2, at = seq(-0.5, 0.5, by = 0.2), col = "black", cex.axis = 2, cex.lab = 2)

# Add x-axis title
mtext("Relative growth rate per day", side = 2, line = 2.8, cex = 1.8)
mtext("//", side = 2, at = -0.5 - 0.1, cex = 2, las = 1)


# Define the correct color mapping for treatments
color_map <- c("hot-cold" = "#D55E00", "cold-hot" = "#0072B2", "no autocorrelation" = "grey37")

# Define the treatment names in the same order as in the color map
treatments_order <- c("hot-cold", "cold-hot", "no autocorrelation")

# Add legend with correct colors and line types
legend(
  "bottomleft", 
  legend = treatments_order,  # Specify the order of the treatments
  col = color_map[treatments_order],  # Assign the correct colors based on the treatment order
  lwd = 3, 
  lty = ifelse(treatments_order == "cold-hot", 2, 1),  # Dashed line for "cold-hot"
  cex = 2, 
  bty = "n"
)

# Loop through each treatment
for (treatment in unique(tpc$Treatment)) {
  tpc_treatm <- subset(tpc, Treatment == treatment)
  tpc_treatm <- data.frame(temp = as.numeric(tpc_treatm$Mean_temperature),
                           frond = as.numeric(tpc_treatm$r))
  
  # Fit modified Ratkowsky (cubic)
  modified_ratkowsky_fit <- tryCatch({
    nls_multstart(
      frond ~ modified_ratkowsky(temp, a, b, tmin, tmax),
      data = tpc_treatm,
      start_lower = list(a = 0.00001, b = 1, tmin = 0, tmax = 30),
      start_upper = list(a = 0.0001, b = 10, tmin = 10, tmax = 40),
      iter = 100, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
  # Fit Hinshelwood model
  hinshelwood_fit <- tryCatch({
    nls_multstart(
      frond ~ hinshelwood(temp, a, b, E, Eh),
      data = tpc_treatm,
      start_lower = list(a = 0.01, b = 0.01, E = 50, Eh = 50),
      start_upper = list(a = 10, b = 10, E = 200, Eh = 200),
      iter = 100, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
  # Fit Quadratic model
  quadratic_fit <- tryCatch({
    nls_multstart(
      frond ~ quadratic(temp, a, b, c),
      data = tpc_treatm,
      start_lower = list(a = -10, b = 0, c = 0),
      start_upper = list(a = 10, b = 5, c = 5),
      iter = 100, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
    # Fit Logistic1 Model
  logistic1_fit <- tryCatch({
    nls_multstart(
      frond ~ logistic1(temp, a, b, c, d),
      data = tpc_treatm,
      start_lower = list(a = 0.01, b = 0.01, c = 0.01, d = 30),
      start_upper = list(a = 10, b = 10, c = 10, d = 40),
      iter = 200, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
   # Fit Sharpe-Schoolfield Model
  sharpe_schoolfield_fit <- tryCatch({
    nls_multstart(
      frond ~ sharpe_schoolfield(temp, k, c, a, b),
      data = tpc_treatm,
      start_lower = list(k = 0.01, c = 0.01, a = 0.01, b = 0.01),
      start_upper = list(k = 10, c = 10, a = 10, b = 10),
      iter = 200, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
  # Fit Ratkowsky Model
  ratkowsky_fit <- tryCatch({
    nls_multstart(
      frond ~ ratkowsky(temp, a, b, tmin, tmax),
      data = tpc_treatm,
      start_lower = list(a = 0.01, tmin = 10, b = 0.01, tmax = 30),
      start_upper = list(a = 10, tmin = 30, b = 10, tmax = 50),
      iter = 100, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
  # Fit Briere 1 Model
  briere1_fit <- tryCatch({
    nls_multstart(
      frond ~ briere1(temp, a, tmin, tmax),
      data = tpc_treatm,
      start_lower = list(a = 0.0001, tmin = 0, tmax = 30),
      start_upper = list(a = 1, tmin = 10, tmax = 40),
      iter = 100, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
  # Fit Weibull Model
  weibull_fit <- tryCatch({
    nls_multstart(
      frond ~ weibull(temp, a, b, c, d),
      data = tpc_treatm,
      start_lower = list(a = 0.01, b = 1, c = 10, d = 30),
      start_upper = list(a = 10, b = 5, c = 50, d = 50),
      iter = 100, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
  # Fit Eppley Curve Model with z fixed at 27B0C
  eppley_fit <- tryCatch({
    nls_multstart(
      frond ~ eppley(temp, a, b, z, w),
      data = tpc_treatm,
      start_lower = list(a = 0.01, b = 0.05, z = 20, w = 5),  
      start_upper = list(a = 10, b = 0.12, z = 27, w = 30),   
      iter = 100, supp_errors = 'Y', convergence_count = FALSE
    )
  }, error = function(e) {
    return(NULL)  
  })
  
  
  color_index <- color_index %% length(colors) + 1
  
  # Compare models based on AICc
  fit_list <- list(
    Modified_Ratkowsky = modified_ratkowsky_fit,
    Hinshelwood = hinshelwood_fit,
    Quadratic = quadratic_fit,
    Logistic1 = logistic1_fit,
    Sharpe_Schoolfield = sharpe_schoolfield_fit,
    Ratkowsky = ratkowsky_fit,
    Briere1 = briere1_fit,
    Weibull = weibull_fit,
    Eppley = eppley_fit
  )
  
  fit_models[[treatment]] <- fit_list
  
  aicc_values <- sapply(fit_list, function(fit) {
    if (!is.null(fit)) AICc(fit) else Inf  
  })
  
  aicc[[treatment]] <- aicc_values
  
  best_model_name <- if (treatment == "hot-cold") {
    "Eppley"
  } else {
    "Briere1"
  }
  
  best_model <- fit_list[[best_model_name]]
  
  if (!is.null(best_model)) {
    fit_results[[treatment]] <- list(model_name = best_model_name, model = best_model)
    
    # Plot the best model
    new_data <- data.frame(temp = seq(min(tpc_treatm$temp), max(tpc_treatm$temp), by = 0.1))
    new_data$frond <- predict(best_model, newdata = new_data)
    
    if (treatment == "cold-hot") {
      lines(new_data$temp, new_data$frond, col = colors[color_index], lwd = 3, lty = 2)  
    } else {
      lines(new_data$temp, new_data$frond, col = colors[color_index], lwd = 3) 
    }
    
    points(tpc_treatm$temp + rnorm(nrow(tpc_treatm), mean = 0, sd = 0.8),
           tpc_treatm$frond, pch = 1, cex = 2, col = colors[color_index], lwd = 1)
  }
  
  color_index <- color_index + 1
  abline(h = 0, lty = 2, col = "black")
  
}

par(mar = c(5, 5, 1, 1) + 0.1,
    mgp = c(4, 1, 0)) 
# Second plot (hot-cold group: 2 very low points)
plot(
  "",  
  xlim = c(min(tpc$Mean_temperature)-2, max(tpc$Mean_temperature)+5), ylim = c(-2.9, -2.7),
  yaxt = "n", xaxt = "n", frame.plot = FALSE,
  xlab = "", ylab = ""
)

# Manually add the x-axis and y-axis for the second plot
axis(1, col = "black", cex.axis = 2, cex.lab = 2)  # x-axis with black lines
axis(2, at = -2.8, labels = -2.8, col = "black", cex.axis = 2, cex.lab = 2)  # y-axis showing only -2.8

# Add x-axis title for the second plot
mtext("Temperature (\u00B0C)", side = 1, line = 2.8, cex = 1.8)

# Add a vertical line to act as the y-axis
abline(v = min(tpc$Mean_temperature) - 2, col = "black", lwd = 1)  

# Filter for hot-cold group
tpc_hot_cold <- subset(tpc, Treatment %in% c("hot-cold"))

# Plot scatter for hot-cold group
jittered_temp <- tpc_hot_cold$Mean_temperature + rnorm(length(tpc_hot_cold$Mean_temperature), mean = 0, sd = 0.8)
points(jittered_temp, tpc_hot_cold$r, col = colors[2],
       pch = 1, cex = 2, lwd = 1)

# Add a horizontal dashed black line at y = 0
abline(h = 0, lty = 2, col = "black")


aicc[[1]]
aicc[[2]]
aicc[[3]]


library(knitr)


aicc_table <- lapply(aicc, function(treatment_aicc) {
  # Replace Inf with "No fit"
  treatment_aicc[treatment_aicc == Inf] <- "No fit"
  
  # Convert character strings back to numeric and round to 2 decimal places
  treatment_aicc <- sapply(treatment_aicc, function(x) {
    if(is.character(x)) {
      if(x == "No fit") {
        return(x)
      } else {
        return(round(as.numeric(x), 2))
      }
    } else {
      return(round(x, 2))
    }
  })
  
  return(treatment_aicc)
})

# Convert to a data frame for better readability
aicc_table_df <- as.data.frame(aicc_table)

# Print the table using kable
kable(aicc_table_df, digits = 2)

options(scipen = 999)
coef(fit_results[["cold-hot"]]$model)
coef(fit_results[["hot-cold"]]$model)
coef(fit_results[["no autocorrelation"]]$model)
