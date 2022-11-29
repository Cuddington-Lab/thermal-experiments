#title: Thermal performance models for *Lemna minor*
#subtitle: Response variable: RGR
#author: Debora
#date: 11/29/2022

### Installing packages and enabling libraries
library(rTPC)
library(ggplot2)
library(nls.multstart)
library(broom)
library(dplyr)
library(AICcmodavg)
library(tidyverse)

### Download data
duckweed_constant <- read.csv("https://raw.githubusercontent.com//Cuddington-Lab/thermal-experiments/main/full_experimental_data.csv",
                              header=TRUE)

# Remove failed experiments
duckweed_constant <- duckweed_constant[!(duckweed_constant$Technical_issue=="y"),]

# Select constant temperature regimes
duckweed_constant <- subset(duckweed_constant, Thermal_regime == "constant")

# Calculate relative growth rates
duckweed_constant$Frond_count <- duckweed_constant$LM_Rep1 + duckweed_constant$LM_Rep2 + duckweed_constant$LM_Rep3
duckweed_constant$RGR <- (log(duckweed_constant$Frond_count) - log(12)) / 5

# Remove experiments which did not include duckweeds or negative growth rates
duckweed_constant <- subset(duckweed_constant, RGR >= 0)

#Subset to include only performance and temperature
duckweed_constant <- subset(duckweed_constant, select = c(Mean_Temp, RGR))

### Fit the chosen models to the duckweed data

label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}

d_fits <- nest(duckweed_constant, data = c(Mean_Temp, RGR)) %>%
  mutate(
    ratkowsky = map(data, ~nls_multstart(RGR~ratkowsky_1983(temp = Mean_Temp, tmin, tmax, a, b), data = duckweed_constant, iter = c(4,4,4,4),
                                         start_lower = get_start_vals(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                                      model_name = 'ratkowsky_1983') - 1,
                                         start_upper = get_start_vals(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                                      model_name = 'ratkowsky_1983') + 1,
                                         lower = get_lower_lims(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                                model_name = 'ratkowsky_1983'), upper = get_upper_lims(duckweed_constant$Mean_Temp, 
                                                                                                                       duckweed_constant$RGR, model_name = 'ratkowsky_1983'), supp_errors = 'Y',
                                         convergence_count = FALSE)),
    
    gaussian = map(data, ~nls_multstart(RGR~gaussian_1987(temp = Mean_Temp,rmax, topt, a),
                                       data = duckweed_constant, iter = c(4,4,4), 
                                       start_lower = get_start_vals(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                                    model_name = 'gaussian_1987') - 1, 
                                       start_upper = get_start_vals(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                                    model_name = 'gaussian_1987') + 1, lower = get_lower_lims(duckweed_constant$Mean_Temp, 
                                                                                                                             duckweed_constant$RGR, model_name = 'gaussian_1987'), 
                                       upper = get_upper_lims(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                              model_name = 'gaussian_1987'), supp_errors = 'Y', convergence_count = FALSE)),
    
    beta = map(data, ~nls_multstart(RGR~beta_2012(temp = Mean_Temp, a, b, c, d, e), data = duckweed_constant, 
                                    iter = c(6, 6, 6, 6, 6),
                                    start_lower = get_start_vals(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                                 model_name = 'beta_2012') - 10,
                                    start_upper = get_start_vals(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                                 model_name = 'beta_2012') + 10,
                                    lower = get_lower_lims(duckweed_constant$Mean_Temp, duckweed_constant$RGR, 
                                                           model_name = 'beta_2012'), upper = get_upper_lims(duckweed_constant$Mean_Temp, 
                                                                                                             duckweed_constant$RGR, model_name = 'beta_2012'), supp_errors = 'Y',
                                    convergence_count = FALSE))
          )


### Stack models
d_stack <- select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', ratkowsky:beta)


### Obtain parameters
params <- d_stack %>%
  mutate(., est = map(fit, tidy)) %>%
  select(-fit) %>%
  unnest(est)


### Obtain model predictions
newdata <- tibble(Mean_Temp = seq(min(duckweed_constant$Mean_Temp), max(duckweed_constant$Mean_Temp), length.out = 60))

d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

params1 <- d_stack %>%
  mutate(., params = map(fit, calc_params)) %>%
  select(-fit) %>%
  unnest(params)
estimates <- bind_rows(select(params1, model_name, rmax:skewness), params1) %>%
  mutate_if(is.numeric, round, 2)
estimates[!duplicated(estimates$model_name), ]


### Plot model fits
ggplot(d_preds) +
  geom_line(aes(Mean_Temp, .fitted, col = model_name),size = 1.5) +
  geom_point(aes(Mean_Temp, RGR), duckweed_constant) +
  labs(x = "Mean temperature (ºC)",
       y = "Relative growth rate (RGR)",
       title = "L. minor thermal performance curves") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))                    

### Model assessment
d_ic <- d_stack %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc)) %>%
  select(-fit) %>%
  unnest(info) %>%
  select(model_name, AICc)
d_ic <- distinct(d_ic)
d_ic %>% arrange(AICc)


### Reference
# Padfield, D., Sullivan, H., & Pawar, S. (2021). rTPC and nls.multstart: A new pipeline to fit thermal 
# performance curves in R. Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.13585