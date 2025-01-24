# Literature data for development rate (1/development time) of
# aphids
# Data collected and code by: Jonathan Fan, lab technician, 2022



library(ggplot2)

# creating data frames for each data source 
 aphid_mastoi <- data.frame(
  reference = c(rep(c("Mastoi"), times=150)),
  species = c(rep(c("A. pisum"), times=150)),
  temp = c(rep(c(27), times=30), rep(c(30), times=30), rep(c(33), times=30), rep(c(36), times=30), rep(c(39), times=30)),
  devtime = c(rnorm(n=30, mean=7, sd=0.2), rnorm(n=30, mean=7.2, sd=0.3), rnorm(n=30, mean=7.2, sd=0.3), rnorm(n=30, mean=6.3, sd=0.1), rnorm(n=30, mean=7.9, sd=0.3))
  )
 aphid_mastoi = transform(aphid_mastoi, devrate = 1/aphid_mastoi$devtime)

 aphid_asin = data.frame(
 reference = c(rep(c("Asin"), times=674)),
 species = c(rep(c("R. padi"), times=320), rep(c("S. avenae"), times=228), rep(c("M. dirhodum"), times=126)),
 temp = c(rep(c(18), times=71), rep(c(22), times=70), rep(c(25), times=62), rep(c(27.5), times=76), rep(c(30), times=41), rep(c(18), times=72), rep(c(22), times=53), rep(c(25), times=59), rep(c(27.5), times=44), rep(c(18), times=46), rep(c(22), times=40), rep(c(25), times=40)),
 devtime = c(rnorm(n=71, mean=8.8, sd=0.18), rnorm(n=70, mean=6.2, sd=0.1), rnorm(n=62, mean=5.4, sd=0.07), rnorm(n=76, mean=4.6, sd=0.05), rnorm(n=41, mean=6.2, sd=0.1), rnorm(n=72, mean=9, sd=0.1), rnorm(n=53, mean=8, sd=0.13), rnorm(n=59, mean=7.3, sd=0.12), rnorm(n=44, mean=6.6, sd=0.12), rnorm(n=46, mean=10.8, sd=0.24), rnorm(n=40, mean=8.7, sd=0.15), rnorm(n=40, mean=7.5, sd=0.15))
 )
 aphid_asin = transform(aphid_asin, devrate = 1/aphid_asin$devtime)

 aphid_siddiqui = data.frame(
 reference = c(rep(c("Siddiqui"), times=600)),
 species = c(rep(c("A. pisum"), times=600)),
 temp = c(rep(c(5), times=100), rep(c(15), times=100), rep(c(17.5), times=100), rep(c(20), times=100), rep(c(22.5), times=100), rep(c(25), times=100)),
 devtime = c(rnorm(n=100, mean=56.4, sd=0.4), rnorm(n=100, mean=12.2, sd=0.4), rnorm(n=100, mean=10.2, sd=0.4), rnorm(n=100, mean=8.4, sd=0.4), rnorm(n=100, mean=7.3, sd=0.4), rnorm(n=100, mean=6.8, sd=0.4))
 )
 aphid_siddiqui = transform(aphid_siddiqui, devrate = 1/aphid_siddiqui$devtime)

 aphid_hazell = data.frame(
 reference = c(rep(c("Hazell"), times=360)),
 species = c(rep(c("M. persicae"), times=360)),
 temp = c(rep(c(5), times=60), rep(c(10), times=60), rep(c(15), times=60), rep(c(20), times=60), rep(c(25), times=60), rep(c(30), times=60)),
 devtime = c(rnorm(n=60, mean=55.7, sd=0.03), rnorm(n=60, mean=23, sd=0.03), rnorm(n=60, mean=12.3, sd=0.03), rnorm(n=60, mean=8.4, sd=0.03), rnorm(n=60, mean=6.7, sd=0.03), rnorm(n=60, mean=8.1, sd=0.03))
 )
 aphid_hazell = transform(aphid_hazell, devrate = 1/aphid_hazell$devtime)

 aphid_ahn = data.frame(
 reference = c(rep(c("Ahn"), times=187)),
 species = c(rep(c("A. pisum"), times=187)),
 temp = c(rep(c(10), times=39), rep(c(15), times=53), rep(c(20), times=45), rep(c(25), times=42), rep(c(30), times=8)),
 devtime = c(rnorm(n=39, mean=21.3, sd=0.31), rnorm(n=53, mean=12.2, sd=0.23), rnorm(n=45, mean=8.3, sd=0.16), rnorm(n=42, mean=6.7, sd=0.16), rnorm(n=8, mean=6.5, sd=0.26))
 )
 aphid_ahn = transform(aphid_ahn, devrate = 1/aphid_ahn$devtime)

# combining data frames into one large data frame
 aphid_growth = rbind(aphid_ahn, aphid_asin, aphid_hazell, aphid_mastoi, aphid_siddiqui)

# creating plot of all data sources with devrate 
 ggplot(aphid_growth, aes(temp, devrate)) +
   geom_point(aes(shape = reference, color = species)) +
   theme_bw(base_size = 12)
 