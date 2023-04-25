# Weighted mean calc and plot test and notes

library(tidyverse)
library(ggplot2)

# Function to compute weighted mean and weighted quantiles for a given range
w_mean <- function(value, weights, lower_ci = 0.05, upper_ci = 0.95) {
  
  # Find the weighted mean
  w_mean <- weighted.mean(value, w = weights) 
  
  # compute lower confidence interval
  ci_low <- quantile(value, probs = lower_ci, w = weights)
  
  # compute upper confidence interal
  ci_upper <- quantile(value, probs = upper_ci, w = weights)
  
  # Return a vector with the median and quantiles
  return(c(w_mean, ci_low, ci_upper))
}

# Function to compute weighted median
w_med <- function(value, weights, lower_ci = 0.25, upper_ci = 0.75) {
  
  # Find the weighted mean
  w_median <- quantile(value, probs = 0.5, w = weights) 
  
  # compute lower confidence interval
  ci_low <- quantile(value, probs = lower_ci, w = weights)
  
  # compute upper confidence interal
  ci_upper <- quantile(value, probs = upper_ci, w = weights)
  
  # Return a vector with the median and quantiles
  return(c(w_median, ci_low, ci_upper))
}

# Here we want to compute weigted aaverages for each of the year in
h_results_scored <- merge(h_result, scored_data, by = "run_number")

# Example of global tas weighted mean?
global_tas_dat <- subset(h_results_scored, variable == GLOBAL_TAS())

# Group data by year and compute weighted median and 50% quantile ribbon for each year
split_gtas_year <- split(global_tas_dat, global_tas_dat$year)
weighted_mean <-
  sapply(split_gtas_year, function(x) {
    w_mean(x$value, x$posterior_prob, lower_ci = .05, upper_ci = 0.95)
    })
mean_data <- t(weighted_mean)
mean_df <- rownames_to_column(as.data.frame(med_data), var = 'year')
colnames(mean_df) <- c('year', 'mean', 'lower', 'upper')
mean_df$year <- as.numeric(mean_df$year)

mean_plot <- subset(mean_df, year > 2001 & year < 2101)

# Create ggplot
ggplot(mean_plot, aes(x = year, y = mean)) +
  geom_line(size = 1, color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Temperature") +
  theme_classic()

#################
# Group data by year and compute weighted median and 50% quantile ribbon for each year
split_gtas_year <- split(global_tas_dat, global_tas_dat$year)
weighted_median <-
  sapply(split_gtas_year, function(x) {
    w_med(x$value, x$posterior_prob)
  })
med_data <- t(weighted_median)
med_df <- rownames_to_column(as.data.frame(med_data), var = 'year')
colnames(med_df) <- c('year', 'median', 'lower', 'upper')
med_df$year <- as.numeric(med_df$year)

med_plot <- subset(med_df, year > 2001 & year == 2300)

# Create ggplot
ggplot(med_plot, aes(x = year, y = median)) +
  geom_line(size = 1, color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Temperature") +
  theme_classic()
