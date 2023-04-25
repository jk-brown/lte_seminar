## Contents:  Code for completing matilda analysis and plotting workflows. 

library(matilda)
library(tidyverse)
library(ggplot2)

# 1 Running iterate_hector across 4 SSPs ------------------------------------
source(file = "functions/functions_not_in_matilda.R")

# SSPs to initiate a Hector instance
ssp119 <- newcore(system.file("input/hector_ssp119.ini", package = "hector"),
                  name = "SSP1-1.9")
ssp126 <- newcore(system.file("input/hector_ssp126.ini", package = "hector"),
                  name = "SSP1-2.6")
ssp245 <- newcore(system.file("input/hector_ssp245.ini", package = "hector"),
                  name = "SSP2-4.5")
ssp370 <- newcore(system.file("input/hector_ssp370.ini", package = "hector"),
                  name = "SSP3-7.0")

# Create a list of environments
env_list <- list(ssp119, ssp126, ssp245, ssp370)

# Run the generating parameters
set.seed(999)
params <- lapply(env_list, generate_params, 5)

# Running the model for CO2 and Global temperature
start <- Sys.time()
h_result <- Map(iterate_hector, env_list, params)
print(Sys.time()-start)


# * 1.1 Computing 'weights' for each model run --------------------------------

# Compute weights (posterior probabilities) across list of Hector results 
# (h_result) using observed CO2 with default decay rate (e = 2).
weights <- lapply(h_result, score_hruns_ed, criterion_co2_obs(),
                  bayes_wts, e = 2)

# Merge results so each Hector run in h_result is assigned its corresponding 
# weight. 
weighted_hector <- Map(merge, weights, h_result, by = "run_number")

# Bind the list of data frames into one large data frame result of weighted 
# Hector runs for each SSPs scenario
result_full <- do.call(rbind, weighted_hector)


# * 1.2 Plotting model weight decay ---------------------------------------

# Plotting weight decay - decay line in blue and models as points
weight_decay <- ggplot(data = result_full, aes(x = RMSE, 
                                               y = posterior_prob)) +
  geom_line(color = "blue") +
  geom_point(data = result_full,
             aes(x = RMSE, 
                 y = posterior_prob,
                 group = run_number),
             size = 2, 
             color = "black") +
  labs(y = "Likelihood (probability)")+
  facet_wrap(~scenario, scales = "free_y") +
  theme_light()
weight_decay

ggsave("figure_output/example_scoring_curve.png",
       plot_wts,
       device = "png",
       height = 12,
       width = 24,
       units = "cm",
       dpi = 300)

# * 1.3 Plotting CO2 projections -----------------------------------------

# Subset to include CO2 1990:2100
co2_1990_2100 <- subset(result_full, 
                        year >= 1990 
                        & year <= 2100 
                        & variable == CONCENTRATIONS_CO2())

# creates observed data frame - will add this as a layer to the plot
obs_dat <- data.frame(year = criterion_co2_obs()$year, 
                      value_obs = criterion_co2_obs()$obs_values)

# Plotting CO2 values faceting scenario and weighting model trajectories by 
# posterior probability

# Start by creating plot of CO2 projections 
plot_co2 <- ggplot(data = co2_1990_2100) +
  geom_line(
    aes(x = year,y = value,
        group = run_number,
        color = posterior_prob,
        alpha = posterior_prob),
    linewidth = 1) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue") +
  scale_alpha_continuous(range(c(0.5, 1))) +
  ylab(expression(CO[2]~Concentration~(ppm))) +
  theme_light() +
  guides(alpha = "none") +
  ggtitle(expression("CO"[2]~"concentration from 1990 to 2100")) +
  facet_wrap(~scenario)
plot_co2

# Add observed CO2 values to aid visualization of most plausible models
co2_plot_with_obs <- plot_co2 + geom_line(data = obs_dat, aes(x = year, y = value_obs),
                                          color = "red",
                                          linewidth = 1, 
                                          linetype = "dashed")
co2_plot_with_obs

# save the resulting figure in the "figures" folder
ggsave("figures/co2_concentration.png",
       co2_plot_with_obs,
       device = "png",
       height = 12,
       width = 24,
       units = "cm",
       dpi = 300)


# 2 Computing probabilities of GLOBAL_TAs across 4 SSPs -------------------

# Define your metric of interest.
# Here, I am looking at the long term (IPCC) 20 year average of global 
# temperature anomaly (GLOBAL_TAs).  
longterm_metric <- new_metric(GLOBAL_TAS(), years = 2081:2100, op = mean)

# Computes my metric values (longterm_metric) for each hector run across all SSPs 
# in the weighted_hector list 
met_calcs <- lapply(weighted_hector, metric_calc, longterm_metric)

# Merges the calculated metrics for each run and the weights for each run by the
# run_number. 
met_and_scores <-Map(merge, met_calcs, weights, by = "run_number")

# Establish bins for sorting probabilities
bins <- c(0, 1.5, 2.0, 2.5, 3.5, 4, Inf)

# Computing probabilities across the list of met_and_scores dfs
probabilities <- lapply(met_and_scores, function(x) 
  prob_calc(metrics = x$metric_result, bins, scores = x$posterior_prob))

# Computing probabilities does not carry over scenario names (do we want to
# fix this in matilda?)
# Define the labels to be added to each probability data frame
scenarios <- c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0")

# Use Map to add the new column to each element of the list - this will be our
# scenario column
probability_list <- Map(function(df, col_name) {
  df$scenario <- col_name
  return(df)}, probabilities, scenarios)

# Finally, we can merge the probability dfs in probability_list into a single df
# This will give us the probability of warming ranges for each SSP scenario in 
# a single df.
probability_results <- do.call(rbind, probability_list)

# * 2.1 Plotting probability range as horizontal stacked bar --------------

# Change the colnames to make is easier to produce an aesthetic plot
colnames(probability_results) <- c("Warming", "Score" ,"Probability", "Scenario")

# Use the probability_results df to plot results
probability_plot <- ggplot(probability_results, 
                           aes(fill = Warming, 
                               y = Probability, 
                               x = Scenario)) +
  geom_bar(position = position_fill(reverse = T),
           stat = "identity",
           width = 0.6) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.1)) +
  scale_fill_manual(
    values = c("dodgerblue",
               "skyblue1",
               "mistyrose",
               "lightcoral",
               "red",
               "darkred"),
    labels = c("0 to 1.5 C", 
               "1.5 to 2 C", 
               "2 to 2.5 C", 
               "2.5 to 3.5 C", # used this bin specifically for LTE seminar example 
               "3.5 to 4 C", 
               ">4 C")) +
  coord_flip() +
  theme_light()
probability_plot

# save probability bar graph to "figures" folder
ggsave("figures/probability_plot.png",
       probability_plot,
       device = "png",
       height = 12,
       width = 24,
       units = "cm",
       dpi = 300)

# 3 Computing weighted means and attempting to plot -----------------------

# The way I want this code to work is to group the Hector results by year and 
# scenario and then use the 150 runs each Hector result has to compute mean 
# GLOBAL_TAs for each year, weighted by the posterior probability.

# * 3.1 Defining function to compute weighted means -----------------------

# Define a function to compute the weighted mean grouping by year and scenario
w_mean_calc <- function(df) {
  df %>% 
    # we will split by scenario but by grouping here we preserve scenario names
    # during plotting
    group_by(year, scenario) %>%  
    summarize(
      weighted_mean = weighted.mean(value, w = posterior_prob),
      ci_upper = quantile(value, probs = 0.95, w = posterior_prob),
      ci_lower = quantile(value, probs = 0.05, w = posterior_prob),
      .groups = "drop"
    )
}

# Define a function to compute the weighted CI grouping by year and scenario
w_mean_historic <- function(df) {
  df %>% 
    group_by(year) %>% 
    summarize(
      weighted_mean = mean(value, w = posterior_prob),
      h_ci_upper = quantile(value, probs = 0.95, w = posterior_prob),
      h_ci_lower = quantile(value, probs = 0.05, w = posterior_prob),
      .groups = "drop"
    )
}

## An update: these figures were a struggle - we may want to be plotting the 
## credible interval rather than a confidence interval (?).

# * 3.2 Calculating weighted means and CIs --------------------------------

# Subset to include temp for years 2023-2100 these are weighted means of
# GLOBAL_TAs projections. 
temps <- subset(result_full, 
                year >= 2022 & 
                year <= 2100 & 
                variable == GLOBAL_TAS())
                
# Subset to include temp for years 1850-2021 these are weighted means of
# GLOBAL_TAs hindcast (idk if I am using that word right). 
historic <- subset(result_full, 
                   year >= 1850 & 
                   year <= 2021 & 
                   variable == GLOBAL_TAS())

# Split the subsetted data frames by scenario
split_scenario <- split(temps, temps$scenario)
split_historic <- split(historic, historic$scenario)

# Computed weighted means for each scenario in the split list
temp_summary <- lapply(split_scenario, w_mean_calc)
historic_summary <- lapply(split_historic, w_mean_historic)

# Creating dfs with the weighted mean and CI values
mean_temps_df <- do.call(rbind, temp_summary)
rownames(mean_temps_df) <- NULL
historic_df <- do.call(rbind, historic_summary)
rownames(historic_df) <- NULL
historic_df$scenario <- "historic"


# * 3.3 Plotting projected and hindcast global temperature anomaly --------

## I am not sure about the method for plotting the hind cast. I am sure there
## is a more robust way to get this data from Hector Than what I used in 3.2 and
## 3.3 of this script.

# Create ggplot for scenario data
# first producing a plot of the 
ssps <- ggplot(mean_temps_df, aes(x = year, 
                                  y = weighted_mean, 
                                  color = scenario, 
                                  fill = scenario)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_lower, 
                  ymax = ci_upper), 
              alpha = 0.1, color = NA) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Temperature") +
  theme_light() +
  guides(fill = "none")
ssps

# Add historic data to the plot
ssps +
  geom_smooth(data = historic_df, 
              aes(x = year, 
                  y = weighted_mean), 
              se = FALSE, 
              span = 0.03, 
              color = "black")

# Save the plot in the "figures" folder
ggsave("figures/SSPs_mean_warming.png",
       device = "png",
       width = 23,
       height = 18,
       units = c("cm"),
       dpi = 300)


