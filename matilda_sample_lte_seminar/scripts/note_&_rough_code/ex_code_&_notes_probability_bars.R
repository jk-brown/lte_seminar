#Example Code and notes for computing the probability bars

met <- new_metric(GLOBAL_TAS(), years = 2080:2100, op = mean)
met_calc <- metric_calc(h_result, met)

bins <- c(0, 1.5, 2.0, 2.5, 3.5, 4, Inf)

prob_test <- prob_calc(met_calc$metric_result, bins, scores = scored_data$posterior_prob)
prob_test$scenario <- rep("SSP2-4.5")

probs <- round(prob_test$probability, digits = 3) * 100

ggplot(data = prob_test, aes(x = probability, y = scenario, fill = bins)) +
  geom_bar(position = position_fill(reverse = T),
           stat = "identity") +
  scale_fill_manual(values = c("dodgerblue","lightblue","beige", "lightcoral", "red", "darkred", "black"))+
  scale_x_continuous(breaks = seq(0, 1.0, 0.1)) +
  theme_light()


# Filter your data to select only the years 2081-2100
subset_data_longterm <- subset(mean_df, year >= 2081 & year <= 2100)
subset_data_rel <- subset(mean_df, year >= 1850 & year <= 1900)

# Calculate the mean of the 'mean' column in your filtered data
mean_value_longterm <- mean(subset_data_longterm$mean)
mean_value_rel <- mean(subset_data_rel$mean)

# warming relative to 1850:1900
mean_value_longterm - mean_value_rel

#mean values from data
mean_vals_longterm <- subset_data_longterm$mean
mean_vals_rel <- subset_data$mean

# compute the 5-95% confidence interval using quantile()
ci_longterm <- quantile(mean_vals_longterm, probs = c(0.05, 0.95))
ci_rel <- quantile(mean_vals_rel, probs = c(0.05, 0.95))

# print the result
fut_lower = mean_value_longterm - ci_longterm[1]
fut_upper = mean_value_longterm + ci_longterm[2]

rel_lower = mean_value_rel - ci_rel[1]
rel_upper = mean_value_rel + ci_rel[2]

warming <- mean_value_longterm - mean_value_rel
lower_diff <- fut_lower - rel_lower
upper_diff <- fut_upper - rel_upper

# Display the results
cat("Warming:", warming, "\n")
cat("Lower bound of difference:", lower_diff, "\n")
cat("Upper bound of difference:", upper_diff, "\n")
