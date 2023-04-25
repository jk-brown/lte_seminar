# Run the function with different exponents
results_2 <- score_hruns_ed(h_result, criterion_co2_obs(), bayes_wts)
results_2$ex <- rep(c("2"), each = length(results_2))
results_3 <- score_hruns_ed(h_result, criterion_co2_obs(), bayes_wts)
results_3$ex <- rep(c("3"), each = length(results_3))
results_1 <- score_hruns_ed(h_result, criterion_co2_obs(), bayes_wts)
results_1$ex <- rep(c("1"), each = length(results_1))

df <- rbind(results_1, results_2, results_3)

ggplot(data=df, aes(x = RMSE, y = posterior_prob, group = ex, color = ex)) +
  geom_line(linewidth = 1) +
  ylim (0, 0.004) +
  theme_light()

#### Need to combine this information with the information for each of the core 
# hector runs
results_2 <- score_hruns_ed(h_result, criterion_co2_obs(), bayes_wts, e = 2)
results_2$ex <- rep(c("2"), each = length(results_2))
results_3 <- score_hruns_ed(h_result, criterion_co2_obs(), bayes_wts, 3)
results_3$ex <- rep(c("3"), each = length(results_3))
results_1 <- score_hruns_ed(h_result, criterion_co2_obs(), bayes_wts, 1)
results_1$ex <- rep(c("1"), each = length(results_1))

h_result_1 <- merge(h_result, results_1, by = "run_number")
h_result_2 <- merge(h_result, results_2, by = "run_number")
h_result_3 <- merge(h_result, results_3, by = "run_number")

hector_df <- rbind(h_result_1, h_result_2, h_result_3)

top_50pct <- subset(hector_df, hector_df$RMSE <= quantile(hector_df$RMSE, 0.5))

###
res_1 <- 
ggplot(data = h_result_1) +
  geom_line(
    aes(
      x = year,
      y = value,
      group = run_number,
      color = posterior_prob,
      alpha = posterior_prob),
    linewidth = 1) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue") +
  scale_alpha_continuous(range(c(0.5, 1))) +
  ylab(expression(CO[2] ~ Concentration ~ (ppm))) +
  theme_light() +
  guides(alpha = "none") +
  ggtitle(expression("CO"[2] ~ "concentration projections through 2100"))

res_2 <- 
ggplot(data = h_result_2) +
  geom_line(
    aes(
      x = year,
      y = value,
      group = run_number,
      color = posterior_prob,
      alpha = posterior_prob),
    linewidth = 1) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue") +
  scale_alpha_continuous(range(c(0.5, 1))) +
  ylab(expression(CO[2] ~ Concentration ~ (ppm))) +
  theme_light() +
  guides(alpha = "none") +
  ggtitle(expression("CO"[2] ~ "concentration projections through 2100"))

res_3 <- 
  ggplot(data = h_result_3) +
  geom_line(
    aes(
      x = year,
      y = value,
      group = run_number,
      color = posterior_prob,
      alpha = posterior_prob),
    linewidth = 1) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue") +
  scale_alpha_continuous(range(c(0.5, 1))) +
  ylab(expression(CO[2] ~ Concentration ~ (ppm))) +
  theme_light() +
  guides(alpha = "none") +
  ggtitle(expression("CO"[2] ~ "concentration projections through 2100"))

res_1
res_2
res_3
