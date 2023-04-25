library(ggplot2)

# Define a range of RMSE values to plot
rmse_vals <- seq(0, 10, length.out = 100)

# Compute the likelihood functions for e = 1, 2, and 3
likelihood_e1 <- exp(-0.5 * (rmse_vals) ^ 1)
likelihood_e2 <- exp(-0.5 * (rmse_vals) ^ 2)
likelihood_e3 <- exp(-0.5 * (rmse_vals) ^ 3)
likelihood_e5 <- exp(-0.5 * (rmse_vals) ^ 5)

# Combine the likelihood functions into a data frame
likelihood_df <- data.frame(rmse = rmse_vals,
                            one = likelihood_e1,
                            two = likelihood_e2,
                            three = likelihood_e3,
                            five = likelihood_e5)

# Melt the data frame into long format for plotting
likelihood_df_long <- reshape2::melt(likelihood_df,
                                     id.vars = "rmse",
                                     variable.name = "Decay_Rate",
                                     value.name = "likelihood")

# Plot the likelihood functions
likelihood_decay <- ggplot(likelihood_df_long, aes(x = rmse, y = likelihood, color = Decay_Rate)) +
  geom_line(linewidth = 1) +
  labs(x = "RMSE", y = "Likelihood") +
  ggtitle("Likelihood decay rates")+
xlim(0, 10) +
  theme_light()

ggsave("figures/likelihood_decay.png",
       plot = likelihood_decay,
       device = "png",
       height = 12,
       width = 16,
       units = "cm",
       dpi = 300)

#### Using sequence of e values ####

# Define a range of RMSE values to plot
rmse_vals <- seq(0, 10, length.out = 100)

# Create an empty data frame to store the likelihoods
likelihood_df <- data.frame(rmse = numeric(),
                            e = numeric(),
                            likelihood = numeric())

# Loop through the range of e values, compute the likelihoods for each e, and add them to the data frame
for (e in seq(2, 8, by = 0.2)) {
  likelihood_e <- exp(-0.5 * (rmse_vals) ^ e)
  likelihood_df_e <- data.frame(rmse = rmse_vals,
                                e = rep(e, length(rmse_vals)),
                                likelihood = likelihood_e)
  likelihood_df <- rbind(likelihood_df, likelihood_df_e)
}

# Melt the data frame into long format for plotting
likelihood_df_long <- reshape2::melt(likelihood_df,
                                     id.vars = c("rmse", "e"),
                                     variable.name = "likelihood",
                                     value.name = "likelihood_value")

# Plot the likelihood functions
ggplot(likelihood_df_long, aes(x = rmse, y = likelihood_value, color = factor(e))) +
  geom_line(linewidth = 1) +
  scale_color_discrete(name = "e") +
  labs(x = "RMSE", y = "Likelihood") +
  ggtitle("Likelihood functions for a range of e values") +
  theme_light()+
  theme(legend.position = "none") +
  xlim(0, 10)


