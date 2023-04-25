# creating new figure
# basic figure of predicted co2 against observed co2 values 
# build example data frame from hector (100 runs)

# load packages
library(matilda)
library(ggplot2)

# initiate with core
core <-
  newcore(system.file("input/hector_ssp245.ini", package = "hector"))

# generate parameters for a 100-run analysis
set.seed(1000)
params <- generate_params(core, 100)

# Run hector iterations; 100 model iterations from 1959 to 2100
h_result <-
  iterate_hector(core,
                 params,
                 save_years = 1959:2100,
                 save_vars = CONCENTRATIONS_CO2())

# compute wieghts for each run
run_weights <-
  score_hruns_ed(h_result, criterion_co2_obs(), bayes_wts)

# merge run_weights with the h_result
scores_hresult <- merge(h_result, run_weights, by = "run_number")

# subset scores_hreuslt to create a df to plot co2 1959:2021 against observed co2
# subset to include years for CO2 screening
scored_subset <- subset(scores_hresult, 
                        scores_hresult$year %in% criterion_co2_obs()$years & 
                          scores_hresult$variable == criterion_co2_obs()$var)

# filter the models whose RMSE values are below the median value
top_50pct <- subset(scored_subset, scored_subset$RMSE <= quantile(scored_subset$RMSE, 0.5))

# creates observed data frame
obs_dat <- data.frame(year = criterion_co2_obs()$year, value_obs = criterion_co2_obs()$obs_values)

# start by plotting predicted co2 values 
plot_co2 <- ggplot(data = scored_subset) +
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
  ggtitle(expression("CO"[2]~"concentration from 1959 to 2021"))
plot_co2

co2_plot_with_obs <- plot_co2 + geom_line(data = obs_dat, aes(x = year, y = value_obs),
                     color = "red",
                     linewidth = 1, 
                     linetype = "longdash")
ggsave("co2_plot_with_obs.png",
device = "png",
path = "figures/",
width = 15,
height = 12,
units = c("cm"),
dpi = 300)


co2_projections <- ggplot(data = scores_hresult) +
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
  ggtitle(expression("CO"[2]~"concentration projections through 2100"))
co2_projections

co2_projections + geom_line(data = obs_dat, aes(x = year, y = value_obs),
                     color = "red",
                     linewidth = 1, 
                     linetype = "longdash")
ggsave("co2_projections_with_obs.png",
       device = "png",
       path = "figures/",
       width = 15,
       height = 12,
       units = c("cm"),
       dpi = 300)
