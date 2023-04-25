# Functions needed to run code in "scripts"

#' Calculating Root Mean Square Error (RMSE) 
#' 
#' @description Function takes vectors of modeled data values and compares them 
#' to observed data values to report the RMSE. 
#'
#' @param x A vector of modeled data values
#' @param y A vector of observed data values
#'
#' @return A vector of RMSE values
#' @export
#'
#' @examples
#' x = c(1:5)
#' y = c(5:10)
#' 
#' RMSE_calc(x, y)
#' 
RMSE_calc <- function(x, y) {
  
  # compute RMSE
  rmse_vals = sqrt(mean((x - y)^2))
  
  # return a vector of RMSE values
  return(rmse_vals)
  
}

#' Computing Posterior Probabilities using Bayesina Inference
#'
#' @description This function takes a data frame of multiple Hector runs containing
#' modeled data values and observed data values for a corresponding year range and 
#' variable. This function is best used as the \code{score_function} arguement in
#' \code{\link{score_hruns()}}   
#'
#' @param x A data frame of modeled and obsereved data for a corresponding 
#' variable and year length.
#' @param e A value from 0-Inf. Default is set to 2 which is a reasonable decay 
#' rate.
#'
#' @return A data frame of root mean square (\code{RMSE}), posterior values 
#' (\code{posterior_vals}), and normalized posterior probabilities 
#' (\code{posterior_prob}) labeled by \code{run_number}.
#' 
#' @export
#'
#' @examples
#' x <- data.frame(run_number = 1:5,
#'                 year = 2000:2005,
#'                 variable = "ex_variable",
#'                 value = 300:305,
#'                 obs_data = 305:310)
#'                 
#' bayes_wts(x, e = 2)
#'                 
bayes_wts <- function(x, e = 2) {
  
  print(e)
  
  # split the x data frame by model run_number 
  split_list = split(x, x$run_number)
  
  # for each of the dfs in the split_list compute RMSE using model predicted 
  # data and obs_data  
  rmse_vals = lapply(split_list, function(df) {
    RMSE_calc(x = df$value, y = df$value_obs)
  })
  
  # rbind the rmse_vals - makes it easier to complete subsequent steps
  rmse_bind <- do.call(rbind, rmse_vals)
  
  # Compute likelihood using normal distribution likelihood function.
  # This is the probability of observing the modeled data given the 
  # observed data.
  likelihood = exp(-0.5 * (rmse_bind) ^ e)
  
  # Computing unnormalized posterior scores 
  # Currently only computing posterior scores using uniform prior.
  # uniform prior is calculated as 1/length(likelihood) which is 
  # the same as 1 / # of runs.
  posterior = likelihood * (1 / length(likelihood))
  
  # Computes posterior probabilities - normalized posterior weights. 
  # Will sum to 1 and there for get significantly smaller as number 
  # of runs increases.
  posterior_probs = posterior / sum(posterior)
  
  # Create data frame of results - get run_numbers from the list where RMSE values
  # are computed (names of the split_list components)
  return(
    data.frame(
      RMSE = rmse_bind,
      posterior_vals = posterior,
      posterior_prob = posterior_probs,
      run_number = names(rmse_vals))
    )
}


## Not spending too much time ont the function documentation here because it is
## already done in the matilda package.

#' Score Hector Runs 
#'
#' @description Function computes scores (or weights) indicating model performance
#' in predicting observed data.
#'
#' @param x A data frame of Hector results.
#' @param criterion An object of class criterion that contains criterion parameters.
#' See \link{\code{new_criterion}} for details.
#' @param score_function A function that applies a scoring method 
#' @param e Decay factor (for use in \link{\code{bayes_wts}} scoring function)
#' @param ... Aditional arguements.
#'
#' @return returns df produced in \code{score_hruns}
#' @export
#'
#' @examples
score_hruns_ed <- function(x, criterion, score_function, e,...) {
  
  # subset to include years for CO2 screening
  x_subset <- subset(x, x$year %in% criterion$years & x$variable == criterion$var)
  
  # creates observed data frame
  obs_dat <- data.frame(year = criterion$year, value_obs = criterion$obs_values)
  
  # merge hector results with calibration data observed CO2 data
  x_merge <- merge(x_subset, obs_dat, by = 'year')
  
  # use x_merge as input for `bayes_wts` -> score_function, return values  
  return(score_function(x_merge, e = e))
  
}