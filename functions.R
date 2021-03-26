###################################
# Functions for Death Forecasts
# Jakob Ketterer and Johannes Bracher, March 2021
###################################

#' calculate rolling sums over time series data
#' @param ts: time series data
#' @param window: window size for rolling sums of time series 
#' @return time series after rolling sum that is window-1 points shorter 
rolling_sum <- function(ts, window=7){
  w <- window 
  len <- length(ts)
  ts_ <- numeric(length=(len-w+1))
  for(i in 1:(len-w+1)){
    ts_[i] <- sum(ts[i:(i+w-1)])
  }
  return(ts_)
}


#' prepare case and death data frames for training:
#' filter age_group, dates, calculate rolsum 
#' @param cases_frame: data frame of date and age group columns with case values
#' @param deaths_frame: data frame of date and age group columns with death values
#' @param start: specific starting date for cases and deaths time series
#' @param end: specific end date or NA for last entry of time series
#' @param window: window size for rolling sums of time series 
#' @return processed case and death data frame
prepare_data <- function(cases_frame, deaths_frame, start, end, window){
  
  # sanity check
  if(end<=start){stop("start of training period must be before end of training period")}
  
  # truncate data to training period
  cases_ <- cases_frame[cases_frame[,"date"]>=start & cases_frame[,"date"]<=end,]
  deaths_ <- deaths_frame[deaths_frame[,"date"]>=start & deaths_frame[,"date"]<=end,]
  
  # compute rolling sums; first window-1 dates are excluded
  start_after_rolsum <- start+window-1
  
  for(ag in colnames(cases_)[-1]){
    cases_[cases_[,"date"]>=(start_after_rolsum),ag] <- rolling_sum(cases_[,ag], window = window)
    deaths_[deaths_[,"date"]>=(start_after_rolsum),ag] <- rolling_sum(deaths_[,ag], window = window)
  }
  
  # exclude first window-1 dates bec of rolsum
  cases_ <- cases_[cases_[,"date"]>=(start_after_rolsum),]
  deaths_ <- deaths_[deaths_[,"date"]>=(start_after_rolsum),]
  
  return(list(cases=cases_, deaths=deaths_))
  
}


#' get time series of deaths on Saturday and lagged cases
#' @param lag: number of days cases are lagged against deaths
#' @param cases_: cases returned from function prepare_data
#' @param deaths_: deaths returned from function prepare_data
#' @param dates_excluded: dates to be excluded from training data
#' @param start_after_rolsum: starting date after taking the rolsum
#' @return training tuples (cases lagged by lag param, feasible deaths on Saturday) 
get_lagged_train_data <- function(lag, cases_, deaths_, dates_excluded){
  
  start_after_rolsum <- cases_[,"date"][1]
  
  # based on current lag values, compute feasible death forecast Saturdays
  saturdays <- weekdays(deaths_[,"date"]) == "Saturday"
  deaths_sat <- deaths_[saturdays & deaths_[,"date"] >= (start_after_rolsum+lag),]
  
  # compute lagged cases corresponding to deaths on saturday
  cases_lagged_dates <- deaths_sat[,"date"]-lag
  cases_lagged <- cases_[cases_[,"date"] %in% cases_lagged_dates,]
  
  ## exclusion of time period
  # exclude deaths and cases based on deaths in exclusion period
  # (also captures cases before period that correspond to deaths in period)
  deaths_sat_ex <- deaths_sat[!(deaths_sat[,"date"] %in% dates_excluded),]
  cases_lagged_ex <- cases_lagged[!(deaths_sat[,"date"] %in% dates_excluded),]
  # exclude deaths and cases based on cases in exclusion period
  # (also captures deaths after period that correspond to cases in period)
  deaths_sat_ex <- deaths_sat_ex[!(cases_lagged_ex[,"date"] %in% dates_excluded),]
  cases_lagged_ex <- cases_lagged_ex[!(cases_lagged_ex[,"date"] %in% dates_excluded),]
  
  # get time series without dates
  cases_lagged_ts <- cases_lagged[,-1]
  deaths_sat_ts <- deaths_sat[,-1]
  
  return(list(cases=cases_lagged_ts, deaths=deaths_sat_ts))
}


#' fit forecast parameters for individual age group on specific training period
#' parameters to be fitted: lag, size of negat. binom. distr., case fatality rate (cfr)
#' @param age_group: specific age group to fit parameters for
#' @param lags: vector of possible values to lag case series against death series
#' @param cases_frame: cases returned from function prepare_data
#' @param deaths_frame: deaths returned from function prepare_data
#' @param start: specific starting date for cases and deaths time series
#' @param end: specific end date or NA for last entry of time series
#' @param start_exclude: specific starting date for period to exclude 
#' @param end_exclude: specific end date for period to exclude
#' @param window: window size for rolling sums of time series 
#' @param param_names: names of parameters to be fitted
#' @param print_plot: whether to print fitting progress and plot results  
#' @return optimal values of parameters for age group
fit_params_ag <- function(age_group, lags, processed_cases, processed_deaths, start, end, start_exclude, end_exclude, param_names, print_plot=T){
  
  # select specific age group
  cases_ <- processed_cases[,c("date", age_group)]
  deaths_ <- processed_deaths[,c("date", age_group)]
  
  # sanity checks for period to exclude
  training_period <- seq(start, end, by="days")
  if(end_exclude<=start_exclude){stop("start of excluded period must be before end of excluded period")}
  dates_excluded <- seq(start_exclude, end_exclude, by="days")
  if(!all(dates_excluded %in% training_period)){stop("not all dates to exclude are contained in training period")}
  
  # save param values and objective value per lag
  results <- matrix(data = NA, nrow=length(lags), ncol=length(param_names)+1) 
  colnames(results) <- c(param_names, "value")
  
  # iterate over lag space
  for (i in 1:length(lags)){
    lag <- lags[i]
    
    train_data <- get_lagged_train_data(lag, cases_, deaths_, dates_excluded)
    
    cases_lagged_ts <- train_data$cases
    deaths_sat_ts <- train_data$deaths
    
    stopifnot(length(cases_lagged_ts) == length(deaths_sat_ts))
    if(print_plot){print(paste("Fitting on", length(cases_lagged_ts), "data points."))}
    
    # starting values for optimization
    if (i >= 2){
      init_values <- results[i,c("size", "cfr")]
    } else {
      init_values <- NA
    }
    
    # compute parameters
    res <- fit_params(cases_lagged_ts, deaths_sat_ts, init_values)
    
    # save results
    results[i,] <- c(lag, res$par, res$value)
  }
  
  ag_best_params <- results[which.min(results[,"value"]),]
  
  if(print_plot){print(results)}
  if(print_plot){plot(results[,"lag"], results[,"value"], type="b", main = ag)}
  if(print_plot){print(ag_best_params)}
  
  return(ag_best_params)
}


# fitting routine for function fit_params_ag
fit_params <- function(cases_lagged, deaths_truth, init_values, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  
  wis_sum <- function(par){
    # retransform with exp to render params positive
    size <- exp(par["log_size"])
    cfr <- exp(par["log_cfr"])
    
    # estimate deaths with case fatality rate (cfr)
    deaths_pred = cfr * cases_lagged
    
    sum(wis_nb(x = deaths_truth, mu = deaths_pred, size = size, p = p))
  }
  
  # initial values (log-transformed)
  if (any(is.na(init_values))){
    log_size_start <- log(3)
    log_cfr_start <- log(0.05)
  } else {
    log_size_start <- log(init_values[1])
    log_cfr_start <- log(init_values[2])
  }
  
  # optimization
  # params are log-transformed 
  opt <- optim(par = c(log_size = log_size_start, log_cfr = log_cfr_start), fn = wis_sum)#,method="BFGS") #method="L-BFGS-B", lower=lower_bounds)
  
  # retransform
  opt$par <- exp(opt$par)
  
  return(opt)
}


#' Evaluate WIS of a negative binomial distribution
#' Johannes Bracher, March 2021
#' @param x the vector of observations
#' @param size the vector of size parameters of the negative binomial distribution
#' @param mu the vector of expectations of the negative binomial distribution
#' @param p the vector of quantile levels used in the computation of the WIS
wis_nb <- function(x, size, mu, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  # check input
  # print(paste(x, mu, size))
  if(any(mu <= 0) | any(size <= 0)) stop("mu and size need to be positive.")
  if(length(mu) == 1) mu <- rep(mu, length(x))
  if(length(size) == 1) size <- rep(size, length(x))
  if(length(mu) != length(x) | length(size) != length(x)) stop("mu and size need to be either of length 1 or the same length as x.")  
  # function to evaluate wis for x[i] and parameters mu[i] and size[i]
  wis_nb.i <- function(i){
    # compute quantiles
    q <- qnbinom(p, mu = mu[i], size = size[i])
    # evaluate WIS (see eq 4 here https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618#sec001)
    mean(2*((x[i] <= q) - p)*(q - x[i]))
  }  
  # apply to vector
  sapply(seq_along(x), wis_nb.i)
}


# calculate death number distribution for forecast date for one age group
#' @param age_group: specific age group that has been fitted 
#' @param forecast_date: date for estimating death distribution for one age group
#' @param processed_cases: processed case data frame that was used for fitting
#' @param lag: estimated lag parameter for age group
#' @param size: estimated size parameter for age group
#' @param cfr: estimated case fatality rate for age group
#' @param p: forecast quantile levels
#' @return: parameters of nb distribution and estimated quantiles 
calc_forecast_ag <- function(age_group, forecast_date, processed_cases, lag, size, cfr, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  
  # select age group
  processed_cases_ag <- processed_cases[,c("date",ag)]
  
  # get lagged case count
  end_date <- processed_cases_ag[dim(processed_cases_ag)[1],"date"]
  lag_date <- as.Date(forecast_date) - lag
  stopifnot(lag_date <= end_date) # can only forecast lag days into future
  case_count <- processed_cases_ag[processed_cases_ag[,"date"]==lag_date,ag]
  
  # compute death forecast
  mu <- cfr * case_count
  # get quantiles according to estimated negative binomial distribution
  q <- qnbinom(p, mu = mu, size = size)
  names(q) <- p 
  
  return(list(quantiles=q, mu=mu, size=size))
}


# estimate dispersion parameter for pooled forecast (all age strata)
#' @param ags: age groups
#' @param best_params: best parameters for all age groups
#' @param processed_cases: cases from prepare_data
#' @param processed_deaths: deaths from prepare_data
#' @param dates_excluded: dates to exclude
#' @param print_plot: if plots should be visualized
get_pooled_size <- function(ags, best_params, processed_cases, processed_deaths, dates_excluded, print_plot=T){
  
  # get number of training points for age group with maximal lag
  # number of training pairs differs with optimal lag estimate
  # age group with maximum number of lags has fewest training points (min_train_length)
  # min_train_length will determine number of mus that are summed up to form pooled distr
  max_lag <- max(best_params[,"lag"])
  ag_max_lag <- row.names(best_params)[which.max(best_params[,"lag"])]
  max_lag_train_data <- get_lagged_train_data(lag = max_lag, 
                                              cases_ = processed_cases[,c("date",ag_max_lag)], 
                                              deaths_ = processed_deaths[,c("date", ag_max_lag)], 
                                              dates_excluded = dates_excluded)
  min_train_true_deaths <- max_lag_train_data$deaths
  min_train_length <- length(min_train_true_deaths)
  
  # matrix to store mus
  mus <- matrix(nrow=min_train_length, ncol = length(ags))
  colnames(mus) <- paste("mus", ags)
  
  # matrix to store true deaths on Saturdays per age group 
  true_deaths <- matrix(nrow=min_train_length, ncol = length(ags))
  colnames(mus) <- paste("true deaths", ags)
  
  for(i in 1:length(ags)){
    ag <- ags[i]
    lag <- best_params[i,"lag"]
    
    # get death numbers for training saturdays and cases lagged by estimated lag
    train_data <- get_lagged_train_data(lag = lag, 
                                        cases_ = processed_cases[,c("date",ag)], 
                                        deaths_ = processed_deaths[,c("date",ag)], 
                                        dates_excluded = dates_excluded)
    
    # truncate time series to the length of smallest train data
    cases_lagged_ts <- train_data$cases[1:min_train_length]
    deaths_sat_ts <- train_data$deaths[1:min_train_length]
    
    # calculate death forecast (=mu) based on estimated cfr
    mus_ag <- cases_lagged_ts * best_params[i,"cfr"]
    
    true_deaths[,i] <- deaths_sat_ts
    mus[,i] <- mus_ag
    # print(mus_ag)
    
    if(print_plot){
      plot(deaths_sat_ts, type="l")
      points(mus_ag, col="red")
    }
  }
  ## take sum of mu vectors over age groups (sum for every forecast date)
  mu_sums <- rowSums(mus)
  true_death_sums <- rowSums(true_deaths)
  
  ## based on pooled truth deaths and pooled mus, fit size param of pooled nb distr
  opt <- estimate_size_all_ages(x=true_death_sums, mu=mu_sums)
  size_pooled <- opt$size
  
  return(size_pooled)
}


#' Fit an nb distribution via minimum WIS.
#' @param x a vector of realizations
estimate_size_all_ages <- function(x, mu, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  # evaluate wis
  # par contains log-transformed parameters (vector with elements named "log_mu" and "log_size")
  wis <- function(log_size){
    size <- exp(log_size)
    sum(wis_nb(x = x, mu = mu, size = size, p = p))
  }  
  
  # define starting values:
  log_size_start <- 3 # not sure how to choose clever starting value  
  log_size_lower <- log(1)
  log_size_upper <- log(10000)
  interval <- c(log_size_lower, log_size_upper)
  
  # run optimizer:
  opt <- optimize(f = wis, interval=interval)
  size <- exp(opt$minimum)
  optim_value <- opt$objective 
  
  return(list(size = size, value = optim_value))
}


# calculate pooled forecast based on multiple age groups 
#' @param fc_date: date for estimating death distribution
#' @param ags: age groups for cumulating forecast
#' @param size_pooled: size parameter of pooled negat binom distrib
#' @param processed_cases: cases from prepare_data
#' @param levels: quantile levels to return
#' @return: quantile levels of pooled forecast distribution
calc_forecast <- function(fc_date, ags, best_params, size_pooled, processed_cases, levels=c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  
  fc_mus <- numeric(length = length(ags))
  
  for(i in 1:length(ags)){
    ag <- ags[i] # age group
    lag = best_params[i,"lag"]
    cfr = best_params[i,"cfr"]
    
    end_date <- processed_cases[dim(processed_cases)[1],"date"]
    lag_date <- as.Date(fc_date) - lag
    stopifnot(lag_date <= end_date) # can only forecast lag days into future
    
    case_count <- processed_cases[processed_cases[,"date"]==lag_date,ag]
    
    # compute death forecast
    mu_ag <- cfr * case_count
    fc_mus[i] <- mu_ag
  }
  
  mu_pooled <- sum(fc_mus)
  
  fc_quantiles <- qnbinom(p = levels, mu=mu_pooled, size = size_pooled)
  names(fc_quantiles) <- levels
  
  return(fc_quantiles)
}




