# input: time series, window size of rolling window
# output: time series that contains window-1 less time points that are aggregated
# over the previous 6 and the current time point 
rolling_sum <- function(ts, window=7){
  w <- window 
  len <- length(ts)
  ts_ <- numeric(length=(len-w+1))
  for(i in 1:(len-w+1)){
    ts_[i] <- sum(ts[i:(i+w-1)])
  }
  return(ts_)
}

# input: two time series and lag
# output: two time series that have been lagged against each other
get_lagged_ts <- function(ts1, ts2, lag){
  l <- lag
  stopifnot(length(ts1)==length(ts2))
  len <- length(ts1)
  lagged_ts1 <- ts1[1:(len-l)]
  lagged_ts2 <- ts2[(l+1):len]
  # return_list <- list("ts1_lagged"=lagged_ts1, "ts2_lagged"=lagged_ts2)
  return_frame <- data.frame(lagged_ts1, lagged_ts2)
  return(return_frame)
}

# get lag that maximizes correlation between time series
# input: two time series, lags, print results?
# output: lag that maximizes correlation between ts
max_cor_lag <- function(ts1, ts2, lags=10:40, plot_out=T, 
                        plot_title="default title", print_out=F,
                        rolsum=F, window=7){
  cors <- numeric(length=length(lags))
  i <- 1
  for(l in lags){
    lagged_ts <- get_lagged_ts(ts1, ts2, lag=l)
    if(rolsum==T){
      cd_cor <- cor(rolling_sum(lagged_ts[,1], window=window), 
                    rolling_sum(lagged_ts[,2], window=window))
    } else {
      cd_cor <- cor(lagged_ts[,1], lagged_ts[,2])
    }
    cors[i] <- cd_cor
    if (print_out == T){print(paste("lag =", l, "cor =", cd_cor))}
    i <- i + 1
  }
  if(any(is.na(cors)) == T){
    return(NA)
  }
  if(plot_out == T){plot(lags, cors, main=plot_title)}
  return(lags[which.max(cors)])
}

# calculate avg fatality rate: fr = death ts / case ts; both ts should be lagged
# input: lagged death and case 
# output: average fatality rate 
calc_fat_rate <- function(deaths, cases, avg=T){
  fat_rates <- deaths/cases
  if(avg==T){
    return(mean(fat_rates))
  } else {
    return(fat_rates)    
  }
}



#' Fit an nb distribution via maximum likelihood:
#' @param x a vector of realizations
fit_nb_maxlik <- function(x){
  # negative log likelihood
  # par contains log-transformed parameters (vector with elements named "log_mu" and "log_size")
  nllik <- function(par){
    # transform back to original scale:
    mu <- exp(par["log_mu"])
    size <- exp(par["log_size"])
    # evaluate negative log-likelihood
    -sum(dnbinom(x, mu = mu, size = size, log = TRUE))
  }  
  
  # define starting values:
  log_mu_start <- log(mean(x))
  log_size_start <- 3 # not sure how to choose clever starting value  
  
  # run optimizer:
  opt <- optim(par = c(log_mu = log_mu_start, log_size = log_size_start), fn = nllik)
  
  # extract parameters:
  par <- exp(opt$par)
  names(par) <- c("mu", "size") # re-name elements  
  
  return(list(par = par, opt = opt))
}

#' Evaluate WIS of a negative binomial distribution
#' @param x the vector of observations
#' @param size the vector of size parameters of the negative binomial distribution
#' @param mu the vector of expectations of the negative binomial distribution
#' @param p the vector of quantile levels used in the computation of the WIS
wis_nb <- function(x, size, mu, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  # check input
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

#' Fit an nb distribution via minimum WIS.
#' @param x a vector of realizations
fit_nb_wis <- function(x, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  # evaluate wis
  # par contains log-transformed parameters (vector with elements named "log_mu" and "log_size")
  wis <- function(par){
    mu <- exp(par["log_mu"])
    size <- exp(par["log_size"])
    sum(wis_nb(x = x, mu = mu, size = size, p = p))
  }  
  
  # define starting values:
  log_mu_start <- log(mean(x))
  log_size_start <- 3 # not sure how to choose clever starting value  
  
  # run optimizer:
  opt <- optim(par = c(log_mu = log_mu_start, log_size = log_size_start), fn = wis)
  par <- exp(opt$par)
  names(par) <- c("mu", "size") # re-name elements  
  
  return(list(par = par, opt = opt))
}

# # Example:
# mu <- 10
# size <- 3
# n <- 1000
# # simulate x:
# x <- rnbinom(n = n, mu = mu, size = size)
# # fit nb dist using our maximum likelihood function:
# fit_nb_maxlik(x)
# # fit nb dist using minimum wis function:
# fit_nb_wis(x)
# 
# # fit using glm.nb from the MASS package
# # library(MASS)
# fit_glm.nb <- glm.nb(x ~ 1, link = "identity")
# summary(fit_glm.nb)# fit using out min WIS function:
# fit_nb_wis(x)

fit_params <- function(cases_lagged, deaths_truth, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){
  
  wis_sum <- function(par){
    size <- par["size"]
    cfr <- par["cfr"]
    
    # estimate deaths with case fatality rate (cfr)
    deaths_pred = cfr * cases_lagged
    
    sum(wis_nb(x = deaths_truth, mu = deaths_pred, size = size, p = p))
  }
  
  # optimize
  # initial values
  size_start <- 3
  cfr_start <- 0.05
  # lower bounds
  lb_size <- 10^-6
  lb_cfr <- 10^-6
  lower_bounds <- c(lb_size,lb_cfr)
  # upper bounds
  # ub_size <- 
  # ub_cfr <- 
  opt <- optim(par = c(size = size_start, cfr = cfr_start), method="L-BFGS-B", lower=lower_bounds, fn = wis_sum)
  
  return(opt)
}
