###################################
# Functions for Exploratory Analysis
# Jakob Ketterer, March 2021
###################################

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
