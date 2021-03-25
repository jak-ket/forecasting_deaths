# calculate death number negat. binom. dist. for given forecast date for one age group
#' @param processed_cases: processed case data frame that was used for fitting
#' @param fc_date: forecast date <= last available case date + lag
#' @param params: fitted parameters lag, size and case fatality rate
#' @param p: forecast quantile levels
#' @return: parameters of nb distribution
get_nb_params <- function(processed_cases, fc_date, params){

  # unpack parameters
  lag <- params["lag"]
  size <- params["size"]
  cfr <- params["cfr"]

  # get lagged case count
  end_date <- processed_cases[dim(processed_cases)[1],"date"]
  lag_date <- as.Date(fc_date) - lag
  stopifnot(lag_date <= end_date) # can only forecast lag days into future
  case_count <- processed_cases[processed_cases[,"date"]==lag_date,ag]

  # compute death forecast
  mu <- cfr * case_count

  return(list(mu=mu, size=size))
}

# gets params of nb dist and calculates quantile forecast
#' @param
#' @param
#' @param
#' @return:
calc_forecast <- function(mu, size, p = c(0.01, 0.025, 1:19/20, 0.975, 0.99)){

  # get quantiles according to estimated negative binomial distribution
  q <- qnbinom(p, mu = mu, size = size)
  names(q) <- p

  return(q)
}


#' prepare case and death data frames for training:
#' filter age_group, dates, calculate rolsum
#' @param age_group: specific age group to fit parameters for
#' @param cases_frame: data frame of date and age group columns with case values
#' @param deaths_frame: data frame of date and age group columns with death values
#' @param start: specific starting date for cases and deaths time series
#' @param end: specific end date or NA for last entry of time series
#' @param window: window size for rolling sums of time series
#' @return processed case and death data frame
prepare_data <- function(ag, cases_frame, deaths_frame, start, end, window){

  # set end date to last available date if end == NA
  if(is.na(end)){end <- cases_frame[dim(cases_frame)[1],"date"]}

  # sanity check
  if(end<=start){stop("start of training period must be before end of training period")}

  # truncate data to training period
  cases_trunc <- cases_frame[cases_frame[,"date"]>=start & cases_frame[,"date"]<=end,]
  deaths_trunc <- deaths_frame[deaths_frame[,"date"]>=start & deaths_frame[,"date"]<=end,]

  # subset date and age group
  cases_ <- cases_trunc[,c("date",ag)]
  deaths_ <- deaths_trunc[,c("date",ag)]

  # compute rolling sums; first window-1 dates are excluded
  start_after_rolsum <- start+window-1
  cases_[cases_[,"date"]>=(start_after_rolsum),ag] <- rolling_sum(cases_[,ag], window = window)
  deaths_[deaths_[,"date"]>=(start_after_rolsum),ag] <- rolling_sum(deaths_[,ag], window = window)
  cases_ <- cases_[cases_[,"date"]>=(start_after_rolsum),]
  deaths_ <- deaths_[deaths_[,"date"]>=(start_after_rolsum),]

  return(list(cases=cases_, deaths=deaths_))

}