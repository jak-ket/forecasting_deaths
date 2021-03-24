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

# Example:
mu <- 10
size <- 3
n <- 1000
# simulate x:
x <- rnbinom(n = n, mu = mu, size = size)
# fit nb dist using our maximum likelihood function:
fit_nb_maxlik(x)
# fit nb dist using minimum wis function:
fit_nb_wis(x)

# fit using glm.nb from the MASS package
# library(MASS)
fit_glm.nb <- glm.nb(x ~ 1, link = "identity")
summary(fit_glm.nb)# fit using out min WIS function:
fit_nb_wis(x)