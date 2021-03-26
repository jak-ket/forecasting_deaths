# Forecasting deaths
Use historical, age-stratified data on the COVID-19 pandemic to forecast deaths in Germany

Jakob Ketterer and Johannes Bracher
March 2021

## Objective
* forecast weekly incident deaths for Germany 
* using historical case, death (and hospitalization) data
* fit parameters (lag between cases and deaths, case fatality rate and size parameter for negative binomial distribution) for differenct age strata
* aggregate age-stratified forecasts to get pooled forecast

## File overview 
### Data exploration
Exploration of correlations between case and death as well as between case/death and ICU/ventilated data
* exploratory_data_analysis.Rmd
* functions_data_analysis.R

### Forecast deaths for individual age stratum
Estimate parameters and forecast distribution for single age group and visualize forecast
* forecast_deaths_individual_age_group.Rmd
* functions.R

### Forecast deaths for selection of age strata
Calculate forecast distributions for selected age groups, estimate pooled forecast distributions for 1 and 2 weeks, write forecasts to hub format csv, visualize forecasts
* forecasting deaths.Rmd
* functions.R

## Folder overview
* ./data-truth/: contains truth data for estimation
* ./data-processed/: contains death forecasts in forecast hub format

## Details on approach
* RKI age strata: "A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+", "unbekannt"
* Key equation: mus = lagged case number * case fatality rate (mu is intermediate death forecast and serves as estimated value for negative binomial forecast distribution)
* Estimated parameters per age stratum:
    * lag between cases and deaths
    * case fatality rate and for negative binomial distribution
    * size parameter for negative binomial distribution
* Estimated parameters for all strata:
    * mu_pooled = sum of age group mus
    * size_pooled: estimate via minimization of sum of WIS conditioned on true death data and mus during training period

## Future work
* take ALL age groups (not only the 3 most important) to estimate pooled death forecast
* extend forecast horizons to 3 and 4 wks ahead (needs extrapolation of case data)
* more covariates to forecast deaths: ICU/ventilated data
* backtesting and comparison with predictions of other models
* how to handle unbekannt category?
* Is gender relevant? could be extracted and aggregated from direct RKI data
* literature: 
    * how ist lag length between causal relation between cases and deaths decided?
    * delay time distributions between exposure, symptom onset, reporting of case,
    hospizalization, death in Germany
    * research time hosp -> death (data driven RKI)

## Further concerns / problems
* DIVI reporting delay: not all hospitals have reported during the first wave (approx. 20.03.2020 - 30.04.2020)
* mismatch JHU vs RKI data