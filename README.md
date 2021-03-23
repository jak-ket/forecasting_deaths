# forecasting_deaths
Use historical data on the COVID-19 pandemic to forecast deaths in Germany

## Objective
* forecast incident deaths 
* in Germany 
* with historical case, death and hospitalization data
* age-stratification: <30, 10-year steps, >80
* use sum of daily cases from today and the past 6 days to predict deaths in 1,2,3,4 weeks
* highly parameterized and readible code, easily modified 
    * age strata
* forecasts
    * Zählregression mit negat binomvert., Autoregression
    * Parameter: fat_rate, lag
    * EW mit fallzahlen
    * Var auch aus Daten
    * WIS zur Erfassung der Prognosegüte (generalisierter absoluter Fehler)
    * in sample WIS optimieren
    * Interzept? wenn keine Fälle, auch keine lagged Tode
    

## Questions
* Are there explicit formulations for nb distr. params? Would speed up optimization!
* What do negative case and death data mean? due to corrections
* Is gender relevant? could be extracted and aggregated from direct RKI data

## Tasks
* time series: exclude christmas 20.12.2020-22.1.
* aggregate age stratified forecasts: can take mean of mu, but have to estimate size
* backtesting and comparison with predictions of other models
* how to handle unbekannt category?
* literature: 
    * how ist lag length between causal relation between cases and deaths decided?
    * are there standard groups for age stratification?
    * delay time distributions between exposure, symptom onset, reporting of case,
    hospizalization, death in Germany
    * research time hosp -> death (Daniel: data driven RKI)

## Done
* subset >sep.2020 approx.
* fat_rate über Zeit plotten
* paper section 2
* functions for with 1 covariable (lagged cases), deaths, estimate fat_rates
* minimize wis with lag, fat_rate, size (Überdispersion) param for one age group
Exploratory analyses:
- Generate age-stratified plots of cases, hospitalizations (DIVI) and deaths
- Are clear "shifted" patterns recognizable?
* exploratory data analysis: evalutate relation between cases, hospitalization and deaths

## Knowledge
DIVI categories:
* ICU - intensive care unit (low care and high care)
* ECMO - Extracorporeal membrane oxygenation
* Ventilated

## Further concerns / problems
* DIVI reporting delay: not all hospitals have reported during the first wave (approx. 20.03.2020 - 30.04.2020)
* mismatch JHU vs RKI data