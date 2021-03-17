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

## Questions
* What do negative case and death data mean? due to corrections
* Is gender relevant? could be extracted and aggregated from direct RKI data

## Tasks
* literature: 
    * how ist lag length between causal relation between cases and deaths decided?
    * are there standard groups for age stratification?
    * Wie lange kommt die Hospitalisierung nach infection bzw. wie lange im krankenhaus?
* exploratory data analysis:
    * evalutate relation between cases, hospitalization and deaths

## Knowledge
* Atlantic Article:
    * am Anfang wurde zu wenig getestet
    * key equation: deaths/cases = fatality rate
    * bredford methode: 7-Tage Durchschnitt aktuelle deaths/7-Tage Durchschnitt Cases for 22 Tagen
    * Kritik: lag stark variabel und eher niedriger
    * Ensemble hatte am Ende aber eigentlich recht: maximal 4,000 Tote täglich, war im Konfidenzintervall drin 
* Wochen gehen von So-Sa
* 7 Tages Summen der Fallzahlen (7-Tage kumuliert), nicht mitteln
* Zählverteilung, negative Binomialverteilung: mehr Varianz als EW in der Realität, Kopf-Zahl-Beispiel

## Further concerns
* mismatch JHU vs RKI data