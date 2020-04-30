# COVID19-Jails
Data and code for 'The Epidemiological Implications of Incarceration Dynamics in Jails for Community, Corrections Officer, and Incarcerated Population Risks from COVID-19'

Files
-----
* covid_scenario_report.Rmd: The main file for simply replicating the results of the paper - this will run you through the scenarios modeled.
* standalone_plots.R: A script for generating the plots used in the manuscript
* /R/jail_outbreak_ode.R: The primary code specifying the system of ODEs that make up the model
* /R/plot_utils.R: Utilities for plotting output
* /R/tidying_utils.R: Utility functions for "tidyverse" style programming
* /R/set_parameters.R: Functions for parameterizing the model
* /R/gammaAestimation.R: Statistical estimation of the asymotomatic transmission time, accounting for censoring.


Dependencies
-------
* deSolve
* flexsurv
* dplyr
* tidyverse
* forcats
* ggforce
* gghighlight
* ggplot2
* ggrepel
* ggsci
* colorspace
* purrr
