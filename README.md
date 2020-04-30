# COVID19-Jails
Data and code for 'The Epidemiological Implications of Incarceration Dynamics in Jails for Community, Corrections Officer, and Incarcerated Population Risks from COVID-19'

Files
-----
* covid_scenario_report.Rmd: The main file for simply replicating the results of the paper - this will run you through the scenarios modeled. Running this will provide all the graphs in the manuscript as well as a number of scenario-specific diagnostic plots.
* standalone_plots.R: A script for generating the plots used in the manuscript
* /R/jail_outbreak_ode.R: The primary code specifying the system of ODEs that make up the model
* /R/plot_utils.R: Utilities for plotting output
* /R/tidying_utils.R: Utility functions for "tidyverse" style programming
* /R/set_parameters.R: Functions for parameterizing the model
* /R/gammaAestimation.R: Statistical estimation of the asymotomatic transmission time, accounting for censoring.
* /data/asymotomatic_transmission.csv: The data set needed for the estimation procedures found in gammaAestimation.R


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

All dependencies may be installed using standard install.packages() procedures in R.

This has been tested using R 3.6.2 and R 4.0.0 on Mac OS 10.15.3, Windows 10, and Linux Mint 19.3

Run Time
-------
The models in this repository are relatively complex, and thus fairly slow by the standards of deterministic compartmental models. Each scenario may take up to a minute to run on a standard desktop computer, and running the entire contents of the covid_scenario_report.Rmd script may take on the order of 15-20 minutes.

Outputs
------
Running the R Markdown file will provide a series of specific diagnostic plots, as well as the figures seen in the manuscript. Specific numerical information can be accessed directly via the scenario-specific dataframes produced by running the code. Some specific variables of note:

* metric_community_infections: Cumulative COVID-19 cases in the community
* metric_incarceratedpeople_infections: Cumulative COVID-19 cases among incarcerated persons
* metric_staff_infections: Cumulative COVID-19 cases among jail staff

Similar metrics for X_hosp and X_deaths exist for hospitalizations and deaths respectively.

For example, if you wish to see the cumulative deaths in jail staff under the "Shelter-in-Place" scenario, you would reference the resdf_sip$metric_staff_deaths variable.

License
----
MIT License

Copyright (c) [2020] [Eric T. Lofgren]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.