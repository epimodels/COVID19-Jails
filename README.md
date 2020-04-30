# covid_jails
Project modeling the spread (and prevention) of COVID-19 in jails and prisons.

Files
-----
* jail_outbreak.R: The main model for COVID-19 and the interaction between jails and the community.
* asymptomatic_transmission.csv: A csv file of the data from the supplementary table of https://link.springer.com/content/pdf/10.1007/s11427-020-1661-4.pdf . tdays denotes the estimated transmission days from the original paper, and Nt is an indicator for censoring - namely whether or not the patient was test negative at the end of the study.
* gammaAestimation.R: Statistical estimation of the asymotomatic transmission time, accounting for censoring, using the data above
* boneyard: A directory of old model files, primarily written in Python. Now superseded by the main files in R

Dependencies
-------
* deSolve
* survival

Compartments
------
How to read the compartments:

All the compartments in the model are denoted with an Xyz type format.

X is the disease state of the model. Your options are:
* S: Suseptible
* E: Exposed (and Asymptomatic)
* I: Active Symptomatic Infection
* H: Hospitalized
* R: Recovered/Removed. These are people both recovered and dead.

Note that all types of populations don't have all categories.

y is the Risk Group for a given compartment. Your options are:
* l : Low risk
* h : High risk
* e : Elderly (65+). These are treated as high risk individuals, but it's important to keep them seperate for admission and discharge bookkeeping
* o : Staff. These are treated as low risk individuals, but again, necessary book keeping.
* k : Under 18.

z is Where a given compartment it. Your options are:
* c : The community
* p : Processing
* j : Jail
* t : "Trial". These are people who are mixing with those in processing, but aren't eligible for the release process.

So, for example:

* Ilp : Infected individuals at low risk in processing
* Roc : Recovered/Removed staff in the community

A few notes: Under 18's, because they don't interact with the jails in this simulation, also don't have a "where" as they're inherently just in the community. Hospitalized patients *do* have a subscript, because while, for example, Hej individuals are not physically in jail, they will be returning to it once they are discharged.

There are also nine specialized "outcome" compartments. These compartments are not actually part of the population, and don't interact with any other compartment. Essentially, they are "buckets" that particular outcomes to fall into, such that cumulative numbers of certain outcomes can be easily calculated. They are all also mutually exclusive - for example, while some staff get COVID-19 in the community, they are at present only counted in staff. They are:

* CommunityInfections: The cumulative number of COVID-19 infections in the community
* StaffInfections: The cumulative number of COVID-19 infections among staff
* InmateInfections: The cumulative number of COVID-19 infections among incarcerated persons
* CommunityDeaths: The cumulative number of COVID-19 fatalities in the community
* StaffDeaths: The cumulative number of COVID-19 fatalities among staff
* InmateDeaths: The cumulative number of COVID-19 fatalities among incarcerated persons
* CommunityHosp: The cumulative number of community members requiring hospitalization/intensive care
* StaffHosp: The cumulative number of staff members requiring hospitalization/intensive care
* InmateHosp: The cumulative number of incarcerated persons requiring intensive care

Parameter Values and Logic
-----------
Note most of these parameters are presented in the code as formulas, not hard coded values, in order to facilitate tinkering.

**Arrest Rate for High Risk Individual (alpha_h)**
Value: 0.0000274
Reasoning: Per KL, we see an average intake of about 40 high risk, non-elderly people per day, or 1.667 per hour. Assuming that these individuals make up 5% of the adult population, this gives us a probability of 1.667/(1218452*0.05)

**Arrest Rate for Low Risk Individual (alpha_l)**
Value: 0.00000357
Reasoning: Per KL, we see an average intake of about 60 low risk people per day, or 2.5 per hour. Given a population of 1,218,452 of which 57.4% (this plus high risk adults adds up to 62.4%) are over 18 and under 65, this gives us a probability of: 2.5/(1218452*0.574)

**Arrest Rate for Elderly Individual (alpha_e)**
Value: 1.809e-07
Elderly individuals are ~1% of the jail population, but 18.9% total population. Conveniently given the estimate of the average intake of 100 people, that means 1 person per day is arrested from this group, or 0.041667 per hour. Using the same math as above with the corresponding population: 0.041667/(1218452*0.189)

**Processing Time psi_x**
This is split into two parameters, psi_j and psi_c for processing that puts you in jail versus in the community respectively. We assume an average processing time of 12 hours, and per KL's notes, an average daily admission of 40 implies 40% go to jail and 60% go to the community. Assuming this is *not* associated with risk group (which it probably is), that gives us:

psi_j = 1/12 * 0.4 = 0.03333333
psi_c = 1/12 * 0.6 = 0.05

**Average Length of Stay (rho)**
Per KL's notes, setting this to 62 days, for a parameter value of 1/(62*24) = 0.000672 for the hourly probability of being released.

**Movement from Jail to Processing For Court, etc. (kappa)**
Assuming a patient census of ~ 2500 inmates, and 150 per day (6.25 per hour), we get an hourly probability of having a court appearance of 0.0025. 

**Length of Time Away from Jail (tau)**
Related to kappa, this is how long, once you have a court appearance, you hang out essentially in contact with the people in processing, rather than the people in jail. This is assumed in our model to be a half day, and thus 1/12. Note that during this time, and processing in general, you are assumed not to be able to be hospitalized - essentially, you have to get back to jail first to be evaluated.

**Average Shift Length (mu_c and mu_j)**
This is assumed to be 8 hours, during which the staff interact with the jail population, while for the other 16 they interact with the community. Currently we do not model interactions in the processing and trial areas due to the complexity involved.

**Relative Transmissability of Asymptomatics (sigma)**
Assumed to be 50%, per https://github.com/jsweitz/covid-19-ga-summer-2020/blob/master/covid19-ga-summary_jsw.pdf . This applies to the people in the E compartment.

**Probability of Requiring Hospitalization (omega and omegaH)**
Using Table 3 from https://www.medrxiv.org/content/10.1101/2020.03.09.20033357v1.full.pdf, and assuming that high risk and elderly patients conform to the 60-69 years age category, we estimate omegaH to be 0.118. Similarly, from Table 3 and some crude weighted averaging, we're going to estimate the hospitalization percentage for low risk individuals at 0.0625. For both of these figures, we assume the time it takes to be hospitalized is 5.9 days per https://www.medrxiv.org/content/10.1101/2020.03.03.20029983v1.full.pdf.

**Incubation Period (gamma)**
The incubation period (exposure to symptom onset) is 5.1 days per https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported

**Incubation Period + Asymptomatic Period (gammaA)**
This is for folks who will linger longer in the E category, transmitting at a reduced level before their infection resolves. Inherently, this must be >5.1 days. Reanalyzing the data from https://link.springer.com/content/pdf/10.1007/s11427-020-1661-4.pdf using an exponential fit, the estimation length of asymptomatic transmission is 11.9 days. Subtracting 5.1 from that number, as this is already accounted for in gamma, we get 1/(6.8*24)


**Probability of Need for Hospitalization Being Detected (zeta)**
This is assumed to be 100% accurate in the community, as we are modeling a community who is otherwise not having issues with its response to the epidemic (i.e. no bed shortages etc.). Currently set to be 0.80 in the jail setting - highly recommend we use this as one of the parameters to explore as an intervention.

**Symptomatic Period (delta)**
We estimate the symptomatic period to be 10 days per https://github.com/jsweitz/covid-19-ga-summer-2020/blob/master/covid19-ga-summary_jsw.pdf.

**Hospital Length-of-Stay Parameters (delta_discharge & delta_death)**
Building off the estimates from CHIME https://penn-chime.phl.io/ which seem especially relevant for this study, and recognizing that our hospitalized category primarily represents patients in need of critical ICU care, delta_discharge is set to 9 days. Per https://www.medrxiv.org/content/10.1101/2020.03.09.20033357v1.full.pdf, there is a 4.8 day *shorter* time between onset-of-symptoms to death vs. discharge, so delta_death is set to 4.2 days.

**Untreated Time until Death (delta_deathU)**
A fairly grim parameter for which we have no good data that I've seen. Thankfully. For the moment, this is set to the same value as delta_death. I suspect this is longer than it actually is.

**1-Case Fatality Rates (nu)**
For reasons that escape me now, but seemed reasonable at the time, nu is the probability of *surviving* one's COVID-19 infection, so 1-CFR. There are four rates here:

* nu: The case fatality rate for low-risk individuals
* nuH: The case fatality rate for high-risk/elderly individuals
* nuU: The case fatality rate for untreated individuals
* nuUH: The case fatality rate for high risk/elderly & untreated individuals

I am making the assumption for both nuU and nuUH that the CFR is 100%, and as a result, nuU and nuUH are both equal to 0. Note that these are the CFR's *given hospitalization*, hence the particularly dire assumption. The two parameters are being kept seperate for the moment, but that may change in the future - as will the fact that the discharge-related terms for untreated patients are, at the moment, irrelevant. It's retaining some of that in case we need it.

Estimating the CFR for the other two categories is built from https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6912e2-H.pdf

Assuming that our high-risk category is most similar to the 65-74 age group, the lower bound for the CDC's case-fatality rate is 4.3%. This is slightly conservative compared to other estimates, but in the same ball park. This would imply nuH = 0.957. This same report has a lower bound for the 20-44 year old age category, which will be the bulk of our low risk invidiuals, of 0.1%, implying a nu = 0.999.

This MMWR was published on March 27th, and is the latest U.S. specific information.