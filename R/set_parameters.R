
set_parameters <- function(betas, processing_mix, jail_mix, pop,
                           arrest_rate_cut = c("low_risk" = 1, "high_risk" = 1, "elderly" = 1),
                           faster_release=1,faster_release_h=1, detection_proportion=1,
                           quarantine_speedup_community = 1, quarantine_speedup_jail=1, high_risk_factor = 1.4){

  #this is just because R doesnt handle named vectors well inside other named vectors
  arc_l <- unname(arrest_rate_cut[names(arrest_rate_cut) =='low_risk'])
  arc_e <- unname(arrest_rate_cut[names(arrest_rate_cut) =='elderly'])
  arc_h <- unname(arrest_rate_cut[names(arrest_rate_cut) =='high_risk'])

  # reducing hardcoding - these could mostly become variable inputs too if we want
  period <- 24 #hours in a day
  trial_hours <- period/2 #half a day
  processing_to_community_time <- period/2 #half a day
  processing_to_jail_time <- period/2 #half a day
  workday <- period/3
  off_time <- period - workday
  allegheny_co_pop <- 1218452
  target_jail_pop <- 2500


  params <- c(
    Bkk = betas[1,1], #from kid to kid (all kids assumed to be in community)
    #Bkk = beta1
    Blck = betas[2,1], #from low adult/staff comm to kid
#    Blck = beta2
    Beck = betas[2,1], #from elderly community to kid
    #Beck = beta3
    Bhck = betas[2,1], #from high adult comm  to kid
    #Bhck = beta4
    Bklc = betas[1,2], #from kid to low adult comm
    #Bklc = beta5
    Blclc = betas[2,2], #from low adult comm to low adult comm
    #Blclc = beta6
    Beclc = betas[3,2], #from elderly comm to low adult comm
    #Beclc = beta7
    Bhclc = betas[2,2], #from high adult comm to low adult comm
#    Bhclc = beta8
    Bkec = betas[1,3], #from kid to elderly comm
  #Bkec = beta9
    Blcec = betas[2,3], #from low adult comm / staff com to elderly comm
  #Blcec = beta10
    Becec = betas[3,3], #from elderly comm to elderly comm
  #Becec = beta11
    Bhcec = betas[2,3], #from high comm to elderly comm
  #Bhcec = beta12
    Bkhc = high_risk_factor*betas[1,2], #from kid to high adult comm
  #Bkhc = beta13
    Blchc = high_risk_factor*betas[2,2], #from low adult comm/ staff comm to high adult comm
  #Blchc = beta14
    Bechc = high_risk_factor*betas[3,2], #from elderly comm to high adult comm
  #Bechc = beta15
    Bhchc = high_risk_factor*betas[2,2], #from high adult comm to high adult comm
  #Bhchc = beta16
    Bkoc = betas[1,2], #from kid to staff comm
  #Bkoc = beta17
    Blcoc = betas[2,2], #from low adult comm / staff comm to staff comm
  #Blcoc = beta18
    Becoc = betas[3,2], #from elderly comm to staff comm
  #Becoc = beta19
    Bhcoc = betas[2,2], #from high adult comm to staff comm
  #Bhcoc = beta20
    Blplp = processing_mix, #from low adult processing/trial to low adult processing
  #Blplp = beta21
    Beplp = processing_mix, #from elderly (processing/trial) to low processing
  #Beplp = beta22
    Bhplp = processing_mix, #from high adult (processing/trial) to low processing
  #Bhplp = beta23
    Blpep = processing_mix, #from low adult (processing/trial) to elderly processing
  #Blpep = beta24
    Bepep = processing_mix, #from from elderly (processing/trial) to elderly processing
  #Bepep = beta25
    Bhpep = processing_mix, #from high adult (processing/trial) to elderly processing
  #Bhpep = beta26
    Blphp = high_risk_factor*processing_mix, #from low adult (processing/trial) to high adult processing
  #Blphp = beta27
    Bephp = high_risk_factor*processing_mix, #from elderly (processing/trial) to high adult processing
  #Bephp = beta28
    Bhphp = high_risk_factor*processing_mix, #from high adult (processing/trial) to high adult processing
  #Bhphp = beta29
    Bljlj = jail_mix, #from low adult jail to low adult jail
  #Bljlj = beta30
    Bejlj = jail_mix, #from elderly jail to low adult jail
  #Bejlj = beta31
    Bhjlj = jail_mix, #from high adult jail to low adult jail
  #Bhjlj = beta32
    Bljej = jail_mix, #from low adult jail to elderly jail
  #Bljej = beta33
    Bejej = jail_mix, #from elderly jail to elderly jail
  #Bejej = beta34
    Bhjej = jail_mix, #from high adult jail to elderly jail
  #Bhjej = beta35
    Bljhj = high_risk_factor*jail_mix, #from low adult jail to high adult jail
  #Bljhj = beta36
    Bejhj = high_risk_factor*jail_mix, #from elderly jail to high adult jail
  #Bejhj = beta37
    Bhjhj = high_risk_factor*jail_mix, #from high adult jail to high adult jail
  #Bhjhj = beta38
    Blplt = processing_mix, #from low adult (trial/processing) to low adult trial
  #Blplt = beta39
    Beplt = processing_mix, #from elderly (trial/processing) to low adult trial
  #Beplt = beta40
    Bhplt = processing_mix, #from high adult (trial/processing) to low adult trial
  #Bhplt = beta41
    Blpet = processing_mix, #from low adult (trial/prodcessing) to elderly trial
  #Blpet = beta42
    Bepet = processing_mix, #from elderly (trial/processing) to elderly trial
  #Bepet = beta43
    Bhpet = processing_mix, #from high adult (trial/processing) to elderly trial
  #Bhpet = beta44
    Blpht = high_risk_factor*processing_mix, #from low adult (trial/processing) to high adult trial
  #Blpht = beta45
    Bepht = high_risk_factor*processing_mix, #from elderly (trial/processing) to high adult trial
  #Bepht = beta46
    Bhpht = high_risk_factor*processing_mix, #from high adult (trial/processing) to high adult trial
  #Bhpht = beta47
    Baoj = jail_mix, #from low/high/elderly jail to officer jail
  #Baoj = beta48
    Bojoj = jail_mix, #staff-staff interaction
  #Boo = beta49
    Balj = jail_mix, #staff-inmate interaction (low risk)
  #Balj
    Bojaj = high_risk_factor*jail_mix, #staff-inmate interaction (high risk)
  #Bojaj = beta51



    sigma = 0.5, # Percent decreased infectivity while exposed
    gamma = 1/(5.1*period), # Incubation period
    gammaA = 1/(6.7*period), # Recovery rate from E (asymptomatic period)
    delta = 1/(10*period), # Recovery rate from I (symptomatic period)  #in simulation going to speedup for people going into self-isolation
    delta_discharge = 1/(9*period), # Time from hospitalization to discharge
    delta_death = 1/(4.2*period), # Time from hospitalization to death
    delta_deathU = 1/(4.2*period), # Time to death for unhospitalized
    omega = 1/(5.9*period)*0.0625, # Rate of hospitalization (low risk)
    omegaH = 1/(5.9*period)*0.118, # Rate of hospitalization (high risk)
    nu = 0.95, # Probability of survival (low risk)
    nuH = 0.6666, # Probability of survival (high risk)
    nuU = 0.0, # Probability of survival (low risk untreated)
    nuUH = 0.0, # Probability of survival (high risk untreated)
    alphaL = 2.5/(allegheny_co_pop*0.574) * arc_l, # low risk adult arrest rate
    alphaE = 0.041667/(allegheny_co_pop*0.189)* arc_e, # elderly adult arrest rate
    alphaH = 1.667/(allegheny_co_pop*0.05)* arc_h, # high risk adult arrest rate
    psiC = 1/processing_to_community_time * 0.6, #rate of release to community from processing
    psiJ = 1/processing_to_jail_time * 0.4, #rate of admission to jail from processing
    kappa = 6.5/target_jail_pop, # rate of movement from jail to trial and back
    tau = 1/trial_hours, # duration of time at "trial"
    rho = 1/(62 * period) * faster_release, # rate of release from jail to community
    rhoH = 1/(62*period)*min(faster_release,faster_release_h), #rate of release from jail to community for high risk
    muC = 1/workday, # rate at which staff return to community from work
    muJ = 1/off_time, # rate at which staff arrive at work from community
    zeta = 1.0 * detection_proportion,# probability an incarcerated person needing treatment will recieve it
    quarantine_speedup_community = quarantine_speedup_community,
    quarantine_speedup_jail = quarantine_speedup_jail

  )

return(params)
}

generate_mixing_betas <- function(kids = c(1, 2.57, .28),
                                  adult = c(0.57, 12.29, 3.8),
                                  elderly = c(.02, 1.51, 1.78)
                                  ){

  betas <- rbind(kids,
                 adult,
                 elderly
                 )

  rownames(betas) <- c("kid", "adult", "elderly")
  colnames(betas) <- c("kid", "adult", "elderly")

  return(betas)
}


fake_function <- function(params){

  with(as.list(params), print(beta1))
}

