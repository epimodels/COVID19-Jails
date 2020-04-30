require(deSolve)

covid.model <- function(t, x, params) {
  Sk <- x[1]
  Ek <- x[2]
  Ik <- x[3]
  Rk <- x[4]
  Slc <- x[5]
  Elc <- x[6]
  Ilc <- x[7]
  Hlc <- x[8]
  Rlc <- x[9]
  Sec <- x[10]
  Eec <- x[11]
  Iec <- x[12]
  Hec <- x[13]
  Rec <- x[14]
  Shc <- x[15]
  Ehc <- x[16]
  Ihc <- x[17]
  Hhc <- x[18]
  Rhc <- x[19]
  Soc <- x[20]
  Eoc <- x[21]
  Ioc <- x[22]
  Hoc <- x[23]
  Roc <- x[24]
  Slp <- x[25]
  Elp <- x[26]
  Ilp <- x[27]
  Rlp <- x[28]
  Sep <- x[29]
  Eep <- x[30]
  Iep <- x[31]
  Rep <- x[32]
  Shp <- x[33]
  Ehp <- x[34]
  Ihp <- x[35]
  Rhp <- x[36]
  Slj <- x[37]
  Elj <- x[38]
  Ilj <- x[39]
  Hlj <- x[40]
  Rlj <- x[41]
  Sej <- x[42]
  Eej <- x[43]
  Iej <- x[44]
  Hej <- x[45]
  Rej <- x[46]
  Shj <- x[47]
  Ehj <- x[48]
  Ihj <- x[49]
  Hhj <- x[50]
  Rhj <- x[51]
  Slt <- x[52]
  Elt <- x[53]
  Ilt <- x[54]
  Rlt <- x[55]
  Set <- x[56]
  Eet <- x[57]
  Iet <- x[58]
  Ret <- x[59]
  Sht <- x[60]
  Eht <- x[61]
  Iht <- x[62]
  Rht <- x[63]
  Soj <- x[64]
  Eoj <- x[65]
  Ioj <- x[66]
  Roj <- x[67]
  CommunityInfections <- x[68]
  StaffInfections <- x[69]
  InmateInfections <- x[70]
  CommunityDeaths <- x[71]
  StaffDeaths <- x[72]
  InmateDeaths <- x[73]
  CommunityHosp <- x[74]
  StaffHosp <- x[75]
  InmateHosp <- x[76]
  TotalArrests <- x[77]


  with (
    as.list(params),
    {
      dSk <- -Bkk*Sk*(Ik+sigma*Ek) - Blck*Sk*(Ilc+Ioc+sigma*(Elc+Eoc)) - Beck*Sk*(Iec+sigma*Eec) - Bhck*Sk*(Ihc+sigma*Ehc)
      dEk <- Bkk*Sk*(Ik+sigma*Ek) + Blck*Sk*(Ilc+Ioc+sigma*(Elc+Eoc)) + Beck*Sk*(Iec+sigma*Eec) + Bhck*Sk*(Ihc+sigma*Ehc) - gamma*Ek - gammaA*Ek
      dIk <- gamma*Ek - quarantine_speedup_community*delta*Ik
      dRk <- quarantine_speedup_community*delta*Ik + gammaA*Ek

      dSlc <- -Bklc*Slc*(Ik+sigma*Ek) - Blclc*Slc*(Ilc+Ioc+sigma*(Elc+Eoc)) - Beclc*Slc*(Iec+sigma*Eec) - Bhclc*Slc*(Ihc+sigma*Ehc) - alphaL*Slc + psiC*Slp + rho*Slj
      dElc <- Bklc*Slc*(Ik+sigma*Ek) + Blclc*Slc*(Ilc+Ioc+sigma*(Elc+Eoc)) + Beclc*Slc*(Iec+sigma*Eec) + Bhclc*Slc*(Ihc+sigma*Ehc) - gamma*Elc - gammaA*Elc -alphaL*Elc + psiC*Elp + rho*Elj
      dIlc <- gamma*Elc - quarantine_speedup_community*delta*Ilc - omega*Ilc - alphaL*Ilc + psiC*Ilp + rho*Ilj
      dHlc <- omega*Ilc - delta_death*(1-nu)*Hlc - delta_discharge*nu*Hlc
      dRlc <- quarantine_speedup_community*delta*Ilc + gammaA*Elc + delta_death*(1-nu)*Hlc + delta_discharge*nu*Hlc - alphaL*Rlc + psiC*Rlp + rho*Rlj

      dSec <- -Bkec*Sec*(Ik+sigma*Ek) - Blcec*Sec*(Ilc+Ioc+sigma*(Elc+Eoc)) - Becec*Sec*(Iec+sigma*Eec) - Bhcec*Sec*(Ihc+sigma*Ehc) - alphaE*Sec + psiC*Sep + rhoH*Sej
      dEec <- Bkec*Sec*(Ik+sigma*Ek) + Blcec*Sec*(Ilc+Ioc+sigma*(Elc+Eoc)) + Becec*Sec*(Iec+sigma*Eec) + Bhcec*Sec*(Ihc+sigma*Ehc) - gamma*Eec - gammaA*Eec - alphaE*Eec + psiC*Eep + rhoH*Eej
      dIec <- gamma*Eec - quarantine_speedup_community*delta*Iec - omegaH*Iec - alphaE*Iec + psiC*Iep + rhoH*Iej
      dHec <- omegaH*Iec - delta_death*(1-nuH)*Hec - delta_discharge*nuH*Hec
      dRec <- quarantine_speedup_community*delta*Iec + gammaA*Eec + delta_death*(1-nuH)*Hec + delta_discharge*nuH*Hec - alphaE*Rec + psiC*Rep + rhoH*Rej

      dShc <- -Bkhc*Shc*(Ik+sigma*Ek) - Blchc*Shc*(Ilc+Ioc+sigma*(Elc+Eoc)) - Bechc*Shc*(Iec+sigma*Eec) - Bhchc*Shc*(Ihc+sigma*Ehc) - alphaH*Shc + psiC*Shp + rhoH*Shj
      dEhc <- Bkhc*Shc*(Ik+sigma*Ek) + Blchc*Shc*(Ilc+Ioc+sigma*(Elc+Eoc)) + Bechc*Shc*(Iec+sigma*Eec) + Bhchc*Shc*(Ihc+sigma*Ehc) - gamma*Ehc - gammaA*Ehc - alphaH*Ehc + psiC*Ehp + rhoH*Ehj
      dIhc <- gamma*Ehc - quarantine_speedup_community*delta*Ihc - omegaH*Ihc - alphaH*Ihc + psiC*Ihp + rhoH*Ihj
      dHhc <- omegaH*Ihc - delta_death*(1-nuH)*Hhc - delta_discharge*nuH*Hhc
      dRhc <- quarantine_speedup_community*delta*Ihc + gammaA*Ehc + delta_death*(1-nuH)*Hhc + delta_discharge*nuH*Hhc - alphaH*Rhc + psiC*Rhp + rhoH*Rhj

      dSoc <- muC*Soj - muJ*Soc - Bkoc*Soc*(Ik+sigma*Ek) - Blcoc*Soc*(Ilc+Ioc+sigma*(Elc+Eoc)) - Becoc*Soc*(Iec+sigma*Eec) - Bhcoc*Soc*(Ihc+sigma*Ehc)
      dEoc <- muC*Eoj - muJ*Eoc + Bkoc*Soc*(Ik+sigma*Ek) + Blcoc*Soc*(Ilc+Ioc+sigma*(Elc+Eoc)) + Becoc*Soc*(Iec+sigma*Eec) + Bhcoc*Soc*(Ihc+sigma*Ehc) - gamma*Eoc - gammaA*Eoc
      dIoc <- muC*Ioj  -muJ*Ioc + gamma*Eoc - quarantine_speedup_community*delta*Ioc - omega*Ioc
      dHoc <- omega*(Ioc+Ioj) - delta_death*(1-nu)*Hoc - delta_discharge*nu*Hoc
      dRoc <- muC*Roj - muJ*Roc + quarantine_speedup_community*delta*Ioc + gammaA*Eoc + delta_death*(1-nu)*Hoc + delta_discharge*nu*Hoc

      dSlp <- alphaL*Slc - psiC*Slp - psiJ*Slp - Blplp*Slp*(Ilp+Ilt+sigma*(Elp+Elt)) - Beplp*Slp*(Iep+Iet+sigma*(Eep+Eet)) - Bhplp*Slp*(Ihp+Iht+sigma*(Ehp+Eht))
      dElp <- alphaL*Elc - psiC*Elp - psiJ*Elp + Blplp*Slp*(Ilp+Ilt+sigma*(Elp+Elt)) + Beplp*Slp*(Iep+Iet+sigma*(Eep+Eet)) + Bhplp*Slp*(Ihp+Iht+sigma*(Ehp+Eht)) - gamma*Elp - gammaA*Elp
      dIlp <- alphaL*Ilc - psiC*Ilp - psiJ*Ilp + gamma*Elp - quarantine_speedup_jail*delta*Ilp
      dRlp <- alphaL*Rlc - psiC*Rlp - psiJ*Rlp + quarantine_speedup_jail*delta*Ilp + gammaA*Elp

      dSep <- alphaE*Sec - psiC*Sep - psiJ*Sep - Blpep*Sep*(Ilp+Ilt+sigma*(Elp+Elt)) - Bepep*Sep*(Iep+Iet+sigma*(Eep+Eet)) - Bhpep*Sep*(Ihp+Iht+sigma*(Ehp+Eht))
      dEep <- alphaE*Eec - psiC*Eep - psiJ*Eep + Blpep*Sep*(Ilp+Ilt+sigma*(Elp+Elt)) + Bepep*Sep*(Iep+Iet+sigma*(Eep+Eet)) + Bhpep*Sep*(Ihp+Iht+sigma*(Ehp+Eht)) - gamma*Eep - gammaA*Eep
      dIep <- alphaE*Iec - psiC*Iep - psiJ*Iep + gamma*Eep - quarantine_speedup_jail*delta*Iep
      dRep <- alphaE*Rec - psiC*Rep - psiJ*Rep + quarantine_speedup_jail*delta*Iep + gammaA*Eep

      dShp <- alphaH*Shc - psiC*Shp - psiJ*Shp - Blphp*Shp*(Ilp+Ilt+sigma*(Elp+Elt)) - Bephp*Shp*(Iep+Iet+sigma*(Eep+Eet)) - Bhphp*Shp*(Ihp+Iht+sigma*(Ehp+Eht))
      dEhp <- alphaH*Ehc - psiC*Ehp - psiJ*Ehp + Blphp*Shp*(Ilp+Ilt+sigma*(Elp+Elt)) + Bephp*Shp*(Iep+Iet+sigma*(Eep+Eet)) + Bhphp*Shp*(Ihp+Iht+sigma*(Ehp+Eht)) - gamma*Ehp - gammaA*Ehp
      dIhp <- alphaH*Ihc - psiC*Ihp - psiJ*Ihp + gamma*Ehp - quarantine_speedup_jail*delta*Ihp
      dRhp <- alphaH*Rhc - psiC*Rhp - psiJ*Rhp + quarantine_speedup_jail*delta*Ihp + gammaA*Ehp

      dSlj <- psiJ*Slp - kappa*tau*Slj - rho*Slj - Bljlj*Slj*(Ilj+sigma*Elj) - Bejlj*Slj*(Iej+sigma*Eej) - Bhjlj*Slj*(Ihj+sigma*Ehj) -  Balj*Slj*(Ioj+sigma*Eoj) +kappa*Slt
      dElj <- psiJ*Elp - kappa*tau*Elj - rho*Elj + Bljlj*Slj*(Ilj+sigma*Elj) + Bejlj*Slj*(Iej+sigma*Eej) + Bhjlj*Slj*(Ihj+sigma*Ehj) +  Balj*Slj*(Ioj+sigma*Eoj) - gamma*Elj - gammaA*Elj + kappa*Elt
      dIlj <- psiJ*Ilp - kappa*tau*Ilj - rho*Ilj + gamma*Elj - quarantine_speedup_jail*delta*Ilj - omega*zeta*Ilj - (1-nuU)*(1-zeta)*delta_deathU*Ilj - nuU*(1-zeta)*delta_discharge*Ilj + kappa*Ilt
      dHlj <- omega*zeta*Ilj - delta_death*(1-nu)*Hlj - delta_discharge*nu*Hlj
      dRlj <- psiJ*Rlp - kappa*tau*Rlj - rho*Rlj + quarantine_speedup_jail*delta*Ilj + delta_death*(1-nu)*Hlj + delta_discharge*nu*Hlj + (1-nuU)*(1-zeta)*delta_deathU*Ilj + nuU*(1-zeta)*delta_discharge*Ilj + gammaA*Elj + kappa*Rlt

      dSej <- psiJ*Sep - kappa*tau*Sej - rhoH*Sej - Bljej*Sej*(Ilj+sigma*Elj) - Bejej*Sej*(Iej+sigma*Eej) - Bhjej*Sej*(Ihj+sigma*Ehj) -  Bojaj*Sej*(Ioj+sigma*Eoj) + kappa*Set
      dEej <- psiJ*Eep - kappa*tau*Eej - rhoH*Eej + Bljej*Sej*(Ilj+sigma*Elj) + Bejej*Sej*(Iej+sigma*Eej) + Bhjej*Sej*(Ihj+sigma*Ehj) +  Bojaj*Sej*(Ioj+sigma*Eoj) - gamma*Eej -gammaA*Eej + kappa*Eet
      dIej <- psiJ*Iep - kappa*tau*Iej - rhoH*Iej + gamma*Eej - quarantine_speedup_jail*delta*Iej - omegaH*zeta*Iej- (1-nuUH)*(1-zeta)*delta_deathU*Iej - nuUH*(1-zeta)*delta_discharge*Iej + kappa*Iet
      dHej <- omegaH*zeta*Iej - delta_death*(1-nuH)*Hej - delta_discharge*nuH*Hej
      dRej <- psiJ*Rep - kappa*tau*Rej - rhoH*Rej + quarantine_speedup_jail*delta*Iej + delta_death*(1-nuH)*Hej + delta_discharge*nuH*Hej + (1-nuUH)*(1-zeta)*delta_deathU*Iej + nuUH*(1-zeta)*delta_discharge*Iej + gammaA*Eej + kappa*Ret

      dShj <- psiJ*Shp - kappa*tau*Shj - rhoH*Shj - Bljhj*Shj*(Ilj+sigma*Elj) - Bejhj*Shj*(Iej+sigma*Eej) - Bhjhj*Shj*(Ihj+sigma*Ehj) -  Bojaj*Shj*(Ioj+sigma*Eoj) + kappa*Sht
      dEhj <- psiJ*Ehp - kappa*tau*Ehj - rhoH*Ehj + Bljhj*Shj*(Ilj+sigma*Elj) + Bejhj*Shj*(Iej+sigma*Eej) + Bhjhj*Shj*(Ihj+sigma*Ehj) +  Bojaj*Shj*(Ioj+sigma*Eoj) - gamma*Ehj - gammaA*Ehj + kappa*Eht
      dIhj <- psiJ*Ihp - kappa*tau*Ihj - rhoH*Ihj + gamma*Ehj - quarantine_speedup_jail*delta*Ihj - omegaH*zeta*Ihj- (1-nuUH)*(1-zeta)*delta_deathU*Ihj - nuUH*(1-zeta)*delta_discharge*Ihj + kappa*Iht
      dHhj <- omegaH*zeta*Ihj - delta_death*(1-nuH)*Hhj - delta_discharge*nuH*Hhj
      dRhj <- psiJ*Rhp - kappa*tau*Rhj - rhoH*Rhj + quarantine_speedup_jail*delta*Ihj + delta_death*(1-nuH)*Hhj + delta_discharge*nuH*Hhj + (1-nuUH)*(1-zeta)*delta_deathU*Ihj + nuUH*(1-zeta)*delta_discharge*Ihj + gammaA*Ehj + kappa*Rht

      dSlt <- kappa*tau*Slj - kappa*Slt - Blplt*Slt*(Ilp+Ilt+sigma*(Elp+Elt)) - Beplt*Slt*(Iep+Iet+sigma*(Eep+Eet)) - Bhplt*Slt*(Ihp+Iht+sigma*(Ehp+Eht))
      dElt <- kappa*tau*Elj - kappa*Elt + Blplt*Slt*(Ilp+Ilt+sigma*(Elp+Elt)) + Beplt*Slt*(Iep+Iet+sigma*(Eep+Eet)) + Bhplt*Slt*(Ihp+Iht+sigma*(Ehp+Eht)) - gamma*Elt - gammaA*Elt
      dIlt <- kappa*tau*Ilj - kappa*Ilt + gamma*Elt - quarantine_speedup_jail*delta*Ilt
      dRlt <- kappa*tau*Rlj - kappa*Rlt + quarantine_speedup_jail*delta*Ilt + gammaA*Elt

      dSet <- kappa*tau*Sej - kappa*Set - Blpet*Set*(Ilp+Ilt+sigma*(Elp+Elt)) - Bepet*Set*(Iep+Iet+sigma*(Eep+Eet)) - Bhpet*Set*(Ihp+Iht+sigma*(Ehp+Eht))
      dEet <- kappa*tau*Eej - kappa*Eet + Blpet*Set*(Ilp+Ilt+sigma*(Elp+Elt)) + Bepet*Set*(Iep+Iet+sigma*(Eep+Eet)) + Bhpet*Set*(Ihp+Iht+sigma*(Ehp+Eht)) - gamma*Eet - gammaA*Eet
      dIet <- kappa*tau*Iej - kappa*Iet + gamma*Eet - quarantine_speedup_jail*delta*Iet
      dRet <- kappa*tau*Rej - kappa*Ret + quarantine_speedup_jail*delta*Iet + gammaA*Eet

      dSht <- kappa*tau*Shj - kappa*Sht - Blpht*Sht*(Ilp+Ilt+sigma*(Elp+Elt)) - Bepht*Sht*(Iep+Iet+sigma*(Eep+Eet)) - Bhpht*Sht*(Ihp+Iht+sigma*(Ehp+Eht))
      dEht <- kappa*tau*Ehj - kappa*Eht + Blpht*Sht*(Ilp+Ilt+sigma*(Elp+Elt)) + Bepht*Sht*(Iep+Iet+sigma*(Eep+Eet)) + Bhpht*Sht*(Ihp+Iht+sigma*(Ehp+Eht)) - gamma*Eht - gammaA*Eht
      dIht <- kappa*tau*Ihj - kappa*Iht + gamma*Eht - quarantine_speedup_jail*delta*Iht
      dRht <- kappa*tau*Rhj - kappa*Rht + quarantine_speedup_jail*delta*Iht + gammaA*Eht

      dSoj <- muJ*Soc - muC*Soj -  Baoj*Soj*(Ilj+Iej+Ihj+sigma*(Elj+Eej+Ehj)) -   Bojoj*Soj*(Ioj+sigma*Eoj)
      dEoj <- muJ*Eoc - muC*Eoj +  Baoj*Soj*(Ilj+Iej+Ihj+sigma*(Elj+Eej+Ehj)) +   Bojoj*Soj*(Ioj+sigma*Eoj) - gamma*Eoj - gammaA*Eoj
      dIoj <- muJ*Ioc - muC*Ioj + gamma*Eoj - omega*Ioj - quarantine_speedup_jail*delta*Ioj
      dRoj <- muJ*Roc - muC*Roj + quarantine_speedup_jail*delta*Ioj + gammaA*Eoj

      dCommunityInfections <- Bkk*Sk*(Ik+sigma*Ek)+Blck*Sk*(Ilc+Ioc+sigma*(Elc+Eoc))+Beck*Sk*(Iec+sigma*Eec)+Bhck*Sk*(Ihc+sigma*Ehc)+Bklc*Slc*(Ik+sigma*Ek)+Blclc*Slc*(Ilc+Ioc+sigma*(Elc+Eoc))+Beclc*Slc*(Iec+sigma*Eec)+Bhclc*Slc*(Ihc+sigma*Ehc)+Bkec*Sec*(Ik+sigma*Ek)+Blcec*Sec*(Ilc+Ioc+sigma*(Elc+Eoc))+Becec*Sec*(Iec+sigma*Eec)+Bhcec*Sec*(Ihc+sigma*Ehc)+Bkhc*Shc*(Ik+sigma*Ek)+Blchc*Shc*(Ilc+Ioc+sigma*(Elc+Eoc))+Bechc*Shc*(Iec+sigma*Eec)+Bhchc*Shc*(Ihc+sigma*Ehc)
      dStaffInfections <- Bkoc*Soc*(Ik+sigma*Ek)+Blcoc*Soc*(Ilc+Ioc+sigma*(Elc+Eoc))+Becoc*Soc*(Iec+sigma*Eec)+Bhcoc*Soc*(Ihc+sigma*Ehc)+ Baoj*Soj*(Ilj+Iej+Ihj+sigma*(Elj+Eej+Ehj))
      dInmateInfections <-Blplp*Slp*(Ilp+Ilt+sigma*(Elp+Elt))+Beplp*Slp*(Iep+Iet+sigma*(Eep+Eet))+Bhplp*Slp*(Ihp+Iht+sigma*(Ehp+Eht))+Blpep*Sep*(Ilp+Ilt+sigma*(Elp+Elt))+Bepep*Sep*(Iep+Iet+sigma*(Eep+Eet))+Bhpep*Sep*(Ihp+Iht+sigma*(Ehp+Eht))+Blphp*Shp*(Ilp+Ilt+sigma*(Elp+Elt))+Bephp*Shp*(Iep+Iet+sigma*(Eep+Eet))+Bhphp*Shp*(Ihp+Iht+sigma*(Ehp+Eht))+Bljlj*Slj*(Ilj+sigma*Elj)+Bejlj*Slj*(Iej+sigma*Eej)+Bhjlj*Slj*(Ihj+sigma*Ehj)+Bljej*Sej*(Ilj+sigma*Elj)+Bejej*Sej*(Iej+sigma*Eej)+Bhjej*Sej*(Ihj+sigma*Ehj)+Bljhj*Shj*(Ilj+sigma*Elj)+Bejhj*Shj*(Iej+sigma*Eej)+Bhjhj*Shj*(Ihj+sigma*Ehj)+Blplt*Slt*(Ilp+Ilt+sigma*(Elp+Elt))+Beplt*Slt*(Iep+Iet+sigma*(Eep+Eet))+Bhplt*Slt*(Ihp+Iht+sigma*(Ehp+Eht))+Blpet*Set*(Ilp+Ilt+sigma*(Elp+Elt))+Bepet*Set*(Iep+Iet+sigma*(Eep+Eet))+Bhpet*Set*(Ihp+Iht+sigma*(Ehp+Eht))+Blpht*Sht*(Ilp+Ilt+sigma*(Elp+Elt))+Bepht*Sht*(Iep+Iet+sigma*(Eep+Eet))+Bhpht*Sht*(Ihp+Iht+sigma*(Ehp+Eht))
      dCommunityDeaths <- delta_death*(1-nu)*Hlc + delta_death*(1-nuH)*Hec + delta_death*(1-nuH)*Hhc
      dStaffDeaths <- delta_death*(1-nu)*Hoc
      dInmateDeaths <- delta_death*(1-nu)*Hlj+(1-nuU)*(1-zeta)*delta_deathU*Ilj+delta_death*(1-nuH)*Hej+(1-nuUH)*(1-zeta)*delta_deathU*Iej+delta_death*(1-nuH)*Hhj+(1-nuUH)*(1-zeta)*delta_deathU*Ihj
      dCommunityHosp <- omega*Ilc + omegaH*Iec + omegaH*Ihc
      dStaffHosp <- omega*(Ioc + Ioj)
      dInmateHosp <- omega*zeta*Ilj + omegaH*zeta*Iej + omegaH*zeta*Ihj
      dTotalArrests <- alphaL*(Slc+Elc+Ilc+Rlc)+alphaE*(Sec+Eec+Iec+Rec)+alphaH*(Shc+Ehc+Ihc+Rhc)

       res <- c(dSk, dEk, dIk, dRk,
               dSlc,dElc,dIlc,dHlc,dRlc,
               dSec,dEec,dIec,dHec,dRec,
               dShc,dEhc,dIhc,dHhc,dRhc,
               dSoc,dEoc,dIoc,dHoc,dRoc,
               dSlp,dElp,dIlp,dRlp,
               dSep,dEep,dIep,dRep,
               dShp,dEhp,dIhp,dRhp,
               dSlj,dElj,dIlj,dHlj,dRlj,
               dSej,dEej,dIej,dHej,dRej,
               dShj,dEhj,dIhj,dHhj,dRhj,
               dSlt,dElt,dIlt,dRlt,
               dSet,dEet,dIet,dRet,
               dSht,dEht,dIht,dRht,
               dSoj,dEoj,dIoj,dRoj,
               dCommunityInfections,
               dStaffInfections,
               dInmateInfections,
               dCommunityDeaths,
               dStaffDeaths,
               dInmateDeaths,
               dCommunityHosp,
               dStaffHosp,
               dInmateHosp,
               dTotalArrests

               )
      list(res)
    }
  )
}