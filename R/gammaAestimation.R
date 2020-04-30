library(flexsurv)

at <- read.csv(here::here("data/asymptomatic_transmisison.csv"))

transmission_times <- Surv(at$Tdays, at$Nt)

sEXP  <- flexsurvreg(transmission_times ~ 1,dist='exponential',data=at)
plot(sEXP,col="blue",lwd=2,lwd.obs=2,lty.obs=2,xlab="Days",ylab="Proportion Shedding",cex.axis=1.2,cex.lab=1.5)
legend("topright",inset=0.02, legend=c("Kaplan-Meier Estimate","Exponential Fit"), col=c("black","blue"),lwd=c(2,2),lty=c(2,1),bty='n',cex=1.2)