devtools::install_github("EcoForecast/ecoforecastR")
library(ecoforecastR)
##` @param IC    Initial Conditions
##` @param beta_precip     Intrinsic growth rate
##` @param Kg    Across-site ('global') mean carrying capacity
##` @param alpha Site random effect
##` @param beta  Slope of precipitation effect on K
##` @param ppt   Precipitation forecast
##` @param Q     Process error (default = 0 for deterministic runs)
##` @param n     Size of Monte Carlo ensemble

### read in MODIS data
modis1 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2017/MOD14A2.csv")
modis2 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2018/MOD14A2.csv")
# modis3 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2019/MOD14A2.csv")
modis <- rbind(modis1, modis2)
rm(modis1)
rm(modis2)
# modis_days <- get_days(data_type = "MOD14A2")
dates <- format(as.POSIXct(modis$X1), "%Y%m%d")
modis <- modis$X2
# to make it run automatically, we will need to get the latest mcmc result
load("/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/output/mcmc/20190506.historical_calibration_with_modis_and_viirs_better_priors_and_sigma.convergence_passed_GBR_test.JAGS_run.Rdata")
setwd("/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/data/GEFS/sum_data_v2")
metes <- list.files()
days <- seq(1,length(metes),8)
temp <- c()
precip <- c()
for(i in 1:length(days)){
  mete <- read.csv(metes[days[i]],sep=";")
  this.temp <- as.numeric(gsub(",",".",mete[,2]))
  this.precip <- as.numeric(gsub(",",".",mete[,3]))
  temp <- append(this.temp,temp)
  precip <- append(this.precip,precip)
}



### settings
Nmc = 1000         ## set number of Monte Carlo draws
# N.cols <- c("black","red","green","blue","orange") ## set colors
trans <- 0.5       ## set transparancy
NT = length(modis)
time1 = 1:NT    ## calibration period
time2 = 6       ## forecast period
time = 1:(NT+8+time2)    ## total time



### function to plot
plot.run <- function(xlim,ylim){
  plot(time,time,type='n',xlim=xlim,ylim=ylim,
       xlab = "time",ylab="Bure Area (m2)")
  ecoforecastR::ciEnvelope(time1,ci[1,],ci[3,],col=col.alpha("lightBlue",0.6))
  lines(time1,ci[2,],col="blue")
  points(time1,modis)
}
## separate jags.burn into params and predict and sigma
params <- list()
predict <- list()
sigma <- list()
for(i in 1:3){
  mcmc <- jags.burn[[i]]
  params.i <- mcmc[,c(1,2)]
  sigma.i <- mcmc[,3]
  predict.i <- mcmc[,4:dim(mcmc)[2]]
  params[[i]] <- params.i
  predict[[i]] <- predict.i
  sigma[[i]] <- sigma.i
}
out.jags.burn <- list(params=params,predict=predict,sigma=sigma)
rm(params.i)
rm(predict.i)
rm(sigma.i)
rm(mcmc)

ci <- apply(as.matrix(rbind(out.jags.burn$predict[[1]],out.jags.burn$predict[[2]],out.jags.burn$predict[[3]])),
                      2,quantile,c(0.025,0.5,0.975))
# ci <- apply(as.matrix(jags.burn$predict),2,quantile,c(0.025,0.5,0.975))
plot.run(xlim=range(time),ylim = c(min(ci),max(ci)))

### function to forecast
forecastN <- function(IC,
                      beta_precip,
                      beta_temp,
                      sigma,
                      y,
                      y_2,
                      n=Nmc){
  N <- matrix(NA,n,time2)  ## storage
  Nprev <- IC           ## initialize
  mu <- Nprev +beta_precip*y[1] + beta_temp*y_2[1]
  N[,1] <- rnorm(n,mu,sigma)
  Nprev <- N[,1]
  for(t in 2:time2) {
    mu <-N[,1] + beta_precip*y[t] + beta_temp*y_2[t] #+beta_IC
    N[,t] <- rnorm(n,mu,sigma)
    Nprev <- N[,t] 
  }
  return(N)
}




### calculate mean of all inputs
## parameters
params <- as.matrix(out.jags.burn$params)
sigma <- c(out.jags.burn$sigma)
params.mean <- matrix(NA,ncol=2,nrow=length(params))
sigma.mean <- c()
for (i in 1:length(params)) {
  this.params.mean <- apply(params[[i]], 2, mean)
  this.sigma.mean <- mean(sigma[[i]])
  params.mean[i,] <- this.params.mean
  sigma.mean <- append(this.sigma.mean,sigma.mean)
}
params.mean <- colMeans(params.mean)
sigma.mean <- mean(sigma.mean)
## initial conditions
IC <- as.matrix(as.matrix(rbind(out.jags.burn$predict[[1]],out.jags.burn$predict[[2]],out.jags.burn$predict[[3]])))



N.det <- forecastN(IC=mean(out.jags.burn$predict[[1]][,55]),
                   beta_precip =params.mean[1],
                   beta_temp = params.mean[2],
                   sigma = 0,
                   y = precip,
                   y_2 = temp,
                   n=1)

## Plot run
plot.run(ylim = c(min(ci),max(ci)))
lines(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.det,col="purple",lwd=3)



### Uncertainty Analysis
prow = sample.int(nrow(IC),Nmc,replace=TRUE)

## IC Uncertainty
N.I <- forecastN(IC=IC[prow,"x[92]"],  ## sample IC
                 beta_precip =params.mean[1],
                 beta_temp = params.mean[2],
                 sigma = 0,
                 y = precip,
                 y_2 = temp,
                 n=Nmc)
N.I.ci = apply(N.I,2,quantile,c(0.025,0.5,0.975))

## Parameter Uncertainty
params <- rbind(out.jags.burn$params[[1]],out.jags.burn$params[[2]],out.jags.burn$params[[3]])
N.IP <- forecastN(IC=IC[prow,"x[92]"],  ## sample IC
                  beta_precip = params[prow,"beta_precip"],
                  beta_temp = params[prow,"beta_temp"],
                  sigma = 0,
                  y = precip,
                  y_2 = temp,
                  n=Nmc)


N.IP.ci = apply(N.IP,2,quantile,c(0.025,0.5,0.975))

## process uncertainty
sigma <- c(out.jags.burn$sigma[[1]],out.jags.burn$sigma[[2]],out.jags.burn$sigma[[3]])
Qmc <- 1/sqrt(1/sigma[prow])
N.IPE <- forecastN(IC=IC[prow,"x[92]"],  ## sample IC
                   beta_precip = params[prow,"beta_precip"],
                   beta_temp = params[prow,"beta_temp"],
                   sigma = Qmc,
                   y = precip,
                   y_2 = temp,
                   n=Nmc)
N.IPE.ci = apply(N.IPE,2,quantile,c(0.025,0.5,0.975))

## plot them together
par(mfrow=c(2,2))
plot.run(xlim = c(101,106),ylim = c(min(N.IPE.ci),max(N.IPE.ci)))
ecoforecastR::ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.IPE.ci[1,],N.IPE.ci[3,],col=col.alpha("green",trans))
lines(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.IPE.ci[2,])
legend("topright",legend=c("IC,Pra,Pro"),col="green",lty=1,lwd=3)
plot.run(xlim = c(101,106),ylim = c(min(N.IPE.ci),max(N.IPE.ci)))
ecoforecastR::ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.IP.ci[1,],N.IP.ci[3,],col=col.alpha("red",trans))
lines(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.IP.ci[2,])
legend("topright",legend=c("IC,Pra"),col="red",lty=1,lwd=3)
plot.run(xlim = c(101,106),ylim = c(min(N.IPE.ci),max(N.IPE.ci)))
ecoforecastR::ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.I.ci[1,],N.I.ci[3,],col=col.alpha("yellow",trans))
lines(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.I.ci[2,])
legend("topright",legend=c("IC"),col="yellow",lty=1,lwd=3)
plot.run(xlim = range(time),ylim = c(min(ci),max(ci)))
lines(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],N.det,col="purple",lwd=3)
legend("topright",legend=c("Obs","Fit","Pre"),col=c(1,"blue","purple"),
       lty=c(NA,1,1),pch=c(1,NA,NA),lwd=c(NA,1,1))


varI <- apply(N.I,2,var)
varIP <- apply(N.IP,2,var)
varIPE <- apply(N.IPE,2,var)
varMat <- rbind(varI,varIP,varIPE)
V.pred.rel <- apply(varMat,2,function(x){x/max(x)})
par(mfrow=c(2,1))
plot(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],V.pred.rel[1,],ylim=c(0,1),type="n",
     xlab=c("time"),ylab="proportion of variance",main = c("Relative variance: out-of-sample"))
ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],rep(0,ncol(V.pred.rel)),V.pred.rel[1,],col="yellow")
ecoforecastR::ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],V.pred.rel[1,],V.pred.rel[2,],col="green")
ecoforecastR::ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],V.pred.rel[2,],V.pred.rel[3,],col="red")
legend("topright",legend=c("IC","IC,Par","IC,Pra,Pro"),col=c("yellow","green","red"),
       lty=1,lwd=2)
V.pred.rel.in <- apply(varMat[-3,],2,function(x){x/max(x)})
plot(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],V.pred.rel.in[1,],ylim=c(0,1),type="n",
     xlab=c("time"),ylab="proportion of variance",main = c("Relative variance: in-sample"))
ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],rep(0,ncol(V.pred.rel.in)),V.pred.rel.in[1,],col="yellow")
ecoforecastR::ciEnvelope(((tail(time1,1)+8):(tail(time1,1)+8+time2))[-1],V.pred.rel.in[1,],V.pred.rel.in[2,],col="green")
legend("topright",legend=c("IC","IC,Par"),col=c("yellow","green"),
       lty=1,lwd=2)