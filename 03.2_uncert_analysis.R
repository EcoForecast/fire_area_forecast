library(ecoforecastR)
##` @param IC    Initial Conditions
##` @param beta_precip     Intrinsic growth rate
##` @param Kg    Across-site ('global') mean carrying capacity
##` @param alpha Site random effect
##` @param beta  Slope of precipitation effect on K
##` @param ppt   Precipitation forecast
##` @param Q     Process error (default = 0 for deterministic runs)
##` @param n     Size of Monte Carlo ensemble
#source('/usr3/graduate/shijuan/Desktop/my_own_fork/fire_area_forecast/01.2.1_data__helper_functions.R')
### read in MODIS data
setwd("/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/output")
modis1 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2017/MOD14A2.csv")
modis2 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2018/MOD14A2.csv")
modis3 <- read.csv("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/MOD14A2/2019/MOD14A2.csv")
modis <- rbind(modis1, modis2, modis3)
#modis_days <- get_days(data_type = "MOD14A2")
dates <- format(as.POSIXct(modis$X1), "%Y%m%d")
modis_f <- modis$X2
#dates <- modis$X1
load('/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/output/mcmc/20190506.historical_calibration_with_modis_and_viirs_better_priors_and_sigma.convergence_passed_GBR_test.JAGS_run.Rdata')
setwd("/projectnb/dietzelab/tmccabe/mccabete/Fire_forecast_509/output")
GEFS <- read.csv("/usr3/graduate/shijuan/Desktop/my_own_fork/fire_area_forecast/summary_data_8days.csv")
precip <- read.csv("/usr3/graduate/shijuan/Desktop/my_own_fork/fire_area_forecast/summary_data_8days.csv")[,4]
temp <- read.csv("/usr3/graduate/shijuan/Desktop/my_own_fork/fire_area_forecast/summary_data_8days.csv")[,3]



### settings
Nmc = 1000         ## set number of Monte Carlo draws
ylim = range(modis_f)  ## set Y range on plot
#N.cols <- c("black","red","green","blue","orange") ## set colors
trans <- 0.8       ## set transparancy
NT = 90
time = 1:(NT*2)    ## total time
time1 = 1:NT       ## calibration period
time2 = time1+NT   ## forecast period



### function to plot
# plot.run <- function(){
#   sel = seq(1,ncol(ci))
# #  plot(1:4,1:4,type='n',xlim=c(1,NT*2),ylim=c(min(modis_f),max(modis_f)),ylab="Bure Area (m2)")
#   plot(1:4,1:4,type='n',xlim=c(1,NT*2),ylab="Bure Area (m2)")
#   ecoforecastR::ciEnvelope(time1,ci[1,sel],ci[3,sel],col=col.alpha("lightBlue",0.6))
#   lines(time1,ci[2,sel],col="blue")
#   points(time1,modis_f)
# }
## separate jags.burn into params and predict
params <- list()
predict <- list()
for(i in 1:3){
  mcmc <- jags.burn[[i]]
  params.i <- mcmc[,c(1,2)]
  predict.i <- mcmc[,3:(2+NT)]
  params[[i]] <- params.i
  predict[[i]] <- predict.i
}
out.jags.burn <- list(params=params,predict=predict)

ci <- apply(as.matrix(out.jags.burn$predict[[1]]),2,quantile,c(0.025,0.5,0.975))
sel = seq(1,ncol(ci))
#  plot(1:4,1:4,type='n',xlim=c(1,NT*2),ylim=c(min(modis_f),max(modis_f)),ylab="Bure Area (m2)")
min_plt = min(min(ci),min(modis_f))
max_plt = max(max(ci),max(modis_f))
plot(1:NT,1:NT,type='n',xlim=c(1,NT*2),ylim=c(min_plt,max_plt), ylab="Bure Area (m2)")
ecoforecastR::ciEnvelope(time1,ci[1,sel],ci[3,sel],col=col.alpha("lightBlue",0.6))
lines(time1,ci[2,sel],col="blue")
points(time1,modis_f[time1])

min_plt = min(min(ci),min(ci))
max_plt = max(max(ci),max(ci))
plot(1:NT,1:NT,type='n',xlim=c(1,NT*2),ylim=c(min_plt,max_plt), ylab="Bure Area (m2)")
ecoforecastR::ciEnvelope(time1,ci[1,sel],ci[3,sel],col=col.alpha("lightBlue",0.6))
lines(time1,ci[2,sel],col="grey")
points(time1,modis_f[time1])



### function to forecast
forecastN <- function(IC,
                      beta_precip,
                      beta_temp,
                      y,
                      y_2,
                      Q=0,
                      n=Nmc){
  N <- matrix(NA,n,NT)  ## storage
  Nprev <- IC           ## initialize
  for(t in 2:N) {
    mu[t] <- N[t-1]+ beta_precip*y[t] + beta_temp*y_2[t] #+beta_IC
    N[t] ~ dnorm(mu[t], 1/sigma)
    Nprev <- N[,t] 
  }
  return(N)
}




### calculate mean of all inputs
## Read in temp and precip ensembles first, then calculate mean for precip and max for temp
#met_days <- get_days(data_type = "GEFS")
dates <- format(as.POSIXct(GEFS$X1), "%Y%m%d")
temp_list <- matrix(ncol = 21, nrow = length(met_days))
precip_list <- matrix(ncol = 21, nrow = length(met_days))
temp.max <- matrix(apply(temp_list,2,max),1,NT) ## driver1
precip.mean <- matrix(apply(precip_list,2,mean),1,NT) ## driver2
## parameters
params <- as.matrix(out.jags.burn$params)
params.mean <- matrix(NA,ncol=2,nrow=length(params))
for (i in 1:length(params)) {
  this.params.mean <- apply(params[[i]], 2, mean)
  params.mean[i,] <- this.params.mean
}
params.mean <- colMeans(params.mean)
## initial conditions
IC <- as.matrix(out.jags.burn$predict)



N.det <- forecastN(IC=mean(IC),
                   beta_precip =params.mean[1],
                   beta_temp = params.mean[2],
                   y = precip.mean,
                   y_2 = temp.max,
                   Q=0,  ## process error off
                   n=1)

## Plot run
plot.run()
lines(time2,N.det,col="purple",lwd=3)



### Uncertainty Analysis
prow = sample.int(nrow(params),Nmc,replace=TRUE)

## IC Uncertainty
N.I <- forecastN(IC=IC[prow,"X[4]"],  ## sample IC
                 beta_precip =params[prow,"beta_precip"],
                 beta_temp = params[prow,"beta_temp"],
                 Q=0,
                 n=Nmc)

## Parameter Uncertainty
N.IP <- forecastN(IC=mean(IC),  ## sample IC
                  beta_precip =params[prow,"beta_precip"],
                  beta_temp = params[prow,"beta_temp"],
                  Q=0,
                  n=Nmc)



plot.run()
N.IP.ci = apply(N.IP,2,quantile,c(0.025,0.5,0.975))
ecoforecastR::ciEnvelope(time2,N.IP.ci[1,],N.IP.ci[3,],col=col.alpha(N.cols[2],trans))
ecoforecastR::ciEnvelope(time2,N.I.ci[1,],N.I.ci[3,],col=col.alpha(N.cols[1],trans))
lines(time2,N.I.ci[2,],lwd=0.5)

