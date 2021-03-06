##` @title run_jags_model     
##` @param ensemble_name     The number of the met ensemble
##` @param n.iter            Number of iterations passed to coda.samples 
##` @param outfile           where the run mcmc output should be saved
##` @param temp              A vector of tempurature values
##` @param precip            a vector of precip values
##` @param modis             a vector of fire area values
##` @description             This function will forecast the model the number of days that we have GEFS data but no Modis data. Will save the output as .Rdata object
library(rjags, lib.loc = "/share/pkg/r/3.5.2/install/lib64/R/library")

run_JAGS_model <- function(ensemble_name = NA, n.iter = 570000, outfile = "/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/output/mcmc/", temp, precip, modis, viirs){

#### Set up Model
Fire_timeseries <-" model {
 ### priors
 beta_precip ~ dnorm(r_0, v_r)
 beta_temp ~ dnorm(k_0, v_k)
 #beta_IC ~ dnorm(I_0, S_0)
 #tau_modis ~ dgamma(mod_1, mod_2)
 sigma ~dgamma(s_1, s_2)
 #sd <- 1/sqrt(sigma)
 x[1] ~ dnorm(mu1, sigmaIC)

 
 ###Observation error
 for ( i in 1:N){
 y_modis[i] ~ dpois(x[i])  #, tau_modis) # modis
 }

 for (j in start_viirs:end_viirs){
  y_viirs[j] ~dpois(x[j])
 }

 ### Add in VIIRS start and stop date 
 
 ### Process model for Fire
 for(t in 2:N) {

 mu[t] <- x[t-1]+ beta_precip*y[t] + beta_temp*y_2[t] #+beta_IC
 x[t] ~ dnorm(mu[t], sigma) # Was 1/sigma 
 

 }
 }
 "
#### Add NA's to modis products
#modis <- c(modis, rep(NA, forecast_days))

#### Fill in data 
data<-list()
data$y <- precip # Precip
data$y_2 <- temp # Temp
data$y_modis <- modis # modis 
data$N<- 92 # N #total number of days. 

### Priors
data$r_0<- -3 ## Probably a negative relationship -- likely more influential than temperature 
data$v_r<- 0.0001 ## Unsure how much uncertainty 
data$k_0<- 3 # Probably positive relationship with temperature
data$v_k<- 0.0001 # no large SD
data$mu1<- 17125000 # MODIS mean burned area from Jan&Feb
#data$v_0<- 10 
#data$mod_1 <- 10
#data$mod_2 <- 1
data$sigmaIC <- 1/13314198
data$s_1 <- 200
data$s_2 <- 1
data$start_viirs <- 47
data$end_viirs <- 92



inits_tess<-list() #
inits_tess[[1]] <- list( beta_precip = -1, beta_temp = 30)
inits_tess[[2]] <-  list(beta_precip = 1, beta_temp = 10 )
inits_tess[[3]] <-  list(beta_precip = 1, beta_temp = 1 )

j.model   <- jags.model (file = textConnection(Fire_timeseries),
                         data = data,
                         inits = inits_tess,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x", "beta_precip", "beta_temp", "sigma"),
                            n.iter = n.iter)
GBR <- gelman.plot(jags.out)
burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
if(is.na(burnin)){
  warning("GBR !< 1.05. Model may have failed to converge")
  jags.burn <- jags.out
  
  did_it_converge <- "convergence_failed_GBR_test" ## Some ensemble members might not converge. Flagging saved file, maybe we will want to re-run? 
}else{
  did_it_converge <- "convergence_passed_GBR_test"
  jags.burn <- window(jags.out,start=burnin, extend = FALSE)
}



#### Save output
  date_stamp <- Sys.time()
  date_stamp <- format(date_stamp, "%Y%m%d")
  file_name <- paste("/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/output/mcmc/", date_stamp, ".", ensemble_name,".", did_it_converge, ".","JAGS_run.Rdata", sep = "")
  save(jags.burn, file = file_name )
}


