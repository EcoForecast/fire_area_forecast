library(rjags)
<<<<<<< HEAD
Fire <-" model {

 ### priors
 beta_precip ~ dnorm(r_0, v_r)
 beta_temp ~ dlnorm(k_0, v_k)
 beta_evi ~ dnorm(e_0, es_0)
 beta_IC ~ dnorm(I_0, S_0)
 tau_precip ~ dgamma(t_1, t_2)
 tau_evi ~ dgamma(evi_1, evi_2)
 tau_temp ~ dgamma(temp_1, temp_2)
 tau_modis ~ dgamma(mod_1, mod_2)
 
 sigma ~dgamma(s_1, s_2)
 x[1] ~ dnorm(mu1, v_0)
 
 
 ### Data model / Observation error
 for ( i in 2:N){
 y[i]~ dnorm(x[i],tau_precip) # Precipitation
 y_2[i]~ dnorm(x[i],tau_temp) # Temp
 y_3[i]~ dnorm(x[i],tau_evi) # Evi
 y_modis ~ dnorm(x[i], tau_modis) # modis
 }
 
 ### Process model for Fire
 for(t in 2:N) {
 mu[t] <- beta_IC*x[t-1] + -1*beta_precip*y[t-1] + beta_temp*y_2[t-1] + beta_evi*y_3[t-1] 
 x[t] ~ dnorm(mu[t],sigma)
 }
 }
 "
 
 ### data
 data<-list()
 data$y<-log(moose$density)
 data$N<-length(moose$year)
 
 ### Priors
 data$r_0<-2
 data$v_r<-1
 data$k_0<-3
 data$v_k<-0.5
 data$t_1<-2
 data$t_2<-2
 data$s_1<-2
 data$s_2<-2
 data$mu1<-4
 data$v_0<-1
 
 
 inits_tess<-list() #
 inits_tess[[1]] <- list(r = meanR, k = max(na.omit(moose$density)), tau = 4, sigma = sd(na.omit(moose$density)))
 inits_tess[[2]] <-  list( r = mean(diff[diff >0]), k = max(na.omit(moose$density))+meanR, tau = 4, sigma = sd(na.omit(moose$density)))
 inits_tess[[3]] <-  list( r = mean(diff[diff < 0]), k = max(na.omit(moose$density))-meanR, tau = 4, sigma = sd(na.omit(moose$density)))
 
 j.model   <- jags.model (file = textConnection(Fire),
                                 data = data,
                                 inits = inits_tess,
                                 n.chains = 3)
 
 jags.out   <- coda.samples (model = j.model,
                                    variable.names = c("r", "k", "tau", "sigma","y[44]", "y[18:21]", "x"),
                                    n.iter = 65000)
=======
devtools::install_github("EcoForecast/ecoforecastR")
Fire <-" model {

  ### priors
  beta_precip ~ dnorm(bp_1, bp_2)
  beta_temp ~ dlnorm(bt_1, bt_2)
  beta_evi ~ dnorm(be_1, be_2)
  beta_IC ~ dnorm(bic_1, bic_2)
  tau_precip ~ dgamma(tp_1, tp_2)
  tau_evi ~ dgamma(evi_1, evi_2)
  tau_temp ~ dgamma(temp_1, temp_2)
  tau_modis ~ dgamma(mod_1, mod_2)
  
  sigma ~dgamma(s_1, s_2)
  x[1] ~ dnorm(mu1, v_0)
  
  
  ### Data model / Observation error
  for ( i in 1:N){
  y[i]~ dnorm(x[i],tau_precip) # Precipitation
  y_2[i]~ dnorm(x[i],tau_temp) # Temp
  y_3[i]~ dnorm(x[i],tau_evi) # Evi
  y_modis[i] ~ dnorm(x[i], tau_modis) # modis
  }
  
  ### Process model for Fire
  for(t in 2:N) {
  mu[t] <- beta_IC*x[t-1] + -1*beta_precip*y[t-1] + beta_temp*y_2[t-1] + beta_evi*y_3[t-1] 
  x[t] ~ dnorm(mu[t],sigma)
  }
  }
  "
  
  ### data
  data<-list()

  ### Priors
  data$bp_1<-0  # precipitation millimeter mean
  data$bp_2<-1    # precipitation st
  data$bt_1<-0    # temperature mean in Kelvin
  data$bt_2<-1    # SD
  data$be_1 <-0 
  data$be_2 <-1   
  data$bic_1 <- 0
  data$bic_2 <- 1
  data$tp_1<-0     # precipitation error prior
  data$tp_2<-1     # precipitation error prior
  data$evi_1<-(-1)  
  data$evi_2<-(1)
  data$temp_1 <- 0
  data$temp_2 <- 1
  data$mod_1 <- 0
  data$mod_2 <- 1
  data$s_1 <- 0 
  data$s_2 <- 1
  data$mu1 <- 0
  data$v_0 <- 1
  data$N <- 100
  y=list(1:100)
  y_2=list(1:100)
  y_3=list(1:100)
  mu=list(1:100)
  nchain = 3
  inits_fire <- list()
  
  for(i in 1:nchain){
    y.samp = sample(y,length(y),replace=TRUE)
    inits_fire[[i]] <- list(beta_precip=1, beta_temp=1, beta_evi=1, beta_IC=1, 
                            tau_precip=0.5, tau_evi=0.5, tau_temp=0.5, tau_modis=0.5,mu=0.5, sigma=0.5)
  }
  
  j.model   <- jags.model (file = textConnection(Fire),
                           data = data,
                           inits = inits_fire,
                           n.chains = 3)
  
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("beta_precip", 'beta_temp', 'beta_evi', 'beta_IC', 'tau_precip', 'tau_evi', 'tau_temp', 'tau_modis','mu', 'sigma'),
                              n.iter = 500)
  
>>>>>>> 813c78ad042be2af42ad6946520d18e0c55d111e
