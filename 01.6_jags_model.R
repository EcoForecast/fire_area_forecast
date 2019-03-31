library(rjags)
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
