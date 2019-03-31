library(rjags)
Fire_timeseries <-" model {

 ### priors
 beta_precip ~ dnorm(r_0, v_r)
 beta_temp ~ dnorm(k_0, v_k)
 beta_evi ~ dnorm(e_0, es_0)
 #beta_IC ~ dnorm(I_0, S_0)
 tau_modis ~ dgamma(mod_1, mod_2)
 tau_evi ~ dgamma(evi_1, evi_2)
 sigma ~dgamma(s_1, s_2)
 x[1] ~ dnorm(mu1, v_0)
 
 
 ###Observation error
 for ( i in 2:N){
 y_modis[i] ~ dnorm(x[i], tau_modis) # modis
 y_evi[i] ~ dnorm(x[i], tau_evi) # modis
 }
 
 ### Process model for Fire
 for(t in 2:N) {
 mu[t] <- x[t-1]+ beta_precip*y[t] + beta_temp*y_2[t] + beta_evi*y_3[t-1] #+beta_IC
 x[t] ~ dgamma(mu[t]^-1,sigma) ## mu is raised to the negative one to use a inverse link function (ie to take our linear funtion and make it gamma)
 }
 }
 "
 
 ### data
 data<-list()
 data$y <- rnorm(10, 10) # Precip
 data$y_2 <- rnorm(10, 200) # Temp
 data$y_3 <- rnorm(10, 10) # Evi
 data$y_modis <- rnorm(10, 50) # modis 
 data$N<- 10
 
 ### Priors
 data$r_0<- -1 ## Probably a negative relationship
 data$v_r<- 1/10 ## Unsure how much uncertainty 
 data$k_0<- 3 # Probably positive relationship with temperature
 data$v_k<- 0.5 # no large SD
 data$e_0 <- 0 ## don't know if postive or negative relationship with EVI
 data$es_0 <- 1/100 # large uncertianty
 data$s_1<-2 
 data$s_2<-2
 data$mu1<-11517700  # I just mulitplied the area of park by 0.01. could change to actual area burned in forrest in previous years
 data$v_0<- 1/3000000  # Make sure varience is roughly max the area of the park (1151770000 m^2), and min 0
 data$mod_1 <- 1
 data$mod_2 <- 1
 data$evi_1 <-1
 data$evi_2 <-1

 inits_tess<-list() #
 inits_tess[[1]] <- list( sigma = 1)
 inits_tess[[2]] <-  list( sigma = 2)
 inits_tess[[3]] <-  list( sigma =4)
 
 j.model   <- jags.model (file = textConnection(Fire_timeseries),
                                 data = data,
                                 inits = inits_tess,
                                 n.chains = 3)
 
 jags.out   <- coda.samples (model = j.model,
                                    variable.names = c("x"),
                                    n.iter = 5000)
 
 
 plot(jags.out)
 