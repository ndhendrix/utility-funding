############################################################
### This code borrows heavily from the code accompanying ###
### John Kruschke's book "Doing Bayesian Data Analysis." ###
############################################################

# set working directory
setwd("D:/ndhen/OneDrive/School/HSERV project/Analysis")

# clear graphics and R's memory
#graphics.off()
#rm(list=ls())

# provides R/JAGS interface
# note that you'll need to have JAGS installed separately to run this code
library(rjags)
library(runjags)

###### MODEL

# JAGS model saved as R string
model_string = " 
model {
# produces estimate of reported study utility 
for ( u in 1:n_utils ) {
  y[u] ~ dbeta( omega_s[state[u], funder[u]] * ( kappa_s[state[u]] - 2) + 1 ,
    (1 - omega_s[state[u], funder[u]] ) * ( kappa_s[state[u]] - 2) + 1 )
}

# health state distribution of utilities
for( s in 1:n_states ) {
  for( f in 1:2) {
    omega_s[s,f] ~ dbeta(( omega_c[country[s]] + tau_omega[s]*(f-1) ) * 
        ( kappa_c[country[s]] - 2 + tau_kappa[s]*(f-1)) + 1 ,
      ( 1 - (omega_c[country[s]] + tau_omega[s]*(f-1))) * 
        ( kappa_c[country[s]] - 2 + tau_kappa[s]*(f-1)) + 1  )
  }
  # uses the same taus for *all* states
  tau_omega[s] ~ dnorm( mu_tau_omega , sigma_tau_omega)
  tau_kappa[s] ~ dnorm( mu_tau_kappa , sigma_tau_kappa)
  kappa_s[s] <- kappa_minus_two_s[s] + 2
  kappa_minus_two_s[s] ~ dgamma( 0.01 , 0.01 )
}
mu_tau_omega ~ dnorm(0,5)
sigma_tau_omega ~ dgamma(0.01 , 0.01)
mu_tau_kappa ~ dnorm(0,5)
sigma_tau_kappa ~ dgamma(0.01 , 0.01)

# country-level distribution of utility values
for( c in 1:n_country ) {
  omega_c[c] ~ dbeta( omega0 * ( kappa0 - 2 ) + 1 ,
    ( 1 - omega0 ) * ( kappa0 - 2 ) + 1 )
  kappa_c[c] <- kappa_minus_two_c[c] + 2
  kappa_minus_two_c[c] ~ dgamma( 0.01 , 0.01 ) 
}
#kappa_c_s ~ dgamma( 0.01 , 0.01 )
#kappa_c_r ~ dgamma( 0.01 , 0.01 )

# hyperparameters : vague, non-commital priors
omega0 ~ dbeta( 1.0 , 1.0 )
kappa0 <- kappa_minus_two0 + 2
kappa_minus_two0 ~ dgamma( 0.01 , 0.01 )
}
" # closing quote of model string

# write model to temporary file
writeLines(model_string, con = "TEMPmodel.txt")

###### DATA

utils <- read.csv("analysis_processed.csv")

# convert factors to numbers
utils$country_num <- as.numeric(utils$setting)
utils$state_num <- as.numeric(utils$health_state)
utils$industry <- ifelse(grepl("Pharm Or Device",utils$funding), 2, 1)

# extract variables that are going into the model
y <- utils$utility
n_utils <- length(y)
country <- utils$country_num
n_country <- length(unique(utils$country_num))
state <- utils$state_num
n_states <- length(unique(utils$state_num))
funder <- utils$industry

# put in list format for JAGS
dataList <- list(
  y = y,
  n_utils = n_utils,
  country = country,
  n_country = n_country,
  state = state,
  n_states = n_states,
  funder = funder
)

###### RUN CHAINS

n_chains <- 7 # set this value to equal k-1 where k = # of cores in your pc 
# (3 laptop, 7 desktop)
n_adapt_steps <- 1000
n_burnin_steps <- 1000
n_use_steps <- 10000
n_thin_steps <- 2

# runjags version
run_jags_out <- run.jags( method = "parallel",
                          model = "TEMPmodel.txt",
                          monitor = c("tau_omega",
                                      "tau_kappa",
                                      "omega_s",
                                      "kappa_s",
                                      "omega_c"),
                          data = dataList,
                          #inits = ...,
                          n.chains = n_chains,
                          adapt = n_adapt_steps,
                          burnin = n_burnin_steps,
                          sample = ceiling(n_use_steps/n_chains),
                          thin = n_thin_steps,
                          summarise = FALSE,
                          plots = FALSE )
coda_samples <- as.mcmc.list( run_jags_out )

# save to a more easily manipulated format
#mcmc_chain2 <- as.data.frame(as.matrix(coda_samples))

# find probability of overlap in means for distribution w/ and w/o private funding

#omega_width <- dim(omegas)[2]/2
#omegas_diff <- data.frame(temp = rep(0, dim(omegas)[1]))
#for(i in 1:omega_width){
#  temp <- omegas[,(i+33)] - omegas[,i]
#  temp_df <- as.data.frame(temp)
#  colnames(temp_df) <- paste("omega", i, sep="")
#  omegas_diff <- cbind(omegas_diff, temp_df)
#}
#omegas_diff$temp <- NULL

# prep parameters for graphing
tau_intercepts <- utils %>%
  select(health_state, state_num) %>%
  distinct(health_state, state_num) %>%
  rename(Label = health_state)
tau_intercepts$Parameter <- lapply(tau_intercepts$state_num,
                                   function(x) paste("tau_omega[", x, "]", sep=""))
tau_intercepts$state_num <- NULL
tau_intercepts$Parameter <- as.character(tau_intercepts$Parameter)
tau_intercepts <- arrange(tau_intercepts, Parameter, Label)

# create caterpillar plot
library(ggmcmc)
mcmc <- ggs(coda_samples,
            par_labels = tau_intercepts,
            family = "^tau_omega")
ggs_caterpillar(mcmc)