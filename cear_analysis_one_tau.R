############################################################
### This code borrows heavily from the code accompanying ###
### John Kruschke's book "Doing Bayesian Data Analysis." ###
############################################################

# set working directory
setwd("C:/Users/ndhen/Dropbox (UW)/School/HSERV project/Revision/utility-funding")

# clear graphics and R's memory
#graphics.off()
#rm(list=ls())

# provides R/JAGS interface
# note that you'll need to have JAGS installed separately to run this code
library(rjags)
library(runjags)
library(dplyr)
library(ggmcmc)

###### MODEL

# JAGS models saved as R string

# This version of the code uses a single tau_omega (i.e., mode of additive effect
# of industry funding) for all health states

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
omega_s[s,f] ~ dbeta(( omega_c[country[s]] + tau_omega*(f-1) ) * 
( kappa_c[country[s]] - 2 + tau_kappa*(f-1)) + 1 ,
( 1 - (omega_c[country[s]] + tau_omega*(f-1))) * 
( kappa_c[country[s]] - 2 + tau_kappa*(f-1)) + 1  )
}
kappa_s[s] <- kappa_minus_two_s[s] + 2
kappa_minus_two_s[s] ~ dgamma( 0.01 , 0.01 )
}
# uses the same taus for *all* states
tau_omega ~ dnorm( mu_tau_omega , sigma_tau_omega)
tau_kappa ~ dnorm( mu_tau_kappa , sigma_tau_kappa)
mu_tau_omega ~ dnorm(0,1)
sigma_tau_omega ~ dgamma(0.01 , 0.01)
mu_tau_kappa ~ dnorm(0,1)
sigma_tau_kappa ~ dgamma(0.01 , 0.01)


# country-level distribution of utility values
for( c in 1:n_country ) {
omega_c[c] ~ dbeta( omega0 * ( kappa0 - 2 ) + 1 ,
( 1 - omega0 ) * ( kappa0 - 2 ) + 1 )
kappa_c[c] <- kappa_minus_two_c[c] + 2
kappa_minus_two_c[c] ~ dgamma( 0.01 , 0.01 ) 
}

# hyperparameters : vague, non-commital priors
omega0 ~ dbeta( 1.0 , 1.0 )
kappa0 <- kappa_minus_two0 + 2
kappa_minus_two0 ~ dgamma( 0.01 , 0.01 )
}
" # closing quote of model string

# write model to temporary file
writeLines(model_string, con = "TEMPmodel.txt")

###### DATA

utils <- read.csv("analysis2.csv")

# convert factors to numbers
utils$country_num <- as.numeric(utils$setting)
utils$state_num <- as.numeric(utils$health_state)
utils$industry <- ifelse(utils$funding=="Pharm Or Device", 2, 1) # exclusive funding
#utils$industry <- ifelse(utils$funding=="Pharm Or Device", 2, 1) #non-exclusive funding

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
mcmc_chain2 <- as.data.frame(as.matrix(coda_samples))

print("p(tau_omega > 0):")
length(which(mcmc_chain2$tau_omega > 0)) / length(mcmc_chain2$tau_omega)
print("p(tau_omega < 0):")
length(which(mcmc_chain2$tau_omega < 0)) / length(mcmc_chain2$tau_omega)

# graph
library(ggplot2)
ggplot(mcmc_chain2, aes(x=tau_omega)) + 
  geom_freqpoly() +
  theme_bw(base_size = 16) +
  xlab("Value of additive effect") +
  ylab("Count") +
  geom_vline(linetype="dashed", xintercept = 0)
