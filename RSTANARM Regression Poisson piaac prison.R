# Generalized Linear Model: Poisson Regression
#
# Data
# Read data file
dfPiaacPrison <- read.csv(file = "C:/Users/Owner/Documents/PIAAC/piaacprison9var.csv",
                          header = TRUE, sep = ",")
names(dfPiaacPrison)[1] <- "useNumSkills"

#
# dfPrison has no missing data.
dfPrison <- data.frame(numeracy = dfPiaacPrison$useNumSkills,
              gender = dfPiaacPrison$gender,
              training = dfPiaacPrison$training,
              health = dfPiaacPrison$health,
              age = dfPiaacPrison$age,
              education = dfPiaacPrison$education,
              trust = dfPiaacPrison$trust)
#
library(rstanarm)
rstanarmPois <- stan_glm(numeracy ~ gender + education +
                         training + health + trust + offset(log(age)), 
                         data = dfPrison, family = poisson(link = "log"), 
                         iter = 10000, warmup = 2000, cores = 4)
summary(rstanarmPois, probs = c(.025, .975), digits = 3)
rstanarmCoeff <- rstanarmPois$coefficients
rstanarmRR <- exp(rstanarmCoeff)
rstanarmRR
print(rstanarmPois)
prior_summary(rstanarmPois)
#
rstanarmNB <- stan_glm(numeracy ~ gender + education +
                       training + health + trust + offset(log(age)), 
                       data = dfPrison, family = neg_binomial_2(link = "log"), 
                       iter = 10000, warmup = 2000, cores = 4)

rstanarmNB <- stan_glm(numeracy ~ education + training +
                         health + trust, offset = log(age), 
                       data = dfPrison, family = neg_binomial_2(link = "log"), 
                       iter = 10000, warmup = 2000, cores = 4, QR = TRUE)
summary(rstanarmNB, probs = c(.025, .975), digits = 3)
rstanarmCoeff <- rstanarmNB$coefficients
rstanarmRR <- exp(rstanarmCoeff)
rstanarmRR
print(rstanarmPois)
prior_summary(rstanarmPois)

library(ggplot2)
library(StanHeaders)
library(rstan)
options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)
stan_hist(rstanarmNB)
stan_dens(rstanarmNB)
stan_plot(rstanarmNB)

stan_diag(rstanarmNB)

library(bayesplot)
library(ggplot2)
rhatMod <- rhat(rstanarmNB)
mcmc_rhat(rhatMod) + yaxis_text()
neffMod <- neff_ratio(rstanarmNB)
mcmc_neff(neffMod)

stanArray <- as.array(rstanarmNB)
mcmc_intervals(stanArray)
mcmc_areas(stanArray, prob = 0.95)
mcmc_hist(stanArray)
mcmc_dens(stanArray)
mcmc_hist_by_chain(stanArray)
mcmc_dens_overlay(stanArray)
mcmc_violin(stanArray)
mcmc_acf(stanArray)
mcmc_trace(stanArray)

library(ggmcmc)
ggSTAN <- ggs(rstanarmNB)
ggs_histogram(ggSTAN)
ggs_density(ggSTAN)
ggs_traceplot(ggSTAN)
ggs_running(ggSTAN)
ggs_compare_partial(ggSTAN)
ggs_autocorrelation(ggSTAN)
ggs_Rhat(ggSTAN) + xlab("R_hat")
ggs_geweke(ggSTAN)
ci(ggSTAN)

library(shinystan)
shinyMod <- launch_shinystan(rstanarmNB)
