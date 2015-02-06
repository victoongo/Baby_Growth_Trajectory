# test reliability of parent reported weight
library(MCMCpack)
library(ggplot2)
library(plyr)
library(nlme)
library(doBy)

setwd("~/Projects/R_lib/bmi/data")
bmi<-read.csv("bmi.csv", as.is=TRUE)


# example from manual
line   <- list(X = c(-2,-1,0,1,2), Y = c(1,3,3,3,5))
posterior  <- MCMCregress(Y~X, b0=0, B0 = 0.1, sigma.mu = 5, sigma.var = 25, data=line, verbose=1000)
plot(posterior)
raftery.diag(posterior)
summary(posterior)
