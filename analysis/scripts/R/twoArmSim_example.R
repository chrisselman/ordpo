# Simulate an two-arm trial with an ordinal endpoint. 
library("rstanarm")
library("rstan")
library('posterior')
library("rms")
options(mc.cores = parallel::detectCores()) # use max # CPUs
library(brms)
library(rmsb)
library(truncnorm)

# ==========================================================================================
# Define states

states <- c("A", "B", "C")
iStates <- seq(states)

# ==========================================================================================
# Simulation inputs. 

set.seed(2001)
OR      <- c(1/0.5, 1/0.75, 1/1, 1/1.25, 1/1.5) # Assume these PORs
logOR   <- log(OR)                     # Take their log.
sdlogOR <- c(0,    # No departure from PO
             0.05, # Slight departure from PO
             0.2) # Major departure from PO
##0.5)  # Extreme departure from PO       

# Munge simulation scenarios
scen <- expand.grid("logOR" = logOR, "sdlogOR" = sdlogOR)

# Have a look - check that simulating cumulative log-odds ratios from 
# truncated normal looks okay 
logOddsratio1 <- matrix(NA,nrow=2000,ncol=nrow(scen))
for (x in 1:nrow(scen)){
  for (j in seq(1,2000,2)){
    p0 <- c(20, 40, 30)/100
    cumProbs0   <- cumsum(p0)
    cumOdds0    <- cumProbs0/(1 - cumProbs0) 
    cumlogOdds0 <- log(cumOdds0)
    cumlogOdds1 <- rep(NA,length(p0))
    cumlogOdds1[length(p0)] <- Inf
    logOddsratio1[j,x] <- rtruncnorm(n=1,b=cumlogOdds1[3],mean=scen$logOR[x],sd=scen$sdlogOR[x])
    cumlogOdds1[2] <- cumlogOdds0[2] + logOddsratio1[j]
    logOddsratio1[j+1,x] <- rtruncnorm(n=1,b=cumlogOdds1[2],mean=scen$logOR[x],sd=scen$sdlogOR[x])
    cumlogOdds1[1] <- cumlogOdds0[1] + logOddsratio1[j+1]
  }
}
simOR <- exp(logOddsratio1)
boxplot(simOR); abline(1,0, col = "red")


# Simulate data
set.seed(12)
simData <- list()

for(i in seq(nrow(scen)))
{ 
  dat <- list()
  for(j in seq(1))
  {
    print(c(i,j))
    dat[[j]] <- simulateTrial(nSamples = 500, 
                              p0     = c(20, 40, 40)/100,
                              states = c("A", "B", "C"),
                              logOddsRatio = with(scen,logOR[i]),
                              sdLogOR = with(scen,sdlogOR[i]))
  }
  simData[[i]] <- dat
}


# Function to make stan inputs.
makeStanData <- function(dat)
{
  
  z <- data.frame("y" = as.numeric(dat[["y"]]), 
                  "X" = as.numeric(dat[["a"]]))
  z <- z[["X"]] == 1 & z[["y"]] > 2
  stanData <- list(N = nrow(dat),
                   y = as.numeric(dat[["y"]]),
                   X = as.matrix(dat[["a"]]),
                   Z = as.matrix(as.numeric(z)),
                   p = 1,
                   q = 1,
                   k = length(states),
                   p_par = rep(1, length(states)),
                   sds = 2.5,
                   sdsppo = 2.5,
                   conc = 1)
}


# Function to fit stan model.
fitStan <- function(stanModel, stanData)
{
  sampling(stanModel, 
           stanData, 
           chains = 4, 
           iter = 500, 
           refresh = 10,
           cores = 4)
}

# Function to extract posteriors.
extractPosterior <- function(stanFit)
{
  results <- extract(stanFit)
  results <- do.call(cbind, results)
  colnames(results) <- rownames(summary(stanFit)[["summary"]])
  rvar(results)
}



# ==========================================================================================
# Single simulation.

# Make a single dataset(example scenario 5)
singleDat <- makeStanData(simData[[5]][[1]])

## Using Frank Harrell's blrm command
x<- singleDat$X[,]

# PO model 
pomod <- blrm(singleDat$y~x)
pomod 

## PPO model 
ppomod <- blrm(singleDat$y~x, ppo=~x)
ppomod 

# Constrained PO 
cppomod <- blrm(singleDat$y ~ x, ppo= ~ x, cppo=function(y) y==3)
cppomod


## Alternatively do this via Stan directly 
# PO model 
stanModelPO <- stan_model(file.path("stan", "po_goodrich.stan"))
fitPO <- fitStan(stanModelPO, singleDat)
print(fitPO, pars = c("alpha", "beta", "OR"))

# Fit goodrich unconstrained PPO model.
stanModelGoodrich <- stan_model(file.path("stan", "ppo_goodrich.stan"))
fitPPO <- fitStan(stanModelGoodrich, singleDat)
print(fitPPO, pars = c("alpha", "beta", "tau"))

# Fit constrained PPO model 
stanModelGoodrich <- stan_model(file.path("stan", "cppo_harrell.stan"))
fitcPPO <- fitStan(stanModelGoodrich, singleDat)
print(fitcPPO, pars = c("alpha", "beta", "tau"))
