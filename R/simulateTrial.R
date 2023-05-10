
#' Simulate a trial with an ordinal outcome.
#' 
#' \code{simulateTrial} simulates a trial.
#'
#' @param oddsRatio A scalar that defines the proportional odds ratio between the reference and intervention.
#' @param nSamples Size of each trial arm.
#'
#' @return
#' @export
#' @import
#' 
#' @examples
#' simulate(oddsRatio = 1.5, nSamples = 100)

simulateTrial <- function(logOddsRatio, 
                          nSamples, 
                          sdLogOR,
                          p0     = c(20, 40, 30,10)/100,
                          states = c("A", "B", "C","D"))
{
  # TODO: Make multi-arm.
  # Error catching
 # if(sum(p0) != 1) 
   # stop("Error: probabilities don't sum to 1.")
 ## if(length(p0) != length(states)) 
  ##  stop("Error: state and probability vector different lengths.")
  # if(length(oddsRatio) != 1 | length(oddsRatio) != length(p0)-1)
  #   stop("Error: number of odds ratios should be either 1 for proportional odds or 1 less than the number of categories for non-proportional odds")
  ## Define outcome probabilities for the treatment group
  p1   <- getProbs(p0, logOddsRatio, sdLogOR)
p1
 ## if(length(p1) != length(states)) 
 ##   stop("Error: state and probability vector different lengths.")
  # if(length(oddsRatio) != 1 | length(oddsRatio) != length(p0)-1)

  # Simulate two arm trial data.
  dMulti0  <- rmultinom(1, size = nSamples, prob = p0)
  dMulti1  <- rmultinom(1, size = nSamples, prob = p1)
  sample0  <- rep(states, dMulti0) 
  sample1  <- rep(states, dMulti1) 
  sample0  <- factor(sample0, levels = states, ordered = T)
  sample1  <- factor(sample1, levels = states, ordered = T)
  
  # Munge simulated data.
  data           <- rbind(data.frame("a" = 0, "y" = sample0),
                          data.frame("a" = 1, "y" = sample1))
  data
}