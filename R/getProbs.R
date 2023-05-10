
#' Get treatment group probabilites.
#' 
#' \code{getProbs} obtains the outcome probabilities in a treatment group.
#'
#' @param oddsRatio A vector that defines the proportional odds ratio between the reference and intervention.
#'
#' @return
#' @export
#' @import 
#' 
#' @examples
#' simulate(oddsRatio = 1.5, nSamples = 100)

library(truncnorm)

getProbs <- function(p0 = c(10, 40, 10,40)/100,
                     logOddsRatio, sdLogOR)
{
  cumProbs0   <- cumsum(p0)
  cumOdds0    <- cumProbs0/(1 - cumProbs0) 
  cumlogOdds0 <- log(cumOdds0)
  cumlogOdds1 <- rep(NA,length(p0))
  cumlogOdds1[length(p0)] <- Inf
  for(i in (length(p0) - 1):1){ ## For 1 to 2
    cumlogOdds1[i] <- cumlogOdds0[i] + rtruncnorm(n=1,b=cumlogOdds1[i+1],mean=logOddsRatio,sd=sdLogOR)
  }
  cumOdds1    <- exp(cumlogOdds1)
  cumProbs1   <- cumOdds1/(1 + cumOdds1)
  cumProbs1[length(cumProbs1)] <- 1
  p1          <- diff(c(0,cumProbs1))
  p1

  # Probabilties add to 1?
  #if(sum(p1) != 1) 
  #  stop("Error: probabilities don't sum to 1.")
  ## Any negative probabilities?
  for(i in 1:length(p1)){
    if(p1[i] < 0)
      stop("Probabilities are negative")

  }
  
  p1
}

