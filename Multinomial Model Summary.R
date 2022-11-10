### Multinomial Models Summary ###
## Consider ordinal outcome: severity of disease (mild, moderate, severe)
## Exposure group: binary (sex: males/females)
## Assume we have other variables we should control for: age, education 
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
## Simulate from statistical population 
## Define variables 
## Outcome 
set.seed(1234)
y<-c(1,2,3)
y<-factor(y,levels=c(1,2,3),labels=c("Mild","Moderate","Severe"))
y2<-as.factor(sample(y,size=5000,replace=TRUE,prob=c(0.5,0.3,0.2)))
y3<-as.factor(sample(y,size=5000,replace=TRUE,prob=c(0.3,0.4,0.3)))
y2<-rbind(y2,y3)

## Exposure 
x<-c(rep(0,500),rep(1,500))
x2<-factor(x,levels=0:1,labels=c("Male","Female"))

## Age
age<-sample(18:80,size=10000,replace=TRUE)

## Education 
edu <- c(1,2,3)
edu<-factor(edu,levels=1:3, labels=c("Primary","Secondary","Tertiary"))
edu2 <-sample(edu,size=10000,replace=TRUE,prob=c(0.1,0.4,0.5))


## Take a sample from population 
ysamp <- factor(sample(y2,size=500,replace=FALSE))
agesamp<-as.vector(sample(age,size=500,replace=FALSE))
edusamp <-as.factor(sample(edu2,size=500,replace=FALSE))
xsamp<-factor(sample(x2,size=500,replace=FALSE),levels=c("Male","Female"))


## Create data frame
df <- cbind(ysamp,xsamp,agesamp,edusamp)
df<-as.data.frame(df)
View(df)

df$ysamp <- factor(ysamp,levels=c(1,2,3),labels=c("Mild","Moderate","Severe"))
df$xsamp <- factor(xsamp,levels=c("Male","Female"),labels=c("Male","Female"))
df$edusamp <- factor(edusamp,levels=c("Primary","Secondary","Tertiary"),labels=c("Primary","Secondary","Tertiary"))
View(df)

library(VGAM)
## Run a baseline category logit model 
fit.full = vglm(ysamp~ xsamp+edusamp, family=multinomial(refLevel=1), data=df)
summary(fit.full)


ci<-confint.default(fit.full) # CIs assuming normality

## odds ratios
exp(coef(fit.full))

## OR and CI
exp(cbind(OR = coef(fit.full), ci))


## Ordered stereotype model 
library(ordinalgmifs)
fit.stereo <- rrvglm(ordered(ysamp)~xsamp+edusamp,data=df, multinomial)
fit.stereo<-ordinalgmifs(ordered(ysamp)~xsamp+edusamp,data=df,
                      probability.model = "Stereotype", link = "logit")
summary(fit.stereo)

## Adjacent category
install.packages("brglm2")
library("brglm2")
mod.adj <- vglm(ordered(ysamp)~ xsamp+edusamp, family=acat(parallel=TRUE), data=df)
summary(mod.adj)


ci<-confint.default(mod.adj) # CIs assuming normality

## odds ratios
exp(coef(mod.adj))

## OR and CI
exp(cbind(OR = coef(mod.adj), ci))


## Probit model 
fit.probit=polr(ordered(df$Disease_Severity)~ df$Sex+df$Education,method="probit",Hess=TRUE)
summary(fit.probit)

ci<-confint.default(fit.full) # CIs assuming normality

## odds ratios
exp(coef(fit.full))

## OR and CI
exp(cbind(OR = coef(fit.full), ci))


## Continuation ratio model 
fit.cratio <- vglm(ordered(edusamp)~ xsamp,
                   family=cratio(parallel=TRUE,reverse=FALSE), data=df)

summary(fit.cratio)


### PROPORTIONAL ODDS ## 
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

## Rename variables
names(df)[names(df) == "ysamp"] <- "Disease_Severity"
names(df)[names(df) == "xsamp"] <- "Sex"
names(df)[names(df) == "edusamp"] <- "Education"
names(df)[names(df) == "agesamp"] <- "Age"


## fit ordered logit model and store results 'm'
m <- polr(Disease_Severity ~ Sex + Education, data = df, Hess=TRUE)

## view a summary of the model
summary(m)

##P-values?
## store table
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
ctable<-round(ctable,4)
ctable


## Profiling the likelihood 
(ci <- confint(m)) # default method gives profiled CIs

confint.default(m) # CIs assuming normality


## odds ratios
exp(coef(m))

## OR and CI
exp(cbind(OR = coef(m), ci))


## The old parametrisation (use of double negative)
library(tidyr)
library(VGAM)
library(MASS)
fit.add.op = vglm(ordered(ysamp) ~ xsamp+edusamp, 
                  family=cumulative(parallel=TRUE,reverse=TRUE), data=df)
summary(fit.add.op)


# ordinal, not proportional odds
# additive
fit.add.o = vglm(ordered(ysamp) ~ xsamp+edusamp, 
                 family=cumulative(parallel=FALSE~xsamp,reverse=TRUE), data=df)
summary(fit.add.o)

# LRT 
g2.add.o = deviance(fit.add.o)
df.add.o = df.residual(fit.add.o)
g2.prop = 2*(logLik(fit.add.o) - logLik(fit.add.op))
df.prop = df.residual(fit.add.op) - df.residual(fit.add.o)
1 - pchisq(g2.prop, df.prop) ## Reject H0

##Assessing proportional odds 
## Brant test 
require(brant)
brant(m)

##Graphical method
library(Hmisc)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

s <- with(df, summary(as.numeric(Disease_Severity) ~ Sex+Education, fun=sf))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s # print

plot(s, which=1:3, pch=1:3, xlab='Logit', main=' ', xlim=range(s[,3:4]))


## Interaction effects 
install.packages("effects")
library(effects)
## fit ordered logit model and store results 'm' - interaction model
m1 <- polr(Disease_Severity ~ Sex*Education, data = df, Hess=TRUE)
summary(m1)

##P-values?
## store table
(ctable1 <- coef(summary(m1)))

## calculate and store p values
p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable1 <- cbind(ctable1, "p value" = p))
ctable1<-round(ctable1,4)
ctable
plot(Effect(focal.predictors = c("Sex","Education"),m1), rug = FALSE,main="",
     ylab = "Probability of Disease Severity")
library(car)
Anova(m1)

m2 <- polr(Disease_Severity ~ Sex*Age, data = df, Hess=TRUE)
plot(Effect(focal.predictors = c("Sex","Age"),m2), rug = FALSE,
     ylab = "Probability of Disease Severity",main="")


## Partial proportional odds models 

impactPO <- function(formula,
                     relax=if(missing(nonpo)) 'multinomial' else 'both',
                     nonpo, newdata, data=environment(formula), B=0, ...) {
  
  do.mn  <- relax != 'ppo'
  do.ppo <- relax != 'multinomial'
  if(do.ppo && missing(nonpo))
    stop('must specify nonpo when relax is not "multinomial"')
  if(do.mn) if(! requireNamespace('nnet', quietly=TRUE))
    stop("This function requires the 'nnet' package")
  if(do.ppo) if(! requireNamespace('VGAM', quietly=TRUE))
    stop("This function requires the 'VGAM' package")
  
  f <- lrm(formula, data=data, ...)
  a <- predict(f, newdata, type='fitted.ind')
  ytable <- f$freq
  nam <- names(ytable)
  if(nrow(newdata) == 1) a <- matrix(a, nrow=1)
  colnames(a) <- nam
  probsPO     <- a
  A <- cbind(method='PO', newdata, a)
  dev   <- deviance(f)
  dev0  <- dev[1]
  devPO <- dev[2]
  dfPO  <- f$stats['d.f.']
  n     <- f$stats['Obs']
  kint  <- length(nam) - 1
  st <- function(method, fit, probs) {
    df  <- length(coef(fit))
    p   <- df - kint
    dev <- deviance(fit)
    dev <- dev[length(dev)]
    aic <- dev + 2 * df
    LR  <- dev0 - dev
    r2m <- R2Measures(LR, p, n, ytable)
    r2    <- r2m[1]
    r2adj <- r2m[2]
    mpo   <- method == 'PO'
    data.frame(method     = method,
               Deviance   = dev,
               `d.f.`     = df,
               AIC        = aic,
               p          = p,
               `LR chi^2` = LR,
               `LR - p`   = LR - p,
               `LR chi^2 test for PO` = if(mpo) NA else devPO - dev,
               `  d.f.`    = if(mpo) NA else p - dfPO,
               `  Pr(>chi^2)` = if(mpo) NA else 1. - pchisq(devPO - dev, p - dfPO),
               `MCS R2`     = r2,
               `MCS R2 adj` = r2adj,
               `McFadden R2`      = 1. - dev / dev0,
               `McFadden R2 adj`  = 1. - (dev + 2 * p) / dev0,
               `Mean |difference| from PO` = if(mpo) NA else mean(abs(probs - probsPO)),
               check.names=FALSE)
  }
  stats <- st('PO', f, probsPO)
  
  mad <- NULL
  
  if(do.ppo) {
    ppo  <- formula(paste('FALSE ~', as.character(nonpo)[-1]))
    g <- VGAM::vglm(formula, VGAM::cumulative(parallel=ppo, reverse=TRUE),
                    data=data, ...)
    vglmcoef <- coef(g)   # save to jump start bootstrap estimates
    b <- VGAM::predict(g, newdata, type='response')
    if(nrow(newdata) == 1) b <- matrix(b, nrow=1)
    colnames(b) <- nam
    probsPPO <- b
    md  <- apply(abs(b - probsPO), 1, mean)
    mad <- rbind(mad, cbind(method='PPO', newdata, `Mean |difference|`=md))
    A <- rbind(A, cbind(method='PPO', newdata, b))
    stats <- rbind(stats, st('PPO', g, b))
  }
  
  if(do.mn) {
    g <- nnet::multinom(formula, data=data, ..., trace=FALSE)
    b <- predict(g, newdata, 'probs')
    if(nrow(newdata) == 1) b <- matrix(b, nrow=1)
    colnames(b) <- nam
    probsM      <- b
    md  <- apply(abs(b - probsPO), 1, mean)
    mad <- rbind(mad, cbind(method='Multinomial', newdata, `Mean |difference|`=md))
    A <- rbind(A, cbind(method='Multinomial', newdata, b))
    stats <- rbind(stats, st('Multinomial', g, b))
  }
  
  z <- reshape(A, direction='long', varying=list(nam),
               times=nam, v.names='Probability', timevar='y')
  z$method <- factor(z$method,
                     c('PO', c('PPO', 'Multinomial')[c(do.ppo, do.mn)]))
  
  rownames(stats) <- NULL
  
  nboot <- 0
  boot  <- array(NA, c(B, nrow(newdata), length(nam), do.ppo + do.mn),
                 dimnames=list(NULL, NULL, nam,
                               c('PPO', 'Multinomial')[c(do.ppo, do.mn)]))
  if(B > 0) {
    for(i in 1 : B) {
      j   <- sample(nrow(data), nrow(data), replace=TRUE)
      dat <- data[j, ]
      
      f <- lrm(formula, data=dat, ...)
      if(length(f$fail) && f$fail) next
      # If a Y category was not selected in this bootstrap sample,
      # go to the next sample
      if(length(names(f$freq)) != length(nam)) next
      a <- predict(f, newdata, type='fitted.ind')
      if(nrow(newdata) == 1) a <- matrix(a, nrow=1)
      colnames(a) <- nam
      
      if(do.ppo) {
        g <- try(VGAM::vglm(formula,
                            VGAM::cumulative(parallel=ppo, reverse=TRUE),
                            data=dat, coefstart=vglmcoef, ...),
                 silent=TRUE)
        if(inherits(g, 'try-error')) next
        b <- VGAM::predict(g, newdata, type='response')
        if(nrow(newdata) == 1) b <- matrix(b, nrow=1)
        colnames(b) <- nam
        pppo <- b
      }
      
      if(do.mn) {
        g <- try(nnet::multinom(formula, data=dat, ..., trace=FALSE))
        if(inherits(g, 'try-error')) next
        b <- predict(g, newdata, 'probs')
        if(nrow(newdata) == 1) b <- matrix(b, nrow=1)
        colnames(b) <- nam
        pmn <- b
      }
      nboot <- nboot + 1
      if(do.ppo) boot[nboot, , , 'PPO']         <- a - pppo
      if(do.mn)  boot[nboot, , , 'Multinomial'] <- a - pmn
    }
    if(nboot < B) boot <- boot[1 : nboot, , , , drop=FALSE]
  }
  
  structure(list(estimates=z, stats=stats, mad=mad, newdata=newdata,
                 nboot=nboot, boot=if(B > 0) boot),
            class='impactPO')
}


##' Print Result from impactPO
##'
##' Prints statistical summaries and optionally predicted values computed by `impactPO`, transposing statistical summaries for easy reading
##' @param x an object created by `impactPO`
##' @param estimates set to `FALSE` to suppess printing estimated category probabilities.  Defaults to `TRUE` when the number of rows < 16.
##' @param ... ignored
##' @author Frank Harrell
##' @method print impactPO
##' @export
##' @md
print.impactPO <- function(x, estimates=nrow(x$estimates) < 16, ...) {
  stats <- x$stats
  fstats <- stats
  integercol <- c('p', 'd.f.', '  d.f.')
  r2col <- c('Mean |difference| from PO',
             names(stats)[grep('R2', names(stats))])
  z <- function(x, digits=0, pval=FALSE) {
    y <- if(pval) ifelse(x < 0.0001, '<0.0001', format(round(x, 4)))
    else format(if(digits == 0) x else round(x, digits))
    y[is.na(x)] <- ''
    y
  }
  pvn <- '  Pr(>chi^2)'
  for(j in integercol) fstats[[j]]   <- z(fstats[[j]])
  for(j in r2col)      fstats[[j]]   <- z(fstats[[j]], 3)
  fstats[[pvn]] <- z(fstats[[pvn]], pval=TRUE)
  for(j in setdiff(names(fstats),
                   c('method', integercol, r2col, pvn)))
    fstats[[j]] <- z(fstats[[j]], 2)
  fstats <- t(fstats)
  colnames(fstats) <- fstats[1, ]
  fstats <- fstats[-1, ]
  print(fstats, quote=FALSE)
  
  if(estimates) {
    est <- x$estimates
    est$Probability <- round(est$Probability, 4)
    cat('\n')
    print(est)
  }
  
  cat('\nCovariate combination-specific mean |difference| in predicted probabilities\n\n')
  x$mad$`Mean |difference|` <- round(x$mad$`Mean |difference|`, 3)
  print(x$mad)
  
  if(x$nboot > 0) {
    boot <- x$boot
    cat('\nBootstrap 0.95 confidence intervals for differences in model predicted\nprobabilities based on', x$nboot, 'bootstraps\n\n')
    nd <- nrow(x$newdata)
    cl <- function(x) {
      qu <- unname(quantile(x, c(0.025, 0.975)))
      c(Lower=qu[1], Upper=qu[2]) }
    for(i in 1 : nd) {
      cat('\n')
      print(x$newdata[i, ])
      b <- boot[, i, , , drop=TRUE]
      b <- round(apply(b, 2:3, cl), 3)
      for(model in dimnames(b)[[3]]) {
        cat('\nPO - ', model, ' probability estimates\n\n', sep='')
        print(b[, , model])
      }
    }
  }
  
  invisible()
}

R2Measures <- function(lr, p, n, ess=NULL, padj=1) {
  R     <- numeric(0)
  r2    <- 1. - exp(- lr / n)
  adj <- function() if(padj == 1) 1. - exp(- max(lr - p, 0) / n)
  else
    1. - exp(- lr / n) * (n - 1.) / (n - p - 1.)
  r2adj <- adj()
  R <- c(R2=r2, R2adj=r2adj)
  g <- function(p, n) {
    n <- as.character(round(n, 1))
    sub <- ifelse(p == 0, n, paste0(p, ',', n))
    paste0('R2(', sub, ')')
  }
  name    <- g(c(0, p), c(n, n))
  
  if(length(ess)) {
    ## Replace n with effective sample size
    nr <- n
    n <- if(length(ess) == 1) ess else {
      P <- ess / sum(ess)
      n * (1. - sum(P ^ 3))
    }
    r2    <- 1. - exp(- lr / n)
    r2adj <- adj()
    if(n == nr) r2 <- r2adj <- NA
    name  <- c(name, g(c(0, p), c(n, n)))
    R     <- c(R, r2, r2adj)
  }
  names(R) <- name
  R
}

## Bootstrapping predicted probabilties 
d   <- expand.grid(Education='Tertiary', Sex=c('Male', 'Female'))
w <- impactPO(Disease_Severity ~ Sex + Education, nonpo = ~ Sex,
              data=df, newdata=d, B=300)

## Make graph of predicted probabilities comparing models 
revo <- function(z) {
  z <- as.factor(z)
  factor(z, levels=rev(levels(as.factor(z))))
}
ggplot(w$estimates, aes(x=method, y=Probability, fill=revo(y))) +
  facet_wrap(~ Sex) + geom_col() +
  xlab('') + guides(fill=guide_legend(title=''))


## Partial proportional odds models 
## Create new data frame 
set.seed(1234)
y<-c(1,2,3,4,5)
y<-factor(y,levels=c(1,2,3,4,5),labels=c("Disease Free","Mild","Moderate","Severe","Death"))


## Timepoint 1
y1<-as.factor(sample(y,size=1000,replace=TRUE,prob=c(0.1,0.2,0.35,0.2,0.2)))
y2<-as.factor(sample(y,size=1000,replace=TRUE,prob=c(0.25,0.15,0.25,0.15,0.15)))
outcome1<-cbind(y1,y2)
outcome1<-factor(outcome1,levels=c(1,2,3,4,5),labels=c("Disease Free","Mild","Moderate","Severe","Death"))

## Exposure 
x<-c(rep(0,1000),rep(1,1000))
x1<-factor(x,levels=0:1,labels=c("Control","Treatment"))

## Timepoint 2
y3<-as.factor(sample(y,size=1000,replace=TRUE,prob=c(0.1,0.15,0.25,0.25,0.25)))
y4<-as.factor(sample(y,size=1000,replace=TRUE,prob=c(0.15,0.25,0.4,0.15,0.05)))
outcome2<-cbind(y3,y4)
outcome2<-factor(outcome2,levels=c(1,2,3,4,5),labels=c("Disease Free","Mild","Moderate","Severe","Death"))

## Timepoint 3
y5<-as.factor(sample(y,size=1000,replace=TRUE,prob=c(0.05,0.1,0.3,0.3,0.25)))
y6<-as.factor(sample(y,size=1000,replace=TRUE,prob=c(0.2,0.35,0.35,0.05,0.05)))
outcome3<-cbind(y5,y6)
outcome3<-factor(outcome3,levels=c(1,2,3,4,5),labels=c("Disease Free","Mild","Moderate","Severe","Death"))

## Education 
edu <- c(1,2,3)
edu<-factor(edu,levels=1:3, labels=c("Primary","Secondary","Tertiary"))
edu2 <-sample(edu,size=2000,replace=TRUE,prob=c(0.1,0.4,0.5))

## Create data frame
df2 <- cbind(outcome1, outcome2, outcome3,edu2,x1)
df2<-as.data.frame(df2)
df2$outcome1 <- factor(df2$outcome1,levels=c(1,2,3,4,5),labels=c("Disease Free","Mild","Moderate","Severe","Death"))
df2$outcome2 <- factor(df2$outcome2,levels=c(1,2,3,4,5),labels=c("Disease Free","Mild","Moderate","Severe","Death"))
df2$outcome3 <- factor(df2$outcome3,levels=c(1,2,3,4,5),labels=c("Disease Free","Mild","Moderate","Severe","Death"))
df2$x1 <- factor(df2$x1,levels=c(1,2),labels=c("Control","Treatment"))
df2$id <- 1:nrow(df2)
df2$edu2 <- factor(df2$edu2,levels=c(1,2,3),labels=c("Primary","Secondary","Tertiary"))

View(df2)


## Unconstrained partial proportional odds model 
## Just use timepoint 1
library(tidyr)
library(VGAM)
library(MASS)
## fit ordered logit model and store results 'm'
m <- vglm(ordered(outcome1) ~ x1 + edu2, data = df2,family=cumulative(parallel=FALSE~x1,reverse=TRUE))

##Graphical method
library(Hmisc)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)))
}

s <- with(df2, summary(as.numeric(outcome1) ~ x1+edu2, fun=sf))

s[, 6] <- s[, 6] - s[, 3]
s[, 5] <- s[, 5] - s[, 3]
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s # print

plot(s, which=1:5, pch=1:5, xlab='Logit', main=' ', xlim=range(s[,3:6]))


m1 <- polr(outcome1 ~ x1+edu2, data = df2, Hess=TRUE)
brant(m1)

## view a summary of the model
summary(m)

##P-values?
## store table
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
ctable<-round(ctable,4)
ctable


## Profiling the likelihood 
(ci <- confint(m)) # default method gives profiled CIs

confint.default(m) # CIs assuming normality


## odds ratios
exp(coef(m))

## OR and CI
exp(cbind(OR = coef(m), ci))


## Using brms - EQUIVALENT as above 
df2$x1 <- x1
library(rstan)
library(brms)
library(rms)
options(mc.cores = parallel::detectCores()) # use multiple cores
library(rmsb)
bppo <- blrm(outcome1 ~ x1+edu2,ppo=~x1,data=df2)
bppo

## Constrained proportional odds 

# Allow for a different effect of treatment at y=5
df2$outcome1<-as.numeric(df2$outcome1)
g <- function(y) y==5    # max(y)=5 and y is discrete
f <- blrm(outcome1 ~ x1+edu2, ~ x1, cppo=g,data=df2)
# Compute the treatment effect log(OR) for y=1, 2, 3, 4, 5
h <- f$cppo     # normalized version of g (mean 0 SD 1)
k <- coef(f)
exp(k[5] + h(1:5) * k[8]) 


##If Y is skewed
z<- function(y) y^(1/3)
fp <- blrm(outcome1 ~ x1+edu2, ~ x1, cppo=z)
rbind(coef(fp), coef(fp, 'mode'), coef(fp, 'mean'))
k <- coef(fp)          # posterior means
h <- fp$cppo     # normalized version of g (mean 0 SD 1)
k[5] + h(2:5) * k[8]   # close to paper

## Location-Shift Model
library(ordDisp)
m3<-ordDisp(outcome1 ~ x1+edu2 | x1, data=df2, family = "cumulative",
             scaling=FALSE, reverse = TRUE)
summary(m3)


## Star plot to visualise effects 
plotordDisp(m3,names=c("x1"),colorvec=c(1,2),KI=TRUE)

##PO model 
summary(vglm(outcome1 ~ x1 + edu2, data = df2, family=cumulative(parallel=TRUE,reverse=TRUE),Hess=TRUE))

## Location-Scale Model
require(ordinal)
m4 <- clm(outcome1 ~ x1 + edu2, scale = ~ x1, data=df2)
summary(m4)
exp(-0.42411-(0.16435*0.5))
exp(-0.42411-(0.16435*1.5))
exp(-0.42411+(0.16435*0.5))
exp(-0.42411+(0.16435*1.5))
        
# Test for scale coefficient (ie is there different variance for treatment or education)
m5 <- clm(outcome1 ~ x1 + edu2, data=df2)
scale_test(m5)

##Splines - additive models 
require(rms)
ddist <- datadist(df2)
options ( datadist =" ddist ")
model2 <- blrm(ysamp ~ xsamp+rcs(agesamp),data=df)
summary(model2 , xsamp="Female",agesamp =c (50 ,60 ,70) ) 
plot(summary(model2,xsamp="Female",agesamp =c (50 ,60 ,70)))
p <- Predict ( model2 , agesamp =seq (20 ,80 , length =100) , xsamp="Female", conf.int = FALSE )
plot(p,xlab="Age (years)",ylab="Log Odds (Disease Severity)")
ggplot(p) 


## Structured thresholds 
equi0 <- clm(outcome1 ~ x1 + edu2, data=df2, threshold="flexible") 
equi1 <- clm(outcome1 ~ x1 + edu2, data=df2, threshold="equidistant") 
anova(equi0, equi1)

## LONGITUDINAL MODELLING 
## Markov model 
## Reshape to long format 
require(data.table)
long_df <- melt(setDT(df2), id.vars = c("id","edu2","x1"), variable.name = "outcome")
long_df[,4]<-(long_df$outcome)
names(long_df)[names(long_df) == "value"] <- "Disease_Severity"
names(long_df)[names(long_df) == "x1"] <- "treatment"
names(long_df)[names(long_df) == "edu2"] <- "education"
names(long_df)[names(long_df) == "outcome"] <- "timepoint" ## Assume it is Day 1, 2 and 3
categories <- c("Disease Free","Mild","Moderate","Severe","Death")
long_df$timepoint <- as.numeric(long_df$timepoint)
long_df$Disease_Severity <- factor(long_df$Disease_Severity, levels = categories, ordered = T) 
View(long_df)
## Absorbing state 
g <- function(x) if(length(x)) min(x, na.rm=TRUE) else 99
# Add variable 'first' which is time of first y=5 for subject (99 if never)
w <- long_df[, .(first=g(timepoint[Disease_Severity == "Death"])), by=id]
w<-as.data.frame(w)

df3 <- merge(long_df,w,by="id")
df3$Disease_Severity[df3$first == 1 & df3$timepoint == 2] <- "Death"
df3$Disease_Severity[df3$first == 2 & df3$timepoint == 3] <- "Death"
df3$Disease_Severity[df3$first == 1 & df3$timepoint == 3] <- "Death"


## Create lag variable 
setDT(df3)
df3 <- df3[order(id, timepoint), ]
df3[, pstatus := shift(Disease_Severity), by=id]
df3[timepoint == 1, pstatus := Disease_Severity]
df3$pstatus <- factor(df3$pstatus, levels = categories, ordered = T) 
df3$pstatus[df3$first == 2 & df3$timepoint == 3] <- "Death"
View(df3)


## Run the Markov PO model 
df3$pstatus<-factor(df3$pstatus,ordered=F)
f <- blrm(Disease_Severity ~ timepoint * (pstatus) + treatment,
         data=df3)
f

##Graphically observe the transitions 
propsTrans(Disease_Severity ~ timepoint + id, data=df3, maxsize=8) +
  theme(legend.position='bottom', axis.text.x=element_text(angle=90, hjust=1))


## Check proportional odds 
##Graphical method
library(Hmisc)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)))
}

s <- with(df3, summary(as.numeric(Disease_Severity) ~ treatment+pstatus+timepoint, fun=sf))

s[, 6] <- s[, 6] - s[, 3]
s[, 5] <- s[, 5] - s[, 3]
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s # print

plot(s, which=1:5, pch=1:5, xlab='Logit', main=' ',xlim=range(-5,0))


## Add random effects - show this has small variance 
f1 <- brm(ordered(Disease_Severity) ~ timepoint * (pstatus) +treatment + (1|id), family=cumulative("logit"),
          data=df3)
f1


## GEEs
library(multgee)
intrinsic.pars(long_df$Disease_Severity, data = df3, id = id, repeated = timepoint, rscale = "ordinal")
modgee <- ordLORgee(Disease_Severity ~ factor(treatment) + timepoint, link="logit",id=id,data=df3,
                 LORstr = "uniform",repeated=timepoint)
summary(modgee)


## Random effects 
## Normal priors for parameters
## Random effects: uses exponential or half t-distribution 
# random intercept
#! takes a few minutes to compile & run
## Using brms
brms.fit1 <- brm(ordered(Disease_Severity) ~ treatment + timepoint + (1|id), 
                 data = df3, family=cumulative("logit"))

plot(brms.fit1) # check posterior densities and traceplots
summary(brms.fit1)


# random intercept and random slope 
#! takes a few minutes to compile & run
brms.fit2 <- brm(factor(Disease_Severity) ~ treatment + timepoint + (1+timepoint|id),
                 data = df3, family=cumulative("logit"),prior=set_prior("normal(0,5)"))
                 
plot(brms.fit2) # check posterior densities and traceplots
summary(brms.fit2)

