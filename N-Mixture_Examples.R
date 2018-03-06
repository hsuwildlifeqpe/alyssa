install.packages("unmakred")
library(unmarked)

data<-read.csv("Count_DITE_Robust.csv")
data<-data[,2:5]

#Repeated count data with 4 primary periodsand
#not secondary sampling period (ie J==1)
y2<-data.matrix(data,rownames.force = NA)

#Site specific co-variates
sc2<-read.csv("SiteCovs.csv")
sc2<-sc2[,2:13]

#Observation Specific Covariates


#Primary Periods of survey
#?

umf2 <- unmarkedFramePCount(y=y2, siteCovs=sc2)

#take a look
umf2
summary(umf2)

#Fitting Models
fm1<-pcount(~1~1,umf2,K=50)
backTransform(fm1,'det')
confint(fm1,type='state')

#Example 1#####
############


#Repeated count data with 5 primary periods and
# no secondary sampling periods (ie J==1)
y1 <- matrix(c(
  0, 2, 3, 2, 0,
  2, 2, 3, 1, 1,
  1, 1, 0, 0, 3,
  0, 0, 0, 0, 0), nrow=4, ncol=5, byrow=TRUE)

# Site-specific covariates
sc1 <- data.frame(x1 = 1:4, x2 = c('A','A','B','B'))

# Observation-specific covariates
oc1 <- list(
  x3 = matrix(1:5, nrow=4, ncol=5, byrow=TRUE),
  x4 = matrix(letters[1:5], nrow=4, ncol=5, byrow=TRUE))

# Primary periods of surveys
primaryPeriod1 <- matrix(as.integer(c(
  1, 2, 5, 7, 8,
  1, 2, 3, 4, 5,
  1, 2, 4, 5, 6,
  1, 3, 5, 6, 7)), nrow=4, ncol=5, byrow=TRUE)


# Create the unmarkedFrame
umf1 <- unmarkedFramePCO(y=y1, siteCovs=sc1, obsCovs=oc1, numPrimary=5,
                         primaryPeriod=primaryPeriod1)

# Take a look
umf1
summary(umf1)






# Repeated count data with 4 primary periods and
# no 2 secondary sampling periods (ie J=2)
y2 <- matrix(c(
  0,0,  2,2,  3,2,  2,2,
  2,2,  2,1,  3,2,  1,1,
  1,0,  1,1,  0,0,  0,0,
  0,0,  0,0,  0,0,  0,0), nrow=4, ncol=8, byrow=TRUE)


# Site-specific covariates
sc2 <- data.frame(x1 = 1:4, x2 = c('A','A','B','B'))

# Observation-specific covariates
oc2 <- list(
  x3 = matrix(1:8, nrow=4, ncol=8, byrow=TRUE),
  x4 = matrix(letters[1:8], nrow=4, ncol=8, byrow=TRUE))

# Yearly-site covariates
ysc <- list(
  x5 = matrix(c(
    1,2,3,4,
    1,2,3,4,
    1,2,3,4,
    1,2,3,4), nrow=4, ncol=4, byrow=TRUE))

# Primary periods of surveys
primaryPeriod2 <- matrix(as.integer(c(
  1,2,5,7,
  1,2,3,4,
  1,2,4,5,
  1,3,5,6)), nrow=4, ncol=4, byrow=TRUE)

# Create the unmarkedFrame
umf2 <- unmarkedFramePCO(y=y2, siteCovs=sc2, obsCovs=oc2,
                         yearlySiteCovs=ysc,
                         numPrimary=4, primaryPeriod=primaryPeriod2)

# Take a look
umf2
summary(umf2)


####Example 2#####

# Simulate data
set.seed(35)
nSites <- 100
nVisits <- 3
x <- rnorm(nSites) # a covariate
beta0 <- 0
beta1 <- 1
lambda <- exp(beta0 + beta1*x) # expected counts at each site
N <- rpois(nSites, lambda) # latent abundance
y <- matrix(NA, nSites, nVisits)
p <- c(0.3, 0.6, 0.8) # detection prob for each visit
for(j in 1:nVisits) {
  y[,j] <- rbinom(nSites, N, p[j])
}
# Organize data
visitMat <- matrix(as.character(1:nVisits), nSites, nVisits, byrow=TRUE)
umf <- unmarkedFramePCount(y=y, siteCovs=data.frame(x=x),
                           obsCovs=list(visit=visitMat))
summary(umf)
# Fit a model
fm1 <- pcount(~visit-1 ~ x, umf, K=50)
fm1
plogis(coef(fm1, type="det")) # Should be close to p
#plogis=Density, distribution function, quantile and random generation for the logistic distribution with param. location and scale
#Is this unsed instead of 'backtransform'?


##Empirical Bayes estimation of random effects##
(fm1re <- ranef(fm1))
#Estimate posterior distributions of the random variables(latenet abundance or occurence) using emprical Bayes methods
#hese methods return an object storing the posterior distributions of the latent variables at each site, and for each 
#year (primary period) in the case of open population models

plot(fm1re, subset=site %in% 1:25, xlim=c(-1,40))
sum(bup(fm1re)) # Estimated population size
?bup
sum(N) # Actual population size
## Not run:

###Example 3###
###############
# Real data
data(mallard)
mallardUMF <- unmarkedFramePCount(mallard.y, siteCovs = mallard.site,
                                  obsCovs = mallard.obs)
(fm.mallard <- pcount(~ ivel+ date + I(date^2) ~ length + elev + forest, mallardUMF, K=30))
(fm.mallard.nb <- pcount(~ date + I(date^2) ~ length + elev, mixture = "NB", mallardUMF, K=30))

plogis(coef(fm.mallard, type="det")) 
(fm.mallard.re <- ranef(fm.mallard))
plot(fm.mallard.re, subset=site %in% 1:25, xlim=c(-1,40))
sum(bup(fm.mallard.re)) # Estimated population size

## End(Not run)