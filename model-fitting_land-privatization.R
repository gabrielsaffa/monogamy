##############################################################
## this script tests the effects of our land shortage proxies:
## population density, altitude, and incline on land privatization


setwd("")
library(rethinking)
library(ape)
library(tidyr)


## load the data and re-code the variables 
mono_data <- read.csv("mono_data.csv",header=TRUE)
mono_data <- transform(mono_data, family=as.numeric(factor(mono_data$family_glottolog))) # assign a unique number to each language family
family <- mono_data$family
soc <- mono_data$soc # society index
mono_data$land_ownership_Kavanagh <- mono_data$land_ownership_Kavanagh + 1 
original <- c(1,2,3)
new <- c(3,2,1)
mono_data$land_communality <- new[match(mono_data$land_communality,original)] # invert the scale
LP <- apply(mono_data[,12:15],1,mean,na.rm=TRUE) # average land ownership variables to create land privatization measure
LP[is.nan(LP)] <- NA # replace NaNs with NAs
LP <- (LP - mean(LP,na.rm=TRUE))/(sd(LP,na.rm=TRUE)*2) # standardize rival wealth
IA <- mono_data$agriculture_intensity
IA <- ifelse(IA==6,1,ifelse(IA==5,1,0)) # intensive agriculture
PD <- mono_data$population_density 
PD <- ifelse(PD==5,1,0) # high population density 
AL <- mono_data$altitude # altitude
AL <- (AL - mean(AL,na.rm=TRUE))/(sd(AL,na.rm=TRUE)*2) # standardize altitude
IC <- mono_data$incline # incline
IC <- (IC - mean(IC,na.rm=TRUE))/(sd(IC,na.rm=TRUE)*2) # standardize incline


mono_tree <- read.tree("SPT.SCCS.tre") # read the tree
R_OU <- corMartins(1,phy=mono_tree,form=~soc,fixed=FALSE) # adjust the tree according to the OU model but let the alpha be estimated rather than fixed
V <- vcv(R_OU) # convert to variance-covariance matrix
V <- V[soc,soc] # order the societies
R <- V/max(V) # convert to a correlation matrix
N <- length(soc)
society <- 1:N # society index


## estimate the total effect of population density on land privatization
## we need to control for agriculture
d <- list(LP=LP,
          IA=IA,
          PD=PD,
          AL=AL,
          IC=IC,
          family=family,
          society=society,
          N=N,
          R=R
)


## fit the model for land privatization
m_list <- alist(
  ## land privatization model
  LP ~ multi_normal(mu,S),
  mu <- mu_a + z_a[family]*sigma_a + bPD*PD + bIA*IA, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bPD ~ normal(0,0.5),
  bIA ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
lp <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
lp_pd_prec <- precis(lp,3,pars=c("mu_a","bPD","sigma_a","sigma_phy"),prob=0.9) 
lp_pd_prec <- data.frame(round(lp_pd_prec,2))
write.csv(data.frame(lp_pd_prec),file="lp_pd_prec.csv")
lp_pd_post <- extract.samples(lp,pars=c("mu_a","bPD","sigma_a","sigma_phy")) 
write.csv(data.frame(lp_pd_post),file="lp_pd_post.csv")


## estimate the total effect of altitude on land privatization
## we need to control for incline
m_list <- alist(
  ## land privatization model
  LP ~ multi_normal(mu,S),
  mu <- mu_a + z_a[family]*sigma_a + bAL*AL + bIC*IC, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bAL ~ normal(0,0.5),
  bIC ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
lp <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
lp_al_prec <- precis(lp,3,pars=c("mu_a","bAL","sigma_a","sigma_phy"),prob=0.9) 
lp_al_prec <- data.frame(round(lp_al_prec,2))
write.csv(data.frame(lp_al_prec),file="lp_al_prec.csv")
lp_al_post <- extract.samples(lp,pars=c("mu_a","bAL","sigma_a","sigma_phy")) 
write.csv(data.frame(lp_al_post),file="lp_al_post.csv")


## estimate the total effect of incline on land privatization
## we don't control for anything
m_list <- alist(
  ## land privatization model
  LP ~ multi_normal(mu,S),
  mu <- mu_a + z_a[family]*sigma_a + bIC*IC, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bIC ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
lp <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
lp_ic_prec <- precis(lp,3,pars=c("mu_a","bIC","sigma_a","sigma_phy"),prob=0.9) 
lp_ic_prec <- data.frame(round(lp_ic_prec,2))
write.csv(data.frame(lp_ic_prec),file="lp_ic_prec.csv")
lp_ic_post <- extract.samples(lp,pars=c("mu_a","bIC","sigma_a","sigma_phy")) 
write.csv(data.frame(lp_ic_post),file="lp_ic_post.csv")


################################################################################