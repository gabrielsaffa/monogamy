################################################################################
## this script tests the interaction models for the effect of population density
## on continuous marriage patterns across each level of agricultural intensity
## in these models, only intercept will be stratified by language family


setwd("")
library(rethinking)
library(ape)
library(tidyr)


## load the data and re-code the variables 
mono_data <- read.csv("mono_data.csv",header=TRUE)
mono_data <- transform(mono_data, family=as.numeric(factor(mono_data$family_glottolog))) # assign a unique number to each language family
family <- mono_data$family
soc <- mono_data$soc # society index
PP <- 100 - mono_data$percent_polygyny_women # convert percent polygynous women to percent monogamous women
PP <- PP/100 # convert to proportions
PP <- ifelse(PP==1.00,0.999,data$PP) # replace 1s with 0.999
IA <- mono_data$agriculture_intensity
IA <- ifelse(IA==6,1,ifelse(IA==5,1,0)) # intensive agriculture
PD <- mono_data$population_density 
PD <- ifelse(PD==5,1,0) # high population density 
data <- data.frame(cbind(family,soc,PP,IA,PD))
data <- data[complete.cases(data$PP),] # N=143
data <- transform(data,family=as.numeric(factor(data$family))) # assign a unique number to each language family


## we'll use phylogenetic relatedness to account for potential confounding due to common ancestry as well as unobserved confounds
## we'll implement the phylogeny as in PGLMM; that is, convert the tree into a matrix of correlations and estimate their magnitude
## this way, it will allow us to readily compare variation in marriage patterns between phylogenetic and language family components
soc <- data$soc
mono_tree <- read.tree("SPT.SCCS.tre") # read the tree
mono_tree <- keep.tip(mono_tree,soc) # prune the tree to match with the data
R_OU <- corMartins(1,phy=mono_tree,form=~soc,fixed=FALSE) # adjust the tree according to the OU model but let the alpha be estimated rather than fixed
V <- vcv(R_OU) # convert to variance-covariance matrix
V <- V[soc,soc] # order the societies
R <- V/max(V) # convert to a correlation matrix
N <- length(soc)
society <- 1:N # society index


## put the variables in a list
d <- list(PP=as.numeric(data$PP),
          IA=as.integer(data$IA) + 1,
          PD=as.integer(data$PD),
          family=as.integer(data$family),
          society=society,
          N=N,
          R=R
)

## write the model in a list
m_list <- alist(
  # marriage patterns model
  PP ~ dbeta2(p,theta),
  logit(p) <- mu_a + z_a[family]*sigma_a + bPD[IA]*PD + phy[society],
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  theta ~ exponential(0.5),
  mu_a ~ normal(0,1),
  bPD[IA] ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
pd <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
pd_prec <- precis(pd,3,pars=c("mu_a","bPD","sigma_a","sigma_phy"),prob=0.9) 
pd_prec <- round(pd_prec,2)
write.csv(pd_prec,file="pd_int_con_prec.csv")
pd_post <- extract.samples(pd)
pd_post <- pd_post[c(-1:-2,-8:-10)] # remove redundant elements
write.csv(pd_post,file="pd_int_con_post.csv",row.names=FALSE)


################################################################################