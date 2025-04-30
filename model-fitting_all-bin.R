####################################
## monogamy - model fitting (binary)

## this script includes data preparation/coding and tests the main hypotheses using binary measure of monogamy
## we group these hypotheses into broader theoretical concepts, which we label as "polygyny-prohibiting" and "polygyny-declining" models
## the script runs multilevel models, using both phylogenetic and language family varying effects, to test these concepts

setwd("")
library(rethinking)
library(ape)
library(tidyr)


## load the data and re-code the variables 
mono_data <- read.csv("mono_data.csv",header=TRUE)
mono_data <- transform(mono_data, family=as.numeric(factor(mono_data$family_glottolog))) # assign a unique number to each language family
family <- mono_data$family # language family
soc <- mono_data$soc # society index
MP <- ifelse(mono_data$normative_monogamy==1,1,0) # binary indicator for monogamy
MP <- as.integer(MP)
AF <- mono_data$assault_original # assault frequency
AF <- (AF - mean(AF,na.rm=TRUE))/(sd(AF,na.rm=TRUE)*2) # standardize assault frequency
PI <- mono_data$jurisdictional_hierarchy
PI <- ifelse(PI==5,1,0) # large states
TR <- mono_data$intercommunity_trade 
TR <- ifelse(TR==7,6,TR)
TR <- ifelse(TR==6,1,ifelse(TR==5,1,0)) # 50% trade
MM <- mono_data$military_mobilization # military mobilization
MM <- ifelse(MM==1,1,0) # age grades, military societies, standing armies
SS <- mono_data$class_differentiation # social stratification
SS <- ifelse(SS==5,1,0) # complex societies
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


## we'll use phylogenetic relatedness to account for potential confounding due to common ancestry as well as unobserved confounds
## we'll implement the phylogeny as in PGLMM; that is, convert the tree into a matrix of correlations and estimate their magnitude
## this way, it will allow us to readily compare variation in marriage patterns between phylogenetic and language family components
mono_tree <- read.tree("SPT.SCCS.tre") # read the tree
R_OU <- corMartins(1,phy=mono_tree,form=~soc,fixed=FALSE) # adjust the tree according to the OU model but let the alpha be estimated rather than fixed
V <- vcv(R_OU) # convert to variance-covariance matrix
V <- V[soc,soc] # order the societies
R <- V/max(V) # convert to a correlation matrix
N <- length(soc)
society <- 1:N # society index


## model fitting
## first we'll estimate the total effect of percent polygyny on assault frequency based on the inter-group competition model
## to see whether monogamy really decreases male-male competition
## put the variables in a list
d <- list(MP=MP,
          AF=AF,
          PI=PI,
          family=family,
          society=society,
          N=N,
          R=R
)


## fit the model for assault frequency
m_list <- alist(
  ## assault frequency model
  AF ~ multi_normal(mu,S),
  mu <- mu_a + z_a[family]*sigma_a + (mu_bMP + z_bMP[family]*sigma_bMP)*MP + (mu_bPI + z_bPI[family]*sigma_bPI)*PI, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMP[family] ~ normal(0,1),
  z_bPI[family] ~ normal(0,1),
  
  # phylogenetic covariance
  matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bMP ~ normal(0,0.5),
  mu_bPI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMP ~ exponential(1),
  sigma_bPI ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMP <<- mu_bMP + z_bMP*sigma_bMP,
  gq> vector[family]:bPI <<- mu_bPI + z_bPI*sigma_bPI
)

## fit the model with ulam
af <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
af_prec <- precis(af,3,pars=c("mu_a","mu_bMP","sigma_a","sigma_phy","sigma_bMP"),prob=0.9) 
af_prec <- round(af_prec,2)
write.csv(af_prec,file="af_bin_prec.csv")
af_post <- extract.samples(af)
af_post <- af_post[c(-1:-3,-11)] # remove redundant elements
write.csv(af_post,file="af_bin_post.csv",row.names=FALSE)


## estimate the total effect of marriage patterns on trade based on the inter-group competition model
## put the variables in a list
d <- list(MP=MP,
          TR=TR,
          PI=PI,
          family=family,
          society=society,
          N=N,
          R=R
)


## write the model in a list
m_list <- alist(
  ## trade model
  TR ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + (mu_bMP + z_bMP[family]*sigma_bMP)*MP + (mu_bPI + z_bPI[family]*sigma_bPI)*PI + phy[society],
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMP[family] ~ normal(0,1),
  z_bPI[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bMP ~ normal(0,0.5),
  mu_bPI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMP ~ exponential(1),
  sigma_bPI ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMP <<- mu_bMP + z_bMP*sigma_bMP,
  gq> vector[family]:bPI <<- mu_bPI + z_bPI*sigma_bPI
)

## fit the model with ulam
tr <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
tr_prec <- precis(tr,3,pars=c("mu_a","mu_bMP","sigma_a","sigma_phy","sigma_bMP"),prob=0.9) 
tr_prec <- round(tr_prec,2)
write.csv(tr_prec,file="tr_bin_prec.csv")
tr_post <- extract.samples(tr)
tr_post <- tr_post[c(-1:-4,-12:-14)] # remove redundant elements
write.csv(tr_post,file="tr_bin_post.csv",row.names=FALSE)


## estimate the total effect of marriage patterns on military organization based on the inter-group competition model
## because Stan imputes missing values for normally distributed variables only, we will do a complete-case analysis
mono_data_MM <- data.frame(cbind(soc,MP,MM,PI,family)) # combine the variables
mono_data_MM <- mono_data_MM[complete.cases(mono_data_MM$MM),] # drop NAs (N=160)
mono_data_MM <- transform(mono_data_MM,family=as.numeric(factor(family)))
mono_tree_MM <- keep.tip(mono_tree,mono_data_MM$soc) # drop societies from the tree that are not in the sample
R_OU <- corMartins(1,phy=mono_tree_MM,form=~mono_data_MM$soc,fixed=FALSE) # adjust the tree according to the OU model but let the alpha be estimated rather than fixed
V <- vcv(R_OU) # convert to a variance-covariance matrix
V <- V[mono_data_MM$soc,mono_data_MM$soc] # order the societies
R_mm <- V/max(V) # convert to a correlation matrix
N_mm <- length(mono_data_MM$soc)
society_mm <- 1:N_mm # society index

## put the variables in a list
d <- list(MP=as.integer(mono_data_MM$MP),
          MM=as.integer(mono_data_MM$MM),
          PI=as.integer(mono_data_MM$PI),
          family=as.integer(mono_data_MM$family),
          society=as.integer(society_mm),
          N=N_mm,
          R=R_mm
)


m_list <- alist(
  ## military organization model
  MM ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + (mu_bMP + z_bMP[family]*sigma_bMP)*MP + (mu_bPI + z_bPI[family]*sigma_bPI)*PI + phy[society],
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMP[family] ~ normal(0,1),
  z_bPI[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bMP ~ normal(0,0.5),
  mu_bPI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMP ~ exponential(1),
  sigma_bPI ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMP <<- mu_bMP + z_bMP*sigma_bMP,
  gq> vector[family]:bPI <<- mu_bPI + z_bPI*sigma_bPI
)


## fit the model with ulam
mm <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
mm_prec <- precis(mm,3,pars=c("mu_a","mu_bMP","sigma_a","sigma_phy","sigma_bMP"),prob=0.9) 
mm_prec <- round(mm_prec,2)
write.csv(mm_prec,file="mm_bin_prec.csv")
mm_post <- extract.samples(mm)
mm_post <- mm_post[c(-1:-4,-12:-14)] # remove redundant elements
write.csv(mm_post,file="mm_bin_post.csv",row.names=FALSE)


## estimate the total effect of land privatization on marriage patterns based on the rival wealth importance model
## put the variables in a list
d <- list(MP=MP,
          LP=LP,
          SS=SS,
          IA=IA,
          PD=PD,
          AL=AL,
          IC=IC,
          family=family,
          society=society,
          N=N,
          R=R
)


## write the model in a list
m_list <- alist(
  # marriage patterns model
  MP ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + (mu_bLP + z_bLP[family]*sigma_bLP)*LP + (mu_bIA + z_bIA[family]*sigma_bIA)*IA + phy[society], 
  
  # estimate missing values in land privatization
  LP ~ multi_normal(mu,S_LP),
  mu <- aLP + bIA_LP*IA + bPD_LP*PD + bAL_LP*AL + bIC_LP*IC,
  
  # phylogenetic covariance for land privatization
  matrix[N,N]:S_LP <- sigma_phy_LP*R,
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bLP[family] ~ normal(0,1),
  z_bIA[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bLP ~ normal(0,0.5),
  mu_bIA ~ normal(0,0.5),
  aLP ~ normal(0,1),
  bIA_LP ~ normal(0,0.5),
  bPD_LP ~ normal(0,0.5),
  bAL_LP ~ normal(0,0.5),
  bIC_LP ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bLP ~ exponential(1),
  sigma_bIA ~ exponential(1),
  sigma_phy_LP ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bLP <<- mu_bLP + z_bLP*sigma_bLP,
  gq> vector[family]:bIA <<- mu_bIA + z_bIA*sigma_bIA
)

## fit the model with ulam
lp <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
lp_prec <- precis(lp,3,pars=c("mu_a","mu_bLP","sigma_a","sigma_phy","sigma_bLP"),prob=0.9) 
lp_prec <- round(lp_prec,2)
write.csv(lp_prec,file="lp_bin_prec.csv")
lp_post <- extract.samples(lp)
lp_post <- lp_post[c(-1:-4,-8:-12,-16,-18:-20)] # remove redundant elements
write.csv(lp_post,file="lp_bin_post.csv",row.names=FALSE)


## we'll also estimate the total effect of social stratification on marriage patterns
## to see whether it complies with the polygyny threshold model
## our causal graph implies two adjustment sets: {land privatization, population density} and {land privatization, agriculture intensity}
## both sets are sufficient for unbiased estimate, we'll choose the second one
## write the model in a list
m_list <- alist(
  # marriage patterns model
  MP ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + (mu_bSS + z_bSS[family]*sigma_bSS)*SS + (mu_bLP + z_bLP[family]*sigma_bLP)*LP + (mu_bIA + z_bIA[family]*sigma_bIA)*IA + phy[society], 
  
  # estimate missing values in land privatization
  LP ~ multi_normal(mu,S_LP),
  mu <- aLP + bIA_LP*IA + bPD_LP*PD + bAL_LP*AL + bIC_LP*IC,
  
  # phylogenetic covariance for land privatization
  matrix[N,N]:S_LP <- sigma_phy_LP*R,
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bSS[family] ~ normal(0,1),
  z_bLP[family] ~ normal(0,1),
  z_bIA[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bSS ~ normal(0,0.5),
  mu_bLP ~ normal(0,0.5),
  mu_bIA ~ normal(0,0.5),
  aLP ~ normal(0,1),
  bIA_LP ~ normal(0,0.5),
  bPD_LP ~ normal(0,0.5),
  bAL_LP ~ normal(0,0.5),
  bIC_LP ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bSS ~ exponential(1),
  sigma_bLP ~ exponential(1),
  sigma_bIA ~ exponential(1),
  sigma_phy_LP ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bSS <<- mu_bSS + z_bSS*sigma_bSS,
  gq> vector[family]:bLP <<- mu_bLP + z_bLP*sigma_bLP,
  gq> vector[family]:bIA <<- mu_bIA + z_bIA*sigma_bIA
)

## fit the model with ulam
ss <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
ss_prec <- precis(ss,3,pars=c("mu_a","mu_bSS","sigma_a","sigma_phy","sigma_bSS"),prob=0.9) 
ss_prec <- round(ss_prec,2)
write.csv(ss_prec,file="ss_bin_prec.csv")
ss_post <- extract.samples(ss)
ss_post <- ss_post[c(-1:-5,-10:-14,-19,-21:-23)] # remove redundant elements
write.csv(ss_post,file="ss_bin_post.csv",row.names=FALSE)


## estimate the total effect of population density on marriage patterns based on the rival wealth importance model
## put the variables in a list
d <- list(MP=MP,
          PD=PD,
          IA=IA,
          AL=AL,
          IC=IC,
          family=family,
          society=society,
          N=N,
          R=R
)


## write the model in a list
m_list <- alist(
  # marriage patterns model
  MP ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + (mu_bPD + z_bPD[family]*sigma_bPD)*PD + (mu_bIA + z_bIA[family]*sigma_bIA)*IA + phy[society],
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bPD[family] ~ normal(0,1),
  z_bIA[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bPD ~ normal(0,0.5),
  mu_bIA ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bPD ~ exponential(1),
  sigma_bIA ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bPD <<- mu_bPD + z_bPD*sigma_bPD,
  gq> vector[family]:bIA <<- mu_bIA + z_bIA*sigma_bIA
)

## fit the model with ulam
pd <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
pd_prec <- precis(pd,3,pars=c("mu_a","mu_bPD","sigma_a","sigma_phy","sigma_bPD"),prob=0.9) 
pd_prec <- round(pd_prec,2)
write.csv(pd_prec,file="pd_bin_prec.csv")
pd_post <- extract.samples(pd)
pd_post <- pd_post[c(-1:-4,-12:-14)] # remove redundant elements
write.csv(pd_post,file="pd_bin_post.csv",row.names=FALSE)


## estimate the total effect of altitude on marriage patterns based on the rival wealth importance model
## because topographic proxies for land shortage are exogenous in our causal graph, no variables are needed to condition the model on
## however, we will condition on incline
## write the model in a list
m_list <- alist(
  # marriage patterns model
  MP ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + (mu_bAL + z_bAL[family]*sigma_bAL)*AL + (mu_bIC + z_bIC[family]*sigma_bIC)*IC + phy[society], 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bAL[family] ~ normal(0,1),
  z_bIC[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bAL ~ normal(0,0.5),
  mu_bIC ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bAL ~ exponential(1),
  sigma_bIC ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bAL <<- mu_bAL + z_bAL*sigma_bAL,
  gq> vector[family]:bIC <<- mu_bIC + z_bIC*sigma_bIC
)

## fit the model with ulam
al <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
al_prec <- precis(al,3,pars=c("mu_a","mu_bAL","sigma_a","sigma_phy","sigma_bAL"),prob=0.9) 
al_prec <- round(al_prec,2)
write.csv(al_prec,file="al_bin_prec.csv")
al_post <- extract.samples(al)
al_post <- al_post[c(-1:-4,-12:-14)] # remove redundant elements
write.csv(al_post,file="al_bin_post.csv",row.names=FALSE)


## estimate the total effect of incline on marriage patterns based on the rival wealth importance model
m_list <- alist(
  # marriage patterns model
  MP ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + (mu_bIC + z_bIC[family]*sigma_bIC)*IC + phy[society], 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bIC[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  mu_bIC ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bIC ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bIC <<- mu_bIC + z_bIC*sigma_bIC
)

## fit the model with ulam
ic <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
ic_prec <- precis(ic,3,pars=c("mu_a","mu_bIC","sigma_a","sigma_phy","sigma_bIC"),prob=0.9) 
ic_prec <- round(ic_prec,2)
write.csv(ic_prec,file="ic_bin_prec.csv")
ic_post <- extract.samples(ic)
ic_post <- ic_post[c(-1:-3,-9:-11)] # remove redundant elements
write.csv(ic_post,file="ic_bin_post.csv",row.names=FALSE)


################################################################################