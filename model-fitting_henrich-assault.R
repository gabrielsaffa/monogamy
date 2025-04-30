################################################################################
## this script tests the interaction models for the effect of assault frequency
## on trade and military organization across the levels of social complexity
## in these models, only intercept will be stratified by language family


setwd("")
library(rethinking)
library(ape)
library(tidyr)


## load the data and re-code the variables 
mono_data <- read.csv("mono_data.csv",header=TRUE)
mono_data <- transform(mono_data, family=as.numeric(factor(mono_data$family_glottolog))) # assign a unique number to each language family
family <- mono_data$family # language family
soc <- mono_data$soc # society index
AF <- mono_data$assault_original # assault frequency
AF <- (AF - mean(AF,na.rm=TRUE))/(sd(AF,na.rm=TRUE)*2) # standardize assault frequency
PI <- mono_data$jurisdictional_hierarchy
PI <- ifelse(PI==5,1,0) # large states
TR <- mono_data$intercommunity_trade 
TR <- ifelse(TR==7,6,TR)
TR <- ifelse(TR==6,1,ifelse(TR==5,1,0)) # 50% trade
MM <- mono_data$military_mobilization # military mobilization
MM <- ifelse(MM==1,1,0) # age grades, military societies, standing armies


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


## put the variables in a list
d <- list(AF=AF,
          PI=PI + 1,
          TR=TR,
          family=family,
          society=society,
          N=N,
          R=R
)


## fit the model for trade
m_list <- alist(
  ## trade
  TR ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + bAF[PI]*AF + phy[society], 
  
  # estimate missing values in assault frequency
  AF ~ multi_normal(mu,S_AF),
  mu <- aAF + bPI_AF*PI,
  
  # phylogenetic covariance for assault frequency
  matrix[N,N]:S_AF <- sigma_phy_AF*R,
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bAF[PI] ~ normal(0,0.5),
  aAF ~ normal(0,1),
  bPI_AF ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy_AF ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
af <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
af_prec <- precis(af,3,pars=c("mu_a","bAF"),prob=0.9) 
af_prec <- round(af_prec,2)
write.csv(af_prec,file="tr_af_prec.csv")
af_post <- extract.samples(af)
af_post <- af_post[c(-1:-2,-5:-14)] # remove redundant elements
write.csv(af_post,file="tr_af_post.csv",row.names=FALSE)


## ## fit the model for military organization
## because Stan imputes missing values for normally distributed variables only, we will do a complete-case analysis
mono_data_MM <- data.frame(cbind(soc,MM,AF,PI,family)) # combine the variables
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
d <- list(MM=as.integer(mono_data_MM$MM),
          AF=as.numeric(mono_data_MM$AF),
          PI=as.integer(mono_data_MM$PI) + 1,
          family=as.integer(mono_data_MM$family),
          society=as.integer(society_mm),
          N=N_mm,
          R=R_mm
)


m_list <- alist(
  ## military organization model
  MM ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + bAF[PI]*AF + phy[society], 
  
  # estimate missing values in assault frequency
  AF ~ multi_normal(mu,S_AF),
  mu <- aAF + bPI_AF*PI,
  
  # phylogenetic covariance for assault frequency
  matrix[N,N]:S_AF <- sigma_phy_AF*R,
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bAF[PI] ~ normal(0,0.5),
  aAF ~ normal(0,1),
  bPI_AF ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy_AF ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
mm <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
mm_prec <- precis(mm,3,pars=c("mu_a","bAF"),prob=0.9) 
mm_prec <- round(mm_prec,2)
write.csv(mm_prec,file="mm_af_prec.csv")
mm_post <- extract.samples(mm)
mm_post <- mm_post[c(-1:-2,-5:-14)] # remove redundant elements
write.csv(mm_post,file="mm_af_post.csv",row.names=FALSE)


################################################################################