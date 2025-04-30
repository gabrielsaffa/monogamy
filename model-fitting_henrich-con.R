##########################################################################################
## this script tests the interaction models for the effect of continuous marriage patterns
## on assault frequency, trade, and military organization across the levels of social complexity
## in these models, only intercept will be stratified by language family


setwd("")
library(rethinking)
library(ape)
library(tidyr)


## load the data and re-code the variables 
mono_data <- read.csv("mono_data.csv",header=TRUE)
family <- mono_data$family_glottolog # language family
soc <- mono_data$soc # society index
PP <- 100 - mono_data$percent_polygyny_women # convert percent polygynous women to percent monogamous women
PP <- PP/100 # convert to proportions
PP <- ifelse(PP==1.00,0.999,PP) # replace 1s with 0.999
PP_sd <- (PP - mean(PP,na.rm=TRUE))/(sd(PP,na.rm=TRUE)*2) # standardize percent monogamous women
AF <- mono_data$assault_original # assault frequency
AF <- (AF - mean(AF,na.rm=TRUE))/(sd(AF,na.rm=TRUE)*2) # standardize assault frequency
PI <- mono_data$jurisdictional_hierarchy
PI <- ifelse(PI==5,1,0) # large states
TR <- mono_data$intercommunity_trade 
TR <- ifelse(TR==7,6,TR)
TR <- ifelse(TR==6,1,ifelse(TR==5,1,0)) # 50% trade
MM <- mono_data$military_mobilization # military mobilization
MM <- ifelse(MM==1,1,0) # age grades, military societies, standing armies
data <- data.frame(cbind(family,soc,PP_sd,AF,PI,TR,MM))
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
d <- list(PP=as.numeric(data$PP_sd),
          AF=as.numeric(data$AF),
          PI=as.integer(data$PI) + 1,
          family=as.integer(data$family),
          society=as.integer(society),
          N=N,
          R=R
)


## fit the model for assault frequency
m_list <- alist(
  ## assault frequency model
  AF ~ multi_normal(mu,S),
  mu <- mu_a + z_a[family]*sigma_a + bPP[PI]*PP, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bPP[PI] ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
af <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
af_prec <- precis(af,3,pars=c("mu_a","bPP","sigma_a","sigma_phy"),prob=0.9) 
af_prec <- round(af_prec,2)
write.csv(af_prec,file="af_int_con_prec.csv")
af_post <- extract.samples(af)
af_post <- af_post[c(-1,-6)] # remove redundant elements
write.csv(af_post,file="af_int_con_post.csv",row.names=FALSE)


## put the variables in a list
d <- list(PP=as.numeric(data$PP_sd),
          PI=as.integer(data$PI) + 1,
          TR=as.integer(data$TR),
          family=as.integer(data$family),
          society=as.integer(society),
          N=N,
          R=R
)

## fit the model for trade
m_list <- alist(
  ## trade model
  TR ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + bPP[PI]*PP + phy[society],
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bPP[PI] ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
tr <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
tr_prec <- precis(tr,3,pars=c("mu_a","bPP","sigma_a","sigma_phy"),prob=0.9) 
tr_prec <- round(tr_prec,2)
write.csv(tr_prec,file="tr_int_con_prec.csv")
tr_post <- extract.samples(tr)
tr_post <- tr_post[c(-1:-2,-7:-9)] # remove redundant elements
write.csv(tr_post,file="tr_int_con_post.csv",row.names=FALSE)


## fit the model for military organization
soc <- mono_data$soc
mono_data_MM <- data.frame(cbind(soc,PP_sd,MM,PI,family)) # combine the variables
mono_data_MM <- mono_data_MM[complete.cases(mono_data_MM[,c(2,3)]),] # drop NAs (N=126)
mono_data_MM <- transform(mono_data_MM,family=as.numeric(factor(family)))
mono_tree_MM <- keep.tip(mono_tree,mono_data_MM$soc) # drop societies from the tree that are not in the sample
R_OU <- corMartins(1,phy=mono_tree_MM,form=~mono_data_MM$soc,fixed=FALSE) # adjust the tree according to the OU model but let the alpha be estimated rather than fixed
V <- vcv(R_OU) # convert to a variance-covariance matrix
V <- V[mono_data_MM$soc,mono_data_MM$soc] # order the societies
R_mm <- V/max(V) # convert to a correlation matrix
N_mm <- length(mono_data_MM$soc)
society_mm <- 1:N_mm # society index

## put the variables in a list
d <- list(PP=as.numeric(mono_data_MM$PP_sd),
          MM=as.integer(mono_data_MM$MM),
          PI=as.integer(mono_data_MM$PI) + 1,
          family=as.integer(mono_data_MM$family),
          society=as.integer(society_mm),
          N=N_mm,
          R=R_mm
)


m_list <- alist(
  ## assault frequency model
  MM ~ dbinom(1,p),
  logit(p) <- mu_a + z_a[family]*sigma_a + bPP[PI]*PP + phy[society],
  
  # z-scores
  z_a[family] ~ normal(0,1),
  
  # phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  mu_a ~ normal(0,1),
  bPP[PI] ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a
)

## fit the model with ulam
mm <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
mm_prec <- precis(mm,3,pars=c("mu_a","bPP","sigma_a","sigma_phy"),prob=0.9) 
mm_prec <- round(mm_prec,2)
write.csv(mm_prec,file="mm_int_con_prec.csv")
mm_post <- extract.samples(mm)
mm_post <- mm_post[c(-1:-2,-7:-9)] # remove redundant elements
write.csv(mm_post,file="mm_int_con_post.csv",row.names=FALSE)


################################################################################