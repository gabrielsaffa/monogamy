################################################################################
################################################################################


## re-fit all the models but for Indo-Europeans only
mono_data <- read.csv("monogamy_NEW.csv",header=TRUE)
mono_data <- subset(mono_data,family_glottolog=="Indo-European") # subset Indo-Europeans only
soc <- mono_data$soc # society index
original <- c(1,2,3,4,5)
new <- c(5,4,3,2,1)
MR <- new[match(mono_data$normative_monogamy,original)] # marriage rules (scale inverted)
MC <- mono_data$assault_original # male competition
MC <- (MC - mean(MC,na.rm=TRUE))/(sd(MC,na.rm=TRUE)*2) # standardize male competition
PI <- mono_data$jurisdictional_hierarchy # political integration
TR <- mono_data$intercommunity_trade # inter-community trade
TR <- ifelse(TR==7,6,TR)
MM <- mono_data$military_mobilization # military mobilization
original <- c(1,2,3)
new <- c(3,2,1)
MM <- new[match(MM,original)] # invert the scale
PS <- ifelse(mono_data$political_succession==7,1,0) # patrilineal succession
WI <- mono_data$class_differentiation # wealth inequality
mono_data$land_ownership_Kavanagh <- mono_data$land_ownership_Kavanagh + 1 
original <- c(1,2,3)
new <- c(3,2,1)
mono_data$land_communality <- new[match(mono_data$land_communality,original)] # invert the scale
RW <- rowMeans(mono_data[,13:16],na.rm=TRUE) # create a rival wealth proxy by averaging land ownership variables for each society
RW <- (RW - mean(RW,na.rm=TRUE))/(sd(RW,na.rm=TRUE)*2) # standardize rival wealth
VI <- mono_data$land_inheritance
VI <- ifelse(VI==4,1,ifelse(VI==5,1,ifelse(VI==7,1,0))) # vertical inheritance
VN <- ifelse(mono_data$virginity_norms==2,1,0) # create a binary indicator for virginity norms
IA <- mono_data$agriculture_intensity # intensity of agriculture
PD <- mono_data$population_density # population density
DC <- mono_data$distance_coast_km # distance to coast
DC <- (DC - mean(DC,na.rm=TRUE))/(sd(DC,na.rm=TRUE)*2) # standardize distance to coast
AL <- mono_data$altitude_m # altitude
AL <- (AL - mean(AL,na.rm=TRUE))/(sd(AL,na.rm=TRUE)*2) # standardize altitude
IC <- mono_data$incline_degrees # incline
IC <- (IC - mean(IC,na.rm=TRUE))/(sd(IC,na.rm=TRUE)*2) # standardize incline


## we'll use phylogenetic information and information on ethnographic present to account for potential confounding due to common ancestry and similar time foci among societies 
mono_tree <- read.tree("SPT.SCCS.tre") # read the tree
mono_tree <- keep.tip(mono_tree,soc) # drop four ancient societies
D <- cophenetic(mono_tree) # compute distance matrix
D <- D[soc,soc]/max(D) # normalize the distances to scale from 0 to 1
year <- mono_data$focal_year
E <- as.matrix(dist(year,method="euclidean"),nrow=length(soc),ncol=length(soc)) # compute a matrix of ethnographic distances
E <- E/max(E) # normalize the distances to scale from 0 to 1
N <- length(soc)
society <- 1:N


## model fitting
## because our measure of marriage rules is a 5-point ordinal scale, we'll use ordered logit model
## fit the model for cultural group selection hypothesis of Henrich et al. (2012)
## i.e. the total effect of monogamy on trade, standing armies and male competition, respectively
## compile the data in a list
d <- list(MR=MR,
          TR=TR,
          PI=PI,
          D=D,
          E=E,
          N=N,
          society=society
)


## fit the model for trade
## write the model in a list
m_list <- alist(
  # trade model
  TR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bMR*MR + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bMR ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
tr <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
tr_prec <- precis(tr,2,pars=c("cutpoints","bMR","eta_phy","eta_ep","rho_phy","rho_ep")) 
tr_prec <- data.frame(round(tr_prec,2))
write.csv(data.frame(tr_prec), file="tr_IE_prec.csv")
tr_post <- extract.samples(tr,pars=c("cutpoints","bMR","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(tr_post), file="tr_IE_post.csv")


## re-fit the model for trade but condition on political integration
## write the model in a list
m_list <- alist(
  # trade model
  TR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bMR*MR + bPI*PI + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bMR ~ normal(0,0.5),
  bPI ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
tr_pi <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
tr_pi_prec <- precis(tr_pi,2,pars=c("cutpoints","bMR","bPI","eta_phy","eta_ep","rho_phy","rho_ep")) 
tr_pi_prec <- data.frame(round(tr_pi_prec,2))
write.csv(data.frame(tr_pi_prec), file="tr_pi_IE_prec.csv")
tr_pi_post <- extract.samples(tr_pi,pars=c("cutpoints","bMR","bPI","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(tr_pi_post), file="tr_pi_IE_post.csv")


## fit the model for the effect of standing armies
## because Stan imputes missing values for continuous variables only, we will do a complete-case analysis
mono_data_MM <- data.frame(cbind(soc,year,MR,MM,PI,mono_data$family_glottolog)) # combine the variables
mono_data_MM <- mono_data_MM[complete.cases(mono_data_MM),] # drop NAs
mono_data_MM <- transform(mono_data_MM, family=as.numeric(factor(V6)))
mono_tree_MM <- keep.tip(mono_tree,mono_data_MM$soc) # drop societies from the tree that are not in the sample
D_mm <- cophenetic(mono_tree_MM) # compute distance matrix
D_mm <- D_mm[mono_data_MM$soc,mono_data_MM$soc]/max(D_mm) # normalize the distances to scale from 0 to 1
E_mm <- as.matrix(dist(mono_data_MM$year,method="euclidean"),nrow=length(mono_data_MM$soc),ncol=length(mono_data_MM$soc)) # compute a matrix of ethnographic distances
E_mm <- E_mm/max(E_mm) # normalize the distances to scale from 0 to 1
N_mm <- length(mono_data_MM$soc)
society_mm <- 1:N_mm


## compile the data in a list
d_mm <- list(MR=as.integer(mono_data_MM$MR),
             MM=as.integer(mono_data_MM$MM),
             PI=as.integer(mono_data_MM$PI),
             D_mm=D_mm,
             E_mm=E_mm,
             N=N_mm,
             society=society_mm
)


## fit the model for military mobilization
m_list <- alist(
  # trade model
  MM ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bMR*MR + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D_mm,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E_mm,eta_ep,rho_ep,0.01),
  
  # priors
  bMR ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
mm <- ulam(m_list, data=d_mm, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mm_prec <- precis(mm,2,pars=c("cutpoints","bMR","eta_phy","eta_ep","rho_phy","rho_ep")) 
mm_prec <- data.frame(round(mm_prec,2))
write.csv(data.frame(mm_prec), file="mm_IE_prec.csv")
mm_post <- extract.samples(mm,pars=c("cutpoints","bMR","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(mm_post), file="mm_IE_post.csv")


## re-fit the model for military mobilization but condition on political integration
m_list <- alist(
  # trade model
  MM ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bMR*MR + bPI*PI + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D_mm,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E_mm,eta_ep,rho_ep,0.01),
  
  # priors
  bMR ~ normal(0,0.5),
  bPI ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
mm_pi <- ulam(m_list, data=d_mm, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mm_pi_prec <- precis(mm_pi,2,pars=c("cutpoints","bMR","bPI","eta_phy","eta_ep","rho_phy","rho_ep")) 
mm_pi_prec <- data.frame(round(mm_pi_prec,2))
write.csv(data.frame(mm_pi_prec), file="mm_pi_IE_prec.csv")
mm_pi_post <- extract.samples(mm_pi,pars=c("cutpoints","bMR","bPI","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(mm_pi_post), file="mm_pi_IE_post.csv")


## fit the model for the effect of male competition
## compile the data in a list
d <- list(MR=MR,
          MC=MC,
          PI=PI,
          D=D,
          E=E,
          N=N,
          society=society
)


## write the model in a list
m_list <- alist(
  # male competition model
  MC ~ normal(mu,sigma),
  mu <- a + bMR*MR + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  a ~ normal(0,1),
  bMR ~ normal(0,0.5),
  sigma ~ exponential(1),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
mc <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mc_prec <- precis(mc,2,pars=c("a","bMR","sigma","eta_phy","eta_ep","rho_phy","rho_ep")) 
mc_prec <- data.frame(round(mc_prec,2))
write.csv(data.frame(mc_prec), file="mc_IE_prec.csv")
mc_post <- extract.samples(mc,pars=c("a","bMR","sigma","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(mc_post), file="mc_IE_post.csv")


## re-fit the model for male competition but condition on political integration
m_list <- alist(
  # male competition model
  MC ~ normal(mu,sigma),
  mu <- a + bMR*MR + bPI*PI + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  a ~ normal(0,1),
  bMR ~ normal(0,0.5),
  bPI ~ normal(0,0.5),
  sigma ~ exponential(1),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
mc_pi <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mc_pi_prec <- precis(mc_pi,2,pars=c("a","bMR","bPI","sigma","eta_phy","eta_ep","rho_phy","rho_ep")) 
mc_pi_prec <- data.frame(round(mc_pi_prec,2))
write.csv(data.frame(mc_pi_prec), file="mc_pi_IE_prec.csv")
mc_pi_post <- extract.samples(mc_pi,pars=c("a","bMR","bPI","sigma","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(mc_pi_post), file="mc_pi_IE_post.csv")


## fit the model based on the hypothesis of Betzig (1992)
## i.e. the total effect of patrilineal succession on monogamy
## compile the data in a list
d <- list(MR=MR,
          PS=PS,
          WI=WI,
          PI=PI,
          D=D,
          E=E,
          N=N,
          society=society
)


## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bPS*PS + bWI*WI + bPI*PI + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bPS ~ normal(0,0.5),
  bWI ~ normal(0,0.5),
  bPI ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
ps <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
ps_prec <- precis(ps,2,pars=c("cutpoints","bPS","bWI","bPI","eta_phy","eta_ep","rho_phy","rho_ep"))
ps_prec <- data.frame(round(ps_prec,2))
write.csv(data.frame(ps_prec), file="ps_IE_prec.csv")
ps_post <- extract.samples(ps,pars=c("cutpoints","bPS","bWI","bPI","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(ps_post), file="ps_IE_post.csv")


## fit the model for rival wealth hypothesis of Oh et al. (2017) and Ross et al. (2018)
## i.e. the total effect of wealth inequality and rival wealth on monogamy, respectively
## compile the data in a list
d <- list(MR=MR,
          WI=WI,
          RW=RW,
          IA=IA,
          PD=PD,
          DC=DC,
          AL=AL,
          IC=IC,
          D=D,
          E=E,
          N=N,
          society=society
)


## fit the model for the effect of wealth inequality
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bWI*WI + bRW*RW + bIA*IA + phy[society] + ep[society],
  
  # imputation model for rival wealth modeled as a function of subsistence
  RW ~ normal(mu,sigma),
  mu <- aRW + bIA_RW*IA + bPD_RW*PD + bDC_RW*DC + bAL_RW*AL + bIC_RW*IC,
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bWI ~ normal(0,0.5),
  bRW ~ normal(0,0.5),
  bIA ~ normal(0,0.5),
  aRW ~ normal(0,1),
  bIA_RW ~ normal(0,0.5),
  bPD_RW ~ normal(0,0.5),
  bDC_RW ~ normal(0,0.5),
  bAL_RW ~ normal(0,0.5),
  bIC_RW ~ normal(0,0.5),
  sigma ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
wi <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
wi_prec <- precis(wi,2,pars=c("cutpoints","bWI","bRW","bIA","aRW","bIA_RW","eta_phy","eta_ep","rho_phy","rho_ep"))
wi_prec <- data.frame(round(wi_prec,2))
write.csv(data.frame(wi_prec), file="wi_IE_prec.csv")
wi_post <- extract.samples(wi,pars=c("cutpoints","bWI","bRW","bIA","aRW","bIA_RW","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(wi_post), file="wi_IE_post.csv")


## fit the model for the effect of rival wealth
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bRW*RW + bIA*IA + bPD*PD + bDC*DC + bAL*AL + bIC*IC + phy[society] + ep[society],
  
  # imputation model for rival wealth modeled as a function of subsistence
  RW ~ normal(mu,sigma),
  mu <- aRW + bIA_RW*IA + bPD_RW*PD + bDC_RW*DC + bAL_RW*AL + bIC_RW*IC,
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bRW ~ normal(0,0.5),
  bIA ~ normal(0,0.5),
  bPD ~ normal(0,0.5),
  bDC ~ normal(0,0.5),
  bAL ~ normal(0,0.5),
  bIC ~ normal(0,0.5),
  aRW ~ normal(0,1),
  bIA_RW ~ normal(0,0.5),
  bPD_RW ~ normal(0,0.5),
  bDC_RW ~ normal(0,0.5),
  bAL_RW ~ normal(0,0.5),
  bIC_RW ~ normal(0,0.5),
  sigma ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
rw <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
rw_prec <- precis(rw,2,pars=c("cutpoints","bRW","bIA","bPD","bDC","bAL","bIC","sigma","eta_phy","eta_ep","rho_phy","rho_ep"))
rw_prec <- data.frame(round(rw_prec,2))
write.csv(data.frame(rw_prec), file="rw_IE_prec.csv")
rw_post <- extract.samples(rw,pars=c("cutpoints","bRW","bIA","bPD","bDC","bAL","bIC","sigma","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(rw_post), file="rw_IE_post.csv")


## fit the model for land shortage of Fortunato and Archetti (2010)
## i.e. the total effect of 
d <- list(MR=MR,
          PD=PD,
          DC=DC,
          AL=AL,
          IC=IC,
          IA=IA,
          D=D,
          E=E,
          N=N,
          society=society
)


## fit the model for the effect of population density
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bPD*PD + bIA*IA + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bPD ~ normal(0,0.5),
  bIA ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
pd <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
pd_prec <- precis(pd,2,pars=c("cutpoints","bPD","bIA","eta_phy","eta_ep","rho_phy","rho_ep")) 
pd_prec <- data.frame(round(pd_prec,2))
write.csv(data.frame(pd_prec), file="pd_IE_prec.csv")
pd_post <- extract.samples(pd,pars=c("cutpoints","bPD","bIA","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(pd_post), file="pd_IE_post.csv")


## fit the model for the effect of distance to coast
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bDC*DC + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bDC ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
dc <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
dc_prec <- precis(dc,2,pars=c("cutpoints","bDC","eta_phy","eta_ep","rho_phy","rho_ep")) 
dc_prec <- data.frame(round(dc_prec,2))
write.csv(data.frame(dc_prec), file="dc_IE_prec.csv")
dc_post <- extract.samples(dc,pars=c("cutpoints","bDC","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(dc_post), file="dc_IE_post.csv")


## fit the model for the effect of altitude and incline
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bAL*AL + bIC*IC + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bAL ~ normal(0,0.5),
  bIC ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
ai <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
ai_prec <- precis(ai,2,pars=c("cutpoints","bAL","bIC","eta_phy","eta_ep","rho_phy","rho_ep")) 
ai_prec <- data.frame(round(ai_prec,2))
write.csv(data.frame(ai_prec), file="ai_IE_prec.csv")
ai_post <- extract.samples(ai,pars=c("cutpoints","bAL","bIC","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(ai_post), file="ai_IE_post.csv")



## fit the model for "monogamous inheritance" of Fortunato and Archetti (2010)
## i.e. total effect of vertical inheritance and virginity norms on monogamy, respectively
## create a complete-case data set again
mono_data_MI <- data.frame(cbind(soc,year,MR,VI,VN,PD,DC,AL,IC,mono_data$family_glottolog)) # combine the variables
mono_data_MI <- mono_data_MI[complete.cases(mono_data_MI),] # drop NAs
mono_data_MI <- transform(mono_data_MI, family=as.numeric(factor(V10)))
mono_tree_MI <- keep.tip(mono_tree,mono_data_MI$soc) # drop societies from the tree that are not in the sample
D_mi <- cophenetic(mono_tree_MI) # compute distance matrix
D_mi <- D_mi[mono_data_MI$soc,mono_data_MI$soc]/max(D_mi) # normalize the distances to scale from 0 to 1
E_mi <- as.matrix(dist(mono_data_MI$year,method="euclidean"),nrow=length(mono_data_MI$soc),ncol=length(mono_data_MI$soc)) # compute a matrix of ethnographic distances
E_mi <- E_mi/max(E_mi) # normalize the distances to scale from 0 to 1
N_mi <- length(mono_data_MI$soc)
society_mi <- 1:N_mi

## compile the data in a list
d_mi <- list(MR=as.integer(mono_data_MI$MR),
             VI=as.integer(mono_data_MI$VI),
             VN=as.integer(mono_data_MI$VN),
             PD=as.integer(mono_data_MI$PD),
             DC=as.numeric(mono_data_MI$DC),
             AL=as.numeric(mono_data_MI$AL),
             IC=as.numeric(mono_data_MI$IC),
             D=D_mi,
             E=E_mi,
             N=N_mi,
             society=society_mi
)


## fit the model for the effect of vertical inheritance
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bVI*VI + bPD*PD + bDC*DC + bAL*AL + bIC*IC + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bVI ~ normal(0,0.5),
  bPD ~ normal(0,0.5),
  bDC ~ normal(0,0.5),
  bAL ~ normal(0,0.5),
  bIC ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
vi <- ulam(m_list, data=d_mi, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
vi_prec <- precis(vi,2,pars=c("cutpoints","bVI","bPD","bDC","bAL","bIC","eta_phy","eta_ep","rho_phy","rho_ep"))
vi_prec <- data.frame(round(vi_prec,2))
write.csv(data.frame(vi_prec), file="vi_IE_prec.csv")
vi_post <- extract.samples(vi,pars=c("cutpoints","bVI","bPD","bDC","bAL","bIC","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(vi_post), file="vi_IE_post.csv")


## fit the model for the effect of virginity norms
## write the model in a list 
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- bVN*VN + bVI*VI + phy[society] + ep[society],
  
  # Gaussian process model for the phylogenetic covariance
  transpars> vector[N]:phy <<- L_SIGMA_p*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_p <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(D,eta_phy,rho_phy,0.01),
  
  # Gaussian process model for the ethnographic covariance
  transpars> vector[N]:ep <<- L_SIGMA_e*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA_e <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- cov_GPL1(E,eta_ep,rho_ep,0.01),
  
  # priors
  bVN ~ normal(0,0.5),
  bVI ~ normal(0,0.5),
  cutpoints ~ normal(0,1.5),
  c(eta_phy,eta_ep) ~ exponential(0.5),
  c(rho_phy,rho_ep) ~ exponential(0.5)
)

## fit the model with ulam
vn <- ulam(m_list, data=d_mi, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
vn_prec <- precis(vn,2,pars=c("cutpoints","bVN","bVI","eta_phy","eta_ep","rho_phy","rho_ep"))
vn_prec <- data.frame(round(vn_prec,2))
write.csv(data.frame(vn_prec), file="vn_IE_prec.csv")
vn_post <- extract.samples(vn,pars=c("cutpoints","bVN","bVI","phy","ep","eta_phy","eta_ep","rho_phy","rho_ep")) 
write.csv(data.frame(vn_post), file="vn_IE_post.csv")


################################################################################
################################################################################