################################################################################
################################################################################


## re-fit the models but with intercepts and slopes varying by language family
## we'll re-parameterize the models for more efficient sampling
## fit the model for cultural group selection hypothesis of Henrich et al. (2012)
d <- list(MR=MR,
          TR=TR,
          PI=PI,
          family=family,
          society=society
)


## fit the model for the effect of trade
## write the model in a list
m_list <- alist(
  ## trade model
  TR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bMR + z_bMR[family]*sigma_bMR)*MR + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMR[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bMR ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMR ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMR <<- mu_bMR + z_bMR*sigma_bMR
)

## fit the model with ulam
tr_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
tr_vareff_prec <- precis(tr_vareff,3,pars=c("cutpoints","a","bMR","mu_a","mu_bMR","sigma_a","sigma_bMR","sigma_e")) 
tr_vareff_prec <- data.frame(round(tr_vareff_prec,2))
write.csv(data.frame(tr_vareff_prec), file="tr_vareff_prec.csv")
tr_vareff_post <- extract.samples(tr_vareff,pars=c("cutpoints","a","bMR","mu_a","mu_bMR","sigma_a","sigma_bMR","sigma_e")) 
write.csv(data.frame(tr_vareff_post), file="tr_vareff_post.csv")


## re-fit the model for the effect of trade but condition on political integration
## write the model in a list
m_list <- alist(
  ## trade model
  TR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bMR + z_bMR[family]*sigma_bMR)*MR + (mu_bPI + z_bPI[family]*sigma_bPI)*PI + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMR[family] ~ normal(0,1),
  z_bPI[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bMR ~ normal(0,0.5),
  mu_bPI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMR ~ exponential(1),
  sigma_bPI ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMR <<- mu_bMR + z_bMR*sigma_bMR,
  gq> vector[family]:bPI <<- mu_bPI + z_bPI*sigma_bPI
)

## fit the model with ulam
tr_pi_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
tr_pi_vareff_prec <- precis(tr_pi_vareff,3,pars=c("cutpoints","a","bMR","bPI","mu_a","mu_bMR","mu_bPI","sigma_a","sigma_bMR","sigma_bPI","sigma_e")) 
tr_pi_vareff_prec <- data.frame(round(tr_pi_vareff_prec,2))
write.csv(data.frame(tr_pi_vareff_prec), file="tr_pi_vareff_prec.csv")
tr_pi_vareff_post <- extract.samples(tr_pi_vareff,pars=c("cutpoints","a","bMR","bPI","mu_a","mu_bMR","mu_bPI","sigma_a","sigma_bMR","sigma_bPI","sigma_e")) 
write.csv(data.frame(tr_pi_vareff_post), file="tr_pi_vareff_post.csv")


## fit the model for the effect of military mobilization
d_mm <- list(MR=as.integer(mono_data_MM$MR),
             MM=as.integer(mono_data_MM$MM),
             PI=as.integer(mono_data_MM$PI),
             family=as.integer(mono_data_MM$family),
             society=as.integer(society_mm)
)


## write the model in a list
m_list <- alist(
  ## military mobilization model
  MM ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bMR + z_bMR[family]*sigma_bMR)*MR + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMR[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bMR ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMR ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMR <<- mu_bMR + z_bMR*sigma_bMR
)

## fit the model with ulam
mm_vareff <- ulam(m_list, data=d_mm, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mm_vareff_prec <- precis(mm_vareff,3,pars=c("cutpoints","a","bMR","mu_a","mu_bMR","sigma_a","sigma_bMR","sigma_e")) 
mm_vareff_prec <- data.frame(round(mm_vareff_prec,2))
write.csv(data.frame(mm_vareff_prec), file="mm_vareff_prec.csv")
mm_vareff_post <- extract.samples(mm_vareff,pars=c("cutpoints","a","bMR","mu_a","mu_bMR","sigma_a","sigma_bMR","sigma_e")) 
write.csv(data.frame(mm_vareff_post), file="mm_vareff_post.csv")


## re-fit the model for military mobilization but condition on political integration
m_list <- alist(
  ## standing armies model
  MM ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bMR + z_bMR[family]*sigma_bMR)*MR + (mu_bPI + z_bPI[family]*sigma_bPI)*PI + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMR[family] ~ normal(0,1),
  z_bPI[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bMR ~ normal(0,0.5),
  mu_bPI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMR ~ exponential(1),
  sigma_bPI ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMR <<- mu_bMR + z_bMR*sigma_bMR,
  gq> vector[family]:bPI <<- mu_bPI + z_bPI*sigma_bPI
)

## fit the model with ulam
mm_pi_vareff <- ulam(m_list, data=d_mm, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mm_pi_vareff_prec <- precis(mm_pi_vareff,3,pars=c("cutpoints","a","bMR","bPI","mu_a","mu_bMR","mu_bPI","sigma_a","sigma_bMR","sigma_bPI","sigma_e")) 
mm_pi_vareff_prec <- data.frame(round(mm_pi_vareff_prec,2))
write.csv(data.frame(mm_pi_vareff_prec), file="mm_pi_vareff_prec.csv")
mm_pi_vareff_post <- extract.samples(mm_pi_vareff,pars=c("cutpoints","a","bMR","bPI","mu_a","mu_bMR","mu_bPI","sigma_a","sigma_bMR","sigma_bPI","sigma_e")) 
write.csv(data.frame(mm_pi_vareff_post), file="mm_pi_vareff_post.csv")


## fit the model for the effect of male competition
d <- list(MR=MR,
          MC=MC,
          PI=PI,
          family=family
)


## write the model in a list
m_list <- alist(
  ## male competition model
  MC ~ normal(mu,sigma),
  mu <- mu_a + z_a[family]*sigma_a + (mu_bMR + z_bMR[family]*sigma_bMR)*MR, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMR[family] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bMR ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMR ~ exponential(1),
  sigma ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMR <<- mu_bMR + z_bMR*sigma_bMR
)

## fit the model with ulam
mc_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mc_vareff_prec <- precis(mc_vareff,3,pars=c("a","bMR","mu_a","mu_bMR","sigma_a","sigma_bMR","sigma")) 
mc_vareff_prec <- data.frame(round(mc_vareff_prec,2))
write.csv(data.frame(mc_vareff_prec), file="mc_vareff_prec.csv")
mc_vareff_post <- extract.samples(mc_vareff,pars=c("a","bMR","mu_a","mu_bMR","sigma_a","sigma_bMR","sigma")) 
write.csv(data.frame(mc_vareff_post), file="mc_vareff_post.csv")


## re-fit the model for male competition but condition on political integration
m_list <- alist(
  ## male competition model
  MC ~ normal(mu,sigma),
  mu <- mu_a + z_a[family]*sigma_a + (mu_bMR + z_bMR[family]*sigma_bMR)*MR + (mu_bPI + z_bPI[family]*sigma_bPI)*PI, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bMR[family] ~ normal(0,1),
  z_bPI[family] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bMR ~ normal(0,0.5),
  mu_bPI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bMR ~ exponential(1),
  sigma_bPI ~ exponential(1),
  sigma ~ exponential(1),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bMR <<- mu_bMR + z_bMR*sigma_bMR,
  gq> vector[family]:bPI <<- mu_bPI + z_bPI*sigma_bPI
)

## fit the model with ulam
mc_pi_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
mc_pi_vareff_prec <- precis(mc_pi_vareff,3,pars=c("a","bMR","bPI","mu_a","mu_bMR","mu_bPI","sigma_a","sigma_bMR","sigma_bPI","sigma")) 
mc_pi_vareff_prec <- data.frame(round(mc_pi_vareff_prec,2))
write.csv(data.frame(mc_pi_vareff_prec), file="mc_pi_vareff_prec.csv")
mc_pi_vareff_post <- extract.samples(mc_pi_vareff,pars=c("a","bMR","bPI","mu_a","mu_bMR","mu_bPI","sigma_a","sigma_bMR","sigma_bPI","sigma")) 
write.csv(data.frame(mc_pi_vareff_post), file="mc_pi_vareff_post.csv")


## fit the model based on the hypothesis of Betzig (1992)
d <- list(MR=MR,
          PS=PS,
          WI=WI,
          PI=PI,
          family=family,
          society=society
)


## write the model in a list
m_list <- alist(
  ## marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bPS + z_bPS[family]*sigma_bPS)*PS + (mu_bWI + z_bWI[family]*sigma_bWI)*WI + (mu_bPI + z_bPI[family]*sigma_bPI)*PI + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bPS[family] ~ normal(0,1),
  z_bWI[family] ~ normal(0,1),
  z_bPI[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bPS ~ normal(0,0.5),
  mu_bWI ~ normal(0,0.5),
  mu_bPI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bPS ~ exponential(1),
  sigma_bWI ~ exponential(1),
  sigma_bPI ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bPS <<- mu_bPS + z_bPS*sigma_bPS,
  gq> vector[family]:bWI <<- mu_bWI + z_bWI*sigma_bWI,
  gq> vector[family]:bPI <<- mu_bPI + z_bPI*sigma_bPI
)

## fit the model with ulam
ps_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
ps_vareff_prec <- precis(ps_vareff,2,pars=c("cutpoints","a","bPS","bWI","bPI","mu_a","mu_bPS","mu_bWI","mu_bPI","sigma_a","sigma_bPS","sigma_bWI","sigma_bPI","sigma_e"))
ps_vareff_prec <- data.frame(round(ps_vareff_prec,2))
write.csv(data.frame(ps_vareff_prec), file="ps_vareff_prec.csv")
ps_vareff_post <- extract.samples(ps_vareff,pars=c("cutpoints","a","bPS","bWI","bPI","mu_a","mu_bPS","mu_bWI","mu_bPI","sigma_a","sigma_bPS","sigma_bWI","sigma_bPI","sigma_e")) 
write.csv(data.frame(ps_vareff_post), file="ps_vareff_post.csv")


## fit the model for rival wealth hypothesis of Oh et al. (2017) and Ross et al. (2018)
d <- list(MR=MR,
          WI=WI,
          RW=RW,
          IA=IA,
          PD=PD,
          DC=DC,
          AL=AL,
          IC=IC,
          family=family,
          society=society
)


## fit the model for the effect of wealth inequality
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bWI + z_bWI[family]*sigma_bWI)*WI + (mu_bRW + z_bRW[family]*sigma_bRW)*RW + (mu_bIA + z_bIA[family]*sigma_bIA)*IA + z_e[society]*sigma_e, 
  
  # imputation model for rival wealth modeled as a function of subsistence
  RW ~ normal(mu,sigma),
  mu <- aRW + bIA_RW*IA,
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bWI[family] ~ normal(0,1),
  z_bRW[family] ~ normal(0,1),
  z_bIA[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  aRW ~ normal(0,1),
  bIA_RW ~ normal(0,0.5),
  mu_a ~ normal(0,1.5),
  mu_bWI ~ normal(0,0.5),
  mu_bRW ~ normal(0,0.5),
  mu_bIA ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bWI ~ exponential(1),
  sigma_bRW ~ exponential(1),
  sigma_bIA ~ exponential(1),
  sigma_e ~ exponential(1),
  sigma ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bWI <<- mu_bWI + z_bWI*sigma_bWI,
  gq> vector[family]:bRW <<- mu_bRW + z_bRW*sigma_bRW,
  gq> vector[family]:bIA <<- mu_bIA + z_bIA*sigma_bIA
)

## fit the model with ulam
wi_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
wi_vareff_prec <- precis(wi_vareff,2,pars=c("cutpoints","a","bWI","bRW","bIA","aRW","bIA_RW","mu_a","mu_bWI","mu_bRW","mu_bIA","sigma_a","sigma_bWI","sigma_bRW","sigma_bIA","sigma_e","sigma"))
wi_vareff_prec <- data.frame(round(wi_vareff_prec,2))
write.csv(data.frame(wi_vareff_prec), file="wi_vareff_prec.csv")
wi_vareff_post <- extract.samples(wi_vareff,pars=c("cutpoints","a","bWI","bRW","bIA","aRW","bIA_RW","mu_a","mu_bWI","mu_bRW","mu_bIA","sigma_a","sigma_bWI","sigma_bRW","sigma_bIA","sigma_e","sigma")) 
write.csv(data.frame(wi_vareff_post), file="wi_vareff_post.csv")


## fit the model for the effect of rival wealth
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bRW + z_bRW[family]*sigma_bRW)*RW + (mu_bIA + z_bIA[family]*sigma_bIA)*IA + (mu_bPD + z_bPD[family]*sigma_bPD)*PD + (mu_bDC + z_bDC[family]*sigma_bDC)*DC + (mu_bAL + z_bAL[family]*sigma_bAL)*AL + (mu_bIC + z_bIC[family]*sigma_bIC)*IC + z_e[society]*sigma_e, 
  
  # imputation model for rival wealth modeled as a function of subsistence
  RW ~ normal(mu,sigma),
  mu <- aRW + bIA_RW*IA + bPD_RW*PD + bDC_RW*DC + bAL_RW*AL + bIC_RW*IC,
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bRW[family] ~ normal(0,1),
  z_bIA[family] ~ normal(0,1),
  z_bPD[family] ~ normal(0,1),
  z_bDC[family] ~ normal(0,1),
  z_bAL[family] ~ normal(0,1),
  z_bIC[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bRW ~ normal(0,0.5),
  mu_bIA ~ normal(0,0.5),
  mu_bPD ~ normal(0,0.5),
  mu_bDC ~ normal(0,0.5),
  mu_bAL ~ normal(0,0.5),
  mu_bIC ~ normal(0,0.5),
  aRW ~ normal(0,1),
  bIA_RW ~ normal(0,0.5),
  bPD_RW ~ normal(0,0.5),
  bDC_RW ~ normal(0,0.5),
  bAL_RW ~ normal(0,0.5),
  bIC_RW ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bRW ~ exponential(1),
  sigma_bIA ~ exponential(1),
  sigma_bPD ~ exponential(1),
  sigma_bDC ~ exponential(1),
  sigma_bAL ~ exponential(1),
  sigma_bIC ~ exponential(1),
  sigma_e ~ exponential(1),
  sigma ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bRW <<- mu_bRW + z_bRW*sigma_bRW,
  gq> vector[family]:bIA <<- mu_bIA + z_bIA*sigma_bIA,
  gq> vector[family]:bPD <<- mu_bPD + z_bPD*sigma_bPD,
  gq> vector[family]:bDC <<- mu_bDC + z_bDC*sigma_bDC,
  gq> vector[family]:bAL <<- mu_bAL + z_bAL*sigma_bAL,
  gq> vector[family]:bIC <<- mu_bIC + z_bIC*sigma_bIC
)

## fit the model with ulam
rw_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
rw_vareff_prec <- precis(rw_vareff,2,pars=c("cutpoints","a","bRW","bIA","bPD","bDC","bAL","bIC","mu_a","mu_bRW","mu_bIA","mu_bPD","mu_bDC","mu_bAL","mu_bIC","aRW","bIA_RW","bPD_RW","bDC_RW","bAL_RW","bIC_RW","sigma_a","sigma_bRW","sigma_bIA","sigma_bPD","sigma_bDC","sigma_bAL","sigma_bIC","sigma_e","sigma"))
rw_vareff_prec <- data.frame(round(rw_vareff_prec,2))
write.csv(data.frame(rw_vareff_prec), file="rw_vareff_prec.csv")
rw_vareff_post <- extract.samples(rw_vareff,pars=c("cutpoints","a","bRW","bIA","bPD","bDC","bAL","bIC","mu_a","mu_bRW","mu_bIA","mu_bPD","mu_bDC","mu_bAL","mu_bIC","aRW","bIA_RW","bPD_RW","bDC_RW","bAL_RW","bIC_RW","sigma_a","sigma_bRW","sigma_bIA","sigma_bPD","sigma_bDC","sigma_bAL","sigma_bIC","sigma_e","sigma")) 
write.csv(data.frame(rw_vareff_post), file="rw_vareff_post.csv")


## fit the model for land shortage of Fortunato and Archetti (2010)
d <- list(MR=MR,
          PD=PD,
          DC=DC,
          AL=AL,
          IC=IC,
          IA=IA,
          family=family,
          society=society
)


## fit the model for the effect of population density
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bPD + z_bPD[family]*sigma_bPD)*PD + (mu_bIA + z_bIA[family]*sigma_bIA)*IA + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bPD[family] ~ normal(0,1),
  z_bIA[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bPD ~ normal(0,0.5),
  mu_bIA ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bPD ~ exponential(1),
  sigma_bIA ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bPD <<- mu_bPD + z_bPD*sigma_bPD,
  gq> vector[family]:bIA <<- mu_bIA + z_bIA*sigma_bIA
)

## fit the model with ulam
pd_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
pd_vareff_prec <- precis(pd_vareff,2,pars=c("cutpoints","a","bPD","bIA","mu_a","mu_bPD","mu_bIA","sigma_a","sigma_bPD","sigma_bIA","sigma_e")) 
pd_vareff_prec <- data.frame(round(pd_vareff_prec,2))
write.csv(data.frame(pd_vareff_prec), file="pd_vareff_prec.csv")
pd_vareff_post <- extract.samples(pd_vareff,pars=c("cutpoints","a","bPD","bIA","mu_a","mu_bPD","mu_bIA","sigma_a","sigma_bPD","sigma_bIA","sigma_e")) 
write.csv(data.frame(pd_vareff_post), file="pd_vareff_post.csv")


## fit the model for the effect of distance to coast
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bDC + z_bDC[family]*sigma_bDC)*DC + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bDC[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bDC ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bDC ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bDC <<- mu_bDC + z_bDC*sigma_bDC
)

## fit the model with ulam
dc_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
dc_vareff_prec <- precis(dc_vareff,2,pars=c("cutpoints","a","bDC","mu_a","mu_bDC","sigma_a","sigma_bDC","sigma_e")) 
dc_vareff_prec <- data.frame(round(dc_vareff_prec,2))
write.csv(data.frame(dc_vareff_prec), file="dc_vareff_prec.csv")
dc_vareff_post <- extract.samples(dc_vareff,pars=c("cutpoints","a","bDC","mu_a","mu_bDC","sigma_a","sigma_bDC","sigma_e")) 
write.csv(data.frame(dc_vareff_post), file="dc_vareff_post.csv")


## fit the model for the effect of altitude and incline
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bAL + z_bAL[family]*sigma_bAL)*AL + (mu_bIC + z_bIC[family]*sigma_bIC)*IC + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bAL[family] ~ normal(0,1),
  z_bIC[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bAL ~ normal(0,0.5),
  mu_bIC ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bAL ~ exponential(1),
  sigma_bIC ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bAL <<- mu_bAL + z_bAL*sigma_bAL,
  gq> vector[family]:bIC <<- mu_bIC + z_bIC*sigma_bIC
)

## fit the model with ulam
ai_vareff <- ulam(m_list, data=d, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
ai_vareff_prec <- precis(ai_vareff,2,pars=c("cutpoints","a","bAL","bIC","mu_a","mu_bAL","mu_bIC","sigma_a","sigma_bAL","sigma_bIC","sigma_e")) 
ai_vareff_prec <- data.frame(round(ai_vareff_prec,2))
write.csv(data.frame(ai_vareff_prec), file="ai_vareff_prec.csv")
ai_vareff_post <- extract.samples(ai_vareff,pars=c("cutpoints","a","bAL","bIC","mu_a","mu_bAL","mu_bIC","sigma_a","sigma_bAL","sigma_bIC","sigma_e")) 
write.csv(data.frame(ai_vareff_post), file="ai_vareff_post.csv")


## fit the model for "monogamous inheritance" of Fortunato and Archetti (2010)
d_mi <- list(MR=as.integer(mono_data_MI$MR),
             VI=as.integer(mono_data_MI$VI),
             VN=as.integer(mono_data_MI$VN),
             PD=as.integer(mono_data_MI$PD),
             DC=as.numeric(mono_data_MI$DC),
             AL=as.numeric(mono_data_MI$AL),
             IC=as.numeric(mono_data_MI$IC),
             family=as.integer(mono_data_MI$family),
             society=as.integer(society_mi)
)


## fit the model for the effect of vertical inheritance
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bVI + z_bVI[family]*sigma_bVI)*VI + (mu_bPD + z_bPD[family]*sigma_bPD)*PD + (mu_bDC + z_bDC[family]*sigma_bDC)*DC + (mu_bAL + z_bAL[family]*sigma_bAL)*AL + (mu_bIC + z_bIC[family]*sigma_bIC)*IC + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bVI[family] ~ normal(0,1),
  z_bPD[family] ~ normal(0,1),
  z_bDC[family] ~ normal(0,1),
  z_bAL[family] ~ normal(0,1),
  z_bIC[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bVI ~ normal(0,0.5),
  mu_bPD ~ normal(0,0.5),
  mu_bDC ~ normal(0,0.5),
  mu_bAL ~ normal(0,0.5),
  mu_bIC ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bVI ~ exponential(1),
  sigma_bPD ~ exponential(1),
  sigma_bDC ~ exponential(1),
  sigma_bAL ~ exponential(1),
  sigma_bIC ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bVI <<- mu_bVI + z_bVI*sigma_bVI,
  gq> vector[family]:bPD <<- mu_bPD + z_bPD*sigma_bPD,
  gq> vector[family]:bDC <<- mu_bDC + z_bDC*sigma_bDC,
  gq> vector[family]:bAL <<- mu_bAL + z_bAL*sigma_bAL,
  gq> vector[family]:bIC <<- mu_bIC + z_bIC*sigma_bIC
)

## fit the model with ulam
vi_vareff <- ulam(m_list, data=d_mi, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
vi_vareff_prec <- precis(vi_vareff,2,pars=c("cutpoints","a","bVI","bPD","bDC","bAL","bIC","mu_a","mu_bVI","mu_bPD","mu_bDC","mu_bAL","mu_bIC","sigma_a","sigma_bVI","sigma_bPD","sigma_bDC","sigma_bAL","sigma_bIC","sigma_e"))
vi_vareff_prec <- data.frame(round(vi_vareff_prec,2))
write.csv(data.frame(vi_vareff_prec), file="vi_vareff_prec.csv")
vi_vareff_post <- extract.samples(vi_vareff,pars=c("cutpoints","a","bVI","bPD","bDC","bAL","bIC","mu_a","mu_bVI","mu_bPD","mu_bDC","mu_bAL","mu_bIC","sigma_a","sigma_bVI","sigma_bPD","sigma_bDC","sigma_bAL","sigma_bIC","sigma_e")) 
write.csv(data.frame(vi_vareff_post), file="vi_vareff_post.csv")


## fit the model for the effect of virginity norms
## write the model in a list
m_list <- alist(
  # marriage rules model
  MR ~ ordered_logistic(phi,cutpoints),
  logit(phi) <- mu_a + z_a[family]*sigma_a + (mu_bVN + z_bVN[family]*sigma_bVN)*VN + (mu_bVI + z_bVI[family]*sigma_bVI)*VI + z_e[society]*sigma_e, 
  
  # z-scores
  z_a[family] ~ normal(0,1),
  z_bVN[family] ~ normal(0,1),
  z_bVI[family] ~ normal(0,1),
  z_e[society] ~ normal(0,1),
  
  # hyper-priors
  mu_a ~ normal(0,1.5),
  mu_bVN ~ normal(0,0.5),
  mu_bVI ~ normal(0,0.5),
  sigma_a ~ exponential(1),
  sigma_bVN ~ exponential(1),
  sigma_bVI ~ exponential(1),
  sigma_e ~ exponential(1),
  cutpoints ~ normal(0,1.5),
  
  ## generated quantities (reconstruct the parameters back)
  gq> vector[family]:a <<- mu_a + z_a*sigma_a,
  gq> vector[family]:bVN <<- mu_bVN + z_bVN*sigma_bVN,
  gq> vector[family]:bVI <<- mu_bVI + z_bVI*sigma_bVI
)

## fit the model with ulam
vn_vareff <- ulam(m_list, data=d_mi, chains=4, cores=4, iter=1000)


## get model summary and posterior samples
vn_vareff_prec <- precis(vn_vareff,2,pars=c("cutpoints","a","bVN","bVI","mu_a","mu_bVN","mu_bVI","sigma_a","sigma_bVN","sigma_bVI","sigma_e"))
vn_vareff_prec <- data.frame(round(vn_vareff_prec,2))
write.csv(data.frame(vn_vareff_prec), file="vn_vareff_prec.csv")
vn_vareff_post <- extract.samples(vn_vareff,pars=c("cutpoints","a","bVN","bVI","mu_a","mu_bVN","mu_bVI","sigma_a","sigma_bVN","sigma_bVI","sigma_e")) 
write.csv(data.frame(vn_vareff_post), file="vn_vareff_post.csv")

