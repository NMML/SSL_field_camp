library(marked)

# simulate a single release cohort of 200 animals with 1 release
# and 10 recapture occasions; at least 2 unique ch are needed in simHMM
simd=data.frame(ch=c("H0000000000","HH000000000"),freq=c(100,100),stringsAsFactors=F)
# define simulation/fitting model; default for non-specified parameters is ~1
modelspec=list(p=list(formula=~stratum))
# process data with S,W strata; S=1 and W=2
sd=process.data(simd,model="hmmuMSCJS",strata.labels=c("H","A"))
## 2 capture histories collapsed into  2
# create design data
ddl=make.design.data(sd)
# set Psi transition for weaned to suckling to 0; fix is created automatically for
# Psi to fix one of the transitions to 1 for mlogit.
# default is to set Psi-xx to 1 (staying in same state)
ddl$Psi$fix[ddl$Psi$stratum=="A" & ddl$Psi$tostratum=="H"]=0
# set delta(W) to 0; non-fixed contain NA
ddl$delta$fix=ifelse(ddl$delta$stratum=="A",0,NA)
# set initial parameter values for model S=~1,p=~stratum,Psi=~1,delta=~1
initial=list(S=log(.95/.05),p=c(log(0.1/0.9),log(0.95/0.05)),delta=log(.8/.2),Psi=log(0.03/.97))
# call simmHMM to get a single realization
realization=simHMM(sd,ddl,model.parameters=modelspec,initial=initial)
# take that realization and process data and fix parameters in design data
sd=process.data(realization,model="hmmuMSCJS",strata.labels=c("H","A"))
## 200 capture histories collapsed into  174
rddl=make.design.data(sd)
rddl$Psi$fix[rddl$Psi$stratum=="A" & rddl$Psi$tostratum=="H"]=0
rddl$delta$fix=ifelse(rddl$delta$stratum=="A",0,NA)
# fit model
crm(sd,rddl,initial=initial,model.parameters=modelspec)
