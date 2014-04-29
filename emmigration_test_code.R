library(marked)
simd=data.frame(ch=c("H0000000000","HH000000000"),freq=c(100,100),stringsAsFactors=F)
modelspec = list(p=list(formula=~stratum))
sd = process.data(simd, model="hmmuMSCJS", strata.labels=c("H","A"))
ddl = make.design.data(sd)
ddl$Psi$fix[ddl$Psi$stratum=="A" & ddl$Psi$tostratum=="H"]=0
ddl$delta$fix=ifelse(ddl$delta$stratum=="A",0,1)
initial=list(S=log(.95/.05),p=c(log(0.1/0.9),log(0.95/0.05)),Psi=log(0.03/.97))
realization=simHMM(sd,ddl,model.parameters=modelspec,initial=initial)
sd=process.data(realization,model="hmmuMSCJS",strata.labels=c("H","A"))
rddl=make.design.data(sd)
rddl$Psi$fix[rddl$Psi$stratum=="A" & rddl$Psi$tostratum=="H"]=0
rddl$delta$fix=ifelse(rddl$delta$stratum=="A",0,1)
crm(sd,rddl,initial=initial,model.parameters=modelspec)

getwd()