#-------------------------------------------------
  # load data
#------------------------------------------------
ourdata1<-read.csv(file= "/Users/kathyliu/Documents/CAM-can/CombinedLavaandata.csv", header=T,  sep=",")
library(lavaan)
TR <-read.csv(file="/Users/kathyliu/merged_behav_data.csv")
#-------------------------------------------------
# two factor model 
#------------------------------------------------
#only emo reg, emo mem and response inhibition
a.two.factor.model <- '
NA_dep =~  EMoneg+EMvneg +EMpneg + ERneg+ERnegreac+SSRTb  
NA_indep =~ Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
NA_dep ~ meRLC
NA_indep ~ meRLC'
a.two.factor.model.fit <- cfa(a.two.factor.model, data=ourdata1, missing = "ML", estimator="mlr")
summary (a.two.factor.model.fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#add hotel
b.two.factor.model <- '
NA_dep =~  EMoneg+EMvneg +EMpneg + ERneg+ERnegreac+SSRTb  + Hoteltask_Time
NA_indep =~ Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
NA_dep ~ meRLC
NA_indep ~ meRLC'
b.two.factor.model.fit <- cfa(b.two.factor.model, data=ourdata1, missing = "ML", estimator="mlr")
summary (b.two.factor.model.fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#compare a vs b
anova(b.two.factor.model.fit, a.two.factor.model.fit)

#add sleep
c.two.factor.model <- '
NA_dep =~  EMoneg+EMvneg +EMpneg + ERneg+ERnegreac+SSRTb  + Hoteltask_Time+ PSQI_score
NA_indep =~ Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
NA_dep ~ meRLC
NA_indep ~ meRLC'
c.two.factor.model.fit <- cfa(c.two.factor.model, data=ourdata1, missing = "ML", estimator="mlr")
summary (c.two.factor.model.fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#compare c vs b
anova(c.two.factor.model.fit, b.two.factor.model.fit)

#add cognitive reserve
d.factor.model <- '
NA_dep =~  EMoneg+EMvneg +EMpneg + ERneg+ERnegreac+SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total  
NA_indep =~ Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
NA_dep ~ meRLC
NA_indep ~ meRLC'
d.factor.model.fit <- cfa(d.factor.model, data=ourdata1, missing = "ML", estimator="mlr")
summary (d.factor.model.fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#compare d vs c
anova(d.factor.model.fit, c.two.factor.model.fit)
#compare d vs a
anova(d.factor.model.fit, a.two.factor.model.fit)

#final two factor model
two.factor.model <- '
NA_dep =~  EMoneg+EMvneg +EMpneg + ERneg+ERnegreac+SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total   
NA_indep =~ Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
NA_dep ~ meRLC
NA_indep ~ meRLC'
two.factor.model.fit <- cfa(two.factor.model, data=ourdata1, missing = "ML", estimator="mlr")
summary (two.factor.model.fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

## compare fit to one factor model
one.factor.model<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg+ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ meRLC'
one.factor.fit<- cfa(one.factor.model, data=ourdata1, missing = "ML", estimator="mlr")
summary(one.factor.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

anova(two.factor.model.fit, one.factor.fit)

## multigroup two factor model #1 - does not converge
twofactor.fit.age <- cfa(two.factor.model, data=ourdata1, group="agecat", missing = "ML")
summary(twofactor.fit.age, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

##try grouping variable for two factor model with constraints
constr.factor.model <- '
NA_dep =~  NA*EMoneg+EMvneg +EMpneg + ERneg+ERnegreac+SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total   
NA_indep =~ NA*Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
NA_dep ~ meRLC
NA_indep ~ meRLC
NA_dep~~1*NA_dep
NA_indep~~1*NA_indep'

constr.model.fit<- cfa(constr.factor.model, data=ourdata1, group="agecat",group.equal = c("loadings", "intercepts"),missing = "ML",estimator='mlr')
summary (constr.model.fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

constr.factor.model2 <- '
NA_dep =~  NA*EMoneg+EMvneg +EMpneg + ERneg+ERnegreac+SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total   
NA_indep =~ NA*Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
NA_dep ~ meRLC
NA_indep ~ meRLC
NA_dep~~1*NA_dep
NA_indep~~1*NA_indep
NA_dep~~c(NA,1)*NA_indep'

constr.model.fit2<- cfa(constr.factor.model2, data=ourdata1, group="agecat",group.equal = c("loadings"), missing = "ML",estimator='mlr')
summary (constr.model.fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#-------------------------------------------------
# unidimensional NON age-adjusted multigroup model
#-------------------------------------------------
unconstrained.model.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*meRLC'

unconstrained.noage.fit<- cfa(unconstrained.model.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.noage.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*meRLC'

constrained.noage.fit<- cfa(constrained.model.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.noage.fit,unconstrained.noage.fit)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model
#-------------------------------------------------
unconstrained.model.uni<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*meRLC
factor~age_years
meRLC~age_years'
unconstrained.model.uni.fit<- cfa(unconstrained.model.uni, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.uni.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model
constrained.model.uni<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*meRLC
factor~age_years
meRLC~age_years'

constrained.model.uni.fit<- cfa(constrained.model.uni, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.uni.fit,unconstrained.model.uni.fit)

#-------------------------------------------------
# unidimensional NON age-adjusted multigroup model for ROSTRAL region
#-------------------------------------------------
unconstrained.rostral.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanhigh'

unconstrained.rostral.noage.fit<- cfa(unconstrained.rostral.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.rostral.noage.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.rostral.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*Rmeanhigh'

constrained.model.rostral.noage.fit<- cfa(constrained.model.rostral.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
anova(constrained.model.rostral.noage.fit,unconstrained.rostral.noage.fit)

#-------------------------------------------------
# unidimensional NON age-adjusted multigroup model for CAUDAL region
#-------------------------------------------------
unconstrained.caudal.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanlow'

unconstrained.caudal.noage.fit<- cfa(unconstrained.caudal.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.caudal.noage.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.caudal.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*Rmeanlow'

constrained.model.caudal.noage.fit<- cfa(constrained.model.caudal.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
anova(constrained.model.caudal.noage.fit,unconstrained.caudal.noage.fit)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model for ROSTRAL region
#-------------------------------------------------
unconstrained.model.rostral<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanhigh
factor~age_years
Rmeanhigh~age_years'

unconstrained.model.rostral.fit<- cfa(unconstrained.model.rostral, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.rostral.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.rostral<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*Rmeanhigh
factor~age_years
Rmeanhigh ~ age_years'

constrained.model.rostral.fit<- cfa(constrained.model.rostral, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.rostral.fit,unconstrained.model.rostral.fit)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model for CAUDAL region
#-------------------------------------------------
unconstrained.model.caudal<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanlow
factor~age_years
Rmeanlow~age_years'
unconstrained.model.caudal.fit<- cfa(unconstrained.model.caudal, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.caudal.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.caudal<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*Rmeanlow
factor~age_years
Rmeanlow ~ age_years'
constrained.model.caudal.fit<- cfa(constrained.model.caudal, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.caudal.fit,unconstrained.model.caudal.fit)

#-------------------------------------------------
# unidimensional multigroup model for PONS control region
#-------------------------------------------------
## age-adjusted model
unconstrained.pons<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*mePT
factor~age_years
mePT~age_years'
unconstrained.pons.fit<- cfa(unconstrained.pons, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

constrained.pons<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*mePT
factor~age_years
mePT~age_years'
constrained.pons.fit<- cfa(constrained.pons, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.pons.fit,unconstrained.pons.fit)

## NON age-adjusted model
unconstrained.pons.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect_young,LC_effect_old)*mePT'
unconstrained.pons.noage.fit<- cfa(unconstrained.pons.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

constrained.pons.noage<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam 
factor ~ c(LC_effect,LC_effect)*mePT'
constrained.pons.noage.fit<- cfa(constrained.pons.noage, data=ourdata1,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.pons.noage.fit,unconstrained.pons.noage.fit)

#========================================
# difference in correlation between rostral and caudal on cognitive factor in older adults
#========================================
#calculate correlation between rostral and caudal in older adults (r23)
olddata<-ourdata1[which(ourdata1$agecat=="older"),]
nrow(olddata) #n=269
cor.test(olddata$Rmeanhigh,olddata$Rmeanlow, method=c("pearson")) # r23=0.7942212, p<0.0001

#adj for age
install.packages("ppcor")
library(ppcor)
pcor.test(olddata$Rmeanhigh,olddata$Rmeanlow, olddata$age_years) # r23=0.793522, p<0.001

#calculate correlation rostral and caudal vs cognitive factor, r12=0.194, r13=0.112
library(psych)
#age adj model
r.test(n=269, r12=.194, r13=.112, r23=.794) #t=2.13, p<0.034
#non age adj model
r.test(n=269, r12=.261, r13=.160, r23=.794) #t=2.67, p<0.0081

#=======================================================================================
#Look at total brain volume (corrected for total intracranial volume TIV) and include in the model
TIV<- read.table("/Users/kathyliu/Documents/CAM-can/TIV.txt",header = TRUE)
head(TIV)
nrow(TIV)
head(ourdata1)
nrow(ourdata1)
# total brain volume (nBV) = (total white matter + total grey matter)/ TIV
TIV$nBV<-(TIV$GM+TIV$WM)/TIV$TIV
head(TIV)
TIV.id <- subset(TIV,  select=c("ident","nBV"))  
ourdata1TIV<-merge(ourdata1, TIV.id, by.x = "id", by.y = "ident", all.x=TRUE)
head(ourdata1TIV)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model with nBV
#-------------------------------------------------
unconstrained.model.uni.TIV<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect_young,LC_effect_old)*meRLC
factor~age_years
factor~nBV
meRLC~age_years
meRLC~nBV
nBV~age_years'
unconstrained.model.uni.fit.TIV<- cfa(unconstrained.model.uni.TIV, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.uni.fit.TIV,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model
constrained.model.uni.TIV<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect,LC_effect)*meRLC
factor~age_years
factor~nBV
meRLC~age_years
meRLC~nBV
nBV~age_years'

constrained.model.uni.fit.TIV<- cfa(constrained.model.uni.TIV, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.uni.fit.TIV,unconstrained.model.uni.fit.TIV)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model for ROSTRAL region with nBV
#-------------------------------------------------
unconstrained.model.rostral.TIV<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanhigh
factor~age_years
factor~nBV
Rmeanhigh~age_years
Rmeanhigh~nBV
nBV~age_years'

unconstrained.model.rostral.fit.TIV<- cfa(unconstrained.model.rostral.TIV, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.rostral.fit.TIV,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.rostral.TIV<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect,LC_effect)*Rmeanhigh
factor~age_years
factor~nBV
Rmeanhigh~age_years
Rmeanhigh~nBV
nBV~age_years'

constrained.model.rostral.fit.TIV<- cfa(constrained.model.rostral.TIV, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.rostral.fit.TIV,unconstrained.model.rostral.fit.TIV)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model for CAUDAL region with nBV
#-------------------------------------------------
unconstrained.model.caudal.TIV<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanlow
factor~age_years
factor~nBV
Rmeanlow~age_years
Rmeanlow~nBV
nBV~age_years'
unconstrained.model.caudal.fit.TIV<- cfa(unconstrained.model.caudal.TIV, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.caudal.fit.TIV,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.caudal.TIV<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect,LC_effect)*Rmeanlow
factor~age_years
factor~nBV
Rmeanlow~age_years
Rmeanlow~nBV
nBV~age_years'
constrained.model.caudal.fit.TIV<- cfa(constrained.model.caudal.TIV, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.caudal.fit.TIV,unconstrained.model.caudal.fit.TIV)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model with nBV ONLY
#-------------------------------------------------
TIV.model.uncon<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect_young,LC_effect_old)*nBV
factor~age_years
nBV~age_years'
uncon.fit.TIV<- cfa(TIV.model.uncon, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(uncon.fit.TIV,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model
TIV.model.con<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect,LC_effect)*nBV
factor~age_years
nBV~age_years'

con.fit.TIV<- cfa(TIV.model.con, data=ourdata1TIV,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(con.fit.TIV,uncon.fit.TIV)

#===============================================================
#add repetition time (TR) data N=599 as covariate of no interest.
nrow(TR)
head(TR)
TR$V2<-TR$V2/100
TR.id <- subset(TR,  select=c("id","V2"))  
ourdata1TR<-merge(ourdata1, TR.id, by = "id", all.x=TRUE)
nrow(ourdata1TR)
head(TR)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model with TR
#-------------------------------------------------
unconstrained.model.uni.TR<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect_young,LC_effect_old)*meRLC
factor~age_years
meRLC~V2
meRLC~age_years'
unconstrained.model.uni.fit.TR<- cfa(unconstrained.model.uni.TR, data=ourdata1TR,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.uni.fit.TR,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model
constrained.model.uni.TR<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect,LC_effect)*meRLC
factor~age_years
meRLC~V2
meRLC~age_years'

constrained.model.uni.fit.TR<- cfa(constrained.model.uni.TR, data=ourdata1TR,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.uni.fit.TR,unconstrained.model.uni.fit.TR)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model for ROSTRAL region with TR
#-------------------------------------------------
unconstrained.model.rostral.TR<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanhigh
factor~age_years
Rmeanhigh~V2
Rmeanhigh~age_years'

unconstrained.model.rostral.fit.TR<- cfa(unconstrained.model.rostral.TR, data=ourdata1TR,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.rostral.fit.TR,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.rostral.TR<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect,LC_effect)*Rmeanhigh
factor~age_years
Rmeanhigh~V2
Rmeanhigh~age_years'

constrained.model.rostral.fit.TR<- cfa(constrained.model.rostral.TR, data=ourdata1TR,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.rostral.fit.TR,unconstrained.model.rostral.fit.TR)

#-------------------------------------------------
# unidimensional age-adjusted multigroup model for CAUDAL region with TIV
#-------------------------------------------------
unconstrained.model.caudal.TR<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect_young,LC_effect_old)*Rmeanlow
factor~age_years
Rmeanlow~V2
Rmeanlow~age_years'
unconstrained.model.caudal.fit.TR<- cfa(unconstrained.model.caudal.TR, data=ourdata1TR,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')
summary(unconstrained.model.caudal.fit.TR,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

##compare regression paths between LC and factor in above model to a constrained model

constrained.model.caudal.TR<-'
factor=~ EMoneg+EMvneg +EMpneg+ERneg + ERnegreac +SSRTb+ Hoteltask_Time+ PSQI_score+education_age+Occ_score+STW_total+Cattell.totalscore+semambVsemunamb_pNo + Faces_FAMnam
factor ~ c(LC_effect,LC_effect)*Rmeanlow
factor~age_years
Rmeanlow~V2
Rmeanlow~age_years'
constrained.model.caudal.fit.TR<- cfa(constrained.model.caudal.TR, data=ourdata1TR,  group="agecat",group.equal = c("loadings"),missing = "ML", estimator='mlr')

anova(constrained.model.caudal.fit.TR,unconstrained.model.caudal.fit.TR)
