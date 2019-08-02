#-------------------------------------------------
  # load data
#------------------------------------------------
ourdata1<-read.csv(file= "/Users/kathyliu/Documents/CAM-can/CombinedLavaandata.csv", header=T,  sep=",")
library(lavaan)

#-------------------------------------------------
# two factor model 
#------------------------------------------------
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
