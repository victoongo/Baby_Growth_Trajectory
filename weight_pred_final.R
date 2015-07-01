library(sitar)
library(ggplot2)
library(plyr)
library(nlme)
library(doBy)
library(reshape2)
library(xtable)
library(rmarkdown)
# library(sas7bdat)

setwd("~/Dropbox/Projects/baby_growth/data")
load(file="nwaz.Rda")

fmla11 <- "~sex+GestAge_TotalDays+parity.f+mom_age_delv+race.f+educ.f+mat_ht_m"
fmla12 <- paste0(fmla11, "+gest_dbt.f+preg_ht.f")
fmla13 <- paste0(fmla12, "+smoker.f+bmic5.f+gest_weight_gain_kg")
# fmla13i <- paste0(fmla12, "+smoker.f+BMI_LMP_kgm2+gest_weight_gain_kg+bmi_wg")

m11 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla11), b.formula=as.formula(fmla11), c.formula=as.formula(fmla11))
m12 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla12), b.formula=as.formula(fmla12), c.formula=as.formula(fmla12))
m13 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla13), b.formula=as.formula(fmla13), c.formula=as.formula(fmla13))
# m13i <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
#              a.formula=as.formula(fmla13i), b.formula=as.formula(fmla13i), c.formula=as.formula(fmla13i))


m11.coef <- data.frame(summary(m11)$tTable)
save(m11.coef, file="m11coef.Rda")
save(m11, file="m11.Rda")

m12.coef <- data.frame(summary(m12)$tTable)
save(m12.coef, file="m12coef.Rda")
save(m12, file="m12.Rda")

m13.coef <- data.frame(summary(m13)$tTable)
save(m13.coef, file="m13coef.Rda")
save(m13, file="m13.Rda")

# m13i.coef <- data.frame(summary(m13i)$tTable)
# save(m13i.coef, file="m13icoef.Rda")
# save(m13i, file="m13i.Rda")

fmla21 <- "~sex+GestAge_TotalDays+parity.f+mom_age_delv+race.f+educ.f+mat_ht_m"
fmla22 <- paste0(fmla21, "+gest_dbt.f+preg_ht.f")
fmla23 <- paste0(fmla22, "+smoker.f+mat_gest_wt_gain_c3.f+BMI_LMP_kgm2")
# fmla23i <- paste0(fmla22, "+smoker.f+mat_gest_wt_gain_c3.f+BMI_LMP_kgm2+bmi_wg1+bmi_wg3")

m21 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla21), b.formula=as.formula(fmla21), c.formula=as.formula(fmla21))
m22 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla22), b.formula=as.formula(fmla22), c.formula=as.formula(fmla22))
m23 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla23), b.formula=as.formula(fmla23), c.formula=as.formula(fmla23))
# m23i <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
#              a.formula=as.formula(fmla23i), b.formula=as.formula(fmla23i), c.formula=as.formula(fmla23i))

m21.coef <- data.frame(summary(m21)$tTable)
save(m21.coef, file="m21coef.Rda")
save(m21, file="m21.Rda")

m22.coef <- data.frame(summary(m22)$tTable)
save(m22.coef, file="m22coef.Rda")
save(m22, file="m22.Rda")

m23.coef <- data.frame(summary(m23)$tTable)
save(m23.coef, file="m23coef.Rda")
save(m23, file="m23.Rda")
write.csv(m23.coef, file="m23coef.csv")

# m23i.coef <- data.frame(summary(m23i)$tTable)
# save(m23i.coef, file="m23icoef.Rda")
# save(m23i, file="m23i.Rda")
# write.csv(m23i.coef, file="m23icoef.csv")

rmarkdown::render('../weight_pred_results.Rmd')
