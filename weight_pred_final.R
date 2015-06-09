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
bmi<-read.csv("bmi.csv", as.is=TRUE)
# bmi <- read.sas7bdat('bmi.sas7bdat')
bmi <- unique(bmi)
bmi$id <- bmi$mom_nestid * 100000 + bmi$nestid
bmi <- bmi[bmi$X_bivwt==0,]

waz <- bmi[!is.na(bmi$weight),]
waz <- subset(waz, select=c(weight, agemos, id, ref_date, wt_source, sex, BABY_WEIGHT,
                            BMI_LMP_kgm2, mat_gest_wt_gain_category2, mat_ht_m, gest_weight_gain_kg,
                            GestAge_TotalDays, maternal_smoking2, parity_3cat,
                            mom_age_delv, race_final, education_final, GEST_DIABETES, PREGNANCY_HYPERTENSION))
waz <- unique(waz)
minmax <- ddply(waz, .(id), summarize, agemos_min=min(agemos), agemos_max=max(agemos))
minmax_keep <- minmax[minmax$agemos_min<=0.5 & minmax$agemos_max>=17,]
waz <- waz[waz$id %in% minmax_keep$id,]
waz <- waz[waz$agemos<=25,]
# mplot(agemos, weight, id, nwaz)
outliers <- velout(agemos, weight, id, waz, limit=0.5, linearise=TRUE)
nwaz <- zapvelout(outliers, icode=c(4,5,6,8))
# ag <- ddply(outliers, .(id), summarize, weight_mean=mean(weight), weight_sd=sd(weight))
# summary(ag)
# box_stats <- boxplot(ag$weight_sd, na.rm=TRUE, col="blue", plot=FALSE)
# keep_list <- ag[ag$weight_sd<2.5,]
# keep_list <- keep_list[!is.na(keep_list$weight_sd),]
# nwaz <- nwaz[nwaz$id %in% keep_list$id,]
nwaz <- nwaz[!is.na(nwaz$weight),]
waz_freq <- table(nwaz$id)
waz_keep <- data.frame(waz_freq[waz_freq>5])
nwaz <- nwaz[nwaz$id %in% rownames(waz_keep),]

# nwaz <- nwaz[!is.na(nwaz$race_final),]
# nwaz <- nwaz[nwaz$race_final!="Other",]
nwaz <- nwaz[nwaz$race_final!="",]

nwaz$weight <- log(nwaz$weight)

nwaz$education_final <- sub("/", "_", nwaz$education_final)

# factor works with integer. numberic will have NaN as a value that leads to the
nwaz$bmic5.f <- factor(cut(nwaz$BMI_LMP_kgm2, c(0, 18.5, 25, 30, 35, 40, 100), right=F))
contrasts(nwaz$bmic5.f) <- contr.treatment(6, base=2)
nwaz$educ.f <- factor(nwaz$education_final, exclude = "")
contrasts(nwaz$educ.f) <- contr.treatment(3, base=3)
nwaz$race.f <- factor(nwaz$race_final)
contrasts(nwaz$race.f) <- contr.treatment(4, base=4)
nwaz$parity.f <- factor(nwaz$parity_3cat)
nwaz$smoker.f <- factor(nwaz$maternal_smoking2)
nwaz$gest_dbt.f <- factor(nwaz$GEST_DIABETES)
contrasts(nwaz$gest_dbt.f) <- contr.treatment(2, base=2)
nwaz$preg_ht.f <- factor(nwaz$PREGNANCY_HYPERTENSION)
contrasts(nwaz$preg_ht.f) <- contr.treatment(2, base=2)
nwaz$mat_gest_wt_gain_c3.f <- factor(nwaz$mat_gest_wt_gain_category2)
contrasts(nwaz$mat_gest_wt_gain_c3.f) <- contr.treatment(3, base=2)

save(nwaz, file="nwaz.Rda")


fmla11 <- "~sex+GestAge_TotalDays+parity.f+mom_age_delv+race.f+educ.f+mat_ht_m"
fmla12 <- paste0(fmla11, "+gest_dbt.f+preg_ht.f")
fmla13 <- paste0(fmla12, "+smoker.f+bmic5.f+gest_weight_gain_kg")

m11 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla11), b.formula=as.formula(fmla11), c.formula=as.formula(fmla11))
m12 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla12), b.formula=as.formula(fmla12), c.formula=as.formula(fmla12))
m13 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla13), b.formula=as.formula(fmla13), c.formula=as.formula(fmla13))

m11.coef <- data.frame(summary(m11)$tTable)
save(m11.coef, file="m11coef.Rda")
save(m11, file="m11.Rda")

m12.coef <- data.frame(summary(m12)$tTable)
save(m12.coef, file="m12coef.Rda")
save(m12, file="m12.Rda")

m13.coef <- data.frame(summary(m13)$tTable)
save(m13.coef, file="m13coef.Rda")
save(m13, file="m13.Rda")

fmla21 <- "~sex+GestAge_TotalDays+parity.f+mom_age_delv+race.f+educ.f+mat_ht_m"
fmla22 <- paste0(fmla21, "+gest_dbt.f+preg_ht.f")
fmla23 <- paste0(fmla22, "+smoker.f+mat_gest_wt_gain_c3.f")

m21 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla21), b.formula=as.formula(fmla21), c.formula=as.formula(fmla21))
m22 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla22), b.formula=as.formula(fmla22), c.formula=as.formula(fmla22))
m23 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3,
             a.formula=as.formula(fmla23), b.formula=as.formula(fmla23), c.formula=as.formula(fmla23))

m21.coef <- data.frame(summary(m21)$tTable)
save(m21.coef, file="m21coef.Rda")
save(m21, file="m21.Rda")

m22.coef <- data.frame(summary(m22)$tTable)
save(m22.coef, file="m22coef.Rda")
save(m22, file="m22.Rda")

m23.coef <- data.frame(summary(m23)$tTable)
save(m23.coef, file="m23coef.Rda")
save(m23, file="m23.Rda")


rmarkdown::render('../weight_pred_results.Rmd')
