library(plyr)
library(dplyr)
library(doBy)
library(reshape2)

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

nwaz$bmi_wg <- nwaz$gest_weight_gain_kg * nwaz$BMI_LMP_kgm2
nwaz$mat_gest_wt_gain_category2[is.na(nwaz$mat_gest_wt_gain_category2)] <- 0
nwaz$mat_gest_wt_gain_category2s <- as.character(nwaz$mat_gest_wt_gain_category2)
nwaz <- cbind(nwaz, model.matrix(~ mat_gest_wt_gain_category2s + 0, nwaz))
nwaz$bmi_wg1 <- nwaz$mat_gest_wt_gain_category2s1 * nwaz$BMI_LMP_kgm2
nwaz$bmi_wg3 <- nwaz$mat_gest_wt_gain_category2s3 * nwaz$BMI_LMP_kgm2

nwaz <- filter(nwaz, gest_weight_gain_kg < 50)
save(nwaz, file="nwaz.Rda")
