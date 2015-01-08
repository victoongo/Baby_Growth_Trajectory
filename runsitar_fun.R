
runsitar <- function(fmla) {
  #   m11 <- sitar(x=agemos, y=yvar, id=id, data=na.omit(dat), df=3, fmla)
  m00 <- lm(as.formula(fmla), data=na.omit(nwaz))
  return(m00)
}
runsitar("weight~agemos")
summary(m00)

runsitar <- function(yvar,dat,fmla) {
#   m11 <- sitar(x=agemos, y=yvar, id=id, data=na.omit(dat), df=3, fmla)
  m00 <- lm(as.formula(fmla), data=na.omit(dat))
}
runsitar(weight,nwaz,"weight~agemos")


fmla <- as.formula(a.formula=~BMI_LMP_kgm2,b.formula=~BMI_LMP_kgm2,c.formula=~BMI_LMP_kgm2)
fmla <- as.formula(~BMI_LMP_kgm2)

m11 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3, a.formula=fmla)
runsitar <- function(afmla=1,bfmla=1,cfmla=1) {
    m00 <- sitar(x=agemos, y=weight, id=id, data=na.omit(nwaz), df=3, 
                 a.formula=afmla, b.formula=bfmla, c.formula=cfmla)
    print(summary(m00))
    return(m00)
}
runsitar(fmla,fmla,fmla)
for (x in c("bmic5.f","mat_gest_wt_gain_c3.f")) {
  fmla <- as.formula(paste("~",x,"+sex"))
  runsitar(fmla,fmla,fmla)
}

for (x in c("bmic5.f","mat_gest_wt_gain_c3.f")) {
  for (x1 in c("maternal_smoking2","smoker.f")) {
    fmla <- as.formula(paste("~",x,"+sex"))
    runsitar(fmla,fmla,fmla)
  }
}
