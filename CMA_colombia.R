---
  title: "Causal Mediation Analysis: SHARP training program"
date: "7/12/2021"
output:
  pdf_document: default
html_document: default
---
  
  ```{r message=F,warning=F}

######################################################
##   Mediation Analysis Course - R Application      ##
##   Lung cancer example                            ##
######################################################


## load in data 
data_sens <- read.csv("sim_data_lung.csv")

######################################
### PART I ######################
### traditional mediation analysis ###
######################################


## Total effect - Adjusting confounders (sex + colgrad + age)
TE.fit <- glm(case ~ snp + sex + colgrad + age, family = binomial, data=data_sens)
summary(TE.fit)
## TE = 0.447
## OR(TE) = exp(0.447) = 1.564

## Direct effect - Adjusting confounders (sex + colgrad + age) and possibly mediator (smoking)
out.fit.noint <- glm(case ~ snp + smoking + sex + colgrad + age, 
                     family = binomial, data=data_sens)
summary(out.fit.noint)

## use difference method to estimate the direct and indirect effects 
## TE = 0.447
## DE = 0.387
## IE = 0.447 - 0.387 = 0.060 (using difference method)
## PM = IE / TE = 0.134 
## OR(DE) = exp(0.387) = 1.473
## OR(IE) = exp(0.060) = 1.062
## PM = (OR(DE)*(OR(IE)-1))/(OR(DE)*OR(IE)-1) = 0.162

## mediation model: Check how the exposure is related to suspected 
# mediator variable
med.fit <- glm(smoking ~ snp + sex + colgrad + age, family = binomial, data=data_sens)
summary(out.fit.noint)
summary(med.fit)

## use product method to estimate the direct and indirect effects 
## DE = 0.387
## IE = 0.352 * 1.605 = 0.565
## Not possible (all effects in same direction but IE >TE!)
## Issue of collapsibility 
## Issue of case-control study

## fit mediation model among controls
med.fit.con <- glm(smoking ~ snp + sex + colgrad + age, family = binomial, 
                   data=data_sens[which(data_sens$case == 0),])
summary(out.fit.noint)
summary(med.fit.con)

## IE = 0.095 * 1.605 = 0.152
## Note: 0.152 + 0.387 = 0.539 > TE
## In addition to the critical issue of non-collapsibility and 
## complications due to case-control study designthat these approaches 
## do not accommodate, there is the issue of model mis-specification.


## fit outcome model WITH interaction
out.fit <- glm(case ~ snp * smoking + sex + colgrad + age, family = binomial, data=data_sens)
summary(out.fit)
## p-value for interaction = 0.054
## use R directly
confint(out.fit)
## or calculate this by "hand"
c(0.352 - 1.96 * 0.183, 0.352 + 1.96 * 0.183)
### 95% CI is (-0.006, 0.710)
### Interaction on the OR scale = 1.422 CI: (0.994, 2.033) 

```

```{r fig.width=8,fig.height=4,message=F,warning=F}

######################################
### PART 2###############
######################################

library(CMAverse)

## run mediation analysis with interaction using cmest()
mediation.int.m1 <- cmest(data = data_sens, model = "rb", casecontrol = TRUE, yrare = TRUE,
                          outcome = "case", 
                          exposure = "snp", mediator = "smoking", EMint = TRUE,
                          prec = c("sex", "colgrad", "age"), 
                          mreg = list("logistic"), yreg = "logistic", 
                          a = 1, astar = 0, mval = list(1),
                          estimation = "imputation", inference = "bootstrap")

mediation.int.m0 <- cmest(data = data_sens, model = "rb", casecontrol = TRUE, yrare = TRUE,
                          outcome = "case", 
                          exposure = "snp", mediator = "smoking", EMint = TRUE,
                          prec = c("sex", "colgrad", "age"), 
                          mreg = list("logistic"), yreg = "logistic", 
                          a = 1, astar = 0, mval = list(0),
                          estimation = "imputation", inference = "bootstrap")

summary(mediation.int.m1)

#plot(mediation.int.m1) +
#  theme(axis.text.x = element_text(angle = 45))

summary(mediation.int.m0)

#plot(mediation.int.m0) +
#  theme(axis.text.x = element_text(angle = 45))

## CDE(1) = 1.600
## CDE(0) = 1.125
## NIE = 1.030 
## NDE = 1.526 
## PM = 0.079 (from output)
# 1.526 * (1.030 - 1) / (1.526 * 1.030 - 1) = 0.080


cmsens(object = mediation.int.m0, sens = "uc")

```

