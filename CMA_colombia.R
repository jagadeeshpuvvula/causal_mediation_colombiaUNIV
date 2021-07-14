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
DE.fit <- glm(case ~ snp + smoking + sex + colgrad + age, 
                     family = binomial, data=data_sens)
summary(out.fit.noint)

## use DIFFERENCE method to estimate the direct and indirect effects 
## TE = 0.447 (beta value of snp from TE.fit)
## DE = 0.387 (beta value of snp from DE.fit)
## IE = 0.447 - 0.387 = 0.060 (using difference method)
## PM = IE / TE = 0.134 
## OR(DE) = exp(0.387) = 1.473
## OR(IE) = exp(0.060) = 1.062
## PM = (OR(DE)*(OR(IE)-1))/(OR(DE)*OR(IE)-1) = 0.162


#(ESTIMATES ARE USED FOR PRODUCT METHOD)
## mediation model: Check how the exposure is related to suspected 
# mediator variable
med.fit <- glm(smoking ~ snp + sex + colgrad + age, family = binomial, data=data_sens)
summary(out.fit.noint)
summary(med.fit)

## use PRODUCT method to estimate the direct and indirect effects 
## DE = 0.387 (beta value of snp from DE.fit)
## IE = 0.352 * 1.605 = 0.565 (beta value of snp and )
## Not possible (all effects in same direction but IE >TE!)
## Issue of collapsibility 
## Issue of case-control study

## fit mediation model among controls
med.fit.con <- glm(smoking ~ snp + sex + colgrad + age, family = binomial, 
                   data=data_sens[which(data_sens$case == 0),])
summary(out.fit.noint)
summary(med.fit.con)

## IE = 0.095 * 1.605 = 0.152 (beta value of snp)
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

cmest
## run mediation analysis with interaction using cmest()
mediation.int.m1 <- cmest(data = data_sens, model = "rb", casecontrol = TRUE, yrare = TRUE,
                         outcome = "case", 
                         exposure = "snp", mediator = "smoking", EMint = TRUE,
                         basec = c("sex", "colgrad", "age"), 
                         mreg = list("logistic"), yreg = "logistic", 
                         a = 1, astar = 0, mval = list(1),
                         estimation = "paramfunc", inference = "delta")

mediation.int.m0 <- cmest(data = data_sens, model = "rb", casecontrol = TRUE, yrare = TRUE,
                         outcome = "case", 
                         exposure = "snp", mediator = "smoking", EMint = TRUE,
                         basec = c("sex", "colgrad", "age"), 
                         mreg = list("logistic"), yreg = "logistic", 
                         a = 1, astar = 0, mval = list(0),
                         estimation = "paramfunc", inference = "delta")

summary(mediation.int.m1)

#plot(mediation.int.m1) +
 #  theme(axis.text.x = element_text(angle = 45))

summary(mediation.int.m0)

#plot(mediation.int.m0) +
 #  theme(axis.text.x = element_text(angle = 45))

## CDE(1) = 1.60
## CDE(0) = 1.12503
## NIE = 1.03
## NDE = 1.52 
## PM = 0.08 (from output)


cmsens(object = mediation.int.m0, sens = "uc")

######################################################
##   Mediation Analysis Course - R Application      ##
##   Bangladesh example                             ##
######################################################


## load in data 
data_sens <- read.csv("sim_data_bangladesh.csv")

######################################
### QUESTION 1A ######################
### traditional mediation analysis ###
######################################

## Total effect: just adjust for suspected confounder
TE.fit <- lm(cognitive_raw ~ ln_mn_c + protein_c + female + approxage, data=data_sens)
summary(TE.fit)
## TE = -0.65445

# Direct effect: add covariate and mediator w/o interaction
out.fit.noint <- lm(cognitive_raw ~ ln_mn_c + birthlength_c + 
                      protein_c + female + approxage, data=data_sens)
summary(out.fit.noint)
## direct effect (beta value of length)
## -0.35228

## use difference method to estimate the direct and indirect effects 
## indirect effect : Total effect - direct effect
## -0.65445 - (-0.35228) = -0.302

## fit mediation model: Use mediator variable as the outcome instead actual outcome
med.fit <- lm(birthlength_c ~ ln_mn_c + protein_c + female + approxage, data=data_sens)
summary(med.fit)

## use product method to estimate the direct and indirect effects 
## direct effect 
## -0.35228 (beta of length from mediation model)
## indirect effect 
#(beta of length from mediation model and beta of birth length from DE)
##-0.349862*0.86369 = -0.302 
## see this is exactly what we got with the difference method

######################################
### QUESTION 1B ######################
######################################

## fit outcome model WITH interaction
out.fit <- lm(cognitive_raw ~ ln_mn_c * birthlength_c + protein_c + 
                female + approxage, data=data_sens)
summary(out.fit)
## p-value for interaction = 0.02
## the harmful effect of Mn is reduced at higher levels at birth length 
## use R directly
confint(out.fit)
## or calculate this by "hand"
## c(0.277738 - 1.96* 0.12058, 0.277738 + 1.96* 0.12058)
### 95% CI is (0.04, 0.51)




######################################
### QUESTION 2A ######################
######################################

#install.packages("CMAverse")
library(CMAverse)

## run mediation analysis w/o interaction using cmest()
mediation.noint <- cmest(data = data_sens, model = "rb", 
                         outcome = "cognitive_raw", 
                         exposure = "ln_mn_c", mediator = "birthlength_c", EMint = FALSE,
                         basec = c("protein_c", "female", "approxage"), 
                         mreg = list("linear"), yreg = "linear", mval = list(1),
                         estimation = "paramfunc", inference = "delta")
summary(mediation.noint)
#plot(mediation.noint) +
 #  theme(axis.text.x = element_text(angle = 45))

## NIE = -0.302 
## NDE = -0.352 
## TE  = -0.654
## the same as both the product and difference methods 

######################################
### QUESTION 2B ######################
######################################

## run mediation analysis including interaction and considering a change from a*=0 to a=1
#EMint = true function allows exposure mediator interaction
mediation.0.1 <- cmest(data = data_sens, model = "rb", 
                       outcome = "cognitive_raw", 
                       exposure = "ln_mn_c", mediator = "birthlength_c", EMint = TRUE,
                       basec = c("protein_c", "female", "approxage"), 
                         mreg = list("linear"), yreg = "linear",
                         astar = 0, a = 1, mval = list(1),
                         estimation = "paramfunc", inference = "delta")

summary(mediation.0.1)
#plot(mediation.0.1) +
 #  theme(axis.text.x = element_text(angle = 45))

## NIE = tnie = -0.392
## NDE = pnde = -0.356
## TE  = -0.747

######################################
### QUESTION 2C ######################
######################################

## determine the 25th and 75th percentiles of manganese exposure 
quantile(data_sens$ln_mn_c, probs=c(0.25, 0.75))

### rerun this with 25th and 75th percentile 
## run mediation analysis 
#astar and a value is to show the effect with change in exposure from astar (using 25th pct) value to a (using 75 pct)
mediation.25.75 <- cmest(data = data_sens, model = "rb", 
                         outcome = "cognitive_raw", 
                         exposure = "ln_mn_c", mediator = "birthlength_c", EMint = TRUE,
                         basec = c("protein_c", "female", "approxage"), 
                         mreg = list("linear"), yreg = "linear",
                         astar = -0.61, a = 0.71, mval = list(1),
                         estimation = "paramfunc", inference = "delta")
summary(mediation.25.75)
#plot(mediation.25.75) +
 #  theme(axis.text.x = element_text(angle = 45))

## NIE = -0.480
## NDE = -0.391 
## TE  = -0.871
## PM  =  0.551 (from output)
## -0.480/-0.871 = 0.551

#Use the function cmsense to assess sensitivity to unmeasured confounding

cmsens(object = mediation.25.75, sens = "uc")

