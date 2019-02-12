# Import the CSV file/dataset
Demography<-read.csv("Demo_Rdata_Tran_Remove.csv")

# Check the dataset
str(Demography)

# Change year into a factor
Demography$Year<-factor(Demography$Year, labels=c("2010","2011"))
detach(Demography)
attach(Demography)
summary(Year)

# Generate a global model for Fruit Set
# 'library' is the R package that is used for this analysis
# The '(1|Year) tells the model to treat Year as a random effect
# All other parameters are fixed/main effects
attach(Demography)
library(lmerTest)
global.model<-lmer(TrFruitSet~TrPopSizeInf+PopDenInf+PropFlw+InfDen+InfHeight
                   +(1|Year),
                   data=Demography)

# Show the results of the global model for Fruit Set
# 'summary' shows the AIC and BIC
# 'anova' shows F and p-values
# 'rand' shows analysis of random effects
summary(global.model)
anova(global.model)
rand(global.model)

# Standardize the model when parameters do not use the same scale
# 'standardize' rescales regression predictors to have mean=0 and SD=0.5
library(arm)
stdz.model<-standardize(global.model,
                        standardize.y=FALSE)

# Show AIC, BIC, SEs, estimates, and z-scores for standardized global model
summary(stdz.model)

# Generate a full set of submodels from the global model (including null model)
library(MuMIn)
model.set<-dredge(stdz.model)
summary(model.set)

# Show the top models from set of submodels (i.e., models with delta-AIC<2)
library(MuMIn)
top.models<-get.models(model.set,subset=delta<4)
summary(top.models)

# Model-average the top models and show estimates and unconditional SEs
library(MuMIn)
summary(model.avg(top.models))

# Generate a 95% Confidence Interval for parameter estimates
avgmod.95p<-model.avg(top.models,cumsum(weight)<=.95)
confint(avgmod.95p)
