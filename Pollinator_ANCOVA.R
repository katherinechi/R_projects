# ANCOVA analysis performed using tutorial found here:
# http://r-eco-evo.blogspot.com/2011/08/comparing-two-regression-slopes-by.html

# Import the CSV file/dataset
Pollinator<-read.csv("PollenLimitation.csv",header=TRUE)

# Check the dataset
str(Pollinator)

# Check for homogeneity of variance using Levene's test
library(car)
leveneTest(Pollinator$ConPollen, Pollinator$Treatment, Pollinator$Habitat, center=median)

# Check to determine if covariate and predictors are independent
Independence.Test <- aov(ConPollen~Habitat*Treatment, data=Pollinator)
summary(Independence.Test)

# Model w/ ConPollen as covariate, Treatment & Habitat as factors
ancova.result <- aov(FruitSet~ConPollen+Treatment+Habitat+Habitat*Treatment, data=Pollinator)
summary(ancova.result)

# Model main effects w/out interactions
anova.result <- aov(FruitSet~ConPollen+Treatment+Habitat, data=Pollinator)
summary(anova.result)

# Compare the two models to determine if removing interaction affects model fit
# If p>0.05, then removing interaction does NOT affect model fit; use Model 2
# If p<0.05, then removing interaction affects model fit; use Model 1
anova(ancova.result,anova.result)

# Fit linear regressions for Control and Exclusion
Control <- subset(Pollinator,Treatment=="NoCage")
Exclusion <- Pollinator[Pollinator$Treatment=='Cage',]
regControl <- lm(FruitSet~ConPollen, data=Control)
summary(regControl)
regExclusion <- lm(FruitSet~ConPollen, data=Exclusion)
summary(regExclusion)

# Plot regression lines for Control and Exclusion
plot(FruitSet~ConPollen, data=Pollinator, type='n')
points(Control$ConPollen,Control$FruitSet, pch=20)
points(Exclusion$ConPollen,Exclusion$FruitSet, pch=1)
abline(regControl, lty=1)
abline(regExclusion, lty=2)
legend("bottomright", c("Control","Exclusion"), lty=c(1,2), pch=c(20,1) )

# Fit linear regressions for Open, Semi-Shade, and Shade
Open <- subset(Pollinator,Habitat=="Open")
SemiShade <- Pollinator[Pollinator$Habitat=='SemiShaded',]
Shade <- Pollinator[Pollinator$Habitat=='Shaded',]
regOpen <- lm(FruitSet~ConPollen, data=Open)
summary(regOpen)
regSemiShade <- lm(FruitSet~ConPollen, data=SemiShade)
summary(regSemiShade)
regShade <- lm(FruitSet~ConPollen, data=Shade)
summary(regShade)

# Plot regression lines for Open, Semi-Shade, and Shade
plot(FruitSet~ConPollen, data=Pollinator, type='n')
points(Open$ConPollen,Open$FruitSet, pch=20)
points(SemiShade$ConPollen,SemiShade$FruitSet, pch=10)
points(Shade$ConPollen,Shade$FruitSet, pch=1)
abline(regOpen, lty=1)
abline(regSemiShade, lty=2)
abline(regShade, lty=3)
legend("bottomright", c("Open","SemiShade", "Shade"), lty=c(1,2,3), pch=c(20,10,1) )