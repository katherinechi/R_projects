# MANOVA and Canonical Discrim Analysis for Inflorescence Morphometrics

# Import the CSV file/dataset
Morphology<-read.csv("InfMorph_Raw_IL2011_NoOut.csv",header=TRUE)

# Check the dataset
attach(Morphology)
str(Morphology)

############################################
##### CHECK FOR MULTIVARIATE NORMALITY #####
############################################

library(mvnormtest)

# Create matrix from data

Morph.Shap <- as.matrix(Morphology[1:121,3:5]) # Convert data from vector to matrix
Morph.Shap2 <- t(Morph.Shap) # Transpose matrix into format for Shapiro test
Morph.Shap3 <- log(Morph.Shap2) # Log-transform matrix

# Run Shapiro-Wilk statistic for multivariate normality

mshapiro.test(Morph.Shap2)
# Note to self: if you get the error message, 'Error in if (rng == 0)...'
# Check the Excel spreadsheet and data range in the matrix above
# This error is caused by blank spaces in the data matrix

# Display a Normal Q-Q Plot for multivariate normality

center <- colMeans(Morph.Shap3) # centroid
n <- nrow(Morph.Shap); p <- ncol(Morph.Shap); cov <- cov(Morph.Shap); 
d <- mahalanobis(Morph.Shap,center,cov) # distances 
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1)

# Detect multivariate outliers in the dataset
library(mvoutlier)
outliers <- aq.plot(Morph.Shap, alpha=0.10)
outliers # Show list of outliers


############################################
#### CHECK FOR HOMOGENEITY OF VARIANCE #####
############################################

library(car)
leveneTest(InfHeight ~ Habitat, data=Morphology)
leveneTest(FlwDen ~ Habitat, data=Morphology)
leveneTest(FlwTotal ~ Habitat, data=Morphology)


############################################
######### MULTIVARIATE STATISTICS ##########
############################################

# Generate box plots to explore data

par(mfrow=c(2, 2))
for (response in c("InfHeight", "FlwDen", "FlwTotal"))
  Boxplot(Morphology[, response] ~ Habitat, data=Morphology, ylab=response)

# Fit a multivariate one-way ANOVA (MANOVA)

library(car)

mod.morph <- lm(cbind(InfHeight, FlwDen, FlwTotal)
                ~ Habitat, data=Morphology)
class(mod.morph)
summary(mod.morph)

manova.morph <- anova(mod.morph)
class(manova.morph)
summary(manova.morph)

manova.morph2 <- manova(mod.morph)

# Canonical Discriminant Analysis

library(candisc)
can.morph <- candisc(manova.morph2)
can.morph                            # Display results of can. disc. analysis
can.morph$coeffs.raw                 # Display raw can. coefficients
can.morph$coeffs.std                 # Display standardized can. coefficients
can.morph$structure                  # Display canonical structure coefficients


# Generate basic Canonical Discriminant Plot

candisc.result <- candisc(manova.morph2)
plot(candisc.result)

# Generate advanced Canonical Plots

library(heplots)
heplot(candisc.result, scale=3, fill=TRUE)

# Floral Trait Scatterplot matrix
library(ggplot2)
suppressMessages(suppressWarnings(library(GGally)))
p <- ggpairs(Morphology.Tran[,c(2,4,5,6,7,8,9)], colour = "Habitat")
print(p)
# detach package after use so reshape2 works (old reshape (v.1) conflicts)
detach("package:GGally", unload=TRUE)
detach("package:reshape", unload=TRUE)

# Can Disc. Scatterplot Matrix
library(ggplot2)
suppressMessages(suppressWarnings(library(GGally)))
p <- ggpairs(can.morph$scores, colour = "Habitat")
print(p)
# detach package after use so reshape2 works (old reshape (v.1) conflicts)
detach("package:GGally", unload=TRUE)
detach("package:reshape", unload=TRUE)


############################################
########## UNIVARIATE STATISTICS ###########
############################################

library(car)

Anova(lm(InfHeight ~ Habitat, Morphology))
AOVHeight <- aov(InfHeight ~ Habitat, data=Morphology)
TukeyHSD(AOVHeight)

Anova(lm(FlwDen ~ Habitat, Morphology))
AOVFlwDen <- aov(FlwDen ~ Habitat, data=Morphology)
TukeyHSD(AOVFlwDen)

Anova(lm(FlwTotal ~ Habitat, Morphology))
AOVFlwTotal <- aov(FlwTotal ~ Habitat, data=Morphology)
TukeyHSD(AOVFlwTotal)
