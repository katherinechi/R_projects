########################################
#####  IMPORT DATA AND R PACKAGES  #####
########################################

# Import expected graduate CSV data file

EXPGRAD <- read.csv("ExpGrad_RegressionData.csv")

# Import R packages for analysis

library(ggplot2)            # Plots graphs/visualizations
library(gridExtra)          # Arranges graphs in grid pattern
library(caret)              # Refines models
library(rms)                # Logistic regression models
library(MASS)               # Loglinear models



##################################################
#####  DATA QUALITY CHECK AND MANIPULATIONS  #####
##################################################

# Check structure of imported data

str(EXPGRAD)

# Check for missing values in each field

sapply(EXPGRAD, function(x) sum(is.na(x)))

# Rename fields that did not import with proper headings

names(EXPGRAD)[1] <- "ID"                   # Student ID number
names(EXPGRAD)[2] <- "TermEB"               # Enrollment term
names(EXPGRAD)[3] <- "TermExpGrad"          # Expected graduation term
names(EXPGRAD)[4] <- "Career"               # Academic career
names(EXPGRAD)[5] <- "Program"              # Academic program
names(EXPGRAD)[6] <- "Plan"                 # Academic plan/major
names(EXPGRAD)[7] <- "School"               # School that major falls into
names(EXPGRAD)[8] <- "Campus"               # Student's home campus during current term
names(EXPGRAD)[9] <- "ONS_ONL"              # If home campus is onsite or online
names(EXPGRAD)[10] <- "Outcome"             # If student graduated at end of term
names(EXPGRAD)[11] <- "GPA"                 # Cumulative GPA at end of term
names(EXPGRAD)[12] <- "Fails"               # Number of fails/withdraws/incompletes
names(EXPGRAD)[13] <- "ONSCred"             # Credits taken onsite during current term
names(EXPGRAD)[14] <- "ONLCred"             # Credits taken online during current term
names(EXPGRAD)[15] <- "OffCred"             # Credits taken off home campus during current term
names(EXPGRAD)[16] <- "TotalCred"           # Total credits taken during current term
names(EXPGRAD)[17] <- "Subs"                # Number of substitions since start of 2018
names(EXPGRAD)[18] <- "SAP"                 # If a student has exceeded 150% SAP

# Create field to indicate if student failed a couse in final term
# Create field to indicate if student received course substitution in 2018
# Create field to indicate if a student took offsite credits in final term
# Create categories for cumulative GPA

EXPGRAD$FailVar <- ifelse(EXPGRAD$Fails > 0,"Y","N")
EXPGRAD$SubVar <- ifelse(EXPGRAD$Subs > 0,"Y","N")
EXPGRAD$OffVar <- ifelse(EXPGRAD$OffCred > 0,"Y","N")
EXPGRAD$GPAGroup <- ifelse(EXPGRAD$GPA >= 2.000,">2.0","<2.0")

# Change semester/term into factor

EXPGRAD$TermExpGrad <- factor(EXPGRAD$TermExpGrad, labels=c("1189"))

# Update data frame with changes

detach(EXPGRAD)
attach(EXPGRAD)



#######################################
#####  EXPLORATORY DATA ANALYSIS  #####
#######################################

# Create bar plots of categorical variables to check for variation in proposed factors 

p1 <- ggplot(EXPGRAD, aes(x = TermExpGrad)) + ggtitle("Exp Grad Term") + xlab("Exp Grad Term") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()
p2 <- ggplot(EXPGRAD, aes(x = GPAGroup)) + ggtitle("Cumulative GPA") + xlab("Cumulative GPA") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()
p3 <- ggplot(EXPGRAD, aes(x = OffVar)) + ggtitle("Offsite Credits") + xlab("Offsite Credits") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()
p4 <- ggplot(EXPGRAD, aes(x = ONS_ONL)) + ggtitle("Onsite/Online") + xlab("Onsite/Online") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()
p5 <- ggplot(EXPGRAD, aes(x = FailVar)) + ggtitle("Failed") + xlab("Failed") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()
p6 <- ggplot(EXPGRAD, aes(x = SubVar)) + ggtitle("Substitution") + xlab("Substitution") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()
p7 <- ggplot(EXPGRAD, aes(x = School)) + ggtitle("School") + xlab("School") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()
p8 <- ggplot(EXPGRAD, aes(x = SAP)) + ggtitle("Exceed M150") + xlab("Exceed M150") +
  geom_bar(aes(y=100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") +
  coord_flip() + theme_minimal()

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4)



########################################
#####  LOGISTIC REGRESSION MODELS  #####
########################################

# Run a logistic regression to see which factors in model explain graduation outcomes
# Factors: onsite vs online, failed classes in final term (Y/N), course substitutions (Y/N),
#     offsite credits (Y/N), number of offsite credits

RegModel <- glm(Outcome ~ ONS_ONL + FailVar + SubVar + OffCred + SAP,
                family=binomial(link="logit"))
print(summary(RegModel))

# Compare glm result to lrm logistic regression

require(rms)
CalcReg <- lrm(Outcome ~ ONS_ONL + FailVar + SubVar + OffCred + SAP, x = TRUE, y = TRUE)
print(CalcReg)

# Examine impact of each factor on deviance

anova(CalcReg, test = "Chisq")



##############################################
#####  POST-HOC TESTS: LOGLINEAR MODELS  #####
##############################################

# Use a loglinear model to determine if failures are dependent on onsite/online enrollment
# Reference: https://mgimond.github.io/Stats-in-R/ChiSquare_test.html

# Create a smaller dataframe for loglinear analysis and rename fields

EXPGRAD2 <- data.frame(EXPGRAD$ONS_ONL,EXPGRAD$OffVar,EXPGRAD$FailVar)

names(EXPGRAD2)[1] <- "ONS_ONL2"          # If student's home campus is onsite/online
names(EXPGRAD2)[2] <- "OffVar2"           # If student took offsite credits in final term
names(EXPGRAD2)[3] <- "FailVar2"          # If student failed courses in final term

# Create an n-dimensional table of data

EGTABLE <- xtabs(~ OffVar2 + FailVar2 + ONS_ONL2, EXPGRAD2)

# Convert crosstabs into matrix and view results

EGMATRIX <- apply(EGTABLE,c(1:3),sum)
ftable(EGMATRIX)

# Create a saturated model to examine model output of all factors, including interactions
# P(>X^2) should equal 1 because model includes all parameters

LLModel <- loglm(~ OffVar2 * FailVar2 * ONS_ONL2, dat=EGMATRIX, fit=TRUE)
print(LLModel)

# Create a model without the interaction term to see how much they contribute to model

LLModel2 <- update(LLModel, ~. - OffVar2:FailVar2:ONS_ONL2)
print(LLModel2)

# Continue testing the impact of removing each interaction term

LLModel3 <- update(LLModel, ~. - OffVar2:FailVar2 - OffVar2:FailVar2:ONS_ONL2)
print(LLModel3)

LLModel4 <- update(LLModel, ~. - OffVar2:ONS_ONL2 - OffVar2:FailVar2:ONS_ONL2)
print(LLModel4)

LLModel5 <- update(LLModel, ~. - FailVar2:ONS_ONL - OffVar2:FailVar2:ONS_ONL2)
print(LLModel5)

# The results indicate that all interactions are significant except for:
#     OffVar2:FailVar2:ONS_ONL2 and FailVar2:ONS_ONL2

# Conduct ANOVAs to confirm best model for the data
anova(LLModel,LLModel2,LLModel3,LLModel4)


