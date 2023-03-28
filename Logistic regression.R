
# import dataset

library(readr)
Diab <- read_csv("Diabetes_Dataset.csv")
View(Diab)

# check dimension

dim(Diab)

# check column names

colnames(Diab)


# Storing columns in a variable

# chol <- Diab['chol']
chol <- Diab$chol
# gender <- Diab['gender']

gender <- factor(Diab$gender) # converted to factor and stored into variable
tg <- table(gender)
addmargins(tg) # sum up the total gender
round(100*prop.table(tg), digits=1) # get %s rounded to 1dp


dm <- factor(Diab$dm) # converted to factor and stored into variable
table(dm)


# Summary of continuous variable

summary(chol)

height <- Diab$height
weight <- Diab$weight
summary(height)
summary(weight)

height_conv <- height * 0.0254 # converted from inches to meters
weight_conv <- weight * 0.453592 # converted from pounds to kilograms
bmi <- weight_conv/height_conv^2
summary(bmi)


# categorizing bmi

bmi_categorised <- ifelse(bmi < 18.5, "underweight",
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal",
                                 ifelse(bmi > 25 & bmi <= 30, "overweight",
                                        ifelse(bmi > 30, "obese", NA))))
                         

table(bmi_categorised, exclude = NULL)


# frequencies of diabeties by BMI category
dm_by_bmi_category <- table(bmi_categorised, dm, exclude = NULL)

dm_by_bmi_category

round(100*prop.table(dm_by_bmi_category, margin = 1), digits=1) # get %s rounded to 1dp





# categorizing age
age <- Diab$age

age_categorised <- ifelse(age < 45, "Young Adult",
                          ifelse(age >= 45 & age <= 64, "Adult",
                                 ifelse(age >= 65 & age <= 74, "Old",
                                        ifelse(age >= 75, "Older", NA))))


table(age_categorised, exclude = NULL)


# frequencies of gender by Age category
gender_by_age_category <- table(age_categorised, gender, exclude = NULL)

gender_by_age_category

round(100*prop.table(gender_by_age_category, margin = 1), digits=1) # get %s rounded to 1dp



# Fitting logistic regression model


# empty of null model (no predictor)
empty <- glm(dm ~ 1, family = binomial(link = logit))
summary(empty)

table(empty$y)

# exponentiation to get the odds of having diabetes
exp(-1.7047)

# relationship between odds and probabilities
rel <- 0.182/1.182
rel
table(dm)

# odds of having diabetes
# 60/330 = 0.182

# probability
# 60/(330+60) = 0.15

# one predictor
p1 <- glm(dm ~ gender, family = binomial(link = logit))
summary(p1)


p2 <- glm(dm ~ age, family = binomial(link = logit))
summary(p2)

# check how R entered gender into the model
contrasts(gender)


# create a cross tabulation of age and diabetes status

dim_by_age <- table(age, dm)

# frequencies of diabeties status by age
freq_table <- prop.table(dim_by_age, margin = 1)

# calculate odds of having diabetes
odds <- freq_table[, "yes"]/freq_table[, "no"]

# calculate log odds
logodds <- log(odds)


# plot the ages found in the sample against the log odds of having diabetes
plot(rownames(freq_table), logodds)



location <- factor(Diab$location)
table(location)

dm_by_location <- table(location, dm)
round(100*prop.table(dm_by_location, margin = 1), digits=1)

p3 <- glm(dm ~ location, family = binomial(link = logit))
summary(p3)
contrasts(location)

freq_table2 <- prop.table(dm_by_location, margin = 1)
odds <- freq_table2[, "Louisa"]/freq_table[, "Buckingham"]

levels(location)
location <- relevel(location, ref = "Louisa")
levels(location)

p3 <- glm(dm ~ location, family = binomial(link = logit))
summary(p3)

exp(-0.1395)





# Multiple Regression Model

m <- glm(dm ~ age + gender + bmi, family = binomial(link = logit))
summary(m)

exp(confint(m))


ins <- factor(Diab$insurance)
summary(ins)
table(ins)
class(ins)

m2 <- glm(dm ~ age + chol + ins, family = binomial(link = logit))
summary(m2)
exp(m2$coefficients)

exp(0.049753) # odds for age
exp(0.008402) # odds for cholesterol
exp(-0.271955) # odds for insurance type 1
exp(-0.589803) # odds for insurance type 2


# McFadden's r-squared

full_model <- glm(dm ~ age + chol + ins, family = binomial(link = logit))
summary(full_model)


null_model <- glm(dm ~ 1, family = binomial(link = logit))
summary(null_model)


R2 <- 1-logLik(full_model)/logLik(null_model)
R2

# c-statistic
install.packages("DescTools")
library(DescTools)

full_model <- glm(dm ~ age + chol + ins, family = binomial(link = logit))
summary(full_model)

Cstat(full_model)


# Hosmer-Lemeshow statistic and test

install.packages("ResourceSelection")
library(ResourceSelection)

full_model <- glm(dm ~ age + chol + ins, family = binomial(link = logit))
full_model$y

HL <- hoslem.test(x=full_model$y, y=fitted(full_model), g=10)
HL

#plot the observed vs expected number of cases for each of the 10 groups
plot(HL$observed[,"y1"], HL$expected[,"yhat1"])

#plot the observed vs expected number of non-cases for each of the 10 groups
plot(HL$observed[,"y0"], HL$expected[,"yhat0"])


#plot the observed vs expected prevalence for each of the 10 groups 
plot(x=HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]),
     y=HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"]))


# Hosmer and Lemeshow  goodness of fit (GOF) test
install.packages("generalhoslem")
library(generalhoslem)

logitgof(obs = full_model$y, exp = fitted(full_model), g=10)

# table of deviance
anova(full_model, test = "Chisq")
