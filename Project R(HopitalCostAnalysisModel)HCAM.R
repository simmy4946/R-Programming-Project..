# Upload the Hospital Data-----

library(readxl)
HospitalData = read_excel('1555054100_hospitalcosts.xlsx')

# View the Hospital Data---
View(HospitalData)

# To View the top heads of data
head(HospitalData)

# To mention the names given in data
names(HospitalData)

#RECORD PATIENT STATISTICS:----------------
#Age: Age of the patient discharged
#Totchg: Hospital discharge costs

# Get number of hospitals visits based on age
summary(HospitalData)

# View summary ---
summary(as.factor(HospitalData$AGE))
#Total no of hospital for 0-1 age group is 307

# Plotting Histogram: 
hist(HospitalData$AGE, main= "Histogram of Age Group and their hospital visits", xlab= "Age Group", border= "white", col=c("light green","light blue"), xlim=c(0,20), ylim=c(0,350))

#SUMMARIZE EXPENDITURE BASED ON AGE GROUP
ExpenseBasedOnAge = aggregate(TOTCHG ~ AGE, FUN=sum, data= HospitalData)

#Get the maximum expense and its age group
which.max(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$TOTCHG, FUN=sum))

#Bar plot:
barplot(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$AGE, FUN=sum))

# Maximum Expenditure for 0-1 YR IS 678118.


# DIAGNOSIS-RELATED GROUP THAT HAS MAXIMUM HOSPITALIZATION AND EXPENDITURE
#In order of severity of the diagnosis and treatments and to find out the 
#expensive treatments, the agency wants to find the diagnosis-related group 
#that has maximum hospitalization and expenditure.

# Aprdrg: All Patient Refined Diagnosis Related Groups
# Totchg: Hospital Discharge Costs

summary(as.factor(HospitalData$APRDRG))


DiagnosisCost = aggregate(TOTCHG ~ APRDRG, FUN = sum, data= HospitalData)

#Get the maximum diagnostic cost----

DiagnosisCost[which.max(DiagnosisCost$TOTCHG), ]
# As can be seen here 640 diagnosis related group had a max cost of 437978

#3.RACE VS HOSPITALIZATION COSTS:---------------
#To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
#Ho (Null hypothesis): Independent Variable(RACE) is not influencing variable(COSTS)
#H0: there is no correlation among residuals,
#p-value = 0.7394<--this is greater than 0.5 # i.e
# they are independent, in case of regression ,we need high p value so that we can not reject the null

summary(as.factor(HospitalData$RACE))
#there is one null value so this needs to be removed

HospitalData = na.omit(HospitalData)
summary(as.factor(HospitalData$RACE))

#As can be seen 484 patients out of 499 fall under group 1, showing that the number of observations for 1 category is way higher than others - hence data is skewed. This will only affect the results from linear regression or ANOVA analysis
#(lm= linear model)
raceInfluence=lm(TOTCHG ~ RACE, data= HospitalData)
summary(raceInfluence)

# p-Value is 0.69 it is much higher than 0.5
# We can say that race doesn’t affect the hospitalization costs

#ANALYSIS USING ANOVA:-----------
raceInfluenceAOV <- aov(TOTCHG ~ RACE, data= HospitalData) 
raceInfluenceAOV

summary(raceInfluenceAOV)

#The residual variance (deviation from original) (of all other variables) is very high.This implies that there is very little influence from RACE on hospitalization costs

#As can be seen, the degree of freedom (Df) for RACE is 1 and that of residuals is 497 observations

#The F-Value, the test statistic is 0.16 which is much less than 0.5 showing that RACE doesn’t affect teh hospitalization cost.

#The Pr(>F), the p_value of 0.69 is high confirming that RACE does not affect hospitalization cost.

4.# TO PROPERLY UTILIZE THE COSTS, THE AGENCY HAS TO ANALYZE THE SEVERITY OF THE HOSPITAL COSTS BY AGE AND GENDER FO THE PROPER ALLOCATION OF RESOURCES.
  # to make a age gender influence model same as raceInfluence model above by formulating anova .
ageGenderInfluence=lm(TOTCHG ~ AGE + FEMALE, data = HospitalData)
summary(ageGenderInfluence)

# we conclude from above since the p -values of AGE is much lesser than 0.05, the ideal statistical significance level,
#and it also has three stars(***)next to it, it means AGE has the most statiscal significance.

#Similarly, gender is also less than 0.05
#Hence, we can conclude that the model is statistically significant.


5. #SINCE THE LENGTH OF STAY IS THE CRUCIAL FACTOR FOR INPATIENTS,THE AGENCY WANTS TO FIND IF THE LENGTH
   # OF STAY CAN BE PREDICTED FROM AGE,GENDER AND RACE.

# to make a age GenderInflenceModel:
# then see the summary of it to conclude their p values which is much or lesser than 0.05

ageGenderRaceInfluenceModel=lm(formula = LOS ~ AGE + FEMALE + RACE, data = HospitalData)
summary(ageGenderRaceInfluenceModel)

# The p-value is higher than 0.05 for age, gender and race, indicating there is no linear relationship between
# these variables and length of the stay

# Hence, age, gender and race cannot be used to predict the length of stay of inpatients.


6.# COMPLETE ANALYSIS
# AGENCY WANTS TO FIND THE VARIABLE THAT MAINLY AFFECTS HOSPITAL COSTS
# significance method- build a model using all independents variables vs dependent variables
# to build a Hospital Data Model by formulating in given formula in below codes.
# to see the summary of it
HospitalDataModel=lm(formula = TOTCHG ~ ., data = HospitalData)
summary(HospitalDataModel)

#As it is apparent from the coefficient values, Age, Length of stay (LOS) and patient refined diagnosis related groups(APRDRG) have three stars (***) next to it. So they are the ones with statistical significance
#Also, RACE is the least significant. build a model after removing RACE

# AFTER REMOVING RACE AND BUILD THE MODEL BY THE FORMULA 
# where,hca (hospital cost analysis)

hca1 = lm(formula = TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = HospitalData)
summary(hca1)

# build a hca2 with LOS plus APRDRG with totchg ~ age
hca2 = lm(formula = TOTCHG ~ AGE + LOS + APRDRG, data = HospitalData)
summary(hca2)

# we conclude from here above that APRDRG has -ve t- value , dropping it

----------
# build hca3 model by dropping APRDRG

hca3=lm(formula = TOTCHG ~ AGE + LOS, data = HospitalData)
summary(hca3)
---------------# COMPARING MODELS:
  
#Data	Approach	Model Name	Detail	R2	adj R2	std err	R2 - adj R2	p-value
#HospitalCosts	Ap1:significance	hospitalCostModel	signifi, all independent variables	0.554	0.549	2610	0.005	<2e-16
#HospitalCosts	Ap1:significance	hcm1	-RACE	0.553	0.549	2610	0.004	<2e-16
#HospitalCosts	Ap1:significance	hcm2	-RACE - FEMALE (gender)	0.551	0.548	2620	0.003	<2e-16
#HospitalCosts	Ap1:significance	hcm3	AGE + LOS	0.419	0.416	2970	0.003	<2e-16
#Removing Race and gender doesn’t change the R2 value. It doesn’t impact cost
#Removing APRDRG in model hcm3 increases the standard error. Hence model hcm2 seems to be better.

#Removing APRDRG in model hca3 increases the standard error. Hence model hcm2 seems to be better.


# Analysis Conclusion:
# As is evident in the multiple models above, health care costs is dependent on age, length of stay and the diagnosis type.
# Healthcare cost is the most for patients in the 0-1 yrs age group category
# Maximum expenditure for 0-1 yr is 678118
# Length of Stay increases the hospital cost

# All Patient Refined Diagnosis Related Groups also affects healthcare costs

# 640 diagnosis related group had a max cost of 437978
# Race or gender doesn’t have that much impact on hospital cost