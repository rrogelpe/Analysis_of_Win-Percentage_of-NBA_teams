# Final Project
# Mohammad Khan, Rodrigo Rogel-Perez, Meixi Chen, Ary Pierre

WinPer = The_Final_Project_Proposal$`WIN%`

PTS = The_Final_Project_Proposal$PTS
FGPer = The_Final_Project_Proposal$`FG%`
ThreePer = The_Final_Project_Proposal$`3P%`
FTPer = The_Final_Project_Proposal$`FT%`

STL = The_Final_Project_Proposal$STL
AST = The_Final_Project_Proposal$AST
REB = The_Final_Project_Proposal$REB
BLKA = The_Final_Project_Proposal$BLKA
PF = The_Final_Project_Proposal$PF
TOV = The_Final_Project_Proposal$TOV
DREB = The_Final_Project_Proposal$DREB
OREB = The_Final_Project_Proposal$OREB

REG = factor(The_Final_Project_Proposal$Region)
PREV = The_Final_Project_Proposal$`Prev_Win%`


# Highly significant 
# PTS, FGPer, ThreePer, BLKA, DREB

# Not significant 
# STL, FT%, BLK, OREB, PF, TOV -- LATER: we need to add why we didn't include these


# # ASSUMPTION #1
# plot(FGPer, WinPer)
# plot(ThreePer, WinPer)
# plot(AST, WinPer)
# plot(PTS, WinPer)
# plot(DREB, WinPer)
# 
# # Check Linearity and normality
# model = lm(WinPer ~ FGPer)
# r = rstudent(model)
# plot(FGPer, r)
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # LA Lakers
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ PTS)
# r = rstudent(model)
# plot(PTS, r) # outlier near 105 PTS, over std. of 2
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # GS Warriors
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ ThreePer)
# r = rstudent(model)
# plot(ThreePer, r) # outlier near 34, over std. of -2
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # GS Warriors
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ BLKA)
# r = rstudent(model)
# plot(BLKA, r)
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # LA Clippers
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ DREB)
# r = rstudent(model)
# plot(DREB, r)
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # None
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None

fullModel = lm(WinPer ~ REG + PREV + DREB + BLKA + ThreePer + PTS + FGPer + AST + FTPer + REB + STL  + PF + TOV + OREB)
summary(fullModel) # .93 R^2 
plot(rstudent(fullModel), type= 'b')

# ASSUMPTION #3 - collinearity
pairs ( ~ PREV + DREB + BLKA + ThreePer + PTS + FGPer + AST + FTPer + REB + STL  + PF + TOV + OREB)
cor(data.frame(PREV, DREB, BLKA, ThreePer, PTS, FGPer, AST, FTPer, REB, STL, PF, TOV, OREB))
# Some of the variables are clearly correlated

library(car)
vif(fullModel)

smallerModel = lm(WinPer ~ REG + PREV + DREB + BLKA + ThreePer + FGPer  + FTPer  + STL  + PF + TOV + OREB)
# removed: PTS, FTPer, AST, REB
# PTS, AST - inconsistent coefficients don't make sense 
# REB is a sum of off, deff rebounds
summary(smallerModel) # .91 R^2

anova(smallerModel, fullModel)
# p-value is large, fail to reject null -> reduced model is adequate
# Confirms reduced model is OK

vif(smallerModel) # looks okay

finalModel = lm(WinPer ~ PREV + DREB + FGPer + STL + TOV + OREB) # OUR BEST MODEL
summary(finalModel) # R^2 .905
vif(finalModel) # looks good

#Pre analysis
boxplot(WinPer)    #Philadelphia 76ers and GS Warriors migh be outliers
boxplot(PREV)
boxplot(DREB)   #There might be an outlier,above 36 (GS Warriors looks like an outlier)
boxplot(FGPer)
boxplot(STL)
boxplot(TOV)
boxplot(OREB)

# Check Linearity 
r = rstudent(finalModel)
plot(PREV, r)
#PREV[r<-3]
plot(DREB, r)
plot(FGPer, r)
plot(STL, r)
plot(TOV, r)
plot(OREB, r)
plot(finalModel$fitted.values,r)
#which(r < -3)

# check normality
qqnorm(r) # ideally we want a nice straight line
qqline(r)
hist(r)
#Rows 12 and 28 seem like they are outliers. We might consider removing
#them and test the reduced model without these points.

# check independence 
plot(r, type = "b")
#It might be a good idea to remove rows 12 and 28
#Autocorrelation does not seem to be a problem. The number of runs seem adequate 
#But we can still test using the DW test.

library(lmtest)
dwtest(finalModel)
#Ho:p=0
#Ha:p<>0
#High p-value, so we fail to reject the null hypothesis.
#So we can statistacilly verify that there is no first order autocorrelation.
#The residual plot of the model also suggests that there is no higher order autocorrelation.

# check linear independence between predictors
pairs ( ~ PREV + DREB + FGPer + STL + TOV + OREB)
cor(data.frame(PREV , DREB , FGPer , STL , TOV , OREB))
#None of the values are greater than 0.4672360, So predictor variables are independent of each other.

#Assumption 4, (All points are equally influential)
plot(r, type="b") 
points(which(r < -2), r[which(r < -2)], col = "red")
which(r < -2)

 model = lm(WinPer ~ FGPer)
 r = rstudent(model)
 plot(r)
 
 # Check leverage
 lev = hat(model.matrix(model))
 plot(lev)
 The_Final_Project_Proposal$TEAM[lev > .2]
 
 cook = cooks.distance(model)
 plot(cook, ylab = "Cooks Distance")
 The_Final_Project_Proposal$TEAM[cook > 1]
 # None
 
 model2 = lm(WinPer ~ PREV)
 r = rstudent(model2)
 plot(r)
 # Check leverage
 lev = hat(model.matrix(model2))
 plot(lev)
 The_Final_Project_Proposal$TEAM[lev > .2]
 
 # Determine influence
 cook = cooks.distance(model2)
 plot(cook, ylab = "Cooks Distance")
 The_Final_Project_Proposal$TEAM[cook > 1]
 # None
 
 model3 = lm(WinPer ~ DREB)
 r = rstudent(model3)
 plot(r)
 # Check leverage
 lev = hat(model.matrix(model3))
 plot(lev)
 The_Final_Project_Proposal$TEAM[lev > .2]
 
 # Determine influence
 cook = cooks.distance(model3)
 plot(cook, ylab = "Cooks Distance")
 The_Final_Project_Proposal$TEAM[cook > 1]
 # None
 
 model4 = lm(WinPer ~ STL)
 r = rstudent(model4)
 plot(r)
 # Check leverage
 lev = hat(model.matrix(model4))
 plot(lev)
 The_Final_Project_Proposal$TEAM[lev > .2]
 
 # Determine influence
 cook = cooks.distance(model4)
 plot(cook, ylab = "Cooks Distance")
 The_Final_Project_Proposal$TEAM[cook > 1]
 # None
 
 model5 = lm(WinPer ~ TOV)
 r = rstudent(model5)
 plot(r)
 # Check leverage
 lev = hat(model.matrix(model5))
 plot(lev)
 The_Final_Project_Proposal$TEAM[lev > .2]
 #"Phoenix Suns" has high leverage
 
 # Determine influence
 cook = cooks.distance(model5)
 plot(cook, ylab = "Cooks Distance")
 The_Final_Project_Proposal$TEAM[cook > 1]
 #Since none of the points have high influence, we don't consider to remove any data
 
 model6 = lm(WinPer ~ OREB)
 r = rstudent(model6)
 plot(r)
 # Check leverage
 lev = hat(model.matrix(model6))
 plot(lev)
 The_Final_Project_Proposal$TEAM[lev > .2]
 #"Oklahoma City Thunder" has a high leverage point
 
 # Determine influence
 cook = cooks.distance(model6)
 plot(cook, ylab = "Cooks Distance")
 The_Final_Project_Proposal$TEAM[cook > 1]
 # None of the points are influential, so dont remove any data.
 
 Red_The_Final_Project_Proposal=The_Final_Project_Proposal[-c(12,28),]
 
 WinPer2 = Red_The_Final_Project_Proposal$`WIN%`
 FGPer2 = Red_The_Final_Project_Proposal$`FG%`
 STL2 = Red_The_Final_Project_Proposal$STL
 TOV2 = Red_The_Final_Project_Proposal$TOV
 DREB2 = Red_The_Final_Project_Proposal$DREB
 OREB2 = Red_The_Final_Project_Proposal$OREB
 PREV2 = Red_The_Final_Project_Proposal$`Prev_Win%`
 
 redFinal = lm(WinPer2 ~ PREV2 + DREB2 + FGPer2 + STL2 + TOV2 + OREB2)
 summary(redFinal)
 r=rstudent(redFinal)
 plot(WinPer2,r)
 plot(PREV2, r)
 #PREV[r<-3]
 plot(DREB2, r)
 plot(FGPer2, r)
 plot(STL2, r)
 plot(TOV2, r)
 plot(OREB2, r)
 
 # check normality
 qqnorm(r) # ideally we want a nice straight line
 qqline(r)
 hist(r)
 
 ##Confidence interval for PREV2(95%)
 0.338407-qt(.975, 21)*0.060203      #Since we removed 2 rows we have 28 observation, 7 variables
 0.338407+qt(.975, 21)*0.060203
 
 #We are 95% confident that PREV2 (slope parameter) falls between 0.213208 and 0.463606.
 #We are 95% confident that for every unit increase in PREV2, WinPer increases between 21.32% and 46.36%.

 ##Confidence interval for DREB2(95%)
 0.045668-qt(.975, 21)*0.007935      #Since we removed 2 rows we have 28 observation, 7 variables
 0.045668+qt(.975, 21)*0.007935
 
 #We are 95% confident that DREB2 (slope parameter) falls between 0.02916626 and 0.06216974
 #We are 95% confident that for every unit increase in DREB2, WinPer increases between 2.92% and 6.22%.
  
 ## Predictions using reduced model, PREV2=0.585 , DREB2=32.9 , FGPer2=45.4 , STL2=7.5 , TOV2=13.3 , OREB2=9.1 
 predict(redFinal, data.frame(PREV2=0.585 , DREB2=32.9 , FGPer2=45.4 , STL2=7.5 , TOV2=13.3 , OREB2=9.1), interval = "confidence")
 predict(redFinal, data.frame(PREV2=0.585 , DREB2=32.9 , FGPer2=45.4 , STL2=7.5 , TOV2=13.3 , OREB2=9.1), interval = "prediction")
 
 # Confidence Interval: We are 95% confident the AVERAGE win percentage when PREV2 = .585 and DREB2 = 32.9
 # FGPer2=45.4, STL2=7.5, TOV2=13.3, OREB2=9.1 is between 51.314 and 58.04 
 
 # Prediction Interval: We are 95% confident the INDIVIDUAL win percentage when PREV2 = .585 and DREB2 = 32.9
 # FGPer2=45.4, STL2=7.5, TOV2=13.3, OREB2=9.1 is between 45.33 and 64.023
 
 
 
 
 
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # LA Lakers
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ PTS)
# r = rstudent(model)
# plot(PTS, r) # outlier near 105 PTS, over std. of 2
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # GS Warriors
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ ThreePer)
# r = rstudent(model)
# plot(ThreePer, r) # outlier near 34, over std. of -2
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # GS Warriors
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ BLKA)
# r = rstudent(model)
# plot(BLKA, r)
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # LA Clippers
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None
# 
# model = lm(WinPer ~ DREB)
# r = rstudent(model)
# plot(DREB, r)
# hist(r)
# 
# # Checking Assumption 4, leverage and influence
# lev = hat(model.matrix(model))
# plot(lev)
# Final_Project_Proposal$TEAM[lev > .2]
# # None
# 
# cook = cooks.distance(model)
# plot(cook, ylab = "Cooks Distance")
# Final_Project_Proposal$TEAM[cook > 1]
# # None


# Checking Assumption 4, leverage and influence
lev = hat(model.matrix(finalModel))
plot(lev)
The_Final_Project_Proposal$TEAM[lev > .2]
# LA Lakers

cook = cooks.distance(onlySig)
plot(cook, ylab = "Cooks Distance")
Final_Project_Proposal$TEAM[cook > 1]
# None

anova(onlySig, smallerModel)
# Hypothesis for ANOVA
# Ho: Reduced Model is good
# Ha: Full Model is good
# p-value = .5191, reduced model is okay

onlySig1 = lm(WinPer ~ DREB + FGPer + STL + TOV)
summary(onlySig1)

anova(onlySig1, onlySig)
# Hypothesis for ANOVA
# Ho: Reduced Model is good
# Ha: Full Model is good
# p-value = .011, reduced model is NOT OKAY

# AUTOCORRELATION
plot(onlySig$residuals, type= 'b') # looks good
library(lmtest)
dwtest(onlySig)

# Hypothesis
# Ho: p = 0; no autocorrelation 
# Ha: p > 0; autocorrelation
# We clearly have no autocorrelation





