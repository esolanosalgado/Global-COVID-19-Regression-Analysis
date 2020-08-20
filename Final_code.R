library("readxl")
library("dplyr")
library('car')
library('olsrr')
library(MASS)
library(ggplot2)
library(interactions)
library(gridExtra)
library('regclass')


covid_data <- read_excel("Final_project_data.xlsx")
attach(covid_data)

#Graphs for all variables:
# 1. With Outliers
# Confirmed cases

par(mfrow=c(1,2))
hist(covid_data$Confirmedcases, xlab="Number of Confirmed Cases", main="Histogram",xlim=c(800,1013000))
boxplot(covid_data$Confirmedcases,
        main = "Boxplot",
        xlab = "Number Confirmed Cases",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
title("COVID-19 Cases (with Outliers)", line=-1, outer=TRUE)

summary(covid_data$Confirmedcases)

#Deaths
hist(covid_data$Deaths, xlab="Number of Deaths", main="No. of Deaths Due to Covid-19 in 100 Countries")

boxplot(covid_data$Deaths,
        main = "No. of Deaths Due to Covid-19 in 100 Countries",
        xlab = "No. of Deaths",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

summary(covid_data$Deaths)

#Urban
hist(covid_data$Urban, xlab="Urban Population %", main="Urban Population (% of total population) in 100 Countries")

boxplot(covid_data$Urban,
        main = "Urban Population (% of total population) in 100 Countries",
        xlab = "Urban Population %",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

summary(covid_data$Urban)

#Region

region_freq <- table(covid_data$Region)
region_freq
barplot(region_freq,xlab="WHO Regions",ylab="Frequency")

#Take out the outliers from the data
covid3 <- subset(covid_data, Entity != 'United States' & Entity != 'Brazil')
attach(covid3)

#2.Without Outliers
# Confirmed cases
par(mfrow=c(1,2))
hist(covid3$Confirmedcases, xlab="Number of Confirmed Cases", main="Histogram")
boxplot(covid3$Confirmedcases,
        main = "Boxplot",
        xlab = "Number of Confirmed Cases",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
title("COVID-19 Cases (without Outliers)", line=-1, outer=TRUE)

summary(covid3$Confirmedcases)

#Deaths
par(mfrow=c(1,2))
hist(covid3$Deaths, xlab="Number of Deaths", main="Histogram")

boxplot(covid3$Deaths,
        main = "Boxplot",
        xlab = "No. of Deaths",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = TRUE
)
title("COVID-19 Deaths (without Outliers)", line=-1, outer=TRUE)

summary(covid3$Deaths)

#Urban
par(mfrow=c(1,2))
hist(covid3$Urban, xlab="Urban Population %", main="Histogram")
boxplot(covid3$Urban,
        main = "Boxplot",
        xlab = "Urban Population %",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
title("Urban (% of Total Population)", line=-1, outer=TRUE)

summary(covid3$Urban)
mean(covid3$Urban)
sd(covid3$Urban)

#Region
par(mfrow=c(1,1))
region_freq1 <- table(covid3$Region)
region_freq1
barplot(region_freq1,xlab="WHO Regions",ylab="Frequency")

region_perc<- (region_freq1/98)*100
region_perc


#Step 1: 
#Scatterplot matrix
pairs(~Confirmedcases+Deaths+Density+HDI+Urban,data=covid3,main="Scatterplot Matrix")
#Corelation between numerical variables
covid2 <- subset(covid3, select = -c(1,2,3,7))
cor(covid2)
#Step 2:
#Check for multi collinearity between numerical variables
fit=lm(Confirmedcases~Deaths+Density+HDI+Urban,data=covid3)
VIF(fit) 
#Step 3: 
#Variable Selection
model <- lm(Confirmedcases~Deaths+Density+HDI+Urban+factor(Region),data=covid3)
ols_step_both_p(model)
#only Deaths shows up
ols_step_all_possible(model)

#Step 4:

#Scatterplot y with each predictor (with outliers)
ggplot(covid_data,aes(x=Deaths,y= Confirmedcases))+geom_point()+geom_smooth(method = 'lm', se = FALSE)
ggplot(covid_data,aes(x=Density,y=Confirmedcases))+geom_point()+geom_smooth(method = 'lm', se = FALSE)
ggplot(covid_data,aes(x=Urban,y=Confirmedcases))+geom_point()+geom_smooth(method = 'lm', se = FALSE)
ggplot(covid_data,aes(x=HDI,y=Confirmedcases))+geom_point()+geom_smooth(method = 'lm', se = FALSE)
detach(covid_data)

#Scatterplot y with each predictor (without outliers)
ggplot(covid3,aes(x=Deaths,y= Confirmedcases))+geom_point()+geom_smooth(method = 'lm', se = FALSE)+ggtitle('Scatterplot of Cases vs Deaths')
ggplot(covid3,aes(x=Deaths,y= Confirmedcases))+geom_point()+ggtitle('Scatterplot of Cases vs Deaths')

#After we see from the scatterplot, we decided to put quadratic term for Deaths since the shape of the scatterplot is curved

plot1<-ggplot(covid3,aes(x=Deaths,y= Confirmedcases))+geom_point()
plot2<-ggplot(covid3,aes(x=Density,y=Confirmedcases))+geom_point()
plot3<-ggplot(covid3,aes(x=Urban,y=Confirmedcases))+geom_point()
plot4<-ggplot(covid3,aes(x=HDI,y=Confirmedcases))+geom_point()
grid.arrange(plot1, plot2, plot3,plot4, ncol=2)


#making quadratic term
covid3$Deaths2 = covid3$Deaths * covid3$Deaths
attach(covid3)

#For Region, we will have 5 dummy variables

#Model 1: Death,Density,Region
fit_density1 <- lm(Confirmedcases~Deaths+Density+factor(Region)+ Deaths2 + Deaths*Density+Deaths*factor(Region)+Density*factor(Region)+Deaths*Density*factor(Region) + Deaths2*factor(Region))
summary(fit_density1)
#adjusted r-sq = 0.6645

#Model 2: Death,Urban,Region
fit_urban1<- lm(Confirmedcases~Deaths*factor(Region)+Deaths2*factor(Region)+Urban*factor(Region)+Deaths:Urban:factor(Region),data=covid3) 
summary(fit_urban1)
#adjusted r-sq = 0.7039


#Model 3: Death,HDI,Region
fit_hdi1 <- lm(Confirmedcases~Deaths+HDI+factor(Region)+Deaths2+Deaths*HDI+Deaths*factor(Region)+HDI*factor(Region)+Deaths*HDI*factor(Region)+Deaths2*factor(Region))
summary(fit_hdi1)
#adjusted r-sq = 0.6836

#Model 4: Death, Density, HDI, Region
fit_den_hdi <- lm(Confirmedcases~ Deaths + Deaths2+HDI+Density+factor(Region)
                  +Deaths*HDI +Deaths*factor(Region)+Deaths*Density 
                  + Deaths*HDI*factor(Region)+Deaths*Density*factor(Region)+Deaths*HDI*Density
                  +Deaths*HDI*Density*factor(Region)
                  +HDI*Density + HDI*factor(Region)+ HDI*Density*factor(Region) 
                  +Density*factor(Region)+Deaths2*factor(Region))
summary(fit_den_hdi)
#adjusted r-sq = 0.767

#Model 5: Death, Density, Urban, Region
fit_den_urban <- lm(Confirmedcases~ Deaths + Deaths2+Urban+Density+factor(Region)
                  +Deaths*Urban +Deaths*factor(Region)+Deaths*Density 
                  + Deaths*Urban*factor(Region)+Deaths*Density*factor(Region)+Deaths*Urban*Density
                  +Deaths*Urban*Density*factor(Region)
                  +Urban*Density + Urban*factor(Region)+ Urban*Density*factor(Region) 
                  +Density*factor(Region)+Deaths2*factor(Region))
summary(fit_den_urban)
#adjusted r-sq = 0.6703

#Interaction plots of full model (model 2)
plot_1<-interact_plot(fit_urban1, pred=Deaths,modx=Region,data=covid3,y.label = 'E(y)',main.title = 'Interaction Plot of Model')
plot_2<-interact_plot(fit_urban1, pred=Urban,modx=Region,data=covid3,y.label = 'E(y)',main.title = 'Interaction Plot of Model')
grid.arrange(plot_1,plot_2,ncol=2)

#Partial F-tests

#Partial F-test with model without interaction terms:
fit.reduced1 <- lm(Confirmedcases~ Deaths + Urban + factor(Region)+ Deaths2)
summary(fit.reduced1)
#adjusted r-sq = 0.6354
anova(fit_urban1,fit.reduced1)
#With p-value = 0.0161 < 0.05 => reject H0 => full model is better

#Partial F-test with model without interaction term between Deaths and others
fit.reduced2 <- lm(Confirmedcases~ Deaths + Urban + factor(Region)+ Deaths2 + Urban*factor(Region))
summary(fit.reduced2)
#adjusted r-sq = 0.5604
anova(fit_urban1,fit.reduced2)
#With p-value = 0.01028 < 0.05 => reject H0 => Full model is better

#Partial F-test with model without interaction term between Urban with others
fit.reduced3 <- lm(Confirmedcases~ Deaths*factor(Region) + Deaths2*factor(Region) + Urban)
summary(fit.reduced3)
#adjusted r-sq = 0.609
anova(fit_urban1,fit.reduced3)
#p-value = 0.1779 > 0.05 => cannot reject H0 => the reduce model is better

#=> Our new model is fit.reduced3 <- lm(Confirmedcases~ Deaths + Urban + factor(Region)+ Deaths2 + Deaths*factor(Region) + Deaths2*factor(Region))

#Check for Assumptions of reduced model
res_density_new = residuals(fit.reduced3)
pred_density_new = predict(fit.reduced3)
plot(pred_density_new,res_density_new,main='Residuals vs Fit Plot', xlab=expression(hat(y)),ylab='Residuals')
abline(0,0)

qqnorm(res_density_new,main='Normal Probability Plot')
qqline(res_density_new)

hist(res_density_new,main='Histogram of Residuals',prob = TRUE,xlab='Residuals')
curve(dnorm(x, mean=mean(res_density_new), sd=sd(res_density_new)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Transform reduced model
fit.reduced3_transformed <- lm(log(Confirmedcases)~ Deaths + Urban + factor(Region)+ Deaths2
                               + Deaths*factor(Region) + Deaths2*factor(Region))
summary(fit.reduced3_transformed)

#Check for Assumptions in transformed reduced model
res_density_tr = residuals(fit.reduced3_transformed)
pred_density_tr = predict(fit.reduced3_transformed)
plot(pred_density_tr,res_density_tr,main='Residuals vs Fit Plot', xlab=expression(hat(y)),ylab='Residuals')
abline(0,0)

qqnorm(res_density_tr,main='Normal Probability Plot')
qqline(res_density_tr)

hist(res_density_tr,main='Histogram of Residuals',prob = TRUE,xlab='Residuals')
curve(dnorm(x, mean=mean(res_density_tr), sd=sd(res_density_tr)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Prediction - We might check with Region: AMRO, Deaths = 60, Urban = 81
newdata = data.frame(Region = 'AMRO', Deaths = 61, Urban = 81, Deaths2 = 61^2)
exp(predict(fit.reduced3_transformed, newdata, interval = 'predict'))

#Since we use nature log for transformation for Confirmedcases, so we need to exp() to bring it back
#Therefore, with Region = AMRO, Deaths =6, Urban = 81, we are 95% confident that the predict of Confirmed cases lies between 1291 cases to 66307 cases

exp(predict(fit.reduced3_transformed,newdata, interval = 'confidence'))
#Therefore, we are 95% confident that the mean of Confirmed cases will lies between 4956 cases to 17275 cases.


#-------------------------------Additional Code---------------------------------------

#Check for Assumptions of all full models
#Model 1
res_density1 = residuals(fit_density1)
pred_density1 = predict(fit_density1)
plot(pred_density1,res_density1)
abline(0,0)

qqnorm(res_density1)
qqline(res_density1)

hist(res_density1)

#Model 2 --> our initial full model
res_urban1 = residuals(fit_urban1)
pred_urban1 = predict(fit_urban1)
plot(pred_urban1,res_urban1)
abline(0,0)

qqnorm(res_urban1)
qqline(res_urban1)

hist(res_urban1)

#Model 3
res_hdi1 = residuals(fit_hdi1)
pred_hdi1 = predict(fit_hdi1)
plot(pred_hdi1,res_hdi1)
abline(0,0)

qqnorm(res_hdi1)
qqline(res_hdi1)

hist(res_hdi1)

#Model 4
res_den_hdi = residuals(fit_den_hdi)
pred_den_hdi = predict(fit_den_hdi)
plot(pred_den_hdi,res_den_hdi)
abline(0,0)

qqnorm(res_den_hdi)
qqline(res_den_hdi)

hist(res_den_hdi)

#Model 5
res_den_urban = residuals(fit_den_urban)
pred_den_urban = predict(fit_den_urban)
plot(pred_den_urban,res_den_urban)
abline(0,0)

qqnorm(res_den_urban)
qqline(res_den_urban)

hist(res_den_urban)

#Transformed full models

#Model 1: Death,Density,Region
fit_density2 <- lm(log(Confirmedcases)~Deaths+Density+factor(Region)+ Deaths2 + Deaths*Density+Deaths*factor(Region)+Density*factor(Region)+Deaths*Density*factor(Region) + Deaths2*factor(Region))
summary(fit_density2)
#adjusted r-sq = 0.5975

#Model 2: Death,Urban,Region
fit_urban2 <- lm(log(Confirmedcases)~Deaths+Urban+factor(Region)+ Deaths2 + Deaths*Urban+Deaths*factor(Region)+Urban*factor(Region)+Deaths*Urban*factor(Region) + Deaths2*factor(Region))
summary(fit_urban2)
#adjusted r-sq = 0.6237

#Model 3: Death,HDI,Region
fit_hdi2 <- lm(log(Confirmedcases)~Deaths+HDI+factor(Region)+Deaths2+Deaths*HDI+Deaths*factor(Region)+HDI*factor(Region)+Deaths*HDI*factor(Region)+Deaths2*factor(Region))
summary(fit_hdi2)
#adjusted r-sq = 0.6225

#Model 4: Death, Density, HDI, Region
fit_den_hdi1 <- lm(log(Confirmedcases)~ Deaths + Deaths2+HDI+Density+factor(Region)
                  +Deaths*HDI +Deaths*factor(Region)+Deaths*Density 
                  + Deaths*HDI*factor(Region)+Deaths*Density*factor(Region)+Deaths*HDI*Density
                  +Deaths*HDI*Density*factor(Region)
                  +HDI*Density + HDI*factor(Region)+ HDI*Density*factor(Region) 
                  +Density*factor(Region)+Deaths2*factor(Region))
summary(fit_den_hdi1)
#adjusted r-sq = 0.6018

#Model 5: Death, Density, Urban, Region
fit_den_urban1 <- lm(log(Confirmedcases)~ Deaths + Deaths2+Urban+Density+factor(Region)
                    +Deaths*Urban +Deaths*factor(Region)+Deaths*Density 
                    + Deaths*Urban*factor(Region)+Deaths*Density*factor(Region)+Deaths*Urban*Density
                    +Deaths*Urban*Density*factor(Region)
                    +Urban*Density + Urban*factor(Region)+ Urban*Density*factor(Region) 
                    +Density*factor(Region)+Deaths2*factor(Region))
summary(fit_den_urban1)
#adjusted r-sq = 0.5879

#After checking the adjusted r-sq after transformation, we decide that we will use the Model 2: Death, Urban, Region as our full model.

#Checking assumptions of transformed full model:
#Model 2
res_urban2 = residuals(fit_urban2)
pred_urban2 = predict(fit_urban2)
plot(pred_urban2,res_urban2)
abline(0,0)

qqnorm(res_urban2)
qqline(res_urban2)

hist(res_urban2, prob = TRUE)
curve(dnorm(x, mean=mean(res_urban2), sd=sd(res_urban2)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Partial F-test of transformed model

#Partial F-test with model without interaction terms:
fit.reduced1 <- lm(log(Confirmedcases)~ Deaths + Urban + factor(Region)+ Deaths2)
summary(fit.reduced1)
#adjusted r-sq = 0.5307

anova(fit_urban2,fit.reduced1)
#With p-value = 0.0123 < 0.05 => reject H0 => full model is better

#Partial F-test with model without interaction term between Deaths and others

fit.reduced2 <- lm(log(Confirmedcases)~ Deaths + Urban + factor(Region)+ Deaths2 + Urban*factor(Region))
summary(fit.reduced2)
#adjusted r-sq = 0.5604

anova(fit_urban2,fit.reduced2)

#With p-value = 0.0332 < 0.05 => reject H0 => Full model is better

#Partial F-test with model without interaction term between Urban with others

fit.reduced3_transformed <- lm(log(Confirmedcases)~ Deaths + Urban + factor(Region)+ Deaths2
                   + Deaths*factor(Region) + Deaths2*factor(Region))
summary(fit.reduced3_transformed)
#adjusted r-sq = 0.609

anova(fit_urban2,fit.reduced3_transformed)

#p-value = 0.2431 > 0.05 => cannot reject H0 => the reduce model is better

#=> Our model is fit.reduced3 <- lm(log(Confirmedcases)~ Deaths + Urban + factor(Region)+ Deaths2 + Deaths*factor(Region) + Deaths2*factor(Region))
