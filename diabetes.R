# Read the dataset present in the 'data' folder
diabetes<-read.table("data/diabetes.txt",header=F,sep=",")
# Set the appropriate column names
colnames(diabetes)<-c("pregnant","glucose","bp","triceps","insulin","bmi","pedigree","age","outcome")
# Display the correlation matrix
cor(diabetes)

# Figure out which columns have missing values
diabetes$glucose[diabetes$glucose==0]<-NA
diabetes$bp[diabetes$bp==0]<-NA
diabetes$triceps[diabetes$triceps==0]<-NA
diabetes$insulin[diabetes$insulin==0]<-NA
diabetes$bmi[diabetes$bmi==0]<-NA

# Factorize the target class
diabetes$outcome<-factor(diabetes$outcome)
levels(diabetes$outcome)<-c("neg","pos")

# Display summary statistics
summary(diabetes)

label<-c("neg","pos")

# Replace all the missing values with the median values of the columns
cc<-is.na(diabetes[,c(2)])
m<-which(cc==c("TRUE"))
diabetes$glucose[m]<-117.0
cc<-is.na(diabetes[,c(3)])
m<-which(cc==c("TRUE"))
diabetes$bp[m]<-72.00
cc<-is.na(diabetes[,c(4)])
m<-which(cc==c("TRUE"))
diabetes$triceps[m]<-29.00
cc<-is.na(diabetes[,c(5)])
m<-which(cc==c("TRUE"))
diabetes$insulin[m]<-125.00
cc<-is.na(diabetes[,c(6)])
m<-which(cc==c("TRUE"))
diabetes$bmi[m]<-32.30

# Exploratory analysis of the dataset
par(mfrow=c(3,3))
boxplot(diabetes$age[diabetes$outcome=="pos"],diabetes$age[diabetes$outcome=="neg"],names=label,main="Age")
hist(diabetes$pregnant[diabetes$outcome=="pos"],breaks=seq(-0.5,max(diabetes$pregnant)+0.5),ylim=c(0,120),xlab="No of times pregnant",main="Diabetic")
hist(diabetes$pregnant[diabetes$outcome=="neg"],breaks=seq(-0.5,max(diabetes$pregnant)+0.5),ylim=c(0,120),xlab="No of times pregnant",main="Non-Diabetic")
boxplot(diabetes$glucose[diabetes$outcome=="pos"],diabetes$glucose[diabetes$outcome=="neg"],names=label,main="Glucose Concentration")
boxplot(diabetes$insulin[diabetes$outcome=="pos"],diabetes$insulin[diabetes$outcome=="neg"],names=label,main="Insulin")
boxplot(diabetes$bp[diabetes$outcome=="pos"],diabetes$bp[diabetes$outcome=="neg"],names=label,main="Blood Pressure")
boxplot(diabetes$triceps[diabetes$outcome=="pos"],diabetes$triceps[diabetes$outcome=="neg"],names=label,main="Skin Fold Thickness")
hist(diabetes$bmi[diabetes$outcome=="pos"],breaks=seq(-0.5,max(diabetes$bmi)+0.5),ylim=c(0,60),xlab="Body Mass Index",main="Diabetic")
hist(diabetes$bmi[diabetes$outcome=="neg"],breaks=seq(-0.5,max(diabetes$bmi)+0.5),ylim=c(0,60),xlab="Body Mass Index",main="Non-Diabetic")

# Fit a logistic regression model
full.fit<-glm(outcome~pregnant+glucose+bp+triceps+insulin+bmi+pedigree+age,data=diabetes,family=binomial)
summary(full.fit)

# Fit a logistic regression model to only the statistically significant features
reduced<-glm(outcome~pregnant+glucose+bmi+pedigree,data=diabetes,family=binomial)
summary(reduced)

# Predict the classes and generate the confusion matrix
oddsratio<-round(exp(reduced$coef),4)

obj<-diabetes[,c(1,2,6,7)]
predict<-predict(reduced,obj,type="response")
pp<-ifelse(predict(reduced,obj,type="response")>0.6,1,0)

xtabs(~pp+diabetes[,9])