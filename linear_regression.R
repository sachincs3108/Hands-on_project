#simple linear regression
df=read.csv("D:/datascience/rstudio/Admission_Predict.csv")
head(df)
colnames(df)
plot(df)
#Auto-correlation
mat=cor(df)
mat
library("corrplot")
corrplot(mat,method="number")

#splitting a dataset
s=sample(nrow(df),.8*nrow(df))
s
nrow(df)
df_tr=df[s,]
df_test=df[-s,]

#regression model building
model1=lm(Chance.of.Admit~CGPA,data=df_tr)
summary(model1)

#interpreting Rsquare
#between 0 to 1 better the value towards 1 better the correlation
#calculation
pl=predict(model1,df_test)
p1
r_sq=cor(df_test$Chance.of.Admit,pl)**2#actual y with predicted y the whole square
r_sq#calculated manually for accuracy of model
ac_pred=data.frame("actual"=df_test$Chance.of.Admit,"predicted"=pl)
ac_pred.head()

#min_max_accuracy
min_max_accuracy=mean(apply(ac_pred,1,min)/apply(ac_pred,1,max))
min_max_accuracy


#multiple linear regression- forward selection
model2=lm(Chance.of.Admit~CGPA+GRE.Score+TOEFL.Score,data=df_tr)
summary(model2)
p2=predict(model2,df_test[-9])
p2
r_sq2=cor(df_test$Chance.of.Admit,p2)**2
r_sq2

#backward elimination method
df=df[,-1]
head(df)
set.seed(100)
s=sample(nrow(df),.8*nrow(df))
s
df_tr=df[s,]
df_test=df[-s,]
model3=lm(Chance.of.Admit~.,data=df_tr)
summary(model3)
p3=predict(model3,df_test)
p3
r_sq2=cor(df_test$Chance.of.Admit,p3)**2
r_sq2
library(moments)
plot(density(model3$residuals))
skewness(model3$residuals)
plot(density(df_tr$Chance.of.Admit))
skewness(df_tr$Chance.of.Admit)
install.packages("psych")
library(psych)
describe(df_tr)
plot(model3)
#backward elimination
model4=lm(Chance.of.Admit~.-University.Rating-SOP,data=df_tr)
summary(model4)
p=predict(model4,df_test)
p
r_sq2=cor(df_test$Chance.of.Admit,p)**2
r_sq2
 
