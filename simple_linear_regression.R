x=c(34,105,64,88,99,51)
y=c(5,17,11,8,14,5)  
df=as.data.frame(cbind(x,y))
df
plot(df$x,df$y)
model1=lm(y~x,data=df)
summary(model1)
p=predict(model1,df) 
p
  
