library(pROC)

# Model 1
Group=df$MACE # 1 = disease, 0 = healthy
df2=data.frame(df$Age..yrs.,df$Female,df$MELD.XI) # Create a data frame of the relevant variables
GLM=glm(Group~df2$df.Age..yrs.+df2$df.Female+df2$df.MELD.XI,data=df2) # Fit generalized linear model with relevant variables
roc1=roc(GLM$y, GLM$fitted.values, ci=T) # Calculate the ROC curve using the fitted values of the model
plot(roc1,col=c('red')) # Plot the ROC curv


# Model 2
Group=df$MACE # 1 = disease, 0 = healthy
df2=data.frame(df$Age..yrs.,df$Female,df$Creatinine..mg.dL.) # Create a data frame of the relevant variables
GLM=glm(Group~df2$df.Age..yrs.+df2$df.Female+df2$df.Creatinine..mg.dL.,data=df2) # Fit generalized linear model with relevant variables
roc2=roc(GLM$y, GLM$fitted.values, ci=T) # Calculate the ROC curve using the fitted values of the model
lines(roc2,col=c('green')) # Plot the ROC curv


# Model 3
Group=df$MACE # 1 = disease, 0 = healthy
df2=data.frame(df$Age..yrs.,df$Female,df$Total.bilirubin..mg.dl.) # Create a data frame of the relevant variables
GLM=glm(Group~df2$df.Age..yrs.+df2$df.Female+df2$df.Total.bilirubin..mg.dl.,data=df2) # Fit generalized linear model with relevant variables
roc3=roc(GLM$y, GLM$fitted.values, ci=T) # Calculate the ROC curve using the fitted values of the model
lines(roc3,col=c('sky blue')) # Plot the ROC curv

legend('bottomright', legend=c('Model 1 (age,gender,MELD-XI','Model 1 (age,gender,Cre','Model 1 (age,gender,T-Bil'), lty = 1,lwd=4, col=c('red','green','skyblue'),bty='n')
