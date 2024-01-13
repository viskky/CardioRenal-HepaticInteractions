library(prodlim)
library(survival)

df$MELD.XI.score = ifelse(df$MELD.XI >=11, "1", "0") # 0 = Low, 1 = High 
df$MELD.XI.score = factor(df$MELD.XI.score)

LowM=subset(df, subset=MELD.XI.score == "0")
HighM=subset(df, subset=MELD.XI.score == "1")


Group=df$MELD.XI.score # 0 = low MELD-XI score, 1 = high MELD-XI score
km0 = prodlim(Hist(time = MACE.interval, event = MACE)~Group, data = df)
plot(km0, ylab = "Event-free survival (%)", xlim = c(0,1000), axis2.las = 2, legend = FALSE,
     confint = FALSE, background = FALSE, marktime = FALSE, col = c("lightblue", "red"),
     percent = FALSE, atrisk.title="Number at risk", xlab = "",
     atrisk.labels = c("Low MELD-XI", "High MELD-XI"), atrisk.col = 1, lty = c(1,3), atrisk.times = c(0,200,400,600,800,1000))
title(main = "MACE") # set title

survdiff(Surv(time = MACE.interval, event = MACE)~Group, data = df) # Log-rank test
text(x = 600, y = 0.1,"Log-rank p<0.001", font = 2) # insert text
text(x = 300, y = 0.3, "High MELD-XI (>=11)", font = 2) # Insert text
text(x = 800, y = 0.8, "Low MELD-XI (<11)", font = 2) # Insert text


#using survival package for Figure 2.

survobj=Surv(df$MACE.interval,df$MACE)
fit=survfit(survobj~Group,data=df,conf.type='plain')

summary(fit)
plot(fit,  ylab = "Event-free survival (%)", xlim = c(0,1000), axis2.las = 2, legend = FALSE,
     confint = FALSE, background = FALSE, marktime = FALSE, col = c("lightblue", "red"),
     percent = FALSE, atrisk.title="Number at risk", xlab = "",
     atrisk.labels = c("Low MELD-XI", "High MELD-XI"), atrisk.col = 1, lty = c(1,3), atrisk.times = c(0,200,400,600,800,1000))

title(main = "MACE") # set title
survdiff(Surv(time = MACE.interval, event = MACE)~Group, data = df) # Log-rank test
text(x = 600, y = 0.1,"Log-rank p<0.001", font = 2) # insert text
text(x = 300, y = 0.3, "High MELD-XI (>=11)", font = 2) # Insert text
text(x = 800, y = 0.8, "Low MELD-XI (<11)", font = 2) # Insert text



