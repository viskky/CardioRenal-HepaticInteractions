
library(survival)
#univariate cox regression
res.cox1=coxph(Surv(time = MACE.interval, event = MACE) ~ Age..yrs., data=df)
summary(res.cox1)


res.cox2=coxph(Surv(time = MACE.interval, event = MACE) ~ Female, data=df)
summary(res.cox2)


#Multivariate cox regression
res.cox3=coxph(Surv(time = MACE.interval, event = MACE) ~ MELD.XI+ Age..yrs. +Female+ NYHA....+Prior.HF.hospitalization+Systolic.blood.pressure..mmHg.+Ischemic.etiology+Ventricular.tachycardia+Anemia+BNP..pg.dL.+LV.EF...., data=df)

summary(res.cox3)
