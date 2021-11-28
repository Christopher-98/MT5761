
admin<-read.csv('admissions.csv')
head(admin)
summary(admin)
admin$admission <- as.factor(admin$admission)
admin$white <- as.factor(admin$white)
admin$died <- as.factor(admin$died)
admin$age80 <- as.factor(admin$age80)
levels(admin$admission)


plot(admin$los, admin$admission)

plot(admin$los,admin$age)

plot(admin$los, admin$provnum)

plot(admin$white, admin$admission)
plot(admin$died, admin$admission)
plot(admin$age80, admin$died)
table(admin$age80, admin$died)

library(nnet)
# fit full model with all terms
mod.full <- multinom(admission ~ age + died + white + los + age80,
                     family = multinomial, data = admin)

summary(mod.full)

mod.full.fac <- multinom(admission ~ as.factor(age) + died + white + los + age80,
                     family = multinomial, data = admin)


library(car)

vif(mod.full)

AIC(mod.full)
AIC(mod.full.fac)
AIC

# AIC implies full model without age as factor is better

Anova(mod.full)
# suggests age is not a significant factor so remove

mod <- update(mod.full, .~. -age)
summary(mod)

Anova(mod)
# anova suggests age 80 is not significant either

mod <- update(mod, .~. -age80)

Anova(mod)

vif(mod)
# all covariates valid at 0.01 level

require(MuMIn)
options(na.action = 'na.fail')
head(dredge(mod.full))

# this supports anova analysis

# test effect of interaction terms
mod <- update(mod, .~. +los:white + died:white + died:los)

Anova(mod)
# all interactions were not signigicant so removed

mod <- update(mod, .~. -los:white - died:white - died:los)

z <- summary(mod)$coefficients/summary(mod)$standard.errors

p <- (1-pnorm(abs(z))) * 2
p

coef(mod)

exp(coef(mod))

1/exp(coef(mod))[,1]

# table of coefficents and ci's
em_coef <- coef(mod)[1,]
ur_coef <- coef(mod)[2,]

em_err <- summary(mod)$standard.errors[1,]
ur_err <- summary(mod)$standard.errors[2,]

em_ci_low <- exp(em_coef  -1*qnorm(0.975)*em_err)
em_ci_up <- exp(em_coef + 1*qnorm(0.975)*em_err)

ur_ci_low <- exp(ur_coef -1*qnorm(0.975)*ur_err)
ur_ci_up <- exp(ur_coef + 1*qnorm(0.975)*ur_err)

emergency <- cbind(exp(em_coef), em_ci_low, em_ci_up)
urgent <- cbind(exp(ur_coef), ur_ci_low, ur_ci_up)

library(effects)

plot(effect("los", mod), main = 'Figure 1: Length of stay effect')
# as length of stay increases, the prob of elective decreases and emergency rises 

plot(effect("died", mod))
plot(effect("white", mod))


# model assumptions


# independance uncertain as provnum used multiple times
# however white and died data suggests these were used for different patients

# plot log odds vs length of stay as only continuous variables
los <- quantile(admin$los, seq(0, 1, length.out = 12))
prob_em <- NA
prob_ur <- NA
meds <- NA
for (i in 1:length(los)-1) {
  tab <- table(admin$admission[admin$los < los[i+1] & admin$los >= los[i]])
  prob_em[i] <- log(tab[2]/tab[1])
  prob_ur[i] <- log(tab[3]/tab[1])
  meds[i] <- median(admin$los[admin$los < los[i+1] & admin$los >= los[i]])
}

plot(meds, prob_em,
     main = 'Figure 1: LO Emergency vs LoS',
     xlab = 'Length of Stay',
     ylab = 'Log Odds Emergency vs Elective')
lines(smooth.spline(meds,prob_em,df=4))

plot(meds, prob_ur,
     main = "Figure 2: LO Urgent vs LoS",
     xlab = 'Length of Stay',
     ylab = 'Log Odds Urgent vs Elective')
lines(smooth.spline(meds,prob_ur,df=4))

# confusion matrix
preds <- predict(mod)


require(caret)
confusionMatrix(preds, admin$admission)

# create variable for saturated mod

require(VGAM)
mod.dev <- vglm(admission ~ died + white + los, data = admin, family = multinomial)

dev <- deviance(mod.dev)
summary(mod.dev)
n <- dim(admin)[1]
p <- length(coef(mod.dev))
1-pchisq(dev, n-p)
# tiny p-value


# mfaddens r squared

mod.null <-vglm(admission ~ 1, data = admin, family = multinomial)

Mc_R2 <-  1-logLik(mod.dev)/logLik(mod.null)
