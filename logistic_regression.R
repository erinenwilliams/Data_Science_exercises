## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
Factor w/ 5 levels "1 Yes","2 No",..: 2 2 1 2 2 1 2 2 1 2 ...
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

Estimate   Std. Error    z value     Pr(>|z|)
(Intercept) -4.269466028 0.0564947294 -75.572820 0.000000e+00
age_p        0.060699303 0.0008227207  73.778743 0.000000e+00
sex2 Female -0.144025092 0.0267976605  -5.374540 7.677854e-08
sleep       -0.007035776 0.0016397197  -4.290841 1.779981e-05
bmi          0.018571704 0.0009510828  19.526906 6.485172e-85
## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

Estimate   Std. Error    z value     Pr(>|z|)
(Intercept) 0.01398925 0.0564947294 -75.572820 0.000000e+00
age_p       1.06257935 0.0008227207  73.778743 0.000000e+00
sex2 Female 0.86586602 0.0267976605  -5.374540 7.677854e-08
sleep       0.99298892 0.0016397197  -4.290841 1.779981e-05
bmi         1.01874523 0.0009510828  19.526906 6.485172e-85
## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.
age_p      sex      bmi   sleep       fit      se.fit residual.scale
1    33 2 Female 29.89565 7.86221 0.1289227 0.002849622              1
2    63 2 Female 29.89565 7.86221 0.4776303 0.004816059              1
>
## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
# collapse all missing values to NA
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))
# run our regression model
wrk.out <- glm(everwrk~age_p+r_maritl,
data=NH11, family="binomial")
coef(summary(wrk.out))

Estimate  Std. Error    z value     Pr(>|z|)
(Intercept)                                  0.44024757 0.093537691  4.7066328 2.518419e-06
age_p                                        0.02981220 0.001645433 18.1181481 2.291800e-73
r_maritl2 Married - spouse not in household -0.04967549 0.217309587 -0.2285932 8.191851e-01
r_maritl4 Widowed                           -0.68361771 0.084335382 -8.1059419 5.233844e-16
r_maritl5 Divorced                           0.73011485 0.111680788  6.5375152 6.254929e-11
r_maritl6 Separated                          0.12809081 0.151366140  0.8462316 3.974236e-01
r_maritl7 Never married                     -0.34361068 0.069222260 -4.9638756 6.910023e-07
r_maritl8 Living with partner                0.44358296 0.137769623  3.2197443 1.283050e-03
r_maritl9 Unknown marital status            -0.39547953 0.492966577 -0.8022441 4.224118e-01
##   2. Predict the probability of working for each level of marital
##      status.
# Create a dataset with predictors set at desired levels
predData <- with(NH11,
expand.grid(r_maritl = c("1 Married - spouse in household", "2 Married - spouse not in household", "4 Widowed", "5 Divorced", "6 Separated", "7 Never married", "8 Living with partner", "9 Unknown marital status"),
age_p = mean(age_p, na.rm = TRUE)))
# predict likelihood of working at those levels
cbind(predData, predict(wrk.out, type = "response",
se.fit = TRUE, interval="confidence",
newdata = predData))

r_maritl                              age_p       fit      se.fit          residual.scale
1     1 Married - spouse in household 48.10983 0.8669790 0.004999417              1
2 2 Married - spouse not in household 48.10983 0.8611449 0.025466108              1
3                           4 Widowed 48.10983 0.7669001 0.013322250              1
4                          5 Divorced 48.10983 0.9311585 0.006624603              1
5                         6 Separated 48.10983 0.8810696 0.015188109              1
6                     7 Never married 48.10983 0.8221375 0.007606786              1
7               8 Living with partner 48.10983 0.9103642 0.010621449              1
8            9 Unknown marital status 48.10983 0.8144257 0.074230320              1
>
This data tells us that a married person with a spouse in the household has an 87% probability of ever having worked, a married person whose spouse does not live in the household has an 86% probability of ever having worked. Widowed people have the lowest probability of ever having worked, at 77%, while divorced people have the highest probability at 93%. Those who are separated have an 88% likelihood, those who were never married have an 82% probability of ever having worked, and those living with a partner have a 91% probability. Finally, those with an unknown status have an 81% probability of ever having worked.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
