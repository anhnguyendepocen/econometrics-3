# Unit 4 code from book

# there are several ways to go about assessing a regression
  # 1.) leave on out statistics
  # 2.) Diagnostic tests
  # 3.) robust covariance matrix estimators

# graphical structures can be very good for identifying issues

# resistant regression techniques also good although less known by econometricians

#########
# 4.1- regression diagnostics
#########

# purpose is to find points with undo unfluence on the regression model or that are not fitted as well

library(sandwich)

data("PublicSchools")

summary(PublicSchools)

# first omit na values

ps <- na.omit(PublicSchools)
ps$Income <- ps$Income / 10000
# regressionL
ps_lm <- lm(Expenditure ~ Income, data = ps)
plot(Expenditure~ Income, data = ps, ylim = c(230, 830))
# add a regression line:
abline(ps_lm)
id<- c(2, 24 ,48)
text(ps[id, 2:1], rownames(ps)[id], pos = 1, xpd = TRUE)

# can see that alaska, DC, and mississippi may pose an issue in the dataset

# can plot the resudiuals:

par(mfrow = c(3,2))

plot(ps_lm, which = 1:6)

# the first plot shows whether the residuals are fitted appropriately - homoskedasticyty

# the second plot QQ, shows whether the residuals follow a normal distribution

# the third plot - scale location looks at the sqrt of hte standardized (studentized residuals) versus the y estimates- 
# - useful for checking assumption that errors are dentically distributed - variance is homogenous

# in all plots, Alaska stands out as being an outlier:

# in top left plot, Alaska has a large residual
# appears in the upper tail of the distripution (qqplot)

# homogenous variance seems to be violated (middle left)

# large cook's distance middle right bottom right)

# bottom row,- large leverage

### Leverage and standardized residuals

# in least squared residuals are not independent and do not have the same variance - coveraiance matrix is hte hat matrix

# hatvalues() gives the generic diagonlas of hte function
# hence residuals for such values are close to zero- this means that hii measure s the leverage of the observation i 
# large = 2-3 times the average

# there are 2 types of values - large hii and unusual yi - possibly bad; large hii and normal yi - good for model

par(mfrow=c(1,1))
ps_hat<- hatvalues(ps_lm)

plot(ps_hat)
abline(h - c(1,3), mean(pf_hat), col = 2)
id<- which(ps_hat > 3 * mean(ps_hat))
text(id, ps_hat[id], rownames(ps)[id], pos =1, xpd = TRUE) # label the outliers

# ri are the standardized residuals, rstandard()

# same as internally- studentized residuals; not to confuse with externally studentized residuals


### Deletion diagnostics - leave one out

# excluding a specific observaton in each iteration
# any observation whose removal causes a big swing n the data is said to be highly influential - 
  #may or may not have large leverage

# rstudent() - externally studentized residuals

### the function influence.measures()

# with 50 obs and 2 regressors the diagonal element of hte hat matrix average is 0.04 - 

which(ps_hat>3 * mean(ps_hat))

# get 2 values

# observations that are potentially influential accourding at least one criterion:

summary(influence.measures(ps_lm))

# noteworthy that AK stands out by any means of measure of influence:

plot(Expenditure~Income, data = ps, ylim = c (230, 830))
abline(ps_lm)

id<- which (apply(influence.measures(ps_lm)$is.inf, 1, any))
text(ps[id, 2:1], rownames(ps)[id], pos = 1, xpd = TRUE)
ps_noinf <- lm(Expenditure ~ Income, data = ps[-id, ])
abline(ps_noinf, lty = 2)

#########
# 4.2- diagnostic tests
#########

# more formal approach to validating regression models is with diganostic tests
# cross section data plagued with heteroskedasticity; econometric data plagued by residual autocorrelction

# uses the lmtest package

library(AER)
data("Journals")

journals<- Journals[, c("subs","price")]
journals$citeprice <- Journals$price / Journals$citations
journals$age <- 2000 - Journals$foundingyear

# consider a simple model that explains the log of the number of subs by the log of price per citation

jour_lm <- lm(log(subs) ~ log(citeprice), data = journals)


### testing for heteroskedasticity

# testing for constant variacne - Breusch-Pagan test:

# fits linear regression model to the squared residuals and rejects if too much of this variance is explained
# by additional explanatory variables

bptest(jour_lm)


# the WHite test picks up the heterol uses the original regressors as well as their squares and 
  #interactions in the aux regression
# passed as a second formula to bptest

bptest(jour_lm, ~log(citeprice) + I(log(citeprice)^2), data = journals)


# goldfield- quandt test:
# splits the data into two and says if there is a difference(f-test) then we should be able to ; takes the center of the 
# data

gqtest(jour_lm, order.by = ~citeprice, data = journals)


### testing the functional form

# assumtion of erro = 0 is essential

# ramsey's reset test does this test regression specification error test


resettest(jour_lm)
# result is clearly not significatn = no misspecification


# the rainbow test:
# even a misspecified model might be able to fit the data reasonably well in the center of the sample 
# but might lack fit in the tails, fits model to subsample
# and comapres to the model fitted with the full sample using an F-test or mahal distance

raintest(jour_lm, order.by = ~age, data = journals)

# Harvey collier test:

# relies on ordering hte data prior to analysis
# recursively fits data : recursive redisuals calculated and are essentialyl standarized one teps ahead prediction errors
# simple t-test for zero mean

harvtest(jour_lm, order.by = ~age, data = journals)
# confirms results from rainbow test - emphasized that age of journals is signiicant

### testing for autocorrelation:

# often, timeseries impacted by serial correlation

data("USMacroG")
library(dynlm)

consump1 <- dynlm(consumption ~dpi + L(dpi), data = USMacroG)

# Durbnin Watson test for autocorrel:

# ratio od SS first difference of hte residuals ad resid SS

dwtest(consump1)

# also the box -pierse and box ljung tests:

Box.test(residuals(consump1), typ ='Ljung-Box')

# breusch-godfrey test for serial correlation of order 1:

# more detail on auto-correlation in chapter 6


##########
# 4.3 Robust standard errors and tests
##########


# today there have been many preocedures createdf for heteroskedasticity and heteroskedasticity with autocorrelation consistent procedures
# the problem is that standard t and f tests performed for a fitted linear moade assume the errors 
# are homoskedastic and uncorrelated 
# sandwich package includes the functions vcovHC() and vcovHAC() - these can be researched further in Zeileis(2004)

# HC estimators

# both the vcovhc and the vcov give about the same answers:

vcov(jour_lm)
vcovHC(jour_lm)

coeftest(jour_lm, vcov = vcovHC)

# to illustrate how the robust covariance matrices can be used, let's compare a simple versus quadratic model

ps_lm<- lm(Expenditure ~ Income, data = ps)
ps_lm2 <- lm(Expenditure ~ Income + I(Income^2), data =ps)
anova ( ps_lm, ps_lm2)

# the significance seen here is simply caused by one spurious outlier


waldtest(ps_lm, ps_lm2, vcov = vcovHC(ps_lm2, type = "HC4"))


# this shows that the quadratic term is in fact not significant; an equivalent result can be obtained
# via coeftest(ps_lm2, vcov = vcovHC(ps_lm2, type = "HC4"))- p.109

### HAC estimators

# pretty much already covered with coeff and wald test