###################
### Unit 5 Code ###
###################

##########
### 4.4 Resistant Regression
##########

# sometimes we may use leave on out diagnistocis, but multiple values can mask eachother
# this is why with high dimensional problems we can use resistance regression

# regressions that can withstand small alterations in the dataset

# least median squares and least trimmed squares

# least trimmed squares tends to be the better choice

# use the example of human capital

data ("OECDGrowth")
solow_lm<- lm(log(gdp85/gdp60) ~log(gdp60) + log(invest) + log(popgrowth + 0.05), data = OECDGrowth )
summary(solow_lm)

# recommend running LTS regression, flagging unusual obs and then running OLS; but LTS may flag too many

# least trtimmed regression (LTS)
# can have points that arelow leverage but unusual yi that are still alright to leave in the analysis because they are not influential

# can determine which ones by looking at LTS residuals

# in MASS package

library(MASS)

solow_lts<- lqs(log(gdp85/gdp60) ~log(gdp60) + log(invest) + log(popgrowth + 0.05), data = OECDGrowth, 
                psamp = 13, # trim 9 of the 22 observations by choosing nsamp exact
                nsamp = 'exact' )
# minimize SS by taking every possible subsample of size 13


summary(solow_lts)

smallresid <- which (abs(residuals(solow_lts) / solow_lts$scale[2]) <=2.5)

# now, minimum volume ellipsoid is a good approachto identifying leverage points in a robust manner

x<- model.matrix(solow_lm)[,-1]

xcv<- cov.rob(x, nsamp = 'exact')

nohighlev <- which (sqrt(mahalanobis(x, xcv$center, xcv$cov)) <= 2.5)  # good observations are either small resid or low leverage

# 2.5 is the number of times are large as the original estimate in absolute terms it is 2.5 times considered bad



# find these leverage points with small resid or low leverage with 

goodobs <- unique(c(smallresid, nohighlev))


rownames(OECDGrowth)[-goodobs]
# there are 4 leverage points here



solow_rob <- update(solow_lm, subset = goodobs)
summary(solow_rob)

# notice that the population growth does not appear to be significant now that we took out the observations that 
# had a large amount of population growth - may be influential if the sets of countries were broader


##########
### 4.4 Quantile Regression
##########

# OLS can be thought of as method for modeling a conditional mean response, othertimes we may want a median or quantiles 

# is a linear programming problem and is fitted  via the quantreg package


library(quantreg)
data('CPS1988')
cps_f<- log(wage) ~ experience + I(experience^2) + education
cps_lad <- rq(cps_f, data = CPS1988)
summary(cps_lad)

# can output the different quantiles of the regression:

cps_rq<- rq(cps_f, tau = c(0.25, 0.75), data = CPS1988)
summary(cps_rq)

# outputs both quantiles

# the next question is whether the regression surfaces are parallel, whether effects of regressors 
# are uniform across quantiles

cps_rq25 <- rq(cps_f, tau = 0.25, data = CPS1988)
cps_rq75 <- rq(cps_f, tau = 0.75, data = CPS1988)
anova(cps_rq25, cps_rq75)

# effects are not uniform across quantiles

# useful to see the results plotted, can plot them with summary

cps_rqbig <- rq(cps_f, tau = seq(0.05, 0.95, by = 0.05), data = CPS1988)
cps_rgbigs <- summary(cps_rqbig)
plot(cps_rgbigs)
