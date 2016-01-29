# Assignment: Regression Models Course Project

library(datasets)
data(mtcars)

# Summaries
?mtcars
summary(mtcars)
str(mtcars)
dim(mtcars)
head(mtcars)
tail(mtcars)

# Formatting factors for analysis (See help page ?mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
## http://stackoverflow.com/questions/18617174/r-mtcars-dataset-meaning-of-vs-variable
mtcars$vs <- as.factor(mtcars$vs)
levels(mtcars$vs) <- c("V-engine", "Straight engine")
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Automatic", "Manual")


# Pair plot
require(GGally)
require(ggplot2)
g <- ggpairs(mtcars, 
             lower = list(continuous = wrap("smooth"))) ##, method = "loess"
g

# Plotting am vs mpg

# g2 <- ggplot(mtcars, aes(x = am, y = mpg, fill = am)) + 
#     geom_boxplot() +
#     labs(title = "Fuel consumption and transmission types", 
#          x = "Transmission", y = "Miles/US gallon (mpg)")
# g2

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
boxplot(mpg ~ am, data = mtcars,
        col = c("coral", "aquamarine"),
        xlab = "Transmission type",
        ylab = "Miles/US gallon (mpg)",
        main = "Fuel consumption by transmission types")

# Linear model
fitPrimary <- lm(mpg ~ am, data = mtcars)
summary(fitPrimary)
summary(fitPrimary)$coefficients
##              Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) 17.147368   1.124603 15.247492 1.133983e-15
## factor(am)1  7.244939   1.764422  4.106127 2.850207e-04
summary(fitPrimary)$r.squared
## [1] 0.3597989 => 36% of the variability on MPG is explained by transmission

# Complete model
fitComplete <- lm(mpg ~ ., data = mtcars)
summary(fitComplete)
summary(fitComplete)$coefficients
summary(fitComplete)$adj.r.squared
## [1] 0.7790215 => 78% of the var. on MPG is explained by the complete model

# Another approach... nested models!
## am is the variable investigated
## According to a quick research, another variables related to fuel consumption:
## - hp: gross horsepower
## - wt: weight
## - qsec: 1/4 mile time
## - gear: number of forward gears
## - carb: number of carburetors
fit2 <- update(fitPrimary, mpg ~ am + hp)
fit3 <- update(fitPrimary, mpg ~ am + hp + wt)
fit4 <- update(fitPrimary, mpg ~ am + hp + wt + qsec)
fit5 <- update(fitPrimary, mpg ~ am + hp + wt + qsec + gear)
fit6 <- update(fitPrimary, mpg ~ am + hp + wt + qsec + gear + carb)

anova(fitPrimary, fit2, fit3, fit4, fit5, fit6)

# According to ANOVA results, the models 2 and 3 are statistically significant
summary(fit2)$coefficients
##               Estimate  Std. Error   t value     Pr(>|t|)
## (Intercept) 26.5849137 1.425094292 18.654845 1.073954e-17
## amManual     5.2770853 1.079540576  4.888270 3.460318e-05
## hp          -0.0588878 0.007856745 -7.495191 2.920375e-08
summary(fit2)$adj.r.squared
## 0.7670025 => am and hp explains 77% of the variability...

summary(fit3)$coefficients
##                Estimate  Std. Error   t value     Pr(>|t|)
## (Intercept) 34.00287512 2.642659337 12.866916 2.824030e-13
## amManual     2.08371013 1.376420152  1.513862 1.412682e-01
## hp          -0.03747873 0.009605422 -3.901830 5.464023e-04
## wt          -2.87857541 0.904970538 -3.180850 3.574031e-03
summary(fit3)$adj.r.squared
## 0.8227357 => am, hp and wt explains 82% of the variability...

# Residuals
par(mfrow = c(2, 2), mar = c(5, 4, 2, 2))
##plot(fitPrimary)
plot(fit2)
plot(fit3)

# Diagnostics
?influence.measures

## Model 2: 
round(dfbetas(fit2)[, 2], 3) 
### dfbetas are small, no points with high potential for influence

round(hatvalues(fit2), 3)
### "Maserati Bora" has high leverage but no influence

## Model 3: 
round(dfbetas(fit3)[, 2], 3)
### dfbetas are small, no points with high potential for influence

round(hatvalues(fit3), 3)
### "Maserati Bora" has high leverage but no influence

# Model selection: SELECT MODEL 3!!!