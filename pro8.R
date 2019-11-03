#Display the data using a scatterplot.

data=read.table("http://media.pearsoncmg.com/aw/aw_sharpe_business_3/datasets/txt/GDP_2013.txt",sep="\t",header=T)
names(data)=c("year","gdp")
attach(data)
plot(year,gdp,ylim=c(0,14),main="scatter plot for gdp and year",col.main="green",col.lab="purple",cex=1,pch=1,col=6)

#Fit a simple linear regression model.

model=lm(gdp~year)
model

Call:
  lm(formula = gdp ~ year)

Coefficients:
  (Intercept)         year  
-387.8433       0.1993  

#Add the fitted line to the scatter plot.

abline(model,lw=3,col=117)

#Determine the coefficient of determination.

summary(model)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -3.878e+02  9.217e+00  -42.08   <2e-16 ***
  year         1.993e-01  4.651e-03   42.84   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6874 on 62 degrees of freedom
Multiple R-squared:  0.9673,	Adjusted R-squared:  0.9668 
F-statistic:  1835 on 1 and 62 DF,  p-value: < 2.2e-16

summary(model)$r.squared


#Analyze the residual plots. Is your model questionable?

par(mfrow=c(2,2))
plot(model)

#as there is a structured plot b/w residulas and fitted values,we can say that our model is questionable.


## Use Box-Cox Transformation to see whether this model can be improved.

library(MASS)
b=boxcox(model)
#let us assume the value of lamda=0.25 according to the graph
newgdp=gdp^(0.25)
m2=lm(newgdp~year)
m2
Call:
  lm(formula = newgdp ~ year)

Coefficients:
  (Intercept)         year  
-22.66422      0.01224  

            ###########  gdp=-(22.66422+0.01224*year)^4 ##########
summary(m2)$r.squared
summary(model)$r.squared

#we can see that our model is improved from 96.7% to 99.5%
#when we will predict any value ,we will multiply the equation^4