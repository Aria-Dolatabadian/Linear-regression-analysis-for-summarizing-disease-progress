#Linear regression analysisfor summarizing disease progress


#Linear regression in R

## Disease severity as a function of temperature
# Response variable, disease severity
diseasesev<-c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4)
# Predictor variable, (Centigrade)
temperature<-c(2,1,5,5,20,20,23,10,30,25)
# Take a look at the data
plot(temperature,diseasesev)

## For convenience, the data may be formatted into a dataframe
severity <- as.data.frame(cbind(diseasesev,temperature))
## Fit a linear model for the data and summarize
##   the output from function lm()
severity.lm <- lm(diseasesev~temperature,data=severity)
## Generate a summary of the linear model
summary(severity.lm)


#Graphical tools for testing assumptions

## which=1 produces a graph of residuals vs fitted values
plot(severity.lm, which=1)

## which=2 produces a graph of a quantile-quantile (QQ) plot
plot(severity.lm,which=2)

options(digits=4)
fit.with.se<-predict(
        severity.lm,
        se.fit=TRUE
)
data.frame(
        severity,
        fitted.value=predict(severity.lm),
        residual=resid(severity.lm),
        fit.with.se
)

plot(
 diseasesev~temperature,
        data=severity,
        xlab="Temperature",
        ylab="% Disease Severity",
        pch=16
)
abline(severity.lm,lty=1)
title(main="Graph of % Disease Severity vs Temperature")

## Predict disease severity for three new temperatures
new <- data.frame(temperature=c(15,16,17))
predict(
        severity.lm,
        newdata=new,
        interval="confidence"
)

#https://www.apsnet.org/edcenter/disimpactmngmnt/topc/EcologyAndEpidemiologyInR/DiseaseProgress/Pages/LinearRegression.aspx
