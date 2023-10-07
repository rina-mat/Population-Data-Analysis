library(ggplot2)
library(dplyr)
library(xlsx)
library(plm)
library(lmtest)
library(sandwich)
library(Hmisc)
library(lme4)
library(foreign)
library(memisc)
library(sjstats)
library(sjPlot)
library(nloptr)
library(stargazer)
library(clubSandwich)
library(aTSA)
library(tseries)

dataBD <- read.csv("urban and gdp data for Bangladesh OLS.csv")
dataBTN <- read.csv("urban and gdp data for Bhutan OLS.csv")
dataCHN <- read.csv("urban and gdp data for China OLS.csv")
dataIND <- read.csv("urban and gdp data for India OLS.csv")
dataIDN <- read.csv("urban and gdp data for Indonesia OLS.csv")
dataJPN <- read.csv("urban and gdp data for Japan OLS.csv")
dataNPL <- read.csv("urban and gdp data for Nepal OLS.csv")
dataPAK <- read.csv("urban and gdp data for Pakistan OLS.csv")
dataPHL <- read.csv("urban and gdp data for Philippines OLS.csv")
dataSGP <- read.csv("urban and gdp data for Singapore OLS.csv")
dataSKR <- read.csv("urban and gdp data for South Korea OLS.csv")
dataVTN <- read.csv("urban and gdp data for Vietnum OLS.csv")

mergedData <- rbind(dataBD, dataBTN, dataCHN, dataIND, dataIDN, dataJPN, dataNPL, dataPAK, dataPHL, dataSGP, dataSKR, dataVTN)
mergedData2 <-  rbind(dataBD, dataBTN, dataCHN, dataIND, dataIDN, dataNPL, dataPAK, dataPHL, dataVTN)
mergedData.p <- pdata.frame(mergedData, index = c("Country", "Year")) ##Panel type data
mergedData2.p <- pdata.frame(mergedData2, index = c("Country", "Year"))
 
summary(mergedData)
summary(mergedData2)

stargazer(mergedData[,c(4,6:8,11)], summary = TRUE, type =  "text", out = "summary.txt")

pooled_OLS_model <- plm(Urban.Population.in.Thousand. ~ (People.Employed.in.Agriculture
   +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US..), effect = "individual", data = mergedData.p,
   model = "pooling")

summary(pooled_OLS_model)

stargazer(pooled_OLS_model, type = "text")
#Breusch-Pagan Lagrange Multiplier Test, which inspects for individual effects 
plmtest(pooled_OLS_model, type = c("bp"))

fixedeff_model <- plm(Urban.Population.in.Thousand. ~ (People.Employed.in.Agriculture
                  +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US..), data = mergedData.p,
                    model = "within", effect = "twoways")
summary(fixedeff_model)
stargazer(fixedeff_model, type = "text")


randomeff_model <- plm(Urban.Population.in.Thousand. ~ (People.Employed.in.Agriculture
                       +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US..), data = mergedData.p, 
                    model = "random")


randomeff_model2 <- plm(Urban.Population.in.Thousand. ~ (People.Employed.in.Agriculture
                                                        +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US..), data = mergedData2.p, 
                       model = "random")
stargazer(randomeff_model, type = "text")
stargazer(randomeff_model2, type = "text")
summary(randomeff_model)
randomeff_model.residuals <- randomeff_model$residuals
hist(randomeff_model.residuals)
stargazer(pooled_OLS_model, fixedeff_model, randomeff_model, type = "text",
          out = "models.txt")



##tab_model(pooled_OLS_model, fixedeff_model, randomeff_model, show.se = TRUE, show.stat = TRUE)


# Hausman test which compares Random and Fixed effects models.
phtest(randomeff_model, fixedeff_model)

#random effects model chosen

plmtest(pooled_OLS_model, type = c("bp"))

# cross-pooled_model# cross-sectional dependence
pcdtest(randomeff_model, test=c("lm"))
pcdtest(randomeff_model, test=c("cd"))


#stationarity
mergedData.set <- plm.data(mergedData, indexes = c("Country", "Year"))

adf.test(mergedData.set$Country)
adf.test(mergedData.set$Year)


bptest(Urban.Population.in.Thousand. ~ (People.Employed.in.Agriculture
                                        +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US..)
       + factor(Country), data = mergedData, studentize=F )

# heteroskedasticity present - robust covariance matrix needed
# serial correlation present
pbgtest(randomeff_model)

# Original coefficients
SE1   = sqrt(diag(vcov(randomeff_model)))
coeftest(randomeff_model)
SE1
tbl1 <- cov2cor(vcov(randomeff_model))
tbl1


## Robust variance estimator
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcov(randomeff_model, type = x)))))
cov2cor(vcov(randomeff_model, type = "HC0"))
Estimate1 <- coeftest(randomeff_model,vcov=vcov(randomeff_model, type="HC0")) ## Same result as 1
stargazer(Estimate1, type = "text")
summary(randomeff_model,vcov=vcov(randomeff_model2, type="HC0"))
summary(randomeff_model)

# Cluster-robust variance estimator with clustered groups
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(randomeff_model, type = x, cluster="group")))))
cov2cor(vcovHC(randomeff_model, type = "HC0", cluster="group"))
summary(randomeff_model,vcov=vcovHC(randomeff_model, type="HC0", cluster = "group"))
Estimate2 <-  coeftest(randomeff_model,vcov=vcovHC(randomeff_model, type="HC0", cluster = "group", method = "arellano"))
# Cluster-robust variance estimator with SEs clustered across time

t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(randomeff_model, type = x, cluster="time")))))
SE2 = sqrt(diag(vcovHC(randomeff_model, type = "HC0", cluster="time")))
Estimate3 <- coeftest(randomeff_model,vcov=vcovHC(randomeff_model, type="HC0", method = "arellano", cluster = "time"))
summary(randomeff_model,vcov=vcovHC(randomeff_model, type="HC0", cluster = "time"))

## Cluster-robust variance estimator with clustering by country
t(sapply(c("CR0", "CR1", "CR2", "CR3", "CR1S", "CR1p"), function(x) sqrt(diag(vcovCR(randomeff_model, mergedData$Country,type = x)))))
coeftest(randomeff_model,vcov=vcovCR(randomeff_model,mergedData$Country, type="CR0")) ###Best Result  1
Estimate4 <- coeftest(randomeff_model,vcov=vcovCR(randomeff_model,mergedData$Country, type="CR0"))
cov2cor(vcovCR(randomeff_model, mergedData$Country,type = "CR2"))

stargazer(Estimate1,Estimate2,Estimate3, type = "html", title = "Panel data Model after adjustment of standard errors by clustering", out = "model2.html")
stargazer(Estimate1,Estimate2,Estimate3, type = "text", title = "Panel data Model after adjustment of standard errors by clustering", out = "model2.txt")

#opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
#plot(lm_All, las = 1)
#par(opar)


plot(randomeff_model$residuals, pch = 16, col = "red")
robust(panel_reg_All)
lm_All <- lm(Urban.Population.in.Thousand. ~  People.Employed.in.Agriculture
                     +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US.., data = mergedData)

robust_lm <-robust(lm_All)

panel_reg_East <- plm(Urban.Population.in.Thousand. ~ (People.Employed.in.Agriculture
                     +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US..), data = panelEast, 
                     index = c("Country", "Year"), model = "within")


panel_reg_South <- plm(Urban.Population.in.Thousand. ~ People.Employed.in.Agriculture
                     +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US.., data = panelSouth, 
                     index = c("Country", "Year"), model = "within")


panel_reg_SouthEast <- plm(Urban.Population.in.Thousand. ~ People.Employed.in.Agriculture
                     +People.Employed.in.Industry+People.Employed.in.Services+GDP.Per.Capita.Constant.2010.US.., data = panelSouthEast, 
                     index = c("Country", "Year"), model = "within") 

tab_model(lm_All, panel_reg_All, vcov.fun = "HC", show.se = TRUE)


coeftest(randomeff_model, vcov. = vcovHC(randomeff_model, type = "HC1"))
summary(randomeff_model, vcov. = vcovHC(randomeff_model, type = "HC1"), type = "text")
coeftest(randomeff_model, vcov. = vcovCL(randomeff_model, cluster = mergedData$Country))
 summary(panel_reg_All)
 summary(ols_reg_All)


coeftest(panel_reg_East, vcov. = vcovHC(panel_reg_East, type = "HC1"))

coeftest(panel_reg_South, vcov. = vcovHC(panel_reg_South, type = "HC1"))

coeftest(panel_reg_SouthEast, vcov. = vcovHC(panel_reg_SouthEast, type = "HC1"))



