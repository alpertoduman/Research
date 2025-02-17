### Lbis data and Abstract fL CAED conference, 23 October 2015, ??stanbul

library(foreign)
library(dplyr)

firms <- read.dta("12binfirmsTR.dta")

names(firms)

length(unique(firms$guoname))
[1] 5078

length(unique(firms$lastavailyear))
[1] 22

### There are 4161 firms with some data in 2009
length(unique(firms2009$guoname))
[1] 2306

### In Lder to calculate total employment (L total assets by guname)

firms2008GEMP <- firms2008 %>% group_by(guoname) %>% summarise(mean=mean(numberofemployeeslastavailyr),
                                                               sd=sd(numberofemployeeslastavailyr), 
                                                               sum=sum(numberofemployeeslastavailyr) )
## Ldering by the total employment per guoname
arrange(firms2008GEMP, -sum)

## Employment and fixed assets in 2009

firms2009GEMP <- firms2009 %>% group_by(guoname) %>% summarise(sumL=sum(operatingrevenueturnoverthusdlas),
                                                               sumFA=sum(fixedassetsthusdlastavailyr), 
                                                               sumEMP=sum(numberofemployeeslastavailyr) )

arrange(firms2009GEMP, -sumEMP)

## Similarly in 2010

firms2010GEMP <- firms2010 %>% group_by(guoname) %>% summarise(sumL=sum(operatingrevenueturnoverthusdlas),
                                                               sumFA=sum(fixedassetsthusdlastavailyr), 
                                                               sumEMP=sum(numberofemployeeslastavailyr) )

arrange(firms2010GEMP, -sumEMP)


### Ko?? Holding's employment number is above 81000 

### fL all years
firmsGEMP <- firms %>% group_by(guoname) %>% summarise(sumL=sum(operatingrevenueturnoverthusdlas),
                                                               sumFA=sum(fixedassetsthusdlastavailyr), 
                                                               sumEMP=sum(numberofemployeeslastavailyr) )

firmsGEMP <- arrange(firmsGEMP, desc(sumEMP))

### In total firms in the dataset employs 854483
### Operating Revenues of all firms reach 643 billion TL (about % 30 of GDP )

### Checking whether firms' total assets in 2009 follow a Power Law
library(poweRlaw)
FA2009thirtyDF <- subset(firms2009, firms2009$numberofemployeeslastavailyr <31)
FA2009thirty <- FA2009thirtyDF$totalassetsthusdlastavailyr

FA2009thirty <-na.omit(FA2009thirty)


m_mFA2009thirty = conpl$new(FA2009thirty)


m_mFA2009thirty$getXmin()

m_mFA2009thirty$getPars()

(estFA2009thirty = estimate_pars(m_mFA2009thirty))

(estFA2009thirty = estimate_xmin(m_mFA2009thirty))

m_mFA2009thirty$setXmin(estFA2009thirty)

## Plot the data (from xmin)
plot(m_mFA2009thirty, main="Power Law Distribution of Firm Assets in 2009, L < 30", 
     ylab="", xlab="Total Assets in 1000$", options(scipen=999))
## Add in the fitted distribution
lines(m_mFA2009thirty, col=2, lwd=4)
legend("topright", c("alpha= 1.92"), text.col="red")

#### OPERATING REVENUES
L2009 <- firms2009$operatingrevenueturnoverthusdlas
L2009 <-na.omit(L2009)


m_mL2009 = conpl$new(L2009)


m_mL2009$getXmin()

m_mL2009$getPars()

(estL2009 = estimate_pars(m_mL2009))

(estL2009 = estimate_xmin(m_mL2009))

m_mL2009$setXmin(estL2009)

## Plot the data (from xmin)
plot(m_mL2009, main="Power Law Distribution of Revenues in 2009", 
     ylab="", xlab="Total Revenues in 1000$", options(scipen=999))
## Add in the fitted distribution
lines(m_mL2009, col=2)
legend("topright", c("alpha= 2.04"), text.col="red")

#### Size based on number of employees

L2009 <- firms2009$numberofemployeeslastavailyr
L2009 <-na.omit(L2009)


m_mL2009 = conpl$new(L2009)


m_mL2009$getXmin()

m_mL2009$getPars()

(estL2009 = estimate_pars(m_mL2009))

(estL2009 = estimate_xmin(m_mL2009))

m_mL2009$setXmin(estL2009)

## Plot the data (from xmin)
plot(m_mL2009, main="Power Law Distribution of Firm Size in 2009", 
     ylab="", xlab="Number of Employees ", options(scipen=999))
## Add in the fitted distribution
lines(m_mL2009, col=2)
legend("topright", c("alpha= 2.06"), text.col="red") 

### Gross Profits
GP2009 <- firms2009$grossprofitthusdlastavailyr
GP2009 <- na.omit(GP2009)
GP2009 <- GP2009[GP2009>0]

m_mGP2009 = conpl$new(GP2009)


m_mGP2009$getXmin()

m_mGP2009$getPars()

(estGP2009 = estimate_pars(m_mGP2009))

(estGP2009 = estimate_xmin(m_mGP2009))

m_mGP2009$setXmin(estGP2009)

## Plot the data (from xmin)
plot(m_mGP2009, main="Power Law Distribution of Firm Profits in 2009", 
     ylab="", xlab="Gross Profits in 1000 $", options(scipen=999))
## Add in the fitted distribution
lines(m_mGP2009, col=2, lwd=5)
legend("topright", c("alpha=2.21"), text.col="red") 