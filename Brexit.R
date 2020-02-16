# Script: Brexit
# Author: Joshua L. Eubanks
# Date:   08 September 2016

# Cleaning up the past
rm(list = ls())

# Installing Necessary Libraries
library(lubridate)
library("psych")



#Data Munging

#Reading in csv files
GrowthUK <- read.csv("C:/Independent Study Asyun/GrowthUK.csv")
GrowthUS <- read.csv("C:/Independent Study Asyun/GrowthUS.csv")
GrowthEU <- read.csv("C:/Independent Study Asyun/GrowthEU.csv")
DEXUSEU <- read.csv("C:/Independent Study Asyun/DEXUSEU.csv")
DEXUSUK <- read.csv("C:/Independent Study Asyun/DEXUSUK.csv")
DEXJPUS <- read.csv("C:/Independent Study Asyun/DEXJPUS.csv")
DTWEXM <- read.csv("C:/Independent Study Asyun/DTWEXM.csv")
G7 <- read.csv("C:/Independent Study Asyun/G7.csv")
VIX <- read.csv("C:/Independent Study Asyun/VIX.csv")
table <- read.csv("C:/Independent Study Asyun/table.csv")

#Growth Data for UK and US
UK_US <- merge(GrowthUK, GrowthUS, by.x = "DATE", by.y = "DATE")
UK_US <- merge(UK_US, G7, by.x = "DATE", by.y = "DATE")
UK_US <- subset(UK_US, UK.Delta != ".")
UK_US$UK.Delta <- as.character(UK_US$UK.Delta)
UK_US$US.Delta <- as.character(UK_US$US.Delta)
UK_US$UK.Delta <- as.numeric(UK_US$UK.Delta)
UK_US$US.Delta <- as.numeric(UK_US$US.Delta)
UK_US$DATE <- as.character(UK_US$DATE)
UK_US$DATE <- mdy(UK_US$DATE)

#Growth data for UK, US, EU
GrowthEU$DATE <- mdy(GrowthEU$DATE)
Three <- merge(UK_US, GrowthEU, by.x = "DATE", by.y = "DATE")
Three$EU.Delta <- as.character(Three$EU.Delta)
Three$EU.Delta <- as.numeric(Three$EU.Delta)
Three$UK.Delta <- as.character(Three$UK.Delta)
Three$US.Delta <- as.character(Three$US.Delta)
Three$UK.Delta <- as.numeric(Three$UK.Delta)
Three$US.Delta <- as.numeric(Three$US.Delta)

table$Date <- as.character(table$Date)
table$ukpi <- as.character(table$ukpi)
table$usgdp <- as.character(table$usgdp)
table$uki <- as.character(table$uki)
table$mideff <- as.character(table$mideff)
table$Lastuspi <- as.character(table$Lastuspi)
table$Midtwd <- as.character(table$Midtwd)
table$exusuk <- as.character(table$exusuk)
table$ukpi <- as.numeric(table$ukpi)
table$usgdp <- as.numeric(table$usgdp)
table$uki <- as.numeric(table$uki)
table$mideff <- as.numeric(table$mideff)
table$Lastuspi <- as.numeric(table$Lastuspi)
table$Midtwd <- as.numeric(table$Midtwd)
table$exusuk <- as.numeric(table$exusuk)
table$Date <- mdy(table$Date)
table <- na.omit(table)

new <- read.csv("C:/Independent Study Asyun/var.csv")
new$Date <- as.character(new$Date)
new$dukpi <- as.character(new$dukpi)
new$usgdp <- as.character(new$usgdp)
new$duki <- as.character(new$duki)
new$dusi <- as.character(new$dusi)
new$duspi <- as.character(new$duspi)
new$dexusuk <- as.character(new$dexusuk)
new$dukpi <- as.numeric(new$dukpi)
new$usgdp <- as.numeric(new$usgdp)
new$duki <- as.numeric(new$duki)
new$dusi <- as.numeric(new$dusi)
new$duspi <- as.numeric(new$duspi)

new$dexusuk <- as.numeric(new$dexusuk)
new$Date <- mdy(new$Date)
new <- na.omit(new)
#Exhchange Rates
DEXUSUK$DATE <- ymd(DEXUSUK$DATE)
DTWEXM$DATE <- ymd(DTWEXM$DATE)
G7$DATE <- mdy(G7$DATE)
EX <- merge(DEXUSUK, DTWEXM, by.x = "DATE", by.y = "DATE")
EX$DEXUSUK <- as.character(EX$DEXUSUK)
EX$DTWEXM <- as.character(EX$DTWEXM)
UK_US$Delt.G7 <- as.character(UK_US$Delt.G7)
EX$DEXUSUK <- as.numeric(EX$DEXUSUK)
EX$DTWEXM <- as.numeric(EX$DTWEXM)
UK_US$Delt.G7 <- as.numeric(UK_US$Delt.G7)
EX <- na.omit(EX)
UK_US <- na.omit(UK_US)

#Converting these to percent changes
library(quantmod)
EX$DEXUSUK <- Delt(EX$DEXUSUK)
EX$DTWEXM <- Delt(EX$DTWEXM)
EX <- na.omit(EX)
detach("package:quantmod", unload = TRUE)

#VIX
VIX$DATE <- ymd(VIX$DATE)
VIX$VIXCLS <- as.character(VIX$VIXCLS)
VIX$VIXCLS <- as.numeric(VIX$VIXCLS)
VIX <- na.omit(VIX)

# Splitting data
GpreEMU <-
  subset(UK_US,
         DATE >= as.Date("1980-01-01") & DATE <= as.Date("1998-12-31"))
GpostEMU <-
  subset(UK_US,
         DATE >= as.Date("1999-01-01") & DATE <= as.Date("2014-12-31"))
XpreEMU <-
  subset(EX, DATE >= as.Date("1980-01-01") &
           DATE <= as.Date("1998-12-31"))
XpostEMU <-
  subset(EX, DATE >= as.Date("1999-01-01") &
           DATE <= as.Date("2014-12-31"))

# Correlation testing
cor.UK.US <- cor(UK_US$US.Delta, UK_US$UK.Delta)
library("psych")
cor.Three <- corr.test(x = Three[2:4])
detach("package:psych", unload = TRUE)
cor.GpreEMU <- cor(GpreEMU$UK.Delta, GpreEMU$US.Delta)
cor.GpostEMU <- cor(GpostEMU$UK.Delta, GpostEMU$US.Delta)

#Analysis
X.i <- (var(XpreEMU$DEXUSUK)) / (var(XpreEMU$DTWEXM))
X.c <- (var(XpostEMU$DEXUSUK)) / (var(XpostEMU$DTWEXM))
G.c <-
  cor(GpostEMU$UK.Delta, GpostEMU$US.Delta) / cor(GpostEMU$Delt.G7, GpostEMU$US.Delta)
G.i <-
  cor(GpreEMU$UK.Delta, GpreEMU$US.Delta) / cor(GpreEMU$Delt.G7, GpreEMU$US.Delta)

#correlation of yield slopes
yielduk <- read.csv("C:/Independent Study Asyun/yielduk.csv")
yieldus <- read.csv("C:/Independent Study Asyun/yieldus.csv")
yields <- merge(yieldus, yielduk, by.x = "Date", by.y = "dates")
yields$deltuss <- as.character(as.numeric(yields$deltuss))
yields$deltuks <- as.character(as.numeric(yields$deltuks))
yields$deltuss <- as.numeric(yields$deltuss)
yields$deltuks <- as.numeric(yields$deltuks)
yields <- na.omit(yields)
yield_cor <- cor(yields$deltuss, yields$deltuks)
yields_lag_cor <- ccf(yields$deltuss, yields$deltuks)
plot(yields_lag_cor)

# VAR
library("vars")

# Simplified Model: UK
vuk <-
  as.data.frame(cbind(new$dukpi, new$ukgdp, new$duki, new$exusuk))
names(vuk) <- c("dukpi", "yuk", "duki", "ex")
varuk <-
  VAR(
    vuk,
    type = "const",
    season = NULL,
    exogen = NULL,
    lag.max = 8,
    ic = "AIC"
  )
irfuk <-
  irf(
    varuk,
    impulse = c("yuk", "dukpi", "duki"),
    response = c("yuk", "dukpi", "duki"),
    boot = TRUE,
    ci = .95
  )
plot(irfuk)
# Simplified Model: US
vus <-
  as.data.frame(cbind(new$duspi, new$usgdp, new$dusi, new$exusuk))
names(vus) <- c("duspi", "yus", "dusi", "ex")
varus <-
  VAR(
    vus,
    type = "const",
    season = NULL,
    exogen = NULL,
    lag.max = 8,
    ic = "AIC"
  )
irfus <-
  irf(
    varus,
    impulse = c("yus", "duspi", "dusi"),
    response = c("yus", "duspi", "dusi"),
    boot = TRUE,
    ci = .95
  )
plot(irfus)
#Full Model
var_table <-
  as.data.frame(cbind(
    new$duspi,
    new$dukpi,
    new$usgdp,
    new$ukgdp,
    new$dusi,
    new$duki,
    new$dexusuk
  ))
names(var_table) <- c("uspi", "ukpi", "yus", "yuk", "usi", "uki", "ex")

var <-
  VAR(
    var_table,
    type = "const",
    season = NULL,
    exogen = NULL,
    lag.max = 8,
    ic = "AIC"
  )
irfboth <-
  irf(
    var,
    impulse = c("yuk", "ex"),
    response = c("uspi", "ukpi", "yus", "yuk", "usi", "uki", "ex")
  )
plot(irfboth)
# Making the shock negative 1 std
#k <- length(irfboth$irf$yuk)
#for(i in 1:k){

#  irfboth$irf$yuk[i] =irfboth$irf$yuk[i]*-1.0

#} # End of for loop I

#j <- length(irfboth$irf$ex)
#for(i in 1:j){irfboth$irf$ex[i]=irfboth$irf$ex[i]*-1.0}

fevd.1 <- fevd(var, n.ahead = 2)
fevd.8 <- fevd(var, n.ahead = 8)





bigfevd <- matrix(rep(0, 98), nrow = 14, ncol = 7)
for (i in 1:7) {
  bigfevd[i, ] <- fevd.1[[i]][2, ]
  bigfevd[i + 7, ] <- fevd.8[[i]][8, ]
}

#Granger test
gt.uk.1 <- grangertest(new$usgdp ~ new$ukgdp, order = 1)
gt.uk.2 <- grangertest(new$usgdp ~ new$ukgdp, order = 2)
gt.uk.3 <- grangertest(new$usgdp ~ new$ukgdp, order = 3)
gt.uk.4 <- grangertest(new$usgdp ~ new$ukgdp, order = 4)
gt.uk.5 <- grangertest(new$usgdp ~ new$ukgdp, order = 5)
gt.uk.6 <- grangertest(new$usgdp ~ new$ukgdp, order = 6)
gt.uk.7 <- grangertest(new$usgdp ~ new$ukgdp, order = 7)
gt.uk.8 <- grangertest(new$usgdp ~ new$ukgdp, order = 8)

gt.us.1 <- grangertest(new$ukgdp ~ new$usgdp, order = 1)
gt.us.2 <- grangertest(new$ukgdp ~ new$usgdp, order = 2)
gt.us.3 <- grangertest(new$ukgdp ~ new$usgdp, order = 3)
gt.us.4 <- grangertest(new$ukgdp ~ new$usgdp, order = 4)
gt.us.5 <- grangertest(new$ukgdp ~ new$usgdp, order = 5)
gt.us.6 <- grangertest(new$ukgdp ~ new$usgdp, order = 6)
gt.us.7 <- grangertest(new$ukgdp ~ new$usgdp, order = 7)
gt.us.8 <- grangertest(new$ukgdp ~ new$usgdp, order = 8)

gt.ex.1 <- grangertest(new$usgdp ~ new$dexusuk, order = 1)
gt.ex.2 <- grangertest(new$ukgdp ~ new$dexusuk, order = 2)
gt.ex.3 <- grangertest(new$ukgdp ~ new$dexusuk, order = 3)
gt.ex.4 <- grangertest(new$ukgdp ~ new$dexusuk, order = 4)
gt.ex.5 <- grangertest(new$ukgdp ~ new$dexusuk, order = 5)
gt.ex.6 <- grangertest(new$ukgdp ~ new$dexusuk, order = 6)
gt.ex.7 <- grangertest(new$ukgdp ~ new$dexusuk, order = 7)
gt.ex.8 <- grangertest(new$usgdp ~ new$dexusuk, order = 8)
