## colour coding 30 day correlation stats

setwd("S:/UEA/Spot C19/SpotOmicron")
library(readxl)
allz <- read_excel("preR Omicron.xlsx", sheet = "data", range = "a582:m873", col_name=c("date", "P12", "P1", "P2", "ONSincid", "ONSprev", "Zoe", "EDSS", "GPIH", "calls", "web", "HospAdm", "C19PrimRsn") )

install.packages("zoo", "plotrix", "gridExtra", "ggplot2")
library(plotrix, gridExtra, zoo)
library(runner)

##creating the file with vart dominance indicated
Delta = c(rep(1000,104), rep(NA, 188))
BA1 = c(rep(NA, 104), rep(7000,69), rep(NA, 119))
BA2 = c(rep(NA, 173), rep(7000,107), rep(NA, 12))
OmicOth = c(rep(NA,280), rep(7000, 12))

vardom = data.frame(allz$date, Delta, BA1, BA2, OmicOth) 
names(vardom) = c("date", "Delta", "BA1", "BA2", "OmicOth")


## create smoothed variables for the pics ## for (varn in c("P12", "HospAdm", "C19PrimRsn", "Zoe")) {
## for (varn in c("P12", "P1", "P2", "HospAdm", "EDSS", "GPIH", "calls", "web", "Zoe", "C19PrimRsn")) {
for (varn in c("P12")) {
vv = paste0(varn,"sm")
allz[[vv]] = rollmean(allz[[varn]], k = 7, fill=NA, align="center") 
} # end of for varn smoothing loop

## par(mfrow=c(4, 3))
par(mfrow=c(1,1))
## par(mgp=c(1,0.4,0), mar=c(1.2,2,1.2,1.9)) ## mgp also determines label position
par(mgp=c(1,0.4,0), mar=c(3,3,3,3)) ## mgp also determines label position

#rows
## for (vv in c("P12", "P1", "P2")) {  ## only to show their correl with each other, supplemental ??
## for (vv in c("P12", "HospAdm", "C19PrimRsn", "Zoe")) {
## for (vv in c("EDSS", "GPIH", "calls", "web")) {
for (vv in c("P12")) {  ## just P12 for the row

## for (gs in c("P12", "ONSincid", "ONSprev")) {  # columns

for (gs in c("ONSprev")) { ## just one column

vardom=data.frame(allz$date, rep(NA, 292), rep(NA, 292), rep(NA,292), rep(NA,292))
names(vardom) = c("date", "Delta", "BA1", "BA2", "OmicOth")
## need this valassign because is approx min of values that appear on respective plots
valassign = switch(gs, "P12" = 1000, "ONSincid" = 40000, 550000)  ## to get correl. line on the plots...
vardom[1:104,3] = valassign 
vardom[105:173,4] = valassign
vardom[174:280,5] = valassign

gscol = ifelse(gs == "P12", "violet", ifelse(gs == "ONSincid", "green", "blue"))
## chdims = c(0,100000,200000)
chdims = switch(gs, "P12" = c(0,100000,200000), "ONSincid" = c(0,300000,600000), c(0,2000000,4000000))  ## chart dims, below
yyll = switch(vv, "P12" = c(0,180000), "P1" = c(0,19000), "P2" = c(0,200000), "EDSS" = c(0,1000), "HospAdm" = c(0, 3000), "C19PrimRsn" = c(0, 10000), "GPIH"=  c(0,5.5), "calls" = c(0,6000), "web" = c(0,4200), "Zoe" = c(0,320000), 0)
r2u = c(1:292) ## switch(gs, "P12" = c(30:468), "ONSincid" = c(30:262), c(30:468))  ## range 2 use

varn = paste0(vv,"sm")
gsplot = ifelse(gs == "P12", paste0(gs, "sm"), gs) 

plot1 = plot(allz$date[r2u], allz[[varn]][r2u], type="l", col=1, ylab="", xlab="Month in 2021-2022", cex.axis=1.0, ylim = yyll)
par(new = TRUE)
plot1 = plot1 + plot(allz$date[r2u], allz[[gsplot]][r2u], type="l", lwd=2, col=gscol, axes=FALSE, bty = "n", ylab="", xlab="")
plot1 = plot1 + axis (side=4, at=chdims, labels=chdims, cex.axis=1.0)

lines(vardom$date[1:292], vardom[,3], lwd = 5, col = "#16B823")
lines(vardom$date[1:292], vardom[,4], lwd = 5, col = "#66C7ED")
lines(vardom$date[1:292], vardom[,5], lwd = 5, col = "#6642F5")


} ## end of gs = gold standard comparison loop, calc'ing the runners 
} ## end of for vv in loop, calc'ing comparators


