
# plot all rates in HMD/HFD
setwd("/home/tim/workspace/KetteringPres")

library(HMDHFDplus)
HMDcountries <- getHMDcountries()

#Datm <- do.call(rbind, lapply(HMDcountries, function( XYZ, .pw, .us){
#					Dati <- readHMDweb(XYZ, "mltper_1x1", username = .us, password = .pw)
#					Dati$Code <- XYZ
#					Dati$Sex <- "m"
#                    Dati
#				}, .pw = pw, .us = us ))
#Datf <- do.call(rbind, lapply(HMDcountries, function( XYZ, .pw, .us){
#					Dati <- readHMDweb(XYZ, "fltper_1x1", username = .us, password = .pw)
#					Dati$Code <- XYZ
#					Dati$Sex <- "f"
#					Dati
#				}, .pw = pw, .us = us ))
#SRB <- do.call(rbind, lapply(HMDcountries, function( XYZ, .pw, .us){
#					Dati <- readHMDweb(XYZ, "Births", username = .us, password = .pw)
#					Dati$Code <- XYZ
#					Dati
#				}, .pw = pw, .us = us ))
#HFDcountries <- getHFDcountries()
#
#Fert <- do.call(rbind, lapply(HFDcountries, function( XYZ, .pw, .us){
#					Dati <- readHFDweb(XYZ, "asfrRR", username = .us, password = .pw)
#					Dati$Code <- XYZ
#					Dati$Sex <- "f"
#					Dati
#				}, .pw = pw, .us = us ))
#HMD <- rbind(Datf,Datm)
#getwd()
#save(HMD, file = "Data/HMD.Rdata")
#save(Fert, file = "Data/HFD.Rdata")
#save(SRB, file = "Data/HMDbirths.Rdata")

HMD <- local(get(load("Data/HMD.Rdata")))
HFD <- local(get(load("Data/HFD.Rdata")))
SRB <- local(get(load("Data/HMDbirths.Rdata")))
#Dat <- local(get(load("/home/tim/git/ThanoRepro/ThanoRepro/Data/DataAll.Rdata")))
library(reshape2)

mxm <- acast(HMD[HMD$Sex == "m", ], Age~Year+Code, value.var="mx")
mxf <- acast(HMD[HMD$Sex == "f", ], Age~Year+Code, value.var="mx")


# Mortality (1)
png("Figures/Mortality1.png",width=1000,height=1000, pointsize=36)
par(mai=c(2.5,2.5,.2,.2))
matplot(0:110, cbind(mxm,mxf), type ='l', lty = 1, col = "#00000005", 
		log = 'y', xlab = "Age", ylab = "log mortality rate",cex.lab=1.5)
dev.off()

png("Figures/Mortality2.png",width=1000,height=1000, pointsize=36)
par(mai=c(2.5,2.5,.2,.2))
matplot(0:110, cbind(mxm,mxf), type ='l', lty = 1, col = "#00000005", 
		log = 'y', xlab = "Age", ylab = "log mortality rate",cex.lab=1.5)
lines(0:110, rowMeans(cbind(mxm,mxf),na.rm=TRUE), col = "white", lwd=5)
dev.off()

png("Figures/Mortality3.png",width=1000,height=1000, pointsize=36)
par(mai=c(2.5,2.5,.2,.2))
matplot(0:110, cbind(mxm,mxf), type ='l', lty = 1, col = "#00000005",
		log = 'y', xlab = "Age", ylab = "log mortality rate",cex.lab=1.5)
lines(0:110, rowMeans(cbind(mxm,mxf),na.rm=TRUE), col = "white", lwd=5)
malavg <- rowMeans(mxm,na.rm=TRUE)
femavg <- rowMeans(mxf,na.rm=TRUE) 
lines(0:110, malavg, col = "blue", lwd=4)
lines(0:110, femavg, col = "red", lwd=4)
segments(30,.04,40, malavg[41]*1.05, col = "white",lwd=2)
text(27,.04,"Males",cex=1.5)
segments(53,.002,44, femavg[45]*.95, col = "white",lwd=2)
text(67,.002,"Females",cex=1.5)
dev.off()

# mortality avg for modelling:
pdf("Figures/MortalityGeneral1.pdf")
plot(0:110, mxm[,"2012_JPN"], col = "black", lwd=1, type = 'l', log = 'y', 
		xlab = "Age", ylab = "log mortality rate")
dev.off()
mxm[,"2013_USA"]
pdf("Figures/MortalityGeneral2.pdf")
plot(0:110, mxm[,"2012_JPN"], col = "black", lwd=1, type = 'l', log = 'y', 
		xlab = "Age", ylab = "log mortality rate")
abline(v=c(11,35,90), col = "red")
dev.off()




# fertility

fx  <- acast(Fert, Age~Year+Code, value.var="ASFR")
fx1 <- acast(Fert[Fert$Year <= 1930, ], Age~Year+Code, value.var="ASFR")
fx2 <- acast(Fert[Fert$Year >= 1980, ], Age~Year+Code, value.var="ASFR")

png("Figures/Fertility1.png",width=1000,height=1000, pointsize=36)
par(mai=c(2.5,2.5,.2,.2))
matplot(12:55, fx, type ='l', lty = 1, col = "#00000010", xlab = "Age", ylab = "fertility rate",cex.lab=1.5)
dev.off()

png("Figures/Fertility2.png",width=1000,height=1000, pointsize=36)
par(mai=c(2.5,2.5,.2,.2))
matplot(12:55, fx, type ='l', lty = 1, col = "#00000010", xlab = "Age", ylab = "fertility rate",cex.lab=1.5)
lines(12:55, rowMeans(fx), col = "white", lwd = 5)
dev.off()

png("Figures/Fertility3.png",width=1000,height=1000, pointsize=36)
par(mai=c(2.5,2.5,.2,.2))
matplot(12:55, fx, type ='l', lty = 1, col = "#00000010", xlab = "Age", ylab = "fertility rate",cex.lab=1.5)
lines(12:55, rowMeans(fx), col = "white", lwd = 5)
lines(12:55, rowMeans(fx2), col = scales::muted("green",80,40), lwd = 4)
lines(12:55, rowMeans(fx1), col = scales::muted("green",30,70), lwd = 4)
text(40,.15,"pre-1930",cex=1.5)
text(27,.055,"nowadays",cex=1.5)
dev.off()

SRB$SRB <- SRB$Male / SRB$Female
png("Figures/SRB.png",width=1000,height=1000, pointsize=36)
par(mai=c(2.5,2.5,.2,.2))
plot(density(SRB$SRB),col="red",lwd=5,xlab = "sex ratio at birth",main="", ylab = "density",cex.lab=1.5)
dev.off()











