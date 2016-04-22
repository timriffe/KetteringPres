# Author: tim
###############################################################################


setwd("/home/tim/workspace/KetteringPres")

# draw half of pyramid and give fertility curve.
Dat <- local(get(load("/home/tim/git/ThanoRepro/ThanoRepro/Data/DataAll.Rdata")))

library(HMDHFDplus)
Pop <- readHMDweb("USA","Population",username=us,password=pw)
Births <- local(get(load("Data/HMDbirths.Rdata")))
library(Pyramid)

Males   <- Pop$Male2[Pop$Year == 2013]
Females <- Pop$Female2[Pop$Year == 2013]
args(Pyramid)

# now sideways
cohorts1 <- 2014 - 1:length(Females)

Births <- Births[Births$Code=="USA", ]

# Plan, make Leslie, then put in animation, let it ride forward. Keep fertility curve overlaid. 
# save births series in light gray, background, and population in darker gray, overlaid, decrementing.
#plot(cohorts1,Females,type = "n")
#rect(Births$Year,0,Births$Year+1,Births$Female,border=gray(.8),col=gray(.8))
#rect(cohorts1,0,cohorts1+1,Females,border=gray(.5),col = gray(.5))

yr   <- 2013
PF   <- with(Births, Female[Year==yr]/Total[Year==yr])
PF <- .49
Fert <- local(get(load("Data/HFD.Rdata")))
Fx   <- Fert$ASFR[Fert$Code == "USA" & Fert$Year == yr]
Fx   <- Fx * PF
Fx   <- c(rep(0,12),Fx,rep(0,55)) 
plot(Fx)

HMD <- local(get(load("Data/HMD.Rdata")))

lx <- HMD$lx[HMD$Code == "USA" & HMD$Year == 2013 & HMD$Sex == "f"]
px <- lx[-1] / lx[-111]

L <- matrix(0,111,111)
L[1, ] <- Fx
L[row(L) == col(L)+1] <- px
head(L)
Pop <- matrix(0,nrow=111,ncol=501)
Pop[,1] <- Females

for (i in 1:500){
	Pop[,i+1] <- L %*% Pop[,i]
}
rownames(Pop) <- 0:110
colnames(Pop) <- 2014:(2014+500)

BirthFlow <- colSums(Pop * Fx)

BirthsPast <- Births$Female
names(BirthsPast) <- Births$Year
BirthFlow <- c(BirthsPast, BirthFlow)
year <- 2014

for (year in 2014:2200){
	pdf(paste0("Figures/Animation/P",year-2013,".pdf"))
	par(xaxs="i",yaxs="i")
plot(NULL, 
		xlim = c(year - 120,year), 
		ylim =c(0,max(c(Pop,BirthFlow))), 
		ylab = "Pop born and alive", 
		xlab = "Birth Cohort", main = "",axes=FALSE)

cohs <- seq(year-120,year,by=1)
rect(cohs,0,cohs+1,BirthFlow[as.character(cohs)],border=gray(.8),col=gray(.8))
axis(1,pretty(cohs))
axis(2, pretty(c(0,max(c(Pop,BirthFlow)))))
rect(year - (0:110), 0, year - (0:110) + 1, Pop[,as.character(year)], border = gray(.5), col = gray(.5))
rect(year - (0:110), 0, year - (0:110) + 1, Fx * Pop[,as.character(year)], border = NA, col = "#DD5500")
text(year-100,2e6,year,col=gray(.4),cex=4)
dev.off()
}


pdf("Figures/PopUSA2014.pdf")
Pyramid(Males, Females, prop = FALSE, 
		fill.males = gray(.6), 
		fill.females = gray(.8),year = 2013,
		grid=FALSE,xlab = "Population (millions)",
		box=FALSE,coh.axis=TRUE)
text(-.5,105,"Males",cex=2,col=gray(.5),pos=2)
text(.5,105,"Females",cex=2,col=gray(.5),pos=4)
dev.off()

pdf("Figures/PopUSA20142.pdf")
Pyramid(Males, Females, prop = FALSE, 
		fill.males = gray(.6), 
		fill.females = gray(.8),year = 2013,
		grid=FALSE,xlab = "Population (millions)",
		box=FALSE,coh.axis=TRUE)
text(-.5,105,"Males",cex=2,col=gray(.5),pos=2)
text(.5,105,"Females",cex=2,col=gray(.5),pos=4)
rect(0,0,2.4,110,border="red",lwd=2)
dev.off()
