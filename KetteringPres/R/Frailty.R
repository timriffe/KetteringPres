# Author: tim
###############################################################################

# illustrate the standard frailty model.

# use a Gompertz baseline:
a <- .002
b <- .08		
		
x <- 0:100
gompmx <- function(a,b,x){
	a * exp(b*x)
}

plot(x,gompmx(a,b,x),type='l')

basemx <- gompmx(a,b,x) 
#(z <- dgamma(seq(.05,1,by=.05),.25,4)+.5)
#
#basemxz <- outer(basemx,z,"*")
#matplot(x,basemxz,type = 'l',lty=1, log='y',col="#00000080",xlab="Age")
#lines(x,basemx,col="blue",lwd=2)

source("/home/tim/git/DistributionTTD/DistributionTTD/R/Functions.R")
#plot(mx2lxHMD(basemxz[,1]))
mx2lx <- function(mx){
	lx <- mx2lxHMD(mx)
	lx[lx<0]<- 0
	lx
}

(z <- dgamma(seq(.05,1,by=.05),1,2))
plot(z)
basemxz <- outer(basemx,z,"*")
baselx <- apply(basemxz,2,mx2lx)
matplot(x,baselx,type = 'l',lty=1,col="#00000080",xlab="Age")


matplot(x,basemxz,type = 'l',lty=1,col="#00000080",xlab="Age",log='y')


lxcum <- cbind(0,t(apply(baselx[,ncol(baselx):1] / ncol(baselx),1,cumsum)))

cols <- colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"),space="Lab")(20)
par(xaxs='i',yaxs="i")
plot(NULL, xlim=c(0,100),ylim=c(0,1), xlab = "time since 30", ylab = "Probability of surviving to x")
for (i in length(z):1){
	polygon(c(x,rev(x)),c(lxcum[,i],rev(lxcum[,i+1])),border = NA, col = cols[i])
}
matplot(x,baselx,type = 'l',lty=1,col="#00000080",xlab="Age",add=TRUE)

#standard mortality curve

matplot(x,basemxz,type = 'l',lty=1, log='y',col="#00000080",xlab="time since 30",xlim=c(0,70))
lines(x,basemx,col="blue",lwd=2)
lines(x, rowSums((baselx * basemxz)/rowSums(baselx)),col="red",lwd=3)


pdf("Figures/Frailty1.pdf")
plot(0:71,basemx[1:72],col="blue",lwd=2,xlab="time since 30",xlim=c(0,70),ylab = "log force of mortality",log='y',type='l')
dev.off()

pdf("Figures/Frailty2.pdf")
plot(0:71,basemx[1:72],col="blue",lwd=2,xlab="time since age 30",xlim=c(0,70),ylab = "log force of mortality",log='y',type='l')
matplot(x,basemxz,type = 'l',lty=1,col="#A3A3A380",add=TRUE)
dev.off()

pdf("Figures/Frailty3.pdf")
matplot(x,baselx,type = 'l',lty=1,col="#00000080",xlab="time since age 30", ylab = "subgroup l(x)")
dev.off()

pdf("Figures/Frailty4.pdf")
matplot(x,baselx,type = 'l',lty=1,col="#00000080",xlab="time since age 30", ylab = "subgroup l(x)")
lines(rowSums(baselx) / length(z), col = "blue", lwd = 3)
dev.off()

pdf("Figures/Frailty5.pdf")
plot(0:70,basemx[1:71],col="blue",lwd=2,xlab="time since age 30",xlim=c(0,70),ylab = "log force of mortality",log='y',type='l')
matplot(x,basemxz,type = 'l',lty=1,col="#A3A3A380",add=TRUE)
lines(x, rowSums((baselx * basemxz)/rowSums(baselx)),col="red",lwd=3)
dev.off()
