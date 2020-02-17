
############################
####### DATA ANALYSIS ###### 
############################

library(gdata)
library(GISTools)
library(maptools)
library(MASS)
library(plyr)
library(raster) 
library(RColorBrewer)
library(rgdal)
library(SDMTools)


## Fig. 1 (part 1) 

setwd("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/")

ut_cnty = readOGR("utahcounty", "utahcounty")
proj4string(ut_cnty) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 + +towgs84=0,0,0")

slc_lat = 40.758701
slc_lon = -111.876183
rbc_lat = 40.81416667
rbc_lon = -111.74750000
coords = data.frame("Lon"=c(slc_lon,rbc_lon), "Lat"=c(slc_lat,rbc_lat))
coordinates(coords) = ~ Lon + Lat
proj4string(coords) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 + +towgs84=0,0,0")

utm.proj = "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
ut_cnty.proj = spTransform(ut_cnty, CRS(utm.proj))
coords.proj = spTransform(coords, CRS(utm.proj))


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.1_part1.pdf", height=6, width=6, encoding="WinAnsi.enc")

plot(ut_cnty.proj)
#extent(ut_cnty.proj)
xmin=228583.4 
xmax=673944.6 
ymin=4094744 
ymax=4653572 
Scalebar(xmax-(100*1000), ymax-(100*1000), 100*1000, unit = "km", scale = 0.001, t.cex = 0.75)
north.arrow(xmax-(50*1000),ymax-(50*1000), len=miles2ft(1), lab='N', cex.lab=1, tcol='black')
title("Utah")

points(coords.proj$Lon[1], coords.proj$Lat[1], pch=16, col="black")
points(coords.proj$Lon[2], coords.proj$Lat[2], pch=16, col="red")

dev.off()


## Fig. 1 (part 2) 

Habitat = raster("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/Habitat2.gri")


## coordinates of sampling sites for environmental water, prey, and feathers   
netCoords = read.table("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/netCoords.csv", header=TRUE, sep=",")
streamCoords = read.table("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/streamCoords.csv", header=TRUE, sep=",")
streamCoords = streamCoords[-2,]
substrateCoords = read.table("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/substrateCoords.csv", header=TRUE, sep=",")

netCoords.sp = netCoords
coordinates(netCoords.sp) = ~ Lon + Lat
projection(netCoords.sp) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 + +towgs84=0,0,0") 
streamCoords.sp = streamCoords
coordinates(streamCoords.sp) = ~ Lon + Lat
proj4string(streamCoords.sp) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 + +towgs84=0,0,0")
substrateCoords.sp = substrateCoords
coordinates(substrateCoords.sp) = ~ Lon + Lat
proj4string(substrateCoords.sp) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 + +towgs84=0,0,0")

netCoords_proj.sp = spTransform(netCoords.sp, CRS=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
streamCoords_proj.sp = spTransform(streamCoords.sp, CRS=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
substrateCoords_proj.sp = spTransform(substrateCoords.sp, CRS=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


## dem model 
DEM = raster("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/DEM.tif")

projection(Habitat) = CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
projection(DEM) = CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

DEM2 = projectRaster(DEM,Habitat)

DEM2_masked = mask(DEM2, Habitat)


col.palette = (terrain.colors(12,1))
Hab.cols = c(col.palette[1],col.palette[5],col.palette[9])


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.1_part2.pdf", height=6, width=6, encoding="WinAnsi.enc")

par(mar=c(4,4,3,0))

plot(DEM2_masked, col=grey(0:100/100), xlab="Longitude (UTM, m)", ylab="Latitude (UTM, m)", legend=FALSE, cex.axis = 0.75, cex.lab=1)

plot(Habitat, col=Hab.cols, alpha=0.50, add=TRUE, oldstyle=T, legend=FALSE)

legend("topright", legend=c("Riparian","Meadow","Slope"), fill=Hab.cols, bg="white", cex=1)

points(streamCoords_proj.sp, col="black", lwd=1.5, cex=1)
points(substrateCoords_proj.sp, col="blue", lwd=1.5, cex=1)
points(netCoords_proj.sp, col="red", pch=3, cex=0.5)

dev.off()


## Fig. 2 (main figure) 

meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.B.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd 


d.B1 = d[d$exp=="B1",]
d.B2A = d[d$exp=="B2A",]
d.B2B = d[d$exp=="B2B",]
d.B2C = d[d$exp=="B2C",]

d.B1.HomeLocHab = aggregate(d.B1[, c("timeRIP","timeMEAD","timeSLP")], list(d.B1$HomeLocHab), mean)
d.B1.HomeLocHab.m = as.matrix(d.B1.HomeLocHab[,2:4])
colnames(d.B1.HomeLocHab.m) = NULL

d.B2A.HomeLocHab = aggregate(d.B2A[, c("timeRIP","timeMEAD","timeSLP")], list(d.B2A$HomeLocHab), mean)
d.B2A.HomeLocHab.m = as.matrix(d.B2A.HomeLocHab[,2:4])
colnames(d.B2A.HomeLocHab.m) = NULL

d.B2B.HomeLocHab = aggregate(d.B2B[, c("timeRIP","timeMEAD","timeSLP")], list(d.B2B$HomeLocHab), mean)
d.B2B.HomeLocHab.m = as.matrix(d.B2B.HomeLocHab[,2:4])
colnames(d.B2B.HomeLocHab.m) = NULL

d.B2C.HomeLocHab = aggregate(d.B2C[, c("timeRIP","timeMEAD","timeSLP")], list(d.B2C$HomeLocHab), mean)
d.B2C.HomeLocHab.m = as.matrix(d.B2C.HomeLocHab[,2:4])
colnames(d.B2C.HomeLocHab.m) = NULL

Values = t(rbind(d.B1.HomeLocHab.m, d.B2A.HomeLocHab.m, d.B2B.HomeLocHab.m, d.B2C.HomeLocHab.m))


NestHabitats = c("B1.RIP", "B1.MEAD", "B1.SLP", "B2A.RIP", "B2A.MEAD", "B2A.SLP", "B2B.RIP", "B2B.MEAD", "B2B.SLP", "B2C.RIP", "B2C.MEAD", "B2C.SLP")
Habitats = c("RIP", "MEAD", "SLP")


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.2_main.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mar=c(6.1, 4.1, 2.1, 6.1), xpd=TRUE)

barplot(Values, col = Hab.cols, ylab = "Proportion of time spent in each habitat", space=1)
box(lwd=1.25)

Habitat.df = as.data.frame(rasterToPoints(Habitat))
propRip = length(Habitat.df$x[Habitat.df$HabType==1]) / length(Habitat.df$x)
propMead = length(Habitat.df$x[Habitat.df$HabType==2]) / length(Habitat.df$x)
propSlp = length(Habitat.df$x[Habitat.df$HabType==3]) / length(Habitat.df$x)

segments(x0=0,x1=25,y0=propRip,y1=propRip,lty=2,lwd=2,col="blue")
segments(x0=0,x1=25,y0=propRip+propMead,y1=propRip+propMead,lty=2,lwd=2,col="red")

NestHabitats = rep(c("Riparian", "Meadow", "Slope"),4)
end_point = 0.5 + length(NestHabitats) + length(NestHabitats)-1
text(seq(1.5,end_point,by=2), par("usr")[3]-0.01, srt=60, adj=1, xpd=TRUE, labels = paste(NestHabitats)) 

legend("topright", legend=c("Riparian","Meadow","Slope"), fill=Hab.cols, bg="white", bty="n", cex=1, inset=c(-0.175,0))

dev.off()


## Fig. 3 

pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.3.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(DrinkYN ~ HomeLocHab:exp, data=d, col=c(Hab.cols[1], Hab.cols[2], Hab.cols[3]),  
xaxt='n', xlab="Experiment", ylab="No. drinking events", cex.axis = 1, cex.lab=1, ylim=c(min(d$DrinkYN),max(d$DrinkYN)) )
legend("topright", legend=c("Riparian","Meadow","Slope"), fill=Hab.cols, bg="white", bty="n", cex=1)
axis(side=1, at=c(2,5,8,11), labels=unique(d$exp), cex.axis=1)
box(lwd=1.25)

boxplot(EatYN ~ HomeLocHab:exp, data=d, col=c(Hab.cols[1], Hab.cols[2], Hab.cols[3]),  
xaxt='n', xlab="Experiment", ylab="No. feeding events", cex.axis = 1)
axis(side=1, at=c(2,5,8,11), labels=unique(d$exp), cex.axis=1)
box(lwd=1.25)

dev.off()


## Fig. 4 

pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.4.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(d2Hker_avg ~ HomeLocHab:exp, data=d, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Experiment", ylab=expression(paste("Ind. avg. keratin ", delta^{2}, "H (\u2030)")), cex.axis = 1, cex.lab=1, ylim=c(min(d$d2Hker_avg),max(d$d2Hker_avg)) )
legend("bottomright", legend=c("Riparian","Meadow","Slope"), fill=Hab.cols, bg="white", bty="n", cex=1)
axis(side=1, at=c(2,5,8,11), labels=unique(d$exp), cex.axis=1)
box(lwd=1.25)

boxplot(d18Oker_avg ~ HomeLocHab:exp, data=d, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Experiment", ylab=expression(paste("Ind. avg. keratin ", delta^{18}, "O (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(2,5,8,11), labels=unique(d$exp), cex.axis=1)
box(lwd=1.25)

dev.off()


## Fig. 5
 
yH = d$d2Hker_avg
yO = d$d18Oker_avg
x1 = d$Turn
x2 = d$pFdw 

mH = lm(yH ~ x1 + x2, data=d) 
mO = lm(yO ~ x1 + x2, data=d)

yhatH = mH$coefficients[1] + mH$coefficients[2]*x1 + mH$coefficients[3]*x2 + resid(mH)
yhatO = mO$coefficients[1] + mO$coefficients[2]*x1 + mO$coefficients[3]*x2 + resid(mO) 


exp.name = c("B1","B2A","B2B","B2C")
exp.cols = c("black","red","chartreuse4","blue1")


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.5.pdf", height=5, width=8, encoding="WinAnsi.enc")
 
par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

resid1 = resid(lm(x1 ~ x2))
resid2 = resid(lm(yhatH ~ x2)) 
plot(resid1, resid2, xlab = "Turnover time | pFdw", ylab = expression(paste("Keratin ", delta^{2}, "H | pFdw")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}
legend("topleft", exp.name, lty=rep(1,3), lwd=rep(2,3), col=exp.cols, bty="n")

resid1 = resid(lm(x1 ~ x2))
resid2 = resid(lm(yhatO ~ x2)) 
plot(resid1, resid2, xlab = "Turnover time | pFdw", ylab = expression(paste("Keratin ", delta^{18}, "O | pFdw")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}

dev.off()


## Fig. 6

meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.E.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd 


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.6.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(d2Hker_avg ~ HomeLocHab:exp, data=d, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Experiment", ylab=expression(paste("Ind. avg. keratin ", delta^{2}, "H (\u2030)")), cex.axis = 1, cex.lab=1, ylim=c(min(d$d2Hker_avg),max(d$d2Hker_avg)+8) )
legend("topleft", legend=c("Riparian","Meadow","Slope"), fill=Hab.cols, bg="white", bty="n", cex=1)
axis(side=1, at=c(2,5,8,11), labels=unique(d$exp), cex.axis=1)
box(lwd=1.25)

boxplot(d18Oker_avg ~ HomeLocHab:exp, data=d, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Experiment", ylab=expression(paste("Ind. avg. keratin ", delta^{18}, "O (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(2,5,8,11), labels=unique(d$exp), cex.axis=1)
box(lwd=1.25)

dev.off()


## Fig. 7

meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.E.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd 
  
yH = d$d2Hker_avg
yO = d$d18Oker_avg
x1H = d$d2Hin
x1O = d$d18Oin
x2 = d$Turn
x3 = d$pFdw 

mH = lm(yH ~ x1H + x2 + x3, data=d)
mO = lm(yO ~ x1O + x2 + x3, data=d)

yhatH = mH$coefficients[1] + mH$coefficients[2]*x1H + mH$coefficients[3]*x2 + mH$coefficients[4]*x3 + resid(mH)
yhatO = mO$coefficients[1] + mO$coefficients[2]*x1O + mO$coefficients[3]*x2 + mO$coefficients[4]*x3 + resid(mO)


exp.name = c("E1","E2A","E2B","E2C")
exp.cols = c("black","red","chartreuse4","blue1")


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.7.pdf", height=5, width=8, encoding="WinAnsi.enc")
 
par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

resid1 = resid(lm(x1H ~ x2 + x3))
resid2 = resid(lm(yhatH ~ x2 + x3)) 
plot(resid1, resid2, xlab = expression(paste("Input ", delta^{2}, "H | Turnover time + pFdw")), ylab = expression(paste("Keratin ", delta^{2}, "H | Turnover time + pFdw")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}
legend("topleft", exp.name, lty=rep(1,3), lwd=rep(2,3), col=exp.cols, bty="n")

resid1 = resid(lm(x1O ~ x2 + x3))
resid2 = resid(lm(yhatO ~ x2 + x3)) 
plot(resid1, resid2, xlab = expression(paste("Input ", delta^{18}, "O | Turnover time + pFdw")), ylab = expression(paste("Keratin ", delta^{18}, "O | Turnover time + pFdw")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}

dev.off()



## Fig. 8: boxplots of squared deviances for all reps from each exp and wild birds 

meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.data_rl.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd

exp.cols = c("grey","red","chartreuse4","blue1", "grey","red","chartreuse4","blue1", "yellow", "white")


## calc population variance (/n) 
test = d[d$exp=="B1_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg)

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testB1 = test


test = d[d$exp=="B2A_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg)

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testB2A = test


test = d[d$exp=="B2B_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg) 

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testB2B = test


test = d[d$exp=="B2C_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg)

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testB2C = test


test = d[d$exp=="E1_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 #this is what we are going to plot 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg) 

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testE1 = test


test = d[d$exp=="E2A_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg) 

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testE2A = test


test = d[d$exp=="E2B_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg)

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testE2B = test


test = d[d$exp=="E2C_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg)

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2 

testE2C = test


test = d[d$exp=="E1B1_rl",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg)

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2 

testE1B1 = test


test = d[d$exp=="DATA",]

mean = mean(test$d2Hker_avg)
test$diff = test$d2Hker_avg - mean
test$diff_squared = test$diff^2 

diff_squared_sum = sum(test$diff_squared)
n = length(test$d2Hker_avg)

test$diff_squared_d18O = (test$d18Oker_avg - mean(test$d18Oker_avg))^2

testDATA = test


test = rbind(testB1, testB2A, testB2B, testB2C, testE1, testE2A, testE2B, testE2C, testE1B1, testDATA)

test$exp = factor(test$exp, levels=c("B1_rl","B2A_rl","B2B_rl","B2C_rl","E1_rl","E2A_rl","E2B_rl","E2C_rl","E1B1_rl","DATA"))


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.8.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(diff_squared ~ exp, data=test, col=exp.cols,  
xaxt="n", xlab="Experiment", ylab=expression(paste("Ind. avg. keratin ", delta^{2}, "H squared deviance (\u2030)")), cex.axis = 1, cex.lab=1, outline=F)
axis(side=1, at=c(1:10), labels=FALSE)
box(lwd=1.25)

labels=paste(c("B1","B2A","B2B","B2C","E1","E2A","E2B","E2C","E1B1","SPTO"))
text(x = seq_along(labels), y = par("usr")[3]-9, srt=60, adj=1, xpd=TRUE, labels=labels, cex=0.90)

boxplot(diff_squared_d18O ~ exp, data=test, col=exp.cols,  
xaxt="n", xlab="Experiment", ylab=expression(paste("Ind. avg. keratin ", delta^{18}, "O squared deviance (\u2030) ")), cex.axis = 1, cex.lab=1, outline=F)
axis(side=1, at=c(1:10), labels=FALSE)
box(lwd=1.25)

labels=paste(c("B1","B2A","B2B","B2C","E1","E2A","E2B","E2C","E1B1","SPTO"))
text(x = seq_along(labels), y = par("usr")[3]-0.5, srt=60, adj=1, xpd=TRUE, labels=labels, cex=0.90)

dev.off()


## Fig. A1 
 
RBCclass = raster("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/RBCclass_na.gri")

col.palette = (terrain.colors(12,1))
VegCover.cols = c(col.palette[9],col.palette[5],col.palette[1],col.palette[7])


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.A1.pdf", height=6, width=6, encoding="WinAnsi.enc")

par(mar=c(4,4,3,0))

plot(RBCclass, col=VegCover.cols, xlab="Longitude (UTM, m)", ylab="Latitude (UTM, m)", legend=FALSE, cex.axis = 0.75, cex.lab=1)
legend("topright", legend=c("Class 1","Class 2","Class 3", "Class 4"), fill=VegCover.cols, bg="white", cex=1)

dev.off()


## Fig. A2
 
hrr = RBCclass
lrr.nrow.ncol = c(nrow(hrr)/10, ncol(hrr)/10) #588.3,544.3 (not integers)
fact = dim(hrr)[1:2] / lrr.nrow.ncol #10,10
a = aggregate(hrr,fact)

PercentCoverClass1 = raster::aggregate(hrr, fact, fun=function(x,na.rm=T) {mean(x==1, na.rm=na.rm)})
PercentCoverClass2 = raster::aggregate(hrr, fact, fun=function(x,na.rm=T) {mean(x==2, na.rm=na.rm)})
PercentCoverClass3 = raster::aggregate(hrr, fact, fun=function(x,na.rm=T) {mean(x==3, na.rm=na.rm)})
PercentCoverClass4 = raster::aggregate(hrr, fact, fun=function(x,na.rm=T) {mean(x==4, na.rm=na.rm)})


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.A2.pdf", height=6, width=6, encoding="WinAnsi.enc")

par(mfrow=c(2,2), mar=c(3,3,2,2)) #mgp=c(2.5,1,0)

plot(PercentCoverClass1, main = "Class 1 % cover")
plot(PercentCoverClass2, main = "Class 2 % cover")
plot(PercentCoverClass3, main = "Class 3 % cover")
plot(PercentCoverClass4, main = "Class 4 % cover")

dev.off()


## Fig. A3 

HOdata_w = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/water-data.csv")    
HOdata_insw = HOdata_w[which(HOdata_w$Substrate_type == "P Consumer"),] 

d18Oinsw.RIP = HOdata_insw$d18O[which(HOdata_insw$Habitat == "Riparian")]
d18Oinsw.MEAD = HOdata_insw$d18O[which(HOdata_insw$Habitat == "Meadow")]
d18Oinsw.SLP = HOdata_insw$d18O[which(HOdata_insw$Habitat == "Slope")] 
d2Hinsw.RIP = HOdata_insw$d2H[which(HOdata_insw$Habitat == "Riparian")]
d2Hinsw.MEAD = HOdata_insw$d2H[which(HOdata_insw$Habitat == "Meadow")]
d2Hinsw.SLP = HOdata_insw$d2H[which(HOdata_insw$Habitat == "Slope")]


d2Hinsw = c(d2Hinsw.RIP, d2Hinsw.MEAD, d2Hinsw.SLP)
d18Oinsw = c(d18Oinsw.RIP, d18Oinsw.MEAD, d18Oinsw.SLP)

Habitat.insw = c(rep("Riparian",length(d18Oinsw.RIP)),rep("Meadow",length(d18Oinsw.MEAD)),rep("Slope",length(d18Oinsw.SLP)))
Substrate.insw = rep("INSW",length(Habitat.insw))

insw = data.frame("d2H"=d2Hinsw, "d18O"=d18Oinsw, "Habitat"=Habitat.insw, "Substrate"=Substrate.insw)
insw$Habitat = ordered(insw$Habitat, levels=c("Riparian", "Meadow", "Slope"))


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.A3.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(d2H ~ Habitat, data=insw, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Habitat", ylab=expression(paste("Prey water ", delta^{2}, "H (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(1,2,3), labels=unique(insw$Habitat), cex.axis=1)
box(lwd=1.25)

boxplot(d18O ~ Habitat, data=insw, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Habitat", ylab=expression(paste("Prey water ", delta^{18}, "O (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(1,2,3), labels=unique(insw$Habitat), cex.axis=1)
box(lwd=1.25)

dev.off()


## Fig. A4
 
pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.A4.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mar=c(4.1, 4.5, 2.1, 2.1))

insw$col = ifelse(insw$Habitat == "Riparian", Hab.cols[1], ifelse(insw$Habitat == "Meadow", Hab.cols[2], Hab.cols[3]))
plot(insw$d18O, insw$d2H,  pch = 16, col=insw$col, cex=2, xlab=expression(paste("Prey water O isotopic composition")), ylab=expression(paste("Prey water H isotopic composition")), cex.axis = 1, cex.lab=1)
abline(lm(insw$d2H ~ insw$d18O), lwd=1.5)
box(lwd=1.25)
legend("topleft", c("Riparian", "Meadow", "Slope"), pch=rep(19,3), col=c("#00A600FF","#ADD900FF","#ECB176FF"), pt.cex=rep(2,3), bty="n")

dev.off()


## Fig. A5

HOdata_ins = read.xls("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/HO isotope data_substrates_2.xlsx", sheet =4, header = T)


ins = HOdata_ins
ins$Habitat = ifelse(ins$Habitat == "RIP", "Riparian", ifelse(ins$Habitat == "MEAD", "Meadow", "Slope"))
ins$Habitat = as.factor(ins$Habitat)
ins$Habitat = ordered(ins$Habitat, levels=c("Riparian", "Meadow", "Slope"))

ins.rip = ins[ins$Habitat == "Riparian",]
ins.mead = ins[ins$Habitat == "Meadow",]
ins.slp = ins[ins$Habitat == "Slope",]

ins2 = rbind(ins.rip, ins.mead, ins.slp)
ins=ins2


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.A5.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(corrd2H_avg ~ Habitat, data=ins, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Habitat", ylab=expression(paste("Prey ", delta^{2}, "H (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(1,2,3), labels=unique(ins$Habitat), cex.axis=1)
box(lwd=1.25)

boxplot(corrd18O_avg ~ Habitat, data=ins, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]),  
xaxt='n', xlab="Habitat", ylab=expression(paste("Prey ", delta^{18}, "O (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(1,2,3), labels=unique(ins$Habitat), cex.axis=1)
box(lwd=1.25)

dev.off()


## Fig. A6
 
HOoffs = read.xls("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/HO isotope data_substrates_2.xlsx", sheet = 5, header = T) 
HOoffs$Habitat = ordered(HOoffs$Habitat, levels=c("SLP.MEAD", "MEAD.RIP", "SLP.RIP"))


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.A6.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(DH ~ Habitat, data=HOoffs, 
xaxt='n', xlab="Habitat pair", ylab=expression(paste("", Delta^{2}, "H (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(1,2,3), labels=c("Sl.-Mead.","Mead.-Rip.","Sl.-Rip."), cex.axis=0.90)
box(lwd=1.25)

boxplot(DO ~ Habitat, data=HOoffs, 
xaxt='n', xlab="Habitat pair", ylab=expression(paste("", Delta^{18}, "O (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(1,2,3), labels=c("Sl.-Mead.","Mead.-Rip.","Sl.-Rip."), cex.axis=0.90)
box(lwd=1.25)

dev.off()


## Fig. A7

HOdata_ew = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/env_water-data.csv")
d18Oew.RIP = HOdata_ew$d18O
d2Hew.RIP = HOdata_ew$d2H

d2Hins.MEAD = HOdata_ins$corrd2H_avg
d18Oins.MEAD = HOdata_ins$corrd18O_avg


n = 50000 


## environmental water  
d18Od2Hew.RIP = mvrnorm(n = n, mu = c(mean(d18Oew.RIP),mean(d2Hew.RIP)), Sigma = cov(cbind(d18Oew.RIP,d2Hew.RIP)))
d18Oew.RIP_sim = c(d18Od2Hew.RIP[,1])
d2Hew.RIP_sim = c(d18Od2Hew.RIP[,2])

d18Od2Hew.RIP = mvrnorm(n = n, mu = c(mean(d18Oew.RIP),mean(d2Hew.RIP)), Sigma = cov(cbind(d18Oew.RIP,d2Hew.RIP)))
Ooff = rnorm(n, mean=2, sd=2/2)
Hoff = Ooff * runif(n, min=4, max=5) 
d18Oew.MEAD_sim = c(d18Od2Hew.RIP[,1]) + Ooff 
d2Hew.MEAD_sim = c(d18Od2Hew.RIP[,2]) + Hoff 

d18Od2Hew.RIP = mvrnorm(n = n, mu = c(mean(d18Oew.RIP),mean(d2Hew.RIP)), Sigma = cov(cbind(d18Oew.RIP,d2Hew.RIP)))
Ooff = rnorm(n, mean=6, sd=6/2)
Hoff = Ooff * runif(n, min=4, max=5) 
d18Oew.SLP_sim = c(d18Od2Hew.RIP[,1]) + Ooff 
d2Hew.SLP_sim = c(d18Od2Hew.RIP[,2]) + Hoff 

d2Hew_sim = c(d2Hew.RIP_sim, d2Hew.MEAD_sim, d2Hew.SLP_sim)
d18Oew_sim = c(d18Oew.RIP_sim, d18Oew.MEAD_sim, d18Oew.SLP_sim)

Habitat.ew = c(rep("RIP",n),rep("MEAD",n),rep("SLP",n))
Substrate.ew = rep("EW",length(Habitat.ew))

ew = data.frame("d2H"=d2Hew_sim, "d18O"=d18Oew_sim, "Habitat"=Habitat.ew, "Substrate"=Substrate.ew)
ew$Habitat = ordered(ew$Habitat, levels=c("RIP", "MEAD", "SLP"))


## prey water
d18O_ew = mean(HOdata_ew$d18O)   
d18O_max.evap = max(c(d18Oinsw.RIP, d18Oinsw.MEAD, d18Oinsw.SLP))
d18O_evap.scale = d18O_max.evap - d18O_ew 
evaps.RIP = rbeta(n,2,4)
evaps.MEAD = rbeta(n,2,2)
evaps.SLP = rbeta(n,4,2)
d18Oinsw_each_ins.RIP = d18O_ew + d18O_evap.scale * evaps.RIP
d18Oinsw_each_ins.MEAD = d18O_ew + d18O_evap.scale * evaps.MEAD
d18Oinsw_each_ins.SLP = d18O_ew + d18O_evap.scale * evaps.SLP
d18Oinsw_each_ins = c(d18Oinsw_each_ins.RIP, d18Oinsw_each_ins.MEAD, d18Oinsw_each_ins.SLP)
 
d2Hdata = c(d2Hinsw.RIP,d2Hinsw.MEAD,d2Hinsw.SLP)
d18Odata = c(d18Oinsw.RIP,d18Oinsw.MEAD,d18Oinsw.SLP) 
regMod = lm(d2Hdata ~ d18Odata)
d2Hinsw_pred = predict(regMod, data.frame(d18Odata = d18Oinsw_each_ins))
names(d2Hinsw_pred) = NULL
d2Hinsw_pred = d2Hinsw_pred + rnorm(n, mean(regMod$residuals), sd(regMod$residuals))

d2Hinsw_sim = d2Hinsw_pred
d18Oinsw_sim = d18Oinsw_each_ins

Habitat.insw = c(rep("RIP",n),rep("MEAD",n),rep("SLP",n))
Substrate.insw = rep("INSW",length(Habitat.insw))

insw = data.frame("d2H"=d2Hinsw_sim, "d18O"=d18Oinsw_sim, "Habitat"=Habitat.insw, "Substrate"=Substrate.insw)
insw$Habitat = ordered(insw$Habitat, levels=c("RIP", "MEAD", "SLP"))
 

## prey 
d18Od2Hins.RIP = mvrnorm(n = n, mu = c(mean(d18Oins.MEAD)-4.45,mean(d2Hins.MEAD)-8.84), Sigma = cov(cbind(d18Oins.MEAD,d2Hins.MEAD)))
d18Oins.RIP_sim = c(d18Od2Hins.RIP[,1])
d2Hins.RIP_sim = c(d18Od2Hins.RIP[,2])

d18Od2Hins.MEAD = mvrnorm(n = n, mu = c(mean(d18Oins.MEAD),mean(d2Hins.MEAD)), Sigma = cov(cbind(d18Oins.MEAD,d2Hins.MEAD)))
d18Oins.MEAD_sim = c(d18Od2Hins.MEAD[,1])
d2Hins.MEAD_sim = c(d18Od2Hins.MEAD[,2])

d18Od2Hins.SLP = mvrnorm(n = n, mu = c(mean(d18Oins.MEAD)+3.02,mean(d2Hins.MEAD)+34.12), Sigma = cov(cbind(d18Oins.MEAD,d2Hins.MEAD))) 
d18Oins.SLP_sim = c(d18Od2Hins.SLP[,1])
d2Hins.SLP_sim = c(d18Od2Hins.SLP[,2])

d2Hins_sim = c(d2Hins.RIP_sim, d2Hins.MEAD_sim, d2Hins.SLP_sim)
d18Oins_sim = c(d18Oins.RIP_sim, d18Oins.MEAD_sim, d18Oins.SLP_sim)

Habitat.ins = c(rep("RIP",length(d18Oins.RIP_sim)),rep("MEAD",length(d18Oins.RIP_sim)),rep("SLP",length(d18Oins.RIP_sim)))
Substrate.ins = rep("INS",length(Habitat.ins))

ins = data.frame("d2H"=d2Hins_sim, "d18O"=d18Oins_sim, "Habitat"=Habitat.ins, "Substrate"=Substrate.ins)
ins$Habitat = ordered(ins$Habitat, levels=c("RIP", "MEAD", "SLP"))


subs = rbind(ew, insw, ins)
subs$Habitat = ordered(subs$Habitat, levels=c("RIP", "MEAD", "SLP"))

 
pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.A7.pdf", height=5, width=8, encoding="WinAnsi.enc")

par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

boxplot(d2H ~ Habitat:Substrate, data=subs, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]), outline=FALSE, xaxt='n', xlab="Substrate", ylab=expression(paste("", delta^{2}, "H (\u2030)")), cex.axis = 1, cex.lab=1)
legend("topleft", legend=c("Riparian","Meadow","Slope"), fill=Hab.cols, bg="white", bty="n", cex=1)
axis(side=1, at=c(2,5,8), labels=c("Env. water","Prey water","Prey"), cex.axis=0.90)
box(lwd=1.25)

boxplot(d18O ~ Habitat:Substrate, data=subs, col=c(Hab.cols[1],Hab.cols[2],Hab.cols[3]), outline=FALSE, xaxt='n', xlab="Substrate", ylab=expression(paste("", delta^{18}, "O (\u2030)")), cex.axis = 1, cex.lab=1)
axis(side=1, at=c(2,5,8), labels=c("Env. water","Prey water","Prey"), cex.axis=0.90)
box(lwd=1.25)

dev.off()


## Fig. B1
 
meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.B.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd
 
 
yH = d$d2Hker_avg
yO = d$d18Oker_avg
x1 = d$Turn
x2 = d$pFdw 

mH = lm(yH ~ x1 + x2, data=d) 
mO = lm(yO ~ x1 + x2, data=d)

yhatH = mH$coefficients[1] + mH$coefficients[2]*x1 + mH$coefficients[3]*x2 + resid(mH)
yhatO = mO$coefficients[1] + mO$coefficients[2]*x1 + mO$coefficients[3]*x2 + resid(mO) 


exp.name = c("B1","B2A","B2B","B2C")
exp.cols = c("black","red","chartreuse4","blue1")


pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.B1.pdf", height=5, width=8, encoding="WinAnsi.enc")
 
par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

resid1 = resid(lm(x2 ~ x1))
resid2 = resid(lm(yhatH ~ x1)) 
plot(resid1, resid2, xlab = "pFdw | Turnover time", ylab = expression(paste("Keratin ", delta^{2}, "H | Turnover time")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}
legend("topright", exp.name, lty=rep(1,3), lwd=rep(2,3), col=exp.cols, bty="n")

resid1 = resid(lm(x2 ~ x1))
resid2 = resid(lm(yhatO ~ x1)) 
plot(resid1, resid2, xlab = "pFdw | Turnover time", ylab = expression(paste("Keratin ", delta^{18}, "O | Turnover time")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}

dev.off()


## Fig. B2

meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.E.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd


yH = d$d2Hker_avg
yO = d$d18Oker_avg
x1H = d$d2Hin
x1O = d$d18Oin
x2 = d$Turn
x3 = d$pFdw 

mH = lm(yH ~ x1H + x2 + x3, data=d)
mO = lm(yO ~ x1O + x2 + x3, data=d)

yhatH = mH$coefficients[1] + mH$coefficients[2]*x1H + mH$coefficients[3]*x2 + mH$coefficients[4]*x3 + resid(mH)
yhatO = mO$coefficients[1] + mO$coefficients[2]*x1O + mO$coefficients[3]*x2 + mO$coefficients[4]*x3 + resid(mO)


exp.name = c("E1","E2A","E2B","E2C")
exp.cols = c("black","red","chartreuse4","blue1")

 
pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.B2.pdf", height=5, width=8, encoding="WinAnsi.enc")
 
par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

resid1 = resid(lm(x2 ~ x1H + x3))
resid2 = resid(lm(yhatH ~ x1H + x3)) 
plot(resid1, resid2, xlab = expression(paste("Turnover time | Input ", delta^{2}, "H + pFdw")), ylab = expression(paste("Keratin ", delta^{2}, "H | Input ", delta^{2}, "H + pFdw")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}
legend("topleft", exp.name, lty=rep(1,3), lwd=rep(2,3), col=exp.cols, bty="n")

resid1 = resid(lm(x2 ~ x1H + x3))
resid2 = resid(lm(yhatO ~ x1H + x3)) 
plot(resid1, resid2, xlab = expression(paste("Turnover time | Input ", delta^{18}, "O + pFdw")), ylab = expression(paste("Keratin ", delta^{18}, "O | Input ", delta^{18}, "O + pFdw")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}

dev.off()


## Fig. B3

pdf("/Users/Sarah/Desktop/CombinedDynamicHOModel/figures/pdf/Fig.B3.pdf", height=5, width=8, encoding="WinAnsi.enc")
 
par(mfrow=c(1,2), oma=c(0,0,0.5,0.5), mar=c(4,4,0.5,0.5), mgp=c(2.5,1,0))

resid1 = resid(lm(x3 ~ x1H + x2))
resid2 = resid(lm(yhatH ~ x1H + x2)) 
plot(resid1, resid2, xlab = expression(paste("pFdw | Input ", delta^{2}, "H + Turnover time")), ylab = expression(paste("Keratin ", delta^{2}, "H | Input ", delta^{2}, "H + Turnover time")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}
legend("topleft", exp.name, lty=rep(1,3), lwd=rep(2,3), col=exp.cols, bty="n")

resid1 = resid(lm(x3 ~ x1O + x2))
resid2 = resid(lm(yhatO ~ x1O + x2)) 
plot(resid1, resid2, xlab = expression(paste("pFdw | Input ", delta^{18}, "O + Turnover time")), ylab = expression(paste("Keratin ", delta^{18}, "O | Input ", delta^{18}, "O + Turnover time")), col=exp.cols[d$exp], pch=16, cex=0.5)
for (i in exp.name) {
	x = resid1[d$exp==i]
	y = resid2[d$exp==i]
	fit = lm(y ~ x)
	xs = range(x)
	ys = predict(fit, newdata=data.frame(x=xs))
	lines(xs, ys, lwd=2, col=exp.cols[exp.name==i])
}

dev.off()



## Table B1

meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.B.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd

exp.name = "B1"

round(mean(d$d2Hker_avg[d$exp==exp.name]),2); round(mean(d$d18Oker_avg[d$exp==exp.name]),2)
round(sd(d$d2Hker_avg[d$exp==exp.name]),2); round(sd(d$d18Oker_avg[d$exp==exp.name]),2)
round((range(d$d2Hker_avg[d$exp==exp.name])[2] - range(d$d2Hker_avg[d$exp==exp.name])[1]),2); round((range(d$d18Oker_avg[d$exp==exp.name])[2] - range(d$d18Oker_avg[d$exp==exp.name])[1]),2) 
length(d$Rep[d$exp==exp.name])


meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.E.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd

exp.name = "E1"

round(mean(d$d2Hker_avg[d$exp==exp.name]),2); round(mean(d$d18Oker_avg[d$exp==exp.name]),2)
round(sd(d$d2Hker_avg[d$exp==exp.name]),2); round(sd(d$d18Oker_avg[d$exp==exp.name]),2)
round((range(d$d2Hker_avg[d$exp==exp.name])[2] - range(d$d2Hker_avg[d$exp==exp.name])[1]),2); round((range(d$d18Oker_avg[d$exp==exp.name])[2] - range(d$d18Oker_avg[d$exp==exp.name])[1]),2) 
length(d$Rep[d$exp==exp.name])


## Table B2

meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.B_rl.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd

exp.name = "B1_rl"

round(mean(d$d2Hker_avg[d$exp==exp.name]),2); round(mean(d$d18Oker_avg[d$exp==exp.name]),2)
round(sd(d$d2Hker_avg[d$exp==exp.name]),2); round(sd(d$d18Oker_avg[d$exp==exp.name]),2)
round((range(d$d2Hker_avg[d$exp==exp.name])[2] - range(d$d2Hker_avg[d$exp==exp.name])[1]),2); round((range(d$d18Oker_avg[d$exp==exp.name])[2] - range(d$d18Oker_avg[d$exp==exp.name])[1]),2) 
length(d$Rep[d$exp==exp.name])


meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.E_rl.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd

exp.name = "E1_rl"

round(mean(d$d2Hker_avg[d$exp==exp.name]),2); round(mean(d$d18Oker_avg[d$exp==exp.name]),2)
round(sd(d$d2Hker_avg[d$exp==exp.name]),2); round(sd(d$d18Oker_avg[d$exp==exp.name]),2)
round((range(d$d2Hker_avg[d$exp==exp.name])[2] - range(d$d2Hker_avg[d$exp==exp.name])[1]),2); round((range(d$d18Oker_avg[d$exp==exp.name])[2] - range(d$d18Oker_avg[d$exp==exp.name])[1]),2) 
length(d$Rep[d$exp==exp.name])


meansInd = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/input_output/tables/meansInd.data_rl.csv", header=T, sep=",")
meansInd = meansInd[,-1]
d = meansInd

exp.name = "DATA"

round(mean(d$d2Hker_avg[d$exp==exp.name]),2); round(mean(d$d18Oker_avg[d$exp==exp.name]),2)
round(sd(d$d2Hker_avg[d$exp==exp.name]),2); round(sd(d$d18Oker_avg[d$exp==exp.name]),2)
round((range(d$d2Hker_avg[d$exp==exp.name])[2] - range(d$d2Hker_avg[d$exp==exp.name])[1]),2); round((range(d$d18Oker_avg[d$exp==exp.name])[2] - range(d$d18Oker_avg[d$exp==exp.name])[1]),2) 
length(d$Rep[d$exp==exp.name])


