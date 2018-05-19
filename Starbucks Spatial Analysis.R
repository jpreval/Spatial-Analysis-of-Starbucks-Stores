library("ggmap")
library("ggplot2")


xTX <- read.table("/Users/ClumsyJace/PycharmProjects/MachineLearningProjects/Texas_Loc.csv", header=TRUE, sep = ",")
head(xTX)



map = get_map(location='Texas',zoom=6)
ggmap(map)


p = ggmap(map) + geom_point(aes(x=Longitude, y = Latitude),data=xTX, color="red", 
                            size = .2 , alpha = .5)
p 

xTX1 <- read.table("/Users/ClumsyJace/PycharmProjects/MachineLearningProjects/TexasQ.csv", header=TRUE, sep = ",")
head(xTX1)

windows(width=1000, height=1000)

plot(xTX1$X, xTX1$Y, data = xTX1,  main="Texas Cartesian Scatter plot", 
     xlab="X Coordinate", ylab="Y Coordinate", cex=.5 , type="p",
     col="blue")


library("spatstat")

pp = ppp(xTX1$X, xTX1$Y, c(-4000,4000),c(-4000,4000))

plot(density(pp, sigma = 200))

s = smooth.ppp(pp)
plot(s)

k = Kest(pp)
plot(k)

Q = quadratcount(pp,nx=2,ny=2)

windows(width=1000, height=1000)
plot(pp, cex=.5)

plot(Q, add=TRUE, cex=1.5, col="blue")

quadrat.test(pp,nx=5,ny=1)


#texas data
xtdata <- read.table("C:/Users/joell/Desktop/TexasData.csv", header=TRUE, sep = ",")
head(xtdata)


xtdata$City <- factor(xtdata$City)
xtdata$Target <- factor(xtdata$Target)


install.packages("nnet")
library("nnet")

formula <- "Target ~  Median_Hous_Inc + Percent_hou_50 + Percent_hou_50more + Population + Median_price_increase"

mlogit <- nnet::multinom(formula, data = xtdata)

summary(mlogit)
