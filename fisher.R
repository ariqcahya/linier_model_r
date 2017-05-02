## Algoritma Fisher untuk IRIS Dataset
## Ariq Cahya Wardhana / G651160411
##========================================

##========== Membuat 2D test data
library(MASS)
rotmat <- function(t){
  return( matrix(c(cos(t),sin(t),-sin(t),cos(t)),nrow=2) )
}
translate <- function(d,v){
  plus <- function(v1,v2){v1+v2}
  return(t(apply(d,1,plus,v)))
}
np <- 1000
nn <- 500
xp <- mvrnorm(n = np, mu=c(0,0),Sigma=matrix(c(1,0.7,0.7,1),nrow=2))
xn <- mvrnorm(n = nn, mu=c(0,0),Sigma=matrix(c(1,0.7,0.7,1),nrow=2))
x <- rbind( translate(xp %*% rotmat(-pi/8),c(3/2,3/2)),translate( xn %*% rotmat(pi/8) , c(-3/2,-3/2)) )
m <- apply(x,2,mean)
x <- translate(x,-m)
x.class <- c(rep(1,np),rep(-1,nn))
plot(x,col=x.class+3,asp=1)

##=========== Max Scatter Ratio
u <- apply(x,2,mean)
up <- apply(subset(x,x.class==+1),2,mean)
un <- apply(subset(x,x.class==-1),2,mean)
np <- sum(x.class==+1)
nn <- sum(x.class==-1)
SB <- nn * (un - u) %*% t(up - u)
scatter <- function(v){
  ( (v - un) %*% t(v - un) ) + ( (v - up) %*% t(v - up) )
}
SW <- matrix( apply( apply(x,1,scatter), 1, sum ), nrow=2 )
t <- seq(- pi , pi, 0.05)
uv <- cbind(cos(t), sin(t))
action <- function(uv, m) {
  abs( uv %*% m %*% matrix(uv) )
}
ratios <- apply(uv, 1, action, SB) / apply(uv, 1, action, SW)
plot(x,col=x.class+3,asp=1)
par(lwd=2)
lines(20 * ratios * uv)
mr <- which.max(ratios)
muv <- uv[mr,]
mv <- 40*ratios[mr]*muv
arrows(0,0,mv[1],mv[2])

##============ Melakukan Prediksi Kelas
xp <- as.vector(x %*% muv)
rxp <- round(range(xp),0)+c(-1,1)
xpn <- subset(xp,x.class==-1)
xpp <- subset(xp,x.class==+1)
break_points <- seq(rxp[1],rxp[2],length.out=30)
hn <- hist(xpn,breaks=break_points,plot=FALSE)
hp <- hist(xpp,breaks=break_points,plot=FALSE)
b = (mean(xpp) * sd(xpn) + mean(xpn) * sd(xpp)) / (sd(xpp) + sd(xpn))

##============ Membuat garis diskriminan
plot(x,col=x.class+3,asp=1)
par(lwd=2)
lines(10 * ratios * uv)
arrows(0,0,mv[1],mv[2])
abline(b/muv[2],-muv[1]/muv[2])

##============ Prediksi class dengan fisher diskriminan
distance.from.plane = function(x,w,b) {
  b - sum(x*w)
}
classify.fisher = function(x,w,b) {
  distances = apply(x, 1, distance.from.plane, w, b)
  return(ifelse(distances < 0, -1, +1))
}

##============ Klasifikasi dengan iris data set
data(iris)
x <- cbind(iris$Petal.Length,iris$Petal.Width)
Y <- ifelse(iris$Species == "virginica", +1, -1)
u <- apply(x,2,mean)
up <- apply(subset(x,Y==+1),2,mean)
un <- apply(subset(x,Y==-1),2,mean)
np <- sum(Y==+1)
nn <- sum(Y==-1)
SB <- nn * (un - u) %*% t(up - u)
SW <- matrix( apply( apply(x,1,scatter), 1, sum ), nrow=2 )
ratios <- apply(uv, 1, action, SB) / apply(uv, 1, action, SW)
mr <- which.max(ratios)
muv <- uv[mr,]
mv <- 40*ratios[mr]*muv
xp <- as.vector(x %*% muv)
rxp <- round(range(xp),0)+c(-1,1)
xpn <- subset(xp,Y==-1)
xpp <- subset(xp,Y==+1)
b = (mean(xpp) * sd(xpn) + mean(xpn) * sd(xpp)) / (sd(xpp) + sd(xpn))
plot(x,col=Y+3,asp=1)
par(lwd=2)
abline(b/muv[2],-muv[1]/muv[2])
summary(abs(Y - classify.fisher(x,muv,b) ))
sum(abs(Y - classify.fisher(x,muv,b) ))
