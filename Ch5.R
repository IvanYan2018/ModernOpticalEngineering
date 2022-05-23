##############Chapter5################

##Fig 5.24
c<-10
d<-0

TA <- function(up) (c*(tan(up))^3+d*(tan(up))^5)

plot(TA, -pi/4, pi/4, main = expression(f(x) == c*(tan(up))^3+d*(tan(up))^5), lwd = 3, col = "blue",ylim = c(-1, +1))


##Exercise 1
LA<-c(-1.0,-0.5)
tanU<-c(-0.5,-0.35)

'a) compute TA based on LA and tanU'
TAa<--1*LA*tanU

'b) compute TA based on distance from focus point and reference point.'
'DA:distance from focus point and reference point'
d<--0.2
DA<-LA-d
TAb<--1*DA*tanU

##Exercise 2
Coma_t<-c(1,0.5,0.25)
R<-Coma_t/3
dy<-2*R
dy
dx<-rep(0,length(dy))


'plot'
library(ggplot2)
library(scales)
x<-seq(-2,2,by=0.01)


sfr<-data.frame(x,y=tan(pi/3)*x,type=rep('ref',length(x)))# 参考线60度
sfl<-data.frame(x,y=tan(-1*pi/3)*x,type=rep('ref',length(x)))# 参考线-60度
df<-rbind(sfr,sfl)


v<-seq(3)
for (i in v)
{
  xp<-x[x>=-1*R[i] & x<=R[i]]-dx[i]
  yup<-sqrt(R[i]^2-xp^2)
  ydp<--1*sqrt(R[i]^2-xp^2)
  yu<-yup+dy[i]
  yd<-ydp+dy[i]
  tmpDfu<-data.frame(x=xp,y=yu,type=rep(paste('C',i),length(xp)))
  tmpDfd<-data.frame(x=xp,y=yd,type=rep(paste('C',i),length(xp)))
  df<-rbind(df,tmpDfu,tmpDfd)
}

g<-ggplot(df,aes(x,y))
# g<-g+geom_line(aes(colour=type,stat='identity'))
# g<-g+geom_line(aes(colour=type))
g<-g+geom_point(aes(colour=type))
g<-g+scale_y_continuous(limits=c(0, 2))
g<-g+scale_x_continuous(limits=c(-1, 1))+theme(aspect.ratio = 1)
g

##Exercise 3
fref<-100
yref<-20
href<-5
LAref<-1
Coma_tref<-1
Ztref<-1

fnew<-c(200,50)
ynew<-c(10,10)
hnew<-c(2.5,10)

LAnew<-LAref*(fnew/fref)*(ynew/yref)^2
LAnew
Coma_tnew<-Coma_tref*(fnew/fref)*(ynew/yref)^2*(hnew/href)
Coma_tnew
Ztnew<-Ztref*(fnew/fref)*(hnew/href)^2
Ztnew


##Exercise 4
U<-seq(-pi/4,pi/4,by=0.01)
TA<-1*tan(U)+1*(tan(U))^2+1*(tan(U))^3
plot(U,TA,type="l")
