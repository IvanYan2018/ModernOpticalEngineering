
library(ggplot2)
library(scales)

# x坐标
x<-seq(-2*pi,2*pi,by=0.01)

# y坐标
s1<-data.frame(x,y=sin(x),type=rep('sin',length(x)))# 正弦
s2<-data.frame(x,y=cos(x),type=rep('cos',length(x)))# 余弦
s3<-data.frame(x,y=tan(x),type=rep('tan',length(x)))# 正切
s4<-data.frame(x,y=1/tan(x),type=rep('cot',length(x)))# 余切
s5<-data.frame(x,y=1/sin(x),type=rep('sec',length(x)))# 正割
s6<-data.frame(x,y=1/cos(x),type=rep('csc',length(x)))# 余割
df<-rbind(s1,s2,s3,s4,s5,s6)

# 用ggplot2画图

g<-ggplot(df,aes(x,y))
g<-g+geom_line(aes(colour=type,stat='identity'))
g<-g+scale_y_continuous(limits=c(0, 2))
g<-g+scale_x_continuous(breaks=seq(-2*pi,2*pi,by=pi),labels=c("-2*pi","-pi","0","pi","2*pi"))
g

y <- function(x) (exp(-(x^2)/2))/sqrt(2*pi)

plot(y, -5, 5, main = expression(f(x) == frac(1,sqrt(2*pi))*e^(-frac(x^2,2))), lwd = 3, col = "blue")


# Chapter1
## length--mm

#example 1
lamda<-0.4e-3
D<-1e3
AB<-0.1

OPMin<-lamda*D/(2*AB)
OPMin

OPMax<-lamda*D/AB
OPMax

#exmaple 2
N<-10
No<-seq(10)
X<-seq(from=1,to=1,length.out=N)
X<-X*(-1)^(No-1)
lamda<-0.4e-3
SH<-seq(from=lamda/4,to=10*lamda/4,by=lamda/4)
R<-20
Y<-sqrt(2*R*SH-SH^2)
Y
plot(Y,X,type='l')

#EX 1
#3
I1=sin(30*pi/180)
n1<-c(1,1.33,1.33)
n2<-c(1.5,1,1.5)
I2<-asin(n1/n2*sin(I1))
I2deg<-I2*180/pi
I2deg


  
