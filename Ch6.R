###############Chapter6################

#Contributions from aspheric surfaces-----------------------------------------------

xlim<-seq(-2,2,by=0.01)
ylim<-seq(-2,2,by=0.01)

c<-0.25
A2<-1
A4<-1
M<-mesh(xlim,ylim)
x<-M$x
y<-M$y
s2<-x^2+y^2
z<-(c*s2)/(1+sqrt(1-c^2*s2))+A2*s2+A4*s2^2
df<-data.frame(x=x,y=y,z=z)


#绘图
library(plot3D)
library(rgl)
plot3d(x,y,z,col = 'blue',alpha = 0.3,type = 'l')

#Sample Calculations---------------------------------------------------------------

#Ray Tracing
'条件'
no<-c(1,1.611,1,1.644,1,1,1.611,1)
n<-no[1:7]
np<-no[2:8]

R<-c(-200,-50)
C<-c(1/37.4,1/(-341.48),1/(-42.65),1/36.4,0,1/204.52,1/(-37.05))
t<-c(5.9,12.93,2.5,2,9.85,5.9,77.405)

nu<-seq(from=0,to=0,length=7)
nup<-seq(from=0,to=0,length=7)
ia<-seq(from=0,to=0,length=7)
y<-seq(from=0,to=0,length=7)

'初始条件'
nu[1]<-1.46e-7
y[1]<-14.6

'追迹计算'
v<-seq(7)
for (i in v)
{
  if(i != 1)
  {
    y[i]<-y[i-1]+t[i-1]*nup[i-1]/np[i-1]
    nu[i]=nup[i-1]
  }
  nup[i]<--y[i]*(np[i]-n[i])*C[i]+nu[i]
  ia[i]<-(C[i]*y[i]+nu[i]/n[i])
}


efl<--y[1]*np[2]/nup[2]
bfl<--y[2]*np[2]/nup[2]
