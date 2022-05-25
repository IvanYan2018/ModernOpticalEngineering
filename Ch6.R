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
library(dplyr)

###计算函数设计
'追迹计算函数'
trcParFunc<-function(nus,ys)
{

  'nus是指入射第一面前的光线倾斜角，ys是指第一面上的投影高度'
  nu<-seq(from=0,to=0,length=7)
  nup<-seq(from=0,to=0,length=7)
  ia<-seq(from=0,to=0,length=7)
  y<-seq(from=0,to=0,length=7)
  
  nu[1]<-nus
  y[1]<-ys
  
  '近轴追迹过程'
  'y[i]是光线在第i面投射高度，nu[i]是光线在第i面左侧的折射率*倾斜角，ia[i]是光线在第i面的入射角'
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
  
  ret<-data.frame(y=y,u=nu/n,up=nup/np,ia=ia)
  return(ret)
}

'近轴横向轴上色差贡献量计算函数--输入参数：轴上近轴光线追迹数据'
TAchCParFunc<-function(x,dn,dnp)
{
  k=nrow(x)
  TAchC<-seq(from=0,to=0,length=k)
  
  v<-seq(k)
  for (i in v)
  {
    TAchC[i]<--1*x$PY[i]*x$PI[i]*(dn[i]-n[i]/np[i]*dnp[i])/np[k]/x$PUP[k]
  }
  return(TAchC)
}
'近轴横向色差贡献量计算函数--输入参数：近轴主光线追迹数据xp和轴上近轴光线追迹数据x'
TchCParFunc<-function(x,xp,dn,dnp)
{
  k=nrow(x)
  TchC<-seq(from=0,to=0,length=k)
  
  v<-seq(k)
  for (i in v)
  {
    TchC[i]<--1*x$PY[i]*xp$PI[i]*(dn[i]-n[i]/np[i]*dnp[i])/np[k]/x$PUP[k]
  }
  return(TchC)
}

'三级像差-横向三级球差贡献量计算函数--输入参数：组合的轴上和倾斜主光线追迹数据x,和物高h1'
TSCParFunc<-function(x,h1)
{
  k=nrow(x)
  TSC<-seq(from=0,to=0,length=k)
  h<-seq(from=0,to=0,length=k)
  hp<-seq(from=0,to=0,length=k)
  h[1]<-h1
  
  i<-1
  Inv<-x$PYC[i]*n[i]*x$PU[i]-x$PY[i]*n[i]*x$PUC[i]
  B<-n*(np-n)*x$PY*(x$PUP+x$PI)/(2*np*Inv)
  
  v<-seq(k)
  for (i in v)
  {
    if(i!=1)
    {
      h[i]=hp[i-1]
    }
    hp[i]=Inv/
    
    
  }
  
  
  
    
  
    
  TSC<-B*x$PI^2*h
  
  return(TSC)
}

###实际计算准备

'环境条件'
'd光折射率-查看玻璃参数手册'
no<-c(1,1.61272,1,1.64769,1,1,1.61272,1)
'FC光色散-查看玻璃参数手册'
dnoFC<-c(0,0.010450,0,0.019135,0,0,0.010450,0)
dnoFd<-c(0,0.00727,0,0.01354,0,0,0.00727,0)
'光路计算折射率表示'
n<-no[1:7]
np<-no[2:8]
dnFC<-dnoFC[1:7]
dnpFC<-dnoFC[2:8]
dnFd<-dnoFd[1:7]
dnpFd<-dnoFd[2:8]

C<-c(1/37.4,1/(-341.48),1/(-42.65),1/36.4,0,1/204.52,1/(-37.05))
t<-c(5.9,12.93,2.5,2,9.85,5.9,77.405)

'计算轴上光线追迹'
nus<-1.46e-7
ys<-14.6
TraAxis<-trcParFunc(nus,ys)%>%rename(PY=y,PU=u,PUP=up,PI=ia)

'计算倾斜主光线追迹及色差'
nus<-0.21
ys<--6.411174
TraSlant<-trcParFunc(nus,ys)%>%rename(PYC=y,PUC=u,PUPC=up,PIC=ia)

'横向轴上初级色差贡献-FC差别'
TAchC_FC<-TAchCParFunc(TraAxis,dnFC,dnpFC)
'横向轴上二级光谱贡献-Fd差别'
TAchC_Fd<-TAchCParFunc(TraAxis,dnFd,dnpFd)

'横向初级色差贡献-FC差别'
TchC_FC<-TchCParFunc(TraAxis,TraSlant,dnFC,dnpFC)
'横向二级光谱贡献贡献-Fd差别'
TchC_Fd<-TchCParFunc(TraAxis,TraSlant,dnFd,dnpFd)

'三级像差-横向三级球差'
TRACDATA<-cbind(TraAxis,TraSlant)
TSC<-TSCParFunc(TRACDATA,-2.1e-7)


##整合计算数据
SUMA<-cbind(TraAxis,TraSlant,TAchC_FC,TAchC_Fd,TchC_FC,TchC_Fd)%>%rename(PYC=y,PUC=u,PUPC=up,PIC=ia)


