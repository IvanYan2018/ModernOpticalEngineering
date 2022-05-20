#近轴ynu追迹
'条件'
n<-c(1,1.5,1.6)
np<-c(1.5,1.6,1.0)
C<-c(0.02,-0.02,0)
t<-c(10,2)

nu<-seq(from=0,to=0,length=3)
nup<-seq(from=0,to=0,length=3)
y<-seq(from=0,to=0,length=3)

'初始条件'
nu[1]<--0.0666
y[1]<-0

'追迹计算'
v<-seq(3)
for (i in v)
{
  if(i != 1)
  {
    y[i]<-y[i-1]+t[i-1]*nup[i-1]/np[i-1]
    nu[i]=nup[i-1]
  }
  nup[i]<--y[i]*(np[i]-n[i])*C[i]+nu[i]
}
up<-nup[3]/np[3]

####################计算焦距和后截距###################

'条件'
no<-c(1,1.5,1.6,1)
n<-no[1:3]
np<-no[2:4]
C<-c(0.02,-0.02,0)
t<-c(10,2)

nu<-seq(from=0,to=0,length=3)
nup<-seq(from=0,to=0,length=3)
y<-seq(from=0,to=0,length=3)

'初始条件'
nu[1]<-0
y[1]<-10

'追迹计算'
v<-seq(3)
for (i in v)
{
  if(i != 1)
  {
    y[i]<-y[i-1]+t[i-1]*nup[i-1]/np[i-1]
    nu[i]=nup[i-1]
  }
  nup[i]<--y[i]*(np[i]-n[i])*C[i]+nu[i]
}

efl<--y[1]*np[3]/nup[3]
bfl<--y[3]*np[3]/nup[3]


'光路逆追迹'
no<-c(1,1.5,1.6,1)

C<-c(0.02,-0.02,0)
t<-c(10,2)

nr<-rev(no)[1:3]
npr<-rev(no)[2:4]
Cr<--1*rev(C)
tr<-rev(t)

nur<-seq(from=0,to=0,length=3)
nupr<-seq(from=0,to=0,length=3)
yr<-seq(from=0,to=0,length=3)

'初始条件'
nur[1]<-0
yr[1]<-10

v<-seq(3)
for (i in v)
{
  if(i != 1)
  {
    yr[i]<-yr[i-1]+tr[i-1]*nupr[i-1]/npr[i-1]
    nur[i]=nupr[i-1]
  }
  nupr[i]<--yr[i]*(npr[i]-nr[i])*Cr[i]+nur[i]
}

ffl<--yr[3]*npr[3]/nupr[3]



###ex3.2
n<-1.5
hp<--50
h<-10
m<-hp/h
s<-120/(-1+m)
sp<-m*s
f<-s*sp/(s-sp)
R=2*f*(n-1)
R
f

###ex3.3 卡塞格林反射镜系统

'条件'
no<-c(1,-1,1)
n<-no[1:2]
np<-no[2:3]

R<-c(-200,-50)
C<-1/R

t<-c(-80)

nu<-seq(from=0,to=0,length=2)
nup<-seq(from=0,to=0,length=2)
y<-seq(from=0,to=0,length=2)

'初始条件'
nu[1]<-0
y[1]<-1

'追迹计算'
v<-seq(2)
for (i in v)
{
  if(i != 1)
  {
    y[i]<-y[i-1]+t[i-1]*nup[i-1]/np[i-1]
    nu[i]=nup[i-1]
  }
  nup[i]<--y[i]*(np[i]-n[i])*C[i]+nu[i]
}


efl<--y[1]*np[2]/nup[2]
bfl<--y[2]*np[2]/nup[2]

#####exe1###


'条件'
no<-c(1.5,1)
n<-no[1]
np<-no[2]
h<-1

R<-c(-10)
C<-1/R

t<-c(10)

nu<-seq(from=0,to=0,length=1)
nup<-seq(from=0,to=0,length=1)
y<-seq(from=0,to=0,length=1)

'初始条件'

y[1]<-1
nu[1]<-y[1]/t*n[1]

'追迹计算'
v<-seq(1)
for (i in v)
{
 #  if(i != 1)
 #  {
 #    y[i]<-y[i-1]+t[i-1]*nup[i-1]/np[i-1]
 #    nu[i]=nup[i-1]
 #  }
  nup[i]<--y[i]*(np[i]-n[i])*C[i]+nu[i]
}

lp<--y[1]*np[1]/nup[1]
up<-nup/np
m<-n*lp/np/t
hp<-m*h


efl<--y[1]*np[2]/nup[2]
bfl<--y[2]*np[2]/nup[2]

#####exe2###


'条件'
no<-c(1,1.5,1)
n<-no[1:2]
np<-no[2:3]
#h<-1

R<-c(+100,-100)
C<-1/R

t<-c(10)

nu<-seq(from=0,to=0,length=2)
nup<-seq(from=0,to=0,length=2)
y<-seq(from=0,to=0,length=2)

'初始条件'

y[1]<-10
nu[1]<-0

'追迹计算'
v<-seq(2)
for (i in v)
{
   if(i != 1)
   {
     y[i]<-y[i-1]+t[i-1]*nup[i-1]/np[i-1]
     nu[i]=nup[i-1]
   }
  nup[i]<--y[i]*(np[i]-n[i])*C[i]+nu[i]
}

lp<--y[2]*np[2]/nup[2]



efl<--y[1]*np[2]/nup[2]
bfl<--y[2]*np[2]/nup[2]

