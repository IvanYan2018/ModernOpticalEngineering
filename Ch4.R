##############Chapter4################

#练习题1 Gregorian望远物镜-------------------
#光线追迹法

'条件'
no<-c(1,-1,1)
n<-no[1:2]
np<-no[2:3]

R<-c(-200,+50)
C<-1/R

t<-c(-130)

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

#双系统法
'主要计算各个子系统的焦距，是以光线依次入射该系统的方向来确定R的正负的，因此f=-R/2是成立的，只是R的正负是与光线传输方向有关的。'
R<-c(-200,-50)
f<--R/2
d<-130
n<--1

ftotal<-f[1]*f[2]/(f[1]+f[2]-d)
B<-ftotal*(f[1]-d)/f[1]

#练习题2 双光学系统-------------------
'正运算 fa,fb => fab,B'
f<-c(10,-10)
d<-5

ftotal<-f[1]*f[2]/(f[1]+f[2]-d)
B<-ftotal*(f[1]-d)/f[1]
ffd<--1*ftotal*(f[2]-d)/f[2]


#练习题3 双光学系统-------------------
'反运算 fab,B => fa,fb'
ftotal<-20
B<-10
d<-5
  
f<-c(0,0)
f[1]<-1*d*ftotal/(ftotal-B)
f[2]<--1*d*B/(ftotal-B-d)
f
