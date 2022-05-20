#Chapter2
# 牛顿与高斯公式
f<-10
h<-5
x<-c(2,1.9)

xp<--f^2/x
m<-f/x
hp<-m*h

s<-x-f
sp<-s*f/(s+f)
m<-f/(s+f)

###ex2
f<-10
h<-50*12
s<--12*200
sp<-s*f/(s+f)
xp<-sp-f
m<-f/(s+f)
hp<-m*h


###ex3
f<--5
h<-1
s<-c(-20,-21)
sp<-s*f/(s+f)
xp<-sp-f
m<-f/(s+f)
hp<-m*h
  
###ex3
f<-2
h<-1
s<--1
sp<-s*f/(s+f)
xp<-sp-f
m<-f/(s+f)
hp<-m*h
