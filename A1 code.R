library(rootSolve)
d= 31+30+31+30+31+31
ytm=function(data,n){
  d=n+184
  m1=n/365
  m2=d/365
  ytm=rep(0,length(data$Dirty))
  ytm1=function(x){data$Dirty[1]-data$C[1]*exp(-x*m1)-100*exp(-x*m1)}
  ytm[1]=uniroot.all(ytm1,c(0:1))
  ytm2=function(x){data$Dirty[2]-data$C[2]*(exp(-ytm[1]*m1)+exp(-x*m2))-100*exp(-x*m2)}
  ytm[2]=uniroot.all(ytm2,c(0:1))
  ytm3=function(x){data$Dirty[3]-data$C[3]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-x*(1+m1)))-100*exp(-x*(1+m1))}
  ytm[3]=uniroot.all(ytm3,c(0:1))
  ytm4=function(x){data$Dirty[4]-data$C[4]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-ytm[3]*(1+m1))+exp(-x*(1+m2)))-100*exp(-x*(1+m2))}
  ytm[4]=uniroot.all(ytm4,c(0:1))
  ytm5=function(x){data$Dirty[5]-data$C[5]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-ytm[3]*(1+m1))+exp(-ytm[4]*(1+m2))+exp(-x*(2+m1)))-100*exp(-x*(2+m1))}
                                             
  ytm[5]=uniroot.all(ytm5,c(0:1))
  ytm6=function(x){data$Dirty[6]-data$C[6]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-ytm[3]*(1+m1))+exp(-ytm[4]*(1+m2))+
                                              exp(-ytm[5]*(2+m1))+exp(-x*(2+m2)))-100*exp(-x*(2+m2))}
  ytm[6]=uniroot.all(ytm6,c(0:1))
  ytm7=function(x){data$Dirty[7]-data$C[7]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-ytm[3]*(1+m1))+exp(-ytm[4]*(1+m2))+
                                               exp(-ytm[5]*(2+m1))+exp(-ytm[6]*(2+m2))+exp(-x*(3+m1)))-100*exp(-x*(3+m1))}
  ytm[7]=uniroot.all(ytm7,c(0:1))
  ytm8 =function(x){data$Dirty[8]-data$C[8]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-ytm[3]*(1+m1))+exp(-ytm[4]*(1+m2))+
                                               exp(-ytm[5]*(2+m1))+exp(-ytm[6]*(2+m2))+exp(-ytm[7]*(3+m1))+exp(-x*(3+m2)))-100*exp(-x*(3+m2))}
  ytm[8]=uniroot.all(ytm8,c(0:1))
  ytm9 =function(x){data$Dirty[9]-data$C[9]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-ytm[3]*(1+m1))+exp(-ytm[4]*(1+m2))+
                                               exp(-ytm[5]*(2+m1))+exp(-ytm[6]*(2+m2))+exp(-ytm[7]*(3+m1))+exp(-ytm[8]*(3+m2))+exp(-x*(4+m1)))-100*exp(-x*(4+m1))}
  ytm[9]=uniroot.all(ytm9,c(0:1))
  ytm10 =function(x){data$Dirty[10]-data$C[10]*(exp(-ytm[1]*m1)+exp(-ytm[2]*m2)+exp(-ytm[3]*(1+m1))+exp(-ytm[4]*(1+m2))+
                                                  exp(-ytm[5]*(2+m1))+exp(-ytm[6]*(2+m2))+exp(-ytm[7]*(3+m1))+exp(-ytm[8]*(3+m2))+exp(-ytm[9]*(4+m1))+exp(-x*(4+m2)))-100*exp(-x*(4+m2))}
  ytm[10]=uniroot.all(ytm10,c(0:1))
  return(ytm)
}

f=function(data,n){
  d=n+184
  m1=n/365
  m2=d/365
  y=rep(0,length(data$Dirty))
  y1=function(x){data$Dirty[1]-data$C[1]*exp(-x*m1)-100*exp(-x*m1)}
  y[1]=uniroot.all(y1,c(0:1))
  y2=function(x){data$Dirty[2]-data$C[2]*(exp(-x*m1)+exp(-x*m2))-100*exp(-x*m2)}
  y[2]=uniroot.all(y2,c(0:1))
  y3=function(x){data$Dirty[3]-data$C[3]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1)))-100*exp(-x*(1+m1))}
  y[3]=uniroot.all(y3,c(0:1))
  y4=function(x){data$Dirty[4]-data$C[4]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1))+exp(-x*(1+m2)))-100*exp(-x*(1+m2))}
  y[4]=uniroot.all(y4,c(0:1))
  y5=function(x){data$Dirty[5]-data$C[5]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1))+exp(-x*(1+m2))+
                                            exp(-x*(2+m1)))-100*exp(-x*(2+m1))}
  y[5]=uniroot.all(y5,c(0:1))
  y6=function(x){data$Dirty[6]-data$C[6]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1))+exp(-x*(1+m2))+
                                            exp(-x*(2+m1))+exp(-x*(2+m2)))-100*exp(-x*(2+m2))}
  y[6]=uniroot.all(y6,c(0:1))
  y7 =function(x){data$Dirty[7]-data$C[7]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1))+exp(-x*(1+m2))+
                                             exp(-x*(2+m1))+exp(-x*(2+m2))+exp(-x*(3+m1)))-100*exp(-x*(3+m1))}
  y[7]=uniroot.all(y7,c(0:1))
  y8 =function(x){data$Dirty[8]-data$C[8]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1))+exp(-x*(1+m2))+
                                             exp(-x*(2+m1))+exp(-x*(2+m2))+exp(-x*(3+m1))+exp(-x*(3+m2)))-100*exp(-x*(3+m2))}
  y[8]=uniroot.all(y8,c(0:1))
  y9 =function(x){data$Dirty[9]-data$C[9]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1))+exp(-x*(1+m2))+
                                             exp(-x*(2+m1))+exp(-x*(2+m2))+exp(-x*(3+m1))+exp(-x*(3+m2))+exp(-x*(4+m1)))-100*exp(-x*(4+m1))}
  y[9]=uniroot.all(y9,c(0:1))
  y10 =function(x){data$Dirty[10]-data$C[10]*(exp(-x*m1)+exp(-x*m2)+exp(-x*(1+m1))+exp(-x*(1+m2))+
                                                exp(-x*(2+m1))+exp(-x*(2+m2))+exp(-x*(3+m1))+exp(-x*(3+m2))+exp(-x*(4+m1))+exp(-x*(4+m2)))-100*exp(-x*(4+m2))}
  y[10]=uniroot.all(y10,c(0:1))
  return(y)
}

forward=function(rate){
  r=rep(0,length(rate)-1)
  for (c in 1:(length(rate)-1))
  {
    r[c]=((c+1)*rate[c+1]-rate[1])/c
  }
  return(r)
}

data02=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0102.csv")
data03=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0103.csv")
data06=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0106.csv")
data07=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0107.csv")
data08=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0108.csv")
data09=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0109.csv")
data10=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0110.csv")
data13=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0113.csv")
data14=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0114.csv")
data15=read.csv("C:\\Users\\lenovo\\Desktop\\APM466\\A1\\0115.csv")
n1=30+31+30+31+2
n2=n1+1
n3=n2+3
n4=n3+1
n5=n4+1
n6=n5+1
n7=n6+1
n8=n7+3
n9=n8+1
n10=n9+1
data02$Dirty=data02$Close_price+data02$Coupon*n1/365
data02$Dirty
data02$C=data02$Coupon*0.5
data03$Dirty=data03$Close_price+ data03$Coupon*n2/365
data03$C= data03$Coupon*0.5
data06$Dirty=data06$Close_price+ data06$Coupon*n3/365
data06$C= data06$Coupon*0.5
data07$Dirty=data07$Close_price+ data07$Coupon*n4/365
data07$C= data07$Coupon*0.5
data08$Dirty=data08$Close_price+ data08$Coupon*n5/365
data08$C= data08$Coupon*0.5
data09$Dirty=data09$Close_price+ data09$Coupon*n6/365
data09$C= data09$Coupon*0.5
data10$Dirty=data10$Close_price+ data10$Coupon*n7/365
data10$C= data10$Coupon*0.5
data13$Dirty=data13$Close_price+ data13$Coupon*n8/365
data13$C= data13$Coupon*0.5
data14$Dirty=data14$Close_price+ data14$Coupon*n9/365
data14$C= data14$Coupon*0.5
data15$Dirty=data15$Close_price+ data15$Coupon*n10/365
data15$C= data15$Coupon*0.5

y=array(0,dim=c(10,10))
y[1, ]=f(data02, 31-2+28)
y[2, ]=f(data03, 31-3+28)
y[3, ]=f(data06, 31-6+28)
y[4, ]=f(data07, 31-7+28)
y[5, ]=f(data08, 31-8+28)
y[6, ]=f(data09, 31-9+28)
y[7, ]=f(data10, 31-10+28)
y[8, ]=f(data13, 31-13+28)
y[9, ]=f(data14, 31-14+28)
y[10, ]=f(data15, 31-15+28)

years=c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(years, y[1, 1:10],type="b", main="Yield to Maturity", ylim=c(min(y),max(y)),ylab="YTM")
for (q in 2:10)
{
  lines(years,y[q, 1:10],type="b",col=q)
  
}
legend("topright",legend=seq(10),col=seq(10),pch=rep(1,10),horiz=TRUE,cex=0.5)

spot=array(0,dim=c(10,10))
spot[1, ]=ytm(data02, 31-2+28)
spot[2, ]=ytm(data03, 31-3+28)
spot[3, ]=ytm(data06, 31-6+28)
spot[4, ]=ytm(data07, 31-7+28)
spot[5, ]=ytm(data08, 31-8+28)
spot[6, ]=ytm(data09, 31-9+28)
spot[7, ]=ytm(data10, 31-10+28)
spot[8, ]=ytm(data13, 31-13+28)
spot[9, ]=ytm(data14, 31-14+28)
spot[10, ]=ytm(data15, 31-15+28)

years=c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(years, spot[1, 1:10],type="b", main="Spot Rate", ylim=c(min(spot),max(spot)),ylab="Spot Rate")
for (q in 2:10)
{
  lines(years,spot[q, 1:10],type="b",col=q)
  
}
legend("topright",legend=seq(10),col=seq(10),pch=rep(1,10),horiz=TRUE,cex=0.5)

fr=array(0, dim=c(10,9))
for (index in 1:10)
{
  fr[index,]=forward(spot[index,])
}
years.fr=c(1,2,3,4)
plot(years.fr, fr[1, 1:4],type="b", main="Forward Rate", ylim=c(min(fr),max(fr)),ylab="Forward Rate")
for (q in 2:10)
{
  lines(years.fr,fr[q, 1:4],type="b",col=q)
  
}
legend("bottomright",legend=seq(10),col=seq(10),pch=rep(1,10),horiz=TRUE,cex=0.5)

y.return=log(y[2:10, ]/y[1:9, ])
fr.return=log(fr[2:10, ]/fr[1:9, ])
var(y.return)[1:5,1:5]
var(fr.return)[1:4,1:4]
eigen(var(y.return)[1:5,1:5])
eigen(var(fr.return)[1:4, 1:4])
  

