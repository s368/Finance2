b11<- 0.0056
b12<- -0.002
b13<- 0.0037
b21<- -0.002
b22<- 0.0022
b23<- -0.0022
b31<- 0.0037
b32<- -0.0022
b33<- 0.0074

#how this matrix (i.e. b11...b33) was calculated?
Vmatrix<-matrix(c(b11,b12,b13,
                + b21,b22,b23,
                + b31,b32,b33),nrow=3,ncol=3,byrow=TRUE)

m1<- -0.5186
m2<-  4.7057
m3<- -0.6986
rFree<- 1

ptf_norm<-sum(solve(Vmatrix) %*% c(m1-rFree,m2-rFree,m3-rFree))
ptfSharpe<-solve(Vmatrix) %*% c(m1-rFree,m2-rFree,m3-rFree)/ptf_norm

meanSharpe<-sum(ptfSharpe * c(m1,m2,m3))

sdSharpe<-sqrt(t(ptfSharpe) %*% Vmatrix %*% ptfSharpe)

slopeSharpe<-(meanSharpe-1)/(sdSharpe * 100)

q1<-slopeSharpe * 5 + 1
q1_round<-round(q1,digits=2)
