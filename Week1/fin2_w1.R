b11<- 0.008
b12<- -0.002
b13<- 0.004
b21<- -0.002
b22<- 0.002
b23<- -0.002
b31<- 0.004
b32<- -0.002
b33<- 0.008
Vmatrix<-matrix(c(b11,b12,b13,
                  + b21,b22,b23,
                  + b31,b32,b33),nrow=3,ncol=3,byrow=TRUE)

m1<- 6
m2<- 2
m3<- 4
rFree<- 1

#Question_1.
#Compute the mean return on the portfolio x=13(1,1,1)\textbf{x} =\frac{1}{3}(1, 1, 1) x=31(1,1,1) consisting only of the risky assets.
ptfVector<-c(1/3,1/3,1/3)
q1<-sum(ptfVector * c(m1,m2,m3))

#Question_2.
q2_not_in_proc<-sqrt(t(ptfVector) %*% Vmatrix %*% ptfVector)
q2_proc<-q2_not_in_proc*100
q2<-round(q2_proc,digits=2)
