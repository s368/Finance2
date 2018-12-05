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

ptfVector<-c(1/3,1/3,1/3)
Evector<-c(1,1,1)

#Question_1: Compute the mean return on the portfolio x=13(1,1,1)\textbf{x} =\frac{1}{3}(1, 1, 1) x=31(1,1,1) consisting only of the risky assets.
q1<-sum(ptfVector * c(m1,m2,m3)) #=4

#Question_2: Compute the volatility of the return on the portfolio x=13(1,1,1)\textbf{x} =\frac{1}{3}(1, 1, 1) x=31(1,1,1) consisting only of the risky assets (i.e. same portfolio as Question 1).
q2_not_in_proc<-sqrt(t(ptfVector) %*% Vmatrix %*% ptfVector)
q2_proc<-q2_not_in_proc*100
q2<-round(q2_proc,digits=2) #=4.47

#Question_3: Compute the mean return on the minimum variance portfolio of just the risky assets.
norm_q3<-t(Evector) %*% solve(Vmatrix) %*% Evector #=1500
ptfOptim<-solve(Vmatrix)%*%Evector/norm_q3[1,1]
q3<-sum(ptfOptim * c(m1,m2,m3)) #=3

#Question_4: Compute the mean return on the Sharpe optimal portfolio for this market.
norm_q4<-sum(solve(Vmatrix) %*% c(m1-rFree,m2-rFree,m3-rFree))
ptfSharpe<-solve(Vmatrix) %*% c(m1-rFree,m2-rFree,m3-rFree)/norm_q4
meanSharpe<-sum(ptfSharpe * c(m1,m2,m3))
q4<-round(meanSharpe,digits = 2)

#Question_5: Compute the volatility of the Sharpe optimal portfolio for this market.
sdSharpe<-sqrt(t(ptfSharpe) %*% Vmatrix %*% ptfSharpe)
sdSharpe_in_proc<-sdSharpe * 100
q5<-round(sdSharpe,digits = 2)

#Question_6: Using the results in the previous question, compute the slope of the capital market line.
slopeSharpe<-(meanSharpe-1)/sdSharpe_in_proc
q6<-round(slopeSharpe,digits = 2)

#Question_7: Suppose the volatility of a an efficient investment opportunity is ??=5%\sigma= 5\%??=5%. What is the return on this opportunity?
q7_draft<-slopeSharpe * 5 + 1
q7<-round(q7_draft,digits = 2)

