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

m1<- -0.5186 # yearly estimated from monthly
m2<-  4.7057 # yearly estimated from monthly
m3<- -0.6986 # yearly estimated from monthly
rFree<- 1

m_real_1<-6
m_real_2<-2
m_real_3<-4

#Question_1: Compute an estimated efficient portfolio with 5% volatility. What is the estimated return on this portfolio?
ptf_norm<-sum(solve(Vmatrix) %*% c(m1-rFree,m2-rFree,m3-rFree))
ptfSharpe<-solve(Vmatrix) %*% c(m1-rFree,m2-rFree,m3-rFree)/ptf_norm

meanSharpe<-sum(ptfSharpe * c(m1,m2,m3))

sdSharpe<-sqrt(t(ptfSharpe) %*% Vmatrix %*% ptfSharpe)

slopeSharpe<-(meanSharpe-1)/(sdSharpe * 100)

q1_draft<-slopeSharpe * 5 + 1
q1<-round(q1_draft,digits=2)

#Question_2: Compute the true expected return (realized return) of the portfolio that you computed in Problem 1.
#
#MyComment: from forum => (1)use ptfSharpe but real rates (2) the same 5% valatility condition.
#
#ptf_norm<-sum(solve(Vmatrix) %*% c(m_real_1-rFree,m_real_2-rFree,m_real_3-rFree))
#ptfSharpe<-solve(Vmatrix) %*% c(m_real_1-rFree,m_real_2-rFree,m_real_3-rFree)/ptf_norm
meanSharpe_real<-sum(ptfSharpe * c(m_real_1,m_real_2,m_real_3)) # Q2 incorrect as asked 5!
slopeSharpe_real<-(meanSharpe_real-1)/(sdSharpe * 100)
q2_draft_real<-slopeSharpe_real * 5 + 1
q2<-round(q2_draft_real,digits = 2)

#Question_3: In the "Value-at-Risk" worksheet we list the simulated monthly returns again, and from these returns we compute the monthly rate of loss on an equally weighted portfolio of only the risky assets in column E. Use this data to estimate the Value-at-Risk at the 90% probability level.
loss_1<-(2.1675-1.8883+3.0712)/3
loss<-c(-1.1168,-1.3565,1.3754,-1.0396,0.5662,-0.0050,-1.9092,1.1039,-0.2332,-0.6678,-1.3045,0.8229,0.9616,-0.9685,1.0631,-2.8888,0.6022,1.1204,-0.9511,0.0810,-0.8619,0.0685,-0.2053,0.9565,0.1795,-2.4565,-0.0656,-0.1942,0.3471,-0.2564,1.2923,-0.3045,0.4619,-1.8819,-1.1397,1.9877,-0.0960,1.0440,-0.2722,-0.0218,0.8140,1.9191,2.1450,-0.3924,0.8846,-2.0569,-0.8699,-0.4551,-0.5114,-0.0412,0.2515,-0.6077,1.8807,-0.2756,-1.2639,-1.4916,-0.9395,2.3707,-0.2759,-0.7360)
loss_sorted<-loss[order(loss)]
p<-90/100 # =90% =0.9 => requested probability level.
N<-60 # =60 => number of samples for 60 months (12 months in 5 years).
q3_draft<-loss_sorted[p*N] # =1.2923
q3<-round(q3_draft,digits = 2) # =1.29

#Question_4: n the "Value-at-Risk" worksheet we list the simulated monthly returns again, and from these returns we compute the monthly rate of loss on an equally weighted portfolio of only the risky assets in column E. Use this data to estimate the Conditional Value-at-Risk at the 90% probability level.
p<-90/100 # =90% =0.9 => requested probability level.
N<-60 # =60 => number of samples for 60 months (12 months in 5 years).
K_p<-p*N # =54 => index.
q4_draft<-sum(loss_sorted[K_p:N])/(1-p)/N
q4<-round(q4_draft,digits = 2)

#Question_5: Consider a portfolio manager who has been successful in 12 years out of 15. Compute the probability of the manager having a track record as good as or better than this if he had no skill. You may assume that success or failure in any year is independent of success or failure in any other year.
n<-15
r<-12
p<-1/2
k<-r:n # 12:15
q5_draft<-p**n*sum(choose(n,k))
q5<-round(q5_draft,digits = 4) # =0.0176 - OK!

#How to calculate covariance matrix?
m_1<-c(2.1675,3.3875,-2.5194,-1.3959,-2.7264,0.7832,3.9749,-3.3869,-0.1714,0.5439,1.0913,-1.0361,-0.871,1.0377,-2.7236,2.7485,-0.8348,-1.2728,-0.5232,0.1174,1.1843,0.6003,-0.5785,-1.8834,1.0666,3.7147,-0.3213,1.3712,-0.7987,0.949,-2.5322,0.4613,-1.6205,4.7482,1.6551,-5.1563,-1.0738,-1.2798,-0.7793,-0.3613,0.1896,-2.6047,-3.1645,-0.6066,0.8595,3.642,0.6684,1.5556,1.4717,-2.6414,-1.6742,1.5918,-1.4625,0.0491,-0.2674,1.0214,1.0679,-5.4239,0.4125,4.9665)

m_y1<-(1+m_1[1]/100)*(1+m_1[2]/100)*(1+m_1[3]/100)*(1+m_1[4]/100)*(1+m_1[5]/100)*(1+m_1[6]/100)*(1+m_1[7]/100)*(1+m_1[8]/100)*(1+m_1[9]/100)*(1+m_1[10]/100)*(1+m_1[11]/100)*(1+m_1[12]/100)


year_return<-function(m, n_year)
{
  m_full<-1
  l<-12*(n_year-1)+1
  r<-12*n_year
  for(i in l:r)
  {
    m_full <- m_full * (1+m[i]/100)
    #print(i) # for debug only!
  }
  
  return((m_full-1)*100)
}

m_yearly<-(year_return(m_1,1)+year_return(m_1,2)+year_return(m_1,3)+year_return(m_1,4)+year_return(m_1,5))/5
