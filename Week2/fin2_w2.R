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
    print(i)
  }
  
  return((m_full-1)*100)
}

m_yearly<-(year_return(m_1,1)+year_return(m_1,2)+year_return(m_1,3)+year_return(m_1,4)+year_return(m_1,5))/5
