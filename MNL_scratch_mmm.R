
N = 1000

x1 = cbind(rep(0,N),rep(0,N),rnorm(N,0,1))
x2 = cbind(rep(1,N),rep(0,N),rnorm(N,0,1))
x3 = cbind(rep(0,N),rep(1,N),rnorm(N,0,1))

head(cbind(x1,x2,x3))



b1 = -1
b2 = 2
bp = -2

beta = c(b1,b2,bp)

choice = rep(0,N)

for(i in 1:N){
  U = c()
  U[1] = beta %*% x1[i,]
  U[2] = beta %*% x2[i,]
  U[3] = beta %*% x3[i,]
  
  P = exp(U)/sum(exp(U))
  
  choice[i] = sample(x=3,size=1,replace =F,prob=P)

}


ll = function(beta){
  res = 0
  for(i in 1:N){
    U[1] = beta %*% x1[i,]
    U[2] = beta %*% x2[i,]
    U[3] = beta %*% x3[i,]
    P = exp(U)/sum(exp(U))
    res = res - log(P[choice[i]])
  }
  return(res)
  }



par = rnorm(3)
ML = nlm(ll,par,hessian = T)

mode = ML$estimate
SE = sqrt(diag(solve(ML$hessian)))

tvalue = mode/SE
ll = 2*ML$minimum

results  = cbind(Estimate = mode, SE= SE,Tvalue = tvalue,minusll=ll)

