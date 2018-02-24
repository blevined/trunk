library(MASS)



a = c(2,sqrt(2)/2)
b = c(sqrt(2)/2,1)

covar_mat = as.matrix(rbind(a,b))

sigma = mvrnorm(n = 1000, mu = c(0,2),Sigma = covar_mat)


sigma = data.frame(sigma) %>% arrange()


x1 = sort(rnorm(1000,0,2))
x2 = sort(rnorm(1000,2,1))

sig2 = cbind(x1,x2)


plot_ly(data = data.frame(sigma),type = "contour")



contour(x=x1,y)

plot(sigma)






x.points <- seq(-3,3,length.out=100)
y.points <- seq(-3,3,length.out=100)
z <- matrix(0,nrow=100,ncol=100)
mu <- c(1,1)
sigma <- matrix(c(2,1,1,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                      mean=mu,sigma=sigma)
  }
}
contour(x.points,y.points,z)







###4.3,e
a1 = c(0,1,0)
a2 = c(-5/2,1,-1)
a = as.matrix(rbind(a1,a2))
aprime = t(a)


zig1 = c(1,-2,0) 
zig2 = c(-2,5,0)
zig3 = c(0,0,2)
zig = as.matrix(rbind(zig1,zig2,zig3))


a%*%zig%*%aprime


##4.4,a
c = as.matrix(c(3,-2,1))
mutu = as.matrix(c(2,-3,1))

t(c)%*%mutu #yay

zig1 = c(1,1,1) 
zig2 = c(1,3,2)
zig3 = c(1,2,2)

zig = as.matrix(rbind(zig1,zig2,zig3))

t(c)%*%zig%*%c #yay



4/6



 N
1.666666666



