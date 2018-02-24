
#7.1


z0 = c(1,1,1,1,1,1)
z1 = c(10,5,7,19,11,8)

z = as.matrix(cbind(z0,z1))


y = as.matrix(c(15,9,3,25,7,13))

betas = solve(t(z)%*%z) %*% t(z) %*% y

yhat = z%*%betas

errs = y -yhat

rss = t(errs)%*%errs




#7.2
z0 = c(1,1,1,1,1,1)
z1 = c(10,5,7,19,11,18)
z2 = c(2,3,3,6,7,9)
z = as.matrix(cbind(z1,z2))
y = as.matrix(c(15,9,3,25,7,13))

z[,2] = (z[,2] - mean(z[,2]))/sqrt(var(z[,2]))
z[,1] = (z[,1] - mean(z[,1]))/sqrt(var(z[,1]))
y[,1] = (y[,1] - mean(y[,1]))/sqrt(var(y[,1]))

betas_72 = solve(t(z)%*%z) %*% t(z) %*% y

yhat_72 = z%*%betas_72
errs_72 = y -yhat_72
rss_72 = t(errs_72)%*%errs_72

b1_raw = betas_72[1,]*sqrt((var(as.matrix(c(15,9,3,25,7,13)))/(length(z[,1])-1))/(var(as.matrix(cbind(z1,z2))[,1])/(length(z[,2])-1)))
b2_raw = betas_72[2,]*sqrt((var(as.matrix(c(15,9,3,25,7,13)))/(length(z[,2])-1))/(var(as.matrix(cbind(z1,z2))[,2])/(length(z[,2])-1)))



cran = as.matrix(cbind(c(17.17,19.71),c(19.71,23.71)))

(t(as.matrix(c(-1.53,1.53))) %*% solve(cran) %*% as.matrix(c(1.53,1.53)))*35

as.matrix(c(22.86,24.39))

(t(as.matrix(c(22.86,24.39))) %*% as.matrix(c(-1,1)) %*% t(as.matrix(c(-1,1)))%*% solve(cran) %*% as.matrix(c(-1,1)) %*% t(as.matrix(c(-1,1))) %*% as.matrix(c(22.86,24.39)) )*35


(1.54*4.3133^-1*1.53)*35*33/34



#7.14
#the sample correlation coefficients are the unconditional correlation between Z1 and Y, and Z2 and Y.
#this is the conditional correlation, i.e. the variance in Y explained by Z1, having already accounted for the variance in Y explained for Z2
-.35 - (-.6*.82)/(sqrt(1-.82^2)*sqrt(1+.6))


#7.17 - a)
sales<-as.matrix(c(108.28, 152.36, 95.04, 65.45, 62.97, 263.99, 265.19, 285.06, 92.01, 165.68))
profits<-as.matrix(c(17.05, 16.59, 10.91, 14.14, 9.52, 25.33, 18.54, 15.73, 8.10, 11.13))
assets <-as.matrix(c(1484.10, 750.33, 766.42, 1110.46, 1031.29, 195.26, 193.83, 191.11, 1175.16, 211.15))

reg <- lm(profits~ sales + assets)
summary(reg)
# 
# Call:
#   lm(formula = profits ~ sales + assets)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -4.954 -1.215 -0.316  1.686  6.224 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 0.013325   7.641453   0.002   0.9987  
# sales       0.068058   0.027851   2.444   0.0445 *
#   assets      0.005768   0.004946   1.166   0.2817  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.863 on 7 degrees of freedom
# Multiple R-squared:  0.5569,	Adjusted R-squared:  0.4303 
# F-statistic: 4.399 on 2 and 7 DF,  p-value: 0.05792


fullreg = augment(reg)

var(fullreg$.resid)






