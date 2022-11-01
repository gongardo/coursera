rho <- 0.7
rem.prop <- 0.2

mu<-c(1, 2)
sigma.1<-1
sigma.2<-1
Sigma.x=matrix(c(sigma.1^2, rho*sigma.1*sigma.2, rho*sigma.1*sigma.2, sigma.2^2),nc=2)
x <- rmvn(n = n,
          mu = c(1, 2),
          Sigma = Sigma.x)

y <- 1 + 1.5 * x[, 2] + rnorm(n)
#x<-scale(x)
x<- (x-mean(x))/sd(x)
simdata <- data.frame(y = y, x = x[, 1])
resultALL <-
  Btest(
    models = list(M0 = y ~ 1, M1 = y ~ .),
    data = simdata,
    prior.betas = "gZellner"
  )
####
resultALL$BFi0

X.miss=ampute(x,prop=rem.prop,patterns=c(0,1),mech = "MAR")
X.miss=X.miss$amp
se.van=is.na(X.miss[,1])

#removing results:
B.without <-
  Btest(
    models = list(M0 = y ~ 1, M1 = y ~ .),
    data = simdata[!se.van, ],
    prior.betas = "gZellner"
  )
x1without <-  B.without$PostProbi[2]
B.without$BFi0


Y = y
X = X.miss


#Y aqui comienza la parte de la implementacion del BF_10, donde se han integrado los missings:
n<-length(Y)
Y.bar<-mean(Y)
miss<-is.na(X[,1])
s2.y<-sum(Y*Y)-n*Y.bar^2
opt<- optim(par=c(0,0,0), fn=log.integrand, control=control, hessian=T)
#$par
#[1]  1.74525330  0.39485614 -0.05340004
#Integración de Laplace:
p<- 3
lap<- exp(opt$value)*(2*pi)^(p/2)*sqrt(-1/det(opt$hessian))
lap
B.without$BFi0
resultALL$BFi0
lap/resultALL$BFi0[2]

B.without$BFi0[2]/resultALL$BFi0[2]
