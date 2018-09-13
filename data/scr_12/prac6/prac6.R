#ベイズ統計学[HMC編]第6章・章末問題スクリプト
#R3.3.1
#rstan2.12.1
###6.3.1

library(rstan)

source("data631.R")
scr<-"model631.stan"
data <-list(N=N, x=x)

par<-c("mu","mu_over","mu_over2")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-1234               #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)

###6.3.2 

source("data632.R")
scr<-"model632.stan"
data <-list(N=N, x=x)

par<-c("sigmasq","sigmasq_under","sigmasq_under2")

war<-1000              
ite<-11000            
see<-1234              
dig<-3                 
cha<-1                 

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)

###6.3.3 

source("data633.R")
scr<-"model633.stan"
data <-list(N=N, x=x)

par<-c("quantile_90","prob_over")

war<-1000              
ite<-11000            
see<-1234              
dig<-3                 
cha<-1                 

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)

###6.3.4 

source("data634.R")
scr<-"model634.stan"
data <-list(N1=N1, N2=N2, x1=x1, x2=x2)

par<-c("mu1","mu2","sigma","delta","delta_over","delta_over1")

war<-1000              
ite<-11000            
see<-1234              
dig<-3                 
cha<-1                 

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)

###6.3.5 

source("data635.R")
scr<-"model635.stan"
data <-list(N=N, x=x)

par<-c("mu","Sigma","rho","delta","delta_over","delta_over2")

war<-1000              
ite<-11000            
see<-1234              
dig<-3                 
cha<-1                 

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)
