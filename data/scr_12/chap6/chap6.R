#ベイズ統計学[HMC編]第6章スクリプト
#R3.3.1
#rstan2.12.1

###6.1.1 正規分布の平均に関する推測

library(rstan)

source("data611.R")
scr<-"model611.stan"
data <-list(N=N, x=x)

par<-c("mu","mu_over","mu_over2","es_over")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-12345              #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)

###6.1.2 正規分布の分散に関する推測

source("data612.R")
scr<-"model612.stan"
data <-list(N=N, x=x)


par<-c("sigmasq","sigmasq_over","sigmasq_over2")

war<-1000              
ite<-11000            
see<-12345              
dig<-3                 
cha<-1                 

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)

###6.1.3 正規分布の分位に関する推測

source("data613.R")   
scr<-"model613.stan"  
data <-list(N=N, x=x) 


par<-c("quantile_3rd","prob_over") 

war<-1000               
ite<-11000              
see<-12345               
dig<-3                  
cha<-1                  

fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig) 

traceplot(fit,inc_warmup=F)
plot(fit)

###6.2.1 独立な2群の平均値差に関する推測

source("data621.R")
scr<-"model621.stan"
data <-list(N1=N1, N2=N2, x1=x1, x2=x2)

par<-c("mu1","mu2","sigma1","sigma2","delta","delta_over","delta_over1")

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

###6.2.2 対応のある2群の平均値差に関する推測
source("data622.R")
scr<-"model622.stan"
data <-list(N=N, x=x)

par<-c("mu","Sigma","rho","delta","delta_over","delta_over2","rho_over","rho_over05")


war<-1000              
ite<-11000              
see<-12345               
dig<-3                  
cha<-1                  


fit <- stan(file = scr, data = data, iter=ite, seed=see, warmup=war,
	      pars=par,chains=cha)
print(fit,pars=par,digits_summary=dig)

traceplot(fit,inc_warmup=F)
plot(fit)
