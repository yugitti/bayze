
library(rstan) #パッケージの読み込み
###第7章・章末問題###################################
###7.7.1
scr<-"model771.stan"
source("data771.R")
data<-list(N=N,Xn=Xn,Xm=Xm)
par<-c("lambdaM","lambdaN","delta","delta_p")
war<-5000
ite<-100000
see<-123
dig<-3
cha<-1
fit<-stan(file=scr, model_name=scr, data=data, pars=par,verbose=F, 
seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par,digits_summary=dig)

###7.7.2
scr<-"model772.stan"
source("data772.R")
data<-list(N=N,x=x)
par<-c("lambda","eta","p","P_underp")
war<-5000
ite<-100000
see<-123
dig<-3
cha<-1
fit<-stan(file=scr, model_name=scr, data=data, pars=par,verbose=F, 
seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par,digits_summary=dig)

###7.7.3
scr <- "model773.stan"
source("data773.R")
data <- list(N=N,x=x)
par <- c("p4", "mu")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1
fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par,digits_summary=dig)

###7.7.4
scr <- "model774.stan"
source("data774.R")
data <- list(N=N,x=x)
par <- c("p", "pred")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1
fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par, digits_summary=dig)

###7.7.5
scr <- "model775.stan"
source("data775.R")
data <- list(N=N,x=x)
par <- c("zeta", "p_500")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1
fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par, digits_summary=dig)
