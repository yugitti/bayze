# 第8章宿題

library(rstan)

#8.7.1
scr<-"model871.stan"
source("data871.R")

data <-list(N=N, n=n)
par<-c("p","d","delta_over","RR","RRover","OR")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-12345              #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)
print(fit,pars=par,digits_summary=dig)


#8.7.2
scr<-"model872.stan"
source("data872.R")
data <- list(S=S, R=R, Score=Score)

war<-15000            #バーンイン期間
ite<-50000            #サンプル数
see<-12345            #シード
dig<-3                #有効数字
cha<-1                #チェーンの数
par<-c("ICC31","ICC37")

fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)
print(fit,pars=par,digits_summary=dig)