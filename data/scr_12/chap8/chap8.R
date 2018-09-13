#ベイズ統計学[入門編]第8章スクリプト
#R3.3.1
#rstan2.12.1

###8.1 比率の差・リスク比・オッズ比

library(rstan)

scr<-"model81.stan"
source("data81.R")
data <-list(N=N, n=n)

par<-c("p","d","delta_over","RR","OR")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-12345              #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)


###8.2 2群の相関係数の差に関する推測

source("data82.R")
scr<-"model82.stan"
data <-list(N=N, xA=xA, xB=xB)

par<-c("rhoA","rhoB","delta_r","delta_r_over")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-1234              #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)


###8.3 対応のある相関係数の差に関する推測

source("data83.R")
scr<-"model83.stan"
data <-list(N=N, x=x)

par<-c("rho","delta_r2","delta_r2_over")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-12345              #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)


###8.4 切断データの相関係数の推定

source("data84.R")
scr<-"model84.stan"
data <-list(Ny=Ny, y=y, Nx=Nx, x=x)

par<-c("rho_truncated","rho_corrected")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-12345              #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)

###8.4.3 完全データの相関係数の推定
source("data843.R")
scr<-"model843.stan"
data <-list(Ny=Ny, y=y)

par<-c("rho_complete")

war<-1000               #バーンイン期間
ite<-11000              #サンプル数
see<-12345              #シード
dig<-3                  #有効数字
cha<-1                  #チェーンの数

fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)

print(fit,pars=par,digits_summary=dig)


###8.5-8.6 級内相関・一般化可能性理論

source("data85.R")
data <- list(S=S, R=R, Score=Score)

war<-15000            #バーンイン期間
ite<-50000            #サンプル数
see<-12345            #シード
dig<-3                #有効数字
cha<-1                #チェーンの数

## 変量効果モデル
scr<-"model85random.stan"
par <- c("mu","alpha","beta","sig2subject", "sig2rater", "sig2within","ICC21","ICC24","rho5","rho6","nine6");
fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)
print(fit,pars=par,digits_summary=dig)

## 混合効果モデル
scr<-"model85mixed.stan"
par <- c("mu","alpha","beta","sig2subject", "sig2within","ICC31","ICC34","rho5","rho6","nine");
fit <- stan(file = scr, data = data, warm=war, iter=ite, seed=see,
	      pars=par,chains=cha)
print(fit,pars=par,digits_summary=dig)