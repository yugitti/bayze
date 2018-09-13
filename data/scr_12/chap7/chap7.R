#R3.3.1
#rstan2.12.1

library(rstan) #パッケージの読み込み

###7.1.1 流れ星問題1
scr <- "model711.stan" #Stanファイル名
source("data711.R") #dataファイル名
data <- list(N=N,x=x) #dataファイル内の変数をリスト形式にまとめる
par <- c("lambda", "sqrt_lambda", "p") #サンプリング結果を出力する変数名
war <- 5000            #ウォームアップ数
ite <- 100000          #繰り返し数
see <- 123             #乱数の種
dig <- 3               #有効数字
cha <- 1               #連鎖構成数

fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par,digits_summary=dig)                   #結果の表示


###7.1.2 ウミガメ問題
scr <- "model712.stan"
source("data712.R")
data <- list(N=N,x1=x1,x2=x2)
par <- c("lambdaA", "lambdaB", "delta", "p_delta")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1

fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par,digits_summary=dig)                   


###7.2 レストラン問題
scr <- "model72.stan"
source("data72.R")
data <- list(N=N,x=x)
par <- c("lambda", "mu", "p", "P_underp")

war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1

fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par,digits_summary=dig)  


###7.3 流れ星問題2
scr <- "model73.stan"
source("data73.R")
data <- list(N=N,x=x)
par <- c("pred")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1

fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par,digits_summary=dig)


###7.4 当たり付き棒アイス問題
scr <- "model74.stan"
source("data74.R")
data <- list(N=N,x=x)
par<-c("theta", "p3", "P", "mu")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1
fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par, digits_summary=dig)


###7.5 エントリーシート問題
scr <- "model75.stan"
source("data75.R")
data <- list(N=N,x=x)
par <- c("theta", "p", "pred")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1

fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par, digits_summary=dig)


###7.6 婚活問題
scr <- "model76.stan"
source("data76.R")
data <- list(N=N,x=x)
par <- c("zeta", "p_450")
war <- 5000
ite <- 100000
see <- 123
dig <- 3
cha <- 1

fit <- stan(file=scr, model_name=scr, data=data, pars=par, verbose=F, 
            seed=see, chains=cha, warmup=war, iter=ite)
print(fit,pars=par, digits_summary=dig)


