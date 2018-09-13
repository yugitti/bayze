#####################################
#ベイズ統計学  第1章  Rスクリプト
#####################################
#表1.1
x<-c(rep(1,40),rep(0,60))
lln <-function(){
for (i in c(5,20,50,100,100000)){
   a<-sample(x, i, replace = T)
   print(round(sum(a)/i,4))
}}
set.seed(1234);lln()
set.seed(2345);lln()
set.seed(3456);lln()
set.seed(7890);lln()

#迷惑メイルフィルタの計算（ベイズ更新の例）
x<-matrix(c(
0.11,0.01,0,
0.12,0.02,0,
0.15,0.01,0,
0.13,0.02,0
),4,3,byrow=T)
rownames(x)<-c("絶対必勝","完全無料","投資指南","急騰予想")
colnames(x)<-c("A1","A2","p")
prior<-0.6
x[1,3]<-(x[1,1]*prior)/(x[1,1]*prior+x[1,2]*(1-prior))
for (i in (2:nrow(x))){
  x[i,3]<-(x[i,1]*x[(i-1),3])/(x[i,1]*x[(i-1),3]+x[i,2]*(1-x[(i-1),3]))
}
print(x)

#血液鑑定問題
真犯人<-function(p){p/(p+(1/100000)*(1-p))}

真犯人(1/2)
真犯人(0.00001)
真犯人(1/37000000)
#どんな結果でもだせる
curve(真犯人, from = 0, to = 0.0001, 
      type = "l",  xlab =  "事前確率", ylab = "事後確率")



#迷惑メイルフィルタの事後確率の変化
tikuji01<-function(x){
(0.1*x)/(0.1*x + 0.01*(1-x))}

逐次更新<-function(prior){
  m<-7
  x<-numeric(m)
  x[1]<-tikuji01(prior)
  for (i in 2:m){
    x[i]<-tikuji01(x[i-1])
  }
  print(round(x,4))
}

逐次更新(0.01)
逐次更新(0.05)
逐次更新(0.1)
逐次更新(0.2)
逐次更新(0.3)
逐次更新(0.4)
逐次更新(0.5)


#####################################
#ベイズ統計学  第2章  Rスクリプト
#####################################


#図2.1　ベルヌイ分布
binom01<-function(x){dbinom(x,1,0.8)}; z01<-numeric(2)
for (i in 0:1){z01[i+1]<-binom01(i)}; names(z01)<-0:1
binom02<-function(x){dbinom(x,1,0.4)}; z02<-numeric(2)
for (i in 0:1){z02[i+1]<-binom02(i)}; names(z02)<-0:1

par(mfrow=c(1,2))
barplot(z01,ylab='probability',xlab='θ = 0.8 ',cex.lab=1.5,cex.axis=1.5)
barplot(z02,ylab='probability',xlab='θ = 0.4 ',cex.lab=1.5,cex.axis=1.5)
par(mfrow=c(1,1))
#dev.copy2eps(file="z02ベルヌイ.eps",family='Japan1')


#図2.2　2項分布PMF
binom01<-function(x){dbinom(x,10,0.8)};z01<-numeric(11)
for (i in 0:10){z01[i+1]<-binom01(i)}; names(z01)<-0:10
binom02<-function(x){dbinom(x,10,0.5)};z02<-numeric(11)
for (i in 0:10){z02[i+1]<-binom02(i)}; names(z02)<-0:10
par(mfrow=c(1,2))
barplot(z01,ylab='probability',xlab='θ = 0.8 ',cex.lab=1.7,cex.axis=1.7)
barplot(z02,ylab='probability',xlab='θ = 0.5 ',cex.lab=1.7,cex.axis=1.7)
par(mfrow=c(1,1))
#dev.copy2eps(file="z022項PMF.eps",family='Japan1')


#図2.3　2項分布/分布関数
binom01<-function(x){pbinom(x,10,0.8)};z01<-numeric(11)
for (i in 0:10){z01[i+1]<-binom01(i)}; names(z01)<-0:10
binom02<-function(x){pbinom(x,10,0.5)};z02<-numeric(11)
for (i in 0:10){z02[i+1]<-binom02(i)}; names(z02)<-0:10
par(mfrow=c(1,2))
barplot(z01,ylab='probability',xlab='θ = 0.8 ',cex.lab=1.7,cex.axis=1.7)
barplot(z02,ylab='probability',xlab='θ = 0.5 ',cex.lab=1.7,cex.axis=1.7)
par(mfrow=c(1,1))
#dev.copy2eps(file="z022項DF.eps",family='Japan1')


#図2.4　一様分布
beta01<-function(x){dbeta(x,1,1)}
curve(beta01,0,1,ylab='probability  density',xlab='α = 0 ,　β = 1 ',cex.lab=1.5,cex.axis=1.5)
#dev.copy2eps(file="z02一様分布F.eps",family='Japan1')


#165cm以上175cm未満の学生が観察される確率
pnorm(175,170,7)- pnorm(165,170,7)

#図2.5　正規分布
par(mfrow=c(1,2))
curve(dnorm(x,170,7.0),145,195,ylab='probability density',xlab='密度関数',cex.lab=1.5,cex.axis=1.5)
segments(165,0,165,dnorm(165,170,7.0))
segments(175,0,175,dnorm(175,170,7.0))
text(170,0.01,"52.5％",cex=1.6)
text(160,0.005,"23.75％",cex=1.6)
curve(pnorm(x,170,7.0),145,195,ylab='probability',xlab='分布関数',cex.lab=1.5,cex.axis=1.5)
lines(c(165,165,175,175),c(0,pnorm(165,170,7.0),pnorm(165,170,7.0),pnorm(175,170,7.0)),lty=2)
text(180,0.5,"52.5％",cex=1.6)
text(170,0.1,"23.75％",cex=1.6)
par(mfrow=c(1,1))
#dev.copy2eps(file="z02normal.eps",family='Japan1')



#ベータ分布の平均分散から母数を求める関数
betapq<-function(m,v){ r<-((m*(1-m))/v)-1
   p<-m*r;   q<-(1-m)*r
   return(c(p,q))}

#平均0.8、分散0.016のベータ分布の母数
betapq(0.8,0.016)


#図2.6　ベータ分布の確率密度関数
par(mfrow=c(1,2))
beta01<-function(x){dbeta(x,7.2,1.8)}
norm01<-function(x){dnorm(x,0.8,sqrt(0.016))}
curve(beta01,0,1.2,ylab='',xlab='',cex.lab=1.5,cex.axis=1.5)
curve(norm01,0,1.2,lty=2,ylab='probability density',xlab='',cex.lab=1.5,cex.axis=1.5,add=T)

curve(dbeta(x,betapq(0.5,0.05^2)[1],betapq(0.5,0.05^2)[2]),0,1,ylab='',xlab='',cex.lab=1.5,cex.axis=1.5)
curve(dbeta(x,betapq(0.5,0.10^2)[1],betapq(0.5,0.10^2)[2]),0,1,ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,add=T)
curve(dbeta(x,betapq(0.5,0.15^2)[1],betapq(0.5,0.15^2)[2]),0,1,ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,add=T)
curve(dbeta(x,betapq(0.5,0.20^2)[1],betapq(0.5,0.20^2)[2]),0,1,ylab='probability density',xlab='m=0.5',cex.lab=1.5,cex.axis=1.5,add=T)
#aaa<-locator(4)
#text(aaa$x,aaa$y,c('sd=0.05','sd=0.1','sd=0.15','sd=0.2'),cex=1.6)
par(mfrow=c(1,1))
#dev.copy2eps(file="z02beta.eps",family='Japan1')


#図2.7　2項分布の尤度
fbi<-function(x){dbinom(4,10,x)}
intbi<-as.numeric(integrate(fbi,0,1)[[1]])
fbi2<-function(x){dbinom(4,10,x)/intbi}
par(mfrow=c(1,2))
curve(fbi2,0,1,ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,ylim=c(0,2.8))
curve(dbinom(4,10,x),0,1,ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,ylim=c(0,2.8),add=T)
segments(-0.02,0.25,0.4,0.25,lty=2)
segments(0.4,-0.1,0.4,fbi2(0.4),lty=2)
#aaa<-locator(2)
#text(aaa$x,aaa$y,c('尤度関数を面積 1 に膨らませた関数','尤度関数'),cex=1.6)

curve(log(fbi(x)),0,1,ylab='',xlab='',cex.lab=1.5,cex.axis=1.5)
segments(0.4,-24,0.4,log(fbi(0.4)),lty=2)
text(0.7,-10,c('対数尤度関数'),cex=1.6)
par(mfrow=c(1,1))
#dev.copy2eps(file="z02likebi.eps",family='Japan1')



#図2.8　正規分布の尤度
set.seed(1234)
n<-10
x<-round(rnorm(n,170,7.0))
(m<-mean(x))
(v<-sum((x-m)^2)/n)
mu<-seq(165,170,0.2)
s2<-seq(30,60,along=mu)
likenorm<-function(mu,s2){
   wa2<-0
   for (i in 1:n){wa2<-wa2+(x[i]-mu)^2}
   ((-1)*n/2)*log(2*pi) + ((-1)*n/2)*log(2*s2) + ((-1)/(2*s2))*wa2
}
likelihood<-outer(mu,s2,likenorm)

par(mfrow=c(1,2))
persp(mu,s2,likelihood,theta=30,phi=30,expand=0.7,col=gray(0.9),cex=1.5)
contour(mu,s2,likelihood,nlevels = 15,cex.axis=1.5)
text(m,v,"+",cex=2.0)
par(mfrow=c(1,1))
#dev.copy2eps(file="z02likenorm.eps",family='Japan1')


#####################################
#ベイズ統計学  第3章  Rスクリプト
#####################################

#正選手問題の事後期待値・事後確率最大値・事後中央値
(eap<-10.2/(10.2+5.8))
(map<-(10.2-1)/(10.2+5.8-2))
(med<-qbeta(0.5,10.2,5.8))

#正選手問題の事後分散・事後標準偏差
(10.2*5.8)/((10.2+5.8)^(2)*(10.2+5.8+1))
sqrt((10.2*5.8)/((10.2+5.8)^(2)*(10.2+5.8+1)))

#正選手問題の信頼区間・確信区間
(b1<-qbeta(0.025,7.2,1.8))
(u1<-qbeta(0.975,7.2,1.8))
(b2<-qbeta(0.025,10.2,5.8))
(u2<-qbeta(0.975,10.2,5.8))

#図3.1　事前分布と事後分布、事後分布の各種統計量
par(mfrow=c(1,2))
curve(dbeta(x,7.2,1.8),0,1,ylab='probability density',xlab='',cex.lab=1.5,cex.axis=1.5,lty=2)
curve(dbeta(x,10.2,5.8),0,1,ylab='probability density',xlab='',cex.lab=1.5,cex.axis=1.5,lty=1,add=T)
text(0.9,3.0,"事前分布",cex=2.0)
text(0.4,1.5,"事後分布",cex=2.0)
curve(dbeta(x,10.2,5.8),0,1,ylab='probability density',xlab='',cex.lab=1.5,cex.axis=1.5,lty=1)
segments(eap,0,eap,dbeta(eap,10.2,5.8),lty=1)
segments(map,0,map,dbeta(map,10.2,5.8),lty=1)
segments(med,0,med,dbeta(med,10.2,5.8),lty=1)
text(0.32,1.0,"左から EAP, MED, MAP",cex=2.0)
par(mfrow=c(1,1))
#dev.copy2eps(file="z03beta01.eps",family='Japan1')

#図3.2　信頼区間・確信区間
par(mfrow=c(1,2))
curve(dbeta(x,7.2,1.8),0,1,ylab='',xlab='推定量',cex.lab=2.5,cex.axis=1.5,lty=1)
segments(b1,0,b1,dbeta(b1,7.2,1.8),lty=1)
segments(u1,0,u1,dbeta(u1,7.2,1.8),lty=1)
segments(b1,0,u1,0,lty=1)
text(0.57,0.2,"0.502",cex=1.5)
text(0.90,0.2,"0.976",cex=1.5)
text(0.3,2.0,"95％信頼区間",cex=2.5)
curve(dbeta(x,10.2,5.8),0,1,ylab='',xlab='母数',cex.lab=2.5,cex.axis=1.5,lty=1)
segments(b2,0,b2,dbeta(b2,10.2,5.8),lty=1)
segments(u2,0,u2,dbeta(u2,10.2,5.8),lty=1)
segments(b2,0,u2,0,lty=1)
text(0.47,0.2,"0.396",cex=1.5)
text(0.77,0.2,"0.846",cex=1.5)
text(0.3,1.95,"95％確信区間",cex=2.5)
par(mfrow=c(1,1))
#dev.copy2eps(file="z03beta02.eps",family='Japan1')

#図3.3　局所一様事前分布
curve(dbeta(x,0.5,0.5),0,1,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lty=1)
text(0.5,2.0,"p=0.5, q=0.5",cex=2.0)
#dev.copy2eps(file="z03beta03.eps",family='Japan1')


#ポアソン分布
#次の10秒間にだれも提出しない確率
dpois(0,0.8)
#次の10秒間に3人以上提出する確率
1-ppois(2,0.8)

#図3.4　ポアソン分布
x<-0:5
y<-dpois(x,0.8)
names(y)<-x
z<-0:10
par(mfrow=c(1,2))
barplot(y,ylab='probability',xlab='λ=0.8 ',cex.lab=1.5,cex.axis=1.7)
plot(z,dpois(z,1.0),ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,type="b",ylim=c(0,0.4));par(new=T)
plot(z,dpois(z,2.0),ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,type="b",ylim=c(0,0.4));par(new=T)
plot(z,dpois(z,3.0),ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,type="b",ylim=c(0,0.4));par(new=T)
plot(z,dpois(z,4.0),ylab='',xlab='',cex.lab=1.5,cex.axis=1.5,type="b",ylim=c(0,0.4));par(new=T)
plot(z,dpois(z,5.0),ylab='probability',xlab='',cex.lab=1.5,cex.axis=1.5,type="b",ylim=c(0,0.4))
#aaa<-locator(5)
#text(aaa$x,aaa$y,c('λ=1.0','λ=2.0','λ=3.0','λ=4.0','λ=5.0'),cex=1.6)
par(mfrow=c(1,1))
#dev.copy2eps(file="z03pois04.eps",family='Japan1')


#指数分布による平均提出時間・分散・30秒間提出のない時間
(1/0.8)*10
sqrt(1/0.8^2)*10
1-pexp(3,0.8)
exp(-2.4) 


#図3.5　指数分布
par(mfrow=c(1,2))
curve(dexp(x,1.0),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lty=1,xlim=c(-0.7,6))
curve(dexp(x,1/2),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lty=1,add=T)
curve(dexp(x,1/3),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lty=1,add=T)
curve(dexp(x,1/4),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lty=1,add=T)
curve(dexp(x,1/5),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lty=1,add=T)
#aaa<-locator(5)
#text(aaa$x,aaa$y,c('λ=1  ','λ=1/2','λ=1/3','λ=1/4','λ=1/5'),cex=1.6)
curve(pexp(x,1.0),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lwd=1.1,xlim=c(0,6))
curve(pexp(x,1/2),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lwd=1.1,add=T)
curve(pexp(x,1/3),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lwd=1.1,add=T)
curve(pexp(x,1/4),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lwd=1.1,add=T)
curve(pexp(x,1/5),0,6,ylab='',xlab='',cex.lab=2.5,cex.axis=1.5,lwd=1.1,add=T)
#aaa<-locator(5)
#text(aaa$x,aaa$y,c('λ=1  ','λ=1/2','λ=1/3','λ=1/4','λ=1/5'),cex=1.6)
par(mfrow=c(1,1))
#dev.copy2eps(file="z03exp05.eps",family='Japan1')

#ガンマ分布問題正解・平均値・中央値・95％
(50/0.8)*10
round(qgamma(0.5,50,0.8)*10)
round(qgamma(0.95,50,0.8)*10)

#図3.6　ガンマ分布gamma(x,α,λ)
par(mfrow=c(1,2))
curve(dgamma(x,2.0,8.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=F)
curve(dgamma(x,2.0,4.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=T)
curve(dgamma(x,2.0,2.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=2.0,add=T)
curve(dgamma(x,2.0,1.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=T)
curve(dgamma(x,2.0,0.5),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=T)
#aaa<-locator(1)
#text(aaa$x,aaa$y,c('α=2.0'),cex=2.5)
#aaa<-locator(5)
#text(aaa$x,aaa$y,c('λ=8.0','λ=4.0','λ=2.0','λ=1.0','λ=0.5'),cex=1.6)
curve(dgamma(x,0.5,2.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=F)
curve(dgamma(x,1.0,2.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=T)
curve(dgamma(x,2.0,2.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=2.0,add=T)
curve(dgamma(x,4.0,2.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=T)
curve(dgamma(x,8.0,2.0),0,6,ylab='',xlab='',cex.axis=1.5,lwd=1.0,add=T)
#aaa<-locator(1)
#text(aaa$x,aaa$y,c('λ=2.0'),cex=2.5)
#aaa<-locator(5)
#text(aaa$x,aaa$y,c('α=0.5','α=1.0','α=2.0','α=4.0','α=8.0'),cex=1.6)
par(mfrow=c(1,1))
#dev.copy2eps(file="z03gamma06.eps",family='Japan1')

#ポアソン最尤推定値
mean(c(0,1,0,0,2,0,1,0,0,1))

#図3.7　ガンマ分布gamma(x,α,λ)
par(mfrow=c(1,1))
curve(dgamma(x,11,13),0,5,ylab='',xlab='θ',cex.axis=1.5,cex.lab=2.0,lwd=1.0,add=F)
curve(dgamma(x, 6, 3),0,5,ylab='',xlab='θ',cex.axis=1.5,cex.lab=2.0,lwd=1.0,add=T)
segments(2,0,2,dgamma(2, 6, 3),lty=2)
s<-0.864
segments(s,0,s,dgamma(s,11,13),lty=2)
text(3.0,0.5,'事前分布',cex=2.0)
text(1.7,1.25,'事後分布',cex=2.0)
text(1.2,0.0,'0.864',cex=1.2)
par(mfrow=c(1,1))
#dev.copy2eps(file="z03gamma07.eps",family='Japan1')


#「波平釣果問題」正解、夕食を用意する確率
round(dpois(0,0.5),3)
round(dpois(0,0.864),3)

#「3囚人問題」の事後分布
curve(1/(x-1)^2,0,0.5,ylab='probability density',xlab='',
cex.lab=1.5,cex.axis=1.5,lty=1,ylim=c(0,4.0))
segments(0.5,0,0.5,1/(0.5-1)^2)
segments(0,0,0,1)
segments(0,0,0.5,0)
#dev.copy2eps(file="z03syujin08.eps",family='Japan1')



#####################################
#ベイズ統計学  第4章  Rスクリプト
#####################################

#表4.2　遷移核
T<-10
P<-matrix(c(0.3,0.3,0.4,0.1,0.5,0.4,0.2,0.6,0.2),3,3,byrow=T)
rownames(P)<-c("紋","縞","玉")
colnames(P)<-c("紋","縞","玉")
print(P)

F<-matrix(0,T,3)
f01<-matrix(c(0.6,0.25,0.15),1,3)
f<-F[1,]<-f01; for(i in 2:T){ f<-F[i,]<-f %*% P };round(F,4)


#図4.1
plot(F[,1],ylim=c(0,1),type="b",xlab="",ylab="",cex.axis=1.5,cex.lab=1.5,lwd=2);par(new=T)
plot(F[,2],ylim=c(0,1),type="b",xlab="",ylab="",cex.axis=1.5,cex.lab=1.5,lwd=2);par(new=T)
plot(F[,3],ylim=c(0,1),type="b",xlab="t",ylab="probability",cex.axis=1.5,cex.lab=1.5,lwd=2)
text(8.5,0.55,"縞",cex=2.0)
text(8.5,0.38,"玉",cex=2.0)
text(8.5,0.23,"紋",cex=2.0)
text(1.0,0.05,"入学式",cex=2.0)
text(4.78,0.80,"バーンイン（焼き入れ）期間　←",cex=2.0)
segments(6.5,0.0,6.5,1.0,lty=2)
#dev.copy2eps(file="z04tie01.eps",family='Japan1')


#表4.3
T<-8;F<-matrix(0,T,3)
f01<-matrix(c(0.6,0.25,0.15),1,3)
f<-F[1,]<-f01; for(i in 2:T){ f<-F[i,]<-f %*% P };round(F,4)
f01<-matrix(c(0.3,0.3,0.4),1,3)
f<-F[1,]<-f01; for(i in 2:T){ f<-F[i,]<-f %*% P };round(F,4)
f01<-matrix(c(0.1,0.1,0.8),1,3)
f<-F[1,]<-f01; for(i in 2:T){ f<-F[i,]<-f %*% P };round(F,4)


#図4.2
curve(dgamma(x, 6, 3),0,5,ylab='',xlab='',cex.axis=1.5,cex.lab=2.0,lwd=2.5)
segments(2,0.02,2,dgamma(2, 6, 3),lty=2)
segments(3.5,0.02,3.5,dgamma(3.5, 6, 3),lty=2)
text(1.75,0.20,"f(θ)",cex=2.0)
text(3.73,0.03,"f(θ')",cex=2.0)
add<-0.1
arrows(3.25,0.08,2.2,0.38,lty=1,lwd=1.8)
arrows(2.2,0.38+add,3.25,0.08+add,lty=1,lwd=1.8)
text(2.75,0.10,"f(θ|θ')",cex=2.0)
text(2.70,0.45,"f(θ'|θ)",cex=2.0)
text(2.0,0,"θ",cex=2.0)
text(3.5,0,"θ'",cex=2.0)
#dev.copy2eps(file="z04tie02.eps",family='Japan1')

rm(list=ls())
#「波平釣果問題」の独立MH法による解
set.seed(1234)     #乱数の種
qme <- 1.0         #提案分布の期待値
qsd <- sqrt(0.5)   #提案分布のＳＤ
Nsi <- 10^5        #サンプルサイズ
Bin <- 10^3        #バーンイン期間
x<-numeric(Nsi)
x[1]<-rnorm(1, mean=qme, sd=qsd)
co<-0
for (t in 2:Nsi){
    a<-rnorm(1, mean=qme, sd=qsd)
    if (runif(1) < (dnorm(x[t-1], mean=qme, sd=qsd)*dgamma(a,shape=11,rate=13))/
                   (dnorm(a, mean=qme, sd=qsd)*dgamma(x[t-1],shape=11,rate=13))) {
       x[t]<-a; co<-co+1
    }else{
       x[t]<-x[t-1]
    }
}
cat("採択率")
print(round(co/Nsi,2))
round(mean(x[(Bin+1):Nsi]),3)
round(var( x[(Bin+1):Nsi]),3)
round(sd(  x[(Bin+1):Nsi]),3)
xxx<-x


#「正選手問題」の独立MH法による解
set.seed(1234)     #乱数の種
Nsi <- 10^5        #サンプルサイズ
Bin <- 10^3        #バーンイン期間
x<-numeric(Nsi)
x[1]<-runif(1)
co<-0
for (t in 2:Nsi){
    a<-runif(1)
    if (runif(1) < dbeta(a,     10.2,5.8)/
                   dbeta(x[t-1],10.2,5.8)　) {
       x[t]<-a; co<-co+1
    }else{
       x[t]<-x[t-1]
    }
}
cat("採択率")
print(round(co/Nsi,2))
round(mean(x[(Bin+1):Nsi]),3)
round(var( x[(Bin+1):Nsi]),3)
round(sd(  x[(Bin+1):Nsi]),3)

#図4.3左 「波平釣果問題」の事後分布と理論分布との重ね合わせ
par(mfrow=c(1,2))
hist(xxx, breaks =50, xlab='', xlim=c(0,2.5), ylim=c(0,2.0), freq=F, main="", cex.axis=1.5)
par(new=T)
curve(dgamma(x,shape=11,rate=13),0,2.5,ylab='',xlab='',xlim=c(0,2.5),ylim=c(0,2.0),lwd=2.0,cex.axis=1.5)
#図4.3右　「正選手問題」の事後分布と理論分布との重ね合わせ
hist(x,   breaks =50,xlab='',xlim=c(0,1),ylim=c(0,4.0),freq=F,main="",cex.axis=1.5)
par(new=T)
curve(dbeta(x,10.2,5.8),0,1,ylab='',xlab='',xlim=c(0,1),ylim=c(0,4.0),lwd=2.0,cex.axis=1.5)
par(mfrow=c(1,1))
#dev.copy2eps(file="z04hist03.eps",family='Japan1')

#図4.4 トレースライン
par(mfrow=c(2,1))
plot(xxx,type="l",ylab='',xlab='',lwd=0.5,cex.axis=1.5)
plot(x,type="l",ylab='',xlab='',lwd=0.5,cex.axis=1.5,ylim=c(0,1))
par(mfrow=c(1,1))
#dev.copy2eps(file="z04tra04.eps",family='Japan1')


#図4.5　目標分布と提案分布の関係
curve(dgamma(x,11,13),-1,4,ylab='',xlab='',cex.axis=1.5,lwd=2.0,add=F)
curve(dnorm(x,1.0,sqrt(0.5)),-1,4,ylab='',xlab='',cex.axis=1.5,lwd=2.0,add=T)
curve(dnorm(x,3.0,sqrt(0.5)),-1,4,ylab='',xlab='',cex.axis=1.5,lwd=1.5,lty=2,add=T)
curve(dnorm(x,1.0,sqrt(2.0)),-1,4,ylab='',xlab='',cex.axis=1.5,lwd=1.5,lty=2,add=T)
curve(dnorm(x,1.0,sqrt(0.01)),-1,4,ylab='',xlab='',cex.axis=1.5,lwd=1.5,lty=2,add=T)
text(0.25,1.5,'G(11,13)',cex=1.2)
text(-0.1,0.4,'N(1,0.5)',cex=1.2)
text(3,0.2,'A',cex=1.2)
text(1.4,1.0,'B',cex=1.2)
text(3,0.65,'C',cex=1.2)
#dev.copy2eps(file="z04pro05.eps",family='Japan1')


#「波平釣果問題」のランダムウォークMH法による解
set.seed(1234)     #乱数の種
qsd <- sqrt(0.1)   #提案分布のＳＤ
Nsi <- 10^6+100    #サンプルサイズ
Bin <- 10^2        #バーンイン期間
x<-numeric(Nsi)
x[1]<-4.0
co<-0
for (t in 2:Nsi){
    a<-rnorm(1, x[t-1], sd=qsd)
    if (runif(1) < dgamma(a,shape=11,rate=13)/
                   dgamma(x[t-1],shape=11,rate=13)) {
       x[t]<-a; co<-co+1
    }else{
       x[t]<-x[t-1]
    }
}
cat("採択率")
print(round(co/Nsi,2))

#図4.6　トレースライン
par(mfrow=c(2,1))
plot(x[1:100],type="l",ylab='',xlab='',lwd=1,ylim=c(0,4.0),cex.axis=2.0)
plot(x,type="l",ylab='',xlab='',lwd=0.5,ylim=c(0,4.0),cex.axis=2.0)
par(mfrow=c(1,1))
#dev.copy2eps(file="z04tra06.eps",family='Japan1')


#MAP推定値　先頭に来るものが経験的なMAP
rev(sort(table(round(x[(Bin+1):Nsi],2))))[1:5]

#確信区間  
lx<-length(x[(Bin+1):Nsi]); sx<-sort(x[(Bin+1):Nsi])
round(sx[lx%/%40],3); round(sx[lx-lx%/%40],3)

#EAP推定値、MED推定値
round(mean(x[(Bin+1):Nsi]),3)
round(median(x[(Bin+1):Nsi]),3)

#事後標準偏差
round(sd(x[(Bin+1):Nsi]),3)

#事後標準偏差をEAPの両側につけ、上側値と下側値を計算、下図で利用
mean(x[(Bin+1):Nsi])+sd(x[(Bin+1):Nsi])
mean(x[(Bin+1):Nsi])-sd(x[(Bin+1):Nsi])


#下側上側限界
qgamma(0.025,shape=11,rate=13)
qgamma(0.975,shape=11,rate=13)
qgamma(0.5,shape=11,rate=13)

#モード
(11-1)/13

#図4.7
hist(x, breaks =250,xlab='',xlim=c(0,2.5),ylim=c(0,2.0),freq=F,main="",cex.axis=2.0)
arrows(0.422,0.8,0.422,0.45,lwd=2);text(0.25 , 0.86,'95%確信区間下限',cex=2)
arrows(1.414,0.6,1.414,0.25,lwd=2);text(1.414, 0.66,'95%確信区間上限',cex=2)
arrows(0.765,1.9,0.765,1.65,lwd=2);  text(0.765 , 2.0,'MAP推定値',cex=2)
arrows(0.9,1.9,0.8209,1.6,lwd=2);  text(1.1,1.95 ,'MED推定値',cex=2)
arrows(0.95,1.65,0.8465,1.55,lwd=2); text(1.1,1.755 ,'EAP推定値',cex=2)
arrows(0.8465,0.5,1.10154,0.5,lwd=2,code=3)
arrows(0.5913,0.5,0.8465,0.5,lwd=2,code=3);text(0.8465,0.65,'事後標準偏差',cex=2.5)
#dev.copy2eps(file="z04hist07.eps",family='Japan1')


#母数の関数の平均・標準偏差・95％確信区間を求める関数
#theta  : 母数の列（バーンイン期間を含む）
#ff     : 関数
#pre    : 接頭語
#b      : バーンイン
#yu     : 有効数字
genqua<- function(theta,ff,pre,b,yu=3) {
   lx <- length(theta)
   sx <- sort(ff(theta[(b+1):lx]))
   yx <- lx-b
   xxx <- matrix(c(round(mean(sx),yu),round(sd(sx),yu),round(sx[yx%/%40],yu),round(sx[yx-yx%/%40],yu)),1,4)
   colnames(xxx) <- c("mean","SD","95b","95u")
   rownames(xxx) <- pre
   return(xxx)
}

f1<-function(x){x}
f2<-function(x){sqrt(x)}
f3<-function(x){1/sqrt(x)}
f4<-function(x){3+(1/x)}
fB<-function(x){dpois(0,x)}

#標準偏差・歪度・尖度の推測
genqua(theta=x,ff=f1,pre='ラムダ',b=100)
genqua(theta=x,ff=f2,pre='SD',b=100)
genqua(theta=x,ff=f3,pre='歪度',b=100)
genqua(theta=x,ff=f4,pre='尖度',b=100)

#事後予測分布によるボウズの確率の推定
genqua(theta=x,ff=fB,pre='ボウズ',b=100)


#条件付き予測分布によるボウズの確率の推定
round(dpois(0,mean(x[(Bin+1):Nsi])),3)
#round(dpois(0,sx[lx%/%40]),3); #事後予測分布と同じ
#round(dpois(0,sx[lx-lx%/%40]),3); #事後予測分布と同じ


#####################################
#ベイズ統計学  第5章  Rスクリプト
#####################################
rm(list=ls())
#図5.1は#図5.5と一緒に描く

#図5.2
plot(1:11,axes=F,type="n",xlab="",ylab="")
text(3, 11,"位置",cex=2); text(3,  6,"速度",cex=2); text(3,  1,"加速度",cex=2)
text(9,  6,"運動量",cex=2); text(9,  1,"力",cex=2); text(2,8.5,"(微分)",cex=2)
text(2,3.5,"(微分)",cex=2); text(10,3.5,"(微分)",cex=2); text(6,  7,"(×質量)",cex=2)
text(6,  2,"(×質量)",cex=2)
arrows(3,10,3,7,lwd=2.0); arrows(3, 5,3,2,lwd=2.0); arrows(9, 5,9,2,lwd=2.0)
arrows(4, 6,7.5,6,lwd=2.0); arrows(4.5, 1,8,1,lwd=2.0)
#dev.copy2eps(file="z05buturi03.eps",family='Japan1')

#図5.3
x<-c(0,6,6,0);y<-c(0,3,0,0)
plot(1:10,type="n",ylim=c(0,5),xlim=c(0,10),ylab="速度",xlab="時間")
text(7.7,2,"速度 = 加速度 × 時間"); text(2,3.5,"面積 = (1/2) × 時間 × 速度")
polygon(x,y,col=8); arrows(0,0,8,4)
#dev.copy2eps(file="z05idou02.eps",family='Japan1')

#図5.4
plot(1:9,axes=F,type="n",xlab="",ylab="",xlim=c(0,8),ylim=c(1,8))
text( 1, 7,"位置",cex=2); text( 1, 3,"運動量",cex=2); text( 1, 1,"時間",cex=2)
text(2.5, 3.5,"(1)",cex=2); text(3, 7.5,"(2)",cex=2); text(3.5, 3.5,"(3)",cex=2)
text(4.5, 3.5,"(4)",cex=2); text(5, 7.5,"(5)",cex=2); text(5.5, 3.5,"(6)",cex=2)
text(6.5, 3.5,"(7)",cex=2); text(2,1,"0.0",cex=2); text(3,1,"0.5",cex=2)
text(4,1,"1.0",cex=2); text(5,1,"1.5",cex=2); text(6,1,"2.0",cex=2)
text(7,1,"2.5",cex=2); arrows(2,7,3.9,7,lwd=2.0)
arrows(4.1,7,5.9,7,lwd=2.0); arrows(2,3,2.9,3,lwd=2.0); arrows(3.1,3,4,3,lwd=2.0)
arrows(4,3,4.9,3,lwd=2.0); arrows(5.1,3,6.0,3,lwd=2.0); arrows(6.0,3,6.9,3,lwd=2.0)
segments(2,7,3,3,lty=2); segments(3,3,4,7,lty=2); segments(4,7,5,3,lty=2)
segments(5,3,6,7,lty=2); segments(6,7,7,3,lty=2)
#dev.copy2eps(file="z05leap04.eps",family='Japan1')


#表5.1、図5.1、図5.2のために
#リープフロッグ(運動量、位置、高さ、ハミルトニアン)
hmc01 <- function(ini,p,cc=15,e=0.05,E=loggamma,D=Dloggamma){
   leapfrog <- function(p,z2,e){
       p2 <- p  - e * D(z2) / 2
       z2 <- z2 + e * p2
       p2 <- p2 - e * D(z2) / 2
       list(p2=p2,z2=z2)
   }
z<- matrix(0,cc,4) 
z[1,1] <- p 
z[1,2] <- z2<- ini
z[1,3] <- E(z[1,2])
z[1,4] <- H <- (p^2)/2 + z[1,3] 
for(j in 2:cc) { 
  pz<-leapfrog(p,z2,e) 
  z[j,1] <- p <- pz$p2
  z[j,2] <- z2<- pz$z2
  z[j,3] <- E(z[j,2])
  z[j,4] <- H <- (p^2)/2 + z[j,3] 
  } 
return(z)
}
#「波平釣果問題」ガンマ分布モデル
alpha <- 11; lambda <- 13; 
#対数尤度関数のマイナス
loggamma<-function(theta){lambda*theta-(alpha-1)*log(theta)}
#対数尤度関数の微分のマイナス
Dloggamma<-function(theta){lambda-(alpha-1)/theta}
#リープフロッグの実行
z01<-hmc01(ini=0.1,p=0, cc=15, e=0.05)
z02<-hmc01(ini=2.4343660,p=-1.143050, cc=15, e=0.05)
library(xtable)
xtable(z01, digits = 2)
xtable(z02, digits = 2)


#図5.1
z<-z01
curve(loggamma(x),0.1,2.5,ylab='(m)',xlab='位置(100 m)',
            cex.axis=1.5,cex.lab=1.5,lwd=2.5)
text(0.6,24.0,"出発点では速度と運動量は0",cex=2)
for (i in 1:15){points(z[i,2],z[i,3],pch=16,cex=2)}
for (i in 2:15){arrows(z[i,2],z[i,3],z[i,2]+z[i,1]/25,z[i,3],cex=1)}
#dev.copy2eps(file="z05jet01.eps",family='Japan1')

#図5.5
z<-z02
curve(loggamma(x),0.1,2.5,ylab='(m)',xlab='位置(100 m)',
            cex.axis=1.5,cex.lab=1.5,lwd=2.5)
text(0.4,24.0,"速度と運動量は0",cex=2)
for (i in 1:15){points(z[i,2],z[i,3],pch=16,cex=2)}
for (i in 1:14){arrows(z[i,2],z[i,3],z[i,2]+z[i,1]/25,z[i,3],cex=1)}
#dev.copy2eps(file="z05jet05.eps",family='Japan1')



#位相空間 ハミルトニアンの計算
Hami01<-function(p,theta){(p*p)/2 +loggamma(theta)}

#図5.6左
par(mfrow=c(1,2))
p<-seq(0,5,0.1)
theta<-seq(0.05,3.0,along=p)
Hamiltonian<-outer(p,theta,Hami01)
persp(p,theta,Hamiltonian,theta=-120,phi=30,expand=0.7,col=gray(0.9),cex=1.5,
ylab="θ")

#図5.6右
p<-seq(-5,5,0.1)
theta<-seq(0.05,3.0,along=p)
Hamiltonian<-outer(p,theta,Hami01)
contour(p,theta,Hamiltonian,nlevels = 15,cex.axis=1.5,xlab="p",ylab="θ",cex.lab=1.5)
z01<-hmc01(ini=0.1,p=0, cc=168, e=0.01)
for (i in seq(1,nrow(z01),by=3)){points(z01[i,1],z01[i,2],pch=16)}
par(mfrow=c(1,1))
#dev.copy2eps(file="z05isou06.eps",family='Japan1')


#表5.2
rownames(z01)<-1:nrow(z01)
z03<-z01[seq(1,nrow(z01),by=10),]
xtable(z03, digits = 2)

#ハミルトニアンモンテカルロ
# N=スカラー、サンプリングする乱数の数
# ini=母数ベクトルの初期値
# E=関数、対数尤度関数のマイナス（母数ベクトルを入力、スカラーを返す）
# D=関数、対数尤度関数の微分のマイナス（母数ベクトルを入力、ベクトルを返す）
# L=スカラー、遷移内の移動時間
# epsi=リープフロッグ法の精度
hmc <- function(N,ini,E,D,L=100,epsi=0.01){
   leapfrog <- function(r,z2,e){
       r2 <- r  - e * D(z2) / 2
       z2 <- z2 + e * r2
       r2 <- r2 - e * D(z2) / 2
       list(r2=r2,z2=z2)
   }
   p<-length(ini)
   z<- matrix(0,N,p); rr<- matrix(0,N,p);
   z[1,] <- ini
   co<- 1; #最初は採択
   for(i in 2:N) { 
      r <- rr[i-1,]<-rnorm(p)
      H <- sum(r^2)/2 + E(z[i-1,]) 
      #e <- sample(c(-1,1), 1) * runif(1, 0.9, 1.1) * epsi 
      e <- epsi 
      z2 <- z[i-1,] 
      #LL <- sample(L:(L*2),1) 
      LL <- L 
      for(j in 1:LL) { 
         rz<-leapfrog(r,z2,e) 
         r <- rz$r2
         z2<- rz$z2
      } 
      dH <- H - (sum(r^2)/2 + E(z2)); 
      if (runif(1) < exp(dH)) {
         z[i,]<-z2; co<-co+1
         }else{
         z[i,]<-z[i-1,]
      }
   } 
   ac<-co/N
   return(list(z=z,rr=rr,ac=ac))
}

#ガンマ分布モデルのための関数の設定
alpha <- 11; 
lambda <- 13; 
#対数尤度関数のマイナス
loggamma<-function(theta){lambda*theta-(alpha-1)*log(theta)}
#対数尤度関数の微分のマイナス
Dloggamma<-function(theta){lambda-(alpha-1)/theta}


#表5.3
set.seed(1234)
fit<-hmc(N=100,ini=c(2.5),E=loggamma,D=Dloggamma)
mean(fit$z[2:100])
mean(fit$rr[2:100])
print(fit$ac)
fit$z[1:3]
fit$rr[1:3]

#図5.7左　フリーハンドで書いている部分があるので再現は難しい
par(mfrow=c(1,2))
p<-seq(-5,5,0.1);theta<-seq(0.05,3.0,along=p)
Hamiltonian<-outer(p,theta,Hami01)
contour(p,theta,Hamiltonian,nlevels = 15,cex.axis=1.5,xlab="p",ylab="θ",cex.lab=1.5,xlim=c(-5,5),ylim=c(0.05,3))
par(new=T)
#plot(fit$rr[1:3],fit$z[1:3],type="p",xlim=c(-5,5),ylim=c(0.05,3),xlab="",ylab="",cex.axis=1.5,cex.lab=1.5)
points(fit$rr[1:3],fit$z[1:3],pch=16,cex=2)
points(-4.44,fit$z[2],pch=15,cex=2)
points(-1.5,fit$z[3],pch=15,cex=2)
text(fit$rr[1]+0.1,fit$z[1]+0.1, "t=1",cex=1.5)
text(fit$rr[2]+0.1,fit$z[2]+0.1,"t=2",cex=1.5)
text(fit$rr[3]+0.1,fit$z[3]+0.1,"t=3",cex=1.5)
#xy<-locator(20)
#for (i in 1:19){segments(xy$x[i],xy$y[i],xy$x[i+1],xy$y[i+1],lwd=2.5)}
#xyxy<-locator(1)
#arrows(xy$x[20],xy$y[20],xyxy$x[1],xyxy$y[1],lwd=2.5)

#図5.7右
p<-seq(-5,5,0.1);theta<-seq(0.05,3.0,along=p)
Hamiltonian<-outer(p,theta,Hami01)
contour(p,theta,Hamiltonian,nlevels = 15,cex.axis=1.5,xlab="p",ylab="θ",cex.lab=1.5,xlim=c(-5,5),ylim=c(0.05,3))
par(new=T)
plot(fit$rr,fit$z,type="b",xlim=c(-5,5),ylim=c(0.05,3),xlab="",ylab="",cex.axis=1.5,cex.lab=1.5)
par(mfrow=c(1,1))
#dev.copy2eps(file="z05isou07.eps",family='Japan1')


#図5.8 ガンマ分布　トレースライン
fit<-hmc(N=1000,ini=c(3.0),E=loggamma,D=Dloggamma)
plot(fit$z,type="l",xlab="t",ylab="θ",cex.axis=1.5,cex.lab=1.5)
#dev.copy2eps(file="z05tore08.eps",family='Japan1')
fit$ac; #採用率



#正規分布モデル
#データの用意
set.seed(1234)
n<-100
x<-round(rnorm(n,170,7.0))
(m<-mean(x))
(v<-sum((x-m)^2)/n)
x
n<-length(x)

#対数尤度関数のマイナス
lognorm<-function(theta){   mu<-theta[1];   sigma2<-theta[2];
   return(((n*log(sigma2)/(-2))-(sum((x-mu)^2)/(2*sigma2)))*(-1))}
#対数尤度関数の微分のマイナス
dmu    <-function(theta){   mu<-theta[1];sigma2<-theta[2];
   return(sum(x-mu)/sigma2)}
dsigma2<-function(theta){   mu<-theta[1];sigma2<-theta[2];
   return((-1*n)/(2*sigma2) + sum((x-mu)^2)/(2*sigma2*sigma2))}
Dlognorm<-function(theta){
   return(c(dmu(theta),dsigma2(theta))*(-1))}

#ハミルトニアンサンプリング（今までの計算と比べると時間がかかります）
fit<-hmc(N=100000,ini=c(168,49),E=lognorm,D=Dlognorm,L=100,epsi=0.01)
z<-fit$z
r<-fit$rr
print(fit$ac); #採用率
zzz<-c(1001:100000)

#MAP推定値　先頭に来るものが経験的なMAP
rev(sort(table(round(z[zzz,1],1))))[1:5]
rev(sort(table(round(z[zzz,2],1))))[1:5]
hist(z[zzz,2],breaks = 50,freq =T)

#EAP推定値、MED推定値
summary(z[zzz,1])
summary(z[zzz,2])

#事後標準偏差
sd(z[zzz,1])
sd(z[zzz,2])


#図5.9　正規分布モデルのトレースライン
par(mfrow=c(2,1))
plot(z[,1],type="l",cex.axis=1.5,xlab="t",ylab="μ",cex.lab=1.5)
plot(z[,2],type="l",cex.axis=1.5,xlab="t",ylab="σ2",cex.lab=1.5)
par(mfrow=c(1,1))
#dev.copy2eps(file="z05tore09.eps",family='Japan1')


#図5.10　平均と分散の10万個の同時分布
plot(z,type="p",cex.axis=1.5,xlab="μ",ylab="σ2",cex.lab=1.5)
#dev.copy2eps(file="z05norm10.eps",family='Japan1')



