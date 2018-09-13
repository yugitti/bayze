//混合効果モデル
data{
	int<lower=0> S; #受験者（競技者）
	int<lower=0> R; #評定者
	vector<lower=0, upper=100>[R] Score[S]; #点数
}
transformed data{
	int<lower=0> Rm1; # betaの総和が0になるようにするための下準備
	Rm1 = R-1;
}
parameters{
	real mu;
	real alpha[S];
	real beta_m1[Rm1];
	real<lower=0> tauSubject; #競技者に関する標準偏差

	real<lower=0> tauWithin;
}
transformed parameters{
	real beta[R];
	real<lower=0> sig2subject;

	real<lower=0> sig2within;
	sig2subject = pow(tauSubject,2); #分散の定義

	sig2within = pow(tauWithin,2);
	# betaの総和が0になるようにするための細工
	for (r in 1:Rm1) {
		beta[r] = beta_m1[r];
	}
	beta[R] = -sum(beta_m1); #betaの最後の要素は発生させないで、R-1番目までの和の符号を逆転させたものを代入（なお実際にサンプリングするのは変数beta_m1についてであることに注意。）
}
model{
	real nu;
	mu ~ normal(0, 1000);
	for(s in 1:S){
		alpha[s] ~ normal(0, tauSubject);
	}
	for(s in 1:S) {
		for(r in 1:R) {
			nu = mu + alpha[s] + beta[r];
			Score[s,r] ~ normal(nu, tauWithin);
		}
	}
}
generated quantities{
	real<lower=0> ICC31;
	real<lower=0> ICC37;
	ICC31 = sig2subject / (sig2subject + sig2within); #Rのところで人数を5や6に変える
	ICC37 = sig2subject / (sig2subject + (sig2within/4)); 
}
