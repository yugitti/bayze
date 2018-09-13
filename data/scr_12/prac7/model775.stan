data { 
	int<lower=0> N;    #データ数
	real<lower=0> x[N]; #年収データ
}
parameters {
	real<lower=0>   mu;
	real<lower=0>	sigma;
}
model {
	x ~ lognormal(mu, sigma);
}

generated quantities{
	real<lower=0>	zeta; #対数正規分布の50パーセンタイル点
	real<lower=0>	p_500; #500万円が上位50%に入っている確率
	zeta = exp(mu);
	p_500 = zeta<500? 1: 0;
}

