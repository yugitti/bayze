data { 
	int<lower=0> N; #八重桜の本数
	real<lower=0>  x[N]; #八重桜が見えるまでに進んだ距離
}
parameters {
	real<lower=0>                lambda;
}
model {
	x ~ exponential(lambda);
}
generated quantities{
	real<lower=0> p; #本文(7.7)式と同様の生成量eta
	real<lower=0> eta; #本文(7.8)式をx*=100とした，生成量p
	real P_underp; #生成量pが0.5以上になる確率
	eta = inv(lambda); 
	p = exponential_cdf(100,lambda);
	P_underp = p > 0.5?1:0;
}

