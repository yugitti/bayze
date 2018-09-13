data { 
	int<lower=0>  N;    #記録数
	int<lower=0> Xn[N]; #波平さん記録
	int<lower=0> Xm[N]; #マスオさん記録
}
parameters {
	real<lower=0>                lambdaN;
	real<lower=0>                lambdaM;
}
model {
	Xn ~ poisson(lambdaN);
	Xm ~ poisson(lambdaM);

}
generated quantities{
	real	delta; #本文(7.3)式と同様に波平さんとマスオさんのlambdaの差をとる
	real    delta_p; #本文(7.4)式と同様の生成量
	delta = lambdaN - lambdaM;
	delta_p = step(delta);
}
