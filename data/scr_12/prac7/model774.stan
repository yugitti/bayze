data { 
	int<lower=0> N;
	int<lower=0> x[N];
}
parameters {
	real<lower=0, upper=1>   theta;
}
model {
	x ~ bernoulli(theta);
}
generated quantities{
	real	p; #4��3��ȏ㐬������m��
	real	beta;
	int	pred; #2�񐬌�����܂ł̑����s��
	p = 1*pow(theta, 4)*pow((1-theta),0)+4*pow(theta, 3)*pow((1-theta),1);
	beta = theta/(1-theta);
	pred = neg_binomial_rng(2, beta)+2;
}

