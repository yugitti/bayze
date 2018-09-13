data { 
	int<lower=0> N;
	real<lower=0> x[N];
}
parameters {
	real<lower=0>   mu;
	real<lower=0>	sigma;
}
model {
	x ~ lognormal(mu, sigma);
}

generated quantities{
	real<lower=0>	zeta;
	real<lower=0>	p_450;
	zeta = exp(mu + 0.5244*sigma);
	p_450 = zeta<450 ? 1:0;
}

