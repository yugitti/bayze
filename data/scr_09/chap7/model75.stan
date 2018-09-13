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
	real	p;
	real	beta;
	int	pred;
	p <- 1*pow(theta, 5)*pow((1-theta),0)+5*pow(theta, 4)*pow((1-theta),1)+10*pow(theta, 3)*pow((1-theta),2)+10*pow(theta, 2)*pow((1-theta),3);
	beta <- theta/(1-theta);
	pred <- neg_binomial_rng(2, beta)+2;
}
