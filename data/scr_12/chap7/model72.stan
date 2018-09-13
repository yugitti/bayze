data { 
	int<lower=0> N;  
	real<lower=0>  x[N];    
}
parameters {
	real<lower=0>                lambda;
}
model {
	x ~ exponential(lambda);
}
generated quantities{
	real<lower=0> mu; 
	real<lower=0> p; 
	real P_underp;
	mu = inv(lambda); 
	p = exponential_cdf(5,lambda); 
	P_underp = p < 0.3 ? 1:0; 
}    

