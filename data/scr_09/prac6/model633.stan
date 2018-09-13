data { 
	int<lower=0>  N;    
	real<lower=0> x[N]; 
}
parameters {
	real                mu;
	real<lower=0>    sigma;
}
transformed parameters {
	real<lower=0> sigmasq;
     	sigmasq <- pow(sigma,2);
}
model {
	for(n in 1:N){
		x[n] ~ normal(mu,sigma);
	}	
}
generated quantities {
      real    quantile_90; 
      real<lower=0, upper=1> prob_over;

	quantile_90 <- mu + 1.282*sigma;
	prob_over <- 1-normal_cdf(quantile_90, 87, 5); 
}
