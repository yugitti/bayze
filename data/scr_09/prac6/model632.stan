data { 
	int<lower=0>  N;    
	real<lower=0> x[N];  
}
parameters {
	real<lower=0>    sigma;
}
transformed parameters {
	real<lower=0> sigmasq;
     	sigmasq <- pow(sigma,2);
}
model {
	x ~ normal(130,sigma);
}
generated quantities{
      real<lower=0,upper=1> sigmasq_under;
      real<lower=0,upper=1> sigmasq_under2;
 
      sigmasq_under<-step(1-sigmasq/1.5);
      sigmasq_under2<-step(1-sigmasq/1.0);
}

