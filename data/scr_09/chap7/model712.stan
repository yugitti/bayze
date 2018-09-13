data { 
	int<lower=0>  N;    
	int<lower=0> x1[N]; 
	int<lower=0> x2[N]; 
}
parameters {
	real<lower=0>                lambdaA;
	real<lower=0>                lambdaB;
}
model {
	x1 ~ poisson(lambdaA);
	x2 ~ poisson(lambdaB);

}
generated quantities{
	real	delta; 
	real    p_delta; 
	delta <- lambdaB - lambdaA;
	p_delta <- step(delta); 
}    

##—¬‚ê¯–â‘è