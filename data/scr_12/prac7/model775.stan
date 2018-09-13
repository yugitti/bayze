data { 
	int<lower=0> N;    #�f�[�^��
	real<lower=0> x[N]; #�N���f�[�^
}
parameters {
	real<lower=0>   mu;
	real<lower=0>	sigma;
}
model {
	x ~ lognormal(mu, sigma);
}

generated quantities{
	real<lower=0>	zeta; #�ΐ����K���z��50�p�[�Z���^�C���_
	real<lower=0>	p_500; #500���~�����50%�ɓ����Ă���m��
	zeta = exp(mu);
	p_500 = zeta<500? 1: 0;
}

