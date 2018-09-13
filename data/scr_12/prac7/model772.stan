data { 
	int<lower=0> N; #���d���̖{��
	real<lower=0>  x[N]; #���d����������܂łɐi�񂾋���
}
parameters {
	real<lower=0>                lambda;
}
model {
	x ~ exponential(lambda);
}
generated quantities{
	real<lower=0> p; #�{��(7.7)���Ɠ��l�̐�����eta
	real<lower=0> eta; #�{��(7.8)����x*=100�Ƃ����C������p
	real P_underp; #������p��0.5�ȏ�ɂȂ�m��
	eta = inv(lambda); 
	p = exponential_cdf(100,lambda);
	P_underp = p > 0.5?1:0;
}

