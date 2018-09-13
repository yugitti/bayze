data { 
	int<lower=0>  N;    #�L�^��
	int<lower=0> Xn[N]; #�g������L�^
	int<lower=0> Xm[N]; #�}�X�I����L�^
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
	real	delta; #�{��(7.3)���Ɠ��l�ɔg������ƃ}�X�I�����lambda�̍����Ƃ�
	real    delta_p; #�{��(7.4)���Ɠ��l�̐�����
	delta = lambdaN - lambdaM;
	delta_p = step(delta);
}
