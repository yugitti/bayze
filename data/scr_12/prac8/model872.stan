//�������ʃ��f��
data{
	int<lower=0> S; #�󌱎ҁi���Z�ҁj
	int<lower=0> R; #�]���
	vector<lower=0, upper=100>[R] Score[S]; #�_��
}
transformed data{
	int<lower=0> Rm1; # beta�̑��a��0�ɂȂ�悤�ɂ��邽�߂̉�����
	Rm1 = R-1;
}
parameters{
	real mu;
	real alpha[S];
	real beta_m1[Rm1];
	real<lower=0> tauSubject; #���Z�҂Ɋւ���W���΍�

	real<lower=0> tauWithin;
}
transformed parameters{
	real beta[R];
	real<lower=0> sig2subject;

	real<lower=0> sig2within;
	sig2subject = pow(tauSubject,2); #���U�̒�`

	sig2within = pow(tauWithin,2);
	# beta�̑��a��0�ɂȂ�悤�ɂ��邽�߂̍׍H
	for (r in 1:Rm1) {
		beta[r] = beta_m1[r];
	}
	beta[R] = -sum(beta_m1); #beta�̍Ō�̗v�f�͔��������Ȃ��ŁAR-1�Ԗڂ܂ł̘a�̕������t�]���������̂����i�Ȃ����ۂɃT���v�����O����͕̂ϐ�beta_m1�ɂ��Ăł��邱�Ƃɒ��ӁB�j
}
model{
	real nu;
	mu ~ normal(0, 1000);
	for(s in 1:S){
		alpha[s] ~ normal(0, tauSubject);
	}
	for(s in 1:S) {
		for(r in 1:R) {
			nu = mu + alpha[s] + beta[r];
			Score[s,r] ~ normal(nu, tauWithin);
		}
	}
}
generated quantities{
	real<lower=0> ICC31;
	real<lower=0> ICC37;
	ICC31 = sig2subject / (sig2subject + sig2within); #R�̂Ƃ���Ől����5��6�ɕς���
	ICC37 = sig2subject / (sig2subject + (sig2within/4)); 
}
