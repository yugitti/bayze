data { 
	int<lower=0> N;    #�ϑ���
	int<lower=0> x[N]; #���߂Đ�������܂ł̎��s��
}
transformed data{
    int<lower=0> n[N];
#stan�ŕ��̓񍀕��z���g���Ċ􉽕��z���`����Ƃ��C��������܂ł̎��s��x-1��^����
    for(i in 1:N)
        n[i] <- x[i]-1;
}
parameters {
	real<lower=0>    beta;
}
transformed parameters{
	real<lower=0,upper=1>   theta; #�����m��
	theta <- beta/(beta+1);
}
model {
	n ~ neg_binomial(1, beta); #���̓񍀕��z�Ń�=1�ɌŒ肷��
}
generated quantities{
	real<lower=0,upper=1>   p4; #4����ɗ��X����m��
	real            mu;
	p4 <- theta*pow((1-theta),3);
	mu <- 1/theta;

}
