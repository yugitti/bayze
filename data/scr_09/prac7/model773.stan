data { 
	int<lower=0> N;    #観測回数
	int<lower=0> x[N]; #初めて成功するまでの試行数
}
transformed data{
    int<lower=0> n[N];
#stanで負の二項分布を使って幾何分布を定義するとき，成功するまでの失敗数x-1を与える
    for(i in 1:N)
        n[i] <- x[i]-1;
}
parameters {
	real<lower=0>    beta;
}
transformed parameters{
	real<lower=0,upper=1>   theta; #成功確率
	theta <- beta/(beta+1);
}
model {
	n ~ neg_binomial(1, beta); #負の二項分布でα=1に固定する
}
generated quantities{
	real<lower=0,upper=1>   p4; #4日後に来店する確率
	real            mu;
	p4 <- theta*pow((1-theta),3);
	mu <- 1/theta;

}
