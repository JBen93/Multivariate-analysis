#plot a histogram of random variables that follow a Poisson distribution with lambda=5
n=10000
x=rpois(n,lambda=5)

#summarize these data
tmp=table(x)
tmp=tmp/sum(tmp) #calculate relative frequency
plot(tmp,type='h')

#plot the the theoretical Poisson distribution assuming lambda=5
k=0:20
prob=dpois(k,lambda=5)
for (i in 1:length(k)){
  lines(rep(k[i],2)+0.1,c(0,prob[i]),col='red',lwd=2)  
}

#plot a histogram of random variables that follow a beta distribution with parameters a=3 and b=3
n=10000
x=rbeta(n,shape1=3,shape2=3)
hist(x,probability=T)

#plot the theoretical beta distribution assuming that the parameters are a=3 and b=3
seq1=seq(from=0,to=1,length.out=1000)
lines(seq1,dbeta(seq1,shape1=3,shape2=3),col='red')