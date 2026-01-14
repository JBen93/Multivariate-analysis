rm(list=ls(all=TRUE))
set.seed(1)

#create probabilities for location
nloc=5
seq1=1:nloc
combo=expand.grid(x=seq1,y=seq1)
combo$prob=dnorm(combo$x,mean=3,sd=2)*dnorm(combo$y,mean=3,sd=2)
combo$prob=combo$prob/sum(combo$prob)

#visual depiction of probabilities
pi1=matrix(NA,nloc,nloc)
for (i in 1:nrow(combo)){
  pi1[combo$x[i],combo$y[i]]=combo$prob[i]  
}

par(mfrow=c(1,1),ask=T)
image(pi1,x=1:nloc,y=1:nloc,xlab='',ylab='')
text(x=combo$x,y=combo$y,round(combo$prob,3),pos=1)