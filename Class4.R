# Descriminant Analys


library("MASS")
set.seed(1)

# params
n = 100
v1 = c(5,2)
v2 = c(1,1)
S = c(1,1,1,2)

# generate data
mu1=matrix(v1,2,1)
mu2=matrix(v2,2,1)

Sigma = matrix(S,2,2)

c1 = mvrnorm(n,mu1,Sigma, empirical = T)
c2 = mvrnorm(n,mu2,Sigma, empirical = T)
dim(c1)

data=data.frame(x1=c(c1[,1],c2[,1]),
                x2=c(c1[,2],c2[,2]),
                label=c(rep("c1",n),rep("c2",n)))

# plot
library(ggplot2)
ggplot(data)+
  aes(x=x1,y=x2,color=label)+
  geom_point()+
  stat_ellipse(level = 0.95, linewidth = 1, type="norm")

### Bayesian Approach (Has to be Gaussian)

Sigma_ = solve(Sigma)
intercept = log(0.5/0.5)+(1/2)%*%t(mu1)%*%Sigma_%*%mu1-(1/2)%*%t(mu2)%*%Sigma_%*%mu2

             
ggplot(data)+
  aes(x=x1,y=x2,color=label)+
  geom_point()+
  stat_ellipse(level = 0.95, linewidth = 1, type="norm")+
  geom_abline(intercept = -(16.5/3), slope = 7/3)

#Determining the treshold
w = Sigma_%*%(mu1-mu2)
proj=as.matrix(data[,1:2])%*%w

best = 0
for (i in seq(min(proj),max(proj),length.out = 1000)){
  acc = mean((proj<i)==(data$label=="c1"))
  if (saved<acc){
    best = acc
    treshold = i
  }
}


### Non-Gaussian Approach (Fisher) 



