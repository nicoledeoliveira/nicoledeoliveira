
# Empirical Analysis: LLN 

########################################################
#part a
########################################################

####################
#section 1 + 2
####################

N = 10000000
mu = 4
sigma = 16

X = rnorm(N, mean = mu, sd = sigma)

E = mu 
Var = sigma^2 
sd = sigma 

hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dnorm(x, mean = mu, sd = sigma), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 3
####################

Sample_Size = 2^(0:23)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

####################
#section 4 
####################

plot(Sample_Size, Sample_Mean, log = "x", ylim =c(mu-sigma,mu+sigma), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)

abline(a = NULL, b = NULL, h = mu, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 5
####################

  #############
  #bullet 1 + 2
  #############

N = 10000000
mu = 3
sigma = 12

X = rnorm(N, mean = mu, sd = sigma)

E = mu 
Var = sigma^2
sd = sigma 

hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dnorm(x, mean = mu, sd = sigma), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

  #########
  #bullet 3
  #########

Sample_Size = 2^(0:25)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

  #########
  #bullet 4
  #########
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(mu-sigma,mu+sigma), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = mu, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )






########################################################
#part b
########################################################

####################
#section 1 + 2
####################

N = 10000000
n_Trials = 50
p = 0.4

X = rbinom(n=N, size = n_Trials, prob=p)

E = n_Trials*p 
Var = n_Trials*p*(1-p) 
sd = as.integer(sqrt(Var)) 

hist(X, 
     col = "steelblue" , 
     prob = FALSE,
     breaks = seq(0,n_Trials,1),
     main = 'Histogram and Underlying Distribution',cex.main=0.75)

x = seq(0,n_Trials,1)
lines(x,dbinom(x,n_Trials,p)* N ,col="red")
legend("topright",c("Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 3
####################

Sample_Size = 2^(0:23)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

####################
#section 4
####################

plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 5
####################

  #############
  #bullet 1 + 2
  #############

N = 10000000
n_Trials = 70
p = 0.5

X = rbinom(n=N, size = n_Trials, prob=p)

E = n_Trials*p 
Var = n_Trials*p*(1-p) 
sd = as.integer(sqrt(Var)) 

hist(X, 
     col = "steelblue" , 
     prob = FALSE,
     breaks = seq(0,n_Trials,1),
     main = 'Histogram and Underlying Distribution',cex.main=0.75)

x = seq(0,n_Trials,1)
lines(x,dbinom(x,n_Trials,p)* N ,col="red")
legend("topright",c("Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

  #########
  #bullet 3
  #########

Sample_Size = 2^(0:25)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

  #########
  #bullet 4
  #########

plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)

abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )





########################################################
#part c
########################################################

####################
#section 1 + 2
####################

N = 10000000
df = 10 

X = rt(N, df, ncp = 0)
E = 0
Var = ifelse(df > 2, df/(df-2), Inf)  
sd = sqrt(Var) 


hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlim= c(-10,10), 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dt(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 3
####################

Sample_Size = 2^(0:23)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

####################
#section 4
####################

plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 5
####################

  #############
  #bullet 1 + 2
  #############

N = 10000000
df = 4 

X = rt(N, df, ncp = 0)
E = 0
Var = ifelse(df > 2, df/(df-2), Inf)  
sd = sqrt(Var) 


hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlim= c(-10,10), 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dt(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

  #########
  #bullet 3
  #########

Sample_Size = 2^(0:25)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

  #########
  #bullet 4
  #########

plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )







########################################################
#part d
########################################################

####################
#section 1 + 2
####################

N = 10000000
df1 = 9
df2 = 7 

X = rf(N, df1, df2)
E = df2/(df2-2)
Var =2*df2^2*(df1+df2-2)/(df1*(df2-2)^2*(df2-4))
sd = sqrt(Var)

hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 5000, 
     xlim= c(0,20),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(df(x,df1,df2), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 3
####################

Sample_Size = 2^(0:23)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

####################
#section 4
####################

plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

####################
#section 5
####################

  #############
  #bullet 1 + 2
  #############

N = 10000000
df1 = 7
df2 = 5 

X = rf(N, df1, df2)
E = df2/(df2-2)
Var =2*df2^2*(df1+df2-2)/(df1*(df2-2)^2*(df2-4))
sd = sqrt(Var)

hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 5000, 
     xlim= c(0,20),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(df(x,df1,df2), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

  #########
  #bullet 3
  #########

Sample_Size = 2^(0:25)

Sample_Mean = numeric(length = length(Sample_Size))

for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}

  #########
  #bullet 4
  #########

plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )





