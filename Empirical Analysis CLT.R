########################################################
# Empirical Analysis: CLT
########################################################

#################################
#part a 
#################################

##################
#section 1 + 2
##################

N = 10000000
mu = 30
sigma = 6

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
legend("topleft",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

##################
#section 3
##################

 
  #small i=1
  #large i=2
  
Sample_Size = c(10, 100000 )

reps = 2000

i=1

samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )

sample_Means <- colMeans(samples)


##################
#section 4 + 5
##################

hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

##################
#section 6 + 7
##################

Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))

hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)

curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#################################
#part b 
#################################

##################
#section 1 + 2
##################

N = 10000000
n_Trials = 60
p = 0.2

X = rbinom(n=N, size = n_Trials, prob=p)

E = n_Trials*p 
Var = n_Trials*p*(1-p)
sd = sqrt(Var) 
print(sd)

hist(X, 
     col = "steelblue" , 
     prob = FALSE,
     breaks = seq(0,n_Trials,1),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)

x = seq(0,n_Trials,1)
lines(x,dbinom(x,n_Trials,p)* N ,col="red")
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

##################
#section 3
##################

  #small i=1
  #large i=2

Sample_Size = c(2, 100000 )

reps = 2000

i=1

samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )

sample_Means <- colMeans(samples)

print(sample_Means)

##################
#section 4 + 5
##################

hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)

x = seq(0,n_Trials,1)
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

##################
#section 6 + 7
##################

Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))

hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)

curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )


#################################
#part c
#################################

##################
#section 1 + 2
##################

N = 10000000
df = 4 

X = rt(N, df, ncp = 0)
E = 0
Var = ifelse(df > 2, df/(df-2), Inf)  
sd = sqrt(Var) 
print(sd)

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

##################
#section 3
##################

  #small i=1
  #large i=2

Sample_Size = c(2, 100000 )

reps = 2000

i=1

samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )

sample_Means <- colMeans(samples)

print(sample_Means)

##################
#section 4 + 5
##################

hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)

x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

##################
#section 6 + 7
##################

Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))

hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)

curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#################################
#part d
#################################

##################
#section 1 + 2
##################

N = 10000000
df1 = 10
df2 = 5 

X = rf(N, df1, df2)
E = df2/(df2-2)
Var =2*df2^2*(df1+df2-2)/(df1*(df2-2)^2*(df2-4))
sd = sqrt(Var)
print(sd)
print(E)
print(Var)

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

##################
#section 3
##################

  #small i=1
  #large i=2

Sample_Size = c(2, 100000 )

reps = 2000

i=2

samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )

sample_Means <- colMeans(samples)

print(sample_Means)

##################
#section 4 + 5
##################

hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)

x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

##################
#section 6 + 7
##################

Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))

hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)

curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

################################################################
#EXTRA CREDIT
################################################################

################################
# Chi Square Distribution
################################

##############
#Section 1 + 2
##############

# The underlying population follows Chi(df)
N = 10000000
df = 10
# randomly generate N samples from the underlying population
X = rchisq(N, df, ncp = 0)
E = df# Theoretical Expectation for  Chi Square Distribution
Var =2*df # Theoretical Expectation for Binomial Distribution
sd = sqrt(Var) # Theoretical standard deviation
print(sd)
# draw the histogram of N samples randomly draw 
# from the underlying population distribution Chi(df)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Chi(df)
curve(dchisq(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

##############
#Section 3
##############

### small Sample vs Large Sample
Sample_Size = c(2, 100000 )
### number of repetition
reps = 2000
### 
#i=1 #small sample 
#i=2 #large sample
i=2
## Conduct reps (2000) times of following experiment: 
## each time draw sample_Size  many of samples from underlying population
## samples is a matrix with dimension Sample_Size*reps
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

print(sample_Means)
##############
#Section 4 + 5
##############

hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)

x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )


##############
#Section 6 + 7
##############

Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))

hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)

curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )
