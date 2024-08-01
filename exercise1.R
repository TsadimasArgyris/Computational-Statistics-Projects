#a
pdf<-function(x, θ) {
  return(θ^2 / (θ + 1) * (1 + x) * (exp(-θ * x)))
}

cdf<-function(x, θ) {
  return(1 - (exp(-θ * x) * (1 + θ + θ * x)) / (1 + θ))
}

inverse_of_cdf<-function(x, u){
  cdf(x, 1) - u
}
uniroot(inverse_of_cdf,c(-1,100),u=runif(1,0,1))


#b
rejection_method <- function(n) {
  samples <- numeric(n)
  for (i in 1:n) {
    accept <- FALSE
    while (!accept) {
      x <- rexp(1, 1.5)  # Generate a sample from the exponential envelope
      u <- runif(1)
      if (u * 4/3 * dexp(x,1.5 ) <= pdf(x, 2)) {
        samples[i] <- x
        accept <- TRUE
      }
    }
  }
  return(samples)
}

# Replicate the rejection method 100 times
replicated_samples <- replicate(100, rejection_method(1000))


xx<-seq(0,5,by=0.01)
plot(ecdf(replicated_samples))
lines(xx,cdf(xx,2),col=2)

#c with simulation

N<-10000
results <- numeric(N)
for(i in 1:N){
θ<-5
π1<-θ/(θ+1)
π2<-1/(θ+1)
x<-runif(1,0,1)
f1<-function(a,u){
  π1<-u/(u+1)
  return(π1*u*(exp(-u*a))) #this is the first part of the density
}

f2<-function(a,u){
  π2<-1/(u+1)
  return((π2*u^2)*u*(exp(-u*a))) #this is the first part of the density
}
comp_pdf<-π1*f1(x,θ)+π2*f2(x,θ)
results[i]<-comp_pdf
}

M<-1000
#finding the sampling distribution of
#the sample mean and the sample median
#for n=100
sample_of_100<-sample(results, 100 , replace = FALSE)
sample_means1 <- numeric(M)
sample_medians1 <- numeric(M)
for(i in 1:M){
  sample_size<-sample(1:100, 1)
  simulated_sample <- sample(sample_of_100, sample_size, replace = TRUE)
  sample_means1[i] <- mean(simulated_sample)
  sample_medians1[i] <- median(simulated_sample)
}

variance_of_means1 <- var(sample_means1)
print(variance_of_means1)
variance_of_medians1 <- var(sample_medians1)
print(variance_of_medians1)
correlation1 <- cor(sample_means1, sample_medians1)
print(correlation1)

#finding the sampling distribution of
#the sample mean and the sample median
#for n=1000

sample_of_1000<-sample(results, 1000 , replace = FALSE)
sample_means2 <- numeric(M)
sample_medians2 <- numeric(M)
for(i in 1:M){
  sample_size<-sample(1:1000, 1)
  simulated_sample <- sample(sample_of_1000, sample_size, replace = TRUE)
  sample_means2[i] <- mean(simulated_sample)
  sample_medians2[i] <- median(simulated_sample)
}

variance_of_means2 <- var(sample_means2)
print(variance_of_means2)
variance_of_medians2 <- var(sample_medians2)
print(variance_of_medians2)
correlation2 <- cor(sample_means2, sample_medians2)
print(correlation2)


# Estimate P(X > 5) using the sample means of each size
prob_estimate1 <- sum(sample_means1 > 5) / length(sample_means1)
prob_estimate2 <- sum(sample_means2 > 5) / length(sample_means2)
print(prob_estimate1)
print(prob_estimate2)