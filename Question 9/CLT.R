library(Rlab)
library(tidyverse)

# Set designated seed 
set.seed(1363)

#declare functions

method_1 <- function(sample_mean, sample_size){
  
  p = 0.3
  gamma <- 0.94
  
  z = qnorm((1 + gamma) / 2)
  
  # Apply quadratic formula
  a = 1 + z^2/sample_size
  b = -z^2/sample_size - 2 * sample_mean
  c = sample_mean^2
  
  lower <- (-b - sqrt(b^2 - 4*a*c))/(2*a)
  upper <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
  
  interval_width = upper - lower
  
  #return width for comparison
  return(interval_width)
}

method_2 <- function(sample_mean, sample_size){
  
  sd <- sqrt(sample_mean * (1 - sample_mean) / sample_size)
  
  # Compute the 94% confidence interval
  z <- qnorm(0.97)
  lower <- sample_mean - z * sd
  upper <- sample_mean + z * sd
  
  interval_width = upper - lower
  
  #return width for comparison
  return(interval_width)
}

# ------------------------------------------ #

sample_size = c(30,50,100,200,300,500,1000)
mean_difference_vector = seq(1,7,by = 1)

for(i in 1:length(sample_size)){
  
  difference_vector = seq(1,1500,by=1)
  
  for(n in 1:1500){
    sample_vector = rbinom(n = sample_size[i], size = 1,prob = 0.3)

    sample_mean = mean(sample_vector)
    interval_width_1 = method_1(sample_mean, sample_size[i])
    interval_width_2 = method_2(sample_mean, sample_size[i])
    
    difference = interval_width_2 - interval_width_1
    difference_vector[n] = difference
  }
  
  mean_difference_vector[i] = mean(difference_vector)
}

# plot
plot(sample_size,mean_difference_vector,pch=19,xaxt="n", xlab = "Tamanho da amostra", ylab = "Diferença média entre métodos")
segments(sample_size, 0, sample_size, mean_difference_vector)
abline(h=0, col="blue",lwd=1.5)
axis(1, at = sample_size)










