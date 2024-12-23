

# Task 1 

# size
n <- 5
# given x vector
x <- c(-2.8,3.4,1.2,-0.3,-2.6)
loc <- seq(-4,4,0.1)


## a

res <- c()

# log likelihood calculations
for (i in loc) {

    
    logli <- -n * log(pi) - sum(log(1 + (x - i)^2))
  
    res <- c(res,logli)
}
res



plot(y=res, x=loc, type='l',ylab='loglikelihood',xlab='x',
     main='Log likelihood function for given data, range and scale parameter')

# there looks to be a local max and min and a the maximum of the log likelihood function 
# for the given data and range. 




# log likelihood derivative calculations

res_der <- c()
for (i in loc) {
  
  
  der <- sum((2 * (x - i))/(1 + (x - i)^2))
  
  res_der <- c(res_der,der)
}

# plotting the derivative
plot(y=res_der, x=loc, type='l',ylab='derivate of loglikelihood',xlab='x',
     main='Derivate of log likelihood function for given data, range and scale parameter')

# adding a line for 0
abline(h=0, col='red')

# The derivative of the log likelihood functions for the given data is zero three times, which can be seen 
# when the line crosses the horizontal red line in the plot. 



## b


bisection <- function(x,a0,b0, criterion){
      ga0 <- sum((2 * (x - a0))/(1 + (x - a0)^2))
      
      gb0 <- sum((2 * (x - b0))/(1 + (x - b0)^2))
      
      # checking if there is a derivate of 0 in the intervall
      if(ga0 * gb0 > 0)stop('Start with interval [a0,b0] such that g`(a0) * g(b0) < 0')
      
      # which is the biggest one if the user made an input error
      if(!(ga0 > gb0))stop('Check that the intervall have atleast 1 maximum')
        
      a <- a0
      b <- b0

      # objects an calculations for the loop
      int_mean <- (a0+b0)/2
      t <- 1
      g_x <- sum((2 * (x - int_mean))/(1 + (x - int_mean)^2))
      absolute_c_c <- 1
      int_mean_c <- c(int_mean)
      
      while (absolute_c_c > criterion) {
        if(g_x==0)break # if we found the max
        
        if (g_x > 0) { # checking if its positive or negative
          a <- int_mean
        }else{
          b <- int_mean
        }
        
        int_mean <- (a+b)/2
        
        int_mean_c <- c(int_mean_c,int_mean)
        
        g_x <- sum((2 * (x - int_mean))/(1 + (x - int_mean)^2))
        
        
        absolute_c_c <- abs(int_mean_c[t+1] - int_mean_c[t])
        
        t <- t+1
      }
      return(int_mean)
}

a0 <- -3

b0 <- 4

bisection(x,a0,b0, 0.000001)

## c

### lokal maximum interval a0 = -4 , b0 = -2
bisection(x,-4,-2, 0.000001)
### maximum interval a0 = -1 , b0 = 2
bisection(x,-1,2, 0.00000001)
bisection(x,-1,2.1, 0.00000001)
# Some pairs doesn't fulfill the criterion of the bisection method
# "Start with interval [a0,b0] such that g`(a0) * g(b0) < 0'" like -4 and -3 
#bisection(x,-4,-3, 0.000001) # == ERROR
# 


## D

# we could modify the program in such a way so you always calculate 



# task 2

## a

# creating my function
myvar <- function(x){
      # creating n
      n <- length(x)
      # the first part of the calculations
      div <- 1 / (n - 1)
      
      # second part of the calculations
      summ <- sum(x^2) - ((1 / n) * (sum(x)^2)) 
      
      # product of them
      variance <- div * summ
      
      # returning my variance
      variance
  
}

myvar(x)

var(x)
## b
set.seed(12345)
x_vec <- rnorm(10000,mean=10^8,sd=1)

## c

yi <- c()

for(i in 1:10000){
  # calculating the difference (i=1 will return NA)
yi[i] <- myvar(x_vec[1:i]) - var(x_vec[1:i])

}

plot(yi, main='Difference between myvar and var() for subset 1 to 1:n',
     ylab = 'Diff')


# The difference gets closer to 0 the more observations we measure from, the 'line' is under 0 
# which might indicate that our function are undershooting the variance a bit, but at the same time
# the 'curves' looks to be more dense on the positive side which might lead the average close to 0.

# So our function might be performing good and the difference between the functions 
# could be rounding of the calculations. 



## d

# If we calculate the mean of x we can calculate the variance as the sum of 
# x-mean(x)² // n-1

mod_var <- function(x){
    # creating n 
    n <- length(x)
    
    
    variance <- sum((x - mean(x))^2) /(n-1)
  
  
    variance
  
}

yi2 <- c()

for(i in 1:10000){
  # calculating the difference (i=1 will return NA)
  yi2[i] <- mod_var(x_vec[1:i]) - var(x_vec[1:i])
  
}

plot(yi2, main='Difference between mod_var and var() for subset 1 to 1:n',
     ylab = 'Diff')

# now the difference between var() and mad_var is now basically 0. 

mean(yi2[-1])

