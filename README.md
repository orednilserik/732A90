# This repository contains some of the work completed by me and another student during a computational statistics course in my master's program!

The code is written in R, using Rmarkdown and a pdf with the results have been generated.

- lab 1: Bisection and calculations
- lab 2: Optimization of functions and steepest ascent
- lab 3: Sampling with envelope, reverse CDF method and rejection 
- lab 4: Metropolis Hastings and Gibbs sampling
- lab 5: Bootstrap
- lab 6: Genetic and EM-algortihm


#### If you have any questions regarding the work, please contact me

<br><br>

# Example on how to solve the n-queens problem with the genetic algortihm
### (size of the board must be 4x4 or greater to be solved)

```R
# Creating an individual 
chessboard_c <- function(n){
   sample(1:n,n) # sampling positions for the queens from 1:n
}

# crossover function
crossover_c <- function(board1,board2){
  p <- length(board1)/2 # finding the p value
  c(board1[1:p],board2[(p+1):length(board2)])  # adding the 2 boards together
}

# Mutate function 
mutate_c <- function(board, prob){
  if(runif(1) > prob){
    board
  }else{
    
    idx <- sample(1:length(board),1) # sampling which queen to switch
    
    pos <- sample((1:length(board)),1) # sampling a position for the sampled queen
    while (pos==board[idx]) {
      pos <- sample((1:length(board)),1) 
    }
    
    board[idx] <- pos # adding the position to the board
    
    board
  }
}

# Three different types of fitness functions

fitnes_c_unkilled <- function(board) {
  unattacked_queens <- length(board)  # starting the count
  
  # Function to check if two queens can attack each other
  queens_attack <- function(queen1, queen2,i,j) {
    # Queens can attack each other if they are in the same row, column, or diagonal
    same_row <- queen1 == queen2
    same_diagonal <- abs(queen1 - queen2) == abs(i - j)
    
    return(same_row  | same_diagonal)
  }
  
  for (i in 1:(length(board) - 1)) { # looping through every queen
    kill <- 0
    for (j in (i + 1):length(board)) {
      if (queens_attack(board[i], board[j],i,j)) { # checking if they can be killed
        kill <- 1
        
        break() # queen can be killed, dont need to check against other queens
      }
    }
    if(kill == 1){ # if killed then unattacked -1
      unattacked_queens <- unattacked_queens - 1
    }
    
    
  }
  
  unattacked_queens
}



fitnes_c_pairs <- function(board) {
  attacking_pairs <- 0 # starting the count of attacking pairs
  
  # Function to check if two queens can attack each other
  queens_can_attack <- function(queen1, queen2,i,j) {
    # Queens can attack each other if they are in the same row, column, or diagonal
    same_row <- queen1 == queen2
    same_diagonal <- abs(queen1 - queen2) == abs(i - j)
    
    return(same_row | same_diagonal)
  }
  
  for (i in 1:(length(board) - 1)) {  # looping through every queen
    for (j in (i + 1):length(board)) {
      if (queens_can_attack(board[i], board[j],i,j)) { # checking if they can be killed
        attacking_pairs <- attacking_pairs + 1
      }
    }
  }
  
  attacking_pairs
}


fitnes_c_binary <- function(board) {
  solution <- 1  # starting the count
  
  # Function to check if two queens can attack each other
  queens_attack <- function(queen1, queen2,i,j) {
    # Queens can attack each other if they are in the same row, column, or diagonal
    same_row <- queen1 == queen2
    same_diagonal <- abs(queen1 - queen2) == abs(i - j)
    
    return(same_row  | same_diagonal)
  }
  
  for (i in 1:(length(board) - 1)) { # looping through every queen
    kill <- 0
    for (j in (i + 1):length(board)) {
      if (queens_attack(board[i], board[j],i,j)) { # checking if they can be killed
        solution <- 0 # if its not a solution
        break()
      }
    }
     if(solution==0){
       break()
       }
    
  }
  solution
}


# pairs, creating a population
pop_c_p <-function(n,pop_size){
  lis <- list()
  fit_vec <- c()
  for (i in 1:pop_size) {
    lis$"pop"[[i]] <- chessboard_c(n)
    fit_vec[i] <-  fitnes_c_pairs(lis$"pop"[[i]] )
  }
  
  lis$fit <- fit_vec
  lis
}

# Binding it all together to 1 function
genetic_c_pairs <- function(n, iter,pop_size,mutprob){
  pop <- pop_c_p(n,pop_size)
  
  fitness_vec <- c() # fitness vector
  i <- 0 # iterations
  pair_queens <- c()
  while(i <= iter){
    
    # a sampling two boards
    b1 <- sample(1:length(pop$pop),1) # sampling 1 parent
    b2 <- sample((1:length(pop$pop))[-b1],1) # sampling another parent
    parent1 <- pop$pop[b1][[1]]
    parent2 <-  pop$pop[b2][[1]]
    
    victim <- pop$"pop"[order(pop$fit, decreasing=TRUE)[1]] # selecting a victim
    
    # Doing the crossover, mutate and fitness step
    kid <- crossover_c(parent1,parent2)
    
    kid <- mutate_c(kid, mutprob)
    
    fit <- fitnes_c_pairs(kid)
    
    pop$pop[[order(pop$fit, decreasing=TRUE)[1]]] <- kid
    
    pop$fit[order(pop$fit, decreasing=TRUE)[1]] <- fit
    
    
    
    pair_queens <- c(pair_queens,fit)
    
    if(fit == 0){
      print(fit)
      print(kid)
      
      mat <- matrix(0,n,n)
      
      for (j in 1:n) {
        mat[kid[j],j] <- 1
        
      }
      print(mat)
      i <- i + 1
      break("Legal configuration found")
    }
    
    i <- i + 1
  }
  
  # Plotting the number of pairs of queens attacking each other at each iteration
  plot(1:(i), pair_queens, type = "l", xlab = "Iteration", ylab = "Attacking Pairs Count", main = "Pairs of queens attacking each other")
}

```

<br>
<br>
<br>
<br>
<br>

<div align="center">
  <img src="https://media4.giphy.com/media/v1.Y2lkPTc5MGI3NjExOHl4Zm44ejQxd2sxeDIxcGF4ZHJhYzI1NjQ0OTFqN3cyMG05ZXEwdyZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/lvXRxRBiFDpYY/giphy.gif" width="400" height="400"/>
</div>

