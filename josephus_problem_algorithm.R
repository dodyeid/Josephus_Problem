#### ---- JOSEPHUS ALGORITHM ----

josephus_solver <- function(n,k) {
  # Initialize counter of people in the circle
  i = 1 
  
  # Initialize counter within the k-cycle
  k_count = 1
  
  # Initilize the list of alive people. Notice the nth person is coded at 0 for modulo purposes.
  alive = append(rep(1:(n-1)),0)
  
  # Initialize the list of dead people.
  dead = c()
  
  # Commence the loop. Continue until 1 person remains.
  while(length(alive) > 1) {
    
    # Check if person is alive
    if (i%%n %in% alive) {
      
      # Check if we've reached the k value.
      if (k_count%%k == 0) {
        
        # Kill the person
        dead <- append(dead, i%%n)
        alive <- alive[!alive %in% c(i%%n)]
        
        # Increment the k counter
        k_count <- (k_count + 1)%%k
        
        # Increment the person counter
        i <- i + 1
        
        # Go to while condition and begin again
        next
      } else { # If we are not yet at the k value
        # Increment the k counter
        k_count <- (k_count + 1)%%k
        
        # Increment the person counter
        i <- i + 1
        
        # Go to while condition and begin again
        next
      }
    } else { # If the person is dead
      # Increment the person counter
      i <- i + 1
      
      # Go to while condition and begin again
      next
    }
  }
  # When all is said and done, turn 0 back to n again in the alive list 
  alive <- ifelse(alive == 0, n, alive)
  
  # Print result
  print(alive)
}


#### ---- TESTING APPROACH ----
josephus_solver(41,3)




