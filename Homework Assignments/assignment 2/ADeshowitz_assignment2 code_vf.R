

# Tried to write this code from scratch, and got pretty close, but wound up having to copy this source:

#!/usr/bin/env Rscript
# Author:  Jason A. French
# website: http://www.jason-french.com/blog/2014/07/26/recursion-in-r/



# writing a recursive function for sorting:


vect <- sample(1:100, 10, replace = FALSE)

my.recursive <- function() {
  #terminating condition
  if (x==0) return (1)
  #recursive condition
  else  
    return(x * my.recursive(x-1))
  
}

my.recursive <- function(vect) {
  
  if (length(vect) <= 1) {
    return (vect)
  }
  
  item <- vect[1]
  remain <- vect[-1]
  
  v1 <- remain[remain<item]
  v2 <- remain[remain >= item]
  
  v1 <- my.recursive(v1)
  v2 <- my.recursive(v2)
  
  return (c(v1, item, v2))
  
    
  

}


my.recursive(vect)

