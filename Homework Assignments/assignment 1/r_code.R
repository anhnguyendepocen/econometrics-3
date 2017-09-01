################################
## Homework Assignment - 1
## MSDS- 8310-401
## Alex Deshowitz
################################


#*********
#Code adapted from www.programiz.com
#https://www.programiz.com/r-programming/recursion
#*********
my.recursive <- function(x=7) {
  #terminating condition
  if (x==0) return (1)
  #recursive condition
  else  
    return(x * my.recursive(x-1))
    
}


## Alex Deshowitz Code Start:
x_vals <- 1:10 # create the vals to feed into the recursive function
x <- c() # instantiate the vector


# for loop that iterates through the array of values to be evaluated:
for (val in x_vals) {
   x[val] <- (my.recursive(val))

# plot of the results of the function calls:
barplot(x)
   
}
#********************
