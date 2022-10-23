# Recursive function example

factoral_recursion_example <- function(x) {
  if(x==0 || x==1){
    print(x)
    return(x)
  } else {
    print(x)
    return(x*factoral_recursion_example(x-1)) 
  }
}

# Should be 6
#    3 x 2 x 1 = 6
factoral_recursion_example(3)

# factoral_recursion_example(3) calls factoral_recursion_example(2)
# factoral_recursion_example(2) calls factoral_recursion_example(1)
# x = 1 so it ends the recursion

# start with 3.  3 does not fulfill the first condition so it calls 3 * factoral_recursion(2)
# start with 2.  2 does not fulfill the first condition so it calls 2 * factoral_recursion(1)
# 1 does fulfill the condition

# Should be 24
factoral_recursion_example(4)

# Should be 720
factoral_recursion_example(6)

