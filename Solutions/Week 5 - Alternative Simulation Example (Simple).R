sim_data <- 0
add_bus <-function(sim_data){
  print(paste0("Adding Bus:", sim_data))
  return(sim_data+1)
}

remove_bus <-function(sim_data){
  if(sim_data -1 >0){
    print(paste0("Removing Bus:", sim_data))
    return(sim_data=sim_data-1)
  }
  print(paste0("Removing Bus:",sim_data))
  return(sim_data)
}

print_results <- function(sim_data){
  print(paste0("Total Bus Count:", sim_data))
  return(0)
}
main <- function(){
  sim_data <- 0
  for (i in 1:100){
    if(sample(1:100,1) > 50){
      sim_data <-add_bus(sim_data)
    } else{
      sim_data <- remove_bus(sim_data)
    }
  }
  print_results(sim_data)
  return("Simulation Complete")
}
main()


## Trying it out in a matrix format

sim_data2 <- matrix(0,nrow=1,ncol=3)
colnames(sim_data2) <-c("BusNumber2", "Bus in Term", "Other")

sim_data2
add_bus2 <-function(sim_data2){
  print(paste0("Adding Bus:", sim_data2))
  return(sim_data2+1)
}
apply(sim_data2,1,add_bus2)

remove_bus2 <-function(sim_data2){
  if(sim_data2 -1 >0){
    print(paste0("Removing Bus:", sim_data2))
    return(sim_data2=sim_data2-1)
  }
  print(paste0("Removing Bus:",sim_data2))
  return(sim_data2)
}
apply(sim_data2,2,remove_bus2)


print_results <- function(sim_data2){
  print(paste0("Total Bus Count:", sim_data2))
  return(0)
}
main2 <- function(){
  sim_data2 <- 0
  for (i in 1:100){
    if(sample(1:100,1) > 50){
      sim_data2 <-add_bus2(sim_data2)
    } else{
      sim_data2 <- remove_bus2(sim_data2)
    }
  }
  print_results(sim_data2)
  return("Simulation Complete")
}
main2()
