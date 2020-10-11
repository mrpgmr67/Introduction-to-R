# Create a discrete event simulation for a queue system involving a bus terminal. 
# It should include an event list, along with components to add and subtract from 
#that list as buses arrive and depart from the terminal. The following functions should be present:
#
# - Core simulation function
# - Variable initialization
# - Add event
# - Event response
# - Get next event
# - Function to print the results of the simulation


# Variable Initialization
sim_data <- 0

# Add Event
add_bus <-function(sim_data){
  print(paste0("Adding Bus:", sim_data))
  return(sim_data+1)
}

# Event Response
remove_bus <-function(sim_data){
  if(sim_data -1 >0){
    print(paste0("Removing Bus:", sim_data))
    return(sim_data=sim_data-1)
  }
  print(paste0("Removing Bus:",sim_data))
  return(sim_data)
}

# Print Results
print_results <- function(sim_data){
  print(paste0("Total Bus Count:", sim_data))
  return(0)
}

# Core Simulation
main <- function(){
  sim_data <- 0
  for (i in 1:100){                     # Event Response
    if(sample(1:100,1) > 50){
      sim_data <-add_bus(sim_data)
    } else{
      sim_data <- remove_bus(sim_data)
    }
  }
  print_results(sim_data)
  return("Simulation Complete")
}

main()  # Test Event by executing it