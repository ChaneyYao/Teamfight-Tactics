stage_number <- 0
big_stage_number <- 1
number_of_components <- 0

start_counting <- FALSE
complete_count <- 0

status = FALSE
first_time = TRUE

hextech_rounds = c("2-1", "3-2", "4-2")

# Empty list to collect results

result_list <- list()

for (plus in 1:40){
  
  stage_number <- stage_number + 1
  
  # Rules and mechanisms
  
  if (stage_number == 5 && big_stage_number == 1){
    stage_number <- stage_number - 4
    big_stage_number <- big_stage_number + 1
    number_of_components <- 3
  }
  
  if (stage_number == 8){
    big_stage_number <- big_stage_number + 1
    stage_number <- stage_number - 7
  }
  
  game_stage_str <- paste(big_stage_number, stage_number, sep = "-")
  
  ## condition achieved?
  
  if (complete_count >= 160 && first_time == TRUE){
    status = TRUE
    first_time = FALSE
    print(game_stage_str)
  }
  
  # Gaining items
  
  ## Items drop off carosel
  
  if (stage_number == 4){
    if (big_stage_number == 2 || big_stage_number == 3 || big_stage_number == 4){
      number_of_components <- number_of_components + 1
    } else if (big_stage_number > 4){
      number_of_components <- number_of_components + 2
    }
  }
  
  ## Items drop from monsters
  
  if (stage_number == 7){
    number_of_components <- number_of_components + 3
  }
  
  ## Items drop from hextech
  
  if (game_stage_str %in% hextech_rounds){
    if (game_stage_str == "2-1"){
      number_of_components = number_of_components + 6
    } else if (game_stage_str == "3-2"){
      number_of_components = number_of_components + 3
    } else if (game_stage_str == "4-2"){
      number_of_components = number_of_components + 2
    }
  }
  
  # Calculating
  
  ## Converting components into completed items
  
  completed_items <- floor(number_of_components / 2)
  
  ## 7 Battle Academia counting
  
  ### Assuming 7 B.A. on 3-5
  
  if (big_stage_number == 3 && stage_number == 6){
    start_counting <- TRUE
  }
  
  if (start_counting && !(stage_number %in% c(4,7))){
    complete_count <- complete_count + completed_items
  }
  
  # Store values in a list of lists
  
  result_list[[plus]] <- list(
    "game stage" = game_stage_str,
    "number of components" = number_of_components,
    "completed items" = completed_items,
    "complete count" = complete_count,
    "status" = status
  )
}

# Convert to data frame

result_df <- do.call(rbind, lapply(result_list, as.data.frame))





