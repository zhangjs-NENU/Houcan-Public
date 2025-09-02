
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

order <- c("Low","Medium-Low","Medium-High","Verify-Low","Verify-High")

learners <- readRDS("data/learners.RDS")

feature_names <- list(
  learners[[1]]$state$feature_names,
  learners[[2]]$state$feature_names,
  learners[[3]]$state$feature_names,
  learners[[4]]$state$feature_names,
  learners[[5]]$state$feature_names
)

setClass(
  Class = "PredictOutput",
  slots = c(
    id = "character",
    response = "character",
    prob = "numeric"
  )
)

Predict_Main <- function(pid,stage,data){
  
  index <- which(order == stage)
  use_learner <- learners[[index]]
  use_features <- feature_names[[index]]
  
  main <- use_learner$predict_newdata(data[use_features])
  
  output <- new("PredictOutput",
                id = pid, 
                response = as.character(main$response),
                prob = as.numeric(main$prob[,as.character(main$response)]))
  
  return(output)
  
}
