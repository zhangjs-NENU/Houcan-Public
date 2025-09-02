
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

library(haven)
library(mlr3verse)
library(tidyverse)

source("R/Predict-Base.R",encoding="utf-8")

data <- readRDS("data/Independent_test_set.RDS")

labelled_idx <- map_lgl(data, ~inherits(., "haven_labelled"))

data[labelled_idx] <- lapply(data[labelled_idx], as.numeric)

# 
# for (i in 1:nrow(data)) {
#   
#   cat(" Predicting Number",i,"\n")
#   use_data <- data[i,]
#   
#   main <- Predict_Main(as.character(i),"Medium-Low",use_data)
#   
#   if (main@response == "Lower") {
#     main <- Predict_Main(as.character(i),"Low",use_data)
#     if (main@response == "Low") {
#       data[i,"response"] <- main@response
#       data[i,"prob"] <- main@prob
#     } else {
#       main <- Predict_Main(as.character(i),"Verify-Low",use_data)
#       data[i,"response"] <- main@response
#       data[i,"prob"] <- main@prob
#     }
#   }
#   
#   
#   if (main@response == "Higher") {
#     main <- Predict_Main(as.character(i),"Medium-High",use_data)
#     if (main@response == "High") {
#       data[i,"response"] <- main@response
#       data[i,"prob"] <- main@prob
#     } else {
#       main <- Predict_Main(as.character(i),"Verify-High",use_data)
#       data[i,"response"] <- main@response
#       data[i,"prob"] <- main@prob
#     }
#   }
#   
# }
# 
# result <- data.frame(
#   response <- factor(data$response, levels = order),
#   score_group <- factor(data$score_group, levels = order))
# 
# table(result$response, result$score_group)
# 
# saveRDS(data,"data/predict_data_total.RDS")
# 

library(foreach)
library(doParallel)


cl <- makeCluster(detectCores() - 1)  # 保留1个核心避免系统卡顿
registerDoParallel(cl)

result <- foreach(i = 1:nrow(data), .combine = rbind) %dopar% {

  cat("Predicting Number", i, "\n")
  
  setClass(
    Class = "PredictOutput",
    slots = c(
      id = "character",
      response = "character",
      prob = "numeric"
    )
  )
  
  use_data <- data[i, ]
  
  main <- Predict_Main(as.character(i),"Medium-Low",use_data)
  
  if (main@response == "Lower") {
    main <- Predict_Main(as.character(i),"Low",use_data)
    if (main@response == "Low") {
    } else {
      main <- Predict_Main(as.character(i),"Verify-Low",use_data)
    }
  }
  
  
  if (main@response == "Higher") {
    main <- Predict_Main(as.character(i),"Medium-High",use_data)
    if (main@response == "High") {
    } else {
      main <- Predict_Main(as.character(i),"Verify-High",use_data)
    }
  }  
  
  data.frame(
    response = main@response,
    prob = main@prob,
    row = i 
  )
}

stopCluster(cl)

data <- data[order(result$row), ]
data$response <- result$response
data$prob <- result$prob

result <- data.frame(
  response <- factor(data$response, levels = c("Low","Medium-Low","Medium-High","High")),
  score_group <- factor(data$score_group, levels = c("Low","Medium-Low","Medium-High","High")))

table(result$response, result$score_group)

saveRDS(data,"data/predict_data_total.RDS")
