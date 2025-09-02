
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

library(mlr3verse)
library(mlr3extralearners)

# 检测并安装catboost包
if (!requireNamespace("catboost", quietly = TRUE)) {
  # 若未安装，提示并自动安装
  message("")
  
  # 安装前确保devtools可用（catboost可能需从GitHub安装）
  if (!requireNamespace('remotes', quietly = TRUE)) {
    install.packages('remotes', dependencies = TRUE)
  }
  
  # 从GitHub安装catboost（官方推荐方式）
  tryCatch({
    install.packages('remotes')
    remotes::install_url('BINARY_URL'[, INSTALL_opts = c("--no-multiarch", "--no-test-load")])
    message("catboost包安装成功！")
  }, error = function(e) {
    stop("catboost包安装失败，请手动运行以下命令安装：\n devtools::install_github('catboost/catboost', subdir = 'R-package')")
  })
}

order <- c("Low","Medium-Low","Medium-High","Verify-Low","Verify-High")

CESD8_name <- c("qn406", "qn407", "qn411", "qn412", "qn414", "qn416", "qn418", "qn420")

learners <- readRDS("learners.RDS")

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


truth_response <- function(use_data) {
  
  SCORE <- rowSums(use_data[1,CESD8_name])
  response <- ifelse(SCORE <= 10,"Low",
                     ifelse(SCORE <= 13,"Medium-Low",
                            ifelse(SCORE <= 16,"Medium-High",
                                   "High" )))
  return(response)
}

