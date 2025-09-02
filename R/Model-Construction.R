
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

library(haven)
library(tidyverse)
library(mlr3verse)
library(mlr3extralearners)
library(Boruta)
library(Cairo)
library(writexl)

lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("info")

order <- c("Low","Medium-Low","Medium-High","High","Verify-Low","Verify-High")


Check_create_dir <- function(Exp_Group, recursive = TRUE) {
  
  dir_path <- paste0("data/",Exp_Group)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = recursive, showWarnings = FALSE)
    message("Filefolder has been creative : ", dir_path)
  } else {
    message("Filefolder already exists : ", dir_path)
  }
}


Data_process <- function(data,Exp_Group,Demographics_save = FALSE){
  
  data <- data[-which(colnames(data) %in% c("age_group","SCORE"))]
  
  if (!Demographics_save) {
    data <- data[-which(colnames(data) %in% c("age","gender"))]
  }
  
  exp_pos <- which(order == Exp_Group)
  if (exp_pos == 2) {  
    use_data <- data 
    use_data$score_group <- ifelse(use_data$score_group %in% order[1:exp_pos],
                                   "Lower","Higher") |> as.factor()
    
  } else if (exp_pos %in% c(1,3)) {
    target_groups <- order[exp_pos:(exp_pos+1)]  
    use_data <- data[data$score_group %in% target_groups, ]
    use_data$score_group <- use_data$score_group |> as.factor()
    
  } else if (exp_pos %in% c(5,6)) { 
    target_groups <- order[2:3]  
    use_data <- data[data$score_group %in% target_groups, ]
    use_data$score_group <- use_data$score_group |> as.factor()
  }
  
  labelled_idx <- map_lgl(use_data, ~inherits(., "haven_labelled"))
  
  use_data[labelled_idx] <- lapply(use_data[labelled_idx], as.numeric)
  
  return(use_data)
}


Initialization_tasks <- function(data,Exp_Group,feature_order = NA,
                                 reversal = TRUE, save_number = 2,split = FALSE){
  
  use_data <- Data_process(data,Exp_Group,Demographics_save = TRUE)
  exp_pos <- which(order == Exp_Group)
  
  if (length(feature_order) == 1 && is.na(feature_order)) {
    feature_order <- readRDS(paste0("data/",Exp_Group,"/feature_order.RDS"))
  } 
  
  if (reversal) {
    feature_order <- rev(feature_order)
  }
  
  if (exp_pos == 2) {  
    feature_choose <- feature_order[1:save_number]
  } else if (exp_pos %in% c(1,3)) {
    last_choose <- readRDS(paste0("data/",order[2],"/feature_choose.RDS"))
    feature_choose <- union(setdiff(feature_order,last_choose)[1:save_number],
                            last_choose)
    
  } else if (exp_pos == 5) { 
    last_choose <- readRDS(paste0("data/",order[1],"/feature_choose.RDS"))
    feature_choose <- union(setdiff(feature_order,last_choose)[1:save_number],
                            last_choose)
    
  } else if (exp_pos == 6) { 
    last_choose <- readRDS(paste0("data/",order[3],"/feature_choose.RDS"))
    feature_choose <- union(setdiff(feature_order,last_choose)[1:save_number],
                            last_choose)
    
  }
  
  saveRDS(feature_choose,paste0("data/",Exp_Group,"/feature_choose.RDS"))
  
  use_data <- use_data[c(feature_choose,"age","gender","score_group")]
  
  task <- as_task_classif(use_data,target = "score_group",
                          positive = ifelse(exp_pos == 2,"Higher",
                                            ifelse(exp_pos %in% c(1,3),Exp_Group,"Medium-High")))
  
  if (split) {
    holdout <- rsmp("holdout", ratio = 0.7)
    holdout$instantiate(task)
    
    train_idx <- holdout$train_set(1) 
    test_idx <- holdout$test_set(1) 
    train_data <- task$data(rows = train_idx)
    test_data <- task$data(rows = test_idx) 
    
    train_task <- as_task_classif(train_data, target = "score_group")
    test_task <- as_task_classif(test_data, target = "score_group")
    
    saveRDS(train_task,paste0("data/",Exp_Group,"/train_task.RDS"))
    saveRDS(test_task,paste0("data/",Exp_Group,"/test_task.RDS"))
  } else {
    saveRDS(task,paste0("data/",Exp_Group,"/train_task.RDS"))
  }

  cat(" Task initialization for : ",Exp_Group," has been done")
  
}


Initialization_learners <- function(){
  
  learner_rf <- lrn("classif.ranger", predict_type = "prob", id = "randomForest")
  learner_log <- lrn("classif.log_reg", predict_type = "prob", id = "logistic")
  learner_xgboost <- lrn("classif.xgboost", predict_type = "prob", id = "xgboost")
  learner_lightgbm <- lrn("classif.lightgbm", predict_type = "prob", id = "lightgbm")
  learner_catboost <- lrn("classif.catboost", predict_type = "prob", id = "catboost")
  learner_knn <- lrn("classif.kknn", predict_type = "prob", id = "knn")
  learner_svm <- lrn("classif.svm", predict_type = "prob", id = "svm",
                     type = "C-classification", kernel = "radial")
  

  learners = list(learner_log,learner_knn,
                      learner_svm,learner_rf,learner_xgboost,
                      learner_lightgbm,learner_catboost)
  
  search_space <- list()
  
  search_space[["randomForest"]] <- ps(  
    randomForest.num.trees = p_int(lower = 200, upper = 1500, tags = "budget"),
    randomForest.max.depth = p_int(lower = 1, upper = 30),
    randomForest.num.threads = p_int(lower = 1, upper = 20)
  )  
  
  search_space[["logistic"]] <- ps(
    logistic.epsilon = p_dbl(lower = 1e-12, upper = 1e-6, logscale = TRUE),  
    logistic.maxit = p_dbl(lower = 10, upper = 1000, tags = "budget")
  )
  
  search_space[["xgboost"]] <- ps(
    xgboost.nrounds = p_int(lower = 16, upper = 2048, tags = "budget"),
    xgboost.eta = p_dbl(lower = 1e-4, upper = 1, logscale = TRUE),
    xgboost.max_depth = p_int(lower = 1, upper = 30),
    xgboost.colsample_bytree = p_dbl(lower = 1e-1, upper = 1),
    xgboost.colsample_bylevel = p_dbl(lower = 1e-1, upper = 1),
    xgboost.lambda = p_dbl(lower = 1e-3, upper = 1e3, logscale = TRUE),
    xgboost.alpha = p_dbl(lower = 1e-3, upper = 1e3, logscale = TRUE),
    xgboost.subsample = p_dbl(lower = 1e-1, upper = 1),
    xgboost.min_child_weight = p_dbl(1, 10)
  )
  
  search_space[["knn"]] <- ps(  
    knn.k = p_int(lower = 1, upper = 10, tags = "budget"),
    knn.distance = p_dbl(lower = 0, upper = 10)
  )
  
  search_space[["catboost"]] <- ps(  
    catboost.iterations = p_int(lower = 1000, upper = 5000),
    catboost.learning_rate = p_dbl(lower = 0.01, upper = 0.1, tags = "budget")
  )
  
  search_space[["lightgbm"]] <- ps(  
    lightgbm.num_leaves = p_int(lower = 5, upper = 50),   
    lightgbm.learning_rate = p_dbl(0.01, 0.1, tags = "budget"),
    lightgbm.num_iterations = p_int(lower = 20, upper = 500)
  )
  
  search_space[["svm"]] <- ps(
    svm.cost = p_dbl(0.1, 2, tags = "budget"),
    svm.gamma = p_dbl(1e-4, 1, logscale = TRUE)
  )
  
  return(list(learners,search_space))
}


Filter_Boruta <- function(data,Exp_Group){

  use_data <- Data_process(data,Exp_Group)
  
  cat(" Processing variables filter, target variable : ",Exp_Group,"\n",
      "Sample size : ",nrow(use_data))
  
  var_sec <- Boruta(score_group~.,data=use_data,doTrace=1,
                    pValue = 0.001,maxRuns = 200,num.threads=0)
  
  imp_history <- as.data.frame(var_sec$ImpHistory)
  shadow_cols <- grep("shadow", colnames(imp_history), value = TRUE)
  real_cols <- setdiff(colnames(imp_history), shadow_cols)
  
  final_decision <- tibble(
    feature = real_cols, 
    decision = var_sec$finalDecision[real_cols]
  )
  
  shadow_decision <- tibble(
    feature = shadow_cols, 
    decision = rep("Shadow", length(shadow_cols))
  )
  
  decision_all <- bind_rows(final_decision, shadow_decision)
  
  imp_history_long <- imp_history %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    left_join(decision_all, by = "feature") %>%
    filter(decision != "Shadow")
  
  feature_order <- imp_history_long %>%
    group_by(feature) %>%
    summarise(median_imp = median(importance, na.rm = TRUE)) %>%
    arrange(median_imp) %>%
    pull(feature)
  
  
  importance_plot <- ggplot(imp_history_long, aes(
    x = fct_relevel(feature, feature_order), 
    y = importance, 
    fill = decision
  )) +
    geom_violin(
      scale = "width", 
      width = 1,    
      color = "black",  
      size = 0.3       
    ) +
    geom_boxplot(
      width = 0.15, 
      fill = "white",
      size = 0.3,
      outlier.size = 0.5
    ) +
    scale_fill_manual(
      values = c(
        "Confirmed" = alpha("#02BBC1", 0.6), 
        "Tentative" = alpha("#FFC107", 0.6), 
        "Rejected" = alpha("#E53935", 0.6)
      )
    ) +
    labs(
      title = "Feature Importance Violin Plot Based on Boruta", 
      x = "Features", 
      y = "Importance (Z-Score)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5,vjust = 2,size = rel(1.5), face = "bold"),
          axis.title.x = element_text(size = rel(1.3), face = "bold"),
          axis.title.y = element_text(size = rel(1.3), face = "bold",vjust = 2),
          axis.text.y = element_text(size=rel(1.2)),
          axis.text.x = element_text(size=rel(1.2)),
          legend.position = "none",
          panel.background = element_blank(),)

  
  cairo_ps(filename = paste0("Figure/Fig_Violin_",Exp_Group,".eps"), onefile = FALSE, fallback_resolution = 700,  width = 11, height = 6)
  print(importance_plot)
  dev.off()
  
  CairoJPEG(filename = paste0("Figure_small/Fig_Violin_",Exp_Group,".jpeg"), width = 11, height = 6,units="in",dpi=400)
  print(importance_plot)
  dev.off()
  
  saveRDS(feature_order,paste0("data/",Exp_Group,"/feature_order.RDS"))
  
}


Hyperband_turning <- function(Exp_Group,learners_index = NA){
  
  init <- Initialization_learners()
  
  learners <- init[[1]]     
  search_space <- init[[2]]  
  
  if(length(learners_index) == 1 && is.na(learners_index)){
    learners <- learners
  } else {
    learners <- learners[learners_index]
  }
  
  train_task <- readRDS(paste0("data/",Exp_Group,"/train_task.RDS"))
  
  future::plan("multisession")
  
  cat("\n Prepare for parameters turning : test the origin performance")
  design_raw <- benchmark_grid(
    tasks = train_task,
    learners = learners,
    resamplings =rsmp("cv", folds = 3) 
  )  
  
  bmr_raw <- benchmark(design_raw)
  
  result_learner_param_vals <- list()
  
  set.seed(111)
  
  cat("\n Begin parameters turning : ")
  for(i in learners){
    instance <- mlr3tuning::tune(
      tuner = tnr("hyperband", eta = 5),
      task = train_task,
      learner = i,
      resampling = rsmp("holdout",ratio =0.7),
      measure = msr("classif.auc"),
      search_space = search_space[[i$id]],
      store_models = T,
    )
    parm_result <- instance$result_learner_param_vals
    names(parm_result) <- gsub(paste0(i$id,"\\."), "", names(parm_result))
    parm_result <- parm_result[!duplicated(names(parm_result), fromLast = TRUE)]
    i$param_set$values <- parm_result
    result_learner_param_vals[[i$id]] <- parm_result
  }
  
  cat("\n Test the performance after turning")
  pre_res <- data.frame(matrix(ncol = 3, nrow = length(learners)))
  colnames(pre_res) <- c("learner","AUC_before","AUC_after")
  
  for (i in 1:length(learners)) {pre_res[i,1] <- learners[[i]]$id}

  design <- benchmark_grid(
    tasks = train_task,
    learners = learners,
    resamplings =rsmp("cv", folds = 3) 
  )
  
  bmr <- benchmark(design)
  
  pre_res[,2] <- bmr_raw$aggregate(msr("classif.specificity"))$classif.specificity
  pre_res[,3] <- bmr$aggregate(msr("classif.specificity"))$classif.specificity
  
  
  writexl::write_xlsx(pre_res,paste0("data/",Exp_Group,"/Performance_tune_table.xlsx"))
  saveRDS(result_learner_param_vals,paste0("data/",Exp_Group,"/tune_results.RDS"))
  saveRDS(learners,paste0("data/",Exp_Group,"/turnned_learners.RDS"))
  
  
}

  