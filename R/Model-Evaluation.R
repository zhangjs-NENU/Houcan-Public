
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

library(mlr3verse)
library(ggplot2)
library(precrec)
library(ggsci)
library(Cairo)

lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("info")

color_function <- ggsci::scale_colour_lancet
color_function_kwargs <- list(alpha = 0.9)


# Define the performance measures to evaluate
measures <- msrs(c("classif.auc","classif.acc",
                   "classif.precision", "classif.recall","classif.specificity"))

Train_Eva <- function(Exp_Group,use_turnner = FALSE,learners_index = NA){
  
  # Read data
  if (use_turnner) {
    learners <- readRDS(paste0("data/",Exp_Group,"/turnned_learners.RDS"))
  } else {
    learners <- Initialization_learners()[[1]]
  }
  
  if(length(learners_index) == 1 && is.na(learners_index)){
    learners <- learners
  } else {
    learners <- learners[learners_index]
  }
  
  train_task <- readRDS(paste0("data/",Exp_Group,"/train_task.RDS"))
  
  # Create a benchmark grid for model evaluation
  design <- benchmark_grid(
    tasks = train_task,
    learners = learners,
    resamplings =rsmp("cv", folds = 10) 
  )
  
  # Perform the benchmarking
  cat("\n Benchmark for train datasets by 10-cv")
  future::plan("multisession")
  bmr <- benchmark(design)

  # Aggregate the results based on the defined measures
  bmr_res <- bmr$aggregate(measures)
  
  # Prepare the performance Train_performance for visualization
  Train_performance <- bmr_res[,c(4,7:11)]
  Train_performance$F1 <- 2*(Train_performance$classif.precision*Train_performance$classif.recall/
                   (Train_performance$classif.precision+Train_performance$classif.recall))
  colnames(Train_performance) <- mlr3misc::capitalize(gsub("classif.", "", colnames(Train_performance)))
  
  # Create an ROC plot for selected models
  plot <- autoplot(bmr$clone(deep = T)#$filter(learner_id = c("catboost","svm","randomForest","logistic","knn"))
                   ,type = "roc",)
  
  data <- data.frame(x = plot$data$x,y = plot$data$y, Learners = plot$data$modname)
  
  plot_ROC <- ggplot(data,aes(x=x,y=y,fill=Learners,colour = Learners))+
    stat_smooth(method = "gam",se=F)+
    do.call(color_function,color_function_kwargs)+
    labs(x = "False positive rate", y = "True positive rate") +
    scale_x_continuous(limits = c(-0.01,1.01),breaks = c(0,0.5,1))+
    scale_y_continuous(limits = c(-0.01,1.1),breaks = c(0,0.5,1))+
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                 linetype = "dashed", color = "red",linewidth = 1.5)+
    # geom_segment(aes(x = 0.74, y = 0.76, xend = 0.64, yend = 0.86), 
    #              linetype = "solid", color = "black",linewidth = 2,
    #              arrow = arrow(length = unit(0.5, "cm")))+
    # geom_segment(aes(x = 0.76, y = 0.74, xend = 0.86, yend = 0.64), 
    #              linetype = "solid", color = "black",linewidth = 2,
    #              arrow = arrow(length = unit(0.5, "cm")))+
    geom_point(aes(x=0,y=1),color = "blue" ,size = 4, show.legend = FALSE)+
    theme(axis.title.x = element_text(size = rel(1.3), face = "bold"),
          axis.title.y = element_text(size = rel(1.3), face = "bold"),
          axis.text.y = element_text(size=rel(1.6)),
          axis.text.x = element_text(size=rel(1.6)),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey",linewidth = 0.8),
          panel.grid = element_line(colour = "grey",linewidth = 0.8),
          legend.text = element_text(size = rel(1.1), face = "bold"),
          legend.title = element_text(size = rel(1.3), face = "bold"))+
    annotate("text", x = 0.65, y = 0.6, 
             label = "Random classifier", color = "red", 
             hjust = 1, vjust = 1, angle = 45,size = 5,fontface = "bold")+
    # annotate("text", x = 0.9, y = 0.6, 
    #          label = "Worse", color = "black", size = 6,fontface = "bold")+
    # annotate("text", x = 0.6, y = 0.91, 
    #          label = "Better", color = "black",size = 6,fontface = "bold")+
    annotate("text", x = 0.12, y = 1.04, 
             label = "Perfect classifier", color = "blue",size = 5,fontface = "bold")
  
  # Save the ROC plot as an .eps file
  cairo_ps(filename = paste0("Figure/Fig_ROC_",Exp_Group,".eps"), onefile = FALSE, fallback_resolution = 700, width = 7.5, height = 6)
  print(plot_ROC)
  dev.off()
  
  CairoJPEG(filename = paste0("Figure_small/Fig_ROC_",Exp_Group,".jpeg"), width = 7.5, height = 6,units="in",dpi=200)
  print(plot_ROC)
  dev.off()
  
  return(Train_performance)
}


Independent_train_learners <- function(Exp_Group,use_turnner = FALSE,learners_index = NA){
  
  if (use_turnner) {
    learners <- readRDS(paste0("data/",Exp_Group,"/turnned_learners.RDS"))
  } else {
    learners <- Initialization_learners()[[1]]
  }
  
  if(length(learners_index) == 1 && is.na(learners_index)){
    learners <- learners
  } else {
    learners <- learners[learners_index]
  }
  
  train_task <- readRDS(paste0("data/",Exp_Group,"/train_task.RDS"))
  
  for (i in 1:length(learners)) {
    Trained_learners <- learners[[i]]$train(train_task)
  }
  
  saveRDS(learners,paste0("data/",Exp_Group,"/Trainned_learners.RDS"))
}


Test_Eva <- function(Exp_Group){
  
  # Read data
  learners <- readRDS(paste0("data/",Exp_Group,"/turnned_learners.RDS"))
  train_task <- readRDS(paste0("data/",Exp_Group,"/train_task.RDS"))
  test_task <- readRDS(paste0("data/",Exp_Group,"/train_task.RDS"))
  
  # Initialize a data frame to store individual model predictions
  Test_performance <- data.frame(matrix(ncol = 7, nrow = 6))
  predictions <- list()
  colnames(Test_performance) <- c("learner_id","classif.auc","classif.acc",
                         "classif.precision","classif.recall","classif.specificity","F1")
  
  # Populate the data frame with model performance metrics
  cat("\n Benchmark for test datasets")
  for (i in 1:length(learners)) {
    prediction <- learners[[i]]$train(train_task)$predict(test_task)
    predictions[[i]] <- prediction
    Test_performance[i,1] <- learners[[i]]$id
    Test_performance[i,2:6] <- prediction$score(measures)
  }
  Test_performance$F1 <- 2*(Test_performance$classif.precision*Test_performance$classif.recall/
                     (Test_performance$classif.precision+Test_performance$classif.recall))
  colnames(Test_performance) <- mlr3misc::capitalize(gsub("classif.", "", colnames(Test_performance)))
  
  saveRDS(learners,paste0("data/",Exp_Group,"/Trainned_learners.RDS"))
  
  return(Test_performance)
  
}


Best_learners <- function(index,learners_index){
  
  Choose_learners <- list()
  
  for (i in 1:length(index)) {
    Choose_learners <- c(Choose_learners,readRDS(paste0("data/",order[index[i]],"/Trainned_learners.RDS"))[[learners_index[i]]])
  }
  
  saveRDS(Choose_learners,"data/learners.RDS")
}

