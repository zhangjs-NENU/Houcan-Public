
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

source("R/Data-Extraction-Summary.R",encoding="utf-8")
source("R/Model-Construction.R",encoding="utf-8")
source("R/Model-Evaluation.R",encoding="utf-8")

lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("info")

data <- readRDS("data/data.RDS")
save_number <- c(3,1,3,0,2,2)

for (i in order[c(2,1,3,5,6)]) {
  
  Check_create_dir(i)
  Filter_Boruta(data,i)
  Initialization_tasks(data,i,save_number = save_number[which(order == i)])
}


for (i in order[c(3,5,6)]) {
  
  Hyperband_turning(i,learners_index = c(1,2,4,5,6,7))

  Train_performance <- Train_Eva(i,use_turnner = T)
  
  Independent_train_learners(i,use_turnner = T)
  
  writexl::write_xlsx(Train_performance,paste0("data/",i,"/Model_Performance.xlsx"))
  
  # Test_performance <- Test_Eva(i)
  # 
  # Performance_table <- rbind(Train_performance,Test_performance)
  # 
  # writexl::write_xlsx(Performance_table,paste0("data/",i,"/Model_Performance.xlsx"))
  # 
}

future::plan("sequential")

Best_learners(index = c(1,2,3,5,6),learners_index = c(6,6,6,6,6))
