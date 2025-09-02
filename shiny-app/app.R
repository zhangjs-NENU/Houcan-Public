
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

options(repos = c( # 显式添加mlr-org仓库
  CRAN = "https://cloud.r-project.org",        # 保留CRAN官方仓库（优先级低于mlrorg）
  shinyapps = "https://rstudio-buildtools.github.io/drat/"  # ShinyApps.io默认仓库
))

# 2. 检测并安装依赖包（确保从指定仓库安装）
required_packages <- c("shiny", "ggplot2","shinythemes", "catboost")  # 替换为你实际用的包
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(shiny)
library(ggplot2)

source("Predict-Base.R",encoding="utf-8")

# 定义UI界面（分步交互设计）
ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  
  
  tags$style(
    HTML("
      body {
        margin: 0;  /* 清除页面默认边距 */
        padding: 0;
      }
      
      body::before {
        content: '';
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-image: url('bg.jpg');  
        background-size: contain; 
        background-position: center center;  
        background-repeat: no-repeat;  
        opacity: 0.5;  
        z-index: -1;  
        background-color: #f0f0f0;  
      }
      
      .sidebarPanel {
        background-color: rgba(255, 255, 255, 0.7) !important;
        border-radius: 10px;
        padding: 20px;
        margin-left: 15px;
      }
      
      .mainPanel {
        background-color: rgba(255, 255, 255, 0.7) !important;
        border-radius: 10px;
        padding: 20px;
        margin-right: 15px;
      }
      
      .container-fluid {
        background-color: transparent !important;
        padding: 0;
        margin-top: 20px;
      }
    ")
  ),
  
  
  titlePanel("基于多阶段机器学习与 Boruta 特征筛选的 CESD-8 量表简化与测评系统\n"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      
      conditionalPanel(
        condition = "input.stage == 1",
        textInput("pid","被试编号：",value = "807"),
        selectInput("gender", "性别：",choices = c("男", "女"),selected = "男"),
        sliderInput("age", "年龄：", value = 18, min = 1, max = 100),
        sliderInput("qn411", "我的睡眠不好：", value = 1, min = 1, max = 4),
        actionButton("calc1", "继续")
      ),
      

      conditionalPanel(
        condition = "input.stage == 2",
        sliderInput("qn406", "我感到情绪低落：", value = 1, min = 1, max = 4),
        sliderInput("qn407", "我觉得做任何事都很费劲：", value = 1, min = 1, max = 4),
        sliderInput("qn412", "我感到愉快：", value = 1, min = 1, max = 4),
        actionButton("calc2", "继续")
      ),

      
      conditionalPanel(
        condition = "input.stage == 3",
        sliderInput("qn414", "我感到孤独：", value = 1, min = 1, max = 4),
        sliderInput("qn416", "我生活快乐：", value = 1, min = 1, max = 4),
        sliderInput("qn412", "我感到愉快：", value = 1, min = 1, max = 4),
        actionButton("calc3", "继续")
      ),
      
      
      conditionalPanel(
        condition = "input.stage == 4",
        sliderInput("qn414", "我感到孤独：", value = 1, min = 1, max = 4),
        sliderInput("qn416", "我生活快乐：", value = 1, min = 1, max = 4),
        actionButton("calc4", "继续")
      ),
      
      
      conditionalPanel(
        condition = "input.stage == 5",
        sliderInput("qn406", "我感到情绪低落：", value = 1, min = 1, max = 4),
        sliderInput("qn407", "我觉得做任何事都很费劲：", value = 1, min = 1, max = 4),
        actionButton("calc5", "继续")
      ),

      
      conditionalPanel(
        condition = "input.stage == 10",
        sliderInput("qn414", "我感到孤独：", value = 1, min = 1, max = 4),
        sliderInput("qn416", "我生活快乐：", value = 1, min = 1, max = 4),
        sliderInput("qn418", "我感到悲伤难过：", value = 1, min = 1, max = 4),
        sliderInput("qn420", "我觉得生活无法继续：", value = 1, min = 1, max = 4),
        actionButton("calc10", "继续计算以验证"),
        actionButton("calc99", "结束")
      ),
      
      
      conditionalPanel(
        condition = "input.stage == 20",
        sliderInput("qn418", "我感到悲伤难过：", value = 1, min = 1, max = 4),
        sliderInput("qn420", "我觉得生活无法继续：", value = 1, min = 1, max = 4),
        actionButton("calc20", "继续计算以验证"),
        actionButton("calc99", "结束")
      ),
      
      
      conditionalPanel(
        condition = "input.stage == 30",
        sliderInput("qn406", "我感到情绪低落：", value = 1, min = 1, max = 4),
        sliderInput("qn407", "我觉得做任何事都很费劲：", value = 1, min = 1, max = 4),
        sliderInput("qn418", "我感到悲伤难过：", value = 1, min = 1, max = 4),
        sliderInput("qn420", "我觉得生活无法继续：", value = 1, min = 1, max = 4),
        actionButton("calc30", "继续计算以验证"),
        actionButton("calc99", "结束")
      ),
      
      
      actionButton("reset", "重新开始")
    ),
    
    
    
    
    
    mainPanel(
      width = 8,
      h4(""),
      verbatimTextOutput("result"),
      textInput("stage", label = NULL, value = 1, width = "0px")
    )
  )
)



server <- function(input, output, session) {

  result_val <- reactiveVal(NULL)
  
  observeEvent(input$calc1, {

    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,    
      stringsAsFactors = FALSE
    )
    
    main <- Predict_Main(input$pid,"Medium-Low",use_data)

    if (main@response == "Higher") {
      result_val(paste("请继续作答"))
      updateTextInput(session, "stage", value = 3)  
    } else {
      result_val(paste("请继续作答"))
      updateTextInput(session, "stage", value = 2)  
    }
  })
  
  
  
  observeEvent(input$calc2, {
    
    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,
      qn406 = input$qn406,
      qn407 = input$qn407,
      qn412 = 5 - input$qn412,
      stringsAsFactors = FALSE
    )
    
    main <- Predict_Main(input$pid,"Low",use_data)
    
    if (main@response == "Low") {
      assign(x = "response", value = main, envir = globalenv())
      result_val(paste("分阶段模型运行完毕：\n可选择继续完成全部问卷加以验证。"))
      updateTextInput(session, "stage", value = 10)  
    } else {
      result_val(paste("请继续作答"))
      updateTextInput(session, "stage", value = 4)  
    }
  })
  
  
  
  observeEvent(input$calc3, {
    
    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,
      qn414 = input$qn414,
      qn416 = 5 - input$qn416,
      qn412 = 5 - input$qn412,
      stringsAsFactors = FALSE
    )
    
    main <- Predict_Main(input$pid,"Medium-High",use_data)

    if (main@response == "High") {
      assign(x = "response", value = main, envir = globalenv())
      result_val(paste("分阶段模型运行完毕：\n可选择继续完成全部问卷加以验证。"))
      updateTextInput(session, "stage", value = 30)  
    } else {
      result_val(paste("请继续作答"))
      updateTextInput(session, "stage", value = 5)  
    }
  })
  
  
  
  observeEvent(input$calc4, {
    
    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,
      qn406 = input$qn406,
      qn407 = input$qn407,
      qn412 = 5 - input$qn412,
      qn414 = input$qn414,
      qn416 = 5 - input$qn416,
      stringsAsFactors = FALSE
    )
    
    main <- Predict_Main(input$pid,"Verify-Low",use_data)
    
    if (main@response == "Medium-Low") {
      assign(x = "response", value = main, envir = globalenv())
      result_val(paste("分阶段模型运行完毕：\n可选择继续完成全部问卷加以验证。"))
      updateTextInput(session, "stage", value = 20)  
    } else {
      assign(x = "response", value = main, envir = globalenv())
      result_val(paste("分阶段模型运行完毕：\n可选择继续完成全部问卷加以验证。"))
      updateTextInput(session, "stage", value = 20)  
    }
  })
  
  
  
  observeEvent(input$calc5, {
    
    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,
      qn406 = input$qn406,
      qn407 = input$qn407,
      qn412 = 5 - input$qn412,
      qn414 = input$qn414,
      qn416 = 5 - input$qn416,
      stringsAsFactors = FALSE
    )
    
    main <- Predict_Main(input$pid,"Verify-High",use_data)
    
    if (main@response == "Medium-Low") {
      assign(x = "response", value = main, envir = globalenv())
      result_val(paste("分阶段模型运行完毕：\n可选择继续完成全部问卷加以验证。"))
      updateTextInput(session, "stage", value = 20)  
    } else {
      assign(x = "response", value = main, envir = globalenv())
      result_val(paste("分阶段模型运行完毕：\n可选择继续完成全部问卷加以验证。"))
      updateTextInput(session, "stage", value = 20)  
    }
  })
  
  
  
  observeEvent(input$calc10, {
    
    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,
      qn406 = input$qn406,
      qn407 = input$qn407,
      qn412 = 5 - input$qn412,
      qn414 = input$qn414,
      qn416 = 5 - input$qn416,
      qn418 = input$qn418,
      qn420 = input$qn420,
      stringsAsFactors = FALSE
    )
    
    main <- truth_response(use_data)
    
    result_val(paste("真实分类：",main,
                     "\n预测分类：",response@response,
                     "\n预测概率：",response@prob))
  })
  
  
  
  observeEvent(input$calc20, {
    
    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,
      qn406 = input$qn406,
      qn407 = input$qn407,
      qn412 = 5 - input$qn412,
      qn414 = input$qn414,
      qn416 = 5 - input$qn416,
      qn418 = input$qn418,
      qn420 = input$qn420,
      stringsAsFactors = FALSE
    )
    
    main <- truth_response(use_data)
    
    result_val(paste("真实分类：",main,
                     "\n预测分类：",response@response,
                     "\n预测概率：",response@prob))
  })
  
  
  
  observeEvent(input$calc30, {
    
    use_data <- data.frame(
      age = input$age,         
      gender = ifelse(input$gender == "男",1,0), 
      qn411 = input$qn411,
      qn406 = input$qn406,
      qn407 = input$qn407,
      qn412 = 5 - input$qn412,
      qn414 = input$qn414,
      qn416 = 5 - input$qn416,
      qn418 = input$qn418,
      qn420 = input$qn420,
      stringsAsFactors = FALSE
    )
    
    main <- truth_response(use_data)
    
    result_val(paste("真实分类：",main,
                     "\n预测分类：",response@response,
                     "\n预测概率：",response@prob))
  })
  
  
  
  observeEvent(input$calc99, {
    
    result_val(paste("预测分类：",response@response,
                     "\n预测概率：",response@prob))
  })
  
  
  
  observeEvent(input$reset, {

    updateTextInput(session, "stage", value = 1)
    updateTextInput(session, "pid", value = "807") 
    updateSelectInput(session, "gender", selected = "男") 
    updateSliderInput(session, "age", value = 18) 
    updateSliderInput(session, "qn411", value = 1) 
    updateSliderInput(session, "qn406", value = 1)
    updateSliderInput(session, "qn407", value = 1)
    updateSliderInput(session, "qn412", value = 1) 
    updateSliderInput(session, "qn414", value = 1)
    updateSliderInput(session, "qn416", value = 1)

    result_val(NULL)
  })
  
  # 输出结果
  output$result <- renderText({
    if (is.null(result_val())) {
      paste("您好！接下来您将进行的是 CESD-8（流调中心抑郁量表简化版）测试。\n
该量表是一种广泛用于评估近期抑郁情绪状态的工具，通过您的作答，\n
我们可以更客观地了解您近一周内的情绪感受和相关状态。\n
本次测试包含 8 个问题，每个问题都描述了一种可能出现的感受或行为。\n
请您根据自己过去一周（包括今天） 的实际情况，选择最符合您感受的选项。\n
选项分为 4 个等级：“1:很少或没有（少于 1 天）”、“2:少有（1-2 天）”\n
                   “3:常有（3-4 天）”、“4:几乎一直有（5-7 天）”。\n
请您如实作答，无需过度思考，您的第一反应往往是最准确的。")
    } else {
      paste(result_val())
    }
  })
}

# 运行应用
shinyApp(ui = ui, server = server)
