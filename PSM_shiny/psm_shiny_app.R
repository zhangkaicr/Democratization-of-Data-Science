##############################################################################
# 倾向性评分匹配 (PSM) Shiny 交互式分析应用
# 基于 R 语言 matchit 包构建
# 
# 使用方法：在 R 或 RStudio 中直接运行本脚本即可启动应用
# 运行命令：source("psm_shiny_app.R")
# 
# 功能：
#   1. 上传 CSV/Excel 数据文件
#   2. 交互式选择处理变量和协变量
#   3. 配置匹配方法和参数
#   4. 执行倾向性评分匹配
#   5. 查看匹配前后协变量平衡性（标准化均数差 SMD）
#   6. 查看匹配摘要和诊断图表
#   7. 导出匹配后的数据集
##############################################################################

# ==============================================================================
# 第一部分：自动安装与加载所需 R 包
# ==============================================================================

# 定义本应用所需的 R 包列表
required_packages <- c(
  "shiny",        # Shiny 框架：构建交互式 Web 应用
  "MatchIt",      # MatchIt 包：核心倾向性评分匹配功能
  "haven",        # Haven 包：读取 Stata (.dta)、SPSS (.sav)、SAS 文件
  "readxl",       # Readxl 包：读取 Excel (.xlsx) 文件
  "dplyr",        # Dplyr 包：数据操作与转换
  "tidyr",        # Tidyr 包：数据整理（如 pivot_longer）
  "ggplot2",      # Ggplot2 包：高质量统计绑图
  "cobalt",       # Cobalt 包：匹配后协变量平衡性诊断
  "scales",       # Scales 包：坐标轴格式化（如百分比显示）
  "bslib",         # Bslib 包：Shiny Bootstrap 主题自定义
  "DT"             # DT 包：可交互数据表格（DataTables）
)

# 检查并安装缺失的包
# 使用 sapply 遍历每个包名，检查是否已安装
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

# 如果存在未安装的包，则批量安装
if (length(missing_packages) > 0) {
  message("正在安装缺失的 R 包：", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cran.r-project.org", quiet = TRUE)
}

# 加载所有必需的包（suppressPackageStartupMessages 隐藏启动信息，保持控制台整洁）
suppressPackageStartupMessages({
  library(shiny)
  library(MatchIt)
  library(haven)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cobalt)
  library(scales)
  library(bslib)
  library(DT)
})

# ==============================================================================
# 第二部分：定义全局辅助函数
# ==============================================================================

# 读取数据文件的通用函数
# 支持格式：CSV、TSV、Excel (.xlsx/.xls)、Stata (.dta)、SPSS (.sav/.por)
read_data_file <- function(file_path) {
  # 根据文件扩展名自动选择对应的读取函数
  ext <- tolower(tools::file_ext(file_path))
  
  data <- switch(ext,
    "csv"  = read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    "tsv"  = read.csv(file_path, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    "xlsx" = read_excel(file_path, progress = FALSE) %>% as.data.frame(),
    "xls"  = read_excel(file_path, progress = FALSE) %>% as.data.frame(),
    "dta"  = read_dta(file_path) %>% as.data.frame(),
    "sav"  = read_sav(file_path) %>% as.data.frame(),
    "por"  = read_por(file_path) %>% as.data.frame(),
    # 如果文件格式不支持，抛出错误
    stop("不支持的文件格式：", ext, "\n支持的格式：CSV、TSV、XLSX、XLS、DTA、SAV、POR")
  )
  
  return(data)
}

# 计算并返回标准化均数差（SMD）的函数
# SMD 是评估匹配前后协变量平衡性的核心指标
# SMD < 0.1 通常被认为平衡性良好
calculate_smd <- function(data, treatment_var, covariate_vars) {
  # 使用 lapply 对每个协变量计算 SMD
  smd_values <- sapply(covariate_vars, function(var_name) {
    # 提取处理组和对照组的数据
    treated <- data[data[[treatment_var]] == 1, var_name]
    control <- data[data[[treatment_var]] == 0, var_name]
    
    # 过滤掉 NA 值
    treated <- treated[!is.na(treated)]
    control <- control[!is.na(control)]
    
    # 计算合并标准差（pooled SD）
    pooled_sd <- sqrt((var(treated) + var(control)) / 2)
    
    # 如果合并标准差为 0（两组完全相同），则 SMD 为 0
    if (pooled_sd == 0) return(0)
    
    # 计算 SMD：两组均值之差 / 合并标准差
    smd <- (mean(treated) - mean(control)) / pooled_sd
    return(abs(smd))
  })
  
  return(smd_values)
}

# ==============================================================================
# 第三部分：Shiny 用户界面 (UI) 定义
# ==============================================================================

# 使用 bslib 定义现代化的 Bootstrap 主题
# `bs_theme` 自定义了整体视觉风格
app_theme <- bs_theme(
  version = 5,                    # 使用 Bootstrap 5
  bootswatch = "flatly",          # 使用 flatly 主题（简洁清爽）
  primary = "#2C7FB8",            # 主题色：蓝色系
  success = "#2CA25F",            # 成功色：绿色
  danger = "#E34A33"              # 警告色：红色
  # 移除 Google Fonts 依赖，使用系统默认字体以避免网络问题
)

# 定义应用的用户界面布局
ui <- page_sidebar(
  # 应用标题
  title = "倾向性评分匹配 (PSM) 分析工具",
  theme = app_theme,
  
  # 侧边栏：放置所有输入控件
  sidebar = sidebar(
    width = 350,
    
    # --- 数据上传区域 ---
    h4("📂 数据上传"),
    fileInput(
      "file_upload",                    # 输入 ID
      "上传数据文件",                    # 显示标签
      accept = c(                       # 接受的文件类型
        ".csv", ".tsv",                 # 文本格式
        ".xlsx", ".xls",                # Excel 格式
        ".dta",                         # Stata 格式
        ".sav", ".por"                  # SPSS 格式
      ),
      buttonLabel = "浏览...",          # 按钮文字
      placeholder = "支持 CSV/Excel/DTA/SAV"  # 占位提示文字
    ),
    
    # 分割线，区分不同功能模块
    hr(),
    
    # --- 变量选择区域（仅在数据上传后显示）---
    conditionalPanel(
      condition = "output.data_uploaded",  # 条件：数据已上传
      
      h4("📋 变量选择"),
      
      # 处理变量（二分类：0/1）选择
      selectInput(
        "treatment_var",       # 输入 ID
        "处理变量 (Treatment)", # 标签：研究中的处理/暴露变量
        choices = NULL,        # 初始为空，数据上传后动态填充
        selected = NULL
      ),
      
      # 协变量（自变量）多选
      selectInput(
        "covariate_vars",      # 输入 ID
        "协变量 (Covariates)",  # 标签：需要平衡的混杂因素
        choices = NULL,        # 初始为空，数据上传后动态填充
        selected = NULL,
        multiple = TRUE        # 允许多选
      ),
      
      hr(),
      
      # --- 匹配方法选择 ---
      h4("⚙️ 匹配设置"),
      
      # 匹配方法下拉菜单
      selectInput(
        "match_method",        # 输入 ID
        "匹配方法 (Method)",   # 标签
        choices = c(
          "最近邻匹配 (Nearest)"    = "nearest",    # 最常用的 1:1 最近邻匹配
          "精确匹配 (Exact)"         = "exact",      # 精确匹配：协变量完全相同
          "完全匹配 (Full)"          = "full",       # 完全匹配：所有处理组个体都匹配
          "最优匹配 (Optimal)"       = "optimal",    # 最优匹配：最小化全局距离
          "卡尺匹配 (Nearest + Mahalanobis)" = "nearest",  # 最近邻 + 马氏距离
          "子分类 (Subclassification)" = "subclass"   # 子分类：按倾向性评分分层
        ),
        selected = "nearest"   # 默认选择最近邻匹配
      ),
      
      # 匹配比例选择（仅最近邻匹配时显示）
      conditionalPanel(
        condition = "input.match_method == 'nearest'",
        numericInput(
          "ratio",              # 输入 ID
          "匹配比例 (Ratio)",    # 标签：每个处理组个体匹配的对照组个体数
          value = 1,            # 默认 1:1 匹配
          min = 1,              # 最小值
          max = 10,             # 最大值
          step = 1              # 步长
        )
      ),
      
      # 卡尺宽度设置（仅最近邻匹配时显示）
      conditionalPanel(
        condition = "input.match_method == 'nearest'",
        numericInput(
          "caliper",            # 输入 ID
          "卡尺宽度 (Caliper)",  # 标签：限制匹配的最大倾向性评分距离
          value = 0,            # 默认为 0（不限制）
          min = 0,              # 最小值
          max = 1,              # 最大值
          step = 0.01           # 步长
        ),
        tags$small(
          style = "color:#666; display:block; margin-top:-5px;",
          "0 表示不使用卡尺；常用值为 0.2 × SD(倾向性评分)"
        )
      ),
      
      # 子分类数量设置（仅子分类匹配时显示）
      conditionalPanel(
        condition = "input.match_method == 'subclass'",
        numericInput(
          "n_subclass",         # 输入 ID
          "子分类数量",          # 标签：将数据分为多少个子层
          value = 5,            # 默认 5 个子层
          min = 2,              # 最小值
          max = 20,             # 最大值
          step = 1              # 步长
        )
      ),
      
      hr(),
      
      # --- 附加选项 ---
      h4("🔧 附加选项"),
      
      # 倾向性评分模型类型
      selectInput(
        "ps_model",            # 输入 ID
        "倾向性评分模型",        # 标签：用于估计倾向性评分的统计模型
        choices = c(
          "逻辑回归 (Logistic)" = "glm",          # 广义线性模型（默认）
          "广义加速模型 (GBM)"  = "gbm"           # 梯度提升机（更灵活）
        ),
        selected = "glm"
      ),
      
      # 是否使用替换匹配
      checkboxInput(
        "replace",             # 输入 ID
        "允许替换匹配 (Replace)", # 标签：是否允许同一个对照组个体被多次匹配
        value = FALSE           # 默认不允许替换
      ),
      
      # 是否对倾向性评分取对数
      checkboxInput(
        "logit",               # 输入 ID
        "使用 Logit 变换的倾向性评分", # 标签
        value = FALSE           # 默认不使用
      ),
      
      hr(),
      
      # 执行匹配按钮
      actionButton(
        "run_match",           # 输入 ID
        "🚀 开始匹配",         # 按钮文字
        class = "btn-primary btn-lg w-100",  # Bootstrap 样式：大号蓝色按钮
        icon = icon("play")    # 播放图标
      ),
      
      tags$small(
        style = "display:block; margin-top:10px; color:#888;",
        "点击后请耐心等待，大数据集可能需要较长时间"
      )
    )
  ),
  
  # 主内容区域：使用导航选项卡组织不同视图
  navset_card_tab(
    
    # --- 选项卡 1：数据预览 ---
    nav_panel(
      "📊 数据预览",
      card(
        # 数据集基本信息
        card_header("数据集概况"),
        card_body(
          # 显示数据集的行列数、变量名等信息
          verbatimTextOutput("data_summary"),
          hr(),
          # 显示数据前几行的预览
          DT::dataTableOutput("data_preview")
        )
      )
    ),
    
    # --- 选项卡 2：匹配结果 ---
    nav_panel(
      "🎯 匹配结果",
      # 如果尚未执行匹配，显示提示信息
      conditionalPanel(
        condition = "!output.match_done",
        card(
          card_body(
            style = "text-align:center; padding:60px;",
            tags$h3("⏳ 尚未执行匹配分析"),
            tags$p("请在左侧侧边栏配置参数后点击「开始匹配」按钮")
          )
        )
      ),
      # 如果已完成匹配，显示结果
      conditionalPanel(
        condition = "output.match_done",
        navset_card_tab(
          # 子选项卡：匹配摘要
          nav_panel(
            "匹配摘要",
            card(
              card_header("匹配执行摘要"),
              card_body(
                # 显示匹配的基本信息（处理组/对照组匹配前后样本量等）
                verbatimTextOutput("match_summary")
              )
            )
          ),
          # 子选项卡：Love Plot
          nav_panel(
            "Love Plot",
            card(
              card_header(
                div(
                  style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:10px;",
                  div("协变量平衡性 Love Plot"),
                  div(
                    style = "display:flex; align-items:center; gap:8px;",
                    numericInput("loveplot_width", "宽(英寸)", value = 8, min = 4, max = 20, step = 1, width = "90px"),
                    numericInput("loveplot_height", "高(英寸)", value = 6, min = 3, max = 20, step = 1, width = "90px"),
                    numericInput("loveplot_dpi", "DPI", value = 300, min = 72, max = 600, step = 1, width = "80px"),
                    downloadButton("download_loveplot", "📥 导出 PDF", class = "btn-primary btn-sm")
                  )
                )
              ),
              card_body(
                tags$div(
                  style = "background:#f0f7fa; padding:10px; border-radius:5px; margin-bottom:15px;",
                  tags$strong("解读说明："),
                  tags$ul(
                    tags$li("蓝色点 = 匹配前的 SMD（Unadjusted）"),
                    tags$li("红色点 = 匹配后的 SMD（Adjusted）"),
                    tags$li("虚线 (|SMD| = 0.1)：通常认为 SMD < 0.1 表示该协变量在两组间达到了良好平衡"),
                    tags$li("越靠近左侧 (SMD 接近 0)，说明两组在该协变量上的差异越小，匹配效果越好")
                  )
                ),
                plotOutput("smd_plot", height = "600px")
              )
            )
          ),
          # 子选项卡：SMD 数值详情
          nav_panel(
            "SMD 数值",
            card(
              card_header("标准化均数差 (SMD) 详情"),
              card_body(
                DT::dataTableOutput("smd_table")
              )
            )
          ),
          # 子选项卡：倾向性评分分布
          nav_panel(
            "PS 分布",
            card(
              card_header(
                div(
                  style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:10px;",
                  div("倾向性评分分布对比"),
                  div(
                    style = "display:flex; align-items:center; gap:8px;",
                    numericInput("psplot_width", "宽(英寸)", value = 8, min = 4, max = 20, step = 1, width = "90px"),
                    numericInput("psplot_height", "高(英寸)", value = 6, min = 3, max = 20, step = 1, width = "90px"),
                    numericInput("psplot_dpi", "DPI", value = 300, min = 72, max = 600, step = 1, width = "80px"),
                    downloadButton("download_psplot", "📥 导出 PDF", class = "btn-primary btn-sm")
                  )
                )
              ),
              card_body(
                tags$div(
                  style = "background:#f0f7fa; padding:10px; border-radius:5px; margin-bottom:15px;",
                  tags$strong("解读说明："),
                  tags$ul(
                    tags$li("匹配前：处理组和对照组的倾向性评分分布可能存在较大差异"),
                    tags$li("匹配后：两组的分布应趋于重叠，说明匹配有效减少了组间差异"),
                    tags$li("如果匹配后仍有较大差异，可能需要调整匹配参数或更换匹配方法")
                  )
                ),
                plotOutput("ps_dens_plot", height = "500px")
              )
            )
          )
        )
      )
    ),
    
    # --- 选项卡 3：匹配数据 ---
    nav_panel(
      "📋 匹配数据",
      # 未匹配时显示提示
      conditionalPanel(
        condition = "!output.match_done",
        card(
          card_body(
            style = "text-align:center; padding:60px;",
            tags$h3("⏳ 尚未执行匹配分析"),
            tags$p("匹配完成后此处将显示匹配后的数据集")
          )
        )
      ),
      # 已匹配时显示结果数据
      conditionalPanel(
        condition = "output.match_done",
        card(
          card_header(
            # 标题栏包含下载按钮
            div(
              style = "display:flex; justify-content:space-between; align-items:center;",
              div("匹配后的数据集"),
              downloadButton(
                "download_matched",    # 下载按钮 ID
                "📥 导出匹配数据 (CSV)", # 按钮文字
                class = "btn-success btn-sm"  # 绿色小按钮
              )
            )
          ),
          card_body(
            # 显示匹配后数据的行数和列数信息
            textOutput("matched_data_info"),
            hr(),
            # 显示匹配后数据的可交互表格
            DT::dataTableOutput("matched_data_preview")
          )
        )
      )
    ),
    
    # --- 选项卡 4：原始未匹配数据导出 ---
    nav_panel(
      "📥 数据导出",
      card(
        card_header("导出完整数据集"),
        card_body(
          tags$p("导出原始数据和匹配后的完整数据（包含所有变量和倾向性评分）"),
          hr(),
          # 下载匹配数据（完整版，包含倾向性评分）
          downloadButton(
            "download_matched_full",
            "📥 导出匹配数据（含倾向性评分）",
            class = "btn-primary",
            style = "margin:10px;"
          ),
          # 下载匹配诊断报告（文本格式）
          downloadButton(
            "download_report",
            "📄 导出匹配诊断报告",
            class = "btn-info",
            style = "margin:10px;"
          ),
          hr(),
          # 导出说明
          tags$div(
            style = "background:#f0f7fa; padding:15px; border-radius:5px;",
            tags$h5("导出文件说明："),
            tags$ul(
              tags$li(tags$strong("匹配数据 (CSV)："),
                "仅包含成功匹配的个体，已去除未匹配的个体"),
              tags$li(tags$strong("匹配数据（含倾向性评分）："),
                "包含所有原始变量 + 倾向性评分 + 匹配权重 + 子分类信息"),
              tags$li(tags$strong("匹配诊断报告："),
                "文本格式报告，包含匹配摘要和平衡性诊断结果")
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# 第四部分：Shiny 服务端 (Server) 逻辑
# ==============================================================================

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # 响应式值：存储应用运行过程中的中间数据
  # ---------------------------------------------------------------------------
  
  # 原始上传的数据
  raw_data <- reactiveVal(NULL)
  
  # 匹配结果对象（MatchIt 的 matchit 返回值）
  match_result <- reactiveVal(NULL)
  
  # 匹配后的数据框
  matched_data <- reactiveVal(NULL)
  
  # 标记是否已完成匹配（用于控制 UI 显示）
  match_done <- reactiveVal(FALSE)
  
  # ---------------------------------------------------------------------------
  # 事件：文件上传处理
  # ---------------------------------------------------------------------------
  
  # 当用户上传文件时触发
  observeEvent(input$file_upload, {
    # 读取上传的文件
    req(input$file_upload)
    
    # 使用 tryCatch 捕获读取错误
    tryCatch({
      # 调用辅助函数读取数据
      data <- read_data_file(input$file_upload$datapath)
      
      # 将数据存储到响应式变量中
      raw_data(data)
      
      # 重置匹配状态（上传新数据后清除之前的匹配结果）
      match_done(FALSE)
      match_result(NULL)
      matched_data(NULL)
      
      # 获取所有列名
      all_cols <- names(data)
      
      # 识别数值型列（用于自动推荐处理变量和协变量）
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      
      # 自动识别二分类变量（只有 0 和 1 两个唯一值的列）
      binary_cols <- numeric_cols[sapply(numeric_cols, function(col) {
        unique_vals <- na.omit(unique(data[[col]]))
        # 检查是否恰好有两个唯一值且为 0 和 1
        length(unique_vals) == 2 && all(sort(unique_vals) == c(0, 1))
      })]
      
      # 更新处理变量的选择列表（仅显示二分类变量）
      updateSelectInput(
        session,
        "treatment_var",
        choices = if (length(binary_cols) > 0) binary_cols else all_cols,
        selected = if (length(binary_cols) > 0) binary_cols[1] else NULL
      )
      
      # 更新协变量的选择列表（排除处理变量的所有其他列）
      updateSelectInput(
        session,
        "covariate_vars",
        choices = all_cols,
        selected = NULL
      )
      
      # 显示成功提示
      showNotification(
        paste("✅ 数据加载成功！共", nrow(data), "行，", ncol(data), "列"),
        type = "message"
      )
      
    }, error = function(e) {
      # 如果读取出错，显示错误提示
      showNotification(
        paste("❌ 数据读取失败：", e$message),
        type = "error"
      )
    })
  })
  
  # ---------------------------------------------------------------------------
  # 输出：数据已上传标志（用于控制条件面板显示）
  # ---------------------------------------------------------------------------
  
  output$data_uploaded <- reactive({
    return(!is.null(raw_data()))
  })
  
  # 确保上述 reactive 输出在 Shiny 中可用
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
  
  # ---------------------------------------------------------------------------
  # 输出：数据预览
  # ---------------------------------------------------------------------------
  
  # 数据集摘要信息（行列数、变量类型等）
  output$data_summary <- renderPrint({
    req(raw_data())
    data <- raw_data()
    
    cat("════════════════════════════════════════\n")
    cat("         数据集基本信息\n")
    cat("════════════════════════════════════════\n\n")
    cat("行数（观测值）：", nrow(data), "\n")
    cat("列数（变量）：  ", ncol(data), "\n\n")
    cat("─── 变量列表 ───\n")
    
    # 遍历每个变量，显示名称和类型
    for (i in seq_along(names(data))) {
      col_name <- names(data)[i]
      col_class <- class(data[[col_name]])[1]
      n_unique <- length(unique(na.omit(data[[col_name]])))
      cat(sprintf("  [%2d] %-25s  类型: %-10s  唯一值: %d\n",
                  i, col_name, col_class, n_unique))
    }
    
    cat("\n════════════════════════════════════════\n")
  })
  
  # 数据预览表格（使用 DT 包的可交互表格）
  output$data_preview <- DT::renderDataTable({
    req(raw_data())
    data <- raw_data()
    
    # 限制显示前 1000 行以提高性能
    display_data <- head(data, 1000)
    
    DT::datatable(
      display_data,
      options = list(
        scrollX = TRUE,          # 允许水平滚动
        pageLength = 15,         # 每页显示 15 行
        searchHighlight = TRUE   # 搜索高亮
      ),
      filter = "top"             # 列顶部筛选器
    )
  })
  
  # ---------------------------------------------------------------------------
  # 事件：执行倾向性评分匹配
  # ---------------------------------------------------------------------------
  
  observeEvent(input$run_match, {
    # 验证输入：确保已上传数据
    req(raw_data())
    
    # 验证输入：确保已选择处理变量
    req(input$treatment_var, "请先选择处理变量 (Treatment)")
    
    # 验证输入：确保已选择至少一个协变量
    req(length(input$covariate_vars) > 0, "请至少选择一个协变量 (Covariates)")
    
    # 显示处理中提示
    withProgress(
      message = "正在进行倾向性评分匹配...",
      detail = "请耐心等待...",
      value = 0.3,
      {
        # 保存当前进度
        incProgress(0.1, detail = "准备数据...")
        
        # 获取原始数据
        data <- raw_data()
        
        # 确保处理变量为数值型（0/1）
        data[[input$treatment_var]] <- as.numeric(data[[input$treatment_var]])
        
        # 构建匹配公式
        # 格式：treatment ~ cov1 + cov2 + cov3 + ...
        formula_str <- paste(
          input$treatment_var,    # 处理变量
          "~",                    # 波浪号
          paste(input$covariate_vars, collapse = " + ")  # 协变量用 + 连接
        )
        match_formula <- as.formula(formula_str)
        
        incProgress(0.2, detail = "配置匹配参数...")
        
        # 构建 matchit 函数的参数列表
        # 使用 do.call 动态构建参数，因为不同方法需要不同参数
        match_args <- list(
          formula = match_formula,   # 匹配公式
          data = data,               # 数据框
          method = input$match_method # 匹配方法
        )
        
        # 根据用户选择的匹配方法添加额外参数
        if (input$match_method == "nearest") {
          # 最近邻匹配：设置匹配比例
          match_args$ratio <- input$ratio
          
          # 如果设置了卡尺宽度（> 0），则添加卡尺参数
          if (input$caliper > 0) {
            match_args$caliper <- input$caliper
          }
          
          # 是否允许替换匹配
          match_args$replace <- input$replace
        }
        
        # 子分类匹配：设置子分类数量
        if (input$match_method == "subclass") {
          match_args$subclass <- input$n_subclass
        }
        
        # 设置倾向性评分估计模型
        # 使用 estimand 参数控制倾向性评分的估计方式
        if (input$ps_model == "glm") {
          match_args$estimand <- "ATT"  # 处理组的平均处理效应
        }
        
        incProgress(0.3, detail = "执行匹配...")
        
        # 使用 tryCatch 捕获匹配过程中的错误
        tryCatch({
          # 执行 matchit 匹配
          result <- do.call(matchit, match_args)
          
          # 保存匹配结果
          match_result(result)
          
          incProgress(0.2, detail = "提取匹配数据...")
          
          # 从匹配结果中提取匹配后的数据集
          # 提取匹配后的数据（distance 参数必须为字符串，指定列名）
          matched <- match.data(result, distance = "ps_score")
          
          # 保存匹配后的数据
          matched_data(matched)
          
          # 标记匹配完成
          match_done(TRUE)
          
          incProgress(0.2, detail = "完成！")
          
          # 显示成功通知
          showNotification(
            paste("✅ 匹配完成！匹配后样本量：", nrow(matched), "行"),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          # 匹配出错时显示错误信息
          showNotification(
            paste("❌ 匹配失败：", e$message),
            type = "error",
            duration = 10
          )
        })
      }
    )
  })
  
  # ---------------------------------------------------------------------------
  # 输出：匹配完成标志
  # ---------------------------------------------------------------------------
  
  output$match_done <- reactive({
    return(match_done())
  })
  
  outputOptions(output, "match_done", suspendWhenHidden = FALSE)
  
  # ---------------------------------------------------------------------------
  # 输出：匹配摘要
  # ---------------------------------------------------------------------------
  
  output$match_summary <- renderPrint({
    req(match_result())
    result <- match_result()
    
    cat("═══════════════════════════════════════════════════\n")
    cat("            倾向性评分匹配分析摘要\n")
    cat("═══════════════════════════════════════════════════\n\n")
    
    # 使用 summary 函数显示匹配摘要
    summary(result)
    
    cat("\n═══════════════════════════════════════════════════\n")
    cat("处理变量：", input$treatment_var, "\n")
    cat("协变量数：", length(input$covariate_vars), "\n")
    cat("匹配方法：", input$match_method, "\n")
    if (input$match_method == "nearest") {
      cat("匹配比例：1:", input$ratio, "\n")
      if (input$caliper > 0) cat("卡尺宽度：", input$caliper, "\n")
    }
    cat("═══════════════════════════════════════════════════\n")
  })
  
  # ---------------------------------------------------------------------------
  # 输出：SMD 平衡性诊断图
  # ---------------------------------------------------------------------------
  
  output$smd_plot <- renderPlot({
    req(match_result())
    result <- match_result()
    
    # 使用 cobalt 包的 love.plot 绘制 Love 图
    # Love 图是 PSM 中最常用的平衡性诊断图表
    love.plot(
      result,                        # 匹配结果对象
      threshold = 0.1,               # 添加 SMD = 0.1 的参考线
      var.order = "unadjusted",      # 按匹配前的 SMD 大小排序
      title = "协变量平衡性诊断 (Love Plot)",
      subtitle = "匹配前 vs 匹配后的标准化均数差",
      colors = c("#2C7FB8", "#E34A33"),  # 蓝色=匹配前，红色=匹配后
      stars = "none"                 # 不显示星号
    )
  })
  
  # ---------------------------------------------------------------------------
  # 输出：SMD 数值表格
  # ---------------------------------------------------------------------------
  
  output$smd_table <- DT::renderDataTable({
    req(match_result(), raw_data())
    result <- match_result()
    data <- raw_data()
    
    # 使用 cobalt 包的 bal.tab 函数计算平衡统计
    bal <- bal.tab(
      result,
      un = TRUE,  # 显示匹配前的不平衡
      thresholds = c(m = 0.1),  # SMD 阈值
      abs = TRUE,  # 显示绝对值
      stats = c("mean.diffs")  # 仅显示均数差
    )
    
    # 提取 SMD 数据
    bal_df <- as.data.frame(bal$Balance)
    
    # 查找正确的列名
    before_col <- grep("Un", colnames(bal_df), value = TRUE)[1]
    after_col <- grep("Adj", colnames(bal_df), value = TRUE)[1]
    
    if (is.na(before_col) || is.na(after_col)) {
      # 如果找不到列名，使用默认值
      before_col <- "Mean Diff.Un"
      after_col <- "Mean Diff.Adj"
    }
    
    # 构建干净的 SMD 汇总数据框
    smd_df <- data.frame(
      协变量 = rownames(bal_df),
      匹配前_SMD = round(bal_df[[before_col]], 4),
      匹配后_SMD = round(bal_df[[after_col]], 4),
      row.names = NULL
    )
    
    # 计算 SMD 改善和达标状态
    smd_df$SMD_改善 <- round(smd_df$匹配前_SMD - smd_df$匹配后_SMD, 4)
    smd_df$平衡达标 <- ifelse(smd_df$匹配后_SMD < 0.1, "✅ 达标", "❌ 未达标")
    
    # 计算总体平衡性指标 - 只计算数值型协变量，跳过 distance 列
    covar_rows <- rownames(smd_df) != "distance"
    if (any(covar_rows)) {
      avg_smd_before <- mean(smd_df$匹配前_SMD[covar_rows], na.rm = TRUE)
      avg_smd_after <- mean(smd_df$匹配后_SMD[covar_rows], na.rm = TRUE)
      
      cat("匹配前平均 SMD：", round(avg_smd_before, 4), "\n")
      cat("匹配后平均 SMD：", round(avg_smd_after, 4), "\n")
      if (!is.na(avg_smd_before) && avg_smd_before > 0) {
        improvement <- (1 - avg_smd_after / avg_smd_before) * 100
        cat("SMD 改善比例：", round(improvement, 1), "%\n\n")
      }
    }
    
    # 使用 DT 渲染可交互表格
    DT::datatable(
      smd_df,
      options = list(
        pageLength = nrow(smd_df),   # 一页显示所有行
        ordering = TRUE,             # 允许排序
        dom = "t"                    # 仅显示表格，隐藏搜索和分页控件
      ),
      rownames = FALSE
    ) %>%
      # 使用条件格式：对 SMD 列进行着色
      DT::formatStyle(
        "匹配后_SMD",
        backgroundColor = DT::styleInterval(
          c(0.05, 0.1),             # 分段阈值
          c("#2CA25F80", "#FFFFB380", "#E34A3380")  # 绿(好)、黄(可接受)、红(差)
        )
      ) %>%
      DT::formatStyle(
        "平衡达标",
        backgroundColor = DT::styleEqual(
          c("✅ 达标", "❌ 未达标"),
          c("#2CA25F30", "#E34A3330")
        )
      )
  })
  
  # ---------------------------------------------------------------------------
  # 输出：倾向性评分分布图
  # ---------------------------------------------------------------------------
  
  output$ps_dens_plot <- renderPlot({
    req(match_result(), matched_data())
    result <- match_result()
    matched <- matched_data()
    
    # 获取处理变量名
    treat_var <- input$treatment_var
    
    # 匹配前：直接使用 matchit 对象中的 distance（所有样本的倾向性评分）
    # result$distance 的长度与原始数据行数一致
    ps_before <- result$distance
    treat_before <- as.numeric(raw_data()[[treat_var]])
    
    df_before <- data.frame(
      PS = ps_before,
      组别 = factor(ifelse(treat_before == 1, "处理组", "对照组"),
                     levels = c("处理组", "对照组")),
      阶段 = "匹配前"
    )
    
    # 匹配后：使用已存储的匹配数据（包含 ps_score 列）
    ps_after <- matched$ps_score
    treat_after <- as.numeric(matched[[treat_var]])
    
    df_after <- data.frame(
      PS = ps_after,
      组别 = factor(ifelse(treat_after == 1, "处理组", "对照组"),
                     levels = c("处理组", "对照组")),
      阶段 = "匹配后"
    )
    
    # 合并匹配前后的数据
    df_all <- rbind(df_before, df_after)
    
    # 使用 ggplot2 绘制核密度分布图
    ggplot(df_all, aes(x = PS, fill = 组别)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~ 阶段, ncol = 1, scales = "free_y") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold")
      ) +
      scale_fill_manual(values = c("处理组" = "#2C7FB8", "对照组" = "#E34A33")) +
      labs(
        x = "倾向性评分 (Propensity Score)",
        y = "密度 (Density)",
        fill = ""
      )
  })
  
  # ---------------------------------------------------------------------------
  # 下载处理器：导出 Love Plot 为 PDF
  # ---------------------------------------------------------------------------
  
  output$download_loveplot <- downloadHandler(
    filename = function() {
      paste0("LovePlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(match_result())
      result <- match_result()
      
      # 获取用户设置的尺寸和 DPI
      w <- input$loveplot_width
      h <- input$loveplot_height
      d <- input$loveplot_dpi
      
      # love.plot() 返回 ggplot 对象，用 ggsave 保存
      p <- love.plot(
        result,
        threshold = 0.1,
        var.order = "unadjusted",
        title = "协变量平衡性诊断 (Love Plot)",
        subtitle = "匹配前 vs 匹配后的标准化均数差",
        colors = c("#2C7FB8", "#E34A33"),
        stars = "none"
      )
      
      # 使用 ggsave 保存为 PDF
      ggsave(file, plot = p, width = w, height = h, dpi = d, device = "pdf")
    }
  )
  
  # ---------------------------------------------------------------------------
  # 下载处理器：导出 PS 分布图为 PDF
  # ---------------------------------------------------------------------------
  
  output$download_psplot <- downloadHandler(
    filename = function() {
      paste0("PS_Distribution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(match_result(), matched_data())
      result <- match_result()
      matched <- matched_data()
      treat_var <- input$treatment_var
      
      # 获取用户设置的尺寸和 DPI
      w <- input$psplot_width
      h <- input$psplot_height
      d <- input$psplot_dpi
      
      # 构建匹配前数据
      ps_before <- result$distance
      treat_before <- as.numeric(raw_data()[[treat_var]])
      df_before <- data.frame(
        PS = ps_before,
        组别 = factor(ifelse(treat_before == 1, "处理组", "对照组"),
                       levels = c("处理组", "对照组")),
        阶段 = "匹配前"
      )
      
      # 构建匹配后数据
      ps_after <- matched$ps_score
      treat_after <- as.numeric(matched[[treat_var]])
      df_after <- data.frame(
        PS = ps_after,
        组别 = factor(ifelse(treat_after == 1, "处理组", "对照组"),
                       levels = c("处理组", "对照组")),
        阶段 = "匹配后"
      )
      
      df_all <- rbind(df_before, df_after)
      
      # 绘图
      p <- ggplot(df_all, aes(x = PS, fill = 组别)) +
        geom_density(alpha = 0.5) +
        facet_wrap(~ 阶段, ncol = 1, scales = "free_y") +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "bottom",
          strip.text = element_text(face = "bold")
        ) +
        scale_fill_manual(values = c("处理组" = "#2C7FB8", "对照组" = "#E34A33")) +
        labs(
          x = "倾向性评分 (Propensity Score)",
          y = "密度 (Density)",
          fill = ""
        )
      
      # 保存为 PDF
      ggsave(file, plot = p, width = w, height = h, dpi = d, device = "pdf")
    }
  )
  
  # ---------------------------------------------------------------------------
  # 输出：匹配数据信息
  # ---------------------------------------------------------------------------
  
  output$matched_data_info <- renderText({
    req(matched_data())
    matched <- matched_data()
    paste0("匹配后数据集：共 ", nrow(matched), " 行，", ncol(matched), " 列")
  })
  
  # ---------------------------------------------------------------------------
  # 输出：匹配数据预览表格
  # ---------------------------------------------------------------------------
  
  output$matched_data_preview <- DT::renderDataTable({
    req(matched_data())
    matched <- matched_data()
    
    DT::datatable(
      matched,
      options = list(
        scrollX = TRUE,            # 水平滚动
        pageLength = 15,           # 每页 15 行
        searchHighlight = TRUE     # 搜索高亮
      ),
      filter = "top"               # 列顶部筛选
    )
  })
  
  # ---------------------------------------------------------------------------
  # 下载处理器：导出匹配后的数据（仅匹配成功的个体）
  # ---------------------------------------------------------------------------
  
  output$download_matched <- downloadHandler(
    # 文件名：包含日期时间戳，便于区分
    filename = function() {
      paste0("matched_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    # 内容：将数据写入临时 CSV 文件
    content = function(file) {
      req(matched_data())
      matched <- matched_data()
      
      # 移除匹配专用的内部列（如 .distance、.weights 等）
      # 保留用户可理解的变量
      export_data <- matched[, !grepl("^\\.", names(matched))]
      
      # 写入 CSV 文件，使用 UTF-8 编码确保中文兼容
      write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ---------------------------------------------------------------------------
  # 下载处理器：导出完整匹配数据（包含倾向性评分等内部列）
  # ---------------------------------------------------------------------------
  
  output$download_matched_full <- downloadHandler(
    filename = function() {
      paste0("matched_data_full_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(matched_data())
      matched <- matched_data()
      
      # 导出完整数据（包含所有列）
      write.csv(matched, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ---------------------------------------------------------------------------
  # 下载处理器：导出匹配诊断报告
  # ---------------------------------------------------------------------------
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("PSM_diagnostic_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      req(match_result(), matched_data(), raw_data())
      
      result <- match_result()
      data <- raw_data()
      matched <- matched_data()
      
      # 打开文件连接以写入报告
      sink(file)
      
      cat("╔══════════════════════════════════════════════════════╗\n")
      cat("║     倾向性评分匹配 (PSM) 诊断报告                   ║\n")
      cat("╚══════════════════════════════════════════════════════╝\n\n")
      
      # 报告生成时间
      cat("报告生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
      
      # 数据概况
      cat("═════════════ 数据概况 ═══════════\n")
      cat("原始数据集行数：", nrow(data), "\n")
      cat("原始数据集列数：", ncol(data), "\n")
      cat("处理变量：", input$treatment_var, "\n")
      cat("协变量数量：", length(input$covariate_vars), "\n")
      cat("协变量列表：", paste(input$covariate_vars, collapse = ", "), "\n\n")
      
      # 匹配参数
      cat("═══════════ 匹配参数 ═══════════\n")
      cat("匹配方法：", input$match_method, "\n")
      if (input$match_method == "nearest") {
        cat("匹配比例：1:", input$ratio, "\n")
        cat("卡尺宽度：", ifelse(input$caliper == 0, "无限制", input$caliper), "\n")
        cat("替换匹配：", ifelse(input$replace, "是", "否"), "\n")
      }
      if (input$match_method == "subclass") {
        cat("子分类数量：", input$n_subclass, "\n")
      }
      cat("\n")
      
      # 匹配摘要
      cat("═══════════ 匹配结果摘要 ═══════════\n")
      print(summary(result))
      cat("\n")
      
      # 平衡性诊断
      cat("═══════════ 平衡性诊断 (SMD) ═══════════\n")
      smd_before <- calculate_smd(data, input$treatment_var, input$covariate_vars)
      smd_after <- calculate_smd(matched, input$treatment_var, input$covariate_vars)
      
      cat(sprintf("%-25s  %10s  %10s  %10s  %8s\n",
                  "协变量", "匹配前SMD", "匹配后SMD", "改善量", "达标"))
      cat(paste(rep("-", 70), collapse = ""), "\n")
      
      for (i in seq_along(smd_before)) {
        cat(sprintf("%-25s  %10.4f  %10.4f  %10.4f  %8s\n",
                    names(smd_before)[i],
                    smd_before[i],
                    smd_after[i],
                    smd_before[i] - smd_after[i],
                    ifelse(smd_after[i] < 0.1, "✅", "❌")))
      }
      
      cat("\n")
      cat("匹配前平均 SMD：", round(mean(smd_before), 4), "\n")
      cat("匹配后平均 SMD：", round(mean(smd_after), 4), "\n")
      cat("SMD 改善比例：", round((1 - mean(smd_after)/mean(smd_before)) * 100, 1), "%\n")
      cat("达到平衡的协变量比例 (SMD < 0.1)：",
          round(sum(smd_after < 0.1) / length(smd_after) * 100, 1), "%\n")
      
      cat("\n")
      cat("═══════════ 结论 ═══════════\n")
      avg_improvement <- round((1 - mean(smd_after)/mean(smd_before)) * 100, 1)
      if (mean(smd_after) < 0.1) {
        cat("✅ 匹配效果良好：匹配后所有协变量的平均 SMD < 0.1，\n")
        cat("   处理组和对照组在协变量上达到了良好平衡。\n")
      } else if (mean(smd_after) < 0.2) {
        cat("⚠️ 匹配效果一般：部分协变量的 SMD 仍在 0.1-0.2 之间，\n")
        cat("   建议检查未达标的协变量并考虑调整匹配参数。\n")
      } else {
        cat("❌ 匹配效果较差：匹配后协变量的平均 SMD > 0.2，\n")
        cat("   建议更换匹配方法或增加更多相关协变量。\n")
      }
      
      cat("\n")
      cat("══════════════════════════════════════════════════════\n")
      cat("报告结束\n")
      
      # 关闭文件连接
      sink()
    }
  )
}

# ==============================================================================
# 第五部分：启动 Shiny 应用
# ==============================================================================

# 使用 shinyApp 函数将 UI 和 Server 组合为完整的 Shiny 应用
# 运行本脚本后，浏览器将自动打开应用界面
shinyApp(ui = ui, server = server)
