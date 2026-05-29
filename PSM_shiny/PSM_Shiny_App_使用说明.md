# 倾向性评分匹配 (PSM) Shiny 应用

基于 R 语言 `MatchIt` 和 `cobalt` 包构建的交互式倾向性评分匹配分析工具。

---

## 文件清单

| 文件名 | 说明 |
|--------|------|
| `psm_shiny_app.R` | 主应用脚本（运行此文件启动应用） |
| `psm_sample_data.csv` | 示例数据集（100 条模拟临床数据，含分类变量） |
| `PSM_Shiny_App_使用说明.md` | 本文档 |

---

## 环境要求

- R >= 4.0
- 首次运行时应用会自动检测并安装缺失的依赖包

依赖包列表：

```
shiny, bslib, MatchIt, cobalt, haven, readxl,
dplyr, ggplot2, scales, DT
```

---

## 启动方式

### 方式一：RStudio（推荐）

1. 打开 RStudio
2. 打开 `psm_shiny_app.R`
3. 点击编辑器右上角 **Run App**

### 方式二：R 控制台

```r
setwd("C:/TRAE_Solo")
source("psm_shiny_app.R")
```

### 方式三：命令行

```bash
Rscript psm_shiny_app.R
```

启动后浏览器将自动打开应用界面。

---

## 示例数据说明

`psm_sample_data.csv` 包含 100 条模拟临床数据，可用于完整测试应用功能。

| 字段名 | 类型 | 说明 |
|--------|------|------|
| `id` | 整数 | 患者编号 |
| `treatment` | 二分类 (0/1) | 处理因素（1=处理组，0=对照组） |
| `age` | 连续 | 年龄（岁） |
| `gender` | 二分类 (0/1) | 性别（1=男，0=女） |
| `bmi` | 连续 | 体质指数 |
| `hypertension` | 二分类 (0/1) | 高血压（1=是，0=否） |
| `diabetes` | 二分类 (0/1) | 糖尿病（1=是，0=否） |
| `cholesterol` | 连续 | 总胆固醇 (mg/dL) |
| `systolic_bp` | 连续 | 收缩压 (mmHg) |
| `smoking` | 二分类 (0/1) | 吸烟（1=是，0=否） |
| `education` | 多分类 | 学历（高中及以下 / 大专 / 本科及以上 / 研究生及以上） |
| `income_level` | 多分类 | 收入水平（低收入 / 中等收入 / 高收入） |

> 应用支持连续变量、二分类变量和多分类变量作为协变量。多分类变量会由 MatchIt 自动创建哑变量参与匹配。

---

## 使用流程

### 第一步：上传数据

点击侧边栏「上传数据文件」，选择本地数据文件。

支持格式：CSV、TSV、Excel (.xlsx/.xls)、Stata (.dta)、SPSS (.sav/.por)

上传后系统自动识别变量类型，并推荐二分类变量作为处理变量候选。

### 第二步：选择变量

- **处理变量**：选择代表组别的二分类变量（0/1 编码）
- **协变量**：选择需要平衡的混杂因素（支持多选，可同时包含连续、二分类和多分类变量）

### 第三步：配置匹配参数

| 参数 | 说明 | 默认值 |
|------|------|--------|
| 匹配方法 | nearest / exact / full / optimal / subclass | nearest |
| 匹配比例 | 每个处理组个体匹配的对照数 | 1:1 |
| 卡尺宽度 | 限制最大倾向性评分距离（0=不限制） | 0 |
| 子分类数 | 子分类匹配的分层数 | 5 |
| PS 模型 | 逻辑回归 (glm) 或 GBM | glm |
| 允许替换 | 同一对照是否可被多次匹配 | 否 |

### 第四步：执行匹配

点击「开始匹配」按钮，等待匹配完成。

### 第五步：查看结果

匹配完成后，结果区域包含 4 个标签页：

**匹配摘要**
- 匹配前后样本量变化
- 匹配方法和参数汇总

**Love Plot**
- 协变量平衡性可视化
- 蓝色点 = 匹配前 SMD，红色点 = 匹配后 SMD
- 虚线 (|SMD| = 0.1) 为平衡性判断阈值
- 支持自定义尺寸导出 PDF

**SMD 数值**
- 各协变量匹配前后的 SMD 数值表
- 包含改善量和达标状态（SMD < 0.1 为达标）
- 表格支持排序和条件着色

**PS 分布**
- 匹配前后处理组与对照组的倾向性评分核密度分布对比
- 匹配后两组分布应趋于重叠
- 支持自定义尺寸导出 PDF

### 第六步：导出结果

**图表导出**（在 Love Plot 和 PS 分布标签页）：
- 可自定义宽度、高度（英寸）和分辨率 (DPI)
- 点击「导出 PDF」按钮下载矢量图

**数据导出**（在「数据导出」标签页）：

| 导出项 | 格式 | 内容 |
|--------|------|------|
| 匹配数据 (CSV) | CSV | 仅匹配成功的个体，去除内部列 |
| 匹配数据（含倾向性评分） | CSV | 完整数据 + PS + 权重 + 子分类 |
| 匹配诊断报告 | TXT | 匹配摘要 + SMD 诊断 + 结论 |

---

## 常见配置建议

| 场景 | 推荐配置 |
|------|----------|
| 基础 1:1 匹配 | nearest + ratio=1 + caliper=0 |
| 严格平衡要求 | nearest + ratio=1 + caliper=0.2 |
| 样本量较少时 | nearest + ratio=2 或 3 |
| 协变量较多 | subclass（5-10 个子层） |

---

## 注意事项

1. 处理变量必须是 0/1 编码的二分类变量
2. 含缺失值的行会被自动删除，建议提前处理缺失值
3. 匹配后应检查所有协变量的 SMD 是否 < 0.1
4. 大样本数据集匹配可能需要较长时间，请耐心等待
5. 建议结合专业背景知识判断匹配结果的合理性

---

## 参考文献

- Ho DE, Imai K, King G, Stuart EA. MatchIt: Nonparametric Preprocessing for Parametric Causal Inference. *Journal of Statistical Software*, 2011.
- Greifer N. cobalt: Covariate Balance Tables and Plots. R package, 2023.
- Austin PC. An Introduction to Propensity Score Methods for Reducing the Effects of Confounding in Observational Studies. *Multivariate Behavioral Research*, 2011.

---

*版本：2.0 | 日期：2026-05-28*
