# 加载必要的库
library(psych)

# 设置随机种子以保证结果可复现
set.seed(123)

# 生成基础数据集
n_samples <- 300  # 样本数量
n_vars <- 16      # 变量数量

# 创建一个空的数据框
data <- data.frame(matrix(nrow=n_samples, ncol=n_vars))

# 假设我们有4个潜在因子，每个因子影响着几个变量
# 我们将为这4个因子创建一些基础分数，然后基于这些分数生成实际变量值
factors <- matrix(rnorm(n=n_samples*4, mean=4, sd=1), nrow=n_samples, ncol=4)

print(factors)
# 定义哪些变量受哪个因子的影响（简单起见，这里假设前四个变量由第一个因子控制等）
loadings <- c(1, 1, 1, 1,  # 第一个因子
              2, 2, 2, 2,  # 第二个因子
              3, 3, 3, 3,  # 第三个因子
              4, 4, 4, 4)       # 第四个因子


# 根据因子和负载生成数据
for (i in 1:n_vars) {
  # 添加一些噪声以增加真实性
  noise <- rnorm(n=n_samples, mean=0, sd=0.5)
  # 使用对应的因子值加上噪声作为最终的变量值
  data[, i] <- factors[, loadings[i]] + noise
  # data[, i] <- factors[, loadings[i]]
  # 确保所有值都在1-7之间
  data[, i] <- round(pmin(pmax(data[, i], 1), 7))
}

# 给数据列命名
names(data) <- paste0("V", 1:n_vars)

# 检查是否有缺失值
if(any(is.na(data))) {
  warning("Data contains NA values, which may cause issues with factor analysis.")
} else {
  print("No missing values found.")
}

print(data)

# 进行因子分析
fa_result <- fa(data, nfactors = 4, rotate = "varimax")

# 输出因子分析的结果
print(fa_result)

# 如果需要，还可以绘制图形
fa.diagram(fa_result)

# 将数据框导出为CSV文件
output_file <- "factor_analysis_data.csv"
write.csv(data, file = output_file, row.names = FALSE)

# 确认文件已创建
cat("数据已成功导出到", output_file, "\n")
