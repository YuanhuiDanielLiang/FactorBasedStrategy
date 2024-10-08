
#1 cleaning
data1 <- readRDS("MktCapTopCoins.rds")
head(data1)


data2 <- readRDS("Top10_cryptoStablePairs.rds")
head(data2)

str(data1)
str(data2)
sum(is.na(data1["CapMrktEstUSD"]))
sum(is.na(data2))


#新列意思：合并正确的mktcap
data1$CapMkt <- ifelse(!is.na(data1$CapMrktCurUSD), data1$CapMrktCurUSD, data1$CapMrktEstUSD)
#head(data1)
data1 <- data1[, c("asset", "time", "CapMkt")]




# 合并前，提取asset
data2$asset <- sapply(strsplit(as.character(data2$market), "-"), function(x) x[2])

merged_data <- merge(data1, data2, by = c("asset","time"))
sum(is.na(merged_data))




#2 crypto log rt
unique(merged_data$asset)   #top 10 crypto



library(dplyr)

merged_data <- merged_data %>%
  group_by(asset) %>%
  arrange(time) %>%
  mutate(log_daily_return = log(price_close / lag(price_close)))


##index rt
index_data <- merged_data %>%
  group_by(time) %>%
  mutate(total_cap = sum(CapMkt, na.rm = TRUE)) %>%   #总市值
  ungroup()

index_data <- index_data %>%
  mutate(weight = CapMkt / total_cap)      #算单个weight

index_data <- index_data %>%
  group_by(time) %>%
  summarise(index_return = sum(log_daily_return * weight, na.rm = TRUE))   #相乘加总

head(index_data)


#return plot
library(ggplot2)
ggplot(index_data, aes(x = time, y = index_return)) +
  geom_line(color = "blue") +
  labs(title = "Market-Capitalization-Weighted Index Return",
       x = "Time",
       y = "Index Return") +
  theme_minimal()


#index plot
# first row=100
index_data$index_value <- 100

#后续行的指数值
for (i in 2:nrow(index_data)) {
  index_data$index_value[i] <- index_data$index_value[i - 1] * (1 + index_data$index_return[i])
}


ggplot(index_data, aes(x = time, y = index_value)) +
  geom_line(color = "red") +
  labs(title = "Market-Capitalization-Weighted Index with time",
       x = "Time",
       y = "Index_value") +
  theme_minimal()



##### Benchmark: Riskfree and S&P500
library(lubridate)
library(dplyr)
rf <- read.csv("Rf.CSV")
sp500 <- read.csv("SP500.csv")


rf$time <- ymd(rf$time)  
sp500$time <- ymd(sp500$time)  

benchmark <- merge(rf, sp500, by = "time") %>%
  select(time,RF,sprtrn)


index_data$time <- ymd(index_data$time)  
crypo_index <- merge(index_data,benchmark,by = "time") %>%
  select(time,index_return,RF,sprtrn)

head(crypo_index)




###3 CDF

library(ggplot2)
library(tidyr)

sp_cdf <- ecdf(crypo_index$sprtrn)   ##备注：sp_cdf是通过ecdf函数生成的一个函数，接受一个数值向量并返回这些数值在累积分布中的累积概率
rf_cdf <- ecdf(crypo_index$RF)
crypto_cdf <- ecdf(crypo_index$index_return)

cdf_data <- data.frame(
  value = c(crypo_index$sprtrn, crypo_index$RF, crypo_index$index_return),
  cdf = c(sp_cdf(crypo_index$sprtrn), rf_cdf(crypo_index$RF), crypto_cdf(crypo_index$index_return)),
  variable = rep(c("S&P500", "RF", "Crypto_index"), each = length(crypo_index$sprtrn))
  )

# plot
ggplot(cdf_data, aes(x = value, y = cdf, color = variable)) +
  geom_step() +
  labs(title = "Cumulative Distribution Functions",
       x = "Return Value",
       y = "Cumulative Probability") +
  scale_color_manual(values = c("blue", "darkgreen", "orange")) +
  theme_minimal() +
  theme(legend.title = element_blank())




