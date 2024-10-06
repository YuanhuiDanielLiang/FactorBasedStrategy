
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







