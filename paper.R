library(tidyverse)

#1 cleaning
data1 <- as.data.frame(readRDS("MktCapTopCoins.rds"))
head(data1)


data2 <- as.data.frame(readRDS("Top10_cryptoStablePairs.rds"))
head(data2)

distinct(data2,market)



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


# Factor 
library(reshape2)
library(slider)

price_close <- dcast(data2, time~asset, value.var = "price_close", mean) %>%
  filter(row_number() <= n()-1)



CapMkt <- dcast(data1, time ~ asset, value.var = "CapMkt", mean)
CapMkt <- CapMkt[CapMkt$time >= price_close$time[1] & CapMkt$time <= price_close$time[2602],names(price_close)[1:11]]
rownames(CapMkt) <- NULL 

price_close <- price_close %>%
  mutate(across(names(price_close)[2:11],
                list(lag1 = ~lag(.x, 7),
                     lag2 = ~lag(.x, 14),
                     lag3 = ~lag(.x, 21),
                     lag4 = ~lag(.x, 28),
                     lag8 = ~lag(.x, 56)),
                .names = "{.col}_{.fn}"))

price_close <- price_close %>%
  mutate(across(names(price_close)[2:11],
                list(mom1 = ~ .x -get(paste0(cur_column(), "_lag1")),
                     mom2 = ~ .x -get(paste0(cur_column(), "_lag2")),
                     mom3 = ~ .x -get(paste0(cur_column(), "_lag3")),
                     mom4 = ~ .x -get(paste0(cur_column(), "_lag4")),
                     mom8 = ~ .x -get(paste0(cur_column(), "_lag8"))),
                .names = "{.col}_{.fn}"))

price_close <- price_close %>%
  mutate(across(names(price_close)[2:11],
                list(std1 = ~slide_dbl(.x, sd, .before = 7, .complete = TRUE),
                     std2 = ~slide_dbl(.x, sd, .before = 14, .complete = TRUE),
                     std3 = ~slide_dbl(.x, sd, .before = 21, .complete = TRUE),
                     std4 = ~slide_dbl(.x, sd, .before = 28, .complete = TRUE),
                     std8 = ~slide_dbl(.x, sd, .before = 56, .complete = TRUE)),
                .names = "{.col}_{.fn}"))

price_close <- price_close %>%
  mutate(across(names(price_close)[2:11],
                list(rmom1 = ~ get(paste0(cur_column(), "_mom1"))/get(paste0(cur_column(), "_std1")),
                     rmom2 = ~ get(paste0(cur_column(), "_mom2"))/get(paste0(cur_column(), "_std2")),
                     rmom3 = ~ get(paste0(cur_column(), "_mom3"))/get(paste0(cur_column(), "_std3")),
                     rmom4 = ~ get(paste0(cur_column(), "_mom4"))/get(paste0(cur_column(), "_std4")),
                     rmom8 = ~ get(paste0(cur_column(), "_mom8"))/get(paste0(cur_column(), "_std8"))),
                .names = "{.col}_{.fn}"))

volume <- dcast(data2, time~asset, value.var = "volume", mean) %>%
  filter(row_number() <= n()-1)

volume <- volume %>%
  mutate(across(names(volume)[2:11],
                list(vol = ~ log(slide_dbl(.x, mean, .before = 7, .complete = TRUE))),
                .names = "{.col}_{.fn}"))

volume <- volume %>%
  mutate(across(names(volume)[2:11],
                list(volprc = ~ get(paste0(cur_column(), "_vol"))*price_close[[cur_column()]],
                     volscale = ~ get(paste0(cur_column(), "_vol"))*price_close[[cur_column()]]/CapMkt[[cur_column()]]),
                .names = "{.col}_{.fn}"))

price_max <- dcast(data2, time~asset, value.var = "price_high", mean) %>%
  filter(row_number() <= n()-1)

CapMkt <- CapMkt %>%
  mutate(across(names(CapMkt)[2:11],
                list(lprc = ~ log(price_close[[cur_column()]]),
                     maxprc = ~slide_dbl(price_max[[cur_column()]], max, .before = 7, .complete = TRUE)),
                .names = "{.col}_{.fn}"))

Volatility <- price_close[1:11]
Volatility <- Volatility %>%
  mutate(across(names(Volatility)[2:11],
                list(return = ~ .x/lag(.x,1) - 1),
                .names = "{.col}_{.fn}"))

library(moments)

Volatility <- Volatility %>%
  mutate(across(names(Volatility)[12:21],
                list(retvol = ~ slide_dbl(.x, sd, .before = 7, .complete = TRUE),
                     retskew = ~ slide_dbl(.x, skewness, .before = 7, .complete = TRUE),
                     retkurt = ~ slide_dbl(.x, kurtosis, .before = 7, .complete = TRUE),
                     maxret = ~ slide_dbl(.x, max, .before = 7, .complete = TRUE)),
                .names = "{.col}_{.fn}"))

Volatility <- Volatility %>%
  mutate(across(names(Volatility)[2:11],
                list(stdprcvol = ~ log(slide_dbl(volume[[paste0(cur_column(),"_volprc")]], sd, .before = 7, .complete = TRUE)),
                     meanabs = ~ slide_dbl(abs(.x)/volume[[paste0(cur_column(),"_volprc")]], mean, .before = 7, .complete = TRUE)),
                .names = "{.col}_{.fn}"))