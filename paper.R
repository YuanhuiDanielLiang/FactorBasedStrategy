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





#################
##AFSD & ASSD   违规区域计算,设计函数
#formula 在这里post出来

# 𝜀1 < 5.9%           𝜀2 should lower critical value of 3.2%

#AFSD
AFSD <- function(X, Y, interval = 0.001) {
  
  # 计算 X 和 Y 的经验累积分布函数
  F_H <- ecdf(X)
  F_L <- ecdf(Y)
  
  # 在 X 和 Y 的联合范围内生成非常密集的点序列
  s_range <- seq(min(c(X, Y)), max(c(X, Y)), by = interval)
  
  # 初始化分子和分母
  numerator <- 0  # 分子，积分不满足条件的区间
  denominator <- 0  # 分母，积分绝对差异
  
  # 逐点计算积分
  for (i in 2:length(s_range)) {
    s <- s_range[i]
    s_prev <- s_range[i - 1]
    
    # CDF 的差异值
    diff_HL <- F_H(s) - F_L(s)
    diff_HL_prev <- F_H(s_prev) - F_L(s_prev)
    
    # 梯形法计算当前区间的差异积分
    delta_s <- s - s_prev
    
    # 分母：计算绝对差异的积分
    abs_diff <- (abs(diff_HL) + abs(diff_HL_prev)) / 2 * delta_s
    denominator <- denominator + abs_diff
    
    # 分子：只在不满足条件时计算积分 (F_H(s) > F_L(s))
    if (diff_HL > 0) {
      num_diff <- (diff_HL + diff_HL_prev) / 2 * delta_s
      numerator <- numerator + num_diff
    }
  }
  
  # 计算比值
  ratio <- numerator / denominator
  
  return(list(
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  ))
}


#ASSD

ASSD <- function(X, Y, interval = 0.001) {
  
  # 计算 X 和 Y 的经验累积分布函数
  F_H <- ecdf(X)
  F_L <- ecdf(Y)
  
  # 在 X 和 Y 的联合范围内生成非常密集的点序列
  s_range <- seq(min(c(X, Y)), max(c(X, Y)), by = interval)
  
  # 初始化分子和分母
  numerator <- 0  # 分子，积分不满足条件的区间
  denominator <- 0  # 分母，积分绝对差异
  
  # 初始化累积积分
  integral_FX <- 0
  integral_FY <- 0
  
  # 逐点计算积分
  for (i in 2:length(s_range)) {
    s <- s_range[i]
    s_prev <- s_range[i - 1]
    
    # CDF 的差异值
    diff_HL <- F_H(s) - F_L(s)
    diff_HL_prev <- F_H(s_prev) - F_L(s_prev)
    
    # 梯形法计算当前区间的 CDF 积分
    delta_s <- s - s_prev
    integral_FX <- integral_FX + (F_H(s) + F_H(s_prev)) / 2 * delta_s
    integral_FY <- integral_FY + (F_L(s) + F_L(s_prev)) / 2 * delta_s
    
    # 分母：计算绝对差异的积分
    abs_diff <- (abs(integral_FX - integral_FY) + abs(diff_HL_prev)) / 2 * delta_s
    denominator <- denominator + abs_diff
    
    # 分子：只在不满足二阶随机占优时计算积分 (integral_FX > integral_FY)
    if (integral_FX > integral_FY) {
      num_diff <- (integral_FX - integral_FY + diff_HL_prev) / 2 * delta_s
      numerator <- numerator + num_diff
    }
  }
  
  # 计算比值
  ratio <- numerator / denominator
  
  return(list(
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  ))
}





###TEST of function
AFSD(crypo_index$sprtrn,crypo_index$index_return)
AFSD(crypo_index$RF,crypo_index$index_return)

ASSD(crypo_index$sprtrn,crypo_index$index_return)
ASSD(crypo_index$RF,crypo_index$index_return)    #check


#for index, not AFSD, but ASSD







###########Portfolio Formation 

#1.因子计算
#2. portfolio与多空：按照因子大小五等分，5组平均-1组平均日、周收益等于当日当周的组合策略收益，组合策略又有总时间的平均收益和其他统计
#3. 计算AFSD与ASSD违例，加入benchmark与marketcap index比较

#########选择前几支portfolio，regression analysis with 3 factor model



# Factor and quantile
library(reshape2)
library(slider)

price_close <- dcast(data2, time~asset, value.var = "price_close", mean) %>%
  filter(row_number() <= n()-1)     ###pivot table



CapMkt <- dcast(data1, time ~ asset, value.var = "CapMkt", mean)
CapMkt <- CapMkt[CapMkt$time >= price_close$time[1] & CapMkt$time <= price_close$time[2602],names(price_close)[1:11]]
rownames(CapMkt) <- NULL 

price_close <- price_close %>%         ###lag
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

library(lubridate)

volume <- volume %>%
  mutate(week = ceiling_date(time, "week", week_start = 1)) %>%
  group_by(week) %>%
  summarize(across(12:41, ~ last(.x, na_rm = TRUE)))

size <- CapMkt %>%
  mutate(week = ceiling_date(time, "week", week_start = 1)) %>%
  group_by(week) %>%
  summarize(across(2:31, ~ last(.x, na_rm = TRUE)))

momentum <- price_close %>%
  mutate(week = ceiling_date(time, "week", week_start = 1)) %>%
  group_by(week) %>%
  summarize(across(62:111, ~ last(.x, na_rm = TRUE)),
            across(162:211, ~ last(.x, na_rm = TRUE)))

rtns <- Volatility %>%
  mutate(week = ceiling_date(time, "week", week_start = 1)) %>%
  group_by(week) %>%
  summarize(across(12:21, ~ prod((1+.x), na.rm = TRUE)-1))

Volatility <- Volatility %>%
  mutate(week = ceiling_date(time, "week", week_start = 1)) %>%
  group_by(week) %>%
  summarize(across(22:81, ~ last(.x, na_rm = TRUE)))

col_names <- names(volume)
cols_to_sort <- col_names[grepl("_", col_names) & col_names != "time"]
sorted_cols <- cols_to_sort[order(sub(".*_", "", cols_to_sort))]
volume <- volume %>%
  select(week, all_of(sorted_cols))

col_names <- names(momentum)
cols_to_sort <- col_names[grepl("_", col_names) & col_names != "time"]
sorted_cols <- cols_to_sort[order(sub(".*_", "", cols_to_sort))]
momentum <- momentum %>%
  select(week, all_of(sorted_cols))

col_names <- names(Volatility)
cols_to_sort <- col_names[grepl("_", col_names) & col_names != "time"]
sorted_cols <- cols_to_sort[order(sub(".*_", "", cols_to_sort))]
Volatility <- Volatility %>%
  select(week, all_of(sorted_cols))

col_names <- names(size)
cols_to_sort <- col_names[grepl("_", col_names) & col_names != "time"]
sorted_cols <- cols_to_sort[order(sub(".*_", "", cols_to_sort))]
size <- size %>%
  select(names(size)[1:11], all_of(sorted_cols))

custom_ntile <- function(row, n = 5) {
  # Remove NaN values
  valid_values <- row[!is.nan(row)]
  
  # If no valid values, return the row as is
  if (length(valid_values) == 0) {
    return(rep(NA, length(row)))
  }
  
  # Apply ntile to valid values
  quantiles <- ceiling(n*(rank(valid_values)-1)/(length(valid_values)-1))
  quantiles <- replace(quantiles, quantiles==0, 1)
  
  # Create an output row with NaNs in the original positions
  result <- rep(NA, length(row))
  result[!is.nan(row)] <- quantiles
  
  return(result)
}



###Quantile
n_variables <- floor(ncol(size)/10)
size[is.na(size)] <- NaN
for(j in 0:(n_variables-1)){
  name <- names(size)[(10*j+2):(10*j+11)]
  size <- size %>%
    mutate(across(name,~ .x, .names = "{.col}_quantile"))
  for(k in 1:372){
    size[k,paste0(name,"_quantile")] <- as.list(custom_ntile(unlist(size[k,name])))
  }
}

n_variables <- floor(ncol(momentum)/10)
momentum[is.na(momentum)] <- NaN
for(j in 0:(n_variables-1)){
  name <- names(momentum)[(10*j+2):(10*j+11)]
  momentum <- momentum %>%
    mutate(across(name,~ .x, .names = "{.col}_quantile"))
  for(k in 1:372){
    momentum[k,paste0(name,"_quantile")] <- as.list(custom_ntile(unlist(momentum[k,name])))
  }
}

n_variables <- floor(ncol(volume)/10)
volume[is.na(volume)] <- NaN
for(j in 0:(n_variables-1)){
  name <- names(volume)[(10*j+2):(10*j+11)]
  volume <- volume %>%
    mutate(across(name,~ .x, .names = "{.col}_quantile"))
  for(k in 1:372){
    volume[k,paste0(name,"_quantile")] <- as.list(custom_ntile(unlist(volume[k,name])))
  }
}

n_variables <- floor(ncol(Volatility)/10)
Volatility[is.na(Volatility)] <- NaN
for(j in 0:(n_variables-1)){
  name <- names(Volatility)[(10*j+2):(10*j+11)]
  Volatility <- Volatility %>%
    mutate(across(name,~ .x, .names = "{.col}_quantile"))
  for(k in 1:372){
    Volatility[k,paste0(name,"_quantile")] <- as.list(custom_ntile(unlist(Volatility[k,name])))
  }
}





###Portfolio Building
#Size
size_port <- merge(size, rtns, by = "week")

#others
# Rename 'time' column to 'week' in CapMkt
names(CapMkt)[names(CapMkt) == "time"] <- "week"

momentum_port<- merge(momentum, rtns, by = "week")
momentum_port<- merge(momentum_port, CapMkt, by = "week")
Volatility_port<- merge(Volatility, rtns, by = "week")
Volatility_port<- merge(Volatility_port, CapMkt, by = "week")
volume_port <- merge(volume, rtns, by = "week")
volume_port <- merge(volume_port, CapMkt, by = "week")





#first, try equal-weighted
calculate_portfolio_returns <- function(df, Factor = "") {
  asset_columns <- grep("_return$", names(df), value = TRUE)
  
  # portfolio build
  portfolio_returns <- rep(0, nrow(df))
  
  for (asset in asset_columns) {
    asset_name <- sub("_return$", "", asset)
    if (Factor == "") {
      quantile_column <- paste0(asset_name, "_quantile")
    } else {
      quantile_column <- paste0(asset_name, "_", Factor, "_quantile")
    }
    # 买入quantile为5的，卖出quantile为1的;特别注意避免NA加数得NA，将NA归0
    long_positions <- df[[quantile_column]] == 5
    short_positions <- df[[quantile_column]] == 1
    long_positions[is.na(long_positions)] <- 0
    short_positions[is.na(short_positions)] <- 0
    
    portfolio_returns <- portfolio_returns + df[[asset]] * long_positions - df[[asset]] * short_positions
  }
  
  return(portfolio_returns)
}



mean(calculate_portfolio_returns(size_port))
mean(calculate_portfolio_returns(size_port, "lprc"))
mean(calculate_portfolio_returns(size_port,"maxprc"))




#####Value weighted portfolio (in paper)


value_weighted_factor_portfolio <- function(df, Factor = "") {
  asset_columns <- grep("_return$", names(df), value = TRUE)
  
  # 初始化投资组合收益
  portfolio_returns <- rep(0, nrow(df))
  
  for (asset in asset_columns) {
    asset_name <- sub("_return$", "", asset)
    
    if (Factor == "") {
      quantile_column <- paste0(asset_name, "_quantile")
    } else {
      quantile_column <- paste0(asset_name, "_", Factor, "_quantile")
    }
    
    # 使用已经存在的市值列
    market_cap_column <- asset_name
    
    if (market_cap_column %in% names(df)) {
      # 计算权重
      total_market_cap <- sum(df[[market_cap_column]], na.rm = TRUE)
      weights <- df[[market_cap_column]] / total_market_cap
      
      # 处理NA值
      long_positions <- (df[[quantile_column]] == 5) * weights
      short_positions <- (df[[quantile_column]] == 1) * weights
      
      long_positions[is.na(long_positions)] <- 0
      short_positions[is.na(short_positions)] <- 0
      
      # 计算投资组合收益
      portfolio_returns <- portfolio_returns + df[[asset]] * long_positions - df[[asset]] * short_positions
    } else {
      warning(paste("Market cap column", market_cap_column, "not found!"))
    }
  }
  
  return(portfolio_returns)
}





#Portfolio Function
#size
mean(value_weighted_factor_portfolio(size_port))
mean(value_weighted_factor_portfolio(size_port, "lprc"))
mean(value_weighted_factor_portfolio(size_port,"maxprc"))
#momentum
value_weighted_factor_portfolio(momentum_port,"rmom1")

#Volatility
value_weighted_factor_portfolio(Volatility_port,"meanabs")

#volume
value_weighted_factor_portfolio(volume_port,"volscale")




#####AFSD ASSD testing

ASSD(crypo_index$sprtrn,value_weighted_factor_portfolio(size_port))
ASSD(crypo_index$sprtrn,value_weighted_factor_portfolio(momentum_port,"rmom1"))    #check

###0.01672426 and  0.08305148 , effective ASSD!!!!!!!!!!!!!
