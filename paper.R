library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


#1 Data Cleaning
data1 <- as.data.frame(readRDS("MktCapTopCoins.rds"))
head(data1)


data2 <- as.data.frame(readRDS("Top10_cryptoStablePairs.rds"))
head(data2)

distinct(data2,market)



str(data1)
str(data2)
sum(is.na(data1["CapMrktEstUSD"]))
sum(is.na(data2))


#new col: merge the right mktcap
data1$CapMkt <- ifelse(!is.na(data1$CapMrktCurUSD), data1$CapMrktCurUSD, data1$CapMrktEstUSD)
#head(data1)
data1 <- data1[, c("asset", "time", "CapMkt")]




# before merge, get "asset"
data2$asset <- sapply(strsplit(as.character(data2$market), "-"), function(x) x[2])

merged_data <- merge(data1, data2, by = c("asset","time"))
sum(is.na(merged_data))




#2 crypto log return
unique(merged_data$asset)   #top 10 crypto





merged_data <- merged_data %>%
  group_by(asset) %>%
  arrange(time) %>%
  mutate(log_daily_return = log(price_close / lag(price_close)))


##Crypto index return
index_data <- merged_data %>%
  group_by(time) %>%
  mutate(total_cap = sum(CapMkt, na.rm = TRUE)) %>%   #total market cap
  ungroup()

index_data <- index_data %>%
  mutate(weight = CapMkt / total_cap)      #weight calculation

index_data <- index_data %>%
  group_by(time) %>%
  summarise(index_return = sum(log_daily_return * weight, na.rm = TRUE))   #相乘加总

head(index_data)


#return plot

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



##### 2.Benchmark: Riskfree and S&P500

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



sp_cdf <- ecdf(crypo_index$sprtrn)   ##备注：sp_cdf是通过ecdf函数生成的一个函数，接受一个数值向量并返回这些数值在累积分布中的累积概率
rf_cdf <- ecdf(crypo_index$RF)      ##Note: is a function generated through the ECDF function that accepts a vector of values and returns the cumulative probability
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



#####Into function for further use
cdf_plot <- function(data1, data2, labels) {
  # Combine the two dataframes
  combined_data <- data.frame(
    value = c(data1, data2),
    variable = rep(labels, each = length(data1))
  )
  
  # Generate ECDFs for each variable
  ecdf1 <- ecdf(data1)
  ecdf2 <- ecdf(data2)
  
  # Create a data frame with the CDF values
  cdf_data <- data.frame(
    value = c(data1, data2),
    cdf = c(ecdf1(data1), ecdf2(data2)),
    variable = rep(labels, each = length(data1))
  )
  
  # Plot
  print(ggplot(cdf_data, aes(x = value, y = cdf, color = variable)) +
    geom_step() +
    labs(title = "Cumulative Distribution Functions",
         x = "Return Value",
         y = "Cumulative Probability") +
    scale_color_manual(values = c("blue", "darkgreen")) +
    theme_minimal() +
    theme(legend.title = element_blank()))
}


#cdf_plot(crypo_index$sprtrn, crypo_index$RF, c("S&P500", "RF"))







##4.AFSD & ASSD --- Violation area calculation, design function
#formula 公式在这一部分Post出来，写论文中
#𝜀1 < 5.9%           𝜀2 should lower critical value of 3.2%



#4.1 AFSD
AFSD <- function(X, Y, interval = 0.001) {
  
  F_H <- ecdf(X)
  F_L <- ecdf(Y)
  s_range <- seq(min(c(X, Y)), max(c(X, Y)), by = interval)
  
  numerator <- 0  # 分子，积分不满足条件的区间
  denominator <- 0  # 分母，积分绝对差异
  
  for (i in 2:length(s_range)) {
    s <- s_range[i]
    s_prev <- s_range[i - 1]
    
    diff_HL <- F_H(s) - F_L(s)
    diff_HL_prev <- F_H(s_prev) - F_L(s_prev)
    delta_s <- s - s_prev
  
    abs_diff <- (abs(diff_HL) + abs(diff_HL_prev)) / 2 * delta_s
    denominator <- denominator + abs_diff
    
    # 分子：只在不满足条件时计算积分 (F_H(s) > F_L(s))
    if (diff_HL > 0) {
      num_diff <- (diff_HL + diff_HL_prev) / 2 * delta_s
      numerator <- numerator + num_diff
    }
  }
  
  ratio <- numerator / denominator
  
  if(ratio>0.5){
    ratio <- 1-ratio
  }
  
  return(list(
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  ))
}




#4.2 ASSD

ASSD <- function(X, Y, interval = 0.001) {
  
  F_H <- ecdf(X)
  F_L <- ecdf(Y)
  
  s_range <- seq(min(c(X, Y)), max(c(X, Y)), by = interval)

  numerator <- 0  # 分子，积分不满足条件的区间
  denominator <- 0  # 分母，积分绝对差异
  
  integral_FX <- 0
  integral_FY <- 0
  
  for (i in 2:length(s_range)) {
    s <- s_range[i]
    s_prev <- s_range[i - 1]
    
    diff_HL <- F_H(s) - F_L(s)
    diff_HL_prev <- F_H(s_prev) - F_L(s_prev)
    
    delta_s <- s - s_prev
    integral_FX <- integral_FX + (F_H(s) + F_H(s_prev)) / 2 * delta_s
    integral_FY <- integral_FY + (F_L(s) + F_L(s_prev)) / 2 * delta_s
    
    # 分母：计算绝对差异的积分
    abs_diff <- (abs(integral_FX - integral_FY) + abs(diff_HL_prev)) / 2 * delta_s
    denominator <- denominator + abs_diff
    # 分子：只在不满足二阶随机占优时计算积分 (integral_FX > integral_FY)
    #Molecules: Calculate integrals only when second-order random dominance is not satisfied (integrat_FX>integrat_FY)
    if (integral_FX > integral_FY) {
      num_diff <- (integral_FX - integral_FY + diff_HL_prev) / 2 * delta_s
      numerator <- numerator + num_diff
    }
  }

  ratio <- numerator / denominator
  
  if(ratio>0.5){
    ratio <- 1-ratio
  }
  
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







########## 5. Portfolio Formation 

#5.1 Factor Calculation因子计算
#5.2 Portfolio and long short: Divided into five equal parts based on factor size, with an average of 5 groups -1 group. 
#5.2 portfolio与多空：按照因子大小五等分，5组平均-1组平均日、周收益等于当日当周的组合策略收益，组合策略又有总时间的平均收益和其他统计
#5.3 Utilize The function of ASSD; 计算AFSD与ASSD违例，加入benchmark与marketcap index比较




# 5.1 Factor and quantile
library(reshape2)
library(slider)
library(moments)
library(lubridate)

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





###5.2 Portfolio Building
#Size
size_port <- merge(size, lead(rtns,1), by = "week")

#others
# Rename 'time' column to 'week' in CapMkt
names(CapMkt)[names(CapMkt) == "time"] <- "week"

momentum_port<- merge(momentum, lead(rtns,1), by = "week")
momentum_port<- merge(momentum_port, CapMkt[1:11], by = "week")
Volatility_port<- merge(Volatility, lead(rtns,1), by = "week")
Volatility_port<- merge(Volatility_port, CapMkt[1:11], by = "week")
volume_port <- merge(volume, lead(rtns,1), by = "week")
volume_port <- merge(volume_port, CapMkt[1:11], by = "week")





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



#####Value weighted portfolio (that's what in paper)


value_weighted_factor_portfolio <- function(df, Factor = "",Quantile=5,holding_period=1) {
  if(Factor==""){
    quantile_factor <- paste0(names(CapMkt)[2:11],"_quantile")
  } else{
  quantile_factor <- paste0(names(CapMkt)[2:11],"_",Factor,"_quantile")
  }
  
  df1 <- ifelse(df[quantile_factor] == Quantile,1,0)
  Mkt <- df1*size_port[names(CapMkt)[2:11]]
  quant_MKt <- rowSums(Mkt, na.rm = TRUE)
  weights_port <- Mkt/quant_MKt
  
  if(holding_period==1){
  ret_columns <- grep("_return$", names(df), value = TRUE)} else{
    ret_columns <- grep(paste0("_return_",as.character(holding_period),"$"), names(df), value = TRUE)
  }
  rets <- df[ret_columns]
  portfolio_returns <- rowSums(weights_port*rets,na.rm = TRUE)
  
  return(portfolio_returns)
}


#Portfolio Function
#size
Summary_Size <- data.frame("size"=1:5,"lprc"=1:5,"maxprc"=1:5,row.names = as.character(1:5))
for(i in 1:5){
  Summary_Size[i,"size"] <- mean(value_weighted_factor_portfolio(size_port,Quantile = i),na.rm = TRUE)
  Summary_Size[i,"lprc"] <- mean(value_weighted_factor_portfolio(size_port,"lprc",Quantile = i),na.rm = TRUE)
  Summary_Size[i,"maxprc"] <- mean(value_weighted_factor_portfolio(size_port,"maxprc",Quantile = i),na.rm = TRUE)
}

Summary_Size <- as.data.frame(t(Summary_Size))
Summary_Size["5-1"] <- Summary_Size["5"] - Summary_Size["1"]
#momentum
Summary_momentum <- data.frame("mom1"=1:5,"mom2"=1:5,"mom3"=1:5,"mom4"=1:5,"mom8"=1:5,"rmom1"=1:5,"rmom2"=1:5,"rmom3"=1:5,"rmom4"=1:5,"rmom8"=1:5,row.names = as.character(1:5))
for(i in 1:5){
  for(j in 1:10){
    Summary_momentum[i,j] <- mean(value_weighted_factor_portfolio(momentum_port,names(Summary_momentum)[j],Quantile = i),na.rm = TRUE)
  }
}

Summary_momentum <- as.data.frame(t(Summary_momentum))
Summary_momentum["5-1"] <- Summary_momentum["5"] - Summary_momentum["1"]

#Volatility
Summary_Volatility <- data.frame("return_maxret"=1:5,"meanabs"=1:5,"return_retkurt"=1:5,"return_retskew"=1:5,"return_retvol"=1:5,"stdprcvol"=1:5,row.names = as.character(1:5))
for(i in 1:5){
  for(j in 1:length(names(Summary_Volatility))){
    Summary_Volatility[i,j] <- mean(value_weighted_factor_portfolio(Volatility_port,names(Summary_Volatility)[j],Quantile = i),na.rm = TRUE)
  }
}

Summary_Volatility <- as.data.frame(t(Summary_Volatility))
Summary_Volatility["5-1"] <- Summary_Volatility["5"] - Summary_Volatility["1"]

#volume
Summary_volume <- data.frame("vol"=1:5,"volprc"=1:5,"volscale"=1:5,row.names = as.character(1:5))
for(i in 1:5){
  for(j in 1:length(names(Summary_volume))){
    Summary_volume[i,j] <- mean(value_weighted_factor_portfolio(volume_port,names(Summary_volume)[j],Quantile = i),na.rm = TRUE)
  }
}

Summary_volume <- as.data.frame(t(Summary_volume))
Summary_volume["5-1"] <- Summary_volume["5"] - Summary_volume["1"]

#####5.3 AFSD ASSD testing and ploting




##Benchmark weekly
s_p500_weekly <- read.csv("S_P_500_Weekly.csv")
s_p500_weekly$Date <- as.Date(s_p500_weekly$Date, format = "%m/%d/%Y")
s_p500_weekly$Price <- as.numeric(gsub(",", "", s_p500_weekly$Price))
s_p500_weekly <- s_p500_weekly %>%
  rename(week = Date)
s_p500_weekly <- s_p500_weekly %>%
  mutate(log_return = log(Price / lag(Price)))
s_p500_weekly <- s_p500_weekly %>%
  select(week, log_return) %>%
  filter(!is.na(log_return))
# debug, correct the dates between data
s_p500_weekly <- s_p500_weekly %>%
  mutate(week = week + 1)




## AFSD ASSD testing
### Size
Size_ASD <- data.frame("AFSD_Ratio"=1:length(row.names(Summary_Size)),"ASSD_Ratio"=1:length(row.names(Summary_Size)),row.names = row.names(Summary_Size))
size_port$week <- as.Date(size_port$week, format = "%Y-%m-%d")
match_dates <- merge(s_p500_weekly, size_port[1], by="week")
for(i in 1:length(row.names(Summary_Size))){
  factors <- row.names(Summary_Size)[i]
  if(factors=="size"){
    factors <- ""
  }
  long_short_performance <- value_weighted_factor_portfolio(size_port,factors,5) - value_weighted_factor_portfolio(size_port,factors,1)
  
  Size_ASD[i,1] <- AFSD(match_dates$log_return,long_short_performance)[3]
  Size_ASD[i,2] <- ASSD(match_dates$log_return,long_short_performance)[3]
  cdf_plot(long_short_performance, match_dates$log_return, c(factors,"S&P 500"))
}

### Momentum
Momentum_ASD <- data.frame("AFSD_Ratio"=1:length(row.names(Summary_momentum)),"ASSD_Ratio"=1:length(row.names(Summary_momentum)),row.names = row.names(Summary_momentum))
for(i in 1:length(row.names(Summary_momentum))){
  factors <- row.names(Summary_momentum)[i]
  long_short_performance <- value_weighted_factor_portfolio(momentum_port,factors,5) - value_weighted_factor_portfolio(momentum_port,factors,1)
  Momentum_ASD[i,1] <- AFSD(match_dates$log_return,long_short_performance)[3]
  Momentum_ASD[i,2] <- ASSD(match_dates$log_return,long_short_performance)[3]
  cdf_plot(long_short_performance, match_dates$log_return, c(factors,"S&P 500"))
}
### Volatility
Volatility_ASD <- data.frame("AFSD_Ratio"=1:length(row.names(Summary_Volatility)),"ASSD_Ratio"=1:length(row.names(Summary_Volatility)),row.names = row.names(Summary_Volatility))
for(i in 1:length(row.names(Summary_Volatility))){
  factors <- row.names(Summary_Volatility)[i]
  long_short_performance <- value_weighted_factor_portfolio(Volatility_port,factors,5) - value_weighted_factor_portfolio(Volatility_port,factors,1)
  Volatility_ASD[i,1] <- AFSD(match_dates$log_return,long_short_performance)[3]
  Volatility_ASD[i,2] <- ASSD(match_dates$log_return,long_short_performance)[3]
  cdf_plot(long_short_performance, match_dates$log_return, c(factors,"S&P 500"))
}
### Volume
Volume_ASD <- data.frame("AFSD_Ratio"=1:length(row.names(Summary_volume)),"ASSD_Ratio"=1:length(row.names(Summary_volume)),row.names = row.names(Summary_volume))
for(i in 1:length(row.names(Summary_volume))){
  factors <- row.names(Summary_volume)[i]
  long_short_performance <- value_weighted_factor_portfolio(volume_port,factors,5) - value_weighted_factor_portfolio(volume_port,factors,1)
  Volume_ASD[i,1] <- AFSD(match_dates$log_return,long_short_performance)[3]
  Volume_ASD[i,2] <- ASSD(match_dates$log_return,long_short_performance)[3]
  cdf_plot(long_short_performance, match_dates$log_return, c(factors,"S&P 500"))
}

### For 4 weeks, 13-week, 26-week, 52-week and 78-week
match_dates <- match_dates %>%
  mutate(log_return_4 = lead(slide_dbl(log_return, sum, .before = 3, .complete = TRUE),3),
         log_return_13 = lead(slide_dbl(log_return, sum, .before = 12, .complete = TRUE),12),
         log_return_26 = lead(slide_dbl(log_return, sum, .before = 25, .complete = TRUE),25),
         log_return_52 = lead(slide_dbl(log_return, sum, .before = 51, .complete = TRUE),51),
         log_return_78 = lead(slide_dbl(log_return, sum, .before = 77, .complete = TRUE),77))

momentum_port <- momentum_port %>%
  mutate(across(names(rtns)[2:11],
                list("4" = ~ lead(slide_dbl((.x+1), prod, .before = 3, .complete = TRUE)-1,3),
                     "13" = ~ lead(slide_dbl((.x+1), prod, .before = 12, .complete = TRUE)-1,12),
                     "26" = ~ lead(slide_dbl((.x+1), prod, .before = 25, .complete = TRUE)-1,25),
                     "52" = ~ lead(slide_dbl((.x+1), prod, .before = 51, .complete = TRUE)-1,51),
                     "78" = ~ lead(slide_dbl((.x+1), prod, .before = 77, .complete = TRUE)-1,77)),
                .names = "{.col}_{.fn}"))

size_port <- size_port %>%
  mutate(across(names(rtns)[2:11],
                list("4" = ~ lead(slide_dbl((.x+1), prod, .before = 3, .complete = TRUE)-1,3),
                     "13" = ~ lead(slide_dbl((.x+1), prod, .before = 12, .complete = TRUE)-1,12),
                     "26" = ~ lead(slide_dbl((.x+1), prod, .before = 25, .complete = TRUE)-1,25),
                     "52" = ~ lead(slide_dbl((.x+1), prod, .before = 51, .complete = TRUE)-1,51),
                     "78" = ~ lead(slide_dbl((.x+1), prod, .before = 77, .complete = TRUE)-1,77)),
                .names = "{.col}_{.fn}"))

volume_port <- volume_port %>%
  mutate(across(names(rtns)[2:11],
                list("4" = ~ lead(slide_dbl((.x+1), prod, .before = 3, .complete = TRUE)-1,3),
                     "13" = ~ lead(slide_dbl((.x+1), prod, .before = 12, .complete = TRUE)-1,12),
                     "26" = ~ lead(slide_dbl((.x+1), prod, .before = 25, .complete = TRUE)-1,25),
                     "52" = ~ lead(slide_dbl((.x+1), prod, .before = 51, .complete = TRUE)-1,51),
                     "78" = ~ lead(slide_dbl((.x+1), prod, .before = 77, .complete = TRUE)-1,77)),
                .names = "{.col}_{.fn}"))

Volatility_port <- Volatility_port %>%
  mutate(across(names(rtns)[2:11],
                list("4" = ~ lead(slide_dbl((.x+1), prod, .before = 3, .complete = TRUE)-1,3),
                     "13" = ~ lead(slide_dbl((.x+1), prod, .before = 12, .complete = TRUE)-1,12),
                     "26" = ~ lead(slide_dbl((.x+1), prod, .before = 25, .complete = TRUE)-1,25),
                     "52" = ~ lead(slide_dbl((.x+1), prod, .before = 51, .complete = TRUE)-1,51),
                     "78" = ~ lead(slide_dbl((.x+1), prod, .before = 77, .complete = TRUE)-1,77)),
                .names = "{.col}_{.fn}"))

Size_ASD[c("AFSD_Ratio_4","ASSD_Ratio_4","AFSD_Ratio_13","ASSD_Ratio_13","AFSD_Ratio_26","ASSD_Ratio_26","AFSD_Ratio_52","ASSD_Ratio_52","AFSD_Ratio_78","ASSD_Ratio_78")] <- NA

Volume_ASD[c("AFSD_Ratio_4","ASSD_Ratio_4","AFSD_Ratio_13","ASSD_Ratio_13","AFSD_Ratio_26","ASSD_Ratio_26","AFSD_Ratio_52","ASSD_Ratio_52","AFSD_Ratio_78","ASSD_Ratio_78")] <- NA

Volatility_ASD[c("AFSD_Ratio_4","ASSD_Ratio_4","AFSD_Ratio_13","ASSD_Ratio_13","AFSD_Ratio_26","ASSD_Ratio_26","AFSD_Ratio_52","ASSD_Ratio_52","AFSD_Ratio_78","ASSD_Ratio_78")] <- NA

Momentum_ASD[c("AFSD_Ratio_4","ASSD_Ratio_4","AFSD_Ratio_13","ASSD_Ratio_13","AFSD_Ratio_26","ASSD_Ratio_26","AFSD_Ratio_52","ASSD_Ratio_52","AFSD_Ratio_78","ASSD_Ratio_78")] <- NA

list_period <- c(4,13,26,52,78)

for(j in 1:5){
  holding_period <- list_period[j]
  for(i in 1:length(row.names(Summary_Size))){
    factors <- row.names(Summary_Size)[i]
    if(factors=="size"){
      factors <- ""
    }
    long_short_performance <- value_weighted_factor_portfolio(size_port,factors,5,holding_period) - value_weighted_factor_portfolio(size_port,factors,1,holding_period)
    long_short_performance <- long_short_performance[-c((length(long_short_performance)+2-holding_period):length(long_short_performance))]
    sp500_rtns <- unlist(drop_na(match_dates[paste0("log_return_",as.character(holding_period))])[1])
    Size_ASD[i,(2*j+1)] <- AFSD(sp500_rtns,long_short_performance)[3]
    Size_ASD[i,(2*j+2)] <- ASSD(sp500_rtns,long_short_performance)[3]
    cdf_plot(long_short_performance, sp500_rtns, c(factors,"S&P 500"))
  }
  for(i in 1:length(row.names(Summary_momentum))){
    factors <- row.names(Summary_momentum)[i]
    long_short_performance <- value_weighted_factor_portfolio(momentum_port,factors,5,holding_period) - value_weighted_factor_portfolio(momentum_port,factors,1,holding_period)
    long_short_performance <- long_short_performance[-c((length(long_short_performance)+2-holding_period):length(long_short_performance))]
    sp500_rtns <- unlist(drop_na(match_dates[paste0("log_return_",as.character(holding_period))])[1])
    Momentum_ASD[i,(2*j+1)] <- AFSD(sp500_rtns,long_short_performance)[3]
    Momentum_ASD[i,(2*j+2)] <- ASSD(sp500_rtns,long_short_performance)[3]
    cdf_plot(long_short_performance, sp500_rtns, c(factors,"S&P 500"))
  }
  for(i in 1:length(row.names(Summary_Volatility))){
    factors <- row.names(Summary_Volatility)[i]
    long_short_performance <- value_weighted_factor_portfolio(Volatility_port,factors,5,holding_period) - value_weighted_factor_portfolio(Volatility_port,factors,1,holding_period)
    long_short_performance <- long_short_performance[-c((length(long_short_performance)+2-holding_period):length(long_short_performance))]
    sp500_rtns <- unlist(drop_na(match_dates[paste0("log_return_",as.character(holding_period))])[1])
    Volatility_ASD[i,(2*j+1)] <- AFSD(sp500_rtns,long_short_performance)[3]
    Volatility_ASD[i,(2*j+2)] <- ASSD(sp500_rtns,long_short_performance)[3]
    cdf_plot(long_short_performance, sp500_rtns, c(factors,"S&P 500"))
  }
  for(i in 1:length(row.names(Summary_volume))){
    factors <- row.names(Summary_volume)[i]
    long_short_performance <- value_weighted_factor_portfolio(volume_port,factors,5,holding_period) - value_weighted_factor_portfolio(volume_port,factors,1,holding_period)
    long_short_performance <- long_short_performance[-c((length(long_short_performance)+2-holding_period):length(long_short_performance))]
    sp500_rtns <- unlist(drop_na(match_dates[paste0("log_return_",as.character(holding_period))])[1])
    Volume_ASD[i,(2*j+1)] <- AFSD(sp500_rtns,long_short_performance)[3]
    Volume_ASD[i,(2*j+2)] <- ASSD(sp500_rtns,long_short_performance)[3]
    cdf_plot(long_short_performance, sp500_rtns, c(factors,"S&P 500"))
  }
}

### Names of Dominant Factors
for(j in 0:5){
  if(j==0){holding_period <- 1}else{
  holding_period <- list_period[j]}
  for(i in 1:length(row.names(Summary_Size))){
    factors <- row.names(Summary_Size)[i]
    if(Size_ASD[i,(2*j+1)] <0.059){
      if(Size_ASD[i,(2*j+2)]<0.032){
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD and ASSD"))
      } else{
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD but not ASSD"))
      }
    }
  }
  for(i in 1:length(row.names(Summary_momentum))){
    factors <- row.names(Summary_momentum)[i]
    if(Momentum_ASD[i,(2*j+1)] <0.059){
      if(Momentum_ASD[i,(2*j+2)]<0.032){
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD and ASSD"))
      } else{
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD but not ASSD"))
      }
    }
  }
  for(i in 1:length(row.names(Summary_Volatility))){
    factors <- row.names(Summary_Volatility)[i]
    if(Volatility_ASD[i,(2*j+1)] <0.059){
      if(Volatility_ASD[i,(2*j+2)]<0.032){
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD and ASSD"))
      } else{
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD but not ASSD"))
      }
    }
  }
  for(i in 1:length(row.names(Summary_volume))){
    factors <- row.names(Summary_volume)[i]
    if(Volume_ASD[i,(2*j+1)] <0.059){
      if(Volume_ASD[i,(2*j+2)]<0.032){
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD and ASSD"))
      } else{
        print(paste0(factors," for a holding period of ", as.character(holding_period)," weeks dominates the AFSD but not ASSD"))
      }
    }
  }
}

## Hypothesis 2 Testing

### rmom1 1 weeks
long_only_performance <- value_weighted_factor_portfolio(momentum_port,"rmom1",5)
short_only_performance <- -value_weighted_factor_portfolio(momentum_port,"rmom1",1)
long_short_performance <- long_only_performance+short_only_performance
benchmark <- match_dates$log_return
AFSD_stats <- AFSD(benchmark,long_only_performance)[3]
ASSD_stats <- ASSD(benchmark,long_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom1: long_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom1: long_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom1: long_only not donminate sp500")
}
AFSD_stats <- AFSD(benchmark,short_only_performance)[3]
ASSD_stats <- ASSD(benchmark,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom1: short_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom1: short_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom1: short_only not donminate sp500")
}
AFSD_stats <- AFSD(long_only_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_only_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom1: short_only donminate long_only AFSD and ASSD.")
  } else{
    print("rmom1: short_only donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom1: short_only not donminate long_only")
}
AFSD_stats <- AFSD(long_short_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_short_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom1: short_only donminate long_short AFSD and ASSD.")
  } else{
    print("rmom1: short_only donminate long_short AFSD but not ASSD.")
  }
} else{
  print("rmom1: short_only not donminate long_short")
}
AFSD_stats <- AFSD(long_only_performance,long_short_performance)[3]
ASSD_stats <- ASSD(long_only_performance,long_short_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom1: long_short donminate long_only AFSD and ASSD.")
  } else{
    print("rmom1: long_short donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom1: long_short not donminate long_only")
}

### rmom2 1 weeks
long_only_performance <- value_weighted_factor_portfolio(momentum_port,"rmom2",5)
short_only_performance <- -value_weighted_factor_portfolio(momentum_port,"rmom2",1)
long_short_performance <- long_only_performance+short_only_performance
benchmark <- match_dates$log_return
AFSD_stats <- AFSD(benchmark,long_only_performance)[3]
ASSD_stats <- ASSD(benchmark,long_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom2: long_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom2: long_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom2: long_only not donminate sp500")
}
AFSD_stats <- AFSD(benchmark,short_only_performance)[3]
ASSD_stats <- ASSD(benchmark,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom2: short_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom2: short_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom2: short_only not donminate sp500")
}
AFSD_stats <- AFSD(long_only_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_only_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom2: short_only donminate long_only AFSD and ASSD.")
  } else{
    print("rmom2: short_only donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom2: short_only not donminate long_only")
}
AFSD_stats <- AFSD(long_short_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_short_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom2: short_only donminate long_short AFSD and ASSD.")
  } else{
    print("rmom2: short_only donminate long_short AFSD but not ASSD.")
  }
} else{
  print("rmom2: short_only not donminate long_short")
}
AFSD_stats <- AFSD(long_only_performance,long_short_performance)[3]
ASSD_stats <- ASSD(long_only_performance,long_short_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom2: long_short donminate long_only AFSD and ASSD.")
  } else{
    print("rmom2: long_short donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom2: long_short not donminate long_only")
}


### rmom3 1 weeks
long_only_performance <- value_weighted_factor_portfolio(momentum_port,"rmom3",5)
short_only_performance <- -value_weighted_factor_portfolio(momentum_port,"rmom3",1)
long_short_performance <- long_only_performance+short_only_performance
benchmark <- match_dates$log_return
AFSD_stats <- AFSD(benchmark,long_only_performance)[3]
ASSD_stats <- ASSD(benchmark,long_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom3: long_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom3: long_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom3: long_only not donminate sp500")
}
AFSD_stats <- AFSD(benchmark,short_only_performance)[3]
ASSD_stats <- ASSD(benchmark,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom3: short_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom3: short_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom3: short_only not donminate sp500")
}
AFSD_stats <- AFSD(long_only_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_only_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom3: short_only donminate long_only AFSD and ASSD.")
  } else{
    print("rmom3: short_only donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom3: short_only not donminate long_only")
}
AFSD_stats <- AFSD(long_short_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_short_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom3: short_only donminate long_short AFSD and ASSD.")
  } else{
    print("rmom3: short_only donminate long_short AFSD but not ASSD.")
  }
} else{
  print("rmom3: short_only not donminate long_short")
}
AFSD_stats <- AFSD(long_only_performance,long_short_performance)[3]
ASSD_stats <- ASSD(long_only_performance,long_short_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom3: long_short donminate long_only AFSD and ASSD.")
  } else{
    print("rmom3: long_short donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom3: long_short not donminate long_only")
}

### rmom4 1 weeks
long_only_performance <- value_weighted_factor_portfolio(momentum_port,"rmom4",5)
short_only_performance <- -value_weighted_factor_portfolio(momentum_port,"rmom4",1)
long_short_performance <- long_only_performance+short_only_performance
benchmark <- match_dates$log_return
AFSD_stats <- AFSD(benchmark,long_only_performance)[3]
ASSD_stats <- ASSD(benchmark,long_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom4: long_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom4: long_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom4: long_only not donminate sp500")
}
AFSD_stats <- AFSD(benchmark,short_only_performance)[3]
ASSD_stats <- ASSD(benchmark,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom4: short_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom4: short_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom4: short_only not donminate sp500")
}
AFSD_stats <- AFSD(long_only_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_only_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom4: short_only donminate long_only AFSD and ASSD.")
  } else{
    print("rmom4: short_only donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom4: short_only not donminate long_only")
}
AFSD_stats <- AFSD(long_short_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_short_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom4: short_only donminate long_short AFSD and ASSD.")
  } else{
    print("rmom4: short_only donminate long_short AFSD but not ASSD.")
  }
} else{
  print("rmom4: short_only not donminate long_short")
}
AFSD_stats <- AFSD(long_only_performance,long_short_performance)[3]
ASSD_stats <- ASSD(long_only_performance,long_short_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom4: long_short donminate long_only AFSD and ASSD.")
  } else{
    print("rmom4: long_short donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom4: long_short not donminate long_only")
}

### rmom8 1 weeks
long_only_performance <- value_weighted_factor_portfolio(momentum_port,"rmom8",5)
short_only_performance <- -value_weighted_factor_portfolio(momentum_port,"rmom8",1)
long_short_performance <- long_only_performance+short_only_performance
benchmark <- match_dates$log_return
AFSD_stats <- AFSD(benchmark,long_only_performance)[3]
ASSD_stats <- ASSD(benchmark,long_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom8: long_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom8: long_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom8: long_only not donminate sp500")
}
AFSD_stats <- AFSD(benchmark,short_only_performance)[3]
ASSD_stats <- ASSD(benchmark,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom8: short_only donminate sp500 AFSD and ASSD.")
  } else{
    print("rmom8: short_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("rmom8: short_only not donminate sp500")
}
AFSD_stats <- AFSD(long_only_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_only_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom8: short_only donminate long_only AFSD and ASSD.")
  } else{
    print("rmom8: short_only donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom8: short_only not donminate long_only")
}
AFSD_stats <- AFSD(long_short_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_short_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom8: short_only donminate long_short AFSD and ASSD.")
  } else{
    print("rmom8: short_only donminate long_short AFSD but not ASSD.")
  }
} else{
  print("rmom8: short_only not donminate long_short")
}
AFSD_stats <- AFSD(long_only_performance,long_short_performance)[3]
ASSD_stats <- ASSD(long_only_performance,long_short_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("rmom8: long_short donminate long_only AFSD and ASSD.")
  } else{
    print("rmom8: long_short donminate long_only AFSD but not ASSD.")
  }
} else{
  print("rmom8: long_short not donminate long_only")
}

### size_52_weeks

long_only_performance <- value_weighted_factor_portfolio(size_port,"",5,52)
short_only_performance <- -value_weighted_factor_portfolio(size_port,"",1,52)

long_only_performance <- long_only_performance[-c((length(long_only_performance)-50):length(long_only_performance))]
short_only_performance <- short_only_performance[-c((length(short_only_performance)-50):length(short_only_performance))]

long_short_performance <- long_only_performance+short_only_performance
benchmark <- unlist(drop_na(match_dates["log_return_52"][1]))
AFSD_stats <- AFSD(benchmark,long_only_performance)[3]
ASSD_stats <- ASSD(benchmark,long_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_52_weeks: long_only donminate sp500 AFSD and ASSD.")
  } else{
    print("size_52_weeks: long_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("size_52_weeks: long_only not donminate sp500")
}
AFSD_stats <- AFSD(benchmark,short_only_performance)[3]
ASSD_stats <- ASSD(benchmark,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_52_weeks: short_only donminate sp500 AFSD and ASSD.")
  } else{
    print("size_52_weeks: short_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("size_52_weeks: short_only not donminate sp500")
}
AFSD_stats <- AFSD(long_only_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_only_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_52_weeks: short_only donminate long_only AFSD and ASSD.")
  } else{
    print("size_52_weeks: short_only donminate long_only AFSD but not ASSD.")
  }
} else{
  print("size_52_weeks: short_only not donminate long_only")
}
AFSD_stats <- AFSD(long_short_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_short_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_52_weeks: short_only donminate long_short AFSD and ASSD.")
  } else{
    print("size_52_weeks: short_only donminate long_short AFSD but not ASSD.")
  }
} else{
  print("size_52_weeks: short_only not donminate long_short")
}
AFSD_stats <- AFSD(long_only_performance,long_short_performance)[3]
ASSD_stats <- ASSD(long_only_performance,long_short_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_52_weeks: long_short donminate long_only AFSD and ASSD.")
  } else{
    print("size_52_weeks: long_short donminate long_only AFSD but not ASSD.")
  }
} else{
  print("size_52_weeks: long_short not donminate long_only")
}

### size_78_weeks

long_only_performance <- value_weighted_factor_portfolio(size_port,"",5,78)
short_only_performance <- -value_weighted_factor_portfolio(size_port,"",1,78)

long_only_performance <- long_only_performance[-c((length(long_only_performance)-76):length(long_only_performance))]
short_only_performance <- short_only_performance[-c((length(short_only_performance)-76):length(short_only_performance))]

long_short_performance <- long_only_performance+short_only_performance
benchmark <- unlist(drop_na(match_dates["log_return_78"][1]))
AFSD_stats <- AFSD(benchmark,long_only_performance)[3]
ASSD_stats <- ASSD(benchmark,long_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_78_weeks: long_only donminate sp500 AFSD and ASSD.")
  } else{
    print("size_78_weeks: long_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("size_78_weeks: long_only not donminate sp500")
}
AFSD_stats <- AFSD(benchmark,short_only_performance)[3]
ASSD_stats <- ASSD(benchmark,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_78_weeks: short_only donminate sp500 AFSD and ASSD.")
  } else{
    print("size_78_weeks: short_only donminate sp500 AFSD but not ASSD.")
  }
} else{
  print("size_78_weeks: short_only not donminate sp500")
}
AFSD_stats <- AFSD(long_only_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_only_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_78_weeks: short_only donminate long_only AFSD and ASSD.")
  } else{
    print("size_78_weeks: short_only donminate long_only AFSD but not ASSD.")
  }
} else{
  print("size_78_weeks: short_only not donminate long_only")
}
AFSD_stats <- AFSD(long_short_performance,short_only_performance)[3]
ASSD_stats <- ASSD(long_short_performance,short_only_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_78_weeks: short_only donminate long_short AFSD and ASSD.")
  } else{
    print("size_78_weeks: short_only donminate long_short AFSD but not ASSD.")
  }
} else{
  print("size_78_weeks: short_only not donminate long_short")
}
AFSD_stats <- AFSD(long_only_performance,long_short_performance)[3]
ASSD_stats <- ASSD(long_only_performance,long_short_performance)[3]
if(AFSD_stats <0.059){
  if(ASSD_stats <0.032){
    print("size_78_weeks: long_short donminate long_only AFSD and ASSD.")
  } else{
    print("size_78_weeks: long_short donminate long_only AFSD but not ASSD.")
  }
} else{
  print("size_78_weeks: long_short not donminate long_only")
}






##plot
# Apply the function and add the resulting column to size_port


size_port$vw_factor <- value_weighted_factor_portfolio(size_port)
size_port$week <- as.Date(size_port$week, format = "%Y-%m-%d")
merged_data <- merge(size_port, s_p500_weekly, by = "week")

# Use the cdf_plot function
cdf_plot(merged_data$vw_factor, merged_data$log_return, c("Size Factor", "benchmark_return"))






momentum_port$vw_factor <- value_weighted_factor_portfolio(momentum_port,"rmom1")
momentum_port$week <- as.Date(momentum_port$week, format = "%Y-%m-%d")
merged_data <- merge(momentum_port, s_p500_weekly, by = "week")

# Use the cdf_plot function
cdf_plot(merged_data$vw_factor, merged_data$log_return, c("momentum1 Factor", "benchmark_return"))




volume_port$vw_factor <- value_weighted_factor_portfolio(volume_port,"volscale")
volume_port$week <- as.Date(volume_port$week, format = "%Y-%m-%d")
merged_data <- merge(volume_port, s_p500_weekly, by = "week")


# Use the cdf_plot function
cdf_plot(merged_data$vw_factor, merged_data$log_return, c("volscale Factor", "benchmark_return"))

