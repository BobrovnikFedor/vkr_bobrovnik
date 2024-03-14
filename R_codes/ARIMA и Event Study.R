#### Пакеты ####
# install.packages(urca)
# install.packages(tseries)
# install.packages(dplyr)
# install.packages(stringr)
# install.packages(devtools)
# install.packages(TSstudio)
# install.packages(forecast)
# install.packages(lmtest)
# install.packages(tidyr)
library(urca)
library(tseries)
library(dplyr)
library(stringr)
library(devtools)
# devtools::install_github("nipfpmf/eventstudies", ref="master")
library(eventstudies)
library(TSstudio)
library(forecast)
library(lmtest)
library(tidyr)

#### Выгрузка данных ####
time <- read.csv("Downloads/Diplom/Sorted_Way/time_sorted.csv")
true_preds <- read.csv("Downloads/Diplom/Sorted_Way/true_preds.csv")
yt <- read.csv("Downloads/Diplom/Sorted_Way/diplom_train_y.csv")
yt <- yt$x
View(time)

#### Обработка датафрейма ####
time$datetime <- gsub(": datetime.datetime","",time$datetime)
time$datetime <- gsub(", tzinfo=datetime.timezone.utc),","",time$datetime)
time$datetime <- sub('.', '', time$datetime)
a_time <- data.frame(str_split_fixed(time$datetime, pattern = ", ", n = 6))
View(a_time)
df_time_pr <- data.frame(as.numeric(a_time$X1),as.numeric(a_time$X2),
                         as.numeric(a_time$X3),as.numeric(a_time$X4), 
                         as.numeric(a_time$X5), time$X)

colnames(df_time_pr) <- c('year','month','day','hour','minute', 'true_index')
View(df_time_pr)
# Перенос поздних публикаций на следующий день
for (i in 1:dim(df_time_pr)[1]){
  if (df_time_pr$hour[i] > 18 | (df_time_pr$hour[i] == 18 & 
                                 df_time_pr$minute[i] >= 50)){
    if (df_time_pr$day[i] == 31){
      df_time_pr$day[i] <-  1
      if (df_time_pr$month[i] != 12){
        df_time_pr$month[i] <- df_time_pr$month[i] + 1
      } else if (df_time_pr$month[i] == 12){
        df_time_pr$month[i] <- 1
        df_time_pr$year[i] <- df_time_pr$year[i] + 1
      }
    } else if (df_time_pr$day[i] == 30){
      if (df_time_pr$month[i] == 4 | df_time_pr$month[i] == 6 |
          df_time_pr$month[i] == 9 | df_time_pr$month[i] == 11){
        df_time_pr$day[i] <-  1
        df_time_pr$month[i] <- df_time_pr$month[i] + 1
      } else if (df_time_pr$month[i] != 4 & df_time_pr$month[i] != 6 &
                 df_time_pr$month[i] != 9 & df_time_pr$month[i] != 11){
        df_time_pr$day[i] <- df_time_pr$day[i] + 1
      }
    } else if ((df_time_pr$day[i] == 28 & df_time_pr$year[i] != 20 &
                df_time_pr$year[i] != 16 & df_time_pr$month[i] == 2)){
      df_time_pr$day[i] <- 1
      df_time_pr$month[i] <- df_time_pr$month[i] + 1
    } else if ((df_time_pr$day[i] == 28 & df_time_pr$month[i] == 2
                & (df_time_pr$year[i] == 20 | df_time_pr$year[i] == 16))){
      df_time_pr$day[i] <- df_time_pr$day[i] + 1
    } else if (df_time_pr$day[i] == 29 & df_time_pr$month[i] == 2
               & (df_time_pr$year[i] == 20 | df_time_pr$year[i] == 16)){
      df_time_pr$day[i] <- 1
      df_time_pr$month[i] <- df_time_pr$month[i] + 1
    }
  }
}
View(df_time_pr)
df_time_pr <- df_time_pr[1:dim(df_time_pr)[1]-1,]
true_preds <- as.factor(true_preds$x)
prpr <- c(as.factor(yt), true_preds)
df_time_pr$pr <- prpr

df_time_pr$year <- paste("20", as.character(df_time_pr$year), sep="")
df_time_pr_comb <- df_time_pr %>% 
  unite(col = "datetime", c("day", "month","year"), sep = ".") 
#df_time_pr_comb <- dplyr::select(df_time_pr_comb, -c('hour','minute'))
df_time_pr_comb$count_zero <- ifelse(df_time_pr_comb$pr == 0, 1, 0)
df_time_pr_comb$count_one <- ifelse(df_time_pr_comb$pr == 1, 1, 0)
df_time_pr_comb$count_two <- ifelse(df_time_pr_comb$pr == 2, 1, 0)
df_time_pr_comb$count_three <- ifelse(df_time_pr_comb$pr == 3, 1, 0)
df_time_pr_comb <- df_time_pr_comb %>%
  mutate(datetime2=as.Date(datetime, format = "%d.%m.%Y"))
df_time_pr_comb <- dplyr::select(df_time_pr_comb, - c('count_zero'))
df_time_pr_comb <- dplyr::select(df_time_pr_comb, - c('datetime'))
df_time_pr_comb$weekday <- weekdays(df_time_pr_comb$datetime2, abbreviate = T)
View(df_time_pr_comb)

#### Временные ряды ####
# Алроса ALRS
{
alrosa_index <- read.csv("~/Downloads/Diplom/Sorted_Way/alrosa_index.csv")
alrosa_pub <- df_time_pr_comb[alrosa_index$X0+1,]
alrosa_pub_gr <- alrosa_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 

alrosa_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/alrosa_pr.csv")
alrosa_pr_t <- dplyr::select(alrosa_pr, c("Дата", "Цена", "Объём"))
colnames(alrosa_pr_t) <- c('date','price','vol')
alrosa_pr_t <- alrosa_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
alrosa_pr_t$price <- as.numeric(str_replace_all(alrosa_pr_t$price, ",", "."))
alrosa_pr_t <- subset(alrosa_pr_t, select = -date)
alrosa_pr_t$vol <- str_replace_all(alrosa_pr_t$vol, "K", "0")
alrosa_pr_t$vol <- str_replace_all(alrosa_pr_t$vol, "M", "0000")
alrosa_pr_t$vol <- gsub(',', '', as.character(alrosa_pr_t$vol), perl = TRUE)
alrosa_pr_t$vol <- as.numeric(alrosa_pr_t$vol)
alrosa_pr_t$r <- c()
for (i in 1:dim(alrosa_pr_t)[1]){
  alrosa_pr_t$r[i] <- (alrosa_pr_t$price[i]-alrosa_pr_t$price[i+1])/alrosa_pr_t$price[i+1]*100
}
alrosa_pr_t$v <- c()
for (i in 1:dim(alrosa_pr_t)[1]){
  alrosa_pr_t$v[i] <- (alrosa_pr_t$vol[i]-alrosa_pr_t$vol[i+1])/alrosa_pr_t$vol[i+1]*100
}
}
alrosa_res <- left_join(alrosa_pr_t, alrosa_pub_gr, by = 'datetime2')
alrosa_res[is.na(alrosa_res)] <- 0
View(alrosa_res)
## Модель
adf.test(ts(alrosa_res$r))
plot.ts((ts(alrosa_res$r)))
acf(ts(alrosa_res$r))

adf.test(ts(alrosa_res$v))
plot.ts((ts(alrosa_res$v)))
acf(ts(alrosa_res$v))

alrosa_mod_r <- auto.arima(ts(alrosa_res$r), 
                           xreg = as.matrix(alrosa_res[,6:8]))
summary(alrosa_mod_r)
coeftest(alrosa_mod_r)

alrosa_mod_v <- auto.arima(ts(alrosa_res$v), 
                           xreg = as.matrix(alrosa_res[,6:8]))
summary(alrosa_mod_v)
coeftest(alrosa_mod_v)

# ОАК UNAC
{
oak_index <- read.csv("~/Downloads/Diplom/Sorted_Way/oak_index.csv")
oak_pub <- df_time_pr_comb[oak_index$X0+1,]
oak_pub_gr <- oak_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(oak_pub_gr)
# Котировки
oak_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/oak_pr.csv")
oak_pr_t <- dplyr::select(oak_pr, c("Дата", "Цена", "Объём"))
colnames(oak_pr_t) <- c('date','price','vol')
oak_pr_t <- oak_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
oak_pr_t$price <- as.numeric(str_replace_all(oak_pr_t$price, ",", "."))
oak_pr_t <- subset(oak_pr_t, select = -date)
oak_pr_t$vol <- str_replace_all(oak_pr_t$vol, "K", "0")
oak_pr_t$vol <- str_replace_all(oak_pr_t$vol, "M", "0000")
oak_pr_t$vol <- str_replace_all(oak_pr_t$vol, "B", "0000000")
oak_pr_t$vol <- gsub(',', '', as.character(oak_pr_t$vol), perl = TRUE)
oak_pr_t$vol <- as.numeric(oak_pr_t$vol)
oak_pr_t$r <- c()
for (i in 1:dim(oak_pr_t)[1]){
  oak_pr_t$r[i] <- (oak_pr_t$price[i]-oak_pr_t$price[i+1])/oak_pr_t$price[i+1]*100
}
oak_pr_t$v <- c()
for (i in 1:dim(oak_pr_t)[1]){
  oak_pr_t$v[i] <- (oak_pr_t$vol[i]-oak_pr_t$vol[i+1])/oak_pr_t$vol[i+1]*100
}
}
# Объединение
oak_res <- left_join(oak_pr_t, oak_pub_gr, by = 'datetime2')
oak_res[is.na(oak_res)] <- 0
View(oak_res)
## Модель
adf.test(ts(oak_res$r))
plot.ts((ts(oak_res$r)))
acf(ts(oak_res$r))

adf.test(ts(oak_res$v))
plot.ts((ts(oak_res$v)))
acf(ts(oak_res$v))

oak_mod_r <- auto.arima(ts(oak_res$r), 
                        xreg = as.matrix(oak_res[,6:8]),
                        d=0)
summary(oak_mod_r)
coeftest(oak_mod_r)

oak_mod_v <- auto.arima(ts(oak_res$v), 
                        xreg = as.matrix(oak_res[,6:8]))
summary(oak_mod_v)
coeftest(oak_mod_v)

# Яндекс YNDX
{
yndx_index <- read.csv("~/Downloads/Diplom/Sorted_Way/yndx_index.csv")
yndx_pub <- df_time_pr_comb[yndx_index$X0+1,]
yndx_pub_gr <- yndx_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(yndx_pub_gr) 
# Котировки
yndx_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/yndx_pr.csv")
yndx_pr_t <- dplyr::select(yndx_pr, c("Дата", "Цена", "Объём"))
colnames(yndx_pr_t) <- c('date','price','vol')
yndx_pr_t <- yndx_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
yndx_pr_t$price <- gsub('\\.|', '', as.character(yndx_pr_t$price), perl = TRUE)
yndx_pr_t$price <- as.numeric(str_replace_all(yndx_pr_t$price, ",", "."))
yndx_pr_t <- subset(yndx_pr_t, select = -date)
yndx_pr_t$r <- c()
for (i in 1:dim(yndx_pr_t)[1]){
  yndx_pr_t$r[i] <- (yndx_pr_t$price[i]-yndx_pr_t$price[i+1])/yndx_pr_t$price[i+1]*100
}
yndx_pr_t$vol <- str_replace_all(yndx_pr_t$vol, "K", "0")
yndx_pr_t$vol <- str_replace_all(yndx_pr_t$vol, "M", "0000")
yndx_pr_t$vol <- str_replace_all(yndx_pr_t$vol, "B", "0000000")
yndx_pr_t$vol <- gsub(',', '', as.character(yndx_pr_t$vol), perl = TRUE)
yndx_pr_t$vol <- as.numeric(yndx_pr_t$vol)
yndx_pr_t$v <- c()
for (i in 1:dim(yndx_pr_t)[1]){
  yndx_pr_t$v[i] <- (yndx_pr_t$vol[i]-yndx_pr_t$vol[i+1])/yndx_pr_t$vol[i+1]*100
}
# Объединение
yndx_res <- left_join(yndx_pr_t, yndx_pub_gr, by = 'datetime2')
yndx_res[is.na(yndx_res)] <- 0
View(yndx_res)
}
## Модель
adf.test(ts(yndx_res$r))
plot.ts((ts(yndx_res$r)))
acf(ts(yndx_res$r))

adf.test(ts(yndx_res$v))
plot.ts((ts(yndx_res$v)))
acf(ts(yndx_res$v))

yndx_mod_r <- auto.arima(ts(yndx_res$r), 
                         xreg = as.matrix(yndx_res[,6:8]))
summary(yndx_mod_r)
coeftest(yndx_mod_r)

yndx_mod_v <- auto.arima(ts(yndx_res$v), 
                         xreg = as.matrix(yndx_res[,6:8]))
summary(yndx_mod_v)
coeftest(yndx_mod_v)

# Самолет SMLT 
{
smlt_index <- read.csv("~/Downloads/Diplom/Sorted_Way/smlt_index.csv")
smlt_pub <- df_time_pr_comb[smlt_index$X0+1,]
smlt_pub_gr <- smlt_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(smlt_pub_gr) 
# Котировки
smlt_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/smlt_pr.csv")
smlt_pr_t <- dplyr::select(smlt_pr, c("Дата", "Цена", "Объём"))
colnames(smlt_pr_t) <- c('date','price','vol')
smlt_pr_t <- smlt_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
smlt_pr_t$price <- gsub('\\.|', '', as.character(smlt_pr_t$price), perl = TRUE)
smlt_pr_t$price <- as.numeric(str_replace_all(smlt_pr_t$price, ",", "."))
smlt_pr_t <- subset(smlt_pr_t, select = -date)
smlt_pr_t$r <- c()
for (i in 1:dim(smlt_pr_t)[1]){
  smlt_pr_t$r[i] <- (smlt_pr_t$price[i]-smlt_pr_t$price[i+1])/smlt_pr_t$price[i+1]*100
}
smlt_pr_t$vol <- str_replace_all(smlt_pr_t$vol, "K", "0")
smlt_pr_t$vol <- str_replace_all(smlt_pr_t$vol, "M", "0000")
smlt_pr_t$vol <- str_replace_all(smlt_pr_t$vol, "B", "0000000")
smlt_pr_t$vol <- gsub(',', '', as.character(smlt_pr_t$vol), perl = TRUE)
smlt_pr_t$vol <- as.numeric(smlt_pr_t$vol)
smlt_pr_t$v <- c()
for (i in 1:dim(smlt_pr_t)[1]){
  smlt_pr_t$v[i] <- (smlt_pr_t$vol[i]-smlt_pr_t$vol[i+1])/smlt_pr_t$vol[i+1]*100
}
}
# Объединение
smlt_res <- left_join(smlt_pr_t, smlt_pub_gr, by = 'datetime2')
smlt_res[is.na(smlt_res)] <- 0
View(smlt_res)
## Модель
adf.test(ts(smlt_res$r))
plot.ts((ts(smlt_res$r)))
acf(ts(smlt_res$r))

adf.test(ts(smlt_res$v))
plot.ts((ts(smlt_res$v)))
acf(ts(smlt_res$v))

smlt_mod_r <- auto.arima(ts(smlt_res$r), 
                         xreg = as.matrix(smlt_res[,6:8]),
                         d=0)
summary(smlt_mod_r)
coeftest(smlt_mod_r)

smlt_mod_v <- auto.arima(ts(smlt_res$v), 
                         xreg = as.matrix(smlt_res[,6:8]))
summary(smlt_mod_v)
coeftest(smlt_mod_v)

# Лензолото LENZ 
{
lenz_index <- read.csv("~/Downloads/Diplom/Sorted_Way/lenz_index.csv")
lenz_pub <- df_time_pr_comb[lenz_index$X0+1,]
lenz_pub_gr <- lenz_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(lenz_pub_gr) 
# Котировки
lenz_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/lenz_pr.csv")
lenz_pr_t <- dplyr::select(lenz_pr, c("Дата", "Цена", "Объём"))
colnames(lenz_pr_t) <- c('date','price','vol')
lenz_pr_t <- lenz_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
lenz_pr_t$price <- gsub('\\.|', '', as.character(lenz_pr_t$price), perl = TRUE)
lenz_pr_t$price <- as.numeric(str_replace_all(lenz_pr_t$price, ",", "."))
lenz_pr_t <- subset(lenz_pr_t, select = -date)
lenz_pr_t$r <- c()
for (i in 1:dim(lenz_pr_t)[1]){
  lenz_pr_t$r[i] <- (lenz_pr_t$price[i]-lenz_pr_t$price[i+1])/lenz_pr_t$price[i+1]*100
}
lenz_pr_t$vol <- str_replace_all(lenz_pr_t$vol, "K", "0")
lenz_pr_t$vol <- str_replace_all(lenz_pr_t$vol, "M", "0000")
lenz_pr_t$vol <- str_replace_all(lenz_pr_t$vol, "B", "0000000")
lenz_pr_t$vol <- gsub(',', '', as.character(lenz_pr_t$vol), perl = TRUE)
lenz_pr_t$vol <- as.numeric(lenz_pr_t$vol)
lenz_pr_t$v <- c()
for (i in 1:dim(lenz_pr_t)[1]){
  lenz_pr_t$v[i] <- (lenz_pr_t$vol[i]-lenz_pr_t$vol[i+1])/lenz_pr_t$vol[i+1]*100
}
}
# Объединение
lenz_res <- left_join(lenz_pr_t, lenz_pub_gr, by = 'datetime2')
lenz_res[is.na(lenz_res)] <- 0
View(lenz_res)
## Модель
adf.test(ts(lenz_res$r))
plot.ts((ts(lenz_res$r)))
acf(ts(lenz_res$r))

adf.test(ts(lenz_res$v))
plot.ts((ts(lenz_res$v)))
acf(ts(lenz_res$v))

lenz_mod_r <- auto.arima(ts(lenz_res$r), 
                         xreg = as.matrix(lenz_res[,6:7]))
summary(lenz_mod_r)
coeftest(lenz_mod_r)

lenz_mod_v <- auto.arima(ts(lenz_res$v), 
                         xreg = as.matrix(lenz_res[,6]))
summary(lenz_mod_v)
coeftest(lenz_mod_v)

# Сбер SBER
{
sber_index <- read.csv("~/Downloads/Diplom/Sorted_Way/sber_index.csv")
sber_pub <- df_time_pr_comb[sber_index$X0+1,]
sber_pub_gr <- sber_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(sber_pub_gr) 
# Котировки
sber_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/sber_pr.csv")
sber_pr_t <- dplyr::select(sber_pr, c("Дата", "Цена", "Объём"))
colnames(sber_pr_t) <- c('date','price','vol')
sber_pr_t <- sber_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
sber_pr_t$price <- gsub('\\.|', '', as.character(sber_pr_t$price), perl = TRUE)
sber_pr_t$price <- as.numeric(str_replace_all(sber_pr_t$price, ",", "."))
sber_pr_t <- subset(sber_pr_t, select = -date)
sber_pr_t$r <- c()
for (i in 1:dim(sber_pr_t)[1]){
  sber_pr_t$r[i] <- (sber_pr_t$price[i]-sber_pr_t$price[i+1])/sber_pr_t$price[i+1]*100
}
sber_pr_t$vol <- str_replace_all(sber_pr_t$vol, "K", "0")
sber_pr_t$vol <- str_replace_all(sber_pr_t$vol, "M", "0000")
sber_pr_t$vol <- str_replace_all(sber_pr_t$vol, "B", "0000000")
sber_pr_t$vol <- gsub(',', '', as.character(sber_pr_t$vol), perl = TRUE)
sber_pr_t$vol <- as.numeric(sber_pr_t$vol)
sber_pr_t$v <- c()
for (i in 1:dim(sber_pr_t)[1]){
  sber_pr_t$v[i] <- (sber_pr_t$vol[i]-sber_pr_t$vol[i+1])/sber_pr_t$vol[i+1]*100
}
}
# Объединение
sber_res <- left_join(sber_pr_t, sber_pub_gr, by = 'datetime2')
sber_res[is.na(sber_res)] <- 0
View(sber_res)
## Модель
adf.test(ts(sber_res$r))
plot.ts((ts(sber_res$r)))
acf(ts(sber_res$r))

adf.test(ts(sber_res$v))
plot.ts((ts(sber_res$v)))
acf(ts(sber_res$v))

sber_mod_r <- auto.arima(ts(sber_res$r), 
                         xreg = as.matrix(sber_res[,6:8]))
summary(sber_mod_r)
coeftest(sber_mod_r)

sber_mod_v <- auto.arima(ts(sber_res$v), 
                         xreg = as.matrix(sber_res[,6:8]))
summary(sber_mod_v)
coeftest(sber_mod_v)

# ОЗОН OZON 
{
ozon_index <- read.csv("~/Downloads/Diplom/Sorted_Way/ozon_index.csv")
ozon_pub <- df_time_pr_comb[ozon_index$X0+1,]
ozon_pub_gr <- ozon_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(ozon_pub_gr) 
# Котировки
ozon_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/ozon_pr.csv")
ozon_pr_t <- dplyr::select(ozon_pr, c("Дата", "Цена", "Объём"))
colnames(ozon_pr_t) <- c('date','price','vol')
ozon_pr_t <- ozon_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
ozon_pr_t$price <- gsub('\\.|', '', as.character(ozon_pr_t$price), perl = TRUE)
ozon_pr_t$price <- as.numeric(str_replace_all(ozon_pr_t$price, ",", "."))
ozon_pr_t <- subset(ozon_pr_t, select = -date)
ozon_pr_t$r <- c()
for (i in 1:dim(ozon_pr_t)[1]){
  ozon_pr_t$r[i] <- (ozon_pr_t$price[i]-ozon_pr_t$price[i+1])/ozon_pr_t$price[i+1]*100
}
ozon_pr_t$vol <- str_replace_all(ozon_pr_t$vol, "K", "0")
ozon_pr_t$vol <- str_replace_all(ozon_pr_t$vol, "M", "0000")
ozon_pr_t$vol <- str_replace_all(ozon_pr_t$vol, "B", "0000000")
ozon_pr_t$vol <- gsub(',', '', as.character(ozon_pr_t$vol), perl = TRUE)
ozon_pr_t$vol <- as.numeric(ozon_pr_t$vol)
ozon_pr_t$v <- c()
for (i in 1:dim(ozon_pr_t)[1]){
  ozon_pr_t$v[i] <- (ozon_pr_t$vol[i]-ozon_pr_t$vol[i+1])/ozon_pr_t$vol[i+1]*100
}
}
# Объединение
ozon_res <- left_join(ozon_pr_t, ozon_pub_gr, by = 'datetime2')
ozon_res[is.na(ozon_res)] <- 0
View(ozon_res)


## Модель
adf.test(ts(ozon_res$r))
plot.ts((ts(ozon_res$r)))
acf(ts(ozon_res$r))

adf.test(ts(ozon_res$v))
plot.ts((ts(ozon_res$v)))
acf(ts(ozon_res$v))

ozon_mod_r <- auto.arima(ts(ozon_res$r), 
                         xreg = as.matrix(ozon_res[,6:8]))
summary(ozon_mod_r)
coeftest(ozon_mod_r)

ozon_mod_v <- auto.arima(ts(ozon_res$v), 
                         xreg = as.matrix(ozon_res[,6:8]))
summary(ozon_mod_v)
coeftest(ozon_mod_v)

# Циан CIAN 
{
cian_index <- read.csv("~/Downloads/Diplom/Sorted_Way/cian_index.csv")
cian_pub <- df_time_pr_comb[cian_index$X0+1,]
cian_pub_gr <- cian_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(cian_pub_gr) 
# Котировки
cian_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/cian_pr.csv")
cian_pr_t <- dplyr::select(cian_pr, c("Дата", "Цена", "Объём"))
colnames(cian_pr_t) <- c('date','price','vol')
cian_pr_t <- cian_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
cian_pr_t$price <- gsub('\\.|', '', as.character(cian_pr_t$price), perl = TRUE)
cian_pr_t$price <- as.numeric(str_replace_all(cian_pr_t$price, ",", "."))
cian_pr_t <- subset(cian_pr_t, select = -date)
cian_pr_t$r <- c()
for (i in 1:dim(cian_pr_t)[1]){
  cian_pr_t$r[i] <- (cian_pr_t$price[i]-cian_pr_t$price[i+1])/cian_pr_t$price[i+1]*100
}
cian_pr_t$vol <- str_replace_all(cian_pr_t$vol, "K", "0")
cian_pr_t$vol <- str_replace_all(cian_pr_t$vol, "M", "0000")
cian_pr_t$vol <- str_replace_all(cian_pr_t$vol, "B", "0000000")
cian_pr_t$vol <- gsub(',', '', as.character(cian_pr_t$vol), perl = TRUE)
cian_pr_t$vol <- as.numeric(cian_pr_t$vol)
cian_pr_t$v <- c()
for (i in 1:dim(cian_pr_t)[1]){
  cian_pr_t$v[i] <- (cian_pr_t$vol[i]-cian_pr_t$vol[i+1])/cian_pr_t$vol[i+1]*100
}
}
# Объединение
cian_res <- left_join(cian_pr_t, cian_pub_gr, by = 'datetime2')
cian_res[is.na(cian_res)] <- 0
View(cian_res)

## Модель
adf.test(ts(cian_res$r))
plot.ts((ts(cian_res$r)))
acf(ts(cian_res$r))

adf.test(ts(cian_res$v))
plot.ts((ts(cian_res$v)))
acf(ts(cian_res$v))

cian_mod_r <- auto.arima(ts(cian_res$r), 
                         xreg = as.matrix(cian_res[,6:8]))
summary(cian_mod_r)
coeftest(cian_mod_r)

cian_mod_v <- auto.arima(ts(cian_res$v), 
                         xreg = as.matrix(cian_res[,6:7]))
summary(cian_mod_v)
coeftest(cian_mod_v)

# hhru HHRU
{
hhru_index <- read.csv("~/Downloads/Diplom/Sorted_Way/hhru_index.csv")
hhru_pub <- df_time_pr_comb[hhru_index$X0+1,]
hhru_pub_gr <- hhru_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(hhru_pub_gr) 
# Котировки
hhru_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/hhru_pr.csv")
hhru_pr_t <- dplyr::select(hhru_pr, c("Дата", "Цена", "Объём"))
colnames(hhru_pr_t) <- c('date','price','vol')
hhru_pr_t <- hhru_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
hhru_pr_t$price <- gsub('\\.|', '', as.character(hhru_pr_t$price), perl = TRUE)
hhru_pr_t$price <- as.numeric(str_replace_all(hhru_pr_t$price, ",", "."))
hhru_pr_t <- subset(hhru_pr_t, select = -date)
hhru_pr_t$r <- c()
for (i in 1:dim(hhru_pr_t)[1]){
  hhru_pr_t$r[i] <- (hhru_pr_t$price[i]-hhru_pr_t$price[i+1])/hhru_pr_t$price[i+1]*100
}
hhru_pr_t$vol <- str_replace_all(hhru_pr_t$vol, "K", "0")
hhru_pr_t$vol <- str_replace_all(hhru_pr_t$vol, "M", "0000")
hhru_pr_t$vol <- str_replace_all(hhru_pr_t$vol, "B", "0000000")
hhru_pr_t$vol <- gsub(',', '', as.character(hhru_pr_t$vol), perl = TRUE)
hhru_pr_t$vol <- as.numeric(hhru_pr_t$vol)
hhru_pr_t$v <- c()
for (i in 1:dim(hhru_pr_t)[1]){
  hhru_pr_t$v[i] <- (hhru_pr_t$vol[i]-hhru_pr_t$vol[i+1])/hhru_pr_t$vol[i+1]*100
}
}
# Объединение
hhru_res <- left_join(hhru_pr_t, hhru_pub_gr, by = 'datetime2')
hhru_res[is.na(hhru_res)] <- 0
View(hhru_res)

## Модель
adf.test(ts(hhru_res$r))
plot.ts((ts(hhru_res$r)))
acf(ts(hhru_res$r))

adf.test(ts(hhru_res$v))
plot.ts((ts(hhru_res$v)))
acf(ts(hhru_res$v))

hhru_mod_r <- auto.arima(ts(hhru_res$r), 
                         xreg = as.matrix(hhru_res[,6:8]))
summary(hhru_mod_r)
coeftest(hhru_mod_r)

hhru_mod_v <- auto.arima(ts(hhru_res$v), 
                         xreg = as.matrix(hhru_res[,6:7]))
summary(hhru_mod_v)
coeftest(hhru_mod_v)

# Лукойл LKOH 
{
luk_index <- read.csv("~/Downloads/Diplom/Sorted_Way/luk_index.csv")
luk_pub <- df_time_pr_comb[luk_index$X0+1,]
luk_pub_gr <- luk_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(luk_pub_gr) 
# Котировки
luk_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/luk_pr.csv")
luk_pr_t <- dplyr::select(luk_pr, c("Дата", "Цена", "Объём"))
colnames(luk_pr_t) <- c('date','price','vol')
luk_pr_t <- luk_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
luk_pr_t$price <- gsub('\\.|', '', as.character(luk_pr_t$price), perl = TRUE)
luk_pr_t$price <- as.numeric(str_replace_all(luk_pr_t$price, ",", "."))
luk_pr_t <- subset(luk_pr_t, select = -date)
luk_pr_t$r <- c()
for (i in 1:dim(luk_pr_t)[1]){
  luk_pr_t$r[i] <- (luk_pr_t$price[i]-luk_pr_t$price[i+1])/luk_pr_t$price[i+1]*100
}
luk_pr_t$vol <- str_replace_all(luk_pr_t$vol, "K", "0")
luk_pr_t$vol <- str_replace_all(luk_pr_t$vol, "M", "0000")
luk_pr_t$vol <- str_replace_all(luk_pr_t$vol, "B", "0000000")
luk_pr_t$vol <- gsub(',', '', as.character(luk_pr_t$vol), perl = TRUE)
luk_pr_t$vol <- as.numeric(luk_pr_t$vol)
luk_pr_t$v <- c()
for (i in 1:dim(luk_pr_t)[1]){
  luk_pr_t$v[i] <- (luk_pr_t$vol[i]-luk_pr_t$vol[i+1])/luk_pr_t$vol[i+1]*100
}
}
# Объединение
luk_res <- left_join(luk_pr_t, luk_pub_gr, by = 'datetime2')
luk_res[is.na(luk_res)] <- 0
View(luk_res)

## Модель
adf.test(ts(luk_res$r))
plot.ts((ts(luk_res$r)))
acf(ts(luk_res$r))

adf.test(ts(luk_res$v))
plot.ts((ts(luk_res$v)))
acf(ts(luk_res$v))

luk_mod_r <- auto.arima(ts(luk_res$r), 
                        xreg = as.matrix(luk_res[,6:8]))
summary(luk_mod_r)
coeftest(luk_mod_r)

luk_mod_v <- auto.arima(ts(luk_res$v), 
                        xreg = as.matrix(luk_res[,6:8]))
summary(luk_mod_v)
coeftest(luk_mod_v)


# Роснефть ROSN
{
rosn_index <- read.csv("~/Downloads/Diplom/Sorted_Way/rosn_index.csv")
rosn_pub <- df_time_pr_comb[rosn_index$X0+1,]
rosn_pub_gr <- rosn_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(rosn_pub_gr) 
# Котировки
rosn_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/rosn_pr.csv")
rosn_pr_t <- dplyr::select(rosn_pr, c("Дата", "Цена", "Объём"))
colnames(rosn_pr_t) <- c('date','price','vol')
rosn_pr_t <- rosn_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
rosn_pr_t$price <- gsub('\\.|', '', as.character(rosn_pr_t$price), perl = TRUE)
rosn_pr_t$price <- as.numeric(str_replace_all(rosn_pr_t$price, ",", "."))
rosn_pr_t <- subset(rosn_pr_t, select = -date)
rosn_pr_t$r <- c()
for (i in 1:dim(rosn_pr_t)[1]){
  rosn_pr_t$r[i] <- (rosn_pr_t$price[i]-rosn_pr_t$price[i+1])/rosn_pr_t$price[i+1]*100
}
rosn_pr_t$vol <- str_replace_all(rosn_pr_t$vol, "K", "0")
rosn_pr_t$vol <- str_replace_all(rosn_pr_t$vol, "M", "0000")
rosn_pr_t$vol <- str_replace_all(rosn_pr_t$vol, "B", "0000000")
rosn_pr_t$vol <- gsub(',', '', as.character(rosn_pr_t$vol), perl = TRUE)
rosn_pr_t$vol <- as.numeric(rosn_pr_t$vol)
rosn_pr_t$v <- c()
for (i in 1:dim(rosn_pr_t)[1]){
  rosn_pr_t$v[i] <- (rosn_pr_t$vol[i]-rosn_pr_t$vol[i+1])/rosn_pr_t$vol[i+1]*100
}
}
# Объединение
rosn_res <- left_join(rosn_pr_t, rosn_pub_gr, by = 'datetime2')
rosn_res[is.na(rosn_res)] <- 0
View(rosn_res)

## Модель
adf.test(ts(rosn_res$r))
plot.ts((ts(rosn_res$r)))
acf(ts(rosn_res$r))

adf.test(ts(rosn_res$v))
plot.ts((ts(rosn_res$v)))
acf(ts(rosn_res$v))

rosn_mod_r <- auto.arima(ts(rosn_res$r), 
                         xreg = as.matrix(rosn_res[,6:8]))
summary(rosn_mod_r)
coeftest(rosn_mod_r)

rosn_mod_v <- auto.arima(ts(rosn_res$v), 
                         xreg = as.matrix(rosn_res[,6:8]))
summary(rosn_mod_v)
coeftest(rosn_mod_v)

# Whoosh WUSH 
{
wush_index <- read.csv("~/Downloads/Diplom/Sorted_Way/wush_index.csv")
wush_pub <- df_time_pr_comb[wush_index$X0+1,]
wush_pub_gr <- wush_pub %>%
  group_by(datetime2) %>%
  summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
View(wush_pub_gr) 
# Котировки
wush_pr <- read.csv("~/Downloads/Diplom/Sorted_Way/wush_pr.csv")
wush_pr_t <- dplyr::select(wush_pr, c("Дата", "Цена", "Объём"))
colnames(wush_pr_t) <- c('date','price','vol')
wush_pr_t <- wush_pr_t %>%
  mutate(datetime2=as.Date(date, format = "%d.%m.%Y"))
wush_pr_t$price <- gsub('\\.|', '', as.character(wush_pr_t$price), perl = TRUE)
wush_pr_t$price <- as.numeric(str_replace_all(wush_pr_t$price, ",", "."))
wush_pr_t <- subset(wush_pr_t, select = -date)
wush_pr_t$r <- c()
for (i in 1:dim(wush_pr_t)[1]){
  wush_pr_t$r[i] <- (wush_pr_t$price[i]-wush_pr_t$price[i+1])/wush_pr_t$price[i+1]*100
}
wush_pr_t$vol <- str_replace_all(wush_pr_t$vol, "K", "0")
wush_pr_t$vol <- str_replace_all(wush_pr_t$vol, "M", "0000")
wush_pr_t$vol <- str_replace_all(wush_pr_t$vol, "B", "0000000")
wush_pr_t$vol <- gsub(',', '', as.character(wush_pr_t$vol), perl = TRUE)
wush_pr_t$vol <- as.numeric(wush_pr_t$vol)
wush_pr_t$v <- c()
for (i in 1:dim(wush_pr_t)[1]){
  wush_pr_t$v[i] <- (wush_pr_t$vol[i]-wush_pr_t$vol[i+1])/wush_pr_t$vol[i+1]*100
}
}
# Объединение
wush_res <- left_join(wush_pr_t, wush_pub_gr, by = 'datetime2')
wush_res[is.na(wush_res)] <- 0
View(wush_res)

## Модель
adf.test(ts(wush_res$r))
plot.ts((ts(wush_res$r)))
acf(ts(wush_res$r))

adf.test(ts(wush_res$v))
plot.ts((ts(wush_res$v)))
acf(ts(wush_res$v))

wush_mod_r <- auto.arima(ts(wush_res$r), 
                         xreg = as.matrix(wush_res[,6:7]))
summary(wush_mod_r)
coeftest(wush_mod_r)

wush_mod_v <- auto.arima(ts(wush_res$v), 
                         xreg = as.matrix(wush_res[,6:7]))
summary(wush_mod_v)
coeftest(wush_mod_v)

#### Event Study ####
df_event <- read.csv("~/df_hour_minute_pr.csv")
df_event_cor <- df_event
for (i in 1:dim(df_event_cor)[1]){
  if (df_event_cor$hour[i] < 10){
    df_event_cor$hour[i] <- 10
    df_event_cor$minute[i] <- 1
  }
}
View(df_event_cor)
# Всего 12 компаний
View(alrosa_res) 
View(oak_res) 
View(yndx_res) 
View(smlt_res) 
View(lenz_res) 
View(sber_res) 
View(ozon_res) 
View(cian_res) 
View(hhru_res) 
View(luk_res) 
View(rosn_res) 
View(wush_res) 

# Алроса
ALRS_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/ALRS_22_23_t.csv", sep=",")
ALRS_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/ALRS_21_22_t.csv", sep=",")
ALRS_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/ALRS_20_21_t.csv", sep=",")
ALRS_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/ALRS_19_20_t.csv", sep=",")
ALRS_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/ALRS_18_19_t.csv", sep=",")
ALRS_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/ALRS_17_18_t.csv", sep=",")
ALRS_event <- rbind(ALRS_17_18, ALRS_18_19, ALRS_19_20, ALRS_20_21,
                    ALRS_21_22, ALRS_22_23)
ALRS_event <- dplyr::select(ALRS_event, c('X.DATE.','X.TIME.',
                                          'X.CLOSE.','X.VOL.'))
colnames(ALRS_event) <- c('date','time','price','vol')
ALRS_event$date <- as.character(ALRS_event$date)
a_ALRS_time_event <- data.frame(str_split_fixed(ALRS_event$date, 
                                                pattern = "/", n = 3))
colnames(a_ALRS_time_event) <- c('day','month','year')
a_ALRS_time_event$year <- paste("20", as.character(a_ALRS_time_event$year), sep="")
a_ALRS_time_event$date <- paste(a_ALRS_time_event$year, a_ALRS_time_event$month,
                                a_ALRS_time_event$day, sep = '-')
ALRS_event$date <- as.Date(a_ALRS_time_event$date, format = '%Y-%m-%d') 
ALRS_event$date_time <- as.POSIXct(paste(ALRS_event$date, 
                                         ALRS_event$time),  
                                   format = "%Y-%m-%d %H:%M:%S")
ALRS_event <- dplyr::select(ALRS_event, -c('date','time'))
View(ALRS_event)
{
  # one-two-three
  ALRS_index <- read.csv("~/Downloads/Diplom/Sorted_Way/alrosa_index.csv")
  df_event_cor_ALRS <-  df_event_cor[ALRS_index$X0+1,]
  df_event_cor_ALRS2 <- dplyr::select(df_event_cor_ALRS, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_ALRS2$sec <- rep('0',dim(df_event_cor_ALRS2)[1])
  df_event_cor_ALRS3 <- df_event_cor_ALRS2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_ALRS3$date <- as.POSIXct(df_event_cor_ALRS3$datetime2) 
  df_event_cor_ALRS3$date_time <- as.POSIXct(paste(df_event_cor_ALRS3$date, 
                                                   df_event_cor_ALRS3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_ALRS_t <- dplyr::select(df_event_cor_ALRS3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_ALRS_t_gr <- df_event_cor_ALRS_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_ALRS_t_gr)
  # Объединение
  ALRS_event_res <- left_join(ALRS_event, df_event_cor_ALRS_t_gr, by = 'date_time')
  ALRS_event_res[is.na(ALRS_event_res)] <- 0
  ALRS_event_res$diff_price <- c(0,diff(ALRS_event_res$price))
  ALRS_event_res$lag_price <- lag(ALRS_event_res$price, 
                                  default = ALRS_event_res$price[1])
  ALRS_event_res$r <- ALRS_event_res$diff_price/ALRS_event_res$lag_price*100
  ALRS_event_res$diff_vol <- c(0,diff(ALRS_event_res$vol))
  ALRS_event_res$lag_vol <- lag(ALRS_event_res$vol, 
                                default = ALRS_event_res$vol[1])
  ALRS_event_res$v <- ALRS_event_res$diff_vol/ALRS_event_res$lag_vol*100
  ALRS_event_res_t <- dplyr::select(ALRS_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(ALRS_event_res_t) <- c('date_time','ALRS_r','ALRS_v',
                                  'ALRS_one','ALRS_two','ALRS_three')
  View(ALRS_event_res_t)
}
# ОАК
UNAC_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/UNAC_22_23_t.csv", sep=",")
UNAC_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/UNAC_21_22_t.csv", sep=",")
UNAC_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/UNAC_20_21_t.csv", sep=",")
UNAC_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/UNAC_19_20_t.csv", sep=",")
UNAC_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/UNAC_18_19_t.csv", sep=",")
UNAC_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/UNAC_17_18_t.csv", sep=",")
UNAC_event <- rbind(UNAC_17_18, UNAC_18_19, UNAC_19_20, UNAC_20_21,
                    UNAC_21_22, UNAC_22_23)
UNAC_event <- dplyr::select(UNAC_event, c('X.DATE.','X.TIME.',
                                          'X.CLOSE.','X.VOL.'))
colnames(UNAC_event) <- c('date','time','price','vol')
UNAC_event$date <- as.character(UNAC_event$date)
a_UNAC_time_event <- data.frame(str_split_fixed(UNAC_event$date, 
                                                pattern = "/", n = 3))
colnames(a_UNAC_time_event) <- c('day','month','year')
a_UNAC_time_event$year <- paste("20", as.character(a_UNAC_time_event$year), sep="")
a_UNAC_time_event$date <- paste(a_UNAC_time_event$year, a_UNAC_time_event$month,
                                a_UNAC_time_event$day, sep = '-')
UNAC_event$date <- as.Date(a_UNAC_time_event$date, format = '%Y-%m-%d') 
UNAC_event$date_time <- as.POSIXct(paste(UNAC_event$date, 
                                         UNAC_event$time),  
                                   format = "%Y-%m-%d %H:%M:%S")
UNAC_event <- dplyr::select(UNAC_event, -c('date','time'))
View(UNAC_event)
{
  # one-two-three
  UNAC_index <- read.csv("~/Downloads/Diplom/Sorted_Way/oak_index.csv")
  df_event_cor_UNAC <-  df_event_cor[UNAC_index$X0+1,]
  df_event_cor_UNAC2 <- dplyr::select(df_event_cor_UNAC, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_UNAC2$sec <- rep('0',dim(df_event_cor_UNAC2)[1])
  df_event_cor_UNAC3 <- df_event_cor_UNAC2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_UNAC3$date <- as.POSIXct(df_event_cor_UNAC3$datetime2) 
  df_event_cor_UNAC3$date_time <- as.POSIXct(paste(df_event_cor_UNAC3$date, 
                                                   df_event_cor_UNAC3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_UNAC_t <- dplyr::select(df_event_cor_UNAC3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_UNAC_t_gr <- df_event_cor_UNAC_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_UNAC_t_gr)
  # Объединение
  UNAC_event_res <- left_join(UNAC_event, df_event_cor_UNAC_t_gr, by = 'date_time')
  UNAC_event_res[is.na(UNAC_event_res)] <- 0
  UNAC_event_res$diff_price <- c(0,diff(UNAC_event_res$price))
  UNAC_event_res$lag_price <- lag(UNAC_event_res$price, 
                                  default = UNAC_event_res$price[1])
  UNAC_event_res$r <- UNAC_event_res$diff_price/UNAC_event_res$lag_price*100
  UNAC_event_res$diff_vol <- c(0,diff(UNAC_event_res$vol))
  UNAC_event_res$lag_vol <- lag(UNAC_event_res$vol, 
                                default = UNAC_event_res$vol[1])
  UNAC_event_res$v <- UNAC_event_res$diff_vol/UNAC_event_res$lag_vol*100
  UNAC_event_res_t <- dplyr::select(UNAC_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(UNAC_event_res_t) <- c('date_time','UNAC_r','UNAC_v',
                                  'UNAC_one','UNAC_two','UNAC_three')
  View(UNAC_event_res_t)
}
# Яндекс
{
  YNDX_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/YNDX_22_23_t.csv", sep=";")
  YNDX_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/YNDX_21_22_t.csv", sep=";")
  YNDX_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/YNDX_20_21_t.csv", sep=";")
  YNDX_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/YNDX_19_20_t.csv", sep=";")
  YNDX_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/YNDX_18_19_t.csv", sep=";")
  YNDX_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/YNDX_17_18_t.csv", sep=";")
  YNDX_event <- rbind(YNDX_17_18, YNDX_18_19, YNDX_19_20, YNDX_20_21,
                      YNDX_21_22, YNDX_22_23)
  YNDX_event <- dplyr::select(YNDX_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(YNDX_event) <- c('date','time','price','vol')
  YNDX_event$date <- as.character(YNDX_event$date)
  a_YNDX_time_event <- data.frame(str_split_fixed(YNDX_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_YNDX_time_event) <- c('day','month','year')
  a_YNDX_time_event$year <- paste("20", as.character(a_YNDX_time_event$year), sep="")
  a_YNDX_time_event$date <- paste(a_YNDX_time_event$year, a_YNDX_time_event$month,
                                  a_YNDX_time_event$day, sep = '-')
  YNDX_event$date <- as.Date(a_YNDX_time_event$date, format = '%Y-%m-%d') 
  YNDX_event$date_time <- as.POSIXct(paste(YNDX_event$date, 
                                           YNDX_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  YNDX_event <- dplyr::select(YNDX_event, -c('date','time'))
  View(YNDX_event)
}
{
  # one-two-three
  YNDX_index <- read.csv("~/Downloads/Diplom/Sorted_Way/yndx_index.csv")
  df_event_cor_YNDX <-  df_event_cor[YNDX_index$X0+1,]
  df_event_cor_YNDX2 <- dplyr::select(df_event_cor_YNDX, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_YNDX2$sec <- rep('0',dim(df_event_cor_YNDX2)[1])
  df_event_cor_YNDX3 <- df_event_cor_YNDX2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_YNDX3$date <- as.POSIXct(df_event_cor_YNDX3$datetime2) 
  df_event_cor_YNDX3$date_time <- as.POSIXct(paste(df_event_cor_YNDX3$date, 
                                                   df_event_cor_YNDX3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_YNDX_t <- dplyr::select(df_event_cor_YNDX3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_YNDX_t_gr <- df_event_cor_YNDX_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_YNDX_t_gr)
  # Объединение
  YNDX_event_res <- left_join(YNDX_event, df_event_cor_YNDX_t_gr, by = 'date_time')
  YNDX_event_res[is.na(YNDX_event_res)] <- 0
  YNDX_event_res$diff_price <- c(0,diff(YNDX_event_res$price))
  YNDX_event_res$lag_price <- lag(YNDX_event_res$price, 
                                  default = YNDX_event_res$price[1])
  YNDX_event_res$r <- YNDX_event_res$diff_price/YNDX_event_res$lag_price*100
  YNDX_event_res$diff_vol <- c(0,diff(YNDX_event_res$vol))
  YNDX_event_res$lag_vol <- lag(YNDX_event_res$vol, 
                                default = YNDX_event_res$vol[1])
  YNDX_event_res$v <- YNDX_event_res$diff_vol/YNDX_event_res$lag_vol*100
  YNDX_event_res_t <- dplyr::select(YNDX_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(YNDX_event_res_t) <- c('date_time','YNDX_r','YNDX_v',
                                  'YNDX_one','YNDX_two','YNDX_three')
  View(YNDX_event_res_t)
}
# Самолёт
{
  SMLT_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/SMLT_22_23_t.csv", sep=",")
  SMLT_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/SMLT_21_22_t.csv", sep=",")
  SMLT_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/SMLT_20_21_t.csv", sep=",")
  
  SMLT_event <- rbind(SMLT_20_21,
                      SMLT_21_22, SMLT_22_23)
  SMLT_event <- dplyr::select(SMLT_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(SMLT_event) <- c('date','time','price','vol')
  SMLT_event$date <- as.character(SMLT_event$date)
  a_SMLT_time_event <- data.frame(str_split_fixed(SMLT_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_SMLT_time_event) <- c('day','month','year')
  a_SMLT_time_event$year <- paste("20", as.character(a_SMLT_time_event$year), sep="")
  a_SMLT_time_event$date <- paste(a_SMLT_time_event$year, a_SMLT_time_event$month,
                                  a_SMLT_time_event$day, sep = '-')
  SMLT_event$date <- as.Date(a_SMLT_time_event$date, format = '%Y-%m-%d') 
  SMLT_event$date_time <- as.POSIXct(paste(SMLT_event$date, 
                                           SMLT_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  SMLT_event <- dplyr::select(SMLT_event, -c('date','time'))
  View(SMLT_event)
}
{
  # one-two-three
  SMLT_index <- read.csv("~/Downloads/Diplom/Sorted_Way/smlt_index.csv")
  df_event_cor_SMLT <-  df_event_cor[SMLT_index$X0+1,]
  df_event_cor_SMLT2 <- dplyr::select(df_event_cor_SMLT, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_SMLT2$sec <- rep('0',dim(df_event_cor_SMLT2)[1])
  df_event_cor_SMLT3 <- df_event_cor_SMLT2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_SMLT3$date <- as.POSIXct(df_event_cor_SMLT3$datetime2) 
  df_event_cor_SMLT3$date_time <- as.POSIXct(paste(df_event_cor_SMLT3$date, 
                                                   df_event_cor_SMLT3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_SMLT_t <- dplyr::select(df_event_cor_SMLT3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_SMLT_t_gr <- df_event_cor_SMLT_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_SMLT_t_gr)
  # Объединение
  SMLT_event_res <- left_join(SMLT_event, df_event_cor_SMLT_t_gr, by = 'date_time')
  SMLT_event_res[is.na(SMLT_event_res)] <- 0
  SMLT_event_res$diff_price <- c(0,diff(SMLT_event_res$price))
  SMLT_event_res$lag_price <- lag(SMLT_event_res$price, 
                                  default = SMLT_event_res$price[1])
  SMLT_event_res$r <- SMLT_event_res$diff_price/SMLT_event_res$lag_price*100
  SMLT_event_res$diff_vol <- c(0,diff(SMLT_event_res$vol))
  SMLT_event_res$lag_vol <- lag(SMLT_event_res$vol, 
                                default = SMLT_event_res$vol[1])
  SMLT_event_res$v <- SMLT_event_res$diff_vol/SMLT_event_res$lag_vol*100
  SMLT_event_res_t <- dplyr::select(SMLT_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(SMLT_event_res_t) <- c('date_time','SMLT_r','SMLT_v',
                                  'SMLT_one','SMLT_two','SMLT_three')
  View(SMLT_event_res_t)
}
# Лензолото
{
  LNZL_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/LNZL_22_23_t.csv", sep=",")
  LNZL_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/LNZL_21_22_t.csv", sep=",")
  LNZL_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/LNZL_20_21_t.csv", sep=",")
  LNZL_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/LNZL_19_20_t.csv", sep=",")
  LNZL_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/LNZL_18_19_t.csv", sep=",")
  LNZL_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/LNZL_17_18_t.csv", sep=",")
  LNZL_event <- rbind(LNZL_17_18, LNZL_18_19, LNZL_19_20, LNZL_20_21,
                      LNZL_21_22, LNZL_22_23)
  LNZL_event <- dplyr::select(LNZL_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(LNZL_event) <- c('date','time','price','vol')
  LNZL_event$date <- as.character(LNZL_event$date)
  a_LNZL_time_event <- data.frame(str_split_fixed(LNZL_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_LNZL_time_event) <- c('day','month','year')
  a_LNZL_time_event$year <- paste("20", as.character(a_LNZL_time_event$year), sep="")
  a_LNZL_time_event$date <- paste(a_LNZL_time_event$year, a_LNZL_time_event$month,
                                  a_LNZL_time_event$day, sep = '-')
  LNZL_event$date <- as.Date(a_LNZL_time_event$date, format = '%Y-%m-%d') 
  LNZL_event$date_time <- as.POSIXct(paste(LNZL_event$date, 
                                           LNZL_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  LNZL_event <- dplyr::select(LNZL_event, -c('date','time'))
  View(LNZL_event)
}
{
  # one-two-three
  LNZL_index <- read.csv("~/Downloads/Diplom/Sorted_Way/lenz_index.csv")
  df_event_cor_LNZL <-  df_event_cor[LNZL_index$X0+1,]
  df_event_cor_LNZL2 <- dplyr::select(df_event_cor_LNZL, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_LNZL2$sec <- rep('0',dim(df_event_cor_LNZL2)[1])
  df_event_cor_LNZL3 <- df_event_cor_LNZL2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_LNZL3$date <- as.POSIXct(df_event_cor_LNZL3$datetime2) 
  df_event_cor_LNZL3$date_time <- as.POSIXct(paste(df_event_cor_LNZL3$date, 
                                                   df_event_cor_LNZL3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_LNZL_t <- dplyr::select(df_event_cor_LNZL3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_LNZL_t_gr <- df_event_cor_LNZL_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_LNZL_t_gr)
  # Объединение
  LNZL_event_res <- left_join(LNZL_event, df_event_cor_LNZL_t_gr, by = 'date_time')
  LNZL_event_res[is.na(LNZL_event_res)] <- 0
  LNZL_event_res$diff_price <- c(0,diff(LNZL_event_res$price))
  LNZL_event_res$lag_price <- lag(LNZL_event_res$price, 
                                  default = LNZL_event_res$price[1])
  LNZL_event_res$r <- LNZL_event_res$diff_price/LNZL_event_res$lag_price*100
  LNZL_event_res$diff_vol <- c(0,diff(LNZL_event_res$vol))
  LNZL_event_res$lag_vol <- lag(LNZL_event_res$vol, 
                                default = LNZL_event_res$vol[1])
  LNZL_event_res$v <- LNZL_event_res$diff_vol/LNZL_event_res$lag_vol*100
  LNZL_event_res_t <- dplyr::select(LNZL_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(LNZL_event_res_t) <- c('date_time','LNZL_r','LNZL_v',
                                  'LNZL_one','LNZL_two','LNZL_three')
  View(LNZL_event_res_t)
}
# Сбер
{
  SBER_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/SBER_22_23_t.csv", sep=",")
  SBER_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/SBER_21_22_t.csv", sep=",")
  SBER_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/SBER_20_21_t.csv", sep=",")
  SBER_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/SBER_19_20_t.csv", sep=",")
  SBER_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/SBER_18_19_t.csv", sep=",")
  SBER_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/SBER_17_18_t.csv", sep=",")
  SBER_event <- rbind(SBER_17_18, SBER_18_19, SBER_19_20, SBER_20_21,
                      SBER_21_22, SBER_22_23)
  SBER_event <- dplyr::select(SBER_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(SBER_event) <- c('date','time','price','vol')
  SBER_event$date <- as.character(SBER_event$date)
  a_SBER_time_event <- data.frame(str_split_fixed(SBER_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_SBER_time_event) <- c('day','month','year')
  a_SBER_time_event$year <- paste("20", as.character(a_SBER_time_event$year), sep="")
  a_SBER_time_event$date <- paste(a_SBER_time_event$year, a_SBER_time_event$month,
                                  a_SBER_time_event$day, sep = '-')
  SBER_event$date <- as.Date(a_SBER_time_event$date, format = '%Y-%m-%d') 
  SBER_event$date_time <- as.POSIXct(paste(SBER_event$date, 
                                           SBER_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  SBER_event <- dplyr::select(SBER_event, -c('date','time'))
  View(SBER_event)
}
{
  # one-two-three
  SBER_index <- read.csv("~/Downloads/Diplom/Sorted_Way/sber_index.csv")
  df_event_cor_SBER <-  df_event_cor[SBER_index$X0+1,]
  df_event_cor_SBER2 <- dplyr::select(df_event_cor_SBER, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_SBER2$sec <- rep('0',dim(df_event_cor_SBER2)[1])
  df_event_cor_SBER3 <- df_event_cor_SBER2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_SBER3$date <- as.POSIXct(df_event_cor_SBER3$datetime2) 
  df_event_cor_SBER3$date_time <- as.POSIXct(paste(df_event_cor_SBER3$date, 
                                                   df_event_cor_SBER3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_SBER_t <- dplyr::select(df_event_cor_SBER3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_SBER_t_gr <- df_event_cor_SBER_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_SBER_t_gr)
  # Объединение
  SBER_event_res <- left_join(SBER_event, df_event_cor_SBER_t_gr, by = 'date_time')
  SBER_event_res[is.na(SBER_event_res)] <- 0
  SBER_event_res$diff_price <- c(0,diff(SBER_event_res$price))
  SBER_event_res$lag_price <- lag(SBER_event_res$price, 
                                  default = SBER_event_res$price[1])
  SBER_event_res$r <- SBER_event_res$diff_price/SBER_event_res$lag_price*100
  SBER_event_res$diff_vol <- c(0,diff(SBER_event_res$vol))
  SBER_event_res$lag_vol <- lag(SBER_event_res$vol, 
                                default = SBER_event_res$vol[1])
  SBER_event_res$v <- SBER_event_res$diff_vol/SBER_event_res$lag_vol*100
  SBER_event_res_t <- dplyr::select(SBER_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(SBER_event_res_t) <- c('date_time','SBER_r','SBER_v',
                                  'SBER_one','SBER_two','SBER_three')
  View(SBER_event_res_t)
}
# ОЗОН
{
  OZON_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/OZON_22_23_t.csv", sep=",")
  OZON_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/OZON_21_22_t.csv", sep=",")
  OZON_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/OZON_20_21_t.csv", sep=",")
  OZON_event <- rbind(OZON_20_21,
                      OZON_21_22, OZON_22_23)
  OZON_event <- dplyr::select(OZON_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(OZON_event) <- c('date','time','price','vol')
  OZON_event$date <- as.character(OZON_event$date)
  a_OZON_time_event <- data.frame(str_split_fixed(OZON_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_OZON_time_event) <- c('day','month','year')
  a_OZON_time_event$year <- paste("20", as.character(a_OZON_time_event$year), sep="")
  a_OZON_time_event$date <- paste(a_OZON_time_event$year, a_OZON_time_event$month,
                                  a_OZON_time_event$day, sep = '-')
  OZON_event$date <- as.Date(a_OZON_time_event$date, format = '%Y-%m-%d') 
  OZON_event$date_time <- as.POSIXct(paste(OZON_event$date, 
                                           OZON_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  OZON_event <- dplyr::select(OZON_event, -c('date','time'))
  View(OZON_event)
}
{
  # one-two-three
  OZON_index <- read.csv("~/Downloads/Diplom/Sorted_Way/ozon_index.csv")
  df_event_cor_OZON <-  df_event_cor[OZON_index$X0+1,]
  df_event_cor_OZON2 <- dplyr::select(df_event_cor_OZON, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_OZON2$sec <- rep('0',dim(df_event_cor_OZON2)[1])
  df_event_cor_OZON3 <- df_event_cor_OZON2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_OZON3$date <- as.POSIXct(df_event_cor_OZON3$datetime2) 
  df_event_cor_OZON3$date_time <- as.POSIXct(paste(df_event_cor_OZON3$date, 
                                                   df_event_cor_OZON3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_OZON_t <- dplyr::select(df_event_cor_OZON3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_OZON_t_gr <- df_event_cor_OZON_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_OZON_t_gr)
  # Объединение
  OZON_event_res <- left_join(OZON_event, df_event_cor_OZON_t_gr, by = 'date_time')
  OZON_event_res[is.na(OZON_event_res)] <- 0
  OZON_event_res$diff_price <- c(0,diff(OZON_event_res$price))
  OZON_event_res$lag_price <- lag(OZON_event_res$price, 
                                  default = OZON_event_res$price[1])
  OZON_event_res$r <- OZON_event_res$diff_price/OZON_event_res$lag_price*100
  OZON_event_res$diff_vol <- c(0,diff(OZON_event_res$vol))
  OZON_event_res$lag_vol <- lag(OZON_event_res$vol, 
                                default = OZON_event_res$vol[1])
  OZON_event_res$v <- OZON_event_res$diff_vol/OZON_event_res$lag_vol*100
  OZON_event_res_t <- dplyr::select(OZON_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(OZON_event_res_t) <- c('date_time','OZON_r','OZON_v',
                                  'OZON_one','OZON_two','OZON_three')
  View(OZON_event_res_t)
}
# ЦИАН
{
  CIAN_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/CIAN_22_23_t.csv", sep=",")
  CIAN_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/CIAN_21_22_t.csv", sep=",")
  CIAN_event <- rbind(CIAN_21_22, CIAN_22_23)
  CIAN_event <- dplyr::select(CIAN_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(CIAN_event) <- c('date','time','price','vol')
  CIAN_event$date <- as.character(CIAN_event$date)
  a_CIAN_time_event <- data.frame(str_split_fixed(CIAN_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_CIAN_time_event) <- c('day','month','year')
  a_CIAN_time_event$year <- paste("20", as.character(a_CIAN_time_event$year), sep="")
  a_CIAN_time_event$date <- paste(a_CIAN_time_event$year, a_CIAN_time_event$month,
                                  a_CIAN_time_event$day, sep = '-')
  CIAN_event$date <- as.Date(a_CIAN_time_event$date, format = '%Y-%m-%d') 
  CIAN_event$date_time <- as.POSIXct(paste(CIAN_event$date, 
                                           CIAN_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  CIAN_event <- dplyr::select(CIAN_event, -c('date','time'))
  View(CIAN_event)
}
{
  # one-two-three
  CIAN_index <- read.csv("~/Downloads/Diplom/Sorted_Way/cian_index.csv")
  df_event_cor_CIAN <-  df_event_cor[CIAN_index$X0+1,]
  df_event_cor_CIAN2 <- dplyr::select(df_event_cor_CIAN, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_CIAN2$sec <- rep('0',dim(df_event_cor_CIAN2)[1])
  df_event_cor_CIAN3 <- df_event_cor_CIAN2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_CIAN3$date <- as.POSIXct(df_event_cor_CIAN3$datetime2) 
  df_event_cor_CIAN3$date_time <- as.POSIXct(paste(df_event_cor_CIAN3$date, 
                                                   df_event_cor_CIAN3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_CIAN_t <- dplyr::select(df_event_cor_CIAN3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_CIAN_t_gr <- df_event_cor_CIAN_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_CIAN_t_gr)
  # Объединение
  CIAN_event_res <- left_join(CIAN_event, df_event_cor_CIAN_t_gr, by = 'date_time')
  CIAN_event_res[is.na(CIAN_event_res)] <- 0
  CIAN_event_res$diff_price <- c(0,diff(CIAN_event_res$price))
  CIAN_event_res$lag_price <- lag(CIAN_event_res$price, 
                                  default = CIAN_event_res$price[1])
  CIAN_event_res$r <- CIAN_event_res$diff_price/CIAN_event_res$lag_price*100
  CIAN_event_res$diff_vol <- c(0,diff(CIAN_event_res$vol))
  CIAN_event_res$lag_vol <- lag(CIAN_event_res$vol, 
                                default = CIAN_event_res$vol[1])
  CIAN_event_res$v <- CIAN_event_res$diff_vol/CIAN_event_res$lag_vol*100
  CIAN_event_res_t <- dplyr::select(CIAN_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(CIAN_event_res_t) <- c('date_time','CIAN_r','CIAN_v',
                                  'CIAN_one','CIAN_two','CIAN_three')
  View(CIAN_event_res_t)
}
# HHRU
{
  HHRU_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/HHRU_22_23_t.csv", sep=",")
  HHRU_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/HHRU_21_22_t.csv", sep=",")
  HHRU_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/HHRU_20_21_t.csv", sep=",")
  HHRU_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/HHRU_19_20_t.csv", sep=",")
  HHRU_event <- rbind(HHRU_19_20, HHRU_20_21,
                      HHRU_21_22, HHRU_22_23)
  HHRU_event <- dplyr::select(HHRU_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(HHRU_event) <- c('date','time','price','vol')
  HHRU_event$date <- as.character(HHRU_event$date)
  a_HHRU_time_event <- data.frame(str_split_fixed(HHRU_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_HHRU_time_event) <- c('day','month','year')
  a_HHRU_time_event$year <- paste("20", as.character(a_HHRU_time_event$year), sep="")
  a_HHRU_time_event$date <- paste(a_HHRU_time_event$year, a_HHRU_time_event$month,
                                  a_HHRU_time_event$day, sep = '-')
  HHRU_event$date <- as.Date(a_HHRU_time_event$date, format = '%Y-%m-%d') 
  HHRU_event$date_time <- as.POSIXct(paste(HHRU_event$date, 
                                           HHRU_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  HHRU_event <- dplyr::select(HHRU_event, -c('date','time'))
  View(HHRU_event)
}
{
  # one-two-three
  HHRU_index <- read.csv("~/Downloads/Diplom/Sorted_Way/hhru_index.csv")
  df_event_cor_HHRU <-  df_event_cor[HHRU_index$X0+1,]
  df_event_cor_HHRU2 <- dplyr::select(df_event_cor_HHRU, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_HHRU2$sec <- rep('0',dim(df_event_cor_HHRU2)[1])
  df_event_cor_HHRU3 <- df_event_cor_HHRU2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_HHRU3$date <- as.POSIXct(df_event_cor_HHRU3$datetime2) 
  df_event_cor_HHRU3$date_time <- as.POSIXct(paste(df_event_cor_HHRU3$date, 
                                                   df_event_cor_HHRU3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_HHRU_t <- dplyr::select(df_event_cor_HHRU3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_HHRU_t_gr <- df_event_cor_HHRU_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_HHRU_t_gr)
  # Объединение
  HHRU_event_res <- left_join(HHRU_event, df_event_cor_HHRU_t_gr, by = 'date_time')
  HHRU_event_res[is.na(HHRU_event_res)] <- 0
  HHRU_event_res$diff_price <- c(0,diff(HHRU_event_res$price))
  HHRU_event_res$lag_price <- lag(HHRU_event_res$price, 
                                  default = HHRU_event_res$price[1])
  HHRU_event_res$r <- HHRU_event_res$diff_price/HHRU_event_res$lag_price*100
  HHRU_event_res$diff_vol <- c(0,diff(HHRU_event_res$vol))
  HHRU_event_res$lag_vol <- lag(HHRU_event_res$vol, 
                                default = HHRU_event_res$vol[1])
  HHRU_event_res$v <- HHRU_event_res$diff_vol/HHRU_event_res$lag_vol*100
  HHRU_event_res_t <- dplyr::select(HHRU_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(HHRU_event_res_t) <- c('date_time','HHRU_r','HHRU_v',
                                  'HHRU_one','HHRU_two','HHRU_three')
  View(HHRU_event_res_t)
}
# Лукойл
{
  LKOH_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/LKOH_22_23_t.csv", sep=",")
  LKOH_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/LKOH_21_22_t.csv", sep=",")
  LKOH_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/LKOH_20_21_t.csv", sep=",")
  LKOH_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/LKOH_19_20_t.csv", sep=",")
  LKOH_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/LKOH_18_19_t.csv", sep=",")
  LKOH_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/LKOH_17_18_t.csv", sep=",")
  LKOH_event <- rbind(LKOH_17_18, LKOH_18_19, LKOH_19_20, LKOH_20_21,
                      LKOH_21_22, LKOH_22_23)
  LKOH_event <- dplyr::select(LKOH_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(LKOH_event) <- c('date','time','price','vol')
  LKOH_event$date <- as.character(LKOH_event$date)
  a_LKOH_time_event <- data.frame(str_split_fixed(LKOH_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_LKOH_time_event) <- c('day','month','year')
  a_LKOH_time_event$year <- paste("20", as.character(a_LKOH_time_event$year), sep="")
  a_LKOH_time_event$date <- paste(a_LKOH_time_event$year, a_LKOH_time_event$month,
                                  a_LKOH_time_event$day, sep = '-')
  LKOH_event$date <- as.Date(a_LKOH_time_event$date, format = '%Y-%m-%d') 
  LKOH_event$date_time <- as.POSIXct(paste(LKOH_event$date, 
                                           LKOH_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  LKOH_event <- dplyr::select(LKOH_event, -c('date','time'))
  View(LKOH_event)
}
{
  # one-two-three
  LKOH_index <- read.csv("~/Downloads/Diplom/Sorted_Way/luk_index.csv")
  df_event_cor_LKOH <-  df_event_cor[LKOH_index$X0+1,]
  df_event_cor_LKOH2 <- dplyr::select(df_event_cor_LKOH, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_LKOH2$sec <- rep('0',dim(df_event_cor_LKOH2)[1])
  df_event_cor_LKOH3 <- df_event_cor_LKOH2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_LKOH3$date <- as.POSIXct(df_event_cor_LKOH3$datetime2) 
  df_event_cor_LKOH3$date_time <- as.POSIXct(paste(df_event_cor_LKOH3$date, 
                                                   df_event_cor_LKOH3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_LKOH_t <- dplyr::select(df_event_cor_LKOH3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_LKOH_t_gr <- df_event_cor_LKOH_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_LKOH_t_gr)
  # Объединение
  LKOH_event_res <- left_join(LKOH_event, df_event_cor_LKOH_t_gr, by = 'date_time')
  LKOH_event_res[is.na(LKOH_event_res)] <- 0
  LKOH_event_res$diff_price <- c(0,diff(LKOH_event_res$price))
  LKOH_event_res$lag_price <- lag(LKOH_event_res$price, 
                                  default = LKOH_event_res$price[1])
  LKOH_event_res$r <- LKOH_event_res$diff_price/LKOH_event_res$lag_price*100
  LKOH_event_res$diff_vol <- c(0,diff(LKOH_event_res$vol))
  LKOH_event_res$lag_vol <- lag(LKOH_event_res$vol, 
                                default = LKOH_event_res$vol[1])
  LKOH_event_res$v <- LKOH_event_res$diff_vol/LKOH_event_res$lag_vol*100
  LKOH_event_res_t <- dplyr::select(LKOH_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(LKOH_event_res_t) <- c('date_time','LKOH_r','LKOH_v',
                                  'LKOH_one','LKOH_two','LKOH_three')
  View(LKOH_event_res_t)
}
# Роснефть
{
  ROSN_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/ROSN_22_23_t.csv", sep=",")
  ROSN_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/ROSN_21_22_t.csv", sep=",")
  ROSN_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/ROSN_20_21_t.csv", sep=",")
  ROSN_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/ROSN_19_20_t.csv", sep=",")
  ROSN_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/ROSN_18_19_t.csv", sep=",")
  ROSN_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/ROSN_17_18_t.csv", sep=",")
  ROSN_event <- rbind(ROSN_17_18, ROSN_18_19, ROSN_19_20, ROSN_20_21,
                      ROSN_21_22, ROSN_22_23)
  ROSN_event <- dplyr::select(ROSN_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(ROSN_event) <- c('date','time','price','vol')
  ROSN_event$date <- as.character(ROSN_event$date)
  a_ROSN_time_event <- data.frame(str_split_fixed(ROSN_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_ROSN_time_event) <- c('day','month','year')
  a_ROSN_time_event$year <- paste("20", as.character(a_ROSN_time_event$year), sep="")
  a_ROSN_time_event$date <- paste(a_ROSN_time_event$year, a_ROSN_time_event$month,
                                  a_ROSN_time_event$day, sep = '-')
  ROSN_event$date <- as.Date(a_ROSN_time_event$date, format = '%Y-%m-%d') 
  ROSN_event$date_time <- as.POSIXct(paste(ROSN_event$date, 
                                           ROSN_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  ROSN_event <- dplyr::select(ROSN_event, -c('date','time'))
  View(ROSN_event)
}
{
  # one-two-three
  ROSN_index <- read.csv("~/Downloads/Diplom/Sorted_Way/rosn_index.csv")
  df_event_cor_ROSN <-  df_event_cor[ROSN_index$X0+1,]
  df_event_cor_ROSN2 <- dplyr::select(df_event_cor_ROSN, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_ROSN2$sec <- rep('0',dim(df_event_cor_ROSN2)[1])
  df_event_cor_ROSN3 <- df_event_cor_ROSN2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_ROSN3$date <- as.POSIXct(df_event_cor_ROSN3$datetime2) 
  df_event_cor_ROSN3$date_time <- as.POSIXct(paste(df_event_cor_ROSN3$date, 
                                                   df_event_cor_ROSN3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_ROSN_t <- dplyr::select(df_event_cor_ROSN3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_ROSN_t_gr <- df_event_cor_ROSN_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_ROSN_t_gr)
  # Объединение
  ROSN_event_res <- left_join(ROSN_event, df_event_cor_ROSN_t_gr, by = 'date_time')
  ROSN_event_res[is.na(ROSN_event_res)] <- 0
  ROSN_event_res$diff_price <- c(0,diff(ROSN_event_res$price))
  ROSN_event_res$lag_price <- lag(ROSN_event_res$price, 
                                  default = ROSN_event_res$price[1])
  ROSN_event_res$r <- ROSN_event_res$diff_price/ROSN_event_res$lag_price*100
  ROSN_event_res$diff_vol <- c(0,diff(ROSN_event_res$vol))
  ROSN_event_res$lag_vol <- lag(ROSN_event_res$vol, 
                                default = ROSN_event_res$vol[1])
  ROSN_event_res$v <- ROSN_event_res$diff_vol/ROSN_event_res$lag_vol*100
  ROSN_event_res_t <- dplyr::select(ROSN_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(ROSN_event_res_t) <- c('date_time','ROSN_r','ROSN_v',
                                  'ROSN_one','ROSN_two','ROSN_three')
  View(ROSN_event_res_t)
}
# WUSH
{
  WUSH_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/WUSH_22_23_t.csv", sep=",")
  WUSH_event <- rbind(WUSH_22_23)
  WUSH_event <- dplyr::select(WUSH_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
  colnames(WUSH_event) <- c('date','time','price','vol')
  WUSH_event$date <- as.character(WUSH_event$date)
  a_WUSH_time_event <- data.frame(str_split_fixed(WUSH_event$date, 
                                                  pattern = "/", n = 3))
  colnames(a_WUSH_time_event) <- c('day','month','year')
  a_WUSH_time_event$year <- paste("20", as.character(a_WUSH_time_event$year), sep="")
  a_WUSH_time_event$date <- paste(a_WUSH_time_event$year, a_WUSH_time_event$month,
                                  a_WUSH_time_event$day, sep = '-')
  WUSH_event$date <- as.Date(a_WUSH_time_event$date, format = '%Y-%m-%d') 
  WUSH_event$date_time <- as.POSIXct(paste(WUSH_event$date, 
                                           WUSH_event$time),  
                                     format = "%Y-%m-%d %H:%M:%S")
  WUSH_event <- dplyr::select(WUSH_event, -c('date','time'))
  View(WUSH_event)
}
{
  # one-two-three
  WUSH_index <- read.csv("~/Downloads/Diplom/Sorted_Way/wush_index.csv")
  df_event_cor_WUSH <-  df_event_cor[WUSH_index$X0+1,]
  df_event_cor_WUSH2 <- dplyr::select(df_event_cor_WUSH, -c('X','true_index',
                                                            'weekday'))
  df_event_cor_WUSH2$sec <- rep('0',dim(df_event_cor_WUSH2)[1])
  df_event_cor_WUSH3 <- df_event_cor_WUSH2 %>% 
    unite(col = "time", c("hour", "minute"), sep = ":")
  df_event_cor_WUSH3$date <- as.POSIXct(df_event_cor_WUSH3$datetime2) 
  df_event_cor_WUSH3$date_time <- as.POSIXct(paste(df_event_cor_WUSH3$date, 
                                                   df_event_cor_WUSH3$time),  
                                             format = "%Y-%m-%d %H:%M")
  df_event_cor_WUSH_t <- dplyr::select(df_event_cor_WUSH3, -c('time','date',
                                                              'datetime2'))
  df_event_cor_WUSH_t_gr <- df_event_cor_WUSH_t %>%
    group_by(date_time) %>%
    summarize(one = sum(count_one),two = sum(count_two), three = sum(count_three)) 
  View(df_event_cor_WUSH_t_gr)
  # Объединение
  WUSH_event_res <- left_join(WUSH_event, df_event_cor_WUSH_t_gr, by = 'date_time')
  WUSH_event_res[is.na(WUSH_event_res)] <- 0
  WUSH_event_res$diff_price <- c(0,diff(WUSH_event_res$price))
  WUSH_event_res$lag_price <- lag(WUSH_event_res$price, 
                                  default = WUSH_event_res$price[1])
  WUSH_event_res$r <- WUSH_event_res$diff_price/WUSH_event_res$lag_price*100
  WUSH_event_res$diff_vol <- c(0,diff(WUSH_event_res$vol))
  WUSH_event_res$lag_vol <- lag(WUSH_event_res$vol, 
                                default = WUSH_event_res$vol[1])
  WUSH_event_res$v <- WUSH_event_res$diff_vol/WUSH_event_res$lag_vol*100
  WUSH_event_res_t <- dplyr::select(WUSH_event_res, c('date_time','r','v',
                                                      'one','two','three'))
  colnames(WUSH_event_res_t) <- c('date_time','WUSH_r','WUSH_v',
                                  'WUSH_one','WUSH_two','WUSH_three')
  View(WUSH_event_res_t)
}
# Индекс МосБиржи 
{
IMOEX_22_23 <- read.csv("~/Downloads/Diplom/Sorted_Way/IMOEX_22_23_t.csv", sep=",")
IMOEX_21_22 <- read.csv("~/Downloads/Diplom/Sorted_Way/IMOEX_21_22_t.csv", sep=",")
IMOEX_20_21 <- read.csv("~/Downloads/Diplom/Sorted_Way/IMOEX_20_21_t.csv", sep=",")
IMOEX_19_20 <- read.csv("~/Downloads/Diplom/Sorted_Way/IMOEX_19_20_t.csv", sep=",")
IMOEX_18_19 <- read.csv("~/Downloads/Diplom/Sorted_Way/IMOEX_18_19_t.csv", sep=",")
IMOEX_17_18 <- read.csv("~/Downloads/Diplom/Sorted_Way/IMOEX_17_18_t.csv", sep=",")
imoex_event <- rbind(IMOEX_17_18, IMOEX_18_19, IMOEX_19_20, IMOEX_20_21,
                     IMOEX_21_22, IMOEX_22_23)
imoex_event <- dplyr::select(imoex_event, c('X.DATE.','X.TIME.',
                                            'X.CLOSE.','X.VOL.'))
colnames(imoex_event) <- c('date','time','price','vol')
imoex_event$date <- as.character(imoex_event$date)
b_time_event <- data.frame(str_split_fixed(imoex_event$date, 
                                           pattern = "/", n = 3))
colnames(b_time_event) <- c('day','month','year')
b_time_event$year <- paste("20", as.character(b_time_event$year), sep="")
View(b_time_event)
b_time_event$date <- paste(b_time_event$year, b_time_event$month,
                           b_time_event$day, sep = '-')
imoex_event$date <- as.Date(b_time_event$date, format = '%Y-%m-%d') 
imoex_event$date_time <- as.POSIXct(paste(imoex_event$date, 
                                          imoex_event$time),  
                                    format = "%Y-%m-%d %H:%M:%S")
imoex_event <- dplyr::select(imoex_event, -c('date','time'))
}
imoex_event$diff_imoex <- c(0,diff(imoex_event$price))
imoex_event$lag_imoex <- lag(imoex_event$price, 
                                   default = imoex_event$price[1])
imoex_event$imoex <- imoex_event$diff_imoex/imoex_event$lag_imoex*100
imoex_event <- dplyr::select(imoex_event, -c('diff_imoex',
                                                         'lag_imoex','vol',
                                                         'price'))
View(imoex_event)
# Соединение в один датафрейм 
df_moex <- imoex_event
View(df_moex)
{
all_event <- left_join(df_moex, ALRS_event_res_t, by = 'date_time')
all_event <- left_join(all_event, UNAC_event_res_t, by = 'date_time')
all_event <- left_join(all_event, YNDX_event_res_t, by = 'date_time')
all_event <- left_join(all_event, LNZL_event_res_t, by = 'date_time')
all_event <- left_join(all_event, SMLT_event_res_t, by = 'date_time')
all_event <- left_join(all_event, SBER_event_res_t, by = 'date_time')
all_event <- left_join(all_event, OZON_event_res_t, by = 'date_time')
all_event <- left_join(all_event, CIAN_event_res_t, by = 'date_time')
all_event <- left_join(all_event, HHRU_event_res_t, by = 'date_time')
all_event <- left_join(all_event, LKOH_event_res_t, by = 'date_time')
all_event <- left_join(all_event, ROSN_event_res_t, by = 'date_time')
all_event <- left_join(all_event, WUSH_event_res_t, by = 'date_time')
}
View(all_event)

all_event$date_time <- as.POSIXlt(all_event$date_time, 
                                  format="%Y-%m-%d  %H:%M:%S")

companies_r <- all_event[,c(1,3,8,13,18,23,28,33,38,43,48,
                            53,58)]
colnames(companies_r) <- c("date_time","ALRS","UNAC","YNDX","LNZL",
                           "SMLT","SBER","OZON","CIAN","HHRU",
                           "LKOH","ROSN","WUSH")
View(companies_r)
# Доходности компаний
zoo_all_event <- read.zoo(companies_r[,],
                          format="%Y-%m-%d  %H:%M:%S", tz='')

zoo_imoex <- read.zoo(all_event[,c(1:3)],
                      format="%Y-%m-%d  %H:%M:%S", tz='')

# Рыночная доходность
zoo_imoex <- zoo(zoo_imoex[,1,drop=F])

# Датафреймы событий
event_buy <- all_event[,c(1,5,10,15,20,25,30,35,40,45,50,55,60)]
colnames(event_buy) <- c("date_time","ALRS","UNAC","YNDX","LNZL",
                         "SMLT","SBER","OZON","CIAN","HHRU",
                         "LKOH","ROSN","WUSH")
View(event_buy)
event_sell <- all_event[,c(1,6,11,16,21,26,31,36,41,46,51,56,61)]
colnames(event_sell) <- c("date_time","ALRS","UNAC","YNDX","LNZL",
                          "SMLT","SBER","OZON","CIAN","HHRU",
                          "LKOH","ROSN","WUSH")
View(event_sell)
event_hold <- all_event[,c(1,7,12,17,22,27,32,37,42,47,52,57,62)]
colnames(event_hold) <- c("date_time","ALRS","UNAC","YNDX","LNZL",
                          "SMLT","SBER","OZON","CIAN","HHRU",
                          "LKOH","ROSN","WUSH")
View(event_hold)

list_buy_companies <- c(rep('ALRS',
                            dim(dplyr::filter(event_buy, ALRS >= 1))[1]),
                        rep('UNAC',
                            dim(dplyr::filter(event_buy, UNAC >= 1))[1]),
                        rep('YNDX',
                            dim(dplyr::filter(event_buy, YNDX >= 1))[1]),
                        rep('LNZL',
                            dim(dplyr::filter(event_buy, LNZL >= 1))[1]),
                        rep('SMLT',
                            dim(dplyr::filter(event_buy, SMLT >= 1))[1]),
                        rep('SBER',
                            dim(dplyr::filter(event_buy, SBER >= 1))[1]),
                        rep('OZON',
                            dim(dplyr::filter(event_buy, OZON >= 1))[1]),
                        rep('CIAN',
                            dim(dplyr::filter(event_buy, CIAN >= 1))[1]),
                        rep('HHRU',
                            dim(dplyr::filter(event_buy, HHRU >= 1))[1]),
                        rep('LKOH',
                            dim(dplyr::filter(event_buy, LKOH >= 1))[1]),
                        rep('ROSN',
                            dim(dplyr::filter(event_buy, ROSN >= 1))[1]),
                        rep('WUSH',
                            dim(dplyr::filter(event_buy, WUSH >= 1))[1]))
list_buy_dates <- c(dplyr::filter(event_buy, ALRS >= 1)$date_time,
                    dplyr::filter(event_buy, UNAC >= 1)$date_time,
                    dplyr::filter(event_buy, YNDX >= 1)$date_time,
                    dplyr::filter(event_buy, LNZL >= 1)$date_time,
                    dplyr::filter(event_buy, SMLT >= 1)$date_time,
                    dplyr::filter(event_buy, SBER >= 1)$date_time,
                    dplyr::filter(event_buy, OZON >= 1)$date_time,
                    dplyr::filter(event_buy, CIAN >= 1)$date_time,
                    dplyr::filter(event_buy, HHRU >= 1)$date_time,
                    dplyr::filter(event_buy, LKOH >= 1)$date_time,
                    dplyr::filter(event_buy, ROSN >= 1)$date_time,
                    dplyr::filter(event_buy, WUSH >= 1)$date_time)
event_list_buy <- data.frame(list_buy_companies, list_buy_dates)
colnames(event_list_buy) <- c('name','when')
View(event_list_buy)

list_sell_companies <- c(rep('ALRS',
                             dim(dplyr::filter(event_sell, ALRS >= 1))[1]),
                         rep('UNAC',
                             dim(dplyr::filter(event_sell, UNAC >= 1))[1]),
                         rep('YNDX',
                             dim(dplyr::filter(event_sell, YNDX >= 1))[1]),
                         rep('LNZL',
                             dim(dplyr::filter(event_sell, LNZL >= 1))[1]),
                         rep('SMLT',
                             dim(dplyr::filter(event_sell, SMLT >= 1))[1]),
                         rep('SBER',
                             dim(dplyr::filter(event_sell, SBER >= 1))[1]),
                         rep('OZON',
                             dim(dplyr::filter(event_sell, OZON >= 1))[1]),
                         rep('CIAN',
                             dim(dplyr::filter(event_sell, CIAN >= 1))[1]),
                         rep('HHRU',
                             dim(dplyr::filter(event_sell, HHRU >= 1))[1]),
                         rep('LKOH',
                             dim(dplyr::filter(event_sell, LKOH >= 1))[1]),
                         rep('ROSN',
                             dim(dplyr::filter(event_sell, ROSN >= 1))[1]),
                         rep('WUSH',
                             dim(dplyr::filter(event_sell, WUSH >= 1))[1]))
list_sell_dates <- c(dplyr::filter(event_sell, ALRS >= 1)$date_time,
                     dplyr::filter(event_sell, UNAC >= 1)$date_time,
                     dplyr::filter(event_sell, YNDX >= 1)$date_time,
                     dplyr::filter(event_sell, LNZL >= 1)$date_time,
                     dplyr::filter(event_sell, SMLT >= 1)$date_time,
                     dplyr::filter(event_sell, SBER >= 1)$date_time,
                     dplyr::filter(event_sell, OZON >= 1)$date_time,
                     dplyr::filter(event_sell, CIAN >= 1)$date_time,
                     dplyr::filter(event_sell, HHRU >= 1)$date_time,
                     dplyr::filter(event_sell, LKOH >= 1)$date_time,
                     dplyr::filter(event_sell, ROSN >= 1)$date_time,
                     dplyr::filter(event_sell, WUSH >= 1)$date_time)
event_list_sell <- data.frame(list_sell_companies, list_sell_dates)
colnames(event_list_sell) <- c('name','when')
View(event_list_sell)

list_hold_companies <- c(rep('ALRS',
                             dim(dplyr::filter(event_hold, ALRS >= 1))[1]),
                         rep('UNAC',
                             dim(dplyr::filter(event_hold, UNAC >= 1))[1]),
                         rep('YNDX',
                             dim(dplyr::filter(event_hold, YNDX >= 1))[1]),
                         rep('LNZL',
                             dim(dplyr::filter(event_hold, LNZL >= 1))[1]),
                         rep('SMLT',
                             dim(dplyr::filter(event_hold, SMLT >= 1))[1]),
                         rep('SBER',
                             dim(dplyr::filter(event_hold, SBER >= 1))[1]),
                         rep('OZON',
                             dim(dplyr::filter(event_hold, OZON >= 1))[1]),
                         rep('CIAN',
                             dim(dplyr::filter(event_hold, CIAN >= 1))[1]),
                         rep('HHRU',
                             dim(dplyr::filter(event_hold, HHRU >= 1))[1]),
                         rep('LKOH',
                             dim(dplyr::filter(event_hold, LKOH >= 1))[1]),
                         rep('ROSN',
                             dim(dplyr::filter(event_hold, ROSN >= 1))[1]),
                         rep('WUSH',
                             dim(dplyr::filter(event_hold, WUSH >= 1))[1]))
list_hold_dates <- c(dplyr::filter(event_hold, ALRS >= 1)$date_time,
                     dplyr::filter(event_hold, UNAC >= 1)$date_time,
                     dplyr::filter(event_hold, YNDX >= 1)$date_time,
                     dplyr::filter(event_hold, LNZL >= 1)$date_time,
                     dplyr::filter(event_hold, SMLT >= 1)$date_time,
                     dplyr::filter(event_hold, SBER >= 1)$date_time,
                     dplyr::filter(event_hold, OZON >= 1)$date_time,
                     dplyr::filter(event_hold, CIAN >= 1)$date_time,
                     dplyr::filter(event_hold, HHRU >= 1)$date_time,
                     dplyr::filter(event_hold, LKOH >= 1)$date_time,
                     dplyr::filter(event_hold, ROSN >= 1)$date_time,
                     dplyr::filter(event_hold, WUSH >= 1)$date_time)
event_list_hold <- data.frame(list_hold_companies, list_hold_dates)
colnames(event_list_hold) <- c('name','when')
View(event_list_hold)

#### Покупка ВСЕ ####
event_study_all_buy <- eventstudy(firm.returns = zoo_all_event,
                                  event.list = event_list_buy,
                                  event.window = 5,
                                  type = "marketModel",
                                  to.remap = TRUE,
                                  remap = "cumsum",
                                  inference = TRUE,
                                  inference.strategy = "bootstrap",
                                  model.args = list(market.returns=zoo_imoex)) 
plot(event_study_all_buy) 

#### Продажа ВСЕ ####
event_study_all_sell <- eventstudy(firm.returns = zoo_all_event,
                                   event.list = event_list_sell,
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 

plot(event_study_all_sell) 

#### Удержание ВСЕ ####
event_study_all_hold <- eventstudy(firm.returns = zoo_all_event,
                                   event.list = event_list_hold,
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 

plot(event_study_all_hold) 

#### Оценка ликвидности ####
names <- c('ALRS','UNAC','YNDX','LNZL','SMLT','SBER',
           'OZON','CIAN','HHRU','LKOH','ROSN','WUSH')
liq <- c(1,3,1,3,1,1,3,3,3,1,1,2)
liquidity <- data.frame(names,liq)
View(liquidity)

#### Низкая-средняя ликвидность ####
zoo_low_event <- zoo_all_event[,liquidity$names[liq!=1]]
event_list_buy_low <- dplyr::filter(event_list_buy, name %in% liquidity$names[liq!=1])
#### Покупка НЕЛИКВИДНЫЕ ####
event_study_low_buy <- eventstudy(firm.returns = zoo_low_event,
                                  event.list = event_list_buy_low,
                                  event.window = 5,
                                  type = "marketModel",
                                  to.remap = TRUE,
                                  remap = "cumsum",
                                  inference = TRUE,
                                  inference.strategy = "bootstrap",
                                  model.args = list(market.returns=zoo_imoex)) 

plot(event_study_low_buy) 

#### Продажа НЕЛИКВИДНЫЕ ####
event_list_sell_low <- dplyr::filter(event_list_sell, name %in% liquidity$names[liq!=1])
event_study_low_sell <- eventstudy(firm.returns = zoo_low_event,
                                   event.list = event_list_sell_low,
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 

plot(event_study_low_sell) 

#### Удержание НЕЛИКВИДНЫЕ ####
event_list_hold_low <- dplyr::filter(event_list_hold, name %in% liquidity$names[liq!=1])
event_study_low_hold <- eventstudy(firm.returns = zoo_low_event,
                                   event.list = event_list_hold_low,
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 

plot(event_study_low_hold) 

#### Высокая ликвидность ####
zoo_high_event <- zoo_all_event[,liquidity$names[liq==1]]
event_list_buy_high <- dplyr::filter(event_list_buy, name %in% liquidity$names[liq==1])
#### Покупка ЛИКВИДНЫЕ ####
event_study_high_buy <- eventstudy(firm.returns = zoo_high_event,
                                   event.list = event_list_buy_high,
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 

plot(event_study_high_buy) 

#### Продажа ЛИКВИДНЫЕ ####
event_list_sell_high <- dplyr::filter(event_list_sell, name %in% liquidity$names[liq==1])
event_study_high_sell <- eventstudy(firm.returns = zoo_high_event,
                                    event.list = event_list_sell_high,
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 

plot(event_study_high_sell) 

#### Удержание ЛИКВИДНЫЕ ####
event_list_hold_high <- dplyr::filter(event_list_hold, name %in% liquidity$names[liq==1])
event_study_high_hold <- eventstudy(firm.returns = zoo_high_event,
                                    event.list = event_list_hold_high,
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 

plot(event_study_high_hold) 

#### По отдельности каждая компания ####
#### 1. ALRS ####
#### Покупка ALRS ####
event_study_ALRS_buy <- eventstudy(firm.returns = zoo(zoo_all_event[,1,drop=F]),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'ALRS'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_ALRS_buy)

#### Продажа ALRS ####
event_study_ALRS_sell <- eventstudy(firm.returns = zoo(zoo_all_event[,1,drop=F]),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'ALRS'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_ALRS_sell)

#### Удержание ALRS ####
event_study_ALRS_hold <- eventstudy(firm.returns = zoo(zoo_all_event[,1,drop=F]),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'ALRS'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_ALRS_hold)

#### 2. UNAC ####
#### Покупка UNAC ####
event_study_UNAC_buy <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,2,drop=F])),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'UNAC'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_UNAC_buy)

#### Продажа UNAC ####
event_study_UNAC_sell <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,2,drop=F])),
                                   event.list = dplyr::filter(event_list_sell,
                                                              name == 'UNAC'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_UNAC_sell)

#### Удержание UNAC ####
event_study_UNAC_hold <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,2,drop=F])),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'UNAC'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_UNAC_hold)

#### 3. YNDX ####
#### Покупка YNDX ####
event_study_YNDX_buy <- eventstudy(firm.returns = zoo(zoo_all_event[,3,drop=F]),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'YNDX'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_YNDX_buy)

#### Продажа YNDX ####
event_study_YNDX_sell <- eventstudy(firm.returns = zoo(zoo_all_event[,3,drop=F]),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'YNDX'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_YNDX_sell)

#### Удержание YNDX ####
event_study_YNDX_hold <- eventstudy(firm.returns = zoo(zoo_all_event[,3,drop=F]),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'YNDX'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_YNDX_hold)

#### 4. LNZL ####
#### Покупка LNZL ####
event_study_LNZL_buy <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,4,drop=F])),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'LNZL'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_LNZL_buy)


#### 5. SMLT ####
#### Покупка SMLT ####
event_study_SMLT_buy <- eventstudy(firm.returns = zoo(zoo_all_event[,5,drop=F]),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'SMLT'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_SMLT_buy)

#### Продажа SMLT ####
event_study_SMLT_sell <- eventstudy(firm.returns = zoo(zoo_all_event[,5,drop=F]),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'SMLT'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_SMLT_sell)


#### Удержание SMLT ####
event_study_SMLT_hold <- eventstudy(firm.returns = zoo(zoo_all_event[,5,drop=F]),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'SMLT'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_SMLT_hold)


#### 6. SBER ####
#### Покупка SBER ####
event_study_SBER_buy <- eventstudy(firm.returns = zoo(zoo_all_event[,6,drop=F]),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'SBER'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_SBER_buy)

#### Продажа SBER ####
event_study_SBER_sell <- eventstudy(firm.returns = zoo(zoo_all_event[,6,drop=F]),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'SBER'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_SBER_sell)

#### Удержание SBER ####
event_study_SBER_hold <- eventstudy(firm.returns = zoo(zoo_all_event[,6,drop=F]),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'SBER'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_SBER_hold)

#### 7. OZON ####
#### Покупка OZON ####
event_study_OZON_buy <- eventstudy(firm.returns = zoo(zoo_all_event[,7,drop=F]),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'OZON'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_OZON_buy)

#### Продажа OZON ####
event_study_OZON_sell <- eventstudy(firm.returns = zoo(zoo_all_event[,7,drop=F]),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'OZON'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_OZON_sell)

#### Удержание OZON ####
event_study_OZON_hold <- eventstudy(firm.returns = zoo(zoo_all_event[,7,drop=F]),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'OZON'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_OZON_hold)

#### 8. CIAN ####
#### Покупка CIAN ####
event_study_CIAN_buy <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,8,drop=F])),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'CIAN'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_CIAN_buy)

#### Продажа CIAN ####
event_study_CIAN_sell <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,8,drop=F])),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'CIAN'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_CIAN_sell)

#### Удержание CIAN ####
event_study_CIAN_hold <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,8,drop=F])),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'CIAN'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_CIAN_hold)

#### 9. HHRU ####
#### Покупка HHRU ####
event_study_HHRU_buy <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,9,drop=F])),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'HHRU'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_HHRU_buy)

#### Удержание HHRU ####
event_study_HHRU_hold <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,9,drop=F])),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'HHRU'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_HHRU_hold)

#### 10. LKOH ####
#### Покупка LKOH ####
event_study_LKOH_buy <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,10,drop=F])),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'LKOH'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_LKOH_buy)

#### Продажа LKOH ####
event_study_LKOH_sell <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,10,drop=F])),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'LKOH'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_LKOH_sell)

#### Удержание LKOH ####
event_study_LKOH_hold <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,10,drop=F])),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'LKOH'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_LKOH_hold)

#### 11. ROSN ####
#### Покупка ROSN ####
event_study_ROSN_buy <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,11,drop=F])),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'ROSN'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_ROSN_buy)

#### Продажа ROSN ####
event_study_ROSN_sell <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,11,drop=F])),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'ROSN'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_ROSN_sell)

#### Удержание ROSN ####
event_study_ROSN_hold <- eventstudy(firm.returns = zoo(zoo_all_event[,11,drop=F]),
                                    event.list = dplyr::filter(event_list_hold,
                                                               name == 'ROSN'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_ROSN_hold)

#### 12. WUSH ####
#### Покупка WUSH ####
event_study_WUSH_buy <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,12,drop=F])),
                                   event.list = dplyr::filter(event_list_buy,
                                                              name == 'WUSH'),
                                   event.window = 5,
                                   type = "marketModel",
                                   to.remap = TRUE,
                                   remap = "cumsum",
                                   inference = TRUE,
                                   inference.strategy = "bootstrap",
                                   model.args = list(market.returns=zoo_imoex)) 
plot(event_study_WUSH_buy)

#### Продажа WUSH ####
event_study_WUSH_sell <- eventstudy(firm.returns = zoo(na.omit(zoo_all_event[,12,drop=F])),
                                    event.list = dplyr::filter(event_list_sell,
                                                               name == 'WUSH'),
                                    event.window = 5,
                                    type = "marketModel",
                                    to.remap = TRUE,
                                    remap = "cumsum",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap",
                                    model.args = list(market.returns=zoo_imoex)) 
plot(event_study_WUSH_sell)



#### Графики для аргумента про стационарность ####
ts_plot(data.frame(as.Date(yndx_res$datetime2),yndx_res$r)[yndx_res$r>=-15,],
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Яндекс',
        Ygrid = T)
ts_plot(data.frame(as.Date(yndx_res$datetime2),yndx_res$price),
        Xtitle = "Время, день",
        Ytitle = "Цена, руб.",
        color = 'darkslategray',
        title = 'Динамика цены акций компании Яндекс',
        Ygrid = T)

#### Создание таблиц с выводами результатов ####
library(kableExtra)
models <- list(alrosa_mod_r,oak_mod_r,yndx_mod_r,lenz_mod_r,
               smlt_mod_r,sber_mod_r,ozon_mod_r,cian_mod_r,
               hhru_mod_r,luk_mod_r,rosn_mod_r,wush_mod_r)
names(models) <- c('ALRS','UNAC','YNDX','SMLT','LNZL','SBER',
                   'OZON','CIAN','HHRU','LKOH','ROSN','WUSH')
modelsummary(models)
tidy(alrosa_mod_r)

{tab <- data.frame(tidy(oak_mod_r)[,1])
tab <- left_join(tab, data.frame(tidy(alrosa_mod_r),coeftest(alrosa_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(oak_mod_r),coeftest(oak_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(yndx_mod_r),coeftest(yndx_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(smlt_mod_r),coeftest(smlt_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(lenz_mod_r),coeftest(lenz_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(sber_mod_r),coeftest(sber_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(ozon_mod_r),coeftest(ozon_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(cian_mod_r),coeftest(cian_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(hhru_mod_r),coeftest(hhru_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(luk_mod_r),coeftest(luk_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(rosn_mod_r),coeftest(rosn_mod_r)[,4])[,c(1:2,4)], by = 'term')
tab <- left_join(tab, data.frame(tidy(wush_mod_r),coeftest(wush_mod_r)[,4])[,c(1:2,4)], by = 'term')
}
View(tab)

tab <- tab %>% mutate_if(is.numeric, round, digits = 2)
for (i in 1:length(tab$term)){
  for (j in list(2,4,6,8,10,12,14,16,18,20,22,24)){
    if (is.na(tab[i,j+1]) == TRUE){
      next
    } else if (tab[i,j+1] <= 0.01){
      tab[i,j] = paste0(as.character(tab[i,j]), '***')
    } else if (0.01 < tab[i,j+1] & tab[i,j+1] <= 0.05){
      tab[i,j] = paste0(as.character(tab[i,j]), '**')
    } else if (0.05 < tab[i,j+1] & tab[i,j+1] <= 0.1){
      tab[i,j] = paste0(as.character(tab[i,j]), '*')
    }
  }
}
tab <- tab[,c(1,2,4,6,8,10,12,14,16,18,20,22,24)]
colnames(tab) <- c('Estimate','ALRS','UNAC','YNDX','SMLT','LNZL','SBER',
                   'OZON','CIAN','HHRU','LKOH','ROSN','WUSH')
tab[13,] <- c(1,alrosa_mod_r$nobs,oak_mod_r$nobs,yndx_mod_r$nobs,
              smlt_mod_r$nobs,lenz_mod_r$nobs,sber_mod_r$nobs,
              ozon_mod_r$nobs,cian_mod_r$nobs,hhru_mod_r$nobs,
              luk_mod_r$nobs,rosn_mod_r$nobs,wush_mod_r$nobs)
tab[14,] <- round(c(2,alrosa_mod_r$aicc,oak_mod_r$aicc,yndx_mod_r$aicc,
              smlt_mod_r$aicc,lenz_mod_r$aicc,sber_mod_r$aicc,
              ozon_mod_r$aicc,cian_mod_r$aicc,hhru_mod_r$aicc,
              luk_mod_r$aicc,rosn_mod_r$aicc,wush_mod_r$aicc),1)
rownames(tab) <- c('AR(1)','AR(2)','AR(3)','AR(4)',
                   'MA(1)','MA(2)','MA(3)','MA(4)',
                   'Intercept','Buy','Sell','Hold',
                   'Observations','AIC')
View(tab)
tab1 <- tab[,2:13]
tab1[is.na(tab1)] <- ""
View(tab1)
#write.csv(tab1,'tab1.csv')

tab1 %>%
  kbl(format = 'html')%>%
  kable_classic(full_width = F, html_font = "Times New Roman",font_size = 11)%>%
  footnote(general = " * p-value < 0.1, ** p-value < 0.05, *** p-value < 0.01") %>%
  cat(., file = "tab1.4.doc")

#### Расчёт частоты появления манипулятивных публикаций ####
id <- read.csv("Downloads/Diplom/Sorted_Way/channel_id_sorted.csv")
freq <- data.frame(id$channel.id[1:dim(id)[1]-1],c(yt,true_preds$x))
colnames(freq) <- c('id','cat')
freq$id <- as.factor(freq$id)
levels(freq$id)
levels(freq$id) <- c('rdv','chern','prof','cheh','birzh')
rdv <- dplyr::filter(freq,id=='rdv')
chern <- dplyr::filter(freq,id=='chern')
prof <- dplyr::filter(freq,id=='prof')
cheh <- dplyr::filter(freq,id=='cheh')
birzh <- dplyr::filter(freq,id=='birzh')
f_rdv <- (1-length(rdv$cat[rdv$cat==0])/length(rdv$cat))*100
f_chern <- (1-length(chern$cat[chern$cat==0])/length(chern$cat))*100
f_prof <- (1-length(prof$cat[prof$cat==0])/length(prof$cat))*100
f_cheh <- (1-length(cheh$cat[cheh$cat==0])/length(cheh$cat))*100
f_birzh <- (1-length(birzh$cat[birzh$cat==0])/length(birzh$cat))*100
freq_count <- data.frame(c('rdv','chern','prof','cheh','birzh'),
                         c(f_rdv,f_chern,f_prof,f_cheh,f_birzh))
View(freq_count)

#### Построение графиков динамики доходностей ####
# Алроса
ts_plot(data.frame(as.Date(alrosa_res$datetime2),alrosa_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Алроса',
        Ygrid = T)
# ОАК
ts_plot(data.frame(as.Date(oak_res$datetime2),oak_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании ОАК',
        Ygrid = T)
# Яндекс
ts_plot(data.frame(as.Date(yndx_res$datetime2),yndx_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Яндекс',
        Ygrid = T)
# Самолет
ts_plot(data.frame(as.Date(smlt_res$datetime2),smlt_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Самолёт',
        Ygrid = T)
# Лензолото
ts_plot(data.frame(as.Date(lenz_res$datetime2),lenz_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Лензолото',
        Ygrid = T)
# Сбербанк
ts_plot(data.frame(as.Date(sber_res$datetime2),sber_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Сбербанк',
        Ygrid = T)
# Озон
ts_plot(data.frame(as.Date(ozon_res$datetime2),ozon_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Озон',
        Ygrid = T)
# Циан
ts_plot(data.frame(as.Date(cian_res$datetime2),cian_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Циан',
        Ygrid = T)
# hh.ru
ts_plot(data.frame(as.Date(hhru_res$datetime2),hhru_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании HeadHunter',
        Ygrid = T)
# Лукойл
ts_plot(data.frame(as.Date(luk_res$datetime2),luk_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Лукойл',
        Ygrid = T)
# Роснефть
ts_plot(data.frame(as.Date(rosn_res$datetime2),rosn_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Роснефть',
        Ygrid = T)
# Whoosh
ts_plot(data.frame(as.Date(wush_res$datetime2),wush_res$r),
        Xtitle = "Время, день",
        Ytitle = "Доходность, %",
        color = 'darkslategray',
        title = 'Динамика доходности акций компании Whoosh',
        Ygrid = T)
