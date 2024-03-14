#### Пакеты ####
# install.packages("readxl")
library(readxl)

#### Выгрузка данных ####
text_data <- read_excel("Downloads/Diplom/Sorted_Way/Combined_telegram_data.xlsx", 
                   col_names = FALSE)
View(text_data)

#### Чистка текстовых данных ####
text_data <- text_data[,15:20]
text_data <- text_data[-c(2,4,5)]
colnames(text_data) <- c("channel id","datetime","publication")
text_data$`channel id`<- gsub("[[:punct:]]", "", text_data$`channel id`)
text_data$datetime <- gsub(": datetime.datetime","",text_data$datetime)
text_data$datetime <- gsub(", tzinfo=datetime.timezone.utc),","",text_data$datetime)
text_data$datetime <- sub('.', '', text_data$datetime)
text_data_proc <- na.omit(text_data[-c(1),])
View(text_data_proc)

#### Запись файла для скрипта Python TF-IDF ####
write.csv(text_data_proc, 'text_data_proc.csv')
