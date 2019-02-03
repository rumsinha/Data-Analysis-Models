## 2004
winning_netas_data <- read.csv("winning_netas_2004_data.csv",stringsAsFactors = FALSE)
winning_netas_data <- na.omit(winning_netas_data)
winning_netas_data$Total.Assets <- iconv(winning_netas_data$Total.Assets, 'utf-8', 'ascii', sub='')
winning_netas_data$Liabilities <- iconv(winning_netas_data$Liabilities, 'utf-8', 'ascii', sub='')
head(winning_netas_data)
tail(winning_netas_data)
library(stringr)
winning_netas_data$Total.Assets <-as.numeric(str_trim(str_replace_all(str_replace( winning_netas_data$Total.Assets  , "Rs", ""),",","")))
winning_netas_data$Liabilities<-as.numeric(str_trim(str_replace_all(str_replace( winning_netas_data$Liabilities , "Rs", ""),",","")))


head(winning_netas_data)
tail(winning_netas_data)

write.csv(winning_netas_data,"cleandatanetas2004.csv")

## 2009
winning_netas_data <- read.csv("neta2009info.csv",stringsAsFactors = FALSE)
winning_netas_data <- na.omit(winning_netas_data)
winning_netas_data$Total.Assets <- iconv(winning_netas_data$Total.Assets, 'utf-8', 'ascii', sub='')
winning_netas_data$Liabilities <- iconv(winning_netas_data$Liabilities, 'utf-8', 'ascii', sub='')
head(winning_netas_data)
tail(winning_netas_data)
library(stringr)
winning_netas_data$Total.Assets <-as.numeric(str_trim(str_replace_all(str_replace( winning_netas_data$Total.Assets  , "Rs", ""),",","")))
winning_netas_data$Liabilities<-as.numeric(str_trim(str_replace_all(str_replace( winning_netas_data$Liabilities , "Rs", ""),",","")))


head(winning_netas_data)
tail(winning_netas_data)

write.csv(winning_netas_data,"cleandatanetas2009.csv")

## 2014
winning_netas_data <- read.csv("neta2014info.csv",stringsAsFactors = FALSE)
winning_netas_data <- na.omit(winning_netas_data)
winning_netas_data$Total.Assets <- iconv(winning_netas_data$Total.Assets, 'utf-8', 'ascii', sub='')
winning_netas_data$Liabilities <- iconv(winning_netas_data$Liabilities, 'utf-8', 'ascii', sub='')
head(winning_netas_data)
tail(winning_netas_data)
library(stringr)
winning_netas_data$Total.Assets <-as.numeric(str_trim(str_replace_all(str_replace( winning_netas_data$Total.Assets  , "Rs", ""),",","")))
winning_netas_data$Liabilities<-as.numeric(str_trim(str_replace_all(str_replace( winning_netas_data$Liabilities , "Rs", ""),",","")))


head(winning_netas_data)
tail(winning_netas_data)

write.csv(winning_netas_data,"cleandatanetas2014.csv")
