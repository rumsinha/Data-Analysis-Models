library(ggplot2)
library(sqldf)
library(stringr)

#Reading the data

#2004 year
election_2004_data <- read.csv("Election_Data_2004.csv")
election_2004_data$Year <- '2004'
head(election_2004_data)

## turnout as numeric feature

election_2004_data$Turnout <-as.numeric(str_trim(str_replace_all(str_replace(election_2004_data$Turnout , "%", ""),",","")))
election_2004_data$Turnout <- as.numeric(election_2004_data$Turnout)
election_2004_data$Turnout[is.na(election_2004_data$Turnout)] <- mean(election_2004_data$Turnout, na.rm=TRUE)/2  ## because mean of turnout exceeding 100%

## NAs handling
election_2004_data$Party <- ifelse(election_2004_data$Party == "BJP","BJP",
                                   ifelse(election_2004_data$Party=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2004_data$Party <- as.factor(election_2004_data$Party)


election_2004_data$PrevParty <- ifelse(election_2004_data$PrevParty == "BJP","BJP",
                                       ifelse(election_2004_data$PrevParty=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2004_data$PrevParty <- as.factor(election_2004_data$PrevParty)


election_2004_data$GSDP[is.na(election_2004_data$GSDP)] <- mean(election_2004_data$GSDP, na.rm=TRUE)
sum(is.na(election_2004_data$GSDP))

election_2004_data$Criminal.Case[is.na(election_2004_data$Criminal.Case)] <- 0
sum(is.na(election_2004_data$Criminal.Case))

election_2004_data$Total.Assets[is.na(election_2004_data$Total.Assets)] <- mean(election_2004_data$Total.Assets, na.rm=TRUE)
sum(is.na(election_2004_data$Total.Assets))
election_2004_data$Total.Assets <- as.numeric(election_2004_data$Total.Assets)

election_2004_data$Liabilities[is.na(election_2004_data$Liabilities)] <- mean(election_2004_data$Liabilities, na.rm=TRUE)
sum(is.na(election_2004_data$Liabilities))
election_2004_data$Liabilities <- as.numeric(election_2004_data$Liabilities)

election_2004_data$CandidateAge <- as.numeric(as.character(election_2004_data$CandidateAge))
election_2004_data$CandidateAge[is.na(election_2004_data$CandidateAge)] <- mean(election_2004_data$CandidateAge, na.rm=TRUE)
sum(is.na(election_2004_data$CandidateAge))

election_2004_data$GrowthRateAgri <- as.numeric(election_2004_data$GrowthRateAgri)
election_2004_data$GrowthRateAgri[is.na(election_2004_data$GrowthRateAgri)] <- mean(election_2004_data$GrowthRateAgri, na.rm=TRUE)
sum(is.na(election_2004_data$GrowthRateAgri))

summary(election_2004_data)
str(election_2004_data)


#2009 year
election_2009_data <- read.csv("Election_Data_2009.csv")
election_2009_data$Year <- '2009'
head(election_2009_data)

## turnout as numeric feature

election_2009_data$Turnout <-as.numeric(str_trim(str_replace_all(str_replace(election_2009_data$Turnout , "%", ""),",","")))
election_2009_data$Turnout <- as.numeric(election_2009_data$Turnout)
election_2009_data$Turnout[is.na(election_2009_data$Turnout)] <- mean(election_2009_data$Turnout, na.rm=TRUE)/2

## NAs handling
election_2009_data$Party <- ifelse(election_2009_data$Party == "BJP","BJP",
                                   ifelse(election_2009_data$Party=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2009_data$Party <- as.factor(election_2009_data$Party)


election_2009_data$PrevParty <- ifelse(election_2009_data$PrevParty == "BJP","BJP",
                                       ifelse(election_2009_data$PrevParty=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2009_data$PrevParty <- as.factor(election_2009_data$PrevParty)


election_2009_data$GSDP[is.na(election_2009_data$GSDP)] <- mean(election_2009_data$GSDP, na.rm=TRUE)
sum(is.na(election_2009_data$GSDP))

election_2009_data$Criminal.Case[is.na(election_2009_data$Criminal.Case)] <- 0
sum(is.na(election_2009_data$Criminal.Case))

election_2009_data$Total.Assets[is.na(election_2009_data$Total.Assets)] <- mean(election_2009_data$Total.Assets, na.rm=TRUE)
sum(is.na(election_2009_data$Total.Assets))
election_2009_data$Total.Assets <- as.numeric(election_2009_data$Total.Assets)

election_2009_data$Liabilities[is.na(election_2009_data$Liabilities)] <- mean(election_2009_data$Liabilities, na.rm=TRUE)
sum(is.na(election_2009_data$Liabilities))
election_2009_data$Liabilities <- as.numeric(election_2009_data$Liabilities)

election_2009_data$CandidateAge <- as.numeric(as.character(election_2009_data$CandidateAge))
election_2009_data$CandidateAge[is.na(election_2009_data$CandidateAge)] <- mean(election_2009_data$CandidateAge, na.rm=TRUE)
sum(is.na(election_2009_data$CandidateAge))

election_2009_data$GrowthRateAgri <- as.numeric(election_2009_data$GrowthRateAgri)
election_2009_data$GrowthRateAgri[is.na(election_2009_data$GrowthRateAgri)] <- mean(election_2009_data$GrowthRateAgri, na.rm=TRUE)
sum(is.na(election_2009_data$GrowthRateAgri))

summary(election_2009_data)
str(election_2009_data)

#2014 year
election_2014_data <- read.csv("Election_Data_2014.csv")
election_2014_data$Year <- '2014'
head(election_2014_data)

## turnout as numeric feature

election_2014_data$Turnout <-as.numeric(str_trim(str_replace_all(str_replace(election_2014_data$Turnout , "%", ""),",","")))
election_2014_data$Turnout <- as.numeric(election_2014_data$Turnout)
election_2014_data$Turnout[is.na(election_2014_data$Turnout)] <- mean(election_2014_data$Turnout, na.rm=TRUE)/2

## NAs handling
election_2014_data$Party <- ifelse(election_2014_data$Party == "BJP","BJP",
                                   ifelse(election_2014_data$Party=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2014_data$Party <- as.factor(election_2014_data$Party)


election_2014_data$PrevParty <- ifelse(election_2014_data$PrevParty == "BJP","BJP",
                                       ifelse(election_2014_data$PrevParty=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2014_data$PrevParty <- as.factor(election_2014_data$PrevParty)


election_2014_data$GSDP[is.na(election_2014_data$GSDP)] <- mean(election_2014_data$GSDP, na.rm=TRUE)
sum(is.na(election_2014_data$GSDP))

election_2014_data$Criminal.Case[is.na(election_2014_data$Criminal.Case)] <- 0
sum(is.na(election_2014_data$Criminal.Case))

election_2014_data$Total.Assets[is.na(election_2014_data$Total.Assets)] <- mean(election_2014_data$Total.Assets, na.rm=TRUE)
sum(is.na(election_2014_data$Total.Assets))
election_2014_data$Total.Assets <- as.numeric(election_2014_data$Total.Assets)

election_2014_data$Liabilities[is.na(election_2014_data$Liabilities)] <- mean(election_2014_data$Liabilities, na.rm=TRUE)
sum(is.na(election_2014_data$Liabilities))
election_2014_data$Liabilities <- as.numeric(election_2014_data$Liabilities)

election_2014_data$CandidateAge <- as.numeric(as.character(election_2014_data$CandidateAge))
election_2014_data$CandidateAge[is.na(election_2014_data$CandidateAge)] <- mean(election_2014_data$CandidateAge, na.rm=TRUE)
sum(is.na(election_2014_data$CandidateAge))

election_2014_data$GrowthRateAgri <- as.numeric(election_2014_data$GrowthRateAgri)
election_2014_data$GrowthRateAgri[is.na(election_2014_data$GrowthRateAgri)] <- mean(election_2014_data$GrowthRateAgri, na.rm=TRUE)
sum(is.na(election_2014_data$GrowthRateAgri))

summary(election_2014_data)
str(election_2014_data)


#Combine the data
final_election_data <- rbind(election_2004_data,election_2009_data,election_2014_data)
head(final_election_data)
tail(final_election_data)

#Exploratory Data Analysis

# Average turnout for BJP in the year 2004, 2009 and 2014
bjp_data <- sqldf('select Year,avg(Turnout) AverageTurnout
                   from final_election_data
                   where Party == "BJP"
                   group by Year')

bjp_data

ggplot(data=bjp_data, aes(x=Year, y=AverageTurnout, fill=Year)) +
  geom_bar( stat="identity")

# Average turnout for INC in the year 2004, 2009 and 2014
inc_data <- sqldf('select Year,avg(Turnout) AverageTurnout
                   from final_election_data
                  where Party == "INC"
                  group by Year')

inc_data

ggplot(data=inc_data, aes(x=Year, y=AverageTurnout, fill=Year)) +
  geom_bar( stat="identity")


#Average turnout for Others in the year 2004, 2009 and 2014
others_data <- sqldf('select Year,avg(Turnout) AverageTurnout
                   from final_election_data
                  where Party == "Others"
                  group by Year')

others_data

ggplot(data=others_data, aes(x=Year, y=AverageTurnout, fill=Year)) +
  geom_bar( stat="identity")

#Average turnout for BJP in the year 2004, 2009 and 2014 based on Gender
bjp_data <- sqldf('select Year,Gender, avg(Turnout) AverageTurnout
                   from final_election_data
                  where Party == "BJP"
                  group by Year, Gender')

bjp_data

ggplot(data=bjp_data, aes(x=Year, y=AverageTurnout, fill=Gender)) +
  geom_bar( stat="identity")

#Average turnout for INC in the year 2004, 2009 and 2014 based on Gender
inc_data <- sqldf('select Year,Gender,avg(Turnout) AverageTurnout
                  from final_election_data
                  where Party == "INC"
                  group by Year, Gender')

inc_data

ggplot(data=inc_data, aes(x=Year, y=AverageTurnout, fill=Gender)) +
  geom_bar( stat="identity")

#Average turnout for Others in the year 2004, 2009 and 2014 based on Gender
others_data <- sqldf('select Year,Gender,avg(Turnout) AverageTurnout
                     from final_election_data
                     where Party == "Others"
                     group by Year, Gender')

others_data

ggplot(data=others_data, aes(x=Year, y=AverageTurnout, fill=Gender)) +
  geom_bar( stat="identity")

## Average turnout for BJP in the year 2004, 2009 and 2014 based on Gender
year_2004_data <- sqldf('select count(distinct(Constituency)) seats_count, Party, Year
                   from final_election_data
                   where Year == "2004"
                   group by Party')

year_2004_data

ggplot(data=year_2004_data, aes(x=Party, y=seats_count)) +
  geom_bar(aes(fill=Party),stat="identity")

## 2009 and Total Seats for BJP, INC and Others
year_2009_data <- sqldf('select count(distinct(Constituency)) seats_count, Party, Year
                   from final_election_data
                        where Year == "2009"
                        group by Party')

year_2009_data

ggplot(data=year_2009_data, aes(x=Party, y=seats_count)) +
  geom_bar(aes(fill=Party),stat="identity")

## 2014 and Total Seats for BJP, INC and Others
year_2014_data <- sqldf('select count(distinct(Constituency)) seats_count, Party, Year
                   from final_election_data
                        where Year == "2014"
                        group by Party')

year_2014_data

ggplot(data=year_2014_data, aes(x=Party, y=seats_count)) +
  geom_bar(aes(fill=Party),stat="identity")

##Turnout versus Candidate Age
plot(final_election_data$CandidateAge,final_election_data$Turnout)

##Turnout versus Criminal Cases
plot(final_election_data$Criminal.Case,final_election_data$Turnout)

##Turnout versus GSDP
plot(final_election_data$GSDP,final_election_data$Turnout)

## Turnout versus GrowthRateAgri
plot(final_election_data$GrowthRateAgri,final_election_data$Turnout)

## Average Turnout across different Education Levels
turnoutandeducationdata <- sqldf('select avg(Turnout) AverageTurnout, Education from final_election_data
                                 group by Education')
turnoutandeducationdata

pie(turnoutandeducationdata$AverageTurnout, labels = turnoutandeducationdata$Education, main="Pie Chart of Average Turnout and Votes")
