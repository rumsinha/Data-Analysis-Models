library(dplyr)
library(sparklyr)
sc <- spark_connect(master = "local", spark_home = spark_home_dir(version="1.6.2"))

## reading from csv file and loading directly into the spark context

housea_tbl <- spark_read_csv(sc, name = "house_dataa", 
                             path = "C:/Users/rumsinha/Downloads/csv_hus/ss13husa.csv", 
                             header = TRUE, delimiter = ",")


houseb_tbl <- spark_read_csv(sc, name = "house_datab", 
                             path = "C:/Users/rumsinha/Downloads/csv_hus/ss13husb.csv", 
                             header = TRUE, delimiter = ",")

src_tbls(sc)



housedata_a_tbl_df <- tbl(sc, "house_dataa")
glimpse(housedata_a_tbl_df)

dim(housedata_a_tbl_df)

housedata_b_tbl_df <- tbl(sc, "house_datab")
glimpse(housedata_b_tbl_df)

dim(housedata_b_tbl_df)

finaldata <- rbind(housedata_a_tbl_df,housedata_b_tbl_df) ## combining the 2 spark dataframes

dim(finaldata) ## 1476313     231

class(finaldata)

#null value analysis and treatment

# VALP, value of the property is our response variable

finaldata <- finaldata %>% filter(!is.na(VALP)) ## removing all the observations where response variable is null

dim(finaldata) ## 859691    231

finaldata <- finaldata %>% select(DIVISION,REGION,ST,WGTP,ACR,BDSP,ELEP,FULP,GASP,INSP,RMSP,
                                  VALP,WATP,YBL,HINCP,SMOCP,OCPIP, TAXP)

dim(finaldata) ## 859691     19

## drop all null observations 

finaldata <- finaldata %>% filter(!is.na(ACR)) 

dim(finaldata) ## 818003     19

finaldata <- finaldata %>% filter(!is.na(ELEP))
dim(finaldata) ## 807202     19

finaldata <- finaldata %>% filter(!is.na(OCPIP))
dim(finaldata) ## 801032     18

finaldata <- finaldata %>% filter(!is.na(TAXP))
dim(finaldata) ## 801032     18

##creating a new column logVALP as log(VALP)
finaldata <- finaldata %>% select(DIVISION,REGION,ST,WGTP,ACR,BDSP,ELEP,FULP,GASP,INSP,RMSP,
                                  VALP,WATP,YBL,HINCP,SMOCP,OCPIP, TAXP) %>% mutate(logVALP = log(VALP))


finaldata <- finaldata %>% select(DIVISION,REGION,ST,WGTP,ACR,BDSP,ELEP,FULP,GASP,INSP,RMSP,
                                  logVALP,WATP,YBL,HINCP,SMOCP,OCPIP, TAXP)

# Partition the data
partition <- finaldata %>%
  sdf_partition(train = 0.70, test = 0.30, seed = 123)

# Create table references
ml_train_tbl <- partition$train
ml_test_tbl <- partition$test


# linear regression with the important variables
predictors <- c("WGTP","BDSP","ELEP","FULP","GASP","INSP","RMSP","WATP","YBL","HINCP","SMOCP","OCPIP", "TAXP")

ml_fit <- ml_train_tbl %>%
  ml_linear_regression(response = "logVALP", features = c(predictors))  ### spark throwing error 

ml_fit

summary(ml_fit)