library(tidyverse)

#Import all the relevant parts (T11, T14, T15, T16, T19, T20)

#read files with exotic delimiters in txt-files (T11, T16) as string (for performance reasons)
einzelteil_t11_string <- read_file("Data/Einzelteil/Einzelteil_T11.txt")
einzelteil_t16_string <- read_file("Data/Einzelteil/Einzelteil_T16.txt")
einzelteil_t20_string <- read_file("Data/Einzelteil/Einzelteil_T20.txt")

#replace exotic delimiters between columns
einzelteil_t11_csv <- gsub("\t", ",", einzelteil_t11_string)
einzelteil_t16_csv <- gsub(" \\| \\| ", ",", einzelteil_t16_string)
einzelteil_t20_csv <- gsub(" \\| \\| ", ",", einzelteil_t20_string)

#einzelteil_t16_csv <- paste("\"\"," , einzelteil_t16_csv, sep = "")
#einzelteil_t20_csv <- paste("\"\"," , einzelteil_t20_csv, sep = "")

#replace exotic delimiters between rows
einzelteil_t11_csv <- gsub("", "\n", einzelteil_t11_csv)
einzelteil_t16_csv <- gsub("\t", "\n", einzelteil_t16_csv)
einzelteil_t20_csv <- gsub(" ", "\n", einzelteil_t20_csv)

#write data into one csv file, respectively
write(einzelteil_t11_csv, file = "Data/Einzelteil/Einzelteil_T11.csv")
write(einzelteil_t16_csv, file = "Data/Einzelteil/Einzelteil_T16.csv")
write(einzelteil_t20_csv, file = "Data/Einzelteil/Einzelteil_T20.csv")

#Import final csv-files. Import with utils package because the readr functions return parsing errors 
#and a lot of data is shown as NA even though there is data. The parsing error happens because the 
#data is split on different columns

#read csv
t11 <- read.csv("Data/Einzelteil/Einzelteil_T11.csv")
t16 <- read.csv("Data/Einzelteil/Einzelteil_T16.csv")
t19 <- read.csv("Data/Einzelteil/Einzelteil_T19.csv")
t20 <- read.csv("Data/Einzelteil/Einzelteil_T20.csv")

#read csv2
t14 <- read.csv2("Data/Einzelteil/Einzelteil_T14.csv")
t15 <- read.csv2("Data/Einzelteil/Einzelteil_T15.csv")

#Renaming columns
t11 <- t11 %>% rename(Produktionsdatum = Produktionsdatum_Origin_01011970)
t14 <- t14 %>% rename(Produktionsdatum = Produktionsdatum_Origin_01011970)
t19 <- t19 %>% rename(Produktionsdatum = Produktionsdatum_Origin_01011970)
t20 <- t20 %>% rename(Produktionsdatum = Produktionsdatum_Origin_01011970)

#Produktionsdatum (in T11, T14, T19, T20) is an integer in days which have to be added to 
#the origin date in origin column
t11$Produktionsdatum <- as.Date(t11$origin, format = "%d-%m-%Y") + t11$Produktionsdatum
t14$Produktionsdatum <- as.Date(t14$origin, format = "%d-%m-%Y") + t14$Produktionsdatum
t19$Produktionsdatum <- as.Date(t19$origin, format = "%d-%m-%Y") + t19$Produktionsdatum
t20$Produktionsdatum <- as.Date(t20$origin, format = "%d-%m-%Y") + t20$Produktionsdatum

#Removing unnecessary columns
t11 <- t11 %>% select(-X1, -origin)
t14 <- t14 %>% select(-X, -X1, -origin)
t15 <- t15 %>% select(-X, -X1)
t16 <- t16 %>% select(-X1)
t19 <- t19 %>% select(-X, -X1, -origin)
t20 <- t20 %>% select(-X1, -origin)

#Splitting the three diferent parts of T15 and T16 into three data sets
t15_part1 <- filter(t15, !is.na(ID_T15.x)) 
t15_part2 <- filter(t15, !is.na(ID_T15.y))

t16_part1 <- filter(t16, !is.na(ID_T16.x)) 
t16_part2 <- filter(t16, !is.na(ID_T16.y))
t16_part3 <- filter(t16, !is.na(ID_T16))

#Remove empty column from each data set 
t15_part1 <- t15_part1[, c(-8:-21)] 
t15_part2 <- t15_part2[, c(-1:-7)]

t16_part1 <- t16_part1[, c(-8:-21)] 
t16_part2 <- t16_part2[, c(-1:-7, -15:-21)]
t16_part3 <- t16_part3[, c(-1:-14)]

#Renaming column names to the same names for all parts
colnames(t15_part2) <- c("ID_T15", "Produktionsdatum", "Herstellernummer","Werksnummer", "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung" )
colnames(t15_part1) <- colnames(t15_part2)


colnames(t16_part3) <- c("ID_T16", "Produktionsdatum", "Herstellernummer","Werksnummer", "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung" )
colnames(t16_part2) <- colnames(t16_part3)
colnames(t16_part1) <- colnames(t16_part3)

#Bind the three datasets into one dataset, respectively
t15 <- rbind(t15_part1, t15_part2)

t16 <- rbind(t16_part1, t16_part2, t16_part3)

#Produktionsdatum and Fehlerhaft_Datum into Date-class
t11$Produktionsdatum <- as.Date(t11$Produktionsdatum, format = "%Y-%m-%d")
t14$Produktionsdatum <- as.Date(t14$Produktionsdatum, format = "%Y-%m-%d")
t15$Produktionsdatum <- as.Date(t15$Produktionsdatum, format = "%Y-%m-%d")
t16$Produktionsdatum <- as.Date(t16$Produktionsdatum, format = "%Y-%m-%d")
t19$Produktionsdatum <- as.Date(t19$Produktionsdatum, format = "%Y-%m-%d")
t20$Produktionsdatum <- as.Date(t20$Produktionsdatum, format = "%Y-%m-%d")

t11$Fehlerhaft_Datum <- as.Date(t11$Fehlerhaft_Datum, format = "%Y-%m-%d")
t14$Fehlerhaft_Datum <- as.Date(t14$Fehlerhaft_Datum, format = "%Y-%m-%d")
t15$Fehlerhaft_Datum <- as.Date(t15$Fehlerhaft_Datum, format = "%Y-%m-%d")
t16$Fehlerhaft_Datum <- as.Date(t16$Fehlerhaft_Datum, format = "%Y-%m-%d")
t19$Fehlerhaft_Datum <- as.Date(t19$Fehlerhaft_Datum, format = "%Y-%m-%d")
t20$Fehlerhaft_Datum <- as.Date(t20$Fehlerhaft_Datum, format = "%Y-%m-%d")

# Moving column Produktionsdatum to the last position in T15 and T16 in order to get the same structure for all datasets
t15 <- t15 %>% select(-Produktionsdatum, Produktionsdatum)
t16 <- t16 %>% select(-Produktionsdatum, Produktionsdatum)