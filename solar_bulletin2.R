library(data.table)
library(ggplot2)
library(forecast)
library(RMySQL)
# library(xtable)
# library("reshape2")
#
rm(list = ls()) # Delete old objects
# beginn heavy lifting
# Read in data tables
WHOM<- fread("./Observers.csv")
DAILY <- fread("./Observations.csv")
DAILY$ymd <- as.Date(paste(DAILY$YEAR, DAILY$MONTH, DAILY$DAY, sep = "-"))
colnames(DAILY) <- c("jd","year","mon","day","obs","name","see","g","s","W","ymd")
#
# MySql connect statments (delete and create)
mydb <- dbConnect(MySQL(),user='root',password='dJj12345',dbname="gn",
                  host='localhost')
# Import data.frames to db
#
dbListTables(mydb)
# Drop Tables

# Import Daily numbers

dbRemoveTable(mydb,"daily")
dbWriteTable(mydb, "DAILY", DAILY, row.names = FALSE)
# Changed Ymd and  w(olf) field types
dbSendStatement(mydb,"ALTER table daily add column k real")
dbSendStatement(mydb, "ALTER TABLE DAILY MODIFY COLUMN Ymd date")
dbSendStatement(mydb, "ALTER TABLE DAILY MODIFY COLUMN w int")

# Create Observer table
dbRemoveTable(mydb,"whom")
dbWriteTable(mydb, "WHOM", WHOM, row.names = FALSE)
dbListTables(mydb)
dbDisconnect(mydb)
