# Created  28 May 2011, Jamie Riggs
# Modified 
         # 01 Jul 2017, Jamie Riggs, rank file obsoleted
		     # 201901 for testing SQL, Rodney Howe 

##########################################################
#####     Libraries
##########################################################

library(data.table)
library(ggplot2)
library(forecast)
library(RMySQL)
library(RSQLite)
library(xtable)
library("reshape2")

# Path <- "C:/Users/Howe/Desktop/SPESI/SSN"
Path <- "C:/Users/davidjayjackson/Documents/GitHub/solar_bulletin"
setwd(Path)
(WD <- getwd())


##########################################################
#####     Functions
##########################################################

fetch <- function(fn,ext) {# fn <- Ex
  infile <- paste0(WD, "/", fn, ".", ext)
  X <- data.frame(read.csv(infile, header=TRUE))
}

FreqProp <- function(factor, factorLabel, dump) {
  table.x <- as.data.frame(table(factor),exclude=c(NA,dump))
  names(table.x)[1] <- c(factorLabel)
  prop <- table.x$Freq / sum(table.x$Freq)
  table.x <- data.frame(table.x, prop)
  sum.x <- colSums(table.x[,2:3])
  new.row <- table.x[1,]
  new.row[1] <- "Total"
  new.row[2:3] <- sum.x
  table.x <- rbind(table.x, new.row)
}

FreqProp2 <- function(factor1, factor2, faclab1, faclab2, dump) {
  table.x <- as.data.frame(table(factor1,factor2,useNA="ifany"))
  names(table.x)[1:2] <- c(faclab1,faclab2)
  prop <- table.x$Freq / sum(table.x$Freq)
  table.x <- data.frame(table.x, prop)
  sum.x <- colSums(table.x[,3:4])
  new.row <- table.x[1,]
  new.row[1:2] <- c("","Total")
  new.row[3:4] <- sum.x
  table.x <- rbind(table.x, new.row)
}

WriteCSV <- function(RdataSet, CSVfileName) {
  outfile <- paste(WD, CSVfileName, sep="/")
  write.csv(RdataSet, file=outfile, row.names=F)
}

##########################################################
#     Import AAVSO data
##########################################################

( yr <- format(Sys.Date(), format="%Y") )
( mo <- as.character(as.numeric(format(Sys.Date(), format="%m"))-1) )

#   check if December's data and correct year and month
if (mo=="0") { 
  yr <- as.numeric(yr)-1
  mo <- 12
}
(Ex <- ifelse(nchar(mo)==1,paste0(yr, "0", mo), paste0(yr,mo)))
(ver <- "00")


#path <- paste0(Path, "/Data")
#setwd(path)
#(WD <- getwd())
X <- fetch(Ex,"csv")
summary(X)
nrow(X)
H <- X[,1:10]     # hold current month's raw data
summary(H)
X <- H

##########################################################
#     Import Obsconst data
##########################################################

(fn <- Ex)
(Ex <- paste0(Ex,"obsconst"))   # use year matched to monthly data
X <- fetch(Ex,"csv")
names(X) <- c("obs","k","ow","name","silso","updated")
X$name <- as.character(X$name)
X$updated <- as.character(X$updated)
r <- as.factor(" ")
X <- data.frame(X,r=r)
summary(X)
nrow(X)
K <- X      # obs const data with rank r and silso added
(Ex <- fn)
rm(r)


setwd(Path)
(WD <- getwd())

##########################################################
#####     Column headers
##########################################################

# rename data file names
names(H) <- c("jd","year","mon","day","obs","name","see","g","s","w")
summary(H)
nrow(H)
X <- H
summary(X)
nrow(X)

H <- X     # hold modified monthly data losing original raw data
# X <- H    # restore if needed

##########################################################
part <- "Scrub"
##########################################################

#####     NA omit     ####################################
(tab0 <- table(is.na(X)))    # looks at every row by column cell
#####     year & month errors     ########################
(tab1 <- FreqProp2(X$mon,X$year,"Month","Year"))    # check year and month, both numeric
#####     day number errors     ##########################
(tab2 <- FreqProp(X$day,"Day"))    # check day range
#####     seeing code errors     #########################
(tab3 <- FreqProp(X$see,"Seeing"))    # check seeing codes
#####     observer code errors     #######################
(tab4 <- FreqProp(X$obs,"obs"))    # check obs codes
#####     merge with observer data (K)    ################
a <- tab4[1:(nrow(tab4)-1),1:2]    # a$Freq has each obs monthly submissions
M <- merge(X,a,by.X="obs",by.a="obs",all=TRUE)
summary(M)
nrow(M)
M
X <- data.frame(M[,2:5],M[,1],M[,6:11])
names(X)[c(5,ncol(X))] <- c("obs","NumObs")
summary(X)
nrow(X)
rm(M)
#####      Check Wolf number     #########################
W <- 10*X$g+X$s
(tab5 <- FreqProp(X$w!=W,"w"))
#####      Hold all modified data     ###################
H <- X    # H from above overwritten

##########################################################
part <- "Submissions" # by observer
##########################################################

(tobs <- FreqProp(H$obs,"obs"))    # check obs codes
WriteCSV(tobs,paste0("Tables/",Ex,ver,part,".csv"))
WriteCSV(tobs,paste0("Reports/spesi/",Ex,ver,part,".txt"))  # for wordpress

##########################################################
part <- "RawMinAvgMax" # Daily averages: all observers
##########################################################

A <- X

B <- aggregate(A$w, by = list(day=A$day), FUN = "min")
B
(C <- cbind(B[,1],tab2[1:nrow(B),2],B[,2]))
B <- aggregate(A$w, by = list(day=A$day), FUN = "mean")
(C <- cbind(C[,1:3],B[,2]))
B <- aggregate(A$w, by = list(day=A$day), FUN = "max")
(C <- data.frame(C[,1:4],B[,2]))
names(C) <- c("Day","Submissions","Minimum","Average","Maximum")
C
rm(A,B)

#####     Use in report     ##############################

(loc <- paste0("Tables/", Ex, ver, part, ".tex"))
print(xtable(as.matrix(C), digits=4, caption=paste(Ex,"Daily Raw Counts"), label="tab:da"), caption.placement="top", include.rownames=F, file=loc)

(loc <- paste0("Reports/Bulletins/",Ex,part,".tex"))
print(xtable(as.matrix(C), digits=c(0,0,0,0,4,0), caption=paste(Ex,"Daily Raw Counts"), label="tab:da"), caption.placement="top", include.rownames=F, file=loc)
WriteCSV(C,paste0("Tables/",Ex,ver,part,".csv"))
WriteCSV(C,paste0("Reports/spesi/",Ex,ver,part,".txt"))   # for wordpress

#####     Unprocessed Counts Graph    #######################

M <- melt(C[,-2], id.vars="Day")#, value.name="Counts", variable.name="Stat")
names(M)[2] <- "Legend"
M[,2] <- relevel(M[,2],ref="Average")

yl <- "Minimum, Average, and Maximum Unprocessed Counts"
xl <- "Day"
(main <- paste(Ex, yl,"vs",xl))
(loc <- paste0("Plots/", Ex, ver, part, xl, ".png"))
gp <- ggplot(M, aes(x=Day, y=value, colour = Legend, linetype=Legend)) +
  geom_line() +
  geom_point(col='grey45') + 
  ggtitle(main) + 
  xlab(xl) + 
  ylab(yl) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(loc, width=8, height=4.5)
(loc <- paste0("Reports/spesi/", Ex, ver, part, xl, ".png"))
ggsave(loc, width=8, height=4.5)
(loc <- paste0("Reports/Bulletins/", Ex, part, xl, ".png"))
ggsave(loc, width=8, height=4.5)
gp

##########################################################
part <- "Merge" # current month to obsconst
##########################################################
# X is current month
# K is obsconst
M <- merge(X,K,by.X="obs",by.K="obs",all=TRUE)
summary(M)
nrow(M)

X <- na.omit(M)    # set to X for export
summary(X)
nrow(X)

path <- paste0(Path,"/Data")
setwd(path)
(WD <- getwd())
outfile <- paste0(WD, "/Shapley", Ex, ver, ".RData")
save(X, file=outfile, ascii=FALSE)

M <- X    # reset X to M with no NA's from above
setwd(Path)
(WD <- getwd())

##########################################################
part <- "Append" # current month to master data set
##########################################################

infile <- paste0(WD, "/", "ymtd.RData")
load(infile)     # loads as X
summary(X)
nrow(X)

##########################################################

X <- rbind(X,M)

path <- paste0(Path,"/Data")
setwd(path)
WD <- getwd()
outfile <- paste0(WD, "/ymtd", Ex, ver, ".RData")
save(X, file=outfile, ascii=FALSE)

setwd(Path)
WD <- getwd()
outfile <- paste(WD, "ymtd.RData", sep="/")
save(X, file=outfile, ascii=FALSE)


##########################################################
part <- "SQL" # current month to master data set
##########################################################
summary(X)
#rm(list=ls())


# Import Daily sunspot Data from http://sidc.be
# And create Data Table
DAILY<-X
#DAILY<-fread("http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv",sep = ';')
# ADD column names
#colnames(DAILY) <- c("Year","Month","Day", "Fdate","Spots", "Sd","Obs" ,"Defin"  )
colnames(DAILY) <- c("obs","jd","year","mon","day","see","g","s","w","r","silso","NumObs","k","ow","name","updated")
# Create Year - Month - Day field
DAILY$Ymd <- as.Date(paste(DAILY$year, DAILY$mon, DAILY$day, sep = "-"))
#
###### MySQL & RMySQL
mydb <- dbConnect(MySQL(),user='root',password='antimatter',dbname="gn",
host='localhost')
#
dbListTables(mydb)
# Drop Tables
dbRemoveTable(mydb,"daily")
dbListTables(mydb)

dbWriteTable(mydb, "DAILY", DAILY, row.names = FALSE)
dbSendStatement(mydb, "ALTER TABLE DAILY MODIFY COLUMN Ymd date")
dbSendStatement(mydb, "ALTER TABLE DAILY MODIFY COLUMN w int")
# Read data back into data.frame
ROD <- dbGetQuery(mydb, "SELECT * FROM daily
                  WHERE Year >=2009")
ROD$Ymd <- as.Date(ROD$Ymd)
dbDisconnect(mydb)
# Create Plot of DAily Sunspots numbers.
plot(ROD$Ymd,ROD$w,type="l",
     main="Daily Sunspot Counts: 2009 - 2018",
     xlab="Years",ylab="Sunspot Counts")


# SQLite: stuff
# 
db <- dbConnect(SQLite(), dbname="Rhowe.sqlite3")
# # SQLite: stuff
# 
dbListTables(db)
# # Creat table and Insert data.frame(overwrites existing table)
# Convert Ymd field to Character for import into sqlite
DAILY$Ymd <- as.character(DAILY$Ymd)
dbWriteTable(db, "DAILY", DAILY,overwrite=TRUE)
HOWE <- dbGetQuery(db, "SELECT * FROM daily
                  WHERE Year >=2009 and Year <=2017")

summary(HOWE)

#
dbDisconnect(db)
#
# Plot for daily counts for cycle 24
#
plot(HOWE$jd,HOWE$g, type="l",
     main="Group Count, Cycle 24",
     xlab=" Julian",ylab="Group Counts")