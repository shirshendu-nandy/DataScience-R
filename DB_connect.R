library("RMySQL")
ucscDb <- dbConnect(MySQL(), user ="genome", host = "genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;"); dbDisconnect(ucscDb);



## connect to local mysql
on <- dbConnect(MySQL(),user="root",password="********",dbname="try",host="localhost")

dbListTables(con) # to see what all tables the database has

#data(test) (shows error becuase its not yet in R, its still on server)

dbListFields(con, 'test')  #to see what all fields the table has

rs <- dbSendQuery(con, "SELECT * FROM test") #data is still on the server

data <- fetch(rs, n = -1) #using fetch to bring data into R


## hdf5 connections
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)


## web scrapping
con =  url("https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
htmlCode
close(con)
