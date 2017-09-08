setwd("/Users/celina.paudel/Documents/data science")

#Import and extract subset of data
methyl_data <- read.table("GSM1589192_K562.1_2.txt")
subsection <- methyl_data[1:500,]

#combine methylated columns into one string
subsection$x <- paste(subsection$V4, subsection$V5)

#Add methylated data of previous row to current row
V4 <- subsection$V4
V4 <- c(V4[-500])
subsection$X4 <- paste(c(0,V4))
V5 <- subsection$V5
V5 <- c(V5[-500])
subsection$X5 <- paste(c(0,V5))

#Include location of previous row in current row
values <- subsection$V2
values <- c(values[-500])
subsection$n <- paste(c(0,values))

#Subtract distance
subsection$V2 <- (sapply(subsection$V2, as.numeric))
subsection$n <- (sapply(subsection$n, as.numeric))
subsection$distance <- (subsection$V2 - subsection$n)

#make distance of first item 0
subsection[1, 12] = 0

#include combined methylated data of previous row in current row
methyl <- subsection$x
methyl <- c(methyl[-500])
subsection$y <- paste(c(0,methyl))

#Remove all "1 1"
subsection = subsection[subsection$x != "1 1",]

#Create a new column that determines matches
subsection$match <- ((subsection$V4 & subsection$X4 > 0) | (subsection$V5 & subsection$X5 > 0))
subsection$match <- (sapply(subsection$match, as.numeric))
