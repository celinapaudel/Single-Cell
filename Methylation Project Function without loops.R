setwd("/home/local/ESERVICES/cp6cc/Documents/DNA Methylation")
library(data.table)
library(dplyr)
findMatches <- function(match) {
  
  #Import data
  methyl_data <- match
  
  #combine methylated columns into one string
  methyl_data[,8] <- paste(methyl_data[,4], methyl_data[,5])
  
  View(methyl_data)
  #Remove all "1 1"
  methyl_data = methyl_data[methyl_data[,8] != "1 1",]
  
  #Add methylated data of previous row to current row
  V4 <- methyl_data[,4]
  V4 <- c(V4[-nrow(methyl_data)])
  methyl_data[,9] <- paste(c(0,V4))
  V5 <- methyl_data[,5]
  V5 <- c(V5[-nrow(methyl_data)])
  methyl_data[,10] <- paste(c(0,V5))
  
  #Include location of previous row in current row
  values <- methyl_data[,2]
  values <- c(values[-(NROW(values))])
  methyl_data[,11] <- paste(c(0,values))
  
  #Subtract distance values
  methyl_data[,2] <- (sapply(methyl_data[,2], as.numeric))
  methyl_data[,11] <- (sapply(methyl_data[,11], as.numeric))
  methyl_data[,12] <- (methyl_data[,2] - methyl_data[,11])
  
  #make distance of first item 0
  methyl_data[1, 12] = 0
  
  #Create a new column that determines matches
  methyl_data[,13] <- ((methyl_data[,4] & methyl_data[,9] > 0) | (methyl_data[,5] & methyl_data[,10] > 0))
  methyl_data[,13] <- (sapply(methyl_data[,13], as.numeric))
  
  #Get rid of distance values that are less than 0
  methyl_data = methyl_data[order(methyl_data[,12]),]
  methyl_data = methyl_data[methyl_data[,12] > 0,]
  
  #Create a new list with unique distance values
  distance <- unique(methyl_data[,12])
  
  View(methyl_data)
  
  #Count how many of each distance and how many matches there are
  total = c()
  matches = c()
  
  dl = length(distance)
  ml = length(methyl_data[,12])
  
  df = data.frame(distance = methyl_data[, 12], match = methyl_data[,13])
  distance = df[,1]
  match = df[,2]
 
  final <<- data.frame(distance=unique(df$distance),
             frequency=with(df, tapply(match, distance, length)),
             matches=with(df, tapply(match, distance, sum)))
  
  final[,4] <<- final[,3]/final[,2]
  
}

md <- read.table("GSM1589192_K562.1_2.txt")
subsection <- md[1:50,]
findMatches(md)
subset = final[1:10000,]
plot(subset[,1], subset[,4], xlab = "Distance", ylab = "Percentage of Matches")