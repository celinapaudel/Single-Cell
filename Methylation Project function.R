setwd("/Users/celina.paudel/Documents/data science")

findMatches <- function(match) {

#Import data
methyl_data <- read.table(match)

#combine methylated columns into one string
methyl_data$x <- paste(methyl_data$V4, methyl_data$V5)

#Remove all "1 1"
methyl_data = methyl_data[methyl_data$x != "1 1",]

#Add methylated data of previous row to current row
V4 <- methyl_data$V4
V4 <- c(V4[-nrow(methyl_data)])
methyl_data$X4 <- paste(c(0,V4))
V5 <- methyl_data$V5
V5 <- c(V5[-nrow(methyl_data)])
methyl_data$X5 <- paste(c(0,V5))

#Include location of previous row in current row
values <- methyl_data$V2
values <- c(values[-(NROW(values))])
methyl_data$n <- paste(c(0,values))

#Subtract distance values
methyl_data$V2 <- (sapply(methyl_data$V2, as.numeric))
methyl_data$n <- (sapply(methyl_data$n, as.numeric))
methyl_data$distance <- (methyl_data$V2 - methyl_data$n)

#make distance of first item 0
methyl_data[1, 12] = 0

#Create a new column that determines matches
methyl_data$m <- ((methyl_data$V4 & methyl_data$X4 > 0) | (methyl_data$V5 & methyl_data$X5 > 0))
methyl_data$m <- (sapply(methyl_data$m, as.numeric))

#Get rid of distance values that are less than 0
methyl_data = methyl_data[order(methyl_data$distance),]
methyl_data = methyl_data[methyl_data$distance > 0,]

#Create a new list with unique distance values
distance <- unique(methyl_data$distance)

View(methyl_data)

#Count how many of each distance and how many matches there are
total = 0
matches = 0

#Loop doesn't work properly
for (d in distance) {
  tcounter = 0
  mcounter = 0
  for (x in methyl_data$distance){
    if (distance[d] == methyl_data[x, 12]) {
      tcounter <- tcounter + 1
      if (methyl_data[x, 13] == 1) {
        mcounter <- mcounter + 1
      }
    }
  }
  total = tcounter
  matches = mcounter
}

#Create a table with the frequency of distances and matches
table = cbind(distance, total, matches)

#Add a column to table with percentage of matches
table$percentage = (table$matches)/(table$total)
 
}

findMatches("GSM1589192_K562.1_2.txt")
methyl_data[1,8]