#Install libraries 
library(dplyr)
library(tidyverse)
library(xlsx)

#Create card components
ranks <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
suits <- c("s", "h", "d", "c")
sorting.values <- c(13:1)
sorting.value.pair <- data.frame(ranks, sorting.values)

#Create a list of all 52 cards
cards <- c()
for (r in ranks) {
  for (s in suits) {
    cards <- append(cards,paste(r, s, sep = ""))
  }
}
rm(r, s)

#Create a data frame with all Flops
flops <- as.data.frame(t(combn(cards, 3)))
colnames(flops) <- c("card1", "card2", "card3")

#Create values to sort in data frame
flops$rank1 <- substr(flops$card1, 1,1)
flops$rank2 <- substr(flops$card2, 1,1)
flops$rank3 <- substr(flops$card3, 1,1)
flops$suit1 <- substr(flops$card1, 2,2)
flops$suit2 <- substr(flops$card2, 2,2)
flops$suit3 <- substr(flops$card3, 2,2)
flops$value1 <- sorting.value.pair$sorting.values[match(flops$rank1, sorting.value.pair$ranks)]
flops$value2 <- sorting.value.pair$sorting.values[match(flops$rank2, sorting.value.pair$ranks)]
flops$value3 <- sorting.value.pair$sorting.values[match(flops$rank3, sorting.value.pair$ranks)]

#Sorts flops descending by card 1, card 2, and card 3
flops <- flops[order(-flops$value1, -flops$value2, -flops$value3),]

#Adds an index and writes entire flop
flops$index <- c(1:22100)
flops$flop <- paste(flops$card1, flops$card2, flops$card3, sep = " ")

#Add Paired-Low Card, Paired-High Card, Unpaired, Trips
flops$fullhouse <- case_when(
  flops$rank1 == flops$rank2 & flops$rank2 == flops$rank3 ~ "Trips",
  flops$rank1 == flops$rank2 & flops$rank2 != flops$rank3 ~ "Paired: High",
  flops$rank1 != flops$rank2 & flops$rank2 == flops$rank3 ~ "Pared: Low",
  TRUE ~ "Unpaired")

#Add Rainbow, Two-Tone (top/bottom, top two, bottom two), Monotone
flops$flush <- case_when(
  flops$suit1 == flops$suit2 & flops$suit2 == flops$suit3 ~ "Monotone",
  flops$suit1 == flops$suit2 & flops$suit2 != flops$suit3 ~ "Two-Tone: Top-Two",
  flops$suit1 != flops$suit2 & flops$suit2 == flops$suit3 ~ "Two-Tone: Bottom-Two",
  flops$suit1 == flops$suit3 & flops$suit1 != flops$suit2 ~ "Two-Tone: Top & Bottom",
  TRUE ~ "Rainbow")

#Add Dry, Gutshot, OESD, Straight-1, Straight-2, Straight-3 --> I did this manually for now, come back to this

#Re-import the flops data table here with flops, grouping, and others added
flops <- readxl::read_xlsx("flops.xlsx")

#Add variant numbers for each board
flops$variants <- case_when(
  flops$flush == "Monotone" ~ 4,
  flops$flush == "Two-Tone" ~ 12,
  flops$flush == "Rainbow" & flops$fullhouse == "Unpaired" ~ 24,
  flops$flush == "Rainbow" & flops$fullhouse == "Paired" ~ 12,
  TRUE ~ 4
)

#Standardize Flops
flops$standardized_flops <- case_when(
  str_sub(flops$flop, start = 2, end = 2) == str_sub(flops$flop, start = 5, end = 5) & str_sub(flops$flop, start = 2, end = 2) == str_sub(flops$flop, start = 8, end = 8) & str_sub(flops$flop, start = 5, end = 5) == str_sub(flops$flop, start = 8, end = 8) ~ 
    paste(str_sub(flops$flop, start = 1, end = 1),"x ",str_sub(flops$flop, start = 4, end = 4),"x ",str_sub(flops$flop, start = 7, end = 7),"x",sep=""),
  str_sub(flops$flop, start = 2, end = 2) == str_sub(flops$flop, start = 5, end = 5) & str_sub(flops$flop, start = 2, end = 2) != str_sub(flops$flop, start = 8, end = 8) & str_sub(flops$flop, start = 5, end = 5) != str_sub(flops$flop, start = 8, end = 8) ~ 
    paste(str_sub(flops$flop, start = 1, end = 1),"x ",str_sub(flops$flop, start = 4, end = 4),"x ",str_sub(flops$flop, start = 7, end = 7),"y",sep=""),
  str_sub(flops$flop, start = 2, end = 2) != str_sub(flops$flop, start = 5, end = 5) & str_sub(flops$flop, start = 2, end = 2) != str_sub(flops$flop, start = 8, end = 8) & str_sub(flops$flop, start = 5, end = 5) == str_sub(flops$flop, start = 8, end = 8) ~ 
    paste(str_sub(flops$flop, start = 1, end = 1),"x ",str_sub(flops$flop, start = 4, end = 4),"y ",str_sub(flops$flop, start = 7, end = 7),"y",sep=""),
  str_sub(flops$flop, start = 2, end = 2) != str_sub(flops$flop, start = 5, end = 5) & str_sub(flops$flop, start = 2, end = 2) == str_sub(flops$flop, start = 8, end = 8) & str_sub(flops$flop, start = 5, end = 5) != str_sub(flops$flop, start = 8, end = 8) ~ 
    paste(str_sub(flops$flop, start = 1, end = 1),"x ",str_sub(flops$flop, start = 4, end = 4),"y ",str_sub(flops$flop, start = 7, end = 7),"x",sep=""),
  str_sub(flops$flop, start = 2, end = 2) != str_sub(flops$flop, start = 5, end = 5) & str_sub(flops$flop, start = 2, end = 2) != str_sub(flops$flop, start = 8, end = 8) & str_sub(flops$flop, start = 5, end = 5) != str_sub(flops$flop, start = 8, end = 8) ~ 
    paste(str_sub(flops$flop, start = 1, end = 1),"x ",str_sub(flops$flop, start = 4, end = 4),"y ",str_sub(flops$flop, start = 7, end = 7),"z",sep="")
)

#Output Flops to Excel
write_xlsx(flops, "flops.xlsx")
