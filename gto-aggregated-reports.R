#Install libraries
library(tidyverse)
library(readxl)
library(writexl)

#Read in GTO+ aggregated report data file, drop the last two rows
df <- readxl::read_xlsx("gto-rawdata-input.xlsx")
df <- head(df, -2)

#Read in Flop Metadata file
flops <- readxl::read_xlsx("flops.xlsx")



#Convert data into percentages ---> To do


#Append Pio file with updated flop data
output_df <- merge(x = df, y = flops, by.x = "Tree", by.y = "flop2", all.x = TRUE)

#Reorder columns and remove irrelevant ones
col_order <- c("Tree", "variants", "card1", "card2", "card3", "rank1", "rank2", "rank3", "suit1", "suit2", "suit3", "fullhouse", "paired-card", "flush", "suited-cards", "straight", "no-straights", "group", "strategy-grouping", "Equity(*)", "EV", "Bet 180", "Bet 48", "Bet 38", "Bet 16.5","Check")
output_df <- output_df[, col_order]

#Write back to Excel File
writexl::write_xlsx(output_df, "updated-gto-output.xlsx")

#Store that somewhere that can be referenced / downloaded


# Analysis section --------------------

# Counts of Flops

# Full House
output_df |> 
  count(fullhouse, sort = TRUE)

output_df |> 
  filter(fullhouse == 'Paired') |> 
  count(`paired-card`, sort = TRUE)

output_df |> 
  group_by(fullhouse) |> 
  summarize(
    n = n(),
    equity = sum(`Equity(*)` * variants) / sum(variants),
    ev = sum(EV * variants) / sum(variants),
    bet_shove = sum(`Bet 180` * variants) / sum(variants),
    bet_2e = sum(`Bet 48` * variants) / sum(variants),
    bet_large = sum(`Bet 38` * variants) / sum(variants),
    bet_small = sum(`Bet 16.5` * variants) / sum(variants),
    check = sum(`Check` * variants) / sum(variants)
  )

# Flush
output_df |> 
  count(flush, sort = TRUE)

output_df |> 
  filter(flush == 'Two-Tone') |> 
  count(`suited-cards`, sort = TRUE)

# Straight
output_df |> 
  count(straight, sort = TRUE)

output_df |> 
  filter(straight == 'Straight') |> 
  count(`no-straights`, sort = TRUE)

# Group
output_df |> 
  count(group, sort = TRUE)

# Strategy Grouping
output_df |> 
  count(`strategy-grouping`, sort = TRUE)
