# Adstock Calculator

# get data: format is csv, single column of integer values for media placed in consecutive periods.
# could be impressions, or views, or spend, etc.
media <- read.csv('input/adstock_data.csv', header=F)$V1

# manually set these three vars
halflife_days <- 3 # estimated days for media to lose 50% effect
period_days <- 1 # 7 == 1 week
periods_addl <- 2 # additional periods to calculate after final media placement

# more vars
periods <- length(media)
periods_count <- periods + periods_addl
df <- data.frame(matrix(ncol=periods_count, nrow=0))

# for each element, create a vector and append values from exponential decay formula.
for(m in seq_along(media)){
  
  decay_vec<- c(media[m])
  
  for(p in 1:(periods_count-m)){
    
    decay_vec <- append(decay_vec, round(exp(log(0.5) * p / halflife_days * period_days) * media[m]))
    
  }
  
  # pad each consecutive vector to the right by m-1 elements, append to df
  padded_vec <- append(integer(m-1), decay_vec)
  df <- rbind(df, padded_vec)
  
}

# fix df column labels
colnames(df) <- c(1:periods_count)

# sum up
adstock <- colSums(df)

# pad the media vector and merge to final df
media <- append(media, integer(length(adstock)-length(media)))
adstock_table <- data.frame(media, adstock)

# output
print(adstock)
write.csv(adstock, file='output/adstock_totals.csv') # just totals
write.csv(adstock_table, file='output/adstock_table.csv') # table

# viz .. not the prettiest
period_labels <- as.numeric(row.names(adstock_table))

ggplot(adstock_table) +
  geom_area(aes(x=period_labels, y=adstock), fill='green', alpha=.5, stat='identity') +
  geom_bar(aes(x=period_labels, y=media), fill='blue', stat='identity') +
  ggtitle('Media and Adstock Effect') + xlab('period') + ylab('effect') +
  scale_x_continuous(labels = period_labels, breaks=period_labels) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-6)) 