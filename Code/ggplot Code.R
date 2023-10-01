### Importing Libraries ###

install.packages('ggplot2')
install.packages('ggthemes')
install.packages('plotly')
install.packages('ggridges')
library(ggridges)
library(ggplot2)
library(plotly)
library(ggthemes)

head(salary)

### Checking for density of age ###

salary %>%
  group_by(salary) %>%
  ggplot(aes(x = age, fill = salary)) + geom_density(alpha = 0.5) + theme_bw() + annotate('text', label = c('Salary Affected', 'by Age?'),
                                                                                          x = rep(75,2), y =c(0.027, 0.025))

salary %>%
  ggplot(aes(x = education, fill = salary)) + geom_bar(position = 'fill', col = 'black')

salary %>%
  group_by(race, native.country) %>%
  summarise(mean.hours = mean(hours.per.week)) %>%
  ggplot(aes(x = native.country, y = mean.hours, fill = native.country)) + geom_col(position = 'dodge')

salary %>%
  ggplot(aes(x = marital.status, y = age, col = marital.status)) + geom_point(position = 'jitter', alpha = 0.5) + geom_boxplot(alpha = 0, col = 'black')

labels <- as.character(unique(salary$race))
race_counts <- c()
count <- 1
for(races in labels){
  race_counts[count] <- nrow(subset(salary, race == races))
  count <- count + 1
}
percentages <- round(race_counts/sum(race_counts)*100, 0)
labels <- paste0(labels, ' ', percentages, '%')
pie(race_counts, labels = labels)






?stat_summary
