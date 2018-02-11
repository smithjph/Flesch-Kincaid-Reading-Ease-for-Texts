library(ggplot2)
library(ggthemes)

sources = read.csv("project1.csv")
sources = as.data.frame(sources)


dodge <- position_dodge(width = 0.9)

limits <- aes(ymax = sources$mean + sources$se,
              ymin = sources$mean - sources$se)

p <- ggplot(data = sources, aes(x = names, y = mean, fill = names))

p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank())

fill <- "#4271AE"
line <- "#1F3552"

order = c("FiveThirtyEight", "CNN", "The New York Times", "NPR", "Fox News")

ggplot(sources, aes(x = Source, y = Score)) + geom_boxplot(fill = fill, color = line) + ggtitle("Boxplot of Algorithm Score by Source") + theme_fivethirtyeight() +theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = order) + theme(axis.title = element_text(), axis.title.x = element_blank()) + ylab('Algorithm Score')


novels = read.csv("novels.csv")

novelorder = c("The Scarlet Letter", "Crime and Punishment", "The Adventures of Huckleberry Finn", "Heart of Darkness", "The Great Gatsby", "Of Mice and Men", "The Grapes of Wrath", "Animal Farm", "The Catcher in the Rye", "The Lord of the Rings: The Fellowship of the Ring", "Harry Potter and the Sorcerer's Stone")

ggplot(novels, aes(Year.Published, Algorithm.Score)) + geom_line(color = fill) + theme_fivethirtyeight() + ggtitle("Algorithm Score by Publishing Year") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Algorithm Score") + theme(axis.title = element_text()) + ylab('Algorithm Score') + xlab("Publishing Year")

ggplot(novels, aes(Novel, Difference)) + geom_point(color = fill) + theme_fivethirtyeight() + ggtitle("Difference in Grade Read vs. Reading Level") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = novelorder) + theme(axis.text.x=element_text(angle=45, hjust=1)) + ylab("Grade Level Difference") + xlab("") + theme(axis.title = element_text(), axis.title.x = element_blank()) + ylab('Grade Level Difference') 
