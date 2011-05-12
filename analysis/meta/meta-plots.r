library(ggplot2)

tango_axis_text <- theme_text(family = "Droid Sans",
  colour = "#000000", face="bold")
tango_axis_text_vertical <- theme_text(family = "Droid Sans",
  colour = "#000000", face="bold", angle=90)

theme_update(panel.background = theme_rect(fill = "white"),
  panel.grid.major = theme_line(colour = "#ececec"),
  panel.grid.minor = theme_line(colour = "#f3f3f3"),
  panel.border = theme_rect(colour = "#000000", size=1),
  axis.text.x = tango_axis_text,
  axis.title.x = tango_axis_text,
  axis.text.y = tango_axis_text,
  axis.title.y = tango_axis_text_vertical)
  
meta <- read.csv("meta.csv")
metaworked <- subset(meta, MetaAbductions > 0)

avgpec <- ggplot(metaworked, aes(x=factor(NumberEntities), y=AverageMetaDiffPercentEventsCorrect))
avgpec <- avgpec + geom_hline(yintercept=0, size=2, colour="#bbbbbb")
avgpec <- avgpec + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
avgpec <- avgpec + geom_jitter(colour="#a30000", size=3)
avgpec <- avgpec + scale_x_discrete("Number of entities")
avgpec <- avgpec + scale_y_continuous("Average improvement in P.E.C.")

ggsave("meta-avgpec-numes.png", plot = avgpec, dpi = 900, width = 8, height = 8)



