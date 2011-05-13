library(ggplot2)

tango_axis_text <- theme_text(colour = "#000000", face="bold", size=14)
tango_axis_text_vertical <- theme_text(colour = "#000000", face="bold", angle=90, size=14)
tango_axis_text_vertical_right <- theme_text(colour = "#000000", face="bold", angle=-90, size=14)

theme_update(panel.background = theme_rect(fill = "white"),
  panel.grid.major = theme_line(colour = "#ececec"),
  panel.grid.minor = theme_line(colour = "#f3f3f3"),
  panel.border = theme_rect(colour = "#000000", size=1),
  axis.text.x = tango_axis_text,
  axis.title.x = tango_axis_text,
  axis.text.y = tango_axis_text,
  axis.title.y = tango_axis_text_vertical)

meta <- read.csv("meta.csv")
meta <- subset(meta, NumberEntities>4)

pec <- ggplot(meta, aes(x=factor(NumberEntities), y=BasePercentEventsCorrect))
pec <- pec + geom_jitter(colour="#bbbbbb")
pec <- pec + geom_boxplot(outlier.colour="NA", fill="NA", size=1, colour="#000000")
pec <- pec + scale_x_discrete("Number of entities")
pec <- pec + scale_y_continuous("Percent events correct")
pec <- pec + opts(strip.text.y = tango_axis_text_vertical_right, strip.text.x = tango_axis_text, strip.background = theme_rect(colour = "#ffffff"))

ggsave("pec.pdf", plot = pec, dpi = 900, width = 10, height = 6)


mtl <- ggplot(meta, aes(x=factor(NumberEntities), y=BaseMeanTimeWithLabel))
mtl <- mtl + geom_jitter(colour="#bbbbbb")
mtl <- mtl + geom_boxplot(outlier.colour="NA", fill="NA", size=1, colour="#000000")
mtl <- mtl + scale_x_discrete("Number of entities")
mtl <- mtl + scale_y_continuous("Mean time with label")
mtl <- mtl + opts(strip.text.y = tango_axis_text_vertical_right, strip.text.x = tango_axis_text, strip.background = theme_rect(colour = "#ffffff"))

ggsave("mtl.pdf", plot = mtl, dpi = 900, width = 10, height = 6)

mlc <- ggplot(meta, aes(x=factor(NumberEntities), y=BaseMeanLabelCounts))
mlc <- mlc + geom_jitter(colour="#bbbbbb")
mlc <- mlc + geom_boxplot(outlier.colour="NA", fill="NA", size=1, colour="#000000")
mlc <- mlc + scale_x_discrete("Number of entities")
mlc <- mlc + scale_y_continuous("Mean label counts")
mlc <- mlc + opts(strip.text.y = tango_axis_text_vertical_right, strip.text.x = tango_axis_text, strip.background = theme_rect(colour = "#ffffff"))

ggsave("mlc.pdf", plot = mlc, dpi = 900, width = 10, height = 6)


ec <- ggplot(meta, aes(x=factor(NumberEntities), y=BaseExplainCycles))
ec <- ec + geom_jitter(colour="#bbbbbb")
ec <- ec + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
ec <- ec + scale_x_discrete("Number of entities")
ec <- ec + scale_y_continuous("Explain cycles")

ggsave("ec.pdf", plot = ec, dpi = 900, width = 10, height = 6)


