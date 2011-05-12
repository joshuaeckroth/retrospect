library(ggplot2)

tango_axis_text <- theme_text(family = "Droid Sans",
  colour = "#000000", face="bold")
tango_axis_text_vertical <- theme_text(family = "Droid Sans",
  colour = "#000000", face="bold", angle=90)
tango_axis_text_vertical_right <- theme_text(family = "Droid Sans",
  colour = "#000000", face="bold", angle=-90)

theme_update(panel.background = theme_rect(fill = "white"),
  panel.grid.major = theme_line(colour = "#ececec"),
  panel.grid.minor = theme_line(colour = "#f3f3f3"),
  panel.border = theme_rect(colour = "#000000", size=1),
  axis.text.x = tango_axis_text,
  axis.title.x = tango_axis_text,
  axis.text.y = tango_axis_text,
  axis.title.y = tango_axis_text_vertical)

sb1 = read.csv("sb1.csv")
sb10 = read.csv("sb10.csv")

pec <- ggplot(sb1, aes(x=factor(StepsBetween), y=PercentEventsCorrect))
pec <- pec + facet_grid(SensorSeesColor ~ NumberEntities)
pec <- pec + geom_jitter(colour="#bbbbbb")
pec <- pec + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
pec <- pec + geom_jitter(data=sb10, colour="#bbbbbb")
pec <- pec + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000")
pec <- pec + scale_x_discrete("Steps between reasoning")
pec <- pec + scale_y_continuous("Percent events correct")
pec <- pec + opts(strip.text.y = tango_axis_text_vertical_right, strip.text.x = tango_axis_text, strip.background = theme_rect(colour = "#ffffff"))

mtl <- ggplot(sb1, aes(x=factor(StepsBetween), y=MeanTimeWithLabel))
mtl <- mtl + facet_grid(SensorSeesColor ~ NumberEntities)
mtl <- mtl + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000") + geom_jitter(colour="#a30000")
mtl <- mtl + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000") + geom_jitter(data=sb10, colour="#a30000")
mtl <- mtl + scale_x_discrete("Steps between reasoning")
mtl <- mtl + scale_y_continuous("Mean time with label")

mlc <- ggplot(sb1, aes(x=factor(StepsBetween), y=MeanLabelCounts))
mlc <- mlc + facet_grid(SensorSeesColor ~ NumberEntities)
mlc <- mlc + geom_jitter(colour="#bbbbbb")
mlc <- mlc + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
mlc <- mlc + geom_jitter(data=sb10, colour="#bbbbbb")
mlc <- mlc + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000")
mlc <- mlc + scale_x_discrete("Steps between reasoning")
mlc <- mlc + scale_y_continuous("Mean label count")

ue <- ggplot(sb1, aes(x=factor(StepsBetween), y=Unexplained))
ue <- ue + facet_grid(SensorSeesColor ~ NumberEntities)
ue <- ue + geom_jitter(colour="#bbbbbb")
ue <- ue + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
ue <- ue + geom_jitter(data=sb10, colour="#bbbbbb")
ue <- ue + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000")
ue <- ue + scale_x_discrete("Steps between reasoning")
ue <- ue + scale_y_continuous("Unexplained")

ec <- ggplot(sb1, aes(x=factor(StepsBetween), y=ExplainCycles))
ec <- ec + facet_grid(SensorSeesColor ~ NumberEntities)
ec <- ec + geom_jitter(colour="#bbbbbb")
ec <- ec + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
ec <- ec + geom_jitter(data=sb10, colour="#bbbbbb")
ec <- ec + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000")
ec <- ec + scale_x_discrete("Steps between reasoning")
ec <- ec + scale_y_continuous("Explain cycles")

dl <- ggplot(subset(sb1, NumberEntities==4), aes(x=factor(StepsBetween), y=DistinctLabels))
dl <- dl + geom_jitter(colour="#bbbbbb")
dl <- dl + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
dl <- dl + geom_jitter(data=subset(sb10,NumberEntities==4), colour="#bbbbbb")
dl <- dl + geom_boxplot(data=subset(sb10,NumberEntities==4), outlier.colour="NA", fill="NA", size=2, colour="#000000")
dl <- dl + scale_x_discrete("Steps between reasoning")
dl <- dl + scale_y_continuous("Distinct labels")


sec <- ggplot(sb1, aes(x=factor(StepsBetween), y=SharedExplainsCount))
sec <- sec + geom_jitter(colour="#bbbbbb")
sec <- sec + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
sec <- sec + geom_jitter(data=sb10, colour="#bbbbbb")
sec <- sec + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000")
sec <- sec + scale_x_discrete("Steps between reasoning")
sec <- sec + scale_y_continuous("# of hyps. explaining same dets.")


bc <- ggplot(sb1, aes(x=factor(StepsBetween), y=BadCount))
bc <- bc + geom_jitter(colour="#bbbbbb")
bc <- bc + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
bc <- bc + geom_jitter(data=sb10, colour="#bbbbbb", size=1)
bc <- bc + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000")
bc <- bc + scale_x_discrete("Steps between reasoning")
bc <- bc + scale_y_continuous("# of anomalies")


hc <- ggplot(sb1, aes(x=factor(StepsBetween), y=HypothesisCount))
hc <- hc + geom_jitter(colour="#bbbbbb")
hc <- hc + geom_boxplot(outlier.colour="NA", fill="NA", size=2, colour="#000000")
hc <- hc + geom_jitter(data=sb10, colour="#bbbbbb")
hc <- hc + geom_boxplot(data=sb10, outlier.colour="NA", fill="NA", size=2, colour="#000000")
hc <- hc + scale_x_discrete("Steps between reasoning")
hc <- hc + scale_y_continuous("# of hypotheses + facts")
