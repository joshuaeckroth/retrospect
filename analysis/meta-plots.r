library(ggplot2)

tango_axis_text <- theme_text(colour = "#000000", face="bold", size=18)
tango_axis_text_vertical <- theme_text(colour = "#000000", face="bold", angle=90, size=18)

theme_update(panel.background = theme_rect(fill = "white"),
  panel.grid.major = theme_line(colour = "#ececec"),
  panel.grid.minor = theme_line(colour = "#f3f3f3"),
  panel.border = theme_rect(colour = "#000000", size=1),
  axis.text.x = tango_axis_text,
  axis.title.x = tango_axis_text,
  axis.text.y = tango_axis_text,
  axis.title.y = tango_axis_text_vertical)
  
meta <- read.csv("meta.csv")
metaworked <- subset(meta, MetaAbductions > 0 & NumberEntities > 4)

avgpec <- ggplot(metaworked, aes(x=factor(NumberEntities), y=AverageMetaDiffPercentEventsCorrect))
avgpec <- avgpec + geom_hline(yintercept=0, size=2, colour="#a30000")
avgpec <- avgpec + geom_jitter(colour="#bbbbbb", size=3)
avgpec <- avgpec + geom_boxplot(outlier.colour="NA", fill="NA", size=1, colour="#000000")
avgpec <- avgpec + scale_x_discrete("Number of entities")
avgpec <- avgpec + scale_y_continuous("Average difference in P.E.C.")

ggsave("meta-avgpec-numes.pdf", plot = avgpec, dpi = 900, width = 10, height = 6)



avgmtl <- ggplot(metaworked, aes(x=factor(NumberEntities), y=AverageMetaDiffMeanTimeWithLabel))
avgmtl <- avgmtl + geom_hline(yintercept=0, size=2, colour="#a30000")
avgmtl <- avgmtl + geom_jitter(colour="#bbbbbb", size=3)
avgmtl <- avgmtl + geom_boxplot(outlier.colour="NA", fill="NA", size=1, colour="#000000")
avgmtl <- avgmtl + scale_x_discrete("Number of entities")
avgmtl <- avgmtl + scale_y_continuous("Average difference in M.T.L.")

ggsave("meta-avgmtl-numes.pdf", plot = avgmtl, dpi = 900, width = 10, height = 6)



avgmlc <- ggplot(metaworked, aes(x=factor(NumberEntities), y=AverageMetaDiffMeanLabelCounts))
avgmlc <- avgmlc + geom_hline(yintercept=0, size=2, colour="#a30000")
avgmlc <- avgmlc + geom_jitter(colour="#bbbbbb", size=3)
avgmlc <- avgmlc + geom_boxplot(outlier.colour="NA", fill="NA", size=1, colour="#000000")
avgmlc <- avgmlc + scale_x_discrete("Number of entities")
avgmlc <- avgmlc + scale_y_continuous("Average difference in M.L.C.")

ggsave("meta-avgmlc-numes.pdf", plot = avgmlc, dpi = 900, width = 10, height = 6)



ec <- ggplot(metaworked, aes(x=factor(NumberEntities), y=IncreaseExplainCycles))
ec <- ec + geom_hline(yintercept=0, size=2, colour="#a30000")
ec <- ec + geom_jitter(colour="#bbbbbb", size=3)
ec <- ec + geom_boxplot(outlier.colour="NA", fill="NA", size=1, colour="#000000")
ec <- ec + scale_x_discrete("Number of entities")
ec <- ec + scale_y_continuous("Percent increase in explain cycles")

ggsave("meta-ec-numes.pdf", plot = ec, dpi = 900, width = 10, height = 6)



cbpec <- ggplot(metaworked, aes(x=IncreaseExplainCycles, y=AverageMetaDiffPercentEventsCorrect))
cbpec <- cbpec + geom_vline(xintercept=0, size=2, colour="#a30000")
cbpec <- cbpec + geom_hline(yintercept=0, size=2, colour="#a30000")
cbpec <- cbpec + geom_point(colour="#bbbbbb", size=3)
cbpec <- cbpec + scale_y_continuous("Average difference in P.E.C.")
cbpec <- cbpec + scale_x_continuous("Percent increase in explain cycles",limits=c(0,800))

ggsave("meta-cost-benefit-pec.pdf", plot = cbpec, dpi = 900, width = 10, height = 6)




cbmtl <- ggplot(metaworked, aes(x=IncreaseExplainCycles, y=AverageMetaDiffMeanTimeWithLabel))
cbmtl <- cbmtl + geom_vline(xintercept=0, size=2, colour="#a30000")
cbmtl <- cbmtl + geom_hline(yintercept=0, size=2, colour="#a30000")
cbmtl <- cbmtl + geom_point(colour="#bbbbbb", size=3)
cbmtl <- cbmtl + scale_y_continuous("Average difference in M.T.L.")
cbmtl <- cbmtl + scale_x_continuous("Percent increase in explain cycles",limits=c(0,800))

ggsave("meta-cost-benefit-mtl.pdf", plot = cbmtl, dpi = 900, width = 10, height = 6)




cbmlc <- ggplot(metaworked, aes(x=AverageMetaDiffMeanLabelCounts, y=IncreaseExplainCycles))
cbmlc <- cbmlc + geom_vline(xintercept=0, size=2, colour="#a30000")
cbmlc <- cbmlc + geom_hline(yintercept=0, size=2, colour="#a30000")
cbmlc <- cbmlc + geom_point(colour="#bbbbbb", size=3)
cbmlc <- cbmlc + scale_x_continuous("Average difference in M.L.C.")
cbmlc <- cbmlc + scale_y_continuous("Percent increase in explain cycles",limits=c(0,800))

ggsave("meta-cost-benefit-mlc.pdf", plot = cbmlc, dpi = 900, width = 10, height = 6)



