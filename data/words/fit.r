d <- read.csv("learned-words-stats.csv", header=F)
d$Calc <- ((d$V3-d$V2)/(d$V3+d$V2))
dtrue <- subset(d, V1 > 0)
dfalse <- subset(d, V1 < 0)

densetrue <- density(dtrue$Calc)
densefalse <- density(dfalse$Calc)

ddtrue <- data.frame(x=densetrue$x, y=densetrue$y)
ddfalse <- data.frame(x=densefalse$x, y=densefalse$y)

fittrue <- nls(y ~ dnorm(x, mean=m1, sd=s1) + dnorm(x, mean=m2, sd=s2),
               data=ddtrue, start=list(s1=0.25, m1=0.5, s2=0.25, m2=-0.25),
               algorithm="plinear")

print("True")
print(fittrue)

fitfalse <- nls(y ~ dnorm(x, mean=m1, sd=s1) + dnorm(x, mean=m2, sd=s2),
                data=ddfalse, start=list(s1=0.25, m1=0.5, s2=0.25, m2=-0.75),
                algorithm="plinear")

print("False")
print(fitfalse)
