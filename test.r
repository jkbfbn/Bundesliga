source("func.r")
source("pred.r")
library(ggplot2)
library(reshape2)


# Influence of slope value
ex <- 10
cu <- 6
slopeTest <- melt(
  data.frame(
  a = weight(ex, cu, 0.001),
  b = weight(ex, cu, 0.002),
  c = weight(ex, cu, 0.003),
  d = weight(ex, cu, 0.004),
  e = weight(ex, cu, 0.005),
  f = weight(ex, cu, 0.006),
  g = weight(ex, cu, 0.007),
  h = weight(ex, cu, 0.008),
  i = weight(ex, cu, 0.009),
  j = weight(ex, cu, 0.01),
  id = 1:ex
  ), id.vars = c("id")
)
ggplot(data = slopeTest) + 
  geom_line(aes(x = id, y = value, color = variable), stat = "identity", size = 1)

# extent vs. cutoff
slopeTest <- melt(
  data.frame(
    a = weight(5, 20, 0.001),
    b = weight(20, 20, 0.001),
    c = weight(20, 5, 0.001), #Schönheitsideal
    d = weight(20, 10, 0.001),
    e = weight(20, 15, 0.01),
    f = weight(10, 6, 0.005),
    id = 1:20
  ), id.vars = c("id")
)
ggplot(data = slopeTest) + 
  geom_line(aes(x = id, y = value, color = variable), stat = "identity", size = 1)
