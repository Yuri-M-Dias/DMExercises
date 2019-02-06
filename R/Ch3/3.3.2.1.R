library(dplyr)
library(tidyverse)

data(iris)
iris.stand = cbind(scale(select(iris, -Species)), select(iris, Species))
summary(iris.stand)

mxs = apply(select(iris, -Species), 2, max, na.rm = TRUE)
mns = apply(select(iris, -Species), 2, min, na.rm = TRUE)
iris.norm = cbind(
  scale(select(iris, -Species), center = mns, scale = mxs - mns),
  select(iris, Species)
)
summary(iris.norm)
