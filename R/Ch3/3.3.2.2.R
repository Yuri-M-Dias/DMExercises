library(Hmisc)
library(tidyverse)

data(Boston, package = "MASS")
summary(Boston$age)

Boston$newAge = cut(Boston$age, 5)
table(Boston$newAge)

Boston$newAge = cut(
  Boston$age, 5, labels = c("verynew", "new", "normal", "old", "veryold")
) # alternative using our own labels for the bins
table(Boston$newAge)

# Equal frequency
Boston$newAge = cut2(Boston$age, g=5)
table(Boston$newAge)

# Equal frequency, labels
Boston$newAge = factor(
  cut2(Boston$age, g=5),
  labels = c("verynew", "new", "normal", "old", "veryold")
)
table(Boston$newAge)
