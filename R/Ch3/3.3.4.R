library(here)
library(tidyverse)

data(algae, package = "DMwR2")
mean(algae$a1)
mean(algae$NO3) # NA due to NAs?
mean(algae$NO3, na.rm = TRUE)
median(algae$a3)
median(algae$mxPH, na.rm = TRUE)

alg = tbl_df(algae)
summarise(alg, avgNO3 = mean(NO3, na.rm = TRUE), medA1 = median(a1))

alg %>%
  select(mxPH:Cl) %>%
  summarise_all(funs(mean(., na.rm = TRUE), median(., na.rm = TRUE)))

alg %>%
  group_by(season, size) %>%
  summarize(nObs = n(), mA7 = median(a7)) %>%
  ungroup() %>%
  arrange(desc(mA7))

Mode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
Mode(algae$mxPH, na.rm = TRUE)
Mode(algae$season)

library(DMwR2)
centralValue(algae$a1)
centralValue(algae$speed)

var(algae$a1)
sd(algae$Cl, na.rm = TRUE)
IQR(algae$mxPH, na.rm = TRUE)
quantile(algae$a3)
quantile(algae$a3, probs = c(0.2, 0.8))
range(algae$a1)
max(algae$a5) - min(algae$a5)

alg %>%
  select(a1:a7) %>%
  summarise_all(funs(var))

data(iris)
aggregate(iris$Sepal.Length, list(Species = iris$Species), quantile)
aggregate(Sepal.Length ~ Species, data = iris, quantile)

nasRow = apply(algae, 1, function(r) sum(is.na(r)))
cat("The Algae dataset contains ", sum(nasRow), " NA values.\n")
cat(
  "There are ",
  sum(!complete.cases(algae)),
  " rows that have at least one NA value.\n"
)

bpRule <- function(x, const = 1.5, positions = FALSE) {
  x <- x[!is.na(x)]
  qs <- quantile(x, probs = c(0.25, 0.75))
  iqr <- qs[2] - qs[1]
  if (!positions)
    x[x < qs[1] - const * iqr | x > qs[2] + const * iqr]
  else
    which(x < qs[1] - const * iqr | x > qs[2] + const * iqr)
}

bpRule(algae$a1)
bpRule(algae$NO3)
bpRule(algae$NO3, positions = TRUE)

summary(iris)

library(Hmisc)
Hmisc::describe(iris)

by(algae[, 2:5], algae$season, summary)

plot(sin(seq(0, 10, by = 0.1)), type = "l")

