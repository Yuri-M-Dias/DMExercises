library(here)
library(tidyverse)

data(iris)
prop2sample = 0.5
rowIDs = sample(1:nrow(iris), as.integer(prop2sample * nrow(iris)))
iris.sample = iris[rowIDs, ]

nrLinesFile = function(f) {
  if (.Platform$OS.type == "unix")
    as.integer(
      strsplit(trimws(system(paste("wc -l", f), intern = TRUE)), " ")[[1]][1]
    )
  else
    stop("This function requires unix-based systems")
}

sampleCSV = function(file, percORn, nrLines, header = TRUE, mxPerc = 0.5) {
  if (.Platform$OS.type != "unix")
    stop("This function requires unix-based systems")
  require(readr, quietly = TRUE)
  if (missing(nrLines)) nrLines = nrLinesFile(file)
  if (percORn < 1) {
    if (percORn > mxPerc)
      stop("This function is not adequate for that big samples.")
    else
      percORn = as.integer(percORn * nrLines)
  }
  perc = min(2 * percORn / nrLines, mxPerc)
  system(
    paste0(
      "perl -ne 'print if (rand() < ", perc, ")' ",
      file, " > ", file, ".tmp.csv"
    )
  )
  dt = read_csv(paste0(file, ".tmp.csv"), col_names = header, n_max = percORn)
  file.remove(paste0(file, ".tmp.csv"))

  if (nrow(dt) != percORn)
    warning(paste("Expecting", percORn, "rows, but got", nrow(dt)))

  dt
}

# Not run
t <- Sys.time()
d <- sampleCSV("allsensors.csv", 0.01, header = FALSE)
Sys.time() - t
nrow(d)

# 3.3.4.2

library(CORElearn)
data(iris)

attrEval(Species ~ ., iris, estimator = "GainRatio")
attrEval(Species ~ ., iris, estimator = "InfGain")
attrEval(Species ~ ., iris, estimator = "Gini")
attrEval(Species ~ ., iris, estimator = "MDL")

data(algae, package = "DMwR2")

attrEval(a1 ~ ., algae[, 1:12], estimator = "MSEofMean")
attrEval(a1 ~ ., algae[, 1:12], estimator = "RReliefFexpRank")

pca.data = iris[, -5] # each case is described by the first 4 variables
pca = princomp(pca.data)
loadings(pca)

reduced.iris = data.frame(pca$scores[, 1:2], Species = iris$Species)
dim(reduced.iris)
# Interesting, now I see why PCA is used at all
