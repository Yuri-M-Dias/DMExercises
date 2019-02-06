library(dplyr)
library(stringr)
library(readr)
library(tidyverse)

uci.repo = "https://archive.ics.uci.edu/ml/machine-learning-databases/"
dataset = "audiology/audiology.standardized"

dataF = str_c(uci.repo, dataset, ".data")
namesF = str_c(uci.repo, dataset, ".names")

## Reading the data file
data = read_csv(url(dataF), col_names = FALSE, na = "?")

text = read_lines(url(namesF))

nms = str_split_fixed(text[67:135], ":", n = 2)[, 1] %>% # Names only
  str_trim() %>% # Removes whitespace
  str_replace_all("\\(|\\)", "") # Removes ()

colnames(data)[1:69] <- nms

data[1:3,1:10]
