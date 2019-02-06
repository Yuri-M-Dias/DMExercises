library(readr)
library(here)
library(tm)
library(tidyverse)

docs <- Corpus(DirSource("Documents"))
docs

# Data not available
