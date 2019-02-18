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

library(ggplot2)
data(algae, package="DMwR2")
freqOcc <- table(algae$season)
barplot(freqOcc,main='Frequency of the Seasons')
algae %>%
  ggplot(aes(x=season)) +
  geom_bar() +
  ggtitle("Frequency of the Seasons")

data(iris)
boxplot(iris$Sepal.Width, ylab='Sepal Width')
iris %>%
  ggplot(aes(x = factor(0), y = Sepal.Width)) +
  geom_boxplot() +
  xlab("") +
  ylab("Sepal Width") +
  theme(axis.text.x = element_blank())

boxplot(Sepal.Length ~ Species, iris, ylab = "Sepal.Length")
iris %>%
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()

algae %>%
  ggplot(aes(x=a1)) +
  geom_histogram() +
  facet_grid(size ~ speed)

plot(iris$Sepal.Length,
     iris$Sepal.Width,
     main="Relationship between Sepal Length and Width",
     xlab="Sepal Length",
     ylab="Sepal Width")

iris %>%
  ggplot(aes(x=Sepal.Length,y=Sepal.Width)) +
  geom_point() +
  xlab("Sepal Length") + ylab("Sepal Width") +
  ggtitle("Relationship between Sepal Length and Width")

algae %>%
  ggplot(aes(x = a1, y = a2, color = season)) +
  geom_point() +
  facet_wrap(~ season)

library(GGally)
ggpairs(algae, columns=12:16)
ggpairs(algae, columns=2:5)
ggparcoord(algae,columns=12:18,groupColumn="season")

library(arules)
library(dplyr)
data(Boston,package="MASS")
b <- Boston
b$chas <- factor(b$chas,labels=c("river","noriver"))
b$rad <- factor(b$rad)
b$black <- cut(b$black,breaks=4,labels=c(">31.5%","18.5-31.5%","8-18.5%","<8%"))
discr <- function(x) cut(x,breaks=4, labels=c("low","medLow","medHigh","high"))
b <- select(b,-one_of(c("chas","rad","black"))) %>%
  mutate_all(funs(discr)) %>%
  bind_cols(select(b,one_of(c("chas","rad","black"))))
b <- as(b,"transactions")
summary(b)
itemFrequencyPlot(b, support=0.3,cex.names=0.8)
ars <- apriori(b, parameter=list(support=0.025, confidence=0.75))
ars
table(discr(Boston$medv))

inspect(head(subset(ars, subset=rhs %in% "medv=high"),5,by="confidence"))
inspect(head(subset(ars, subset=rhs %in% "medv=low"),5,by="confidence"))

inspect(
  head(subset(ars, subset = lhs %in% "nox=high" | rhs %in% "nox=high"),
       5, by="confidence")
)

library(arulesViz)
plot(ars)

somerules <- subset(
  ars,
  subset=rhs %in% c("medv=high","medv=medHigh") & confidence>0.75
)
plot(somerules, method="matrix", measure="lift")

somerules <- subset(ars, subset=rhs %in% "medv=high" & confidence > 0.95)
plot(somerules, method="graph", control=list(type="itemsets"))

# 3.4.3

set.seed(1234)
randDat <- matrix(rnorm(50), nrow=5)
dist(randDat) # Euclidean distance (default)
dist(randDat, method="manhattan")
dist(randDat, method="minkowski", p=4)

set.seed(1234) # setting a seed for the random number generator
data(iris)
ir3 <- kmeans(iris[,-5], centers=3, iter.max=200) # not using Species info.
ir3

table(ir3$cluster, iris$Species)
cm <- table(ir3$cluster, iris$Species)
1 - sum(diag(cm)) / sum(cm)

library(cluster)
s <- silhouette(ir3$cluster, dist(iris[,-5]))
plot(s)

set.seed(1234)
d <- dist(iris[,-5])
avgS <- c()
for(k in 2:6) {
  cl <- kmeans(iris[,-5],centers=k,iter.max=200)
  s <- silhouette(cl$cluster,d)
  avgS <- c(avgS,mean(s[,3]))
}
data.frame(nClus=2:6,Silh=avgS)

pc <- pam(iris[,-5],k=3)
(cm <- table(pc$clustering, iris$Species))
100*(1-sum(diag(cm))/sum(cm))
pc$silinfo$avg.width

library(fpc)
sol <- pamk(iris[,-5], krange=2:10, criterion="asw", usepam=TRUE)
sol

d <- dist(scale(iris[,-5]))
h <- hclust(d)
plot(h,hang=-0.1,labels=iris[["Species"]],cex=0.5)

clus3 <- cutree(h, 3)
(cm <- table(clus3, iris$Species))
100*(1-sum(diag(cm))/sum(cm))

plot(h,hang=-0.1,labels=iris[["Species"]],cex=0.5)
rect.hclust(h,k=3)

d <- dist(scale(iris[,-5]))
methds <- c('complete','single','average')
avgS <- matrix(NA,ncol=3,nrow=5,
               dimnames=list(2:6,methds))
for (k in 2:6) {
  for (m in seq_along(methds)) {
    h <- hclust(d,meth=methds[m])
    c <- cutree(h,k)
    s <- silhouette(c,d)
    avgS[k-1,m] <- mean(s[,3])
  }
}
avgS

di <- diana(iris[,-5], metric='euclidean', stand=TRUE)
di3 <- cutree(di, 3)
(cm <- table(di3, iris$Species))
100*(1-sum(diag(cm))/sum(cm))

cm <- cm[c(1,3,2),]
100*(1-sum(diag(cm))/sum(cm))

library(fpc)
d <- scale(iris[,-5])
db <- dbscan(d, eps=0.9, MinPts=5)
db
table(db$cluster,iris$Species)

# 3.4.4



