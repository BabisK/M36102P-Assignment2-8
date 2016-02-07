library(moments)

my_column <- 5 + 1

W <- read.csv(file = "w.csv")[[my_column]]
X <- read.csv(file = "x.csv")[[my_column]]
Y <- read.csv(file = "y.csv")[[my_column]]
Z <- read.csv(file = "z.csv")[[my_column]]

par(mfrow = c(2,2))

plot(sort(W), main = "W", ylab = "W")
plot(sort(X), main = "X", ylab = "X")
plot(sort(Y), main = "Y", ylab = "Y")
plot(sort(Z), main = "Z", ylab = "Z")

par(mfrow = c(1,3))

plot(sort(Y)[0:999], main = "Y without highest sample", ylab = "Y")
plot(sort(Y)[0:900], main = "Y without highest 100 samples", ylab = "Y ")
plot(sort(Y)[0:500], main = "Y without highest 500 samples", ylab = "Y")

par(mfrow = c(2,2))

hist(W, col=terrain.colors(15))
hist(X, col=terrain.colors(15), breaks=c(0:8), right = FALSE, include.lowest = FALSE)
hist(Y, col=terrain.colors(15))
hist(Z, col=terrain.colors(15))

par(mfrow = c(2,2))

boxplot(W, col=terrain.colors(15), ylab = "W")
boxplot(X, col=terrain.colors(15), ylab = "X")
boxplot(Y, col=terrain.colors(15), ylab = "Y")
boxplot(Z, col=terrain.colors(15), ylab = "Z")

class(W)
head(W)
summary(W)
var(W)
sd(W)
skewness(W)
kurtosis(W)

class(X)
head(X)
summary(X)
var(X)
sd(X)
skewness(X)
kurtosis(X)

class(Y)
head(Y)
summary(Y)
var(Y)
sd(Y)
skewness(Y)
kurtosis(Y)

class(Z)
head(Z)
summary(Z)
var(Z)
sd(Z)
skewness(Z)
kurtosis(Z)

test.binom <- function(x, size, prob){
  t <- table(x)
  q <- seq(min(x), max(x))
  for (s in size) {
    for (p in prob) {
      d <- dbinom(q, s, p)
      if(length(d) > length(t)) {
        t <- c(t, rep(0, times = length(d)-length(t)))
      }
      if(sum(d) < 1) {
        c <- chisq.test(c(t,0), p = c(d, 1-sum(d)))
      }
      else {
        c <- chisq.test(t, p = d)
      }
      if (is.na(c["p.value"]) == FALSE & c["p.value"] > 0.05) {
        break;
      }
    }
  }
  c(c, s, p)
}

test.nbinom <- function(x, size, prob){
  t <- table(x)
  q <- seq(min(x), max(x))
  for (s in size) {
    for (p in prob) {
      d <- dnbinom(q, s, p)
      if(length(d) > length(t)) {
        t <- c(t, rep(0, times = length(d)-length(t)))
      }
      if(sum(d) < 1) {
        c <- chisq.test(c(t,0), p = c(d, 1-sum(d)))
      }
      else {
        c <- chisq.test(t, p = d)
      }
      if (is.na(c["p.value"]) == FALSE & c["p.value"] > 0.05) {
        break;
      }
    }
  }
  c(c, s, p)
}

test.geom <- function(x, prob){
  t <- table(x)
  q <- seq(min(x), max(x))
    for (p in prob) {
      d <- dgeom(q, p)
      if(length(d) > length(t)) {
        t <- c(t, rep(0, times = length(d)-length(t)))
      }
      if(sum(d) < 1) {
        c <- chisq.test(c(t,0), p = c(d, 1-sum(d)))
      }
      else {
        c <- chisq.test(t, p = d)
      }
      if (is.na(c["p.value"]) == FALSE & c["p.value"] > 0.05) {
        break;
      }
  }
  c(c, p)
}

test.pois <- function(x, lamda){
  t <- table(x)
  q <- seq(min(x), max(x))
  for (l in lamda) {
    d <- dpois(q, l)
    if(length(d) > length(t)) {
      t <- c(t, rep(0, times = length(d)-length(t)))
    }
    if(sum(d) < 1) {
      c <- chisq.test(c(t,0), p = c(d, 1-sum(d)))
    }
    else {
      c <- chisq.test(t, p = d)
    }
    if (is.na(c["p.value"]) == FALSE & c["p.value"] > 0.05) {
      break;
    }
  }
  c(c, l)
}

test.pois(W, 5.7)

quantiles <- seq(min(X),max(X))
distribution <-dnbinom(quant,size = 1,prob = 0.7)
chisq.test(x = c(table(X),0), p = c(distribution, 1-sum(distribution)))

logY <- log(Y)
class(logY)
head(logY)
summary(logY)
var(logY)
sd(logY)
skewness(logY)
kurtosis(logY)
par(mfrow = c(1,3))
plot(sort(logY), main = "log(Y)", ylab = "log(Y)")
hist(logY, col=terrain.colors(15), freq = F)
curve(dnorm(x, mean=mean(logY), sd=sd(logY)), add=TRUE, lwd=2)
boxplot(logY, col=terrain.colors(15), ylab = "log(Y)")

shapiro.test(logY)
ks.test(logY, "pnorm", mean(logY), sd(logY))
par(mfrow = c(1,1))
hist(Z, col=terrain.colors(15), freq = F)
curve(dnorm(x, mean=mean(Z), sd=sd(Z)), add=TRUE, lwd=2)

ks.test(Z, "pnorm", mean(Z), sd(Z))
shapiro.test(Z)