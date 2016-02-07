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

summary(W)
var(W)
sd(W)
skewness(W)
kurtosis(W)

summary(X)
var(X)
sd(X)
skewness(X)
kurtosis(X)

summary(Y)
var(Y)
sd(Y)
skewness(Y)
kurtosis(Y)

summary(Z)
var(Z)
sd(Z)
skewness(Z)
kurtosis(Z)