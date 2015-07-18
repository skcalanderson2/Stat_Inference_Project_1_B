library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12

## voters 100, 56 for you

round(1/sqrt(10^(1:6)),3)
.56 + c(-1,1) * qnorm(0.975) * sqrt(0.56 * 0.44/100)
binom.test(56,100)$conf.int

## Sleep study,  T confidence intervals example 4:06
data("sleep")
g1 <- sleep$extra[1:10]; g2 <- sleep$extra[11:20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10

## confidence intervals
mn + c(-1,1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)
