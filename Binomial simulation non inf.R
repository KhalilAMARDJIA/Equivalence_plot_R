require(ggplot2)
require(ggthemes)
require(binom)
dat <- NULL #dataframe
real.Rate <- 0.98 #real or expected rate
noninfrate <- 0.91 #lower limit
B <- rbinom(10000, 1, real.Rate) #binomial distribution with 0 or 1 possibilities on 10000 observation
n = 59 #sample size
conf.level = 0.95
iteration <- 100

#looping
for (i in 1:iteration) {
    S <- sample(B, n)
    x <- length(S[S == TRUE])
    Rate <- x / n
    CIL <- binom.confint(x, n, conf.level, methods = "wilson")[, 5] #Lower confidence interval
    CIH <- binom.confint(x, n, conf.level, methods = "wilson")[, 6] #Upper confidence interval
    Inclu <- noninfrate < CIL  # testing the inclusion of the real rate within the CI
    df <- data.frame(Rate, Inclu, CIL, CIH)
    dat <- rbind(dat, df)
}
#plot
ggplot(dat, aes(x = 1:iteration, y = Rate, col = Inclu)) +
    geom_point(size = 1) +
    geom_errorbar(aes(ymax = CIH, ymin = CIL)) +
    geom_hline(yintercept = noninfrate) +
    theme_wsj() +
    scale_colour_brewer(palette = "Set1")
