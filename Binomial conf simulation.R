dat <- NULL
real.Rate <- 0.5
B <- rbinom(10000, 1, real.Rate)
n = 50
conf.level = 0.95


for (i in 1:100) {
    S <- sample(B, n)
    x <- length(S[S == TRUE])
    Rate <- x / n
    CIL <- binom.confint(x, n, conf.level, methods = "wilson")[, 5]
    CIH <- binom.confint(x, n, conf.level, methods = "wilson")[, 6]
    Inclu <- real.Rate >= CIL & real.Rate <= CIH
    df <- data.frame(Rate, Inclu, CIL, CIH)
    dat <- rbind(dat, df)
}


dat
require(ggplot2)
require(ggthemes)

ggplot(dat, aes(x = 1:100, y = Rate, col = Inclu)) +
    geom_point(size = 1) +
    geom_errorbar(aes(ymax = CIH, ymin = CIL, alpha = 0.03)) +
    geom_hline(yintercept = real.Rate) +
    theme_pander() +
    scale_colour_brewer(palette = "Set1")