# SNP example

library(plyr)
library(gplots)

set.seed(1710)

af <- 0.4
or <- 1.2
nconts <- 1000
ncases <- 1000

genofreqConts <- c((1 - af)^2, 2 * af * (1 - af), af^2)
genofreqCases <- genofreqConts * c(1, or, or^2)

conts <- rmultinom(1, ncases, genofreqConts)
cases <- rmultinom(1, ncases, genofreqCases)

snp <- data.frame(cases    = cases,
                  controls = conts,
                  g        = 0:2)

logit <- function(p)
    log(p) - log(1 - p)

snp <- mutate(snp,
              l    = log(cases / controls),
              l.li = logit(qbeta(0.05, cases, controls)),
              l.ui = logit(qbeta(0.95, cases, controls)))

m1 <- glm(cbind(cases, controls) ~ g, snp, family = binomial)

snp$fitted <- fitted(m1)

with(snp,
     plotCI(g, l, li = l.li, ui = l.ui,
            sfrac = 0, gap = 0, pch = 19, col = "blue"))
abline(m1, lty = 2, lwd = 2, col = "magenta")

coef(summary(m1))
confint(m1)

signif(coef(summary(m1)), 2)
signif(exp(coef(m1)), 3)
signif(exp(confint(m1)[2,]), 3)

drop1(m1, test = "LRT")
