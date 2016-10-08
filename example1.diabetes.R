# Diabetes and glucose example

library(MASS)
library(plyr)
library(gplots)

m1 <- glm(type ~ glu, Pima.tr, family = binomial)

d1 <- data.frame(glu = seq(60, 200, 1))
d1$p1 <- predict(m1, newdata = d1)
d1$p2 <- predict(m1, newdata = d1, type = "response")

Pima.tr$gluCut <- cut(Pima.tr$glu, 8)

pseudocount <- 0.5
d2 <- ddply(Pima.tr, .(gluCut), summarise,
            glu  = mean(glu),
            num  = length(type),
            yes  = sum(type == "Yes"),
            no   = sum(type == "No"),
            type    = yes / num,
            type.li = qbeta(0.05, pseudocount + yes, pseudocount + no),
            type.ui = qbeta(0.95, pseudocount + yes, pseudocount + no))

plot(jitter(as.numeric(type) - 1, amount = 0.02) ~ glu, Pima.tr, col = "grey")
with(d2, plotCI(glu, type, li = type.li, ui = type.ui, add = TRUE,
                sfrac = 0, gap = 0, pch = 19, col = "blue"))
lines(p2 ~ glu, d1, ylim = c(0, 1), lty = 2, lwd = 2, col = "magenta")

coef(summary(m1))
confint(m1)

signif(coef(summary(m1)), 2)
signif(exp(coef(m1)), 3)
signif(exp(confint(m1)[2,]), 3)

drop1(m1, test = "LRT")
