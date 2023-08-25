# Rotation age based on reservation price (Brazee & Mendlesohn 1988)
# 07/26/2016

library(XLconnect)

Pr_1 <- rnorm(65, 169.19, 65.73)
x <- ts(Pr_1, start = 15)
x
#