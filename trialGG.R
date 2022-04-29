library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

df <- data.frame(
x = runif(100),
y = runif(100),
z1 = rnorm(100),
z2 = abs(rnorm(100))
)

ggplot(df, aes(x, y)) + geom_point()
