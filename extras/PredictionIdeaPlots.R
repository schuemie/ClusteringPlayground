library(ggplot2)

n <- 100
p <- 0.5
xNotO <- c(0, 0)
xO <- c(0.5, 0.25)

o <- runif(n) < p
x1 <- rnorm(n, ifelse(o, xO[1], xNotO[1]), sd = 0.1)
x2 <- rnorm(n, ifelse(o, xO[2], xNotO[2]), sd = 0.1)

ggplot(data.frame(outcome = o, x1 = x1, x2 = x2), aes(x = x1, y = x2)) +
  geom_point(aes(color = outcome), shape = 16, alpha = 0.8) +
  geom_segment(x = xNotO[1], y = xNotO[2], xend = xO[1], yend = xO[2], arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
ggsave("c:/temp/simpleModel.png", width = 4, height = 3)

p2 <- 0.5
xO1 <- c(0.4, 0.45)
xO2 <- c(0.6, 0.05)

o <- runif(n) < p
o2 <- runif(n) < p2
x1 <- rnorm(n, ifelse(o, ifelse(o2, xO2[1], xO1[1]), xNotO[1]), sd = 0.1)
x2 <- rnorm(n, ifelse(o, ifelse(o2, xO2[2], xO1[2]), xNotO[2]), sd = 0.1)

ggplot(data.frame(outcome = o, x1 = x1, x2 = x2), aes(x = x1, y = x2)) +
  geom_point(aes(color = outcome), shape = 16, alpha = 0.8) +
  geom_segment(x = xNotO[1], y = xNotO[2], xend = xO1[1], yend = xO1[2], arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
  geom_segment(x = xNotO[1], y = xNotO[2], xend = xO2[1], yend = xO2[2], arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
ggsave("c:/temp/mixedModel.png", width = 4, height = 3)
