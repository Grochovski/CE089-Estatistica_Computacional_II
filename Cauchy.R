##   Código da Elen   ###

set.seed(2424)
f <- function(x) dcauchy(x, 0, 1)
N <- 1e5
x <- numeric(N)
x[1] <- 0
for(i in 2:N) {
  z <- rnorm(1,0,1)
  y <- x[i - 1] + z
  alpha <- min(f(y)/f(x[i - 1]), 1)
  u <- runif(1)
  if(u <= alpha) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1]
  }
}


plot(x, type = "l")

plot(ecdf(x))
curve(pcauchy(x,0,1), add = TRUE, col = 6)
legend("right", legend = c("Empírica", "Teórica"),
       lty = 2, col = c(1,6), bty = "n")




