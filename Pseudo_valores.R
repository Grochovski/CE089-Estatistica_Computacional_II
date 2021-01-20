x <- c(8.25, 8.36, 8.42, 8.87, 10, 10.57, 11.18, 11.52)

(n <- length(x))

(xbar <- mean(x))

(sx <- sd(x))


## Obter uma estimativa do erro padrão para o desvio padrão

## Estimativas parciais
theta.jack <- numeric(n)
for(i in 1:n) {
  theta.jack[i] <- sd(x[-i])
}

## Pseudo valores
(pv <- n * sx - (n - 1) * theta.jack)

round(cbind(Amostra = x,"SD" = theta.jack, "PV" = pv), 4)


round(sd(x) + qt(c(.025, .975), df = n - 1) * sqrt(var(pv)/n),3)

