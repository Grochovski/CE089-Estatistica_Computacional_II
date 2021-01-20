## código DO TEACHER

# O primeiro código é o original. Após uma verificação com profvis(), foi notado
# um tempo alto no processamento da função lm().
slow_help <- function(){
  ## Simula do modelo
  set.seed(123)
  n <- 1000
  b0 <- 10
  b1 <- 0.5
  x <- rnorm(n, mean = 150, sd = 15)
  sigma2 <- 20
  y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))
  
  ## Número de amostras
  r <- 1e4
  ## Número de elementos em cada amostra
  m <- 100
  
  ##----------------------------------------------------------------------
  ## Bootstrap
  b0.boot <- numeric(r)
  b1.boot <- numeric(r)
  set.seed(123)
  
  demora_um_pouco <- for(i in 1:r){
    select <- sample(1:length(y), size = m, replace = TRUE)
    x.boot <- x[select]
    y.boot <- y[select]
    mm <- lm(y.boot ~ x.boot) # ESSE PONTO DO código É O QUE MAIS DEMORA
    b0.boot[i] <- coef(mm)[1]
    b1.boot[i] <- coef(mm)[2]
  }
  
}

#________________________________________________________________________________________#

## código DO STUDENT

# Como o professor sugeriu o uso da função lm.fit(), foi utilizado conhecimentos adquiridos 
# na internet para rodar o código. 
# Com algumas pesquisas, foi verificado que o a função  lm.fit() aparentemente utiliza-se
# da linguagem C para rodar a função, tornando assim o processo mais rápido.

fast_yay <- function(){
  ## Simula do modelo
  set.seed(123)
  n <- 1000
  b0 <- 10
  b1 <- 0.5
  x <- rnorm(n, mean = 150, sd = 15)
  sigma2 <- 20
  y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))
  
  ## Número de amostras
  r <- 1e4
  ## Número de elementos em cada amostra
  m <- 100
  
  ##----------------------------------------------------------------------
  ## Bootstrap
  b0.boot <- numeric(r)
  b1.boot <- numeric(r)
  set.seed(123)
  
  demora_menos <- for(i in 1:r){
    select <- sample(1:length(y), size = m, replace = TRUE)
    x.boot <- matrix(x[select])
    y.boot <- matrix(y[select])
    mm <- lm.fit(cbind(1,x.boot),y.boot) # UTILIZEI A função LM.FIT PARA APRIMORAR 
                                         # O TEMPO DO código
    b0.boot[i] <- coef(mm)[1]
    b1.boot[i] <- coef(mm)[2]
  }
}

#________________________________________________________________________________________#

## COMPARAÇÃO DOS TEMPOS ##

microbenchmark(slow_help(), fast_yay(), times = 10)

#________________________________________________________________________________________#
