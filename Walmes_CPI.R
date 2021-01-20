library(ggplot2)
library(tidyverse)
library(lattice)
require(rsm)

da <- structure(list(tempo = c(-1L, -1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L), 
                     temperatura = c(-1L, 1L, -1L, 1L, 0L, 0L, 0L, 0L, 0L), 
                     resp = c(39.3, 40.0, 40.9, 41.5, 40.3, 40.5, 40.7, 40.2, 40.6)),
                .Names = c("tempo", "temperatura", "resp"), 
                class = "data.frame", row.names = c(NA, -9L))

str(da)


#-----------------------------------------------------------------------
# Analise exploratoria.

# Distribuicao dos pontos de suporte.
ftable(xtabs(~tempo + temperatura, data = da))

da$lof <- c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L)
# A variável lof é uma variável indicadora onde o valor 1 
# corresponde aos pontos centrais e o valor 0 aos pontos fatoriais.

#-----------------------------------------------------------------------
# Ajuste do modelo.

# Especificacao do fatorial 2^2 completo, incluindo LoF
m0 <- lm(resp ~ tempo * temperatura + lof, data = da)
# Quadro de anova.
anova(m0)
# Vemos que não há iteração entre as covariáveis

# Modelo final
m1 <- lm(resp ~ tempo + temperatura, data = da)
# Quadro de anova.
summary(m1)

#--------------------------------------------------------------------
beta <- coef(m1)[-1]

pred <- with(da,
             expand.grid(x1 = beta(x1),
                         x2 = beta(x2)))


msteep <- rsm(resp ~ FO(tempo, temperatura), data = da)
summary(msteep)
step1 <- steepest(msteep)
step2 <- dupe(step1[2:9, ])






