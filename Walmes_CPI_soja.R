dados <- read.table("http://leg.ufpr.br/~walmes/data/soja.txt", 
                      comment = "#", 
                      dec = ",",
                      header = T, 
                      sep = "\t")

xtabs(~potassio + agua, data = dados)
library(ggplot)
library(lattice)
install.packages("emmeans")
library(emmeans)
x11()
xyplot(pesograo ~ potassio | agua, data = dados, type = c("p","a"))
xyplot(pesograo ~ agua | potassio, data = dados, type = c("p","a"))

dados$Potassio <- as.factor(dados$potassio)
dados$Agua <- as.factor(dados$agua)

ajuste <-  lm(pesograo ~ bloco+Potassio*Agua, data = dados)
summary(ajuste)

x11()
par(mfrow = c(2,2))
plot(ajuste)
par(mfrow = c(1,1))

anova(ajuste)

ajuste2 <- aov(pesograo ~ bloco + Agua/Potassio, data = dados)
coef(ajuste2)

summary(ajuste2, split = list("Agua:Potassio" = list("Pot@37.5" = c(1,4,7,10),
                                                     "Pot@50.0" = c(2,5,8,11),
                                                     "Pot@62.5" = c(3,6,9,12))))


ajuste3 <- aov(pesograo ~ bloco + Potassio/Agua, data = dados)
coef(ajuste3)

summary(ajuste3, split = list("Potassio:Agua" = list("Agua@0" = c(1,6),
                                                     "Agua@30" = c(2,7),
                                                     "Agua@60" = c(3,8),
                                                     "Agua@120" = c(4,9),
                                                     "Agua@180" = c(5,10))))

emm_pot_ag <- emmeans(ajuste, specs = ~ Potassio | Agua) # medias marginais
emm_ag_pot <- emmeans(ajuste, specs = ~ Agua | Potassio) # medias marginais

contrast(emm_ag_pot, method = "pairwise") # Comparacao de medias 
contrast(emm_pot_ag, method = "pairwise") # Comparacao de medias
