setwd("~/Estatística/6º Semestre/Computacional II")

# questão 2 
dados <- read.table("pib_gapminder.txt",
                    header = T,
                    skip = 8,
                    sep = ",",
                    dec = ",",
                    stringsAsFactors = F)


dados$pibPercap <- as.numeric(dados$pibPercap)
dados$expVida <- as.numeric(dados$expVida)
dados$pop <- as.numeric(dados$pop)

str(dados)
# a. A população do Sri Lanka em 1952 era de 7982342 habitantes. 
# a. verdadeiro
dados[dados$pais == "Sri Lanka" & dados$ano == 1952,]
# ---------------------------------------------------
# b. Apenas 2 paises possuem expectativa de vida acima de 82 anos. 
# b. verdadeiro
dados$pais[dados$expVida > 82]
# ---------------------------------------------------
# c. A expectativa de vida no Brasil em 2007 era de 72.39 anos. 
# c. verdadeiro
dados[dados$pais == "Brazil" & dados$ano == 2007,]
# ---------------------------------------------------
# d. A média do PIB per capita dos 4 paises que compõem o Mercosul atualmente foi de 6281. 
# d. falso
# mercosul atualmente:
# Brazil
# Argentina
# Uruguay
# Paraguay

a <- dados[dados$pais %in% c("Brazil",
                      "Argentina",
                      "Uruguay",
                      "Paraguay") & dados$ano == 2007,]
mean(a[,6])
# --------------------------------------------------
# e. Os dados correspondem a uma série temporal de 56 anos. 
# e. falso
length(unique(dados$ano))
2007 - 1952 # 55
# --------------------------------------------------
# f. A média do número de habitantes no continente europeu nos últimos 3 anos 
#    da base de dados (1997, 2002, 2007) foi de 19258517. 
# f. verdadeiro

b <- dados[dados$continente == "Europe" & dados$ano %in% c(1997,2002,2007),]
mean(b[,3])
# --------------------------------------------------
# g. Apenas 3 paises da Oceania estão presentes na base de dados. 
# g. falso
c <- dados[dados$continente == "Oceania",]
length(unique(c$pais))
# --------------------------------------------------
# h. O número de informações por continente é o seguinte: 
#    Africa (624), Américas (300), Asia (396), Europa (360), Oceania (24). 
# h. verdadeiro
table(dados$continente)

# --------------------------------------------------
# i. Os dados do Marrocos não constam nessa base de dados. 
# i. falso
dados[dados$pais == "Morocco",]

# --------------------------------------------------
# j. Considerando todos os anos disponiveis na base de dados, 
#    a media do PIB per capita do Brasil foi de 5829.317. 
# j. verdadeiro
k <- dados[dados$pais == "Brazil",]
mean(k[,6])

# questão 1

# a. falso
y <- c(1,2,3,4)
y[2.6]
# b. falso
m <- matrix(c(1,2,3,4))
m <- 0
m
# c. falso
x <- c("y","u")
letters[x]
# d. verdadeiro
x <- list(1,2,3)
class(x)
class(x[c(1,3)])
typeof(x)
typeof(x[c(1,3)])
# e. verdadeiro
a <- data.frame(a = c(1,2,3), b = c(1,2,3))
class(a)
class(a[1,2, drop = F])

# questão 3 

x <- 1:100
y <- numeric(length(x))
for(i in seq_along(x)) {
  if (x[i] %% 5 == 0) {
    y[i] <- x[i]/5
  } else {
    if(x[i] %% 2 == 0) {
      y[i] <- x[i]/2
    } else {
      y[i] <- x[i]
    }
  }
}


# a. falso
# não é zero

# b. falso

# c.

# d.

# e.

x
y



