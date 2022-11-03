# Regressão Linear Múltipla (Análise de Resíduos)

# Carregamento dos pacotes
library(tidyverse)
library(faraway) # Belsley, D., Kuh. E. and Welsch, R. (1980) "Regression Diagnostics" Wiley.

## Dados (savings)
data(savings) # Função data() importa o banco de dados savings do pacote faraway
help(savings) # Mostra a descrição dos dados no help
savings <- savings |> 
  as_tibble() # Transforma em tibble
savings

## Dados (gala)
data(gala) # Função data() importa o banco de dados savings do pacote faraway
help(gala) # Mostra a descrição dos dados no help
gala <- gala |> 
  as_tibble() # Transforma em tibble
gala

## Ajuste do modelo

ms <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
summary(ms)

ms <- lm(sr ~ ., savings)
summary(ms)

## Diagnótico da regressão
plot(fitted(ms),residuals(ms), xlab = "Fitted", ylab = "Residuals")

### Resíduos x valores ajustados
tibble(fit = fitted(ms), 
       res = residuals(ms), 
       abs_res = residuals(ms) %>% abs()) -> d_s

d_s |>
  ggplot() + 
  geom_point(aes(x = fit, y = res)) +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted", y = "Residuals") +
  theme_classic()

### Resíduos x x_i
plot(savings$pop15, residuals(m), xlab = "Population under 15", ylab = "Residuals")
plot(savings$pop75, residuals(m), xlab = "Population over 75", ylab = "Residuals")

### Homogeneidade das variâncias

lm(abs_res ~ fit, d_s) %>% 
  summary()
plot(abs_res ~ fit, d_s, xlab = "Fitted", ylab = "|Residuals|")
abline(lm(abs_res ~ fit, d_s)) # A inclinação da reta não é significativa a 5%


### Gráficos ilustrativos para diagnosticar a homogeneidade de variâncias
graficos <- 4 # dando 4 exemplos de cada caso
par(mfrow = c(2, 2))

# - caso 1: homogeneidade de variâncias
for (i in 1:graficos) {
  plot(1:50, rnorm(50), ylab = "Residuals", main = "caso 1")
  abline(h = 0)
}

# - caso 2: heterogeneidade forte
for (i in 1:graficos) {
  plot(1:50, (1:50) * rnorm(50), ylab = "Residuals", main = "caso 2")
  abline(h = 0)
}

# - caso 3: heterogeneidade moderada
for (i in 1:graficos) {
  plot(1:50, sqrt((1:50)) * rnorm(50), ylab = "Residuals", main = "caso 3")
  abline(h = 0)
}

# - caso 4: não-linearidade
for (i in 1:graficos) {
  plot(1:50, cos((1:50) * pi / 25) + rnorm(50), ylab = "Residuals", main = "caso 4")
  abline(h = 0)
}
par(mfrow = c(1, 1))


## Transformação de y

mg <- lm(Species ~ . - Endemics, gala)

tibble(fit = fitted(mg), 
       res = residuals(mg), 
       abs_res = residuals(mg) %>% abs()) -> d_g


par(mfrow = c(1, 1))
plot(res ~ fit, data = d_g, 
     xlab = "Fitted", ylab = "Residuals")


# Tranformando y em raiz

mg_t <- lm(sqrt(Species) ~ . - Endemics, data = gala)
plot(
  x = fitted(mg_t),
  y = residuals(mg_t),
  xlab = "Fitted",
  ylab = "Residuals",
  main = "Usando raiz quadrada"
)


# Verificando normalidade

qqnorm(residuals(ms), ylab = "Residuals")
qqline(residuals(ms))


### Histogramas e boxplot geralmente não são bons para checar normalidade. Veremos:

par(mfrow = c(1, 2)) #rodar os plots acima
hist(residuals(ms))
boxplot(residuals(ms), horizontal = T)
par(mfrow = c(1, 1))


### Gráficos ilustrativos para diagnosticar a homogeneidade de variâncias


## Forma original das distribuções
dev.off()
par(mfrow = c(2, 2))

# - caso 1: Normal
curve(dnorm(x), -5, 5, ylim = c(0, 1), main = "Normal") 

# - caso 2: Lognormal
curve(dlnorm(x), -5, 5, ylim = c(0,1), col = "red", main = "Lognormal")

# - caso 3: Cauchy
curve(dcauchy(x), -5, 5, ylim = c(0,1), col = "blue", main = "Cauchy")

# - caso 4: Uniforme
curve(dunif(x), -5, 5, ylim = c(0,1), col = "green", main = "Uniforme")


## Residuos de cada distribuição
graficos <- 4
par(mfrow = c(2, 2))

# - caso 1: Normal
for (i in 1:graficos) {
  x <- rnorm(50)
  qqnorm(x, main = "Normal")
  qqline(x)
}

# - caso 2: Lognormal
for (i in 1:graficos) {
  x <- exp(rnorm(50))
  qqnorm(x, main = "Lognormal")
  qqline(x)
}

# - caso 3: Cauchy
for (i in 1:graficos) {
  x <- rcauchy(50)
  qqnorm(x, main = "Cauchy")
  qqline(x)
}

# - caso 4: Uniforme
for (i in 1:graficos) {
  x <- runif(50)
  qqnorm(x, main = "Uniforme")
  qqline(x)
}

par(mfrow = c(1, 1))

# Teste de shapiwo-wilkis para normalidade

shapiro.test(rnorm(50))
shapiro.test(rcauchy(50))
curve(dnorm(x), -5, 5)
curve(dcauchy(x), -5, 5, add = T, col = "blue")


## Cuidado quando n for pequeno
for (i in 1:10) {
  print(shapiro.test(rcauchy(5))$p.value)
}

for (i in 1:10) {
  print(shapiro.test(rcauchy(5))$p.value)
} 

# Dados correlacionados

data(airquality)
head(airquality)
pairs(airquality, panel = panel.smooth)
ma <- lm(Ozone ~ . - Day, data = airquality, na.action = na.exclude)
plot(fitted(ma), residuals(ma), xlab = "Fitted", ylab = "Residuals")

"Observa-se que não há constância na variância e também não há não linearidade.
Assim, pode-se fazer uma transformação que, neste caso, será logarítmica.
"

mal <- lm(log(Ozone) ~ . - Day, data = airquality, na.action = na.exclude)
plot(fitted(mal), residuals(mal), xlab = "Fitted", ylab = "Residuals")

## Checando se os erros são correlacionados

plot(residuals(mal), ylab = "Residuos")
abline(h = 0)
lines(residuals(mal), ylab = "Residuos")


## Plotando resíduos sucessivos
x11()
plot(residuals(mal)[-153],
     residuals(mal)[-1],
     xlab = expression(hat(epsilon)[i]),
     ylab = expression(hat(epsilon)[i + 1]))

## teste simples (modelo sem intercepto pois resíduos tem média zero)
summary(lm(residuals(mal)[-1] ~ -1 + residuals(mal)[-153])) # 
"Parece não haver correlação"


# Vejamos um teste formal: aplicando o DW

library(lmtest)
dwtest(Ozone ~ . - Day, data = na.omit(airquality))

"Observamos que não há evidência de correlação, Porém, cuidado, pois há
valores faltantes."


# Regressão Linear Múltipla (Multicolinearidade)
## Carregamento do banco de dados

data(seatpos)
seatpos <- seatpos |> as_tibble()
seatpos

## Estudo da posição do assento do motorista em função de vários preditores
help("seatpos")

## Ajuste do modelo
g <- lm(hipcenter ~ ., seatpos) # ou indica cada preditor de interesse
summary(g)

cat("Vemos, no quadro anterior, alguns sinais de multicolinearidade: \nnenhum preditor é significativo, mas R2 é relativamente alto.")

##
library(flextable)

library(flextable)


data("seatpos")

tab_cor <- round(cor(seatpos), 3)

tab_cor[upper.tri(tab_cor)] <- NA

diag(tab_cor) <- NA

tab_cor |>
  as_tibble(rownames = NA) |>
  rownames_to_column("Variável") |>
  flextable() |>
  bg(
    j = 2:ncol(tab_cor),
    bg = function(x) {
      out <- rep("transparent", length(x))
      out[x < -.9] <- "blue"
      out[x > .9] <- "red"
      out
    }
  )

