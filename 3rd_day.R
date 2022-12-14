# Regress?o Linear M?ltipla (An?lise de Res?duos)

# Carregamento dos pacotes
library(tidyverse) # Para manipula??o
library(faraway) # Cont?m banco de dados e fun??es importantes para o diagn?stico de modelos
library(leaps) # Utilizado para sele??o de vari?veis no modelo
library(lmtest) # Para o teste de correla??o temporal
library(flextable) # Para mostrar tabela personalizada

## Ajuste do modelo

### Dados (savings)
data(savings) # Fun??o data() importa o banco de dados savings do pacote faraway

help(savings) # Mostra a descri??o dos dados no help

savings <- savings |> 
  as_tibble() # Transforma em tibble

savings


### Forma did?tica
(y <- as.matrix(savings[, 1]))

(X <- as.matrix(data.frame(Intercept = 1, savings[, -1])))

(XtX <- t(X)%*%X)

(inv_XtX <- solve(XtX))

(B <- inv_XtX%*%t(X)%*%y)

### Forma pr?tica
ms <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
coef(ms)
summary(ms)

ms <- lm(sr ~ ., savings)
coef(ms)
summary(ms)

## Diagn?tico da regress?o
plot(fitted(ms),residuals(ms), xlab = "Fitted", ylab = "Residuals")

### Res?duos x valores ajustados
#### Homogeneidade das vari?ncias
tibble(fit = fitted(ms), 
       res = residuals(ms), 
       abs_res = residuals(ms) %>% abs()) -> d_s

d_s |>
  ggplot() + 
  geom_point(aes(x = fit, y = res)) +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted", y = "Residuals") +
  theme_classic()

### Res?duos versus x_i
plot(savings$pop15, residuals(ms), xlab = "Population under 15", ylab = "Residuals")
plot(savings$pop75, residuals(ms), xlab = "Population over 75", ylab = "Residuals")

### Res?duos absolutos vessus x_i
lm(abs_res ~ fit, d_s) %>% 
  summary()
plot(abs_res ~ fit, d_s, xlab = "Fitted", ylab = "|Residuals|")
abline(lm(abs_res ~ fit, d_s)) # A inclina??o da reta n?o ? significativa a 5%


### Gr?ficos ilustrativos para diagnosticar a homogeneidade de vari?ncias
graficos <- 4 # dando 4 exemplos de cada caso
par(mfrow = c(2, 2))

# - caso 1: homogeneidade de vari?ncias
for (i in 1:graficos) {
  plot(1:50, rnorm(50), xlab = expression(hat(y)), ylab = "Res?duos", main = "Homogeneidade")
  abline(h = 0)
}

# - caso 2: heterogeneidade forte
for (i in 1:graficos) {
  plot(1:50, (1:50) * rnorm(50), 
       xlab = expression(hat(y)), ylab = "|Res?duos|", main = "Heterogeneidade Forte")
  abline(h = 0)
}

# - caso 3: heterogeneidade moderada
for (i in 1:graficos) {
  plot(1:50, sqrt((1:50)) * rnorm(50), 
       xlab = expression(hat(y)), ylab = "REs?duos", main = "Heterogeneidade Moderada")
  abline(h = 0)
}

# - caso 4: n?o-linearidade
for (i in 1:graficos) {
  plot(1:50, cos((1:50) * pi / 25) + rnorm(50), 
       xlab = expression(hat(y)), ylab = "Res?duos", main = "N?o-Linearidade")
  abline(h = 0)
}
par(mfrow = c(1, 1))


## Transforma??o de y

### Dados (gala)
data(gala) # Fun??o data() importa o banco de dados gala do pacote faraway
help(gala) # Mostra a descri??o dos dados no help
gala <- gala |> 
  as_tibble() # Transforma em tibble
gala # Diversidade de esp?cies nas ilhas Gal?pagos

(mg <- lm(Species ~ . - Endemics, gala))

(tibble(fit = fitted(mg), 
       res = residuals(mg), 
       abs_res = residuals(mg) %>% abs()) -> d_g)

par(mfrow = c(1, 1))
plot(res ~ fit, data = d_g, 
     xlab = "Fitted", ylab = "Residuals")


### Tranformando y em raiz
#### Res?duos versus valores ajustados
(mg_t <- lm(sqrt(Species) ~ . - Endemics, data = gala))
plot(
  x = fitted(mg_t), y = residuals(mg_t),
  xlab = "Fitted", ylab = "Residuals",
  main = "Usando raiz quadrada")

#### Res?duos absolutos versus valores ajustados
plot(
  x = fitted(mg_t), y = abs(residuals(mg_t)),
  xlab = "Fitted",  ylab = "|Residuals|",
  main = "Usando raiz quadrada"
)

res_abs_x_aj <- lm(abs(residuals(mg_t)) ~ fitted(mg_t))
abline(res_abs_x_aj)
anova(res_abs_x_aj)

## Verificando normalidade (savings)
par(mfrow = c(1, 1))
qqnorm(residuals(ms), ylab = "Residuals")
qqline(residuals(ms))

### Histogramas e boxplot geralmente n?o s?o bons para checar normalidade. Veremos:

par(mfrow = c(1, 2)) #rodar os plots acima
hist(residuals(ms))
boxplot(residuals(ms), horizontal = T)
par(mfrow = c(1, 1))


### Gr?ficos ilustrativos para diagnosticar a homogeneidade de vari?ncias

#### Forma original das distribu??es

par(mfrow = c(2, 2))

##### - caso 1: Normal
curve(dnorm(x), -5, 5, ylim = c(0, 1), main = "Normal", lwd = 2) 

##### - caso 2: Lognormal
curve(dlnorm(x), -5, 5, ylim = c(0,1), col = "red", main = "Lognormal", lwd = 2)

##### - caso 3: Cauchy
curve(dcauchy(x), -5, 5, ylim = c(0,1), col = "blue", main = "Cauchy", lwd = 2)

##### - caso 4: Uniforme
curve(dunif(x), -5, 5, ylim = c(0,1), col = "green", main = "Uniforme", lwd = 2)


#### Residuos de cada distribui??o
graficos <- 4
par(mfrow = c(2, 2))

##### - caso 1: Normal
for (i in 1:graficos) {
  x <- rnorm(50)
  qqnorm(x, main = "Normal")
  qqline(x)
}

##### - caso 2: Lognormal
for (i in 1:graficos) {
  x <- exp(rnorm(50))
  qqnorm(x, main = "Lognormal")
  qqline(x)
}

##### - caso 3: Cauchy
for (i in 1:graficos) {
  x <- rcauchy(50)
  qqnorm(x, main = "Cauchy")
  qqline(x)
}

##### - caso 4: Uniforme
for (i in 1:graficos) {
  x <- runif(50)
  qqnorm(x, main = "Uniforme")
  qqline(x)
}

par(mfrow = c(1, 1))

### Teste de shapiwo-wilkis para normalidade
shapiro.test(rnorm(50)) # 50 valores

#### Se o n for grande
for (i in 1:15) {
  print(shapiro.test(rnorm(5000))$p.value) # 5000 valores da normal
}

#### Se o n for pequeno

for (i in 1:15) {
  print(shapiro.test(rnorm(5))$p.value) # 5 valores da normal
}


for (i in 1:15) {
  print(shapiro.test(rcauchy(5))$p.value) # 5 valores da cauchy
}

for (i in 1:15) {
  print(shapiro.test(rgamma(5, 8))$p.value) # 5 valores da gamma
}

for (i in 1:15) {
  print(shapiro.test(runif(50, 2, 10))$p.value) # 5 valores da uniforme
}


## Dados correlacionados temporalmente

data(airquality)
head(airquality)
pairs(airquality, panel = panel.smooth)

### Ajuste do modelo com dados n?o transformados
ma <- lm(Ozone ~ . - Day, data = airquality, na.action = na.exclude)
plot(fitted(ma), residuals(ma), xlab = "Fitted", ylab = "Residuals")

"Observa-se que n?o h? const?ncia na vari?ncia e tamb?m n?o h? n?o linearidade.
Assim, pode-se fazer uma transforma??o que, neste caso, ser? logar?tmica.
"
#### Checando se os erros s?o correlacionados

plot(residuals(ma), ylab = "Residuos")
abline(h = 0)
lines(residuals(ma), ylab = "Residuos")

#### Plotando res?duos sucessivos

plot(residuals(ma)[-153],
     residuals(ma)[-1],
     xlab = expression(hat(epsilon)[i]),
     ylab = expression(hat(epsilon)[i + 1]))

#### teste simples (modelo sem intercepto pois res?duos tem m?dia zero)
summary(lm(residuals(mal)[-1] ~ -1 + residuals(mal)[-153])) # 
"Parece n?o haver correla??o"

### Ajuste do modelo com dados n?o transformados
mal <- lm(log(Ozone) ~ . - Day, data = airquality, na.action = na.exclude)
plot(fitted(mal), residuals(mal), xlab = "Fitted", ylab = "Residuals")

#### Checando se os erros s?o correlacionados

plot(residuals(mal), ylab = "Residuos")
abline(h = 0)
lines(residuals(mal), ylab = "Residuos")


#### Plotando res?duos sucessivos
plot(residuals(mal)[-153],
     residuals(mal)[-1],
     xlab = expression(hat(epsilon)[i]),
     ylab = expression(hat(epsilon)[i + 1]))

#### teste simples (modelo sem intercepto pois res?duos tem m?dia zero)
summary(lm(residuals(mal)[-1] ~ -1 + residuals(mal)[-153])) # 
"Parece n?o haver correla??o"


# Vejamos um teste formal: aplicando o DW
dwtest(Ozone ~ . - Day, data = na.omit(airquality))

"Observamos que n?o h? evid?ncia de correla??o, Por?m, cuidado, pois h?
valores faltantes."

# Regress?o Linear M?ltipla (Multicolinearidade)

## Multicolinearidade perfeita 
(X <- matrix(c(1,2,3,2,4,6,7,5,2), 3, 3)) # rela??o linear exata em duas colunas
(XtX <- t(X)%*%X)
solve(XtX) # N?o ? invert?vel, pois o determinante d? zero
MASS::ginv(XtX) # Usa-se inversa generalizada

(X <- matrix(sample(1:9,9), 3, 3))
(XtX <- t(X)%*%X)
solve(XtX)


## Carregamento do banco de dados
data(seatpos)
(seatpos <- seatpos |> as_tibble())

## Estudo da posi??o do assento do motorista em fun??o de v?rios preditores
help("seatpos")

## Ajuste do modelo
g <- lm(hipcenter ~ ., seatpos) # ou indica cada preditor de interesse

### Resumo da an?lise
summary(g)

### An?lise de vari?ncia
car::Anova(g)

#### Vemos, no quadro anterior, alguns sinais de multicolinearidade: nenhum preditor ? significativo, mas R2 ? relativamente alto.

### Correla??o duas a duas
(tab_cor <- cor(seatpos) |> round(3))
tab_cor[upper.tri(tab_cor, diag = T)] <- NA
tab_cor # correla??es

library(flextable)
tab_cor |>
  as_tibble(rownames = NA) |>
  rownames_to_column("Vari?vel") |>
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

## Autovalores

(X <- model.matrix(g)[,-1]) # obten??o da matriz x

e <- eigen(cor(X)) # fun??o eigen() retorna autovalores e autovetores
e

## N?mero de condi??o
max(e$values)/min(e$values) #crit?rio NC de Montgomery e Peck (1981)? 

#sqrt(max(e$values)/min(e$values)) #Se >=30 indica problema

## Vif para cada vari?vel
### C?lculos manuais do vif
for (i in 1:ncol(X)) {
  R2 <- summary(lm(X[,i] ~ X[,-i]))$r.squared
  print(1/(1-R2))
}

### Diagonal da inversa da matriz de correla??o
m.cor <- cor(X)

diag(solve(m.cor))

### C?lculo do vif pela fun??o do pacote faraway
vif(g) 

#### Interpreta??o: pode-se interpretar sqrt(307.4) = 17.5 como nos dizendo que o erro padr?o para a vari?vel HtShoes ? 17.5 vezes maior do que teria sido sem multicolinearidade.

# Formas de tratar a multicolinearidade

## Eliminando vari?veis muito correlacionadas
g2 <- lm(hipcenter ~ . - HtShoes - Ht, seatpos)
summary(g2)

### C?lculo do NC
X <- model.matrix(g2)[,-1] # obten??o da matriz x

e <- eigen(cor(X)) # fun??o eigen() retorna autovalores e autovetores

max(e$values)/min(e$values) #crit?rio NC de Montgomery e Peck (1981)? 

sqrt(max(e$values)/min(e$values)) #Se >=30 indica problema

### C?lculo do vif
vif(g2) 

### Faz sentido tirar as vari?veis HtShoes e Ht do modelo?
help(seatpos)


# Regress?o Linear M?ltipla (Sele??o de vari?veis)
## Carregando o banco de dados
df_ms <- read_csv(file = "df_ms.csv", show_col_types = F)

## Ajuste do modelo com todas as vari?veis
m123 <- lm(y ~ ., df_ms)

car::Anova(m123) # apenas um preditor ? significativo

## Ajute de todas as poss?veis regress?es
b <- regsubsets(y ~ ., data = df_ms, nbest = 3)

### Extraindo as estat?sticas de cada modelo
rs <- summary(b)

new_rs <- list()

for (i in 1:length(rs)) {
  if (mode(rs[[i]]) == "numeric") {
    new_rs[[names(rs[i])]] <- rs[[i]]
  }
}

### Mostrando as estat?sticas de todos os modelos
(bind_cols(new_rs) |>
  add_column(rs$outmat |> as_tibble(),
             .before = T) -> est_models)

est_models |> 
  flextable() |>
  bg(j = c(4, 6),
    bg = function(x) {
      out <- rep("transparent", length(x))
      out[x == max(x)] <- "lightgreen"
      out } ) |>
  bg(j = c(5, 7,8),
    bg = function(x) {
      out <- rep("transparent", length(x))
      out[x == min(x)] <- "lightblue"
      out}) 

# Regress?o Linear M?ltipla (Backward, Forward, stepwise)
## Carregando o banco de dados
df_ms_2 <- read_csv(file = "df_ms_2.csv", show_col_types = F)
df_ms_2

## BACKWARD ELIMINATION
m1234 <- lm(y ~ x1 + x2 + x3 + x4, data = df_ms_2)
m4321 <- lm(y ~ x4 + x2 + x3 + x1, data = df_ms_2)

### Os testes F da fun??o anova do pacote stats s?o sequenciais e n?o parciais
#### Dependem da ordem de entrada no modelo
anova(m1234)  
anova(m4321) # A ordem importa

### Os testes t da fun??o summary do pacote base n?o parciais
#### N?o dependem da ordem de entrada no modelo
summary(m1234) 
summary(m4321)

### Os testes F da fun??o Anova do pacote car s?o parciais
#### N?o dependem da ordem de entrada no modelo
car::Anova(m1234) 
car::Anova(m4321)

### Recalcular a nova equa??o de regress?o sem a X3
m124 <- lm(y ~ x1 + x2 + x4, data = df_ms_2)
car::Anova(m124) 

### Recalcular a nova equa??o de regress?o sem a X4
m12 <- lm(y ~ x1 + x2, data = df_ms_2)
car::Anova(m12)
summary(m12)

## FORWARD SELECTION
### Obter a vari?vel com a maior correla??o com Y.

(cor_df_ms_2 <- cor(df_ms_2)) # Assim, a vari?vel X4 entrar? primeiro no modelo.

m4 <- lm(y ~ x4, data = df_ms_2)
anova(m4) # Anova
summary(m4) # Resumo

### Acrescentar uma vari?vel por vez e verificar o p-valor

m14 <- lm(y ~ x4 + x1, data = df_ms_2)
anova(m14)

m24 <- lm(y ~ x4 + x2, data = df_ms_2)
anova(m24)

m34 <- lm(y ~ x4 + x3, data = df_ms_2)
anova(m34)

### Depois de escolhido a segunda vari?vel, acrescentar mais uma por vez
m124 <- lm(y ~ x1 + x4 + x2, data = df_ms_2)
anova(m124) # p valor > 0.05

m134 <- lm(y ~ x1 + x4 + x3, data = df_ms_2)
anova(m134) # p valor > 0.05

### Modelo escolhido foi o m14
coef(m14)

## STEPWISE
### Vamos usar alfa do F_in = 10% e alfa do F_out = 5%

### Obter a vari?vel com a maior correla??o absoluta com Y.
cor(df_ms_2)

m4 <- lm(y ~ x4, data = df_ms_2)
anova(m4) 
summary(m4)

### Acrescentar uma vari?vel por vez e checar o p-valor a 10%
m14 <- lm(y ~ x4 + x1, data = df_ms_2) # x1 acrescentada
anova(m14)

m24 <- lm(y ~ x4 + x2, data = df_ms_2)  # x2 acrescentada
anova(m24)

m34 <- lm(y ~ x4 + x3, data = df_ms_2) # x3 acrescentada
anova(m34)

### Avaliar por um test parcial (t ou F) se ? necess?rio deletar a vari?vel inserida
summary(m14) 
car::Anova(m14)

### Acrescentar mais uma vari?vel por vez no modelo com 2 vari?veis e checar o p-valor a 10%
m142 <- lm(y ~ x1 + x4 + x2, data = df_ms_2)
anova(m142) # como a vari?vel X2 tem o menor p_valor ela ? acrescentada no modelo

m143 <- lm(y ~ x1 + x4 + x3, data = df_ms_2)
anova(m143)

### Avaliar por um test parcial (t ou F) se ? necess?rio deletar a vari?vel inserida
summary(m142) # x4 n?o significativa
car::Anova(m142) # x4 n?o significativa

### Deletar a vari?vel que n?o foi significativa a 5% e ajustar o novo modelo.
m12 <- lm(y ~ x1 + x2, data = df_ms_2)
summary(m12)

### Realizar o foward a partir do modelo ajustado considerando 10% de signific?ncia

m123 <- lm(y ~ x1 + x2 + x3, data = df_ms_2)
anova(m123)

m124 <- lm(y ~ x1 + x2 + x4, data = df_ms_2)
anova(m124)

#### Nenhuma das vari?veis (x3 e x4) foram significativas a 10%. 
#### Assim, n?o as inclu?mos no modelo. O modelo final ser?

coef(m12)

## Fun??es prontas
library(MASS)

(step_forward <- step(m1234, direction = "forward"))
summary(step_forward)
step_forward$anova

(step_backward <- step(m1234, direction = "backward"))
summary(step_backward)
step_backward$anova


(step_both <- step(m1234, direction = "both"))
summary(step_both)
step_both$anova
