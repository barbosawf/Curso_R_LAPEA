# Regressão Linear Múltipla (Análise de Resíduos)

# Carregamento dos pacotes
library(tidyverse) # Para manipulação
library(faraway) # Contém banco de dados e funções importantes para o diagnóstico de modelos
library(leaps) # Utilizado para seleção de variáveis no modelo
library(lmtest) # Para o teste de correlação temporal
library(flextable) # Para mostrar tabela personalizada

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

for (i in 1:15) {
  print(shapiro.test(rcauchy(5))$p.value)
}

for (i in 1:15) {
  print(shapiro.test(rnorm(5))$p.value)
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


dwtest(Ozone ~ . - Day, data = na.omit(airquality))

"Observamos que não há evidência de correlação, Porém, cuidado, pois há
valores faltantes."

# Regressão Linear Múltipla (Multicolinearidade)

## Multicolinearidade perfeita 
(X <- matrix(c(1,2,3,2,4,6,7,5,2), 3, 3)) # relação linear exata em duas colunas
(XtX <- t(X)%*%X)
solve(XtX) # Não é invertível, pois o determinante dá zero
MASS::ginv(XtX) # Usa-se inversa generalizada

(X <- matrix(sample(1:9,9), 3, 3))
(XtX <- t(X)%*%X)
solve(XtX)


## Carregamento do banco de dados

data(seatpos)
seatpos <- seatpos |> as_tibble()
seatpos

## Estudo da posição do assento do motorista em função de vários preditores
help("seatpos")

## Ajuste do modelo
g <- lm(hipcenter ~ ., seatpos) # ou indica cada preditor de interesse

cat("Vemos, no quadro anterior, alguns sinais de multicolinearidade: 
nenhum preditor é significativo, mas R2 é relativamente alto.")

### Resumo da análise
summary(g)

cat("Vemos, no quadro anterior, alguns sinais de multicolinearidade: \nnenhum preditor é significativo, mas R2 é relativamente alto.")


### Análise de variância
anova(g)


### Correlação duas a duas
(tab_cor <- cor(seatpos) |> round(3))
tab_cor[upper.tri(tab_cor, diag = T)] <- NA
tab_cor # correlações

library(flextable)
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

## Autovalores

(X <- model.matrix(g)[,-1]) # obtenção da matriz x

e <- eigen(cor(X)) # função eigen() retorna autovalores e autovetores
e

## Número de condição
max(e$values)/min(e$values) #critério NC de Montgomery e Peck (1981)? 

sqrt(max(e$values)/min(e$values)) #Se >=30 indica problema

## Vif para cada variável
### Cálculos manuais do vif
a <- summary(lm(X[,1] ~ X[,-1]))$r.squared
a
1/(1-a)


#matriz de correlação
m.cor <- cor(X)
#solve(m.cor)
diag(solve(m.cor))

### Cálculo do vif pela função do pacote faraway
vif(g) # Interpretação: pode-se interpretar sqrt(307.4) = 17.5 como nos dizendo que o erro padrão para a variável HtShoes é 17.5 vezes maior do que teria sido sem multicolinearidade.

# Formas de tratar a multicolinearidade

## Eliminando variáveis muito correlacionadas
g2 <- lm(hipcenter ~ . - HtShoes - Ht, seatpos)
summary(g2)

### Cálculo do NC
X <- model.matrix(g2)[,-1] # obtenção da matriz x

e <- eigen(cor(X)) # função eigen() retorna autovalores e autovetores

max(e$values)/min(e$values) #critério NC de Montgomery e Peck (1981)? 

sqrt(max(e$values)/min(e$values)) #Se >=30 indica problema

### Cálculo do vif
vif(g2) 

### Faz sentido tirar as variáveis HtShoes e Ht do modelo?
help(seatpos)


# Regressão Linear Múltipla (Seleção de variáveis)
## Carregando o banco de dados
df_ms <- read_csv(file = "df_ms.csv", show_col_types = F)

# 
m123 <- lm(y ~ ., df_ms)

m123 |> anova()

X <- as.matrix(df_ms[, -1])
y <- as.matrix(df_ms[, 1])

m123 <- lm(y ~ X) 

# (sum(y^2) - sum(y)^2/length(y))/(length(y) - 1)
SSTotal <- var(y) * (length(y) - 1)
SSTotal

## Cálculo do R2
R2 <- 693.06 / (693.06 + 195.19)
#ou R2<-693.06/SSTotal
R2

## Cálculo do R2 ajustado
n <- length(y)
p = 4 #nesse caso em particular
1 - ((n - 1) / (n - p)) * (1 - R2)

#ou
1 - (24.399 / (888.25 / 11))

# ou
summary(m123)

Cp <- 195.19 / 24.399 - (n - 2 * p)
Cp


b <- regsubsets(y ~ ., data = df_ms, nbest = 3)
rs <- summary(b)

rs$which
new_rs <- list()

for (i in 1:length(rs)) {
  if (mode(rs[[i]]) == "numeric") {
    new_rs[[names(rs[i])]] <- rs[[i]]
  }
}

(bind_cols(new_rs) |>
  add_column(rs$outmat |> as_tibble(),
             .before = T) -> est_models)

est_models |> 
  flextable() |>
  bg(
    j = c(4, 6),
    bg = function(x) {
      out <- rep("transparent", length(x))
      out[x == max(x)] <- "lightgreen"
      out
    }
  ) |>
  bg(
    j = c(5, 7,8),
    bg = function(x) {
      out <- rep("transparent", length(x))
      out[x == min(x)] <- "lightblue"
      out
    }
  ) 

# Regressão Linear Múltipla (Backward, Forward, stepwise)
## Carregando o banco de dados
df_ms_2 <- read_csv(file = "df_ms_2.csv", show_col_types = F)
df_ms_2

## BACKWARD ELIMINATION
m1234 <- lm(y ~ x1 + x2 + x3 + x4, data = df_ms_2)
m1243 <- lm(y ~ x1 + x2 + x4 + x3, data = df_ms_2)
m1432 <- lm(y ~ x1 + x4 + x3 + x2, data = df_ms_2)
m4321 <- lm(y ~ x4 + x2 + x3 + x1, data = df_ms_2)

### Problema: os testes F são sequenciais e não parciais
anova(m1234)  # depende da ordem de entrada no modelo

anova(m1243)

anova(m1432)

anova(m4321)

summary(m1234) # O teste t é parcial

summary(m1243)

summary(m1432)

summary(m4321)

"observe que os testes t correspondem ao teste F parcial para a última
 variável que entrou no modelo.
 conclusão: melhor usar os testes t parciais"

### vejamos o t tabelado a 5% com  8 g.l.
qt(.975,8)

### vejamos o F tabelado a 5% com 1 e 8 g.l.
qf(.95,1,8)

"Eliminamos a variável X3 pois possui o menor Fcalc, ou o menor tcalc, ou, alternativamente, o maior pvalor.

OBS.: Veja a função Anova() no pacote car que fornece SStipo II library(car) Anova(m1234,type="II") #observe que é equivalente a summary()

Recalcular a nova equação de regressão sem a X3

Realizando os F parciais"

car::Anova(m1234, type = 2)  # depende da ordem de entrada no modelo

car::Anova(m4321, type = 2)

m124 <- lm(y ~ x1 + x2 + x4, data = dados)
anova(m124)


## FORWARD SELECTION
### Obter a variável com a maior correlação com Y.

(cor_df_ms_2 <- cor(df_ms_2)) # Assim, a variável X4 entrará primeiro no modelo.


m4 <- lm(y ~ x4, data = df_ms_2)
anova(m4) # Anova

summary(m4) # Resumo

### Acrescentar uma variável por vez e verificar o p-valor

m14 <- lm(y ~ x4 + x1, data = df_ms_2)
anova(m14)

m24 <- lm(y ~ x4 + x2, data = df_ms_2)
anova(m24)

m34 <- lm(y ~ x4 + x3, data = df_ms_2)
anova(m34)

### Depois de escolhido a segunda variável, acrescentar mais uma por vez
m124 <- lm(y ~ x1 + x4 + x2, data = df_ms_2)
anova(m124) # p valor > 0.05

m134 <- lm(y ~ x1 + x4 + x3, data = df_ms_2)
anova(m134) # p valor > 0.05

### Modelo escolhido foi o m14
coef(m14)

## Funções prontas
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