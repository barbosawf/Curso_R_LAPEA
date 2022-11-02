# Carregamento dos pacotes
library(tidyverse)
library(faraway) # Belsley, D., Kuh. E. and Welsch, R. (1980) "Regression Diagnostics" Wiley.

# Regressão Linear Múltipla
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
boxplot(residuals(ms))
boxplot(residuals(ms), horizontal = T)
par(mfrow = c(1, 1))


Vejamos alguns grÃ¡ficos ilustrativos que ajudam a ganhar experiÃªncias na avaliaÃ§Ã£o da normalidade ou afastamento dela. Para tal, foram utilizados dados gerados de 4 distribuiÃ§Ãµes:
  
  - caso 1: normal
- caso 2: lognormal
- caso 3: Cauchy
- caso 4: Uniforme

Inicialmente devemos plotar cada uma dads distribuiÃ§Ãµes acima para vermos sua forma original.
```{r}
curve(dnorm(x),-5,5,ylim=c(0,1), main = "caso 1")
curve(dlnorm(x),-5,5,add=T,col="red",main = "caso 2")
curve(dcauchy(x),-5,5,add=T,col="blue", main = "caso 3")
curve(dunif(x),-5,5,add=T,col="green", main="caso 4")
#separadamente
par(mfrow=c(2,2))
curve(dnorm(x),-5,5)
curve(dlnorm(x),-5,5,col="red")
curve(dcauchy(x),-5,5,col="blue")
curve(dunif(x),-5,5,col="green")
```

Vamos rodar 4 exemplos (realizaÃ§Ãµes) de cada caso. Assim poderemos ver suas nuances.

```{r}
graficos<-4
par(mfrow=c(2,2))
for (i in 1:graficos) {x<-rnorm(50);qqnorm(x, main = "caso 1");qqline(x)}
for (i in 1:graficos) {x<-exp(rnorm(50));qqnorm(x,main = "caso 2");qqline(x)}
for (i in 1:graficos) {x<-rcauchy(50);qqnorm(x,main="caso 3");qqline(x)}
for (i in 1:graficos) {x<-runif(50);qqnorm(x, main="caso 4");qqline(x)}
```
```{r}
par(mfrow=c(1,1))
```

Usando o teste de Shapiro-Wilks para realizar um teste formal

```{r}
shapiro.test(rnorm(50))
shapiro.test(rcauchy(50))
curve(dnorm(x),-5,5)
curve(dcauchy(x),-5,5,add=T,col="blue")
```

Cuidado: e se n Ã© pequeno? Vejamos um exemplo

```{r}
shapiro.test(rnorm(5))
shapiro.test(rcauchy(5))
#repetir as duas linhas acima e discutir. Usar outros valores de n.
```

Usaremos, agora, um exemplo sobre erros correlacionados (p.61 Faraway)
```{r}
data(airquality)
head(airquality)
pairs(airquality,panel=panel.smooth)
g<-lm(Ozone~Solar.R+Wind+Temp,airquality,na.action=na.exclude)
plot(fitted(g),residuals(g),xlab="Fitted",ylab="Residuals")
```

Observamos que nÃ£o hÃ¡ constÃ¢ncia na variÃ¢ncia, e tambÃ©m hÃ¡ naolinearidade. Assim, optou-se por fazer transformaÃ§Ã£o. No caso, utilizou-se a transformaÃ§Ã£o logarÃ­tmica de y.
```{r}
gl<-lm(log(Ozone)~Solar.R+Wind+Temp, airquality,na.action=na.exclude)
plot(fitted(gl),residuals(gl),xlab="Fitted",ylab="Residuos")
```

## Checando se erros sÃ£o correlacionados
Agora, checando correlaÃ§Ã£o nos residuos
```{r}
plot(residuals(gl),ylab="Residuos")
abline(h=0)
lines(residuals(gl),ylab="Residuos")
```

Observe que hÃ¡ missing values (valores perdidos) nos dados originais. Observe se hÃ¡ ocorrÃªncia de "runs" longos acima ou abaixo da linha zero. No exemplo acima aparentemente nada anormal.

Plotando residuos sucessivos.
```{r}
plot(residuals(gl)[-153],residuals(gl)[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
#teste simples (modelo sem intercepto pois resÃ­duos tem mÃ©dia zero)
summary(lm(residuals(gl)[-1]~-1+residuals(gl)[-153]))
```

Parece nÃ£o haver correlaÃ§Ã£o. Vejamos um teste formal: aplicando o DW
```{r}
library(lmtest)
dwtest(Ozone~Solar.R+Wind+Temp,data=na.omit(airquality))

```
Observamos que nÃ£o hÃ¡ evidÃªncia de correlaÃ§Ã£o. PorÃ©m, cuidado, pois hÃ¡ missing values.