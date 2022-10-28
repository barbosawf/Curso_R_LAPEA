# Carregar o pacote tidyverse
library("tidyverse")

# Gráficos no Pacote ggplot2
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# Gráfico de Dispersão
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() # especifica o bando de dados e a estética dentro da função ggplot

ggplot(data = mpg) + # especifica o bando de dados na função ggplot
  geom_point(mapping = aes(x = displ, y = hwy)) # espeficica a estética dentro da função geom_point

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) # espeficica os dados e a estética dentro da função geom_point

ggplot() + 
  geom_point(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) # Erro!!! Por quê?


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_point(data = dplyr::filter(mpg, displ > 5, hwy > 20), color = "red", size = 2.2)

####

# Change colors
p<-ggplot(df, aes(x=weight)) + 
  geom_histogram(color="black", fill="white")
p

mpg |>
  ggplot() + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

# Fatiamento
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

###


# Histograma
## Criando um banco de dados
set.seed(123) # semente aleatória

df <- tribble(
  ~sex,  ~weight,
  "M", rnorm(200, mean = 80, sd = 5),
  "F", rnorm(200, mean = 60, sd = 5)
) |>
  unnest(-sex) |> mutate(across(sex, as_factor))

df |> filter(sex == "M") -> df_M

str(df)

## Histograma padrão
ggplot(df, aes(x = weight)) + 
  geom_histogram() # histograma padrão

df_M |>
  ggplot() + 
  geom_histogram(aes(x = weight)) # histograma padrão

## Alterações
### Largura das barras
df_M |>
  ggplot(aes(x = weight)) + 
  geom_histogram(binwidth = 2)

### Quantidade de barras
df_M |>
  ggplot(aes(x = weight)) + 
  geom_histogram(bins = 7) 

### Cor do traço e do prenchimento das barras
ggplot(df, aes(x = weight)) +
  geom_histogram(color = "darkblue", fill = "lightblue") # fora da função aes: mudança geral

### Cor por categorias
ggplot(df, aes(x = weight)) +
  geom_histogram(
    aes(fill = sex), 
    color = "black") # dentro da função aes: cor do preenchimento escolhida automaticamente

ggplot(df, aes(x = weight, fill = sex)) +
  geom_histogram(color = "black") +
  scale_fill_manual(values=c("#999999", "#56B4E9")) # preenchimento utilizando outras funções
  

### Transparência das barras
ggplot(df, aes(x = weight)) +
  geom_histogram(aes(fill = sex), color = "black", alpha = 0.5) # alpha muda a transparência


scale_color_manual(values=c("#999999", "#56B4E9"))

# Gráfico de linhas

ipca_selic <- read_csv("ipca_selic.csv")

ipca_selic |> 
  filter(Indicador == "IPCA") %>%
  ggplot(aes(x = Data, y = Valor)) +
  geom_line() +
  ylab("IPCA")


ipca_selic |> 
  ggplot(aes(x = Data, y = Valor)) +
  geom_line(aes(color = Indicador), size = 1)  


## Temas pré-definidos

(ggplot(df, aes(x = weight, fill = sex)) +
    geom_histogram(color = "black") +
    scale_fill_manual(values = c("#999999", "#56B4E9")) -> p_hist)

p_hist + theme_bw()

p_hist + theme_classic() 

p_hist + theme_light()

p_hist + theme_linedraw()

p_hist + theme_minimal()


## Definindo themas
p_hist + theme(legend.position = "top")

(
  df |>
    group_by(sex) |>
    summarise_if(is.numeric, list(mean = mean)) -> df_mean)

p_hist + geom_vline(
  data = df_mean,
  aes(xintercept = mean),
  linetype = "dashed",
  size = 0.75
)

# Facetas

p_hist +   facet_grid(sex ~ .)
