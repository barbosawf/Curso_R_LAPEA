# Carregar o pacote tidyverse
library("tidyverse")

# Gráficos no Pacote ggplot2
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# Gráfico de Dispersão

## Especificação do banco de dados e da estética nas funções
### Tudo dentro da função ggplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() 

### Banco de dados na função ggplot e estética na função geom_*
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) 

### Dados e estética na função geom_*
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) 

### Exemplo de erro
ggplot() + 
  geom_point(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) # Erro!!! Por quê?

### Dados especificados em locais diferentes
ggplot() +
  geom_point(data = filter(mpg, displ > 4.5), 
             aes(x = displ, y = hwy), 
             color = "blue", size = 2.2) + 
  geom_point(data = filter(mpg, displ < 4.5), 
             aes(x = displ, y = hwy),
             color = "red", size = 2.2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_point(data = dplyr::filter(mpg, displ > 5, hwy > 20), 
             color = "red", size = 2.2)


## Mudança de cores dos pontos
### Gráfico padrão
mpg |>
  ggplot() + 
  geom_point(mapping = aes(x = displ, y = hwy))

### Unica coloração dos pontos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

### Coloração por categorias de uma variável
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))


## Mudança da transparência dos pontos
### Tranparência Geral
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), alpha = 1/5)

### Tranparência por categorias de uma variável
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))


## Mudança na forma pontos
### Única forma para todos os pontos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 14)

### Forma dos pontos por classes
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)) # Qual é o aviso?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = drv))


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
  geom_histogram(aes(fill = sex), 
                 color = "black", 
                 alpha = 0.3) # alpha muda a transparência

ggplot(df, aes(x = weight)) +
  geom_histogram(aes(fill = sex, alpha = sex), 
                 color = "black" 
  )

# Gráfico de densidade (Bonus)
{ 
## Gráfico padrão
ggplot(diamonds, aes(x = carat)) +
  geom_density()
  
## Alterações
### Largura da banda
ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 1/5)

ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 5)

### Categorização
ggplot(diamonds, aes(carat, fill = cut, colour = cut)) +
  geom_density(alpha = 0.1) 

ggplot(diamonds, aes(carat, fill = cut, colour = cut)) +
  geom_density(position = "stack") 

ggplot(df) +
  geom_density(aes(x = weight, fill = sex), 
               color = "black", alpha = 0.5) +
  scale_fill_manual(values=c("#999999", "#56B4E9")) +
  labs(title = "Gráfico de Densidade", x ="Weight(kg)", y = "Density")

} # Bonus (Gráficos de densidade)

# Boxplot
## Banco de dados
ToothGrowth |>
  as_tibble() |>
  mutate(dose = as_factor(dose)) -> ToothGrowth

## Gráfico Padrão
(ToothGrowth |>
  ggplot(aes(x = dose, y = len)) +
  geom_boxplot() -> p_bp)

## Alterações
### Cor da caixa por categorias de uma variável
ToothGrowth |>
  ggplot(aes(x = dose, y = len, color = dose)) +
  geom_boxplot()

### Cor do preenchimento por categorias de uma variável
ToothGrowth |>
  ggplot(aes(x = dose, y = len, fill = dose)) +
  geom_boxplot()

ToothGrowth |>
  ggplot(aes(x = dose, y = len, fill = dose)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Dark2") # função para mudar a cor do preenchimento

### Adição de whiskers
ToothGrowth |>
  ggplot(aes(x = dose, y = len, fill = supp)) +
  stat_boxplot(geom = 'errorbar') + 
  geom_boxplot()

### Adição de jitters
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2) # Ele adiciona uma pequena quantidade de variação aleatória à localização de cada ponto e é uma maneira útil de lidar com a sopreposição causada pela discrição em conjuntos de dados menores.

ToothGrowth |>
ggplot( aes( x = dose, y = len, fill = dose)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(position=position_jitter(0.2))

### Reordenação
mpg |>
  ggplot(aes(reorder(class, hwy), hwy, fill = class)) + # uso da função reorder
  geom_boxplot() +
  labs(x = "Class", y = "Highway Miles per Gallon", fill = "Class")

mpg |>
  ggplot(aes(reorder(class, -hwy), hwy, fill = class)) + # Qual foi a mudança?
  geom_boxplot() +
  labs(x = "Class", y = "Highway Miles per Gallon", fill = "Class") 

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

(df |>
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

p_hist +   facet_grid(. ~ sex)


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

df |>
  group_by(sex) |>
  mutate(obs = row_number()) |>
  
  pivot_wider(names_from = sex,
              
              values_from = weight) |>
  ggplot() +
  geom_histogram(aes(x = M, y = ..density..), fill = "#69b3a2") +
  geom_label(aes(x = 75, y = 0.05, label = "Male"), color = "#69b3a2") +
  geom_histogram(aes(x = `F`, y = -..density..), fill = "#404080") +
  geom_label(aes(x = 45, y = -0.05, label="Female"), color="#404080") +
  theme_minimal() +
  labs(x = "Peso (Kg)", y = "Densidade")

