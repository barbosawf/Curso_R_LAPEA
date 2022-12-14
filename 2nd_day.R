# Carregar o pacote tidyverse
library("tidyverse")

# Gr?ficos no Pacote ggplot2 (Slide 52)
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# Gr?fico de Dispers?o (slide 58)

## Especifica??o do banco de dados e da est?tica nas fun??es
### Tudo dentro da fun??o ggplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() 

### Banco de dados na fun??o ggplot e est?tica na fun??o geom_*
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) 

### Dados e est?tica na fun??o geom_*
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) 

### Exemplo de erro
ggplot() + 
  geom_point(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) # Erro!!! Por qu??

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


## Mudan?a de cores dos pontos
### Gr?fico padr?o
mpg |>
  ggplot() + 
  geom_point(mapping = aes(x = displ, y = hwy))

### Unica colora??o dos pontos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

### Colora??o por categorias de uma vari?vel
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))


## Mudan?a da transpar?ncia dos pontos
### Tranpar?ncia Geral
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), alpha = 1/5)

### Tranpar?ncia por categorias de uma vari?vel
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))


## Mudan?a na forma pontos
### ?nica forma para todos os pontos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 14)

### Forma dos pontos por classes
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)) # Qual ? o aviso?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = drv))


# Histograma (slide 62)
## Criando um banco de dados
set.seed(123) # semente aleat?ria

df <- tribble(
  ~sex,  ~weight,
  "M", rnorm(200, mean = 80, sd = 5),
  "F", rnorm(200, mean = 60, sd = 5)
) |>
  unnest(-sex) |> mutate(across(sex, as_factor))

(df |> filter(sex == "M") -> df_M)

str(df)

## Histograma padr?o
ggplot(df, aes(x = weight)) + 
  geom_histogram() # histograma padr?o

df_M |>
  ggplot() + 
  geom_histogram(aes(x = weight)) # histograma padr?o

## Altera??es
### Largura das barras
df_M |>
  ggplot(aes(x = weight)) + 
  geom_histogram(binwidth = 2)

### Quantidade de barras
df_M |>
  ggplot(aes(x = weight)) + 
  geom_histogram(bins = 7) 

### Cor do tra?o e do prenchimento das barras
ggplot(df, aes(x = weight)) +
  geom_histogram(color = "darkblue", fill = "lightblue") # fora da fun??o aes: mudan?a geral

### Cor por categorias
ggplot(df, aes(x = weight)) +
  geom_histogram(
    aes(fill = sex), 
    color = "black") # dentro da fun??o aes: cor do preenchimento escolhida automaticamente

ggplot(df, aes(x = weight, fill = sex)) +
  geom_histogram(color = "black") +
  scale_fill_manual(values = c("#999999", "#56B4E9")) # preenchimento utilizando outras fun??es
  

### Transpar?ncia das barras
ggplot(df, aes(x = weight)) +
  geom_histogram(aes(fill = sex), 
                 color = "black", 
                 alpha = 0.3) # alpha muda a transpar?ncia

ggplot(df, aes(x = weight)) +
  geom_histogram(aes(fill = sex, alpha = sex), 
                 color = "black" 
  )

# Gr?fico de densidade (B?nus) (Slide 67)
{ 
## Gr?fico padr?o
ggplot(diamonds, aes(x = carat)) +
  geom_density()
  
## Altera??es
### Largura da banda
ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 1/5)

ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 5)

### Categoriza??o
ggplot(diamonds, aes(carat, fill = cut, colour = cut)) +
  geom_density(alpha = 0.1) 

ggplot(diamonds, aes(carat, fill = cut, colour = cut)) +
  geom_density(position = "stack") 

ggplot(df) +
  geom_density(aes(x = weight, fill = sex), 
               color = "black", alpha = 0.5) +
  scale_fill_manual(values=c("#999999", "#56B4E9")) +
  labs(title = "Gr?fico de Densidade", x ="Weight(kg)", y = "Density")

} # Bonus (Gr?ficos de densidade)

# Boxplot (slide 70)
## Banco de dados
(ToothGrowth |>
  as_tibble() |>
  mutate(dose = as_factor(dose)) -> ToothGrowth)

## Gr?fico Padr?o
(ToothGrowth |>
  ggplot(aes(x = dose, y = len)) +
  geom_boxplot() -> p_bp)

## Altera??es
### Cor da caixa por categorias de uma vari?vel
ToothGrowth |>
  ggplot(aes(x = dose, y = len, color = dose)) +
  geom_boxplot()

### Cor do preenchimento por categorias de uma vari?vel
ToothGrowth |>
  ggplot(aes(x = dose, y = len, fill = dose)) +
  geom_boxplot()

ToothGrowth |>
  ggplot(aes(x = dose, y = len, fill = dose)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Dark2") # fun??o para mudar a cor do preenchimento

### Adi??o de whiskers
ToothGrowth |>
  ggplot(aes(x = dose, y = len, fill = supp)) +
  stat_boxplot(geom = 'errorbar') + 
  geom_boxplot()


### Reordena??o (Slide 74)
mpg |>
  ggplot(aes(class, hwy, fill = class)) + 
  geom_boxplot() +
  labs(x = "Class", y = "Highway Miles per Gallon", fill = "Class")

mpg |>
  ggplot(aes(reorder(class, hwy), hwy, fill = class)) + # uso da fun??o reorder
  geom_boxplot() +
  labs(x = "Class", y = "Highway Miles per Gallon", fill = "Class")

mpg |>
  ggplot(aes(reorder(class, -hwy), hwy, fill = class)) + # Qual foi a mudan?a?
  geom_boxplot() +
  labs(x = "Class", y = "Highway Miles per Gallon", fill = "Class") 

# Gr?fico de linhas (slide 75)
## Importa??o dos  bancos de dados
ipca_selic <- read_csv("ipca_selic.csv")
tx_housing <- read_csv("tx_housing.csv")

## Gr?fico Padr?o
ipca_selic |> 
  filter(Indicador == "IPCA") %>%
  ggplot(aes(x = Data, y = Valor)) +
  geom_line() +
  ylab("IPCA")

## Altera??es
### Cor das linhas pelas categorias em uma outra vari?vel
ipca_selic |> 
  ggplot(aes(x = Data, y = Valor)) +
  geom_line(aes(color = Indicador), size = 1)  

### Tracejado, cor e espessura das linhas pelas categorias em uma outra vari?vel
ipca_selic |> 
  ggplot(aes(x = Data, y = Valor)) +
  geom_line(aes(color = Indicador, linetype = Indicador), size = 1)

### Mudan?as nas escalas dos eixos x e y
tx_housing |>
  group_by(Data) %>%
  summarise("Volume M?dio" = mean(volume, na.rm = TRUE)) %>%
  ggplot(aes(x = Data, y = `Volume M?dio`)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",")) 

# Gr?fico de colunas ou barras (slide 79)
## Gr?fico padr?o
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

## Altera??es
### Cor das barras por categorias da mesma vari?vel
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

### Cor das barras por categorias de uma outra vari?vel
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), 
           position = "dodge") #

### Mudan?a de orienta??o dos eixos
(ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut), 
           show.legend = FALSE,
           width = 1) -> p_bar)

p_bar + coord_flip()

### Coordenadas polar
p_bar + coord_polar()

### Adi??o dos erros
ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise_if(is.numeric, list(Length = mean, sd = sd)) %>%
  ggplot(aes(x = dose, y = Length, fill = supp)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Length - sd, ymax = Length + sd),
                width = .2,
                position = position_dodge(.9)) +
  scale_fill_brewer(palette = "Paired") 

## Temas pr?-definidos (Slide 84)

(ggplot(df, aes(x = weight, fill = sex)) +
    geom_histogram(color = "black") +
    scale_fill_manual(values = c("#999999", "#56B4E9")) -> p_hist)

p_hist + theme_bw()

p_hist + theme_classic() 

p_hist + theme_light()

p_hist + theme_linedraw()

p_hist + theme_minimal()

p_hist + theme_gray()

p_hist + theme_dark()

p_hist + theme_void()


# Fatiamento (slide 87)
## Usando a fun??o facet_grid
### Fatiamento pelas categorias de uma vari?vel
p_hist + facet_grid(sex ~ .)

p_hist + facet_grid(. ~ sex)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .) # fatiamento por linha

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(rows = vars(drv)) # fatiamento por linha

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl) # fatiamento por coluna

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(cols = vars(cyl)) # fatiamento por coluna

### Fatiamento pelas categorias de mais de uma vari?vel
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl) # duas vari?veis

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(fl + drv ~ cyl) # tr?s vari?veis


## Usando a fun??o facet_wrap
### Fatiamento espeficicando o n?mero de fatias em linhas
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

### Fatiamento espeficicando o n?mero de fatias em colunas
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, ncol = 2)

# Salvamento de gr?ficos (slide 91)
ggplot(mtcars, aes(mpg, wt)) +
  geom_point()

ggsave("mtcars.pdf") # PDF
ggsave("mtcars.png") # PNG
ggsave("mtcars.svg") # SVG
ggsave("mtcars.jpg") #JPG

ggsave("mtcars.pdf", width = 4, height = 4)
ggsave("mtcars.pdf", width = 20, height = 20, units = "cm")

# Exemplo Bonus
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

