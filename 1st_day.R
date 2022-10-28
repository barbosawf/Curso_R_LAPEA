# Instalação e Abertura de Pacotes

## Instalação do tidyverse
# install.packages("tidyverse") # se o pacote não estiver instalado

# update.packages("tidyverse") # se o pacote não estiver instalado

## Carregar o pacote tidyverse
library("tidyverse")

# Importação do banco de dados
## Utilizando o pacote readr
### Dados separados por um determinado delimitador
read_delim("Ames_Data_delim.txt", delim = "|") # separado por |

### Dados separados por vírgula ou ponto e vírgula
read_csv("Ames_Data.csv") # separado por vírgula

read_csv("Data_Sets/Ames_Data.csv") # dentro de outra pasta na pasta do projeto

read_csv2("Ames_Data_semicolon.csv") # separado por ponto e vírgula (csv)

read_csv2("Ames_Data_semicolon.txt") # separado por ponto e vírgula (txt)

### Dados separados por tabulação
read_tsv("mtcars.tsv") # separado por tabulação (tsv)

read_tsv("mtcars_tsv.txt") # separado por tabulação (txt)

### Largura fixa de colunas
locations <- fwf_empty("fwf-sample_2.txt",
                       col_names = 
                         c("first", "last", "state", "ssn")) # adivinha a posição das colunas
read_fwf("fwf-sample_2.txt", locations)

locations <- fwf_widths(c(22, 10, 12), # quantidade de espaços
                        c("name", "state", "ssn")) # especifica as colunas
read_fwf("fwf-sample_2.txt", locations)

locations <-
  fwf_cols(name = 22, state = 10, ssn = 12) # Argumentos nomeados com larguras de coluna
read_fwf("fwf-sample_2.txt", locations)


## Utilizando o pacote readxl
library(readxl) # carregar o pacote

read_xlsx("Ames_Data.xlsx") # ler o banco de dados

readxl::read_xlsx("Ames_Data.xlsx") # ler sem carregar o pacote

read_xlsx("datasets.xlsx") # lê a primeira planilha

read_excel("datasets.xlsx", 2) # lê a planilha especificada pela ordem

read_xlsx("datasets.xlsx", 4) # lê a planilha especificada pela ordem

read_excel("datasets.xlsx", "mtcars") # lê a planilha especificando o nome dela

"datasets.xlsx" |>
  excel_sheets() |>
  set_names() |>
  map(read_excel, path = "datasets.xlsx") # Todas as planilhas numa lista 

excel_sheets("datasets.xlsx") |>
  set_names() |>
  lapply(read_excel, path = "datasets.xlsx")  # Todas as planilhas numa lista

"datasets.xlsx" |>
  excel_sheets() |>
  set_names() |>
  map_dfr(read_excel, path = "datasets.xlsx") |> view() # Junta o banco de dados por linha

"datasets_2.xlsx" |>
  excel_sheets() |>
  set_names() |>
  map_dfc(read_excel, path = "datasets_2.xlsx") |> view() # Junta o banco de dados por coluna


# Banco de dados como tibble
iris |> as_tibble() # transforma em tibble

tibble(x = 5:10, y = 2 * (3 ^ x)) # cria um tibble

tibble(a = 1, b = 1:3) # cria um tibble

tibble(a = character(), b = integer()) # cria um tibble sem linhas

tribble(
  ~colA, ~colB,
  "a",   1,
  "b",   2,
  "c",   3
) # facilita a criação de um tibble através da visualização 

df <- tribble(
  ~x,  ~y,
  "a", 1:3,
  "b", 4:6
) # Se o valor na célula não for escalar, cria-se uma lista

df

df[[1]] # acesso a primeira coluna

df[[2]] # acesso a segunda coluna

# Operações nas linhas do banco de dados
## Importando o banco de dados

Ames_Data <- readRDS("Ames_Data.rds")

msleep <- ggplot2::msleep

iris <- as_tibble(iris)

mtcars <- as_tibble(mtcars)

## Filtragem com operadores lógicos

Ames_Data %>%
  filter(Overall_Qual == "Good") # Qualidade geral do imóvel boa (x)

Ames_Data %>%
  filter(Gr_Liv_Area  > mean(Gr_Liv_Area , na.rm = TRUE)) # Maior que a média da variável (x)

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade geral do imóvel boa 
           Total_Bsmt_SF > 2100)  # Área do porão maior que 2100 (x & y)

Ames_Data %>%
  filter(Overall_Qual == "Good" | # Qualidade geral do imóvel boa
           !Total_Bsmt_SF > 2100) # Área do porão maior que 2100 (x | y)

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade do geral do imóvel boa
           !Total_Bsmt_SF > 2100) # Área do porão maior que 2100 (x & !y)

Ames_Data %>%
  filter(Overall_Qual == "Good" |     # Qualidade geral do imóvel boa
         Overall_Qual == "Very_Good") # Qualidade geral do imóvel muito boa (x | x)

Ames_Data %>%
  filter(Overall_Qual == "Good" &       # Qualidade geral do imóvel boa
           Overall_Qual == "Very_Good") # Qualidade geral do imóvel muito boa (x & x)

Ames_Data %>%
  filter(xor(Total_Bsmt_SF < 500,                 # Área do porão < 500
             Overall_Qual  == "Very_Good")) %>%   # Qualidade muito boa (xor(x, y))
  arrange(Total_Bsmt_SF)

Ames_Data %>%
  filter(xor(Total_Bsmt_SF < 500,                 # Área do porão < 500
             Overall_Qual  == "Very_Good")) %>%   # Qualidade muito boa (xor(x, y))
  arrange(desc(Total_Bsmt_SF))

{# Bonus
Ames_Data |>
  filter(Year_Built %in% c(2000, 2001, 2003))  # Valores contidos em um vetor (x %in% vetor)

Ames_Data |>
  filter(!Year_Built %in% c(2000, 2001, 2003)) # Valores NÃO contidos em um vetor (!x %in% vetor)
  
Ames_Data |>                                      
  filter(between(Gr_Liv_Area, 2500, 3000))     # Entre (intervalo fechado)

Ames_Data |>
  filter(Gr_Liv_Area >= 2500 &
         Gr_Liv_Area <= 3000)                   # Equivalência com o anterior

Ames_Data |>
  filter(near(Gr_Liv_Area, 2750, tol = 250))     # próximo (intervalo aberto)

Ames_Data |>
  filter(near(Sale_Price,
              mean(Sale_Price, na.rm = T), 
              tol = sd(Sale_Price,  na.rm = T)/sqrt(length(Sale_Price)) 
              )
         ) # média mais ou menos erro padrão

Ames_Data %>% 
  filter(str_detect(Overall_Qual, pattern = "Good")) # filtragem encontrando um padrão

tolower("gOoD")

Ames_Data %>% 
  filter(str_detect(tolower(Overall_Qual), pattern = "good")) # filtragem encontrando um padrão

Ames_Data |>
  filter(Overall_Qual == "Very_Good", 
         (Foundation != "PConc" | Garage_Cars > 2)) # Multiplas condições

msleep |> 
  filter(is.na(conservation)) # filtrar linhas vazias por uma variável

msleep |> 
  filter(!is.na(conservation)) # remove as observações com NA em uma variável


### Filtragem por múltiplas colunas

Ames_Data[, 1:3] %>% 
  filter_all(any_vars(. > 2000)) # União

Ames_Data[,1:3] %>% 
  filter_all(all_vars(. > 2000)) # Interseção

msleep %>% 
  filter_if(is.character, any_vars(is.na(.))) # filtra por condição

msleep %>% 
  filter_if(is.numeric, any_vars(is.na(.))) # filtra por condição

msleep %>% filter_all(all_vars(!is.na(.))) # filtra por condição

Ames_Data %>% 
  filter_at(1:3, all_vars(.> 2000)) # Interseção

Ames_Data %>% 
  filter_at(vars(Gr_Liv_Area, Year_Built, Year_Remod_Add), all_vars(.> 2000)) # Interseção

Ames_Data %>% 
  filter_at(vars(contains("Year")), all_vars(.> 2000)) # Interseção

  }

### Ordenação

Ames_Data %>% 
  arrange(Gr_Liv_Area) # crescente

Ames_Data %>% 
  arrange(desc(Gr_Liv_Area)) # decrescente

Ames_Data %>% 
  arrange(Year_Built, Year_Remod_Add) # crescente com duas variáveis

Ames_Data %>% 
  arrange(Year_Built, desc(Year_Remod_Add)) # crescente e decrescente

mtcars %>%
  group_by(cyl) %>% # Agrupa por cilindradas e organiza
  arrange(desc(mpg)) %>% data.frame()

### Partição ou Amostragem

mtcars %>% slice(1:5) # retorna uma sequência de observações 

mtcars %>% slice(-(1:20)) # retorna uma sequência de observações complementar

mtcars %>% slice(20:n()) # retorna uma sequência de observações de x até o total

mtcars %>% slice_head(n = 5) # retorna as n primeiras observações

mtcars %>% slice_tail(n = 5) # retorna as n últimas observações

mtcars %>% slice_min(mpg, n = 5) # retorna as n menores observações

mtcars %>% slice_max(mpg, n = 5) # retorna as n maiores observações

mtcars %>% slice_sample(n = 5) # retorna uma amostra aleatória de tamanho n

mtcars %>% 
  slice_sample(n = 5, replace = TRUE) # retorna uma amostra aleatória de tamanho n com reposição

mtcars %>% sample_frac() # embaralha todo conjunto de dados

mtcars %>% sample_frac(.2) # retorna uma fração de observações aleatórias

mtcars %>% sample_n(10) # retorna uma amostra aleatória de tamanho n

mtcars %>% 
  sample_n(50, replace = T) # retorna uma amostra aleatória de tamanho n com reposição

iris %>% group_by(Species) %>% 
  sample_frac(.1) # retorna uma fração de observações aleatórias dentro de grupos 


## Operações nas Colunas
### Criar ou modificar variáveis
Ames_Data[, -c(1:3)] |>
  mutate(Price_by_Area = Sale_Price/Lot_Area,
         .keep = "all") # used, unused, none

Ames_Data[, -c(1:3)] |>
  mutate(Sale_Price = Sale_Price/1000,
         .keep = "unused") # used, unused, none

Ames_Data[, -c(1:3)] |>
  mutate(Garage_Cars = as_factor(Garage_Cars)) # as_factor(x)

Ames_Data[, 2:3] |>
  mutate(Mean_Year = mean(c(Year_Built, Year_Remod_Add))) # mean(x, y)

Ames_Data[, -c(1:3)] |> 
  mutate(Good_Garage = 
           ifelse(Garage_Cars > 1, Garage_Cars, "No"), # ifelse(condition, a, b) 
         .after = "Garage_Cars"
         ) 
       
Ames_Data |>
  mutate_all(tolower) # aplica uma função em todas as colunas

Ames_Data |>
  mutate_if(is.numeric, scale) # aplica uma função nas variáveis que atendem uma condição

Ames_Data |>
  mutate_at(c("Year_Built", "Year_Remod_Add"), 
            scale) # aplica uma função nas variáveis especificadas

Ames_Data[, 1:3] %>%
  mutate_at(
    vars(contains("Year")), ~(./10)) # nas variáveis que contem um termo especificado

Ames_Data %>% 
  mutate_if(is.numeric, 
            list(log = log10)) # adiciona o nome log nas novas variáveis

### Seleção de variáveis
Ames_Data %>% select(where(is.numeric)) # seleciona onde a condiçãos for satisfeita

Ames_Data %>% select_if(is.numeric) # seleciona se condição for satisfeita

Ames_Data %>% select(contains("Area")) # seleciona a variável que contém um determinado termo

Ames_Data %>% select(Gr_Liv_Area:Year_Remod_Add) # seleciona um intervalo de variáveis

Ames_Data %>% select(1:3) # seleciona um intervalo de variáveis

Ames_Data %>% select(!(Gr_Liv_Area:Year_Remod_Add)) # seleciona variáveis fora do intervalo

iris %>% select(!ends_with("Width")) # não seleciona variáveis com um dado termo no final

iris %>% select(starts_with("Petal") & ends_with("Width")) # duas condições para selecionar

### Renomear Variáveis

iris %>% 
  rename(sepal_length = Sepal.Length, sepal_width = 2) # renomeia variáveis espeficicadas

iris %>% 
  rename_with(toupper) # renomeia utilizando uma função

iris %>% 
  rename_with(toupper, ends_with("Width")) # renomeia utilizando uma função e padrões

mtcars %>% 
  rename_at(vars(mpg:hp), toupper) # renomeia uma sequência de variáveis com uma função

Ames_Data %>% 
  rename_if(is.integer, toupper) # renomeia variáveis que atendam a determinadas condições

mtcars %>% 
  rename_all(toupper) # renomeia todas as variáveis


### Realocar Variáveis

mtcars %>% relocate(gear, carb) # realoca as variáveis na frente das outras

mtcars %>% 
  relocate(gear, carb, .after = hp) # realoca as variáveis após uma variável espeficicada

mtcars %>% 
  relocate(gear, carb, .before = hp) # realoca as variáveis antes de uma variável espeficicada

mtcars %>% relocate(mpg, cyl, .after = last_col()) # realoca por último

Ames_Data %>% relocate(where(is.factor)) # realoca sob uma determinada condição

Ames_Data %>% relocate(where(is.factor), .after = last_col()) # realoca sob uma condição

Ames_Data %>% relocate(where(is.factor), .after = where(is.double)) # combina condições na realocação

## Operações com agrupamentos

mtcars |> 
  group_by(cyl) -> mtcars_by_cyl

mtcars_by_cyl |> tally() # conta as observações em cada grupo

mtcars_by_cyl |> tally(sort = T) # conta as observações em cada grupo e ordena

mtcars_by_cyl %>% group_keys() # mostra os grupos formados

mtcars_by_cyl %>% group_indices() # mostra a que grupo cada observação pertence

mtcars_by_cyl %>% group_rows() # mostra as observações em cada grupo

mtcars_by_cyl %>% group_vars() # mostra as variáveis de agrupamento 

### Sumarização

mtcars_by_cyl %>%
  summarise(
    n = n(),
    disp = mean(disp, na.rm = TRUE)
  ) # conta e tira a média de uma variável específica

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise(mean_dist = mean(disp), mean_mpg = mean(mpg)) # mais de uma variável 

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise(mean_dist = mean(disp), sd_dist = sd(disp)) # reuso correto de variáveis

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise(disp = mean(disp), sd_dist = sd(disp)) # reuso incorreto de variáveis

mtcars_by_cyl %>%
  summarise_at(c("mpg", "disp"), mean, na.rm = TRUE) # média de variáveis espeficicadas

mtcars_by_cyl %>% 
summarise_at(vars(disp:qsec), mean, na.rm = TRUE) # média de variáveis espeficicadas numa faixa

mtcars_by_cyl %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) # média de variáveis que atendem uma condição

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise_all(list(mean, sd)) # aplicação de mais de uma função em todas as variáveis

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise_all(list(mean = mean, sd = sd)) # aplicação de mais de uma função em todas as variáveis


# Salvamento de Banco de Dados
## Utilizando função do pacote base do R
saveRDS(Ames_Data, "Ames_Data.rds") # pacote base

## Utilizando o pacote readr
write_csv(Ames_Data, file = "Ames_Data.csv") # separa os valores por vírgula 

write_csv2(Ames_Data, file = "Ames_Data2.txt") # separa os valores por ponto e vírgula

write_delim(Ames_Data, 
            file = "Ames_Data_delim.txt", 
            delim = "|") # separa os valores por um delimitador especificado

write_tsv(mtcars, "mtcars.tsv") # separa os valores por tabulação
