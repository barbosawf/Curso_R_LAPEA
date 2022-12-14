# Instala??o e Abertura de Pacotes

## Instala??o do tidyverse
# install.packages("tidyverse") # se o pacote n?o estiver instalado

# update.packages("tidyverse") # se o pacote n?o estiver instalado

## Carregar o pacote tidyverse
library("tidyverse")

# Importa??o do banco de dados (slide 11)
## Utilizando o pacote readr
### Dados separados por um determinado delimitador
read_delim("Ames_Data_delim.txt", delim = "|") # separado por |

### Dados separados por v?rgula ou ponto e v?rgula
read_csv("Ames_Data.csv", show_col_types = FALSE) # separado por v?rgula

read_csv("Data_Sets/Ames_Data.csv") # dentro de outra pasta na pasta do projeto

read_csv2("Ames_Data_semicolon.csv") # separado por ponto e v?rgula (csv)

read_csv2("Ames_Data_semicolon.txt") # separado por ponto e v?rgula (txt)

### Dados separados por tabula??o
read_tsv("mtcars.tsv") # separado por tabula??o (tsv)

read_tsv("mtcars_tsv.txt") # separado por tabula??o (txt)

### Largura fixa de colunas
locations <- fwf_empty("fwf-sample_2.txt",
                       col_names = 
                         c("first", "last", "state", "ssn")) # adivinha a posi??o das colunas
read_fwf("fwf-sample_2.txt", locations)

locations <- fwf_widths(c(22, 10, 12), # quantidade de espa?os
                        c("name", "state", "ssn")) # especifica as colunas
read_fwf("fwf-sample_2.txt", locations)

locations <-
  fwf_cols(name = 22, state = 10, ssn = 12) # Argumentos nomeados com larguras de coluna
read_fwf("fwf-sample_2.txt", locations)


## Utilizando o pacote readxl (slide 13)
library(readxl) # carregar o pacote

read_xlsx("Ames_Data.xlsx") # ler o banco de dados

readxl::read_xlsx("Ames_Data.xlsx") # ler sem carregar o pacote

read_xlsx("datasets.xlsx") # l? a primeira planilha

read_excel("datasets.xlsx", 2) # l? a planilha especificada pela ordem

read_xlsx("datasets.xlsx", 4) # l? a planilha especificada pela ordem

read_excel("datasets.xlsx", "mtcars") # l? a planilha especificando o nome dela

{ # Bonus
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
} # Bonus

# Banco de dados como tibble (slide 17)
iris |> as_tibble() # transforma em tibble

tibble(x = 5:10, y = 2 * (3 ^ x)) # cria um tibble

tibble(a = 1, b = 1:3) # cria um tibble

tibble(a = character(), b = integer()) # cria um tibble sem linhas

tribble(
  ~colA, ~colB,
  "a",   1,
  "b",   2,
  "c",   3
) # facilita a cria??o de um tibble atrav?s da visualiza??o 

df <- tribble(
  ~x,  ~y,
  "a", 1:3,
  "b", 4:6
) # Se o valor na c?lula n?o for escalar, cria-se uma lista

df

df[[1]] # acesso a primeira coluna

df[[2]] # acesso a segunda coluna

# Manipula??o do banco de dados (dplyr) (slide 19)
## Opera??es nas linhas do banco de dados (slide 21)
### Importando o banco de dados

Ames_Data <- readRDS("Ames_Data.rds")

msleep <- ggplot2::msleep

iris <- as_tibble(iris)

mtcars <- as_tibble(mtcars)

### Filtragem com operadores l?gicos

Ames_Data %>%
  filter(Overall_Qual == "Good") # Qualidade geral do im?vel boa (x)

Ames_Data %>%
  filter(Gr_Liv_Area  > mean(Gr_Liv_Area , na.rm = TRUE)) # Maior que a m?dia da vari?vel (x)

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade geral do im?vel boa 
           Total_Bsmt_SF > 2100)  # ?rea do por?o maior que 2100 (x & y)

Ames_Data %>%
  filter(Overall_Qual == "Good" | # Qualidade geral do im?vel boa
           !Total_Bsmt_SF > 2100) # ?rea do por?o n?o seja maior que 2100 (x | y)

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade do geral do im?vel boa
           !Total_Bsmt_SF > 2100) # ?rea do por?o n?o seja maior que 2100 (x & !y)

Ames_Data %>%
  filter(Overall_Qual == "Good" |     # Qualidade geral do im?vel boa
         Overall_Qual == "Very_Good") # Qualidade geral do im?vel muito boa (x | x)

Ames_Data %>%
  filter(Overall_Qual == "Good" &       # Qualidade geral do im?vel boa
           Overall_Qual == "Very_Good") # Qualidade geral do im?vel muito boa (x & x)

Ames_Data %>%
  filter(xor(Total_Bsmt_SF < 500,                 # ?rea do por?o < 500
             Overall_Qual  == "Very_Good")) %>%   # Qualidade muito boa (xor(x, y))
  arrange(Total_Bsmt_SF)

Ames_Data %>%
  filter(xor(Total_Bsmt_SF < 500,                 # ?rea do por?o < 500
             Overall_Qual  == "Very_Good")) %>%   # Qualidade muito boa (xor(x, y))
  arrange(desc(Total_Bsmt_SF))

{# Bonus
####  Outras Filtragens (B?nus)
Ames_Data |>
  filter(Year_Built %in% c(2000, 2001, 2003))  # Valores contidos em um vetor (x %in% vetor)

Ames_Data |>
  filter(!Year_Built %in% c(2000, 2001, 2003)) # Valores N?O contidos em um vetor (!x %in% vetor)
  
Ames_Data |>                                      
  filter(between(Gr_Liv_Area, 2500, 3000))     # Entre (intervalo fechado)

Ames_Data |>
  filter(Gr_Liv_Area >= 2500 &
         Gr_Liv_Area <= 3000)                   # Equival?ncia com o anterior

Ames_Data |>
  filter(near(Gr_Liv_Area, 2750, tol = 250))     # pr?ximo (intervalo aberto)

Ames_Data |>
  filter(near(Sale_Price,
              mean(Sale_Price, na.rm = T), 
              tol = sd(Sale_Price,  na.rm = T)/sqrt(length(Sale_Price)) 
              )
         ) # m?dia mais ou menos erro padr?o

Ames_Data %>% 
  filter(str_detect(Overall_Qual, pattern = "Good")) # filtragem encontrando um padr?o

tolower("gOoD")

Ames_Data %>% 
  filter(str_detect(tolower(Overall_Qual), pattern = "good")) # filtragem encontrando um padr?o

Ames_Data |>
  filter(Overall_Qual == "Very_Good", 
         (Foundation != "PConc" | Garage_Cars > 2)) # Multiplas condi??es

msleep |> 
  filter(is.na(conservation)) # filtrar linhas vazias por uma vari?vel

msleep |> 
  filter(!is.na(conservation)) # remove as observa??es com NA em uma vari?vel


#### Filtragem por m?ltiplas colunas

Ames_Data[, 1:3] %>% 
  filter_all(any_vars(. > 2000)) # Uni?o

Ames_Data[,1:3] %>% 
  filter_all(all_vars(. > 2000)) # Interse??o

msleep %>% 
  filter_if(is.character, any_vars(is.na(.))) # filtra por condi??o

msleep %>% 
  filter_if(is.numeric, any_vars(is.na(.))) # filtra por condi??o

msleep %>% filter_all(all_vars(!is.na(.))) # filtra por condi??o

Ames_Data %>% 
  filter_at(1:3, all_vars(.> 2000)) # Interse??o

Ames_Data %>% 
  filter_at(vars(Gr_Liv_Area, Year_Built, Year_Remod_Add), all_vars(.> 2000)) # Interse??o

Ames_Data %>% 
  filter_at(vars(contains("Year")), all_vars(.> 2000)) # Interse??o

  } # Outras Filtragens (B?nus)

#### Ordena??o (slide 30)

Ames_Data %>% arrange(Gr_Liv_Area) # crescente

Ames_Data %>% arrange(desc(Gr_Liv_Area)) # decrescente

Ames_Data %>% arrange(Year_Built, Gr_Liv_Area) # crescente com duas vari?veis

Ames_Data %>% arrange(Year_Built, desc(Gr_Liv_Area)) # crescente e decrescente

mtcars %>%
  group_by(cyl) %>% # Agrupa por cilindradas e organiza
  arrange(desc(mpg)) %>% data.frame()

arrange_all(mtcars, desc) # arranja todas as vari?veis decrescentemente

arrange_if(Ames_Data, is.factor) # arranja sob uma determinada condi??o

arrange_at(mtcars, vars(gear, carb)) # ou arrange_at(mtcars, 11)

#### Parti??o (slide 32)

mtcars %>% slice(1:5) # retorna uma sequ?ncia de observa??es 

mtcars %>% slice(-(1:20)) # retorna uma sequ?ncia de observa??es complementar

mtcars %>% slice(20:n()) # retorna uma sequ?ncia de observa??es de x at? o total

mtcars %>% slice_head(n = 5) # retorna as n primeiras observa??es

mtcars %>% slice_tail(n = 5) # retorna as n ?ltimas observa??es

mtcars %>% slice_min(mpg, n = 5) # retorna as n menores observa??es

mtcars %>% slice_max(mpg, n = 5) # retorna as n maiores observa??es

mtcars %>% slice_sample(n = 5) # retorna uma amostra aleat?ria de tamanho n

mtcars %>% 
  slice_sample(n = 5, replace = TRUE) # retorna uma amostra aleat?ria de tamanho n com reposi??o

#### Amostragem (slide 34)
mtcars %>% sample_frac() # embaralha todo conjunto de dados

mtcars %>% sample_frac(.2) # retorna uma fra??o de observa??es aleat?rias

mtcars %>% sample_n(10) # retorna uma amostra aleat?ria de tamanho n

mtcars %>% 
  sample_n(50, replace = T) # retorna uma amostra aleat?ria de tamanho n com reposi??o

iris %>% group_by(Species) %>% 
  sample_frac(.1) # retorna uma fra??o de observa??es aleat?rias dentro de grupos 


### Opera??es nas Colunas (slide 36)
#### Criar ou modificar vari?veis
Ames_Data[, -c(1:3)] |>
  mutate(Price_by_Area = Sale_Price/Lot_Area,
         .keep = "all") # used, unused, none

Ames_Data[, -c(1:3)] |>
  mutate(Sale_Price = Sale_Price/1000,
         .keep = "unused") # used, unused, none

Ames_Data[, -c(1:3)] |>
  mutate(Garage_Cars = as_factor(Garage_Cars)) # as_factor(x)


Ames_Data[, -c(1:3)] |> 
  mutate(Good_Garage = 
           ifelse(Garage_Cars > 1, Garage_Cars, "No"), # ifelse(condition, a, b) 
         .after = "Garage_Cars"
         ) 
       
Ames_Data |>
  mutate_all(tolower) # aplica uma fun??o em todas as colunas

Ames_Data |>
  mutate_if(is.numeric, scale) # aplica uma fun??o nas vari?veis que atendem uma condi??o

Ames_Data |>
  mutate_at(c("Year_Built", "Gr_Liv_Area"), 
            scale) # aplica uma fun??o nas vari?veis especificadas

Ames_Data[, 1:3] %>%
  mutate_at(
    vars(contains("Year")), ~(./10)) # nas vari?veis que contem um termo especificado

Ames_Data %>% 
  mutate_if(is.numeric, 
            list(log = log10)) # adiciona o nome log nas novas vari?veis

#### Sele??o de vari?veis (slide 39)
Ames_Data %>% select(where(is.numeric)) # seleciona onde a condi??os for satisfeita

Ames_Data %>% select_if(is.numeric) # seleciona se condi??o for satisfeita

Ames_Data %>% select(contains("Area")) # seleciona a vari?vel que cont?m um determinado termo

Ames_Data %>% select(Gr_Liv_Area:Lot_Area) # seleciona um intervalo de vari?veis

Ames_Data %>% select(1:3) # seleciona um intervalo de vari?veis

Ames_Data %>% select(!(Gr_Liv_Area:Lot_Area )) # seleciona vari?veis fora do intervalo

iris %>% select(!ends_with("Width")) # n?o seleciona vari?veis com um dado termo no final

iris %>% select(starts_with("Petal") & ends_with("Width")) # duas condi??es para selecionar

#### Renomear Vari?veis

iris %>% 
  rename(sepal_length = Sepal.Length, sepal_width = 2) # renomeia vari?veis espeficicadas

iris %>% 
  rename_with(toupper) # renomeia utilizando uma fun??o

iris %>% 
  rename_with(toupper, ends_with("Width")) # renomeia utilizando uma fun??o e padr?es

mtcars %>% 
  rename_at(vars(mpg:hp), toupper) # renomeia uma sequ?ncia de vari?veis com uma fun??o

Ames_Data %>% 
  rename_if(is.integer, toupper) # renomeia vari?veis que atendam a determinadas condi??es

mtcars %>% 
  rename_all(toupper) # renomeia todas as vari?veis


#### Realocar Vari?veis (slide 43)

mtcars %>% relocate(gear, carb) # realoca as vari?veis na frente das outras

mtcars %>% 
  relocate(gear, carb, .after = hp) # realoca as vari?veis ap?s uma vari?vel espeficicada

mtcars %>% 
  relocate(gear, carb, .before = hp) # realoca as vari?veis antes de uma vari?vel espeficicada

mtcars %>% relocate(mpg, cyl, .after = last_col()) # realoca por ?ltimo

Ames_Data %>% relocate(where(is.factor)) # realoca sob uma determinada condi??o

Ames_Data %>% relocate(where(is.factor), .after = last_col()) # realoca sob uma condi??o

Ames_Data %>% relocate(where(is.factor), .after = where(is.double)) # combina condi??es na realoca??o

### Opera??es com agrupamentos (slide 45)

mtcars |> 
  group_by(cyl) -> mtcars_by_cyl

mtcars_by_cyl |> tally() # conta as observa??es em cada grupo

mtcars_by_cyl |> tally(sort = T) # conta as observa??es em cada grupo e ordena

mtcars_by_cyl %>% group_keys() # mostra os grupos formados

mtcars_by_cyl %>% group_indices() # mostra a que grupo cada observa??o pertence

mtcars_by_cyl %>% group_rows() # mostra as observa??es em cada grupo

mtcars_by_cyl %>% group_vars() # mostra as vari?veis de agrupamento 

#### Sumariza??o (slide 47)

mtcars_by_cyl %>%
  summarise(
    n = n(),
    mean_disp = mean(disp, na.rm = TRUE)
  ) # conta e tira a m?dia de uma vari?vel espec?fica

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise(mean_disp = mean(disp), mean_mpg = mean(mpg)) # mais de uma vari?vel 

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise(mean_disp = mean(disp), sd_disp = sd(disp)) # reuso correto de vari?veis

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise(disp = mean(disp), sd_dist = sd(disp)) # reuso incorreto de vari?veis

mtcars_by_cyl %>%
  summarise_at(c("mpg", "disp"), mean, na.rm = TRUE) # m?dia de vari?veis espeficicadas

mtcars_by_cyl %>% 
summarise_at(vars(disp:qsec), mean, na.rm = TRUE) # m?dia de vari?veis espeficicadas numa faixa

Ames_Data %>%
  group_by(Overall_Qual) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) # m?dia de vari?veis que atendem uma condi??o

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise_all(list(mean, sd, median, length)) # aplica??o de mais de uma fun??o em todas as vari?veis

mtcars_by_cyl %>%
  select(1:5) %>%
  summarise_all(list(mean = mean, sd = sd)) # aplica??o de mais de uma fun??o em todas as vari?veis


## Salvamento de Banco de Dados ()
### Utilizando fun??o do pacote base do R
write_rds(Ames_Data, "Ames_Data.rds") # 

### Utilizando o pacote readr
write_csv(Ames_Data, file = "Ames_Data.csv") # separa os valores por v?rgula 

write_csv2(Ames_Data, file = "Ames_Data2.txt") # separa os valores por ponto e v?rgula

write_delim(Ames_Data, 
            file = "Ames_Data_delim.txt", 
            delim = "/") # separa os valores por um delimitador especificado

write_tsv(mtcars, "mtcars.tsv") # separa os valores por tabula??o
