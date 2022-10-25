# Instalação e Abertura de Pacotes

## Instalação do tidyverse
install.packages("tidyverse") # se o pacote não estiver instalado

update.packages("tidyverse") # se o pacote não estiver instalado

## Carregar o pacote tidyverse
library("tidyverse")

# Importação do banco de dados
## Utilizando o pacote readr
read_delim("Ames_Data_delim.txt", delim = "|") # separado por |

read_csv("Ames_Data.csv") # separado por vírgula

read_csv("Data_Sets/Ames_Data.csv") # dentro de outra pasta na pasta do projeto

read_csv2("Ames_Data_semicolon.csv") # separado por ponto e vírgula (csv)

read_csv2("Ames_Data_semicolon.txt") # separado por ponto e vírgula (txt)

read_tsv("mtcars.tsv") # separado por tabulação (tsv)

read_tsv("mtcars_tsv.txt") # separado por tabulação (txt)

### Largura fixa de colunas
locations <- fwf_empty("fwf-sample_2.txt",
                       col_names = c("first", "last", "state", "ssn")) # adivinha a posição das colunas
read_fwf("fwf-sample_2.txt", locations)

locations <- fwf_widths(c(22, 10, 12), # quantidade de espaços
                        c("name", "state", "ssn")) # especifica a largura das colunas
read_fwf("fwf-sample_2.txt", locations)

locations <-
  fwf_cols(name = 22, state = 10, ssn = 12) # Argumentos nomeados com larguras de coluna
read_fwf("fwf-sample_2.txt", locations)


## Utilizando o pacote readxl
library(readxl) # carregar o pacote

read_xlsx("Ames_Data.xlsx") # ler o banco de dados

readxl::read_xlsx("Ames_Data.xlsx") # ler o banco de dados sem carregar o pacote

read_xlsx("datasets.xlsx") # lê o banco de dados da primeira planilha

read_excel("datasets.xlsx", 2) # lê o banco de dados da segunda planilha

read_xlsx("datasets.xlsx", 4) # lê o banco de dados da quarta planilha

read_excel("datasets.xlsx", "mtcars") # lê o banco de dados da planilha especificando o nome dela

"datasets.xlsx" |>
  excel_sheets() |>
  set_names() |>
  map(read_excel, path = "datasets.xlsx") # uso

excel_sheets("datasets.xlsx") |>
  set_names() |>
  lapply(read_excel, path = "datasets.xlsx")

"datasets.xlsx" |>
  excel_sheets() |>
  set_names() |>
  map_dfr(read_excel, path = "datasets.xlsx") |> view() # junta por linha

"datasets_2.xlsx" |>
  excel_sheets() |>
  set_names() |>
  map_dfc(read_excel, path = "datasets_2.xlsx") |> view() # junta por coluna


# Banco de dados como tibble
iris |> as_tibble() # transforma em tibble

tibble(x = 5:10, y = 2 * (3 ^ x)) # cria um tibble

tibble(a = 1, b = 1:3) # cria um tibble

tibble(a = character(), b = integer()) # cria um tibble com zero

# Operações nas linhas do banco de dados

Ames_Data <- readRDS("Ames_Data.rds") # Importando o banco de dados
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

Ames_Data |>
  filter(Year_Built %in% c(2000, 2001, 2003))  # Valores contidos em um vetor (x %in% vetor)

Ames_Data |>
  filter(!Year_Built %in% c(2000, 2001, 2003)) # Valores NÃO contidos em um vetor (!x %in% vetor)
  
Ames_Data |>                                      
  filter(between(Gr_Liv_Area, 2500, 3000))     # Entre (intervalo fechado)

Ames_Data |>
  filter(Gr_Liv_Area >= 2500 &
         Gr_Liv_Area <= 3000)

Ames_Data |>
  filter(near(Gr_Liv_Area, 2750, tol = 250))     # próximo (intervalo aberto)

Ames_Data |>
  filter(near(Sale_Price,
              mean(Sale_Price, na.rm = T), 
              tol = sd(Sale_Price,  na.rm = T)/sqrt(length(Sale_Price)) 
              )
         ) # média mais ou menos erro padrão

Ames_Data %>% 
  filter(str_detect(Overall_Qual, pattern = "Good"))

tolower("gOoD")

Ames_Data %>% 
  filter(str_detect(tolower(Overall_Qual), pattern = "good"))

Ames_Data |>
  filter(Overall_Qual == "Very_Good", 
         (Foundation != "PConc" | Garage_Cars > 2)) # Multiplas condições


msleep |> 
  filter(is.na(conservation)) # filtrar linhas vazias

msleep |> 
  filter(!is.na(conservation))

### Filtragem por múltiplas colunas

Ames_Data[, 1:3] %>% 
  filter_all(any_vars(. > 2000)) # União

Ames_Data[,1:3] %>% 
  filter_all(all_vars(. > 2000)) # Interseção

msleep %>% 
  filter_if(is.character, any_vars(is.na(.)))

msleep %>% 
  filter_if(is.numeric, any_vars(is.na(.)))

Ames_Data %>% 
  filter_at(1:3, all_vars(.> 2000)) # União

Ames_Data %>% 
  filter_at(vars(Gr_Liv_Area, Year_Built, Year_Remod_Add), all_vars(.> 2000)) # União

Ames_Data %>% 
  filter_at(vars(contains("Year")), all_vars(.> 2000))

### Ordenação

Ames_Data %>% 
  arrange(Gr_Liv_Area)

Ames_Data %>% 
  arrange(desc(Gr_Liv_Area))

Ames_Data %>% 
  arrange(Year_Built)

Ames_Data %>% 
  arrange(Year_Built, Year_Remod_Add)

Ames_Data %>% 
  arrange(Year_Built, desc(Year_Remod_Add))

mtcars %>%
  group_by(cyl) %>% # Agrupa por cilindradas e organiza
  arrange(desc(mpg)) %>% data.frame()

### Partição ou Amostragem

mtcars %>% slice(1:5)

mtcars %>% slice(-(1:20))

mtcars %>% slice(20:n())

mtcars %>% slice_head(n = 5)

mtcars %>% slice_tail(n = 5)

mtcars %>% slice_min(mpg, n = 5)

mtcars %>% slice_max(mpg, n = 5)

mtcars %>% slice_sample(n = 5)

mtcars %>% slice_sample(n = 5, replace = TRUE)

mtcars %>% sample_frac()

mtcars %>% sample_frac(.2)

iris %>% group_by(Species) %>% sample_frac(.1)

## Operações nas Colunas

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
  mutate_if(is.numeric, scale) # aplica uma função sob uma condição

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

iris %>% rename(sepal_length = Sepal.Length, sepal_width = 2)

iris %>% rename_with(toupper)

iris %>% rename_with(toupper, ends_with("Width"))

mtcars %>% rename_at(vars(mpg:hp), toupper)

Ames_Data %>% rename_if(is.integer, toupper)

mtcars %>% rename_all(toupper)

### Realocar Variáveis

mtcars %>% relocate(gear, carb)

mtcars %>% relocate(gear, carb, .after = hp)

mtcars %>% relocate(gear, carb, .before = hp)

mtcars %>% relocate(mpg, cyl, .after = last_col())

Ames_Data %>% relocate(where(is.factor))

Ames_Data %>% relocate(where(is.factor), .after = last_col())

Ames_Data %>% relocate(where(is.factor), .after = where(is.double))
