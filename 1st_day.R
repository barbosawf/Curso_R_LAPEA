# Instala��o e Abertura de Pacotes

## Instala��o do tidyverse
install.packages("tidyverse") # se o pacote n�o estiver instalado

update.packages("tidyverse") # se o pacote n�o estiver instalado

## Carregar o pacote tidyverse
library("tidyverse")

# Importa��o do banco de dados
## Utilizando o pacote readr
read_delim("Ames_Data_delim.txt", delim = "|") # separado por |

read_csv("Ames_Data.csv") # separado por v�rgula

read_csv("Data_Sets/Ames_Data.csv") # dentro de outra pasta na pasta do projeto

read_csv2("Ames_Data_semicolon.csv") # separado por ponto e v�rgula (csv)

read_csv2("Ames_Data_semicolon.txt") # separado por ponto e v�rgula (txt)

read_tsv("mtcars.tsv") # separado por tabula��o (tsv)

read_tsv("mtcars_tsv.txt") # separado por tabula��o (txt)

### Largura fixa de colunas
locations <- fwf_empty("fwf-sample_2.txt",
                       col_names = c("first", "last", "state", "ssn")) # adivinha a posi��o das colunas
read_fwf("fwf-sample_2.txt", locations)

locations <- fwf_widths(c(22, 10, 12), # quantidade de espa�os
                        c("name", "state", "ssn")) # especifica a largura das colunas
read_fwf("fwf-sample_2.txt", locations)

locations <-
  fwf_cols(name = 22, state = 10, ssn = 12) # Argumentos nomeados com larguras de coluna
read_fwf("fwf-sample_2.txt", locations)


## Utilizando o pacote readxl
library(readxl) # carregar o pacote

read_xlsx("Ames_Data.xlsx") # ler o banco de dados

readxl::read_xlsx("Ames_Data.xlsx") # ler o banco de dados sem carregar o pacote

read_xlsx("datasets.xlsx") # l� o banco de dados da primeira planilha

read_excel("datasets.xlsx", 2) # l� o banco de dados da segunda planilha

read_xlsx("datasets.xlsx", 4) # l� o banco de dados da quarta planilha

read_excel("datasets.xlsx", "mtcars") # l� o banco de dados da planilha especificando o nome dela

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

# Opera��es nas linhas do banco de dados

Ames_Data <- readRDS("Ames_Data.rds") # Importando o banco de dados
msleep <- ggplot2::msleep

## Filtragem com operadores l�gicos

Ames_Data %>%
  filter(Overall_Qual == "Good") # Qualidade geral do im�vel boa (x)

Ames_Data %>%
  filter(Gr_Liv_Area  > mean(Gr_Liv_Area , na.rm = TRUE)) # Maior que a m�dia da vari�vel (x)

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade geral do im�vel boa 
           Total_Bsmt_SF > 2100)  # �rea do por�o maior que 2100 (x & y)

Ames_Data %>%
  filter(Overall_Qual == "Good" | # Qualidade geral do im�vel boa
           !Total_Bsmt_SF > 2100) # �rea do por�o maior que 2100 (x | y)

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade do geral do im�vel boa
           !Total_Bsmt_SF > 2100) # �rea do por�o maior que 2100 (x & !y)

Ames_Data %>%
  filter(Overall_Qual == "Good" |     # Qualidade geral do im�vel boa
         Overall_Qual == "Very_Good") # Qualidade geral do im�vel muito boa (x | x)

Ames_Data %>%
  filter(Overall_Qual == "Good" &       # Qualidade geral do im�vel boa
           Overall_Qual == "Very_Good") # Qualidade geral do im�vel muito boa (x & x)

Ames_Data %>%
  filter(xor(Total_Bsmt_SF < 500,                 # �rea do por�o < 500
             Overall_Qual  == "Very_Good")) %>%   # Qualidade muito boa (xor(x, y))
  arrange(Total_Bsmt_SF)

Ames_Data %>%
  filter(xor(Total_Bsmt_SF < 500,                 # �rea do por�o < 500
             Overall_Qual  == "Very_Good")) %>%   # Qualidade muito boa (xor(x, y))
  arrange(desc(Total_Bsmt_SF))

Ames_Data |>
  filter(Year_Built %in% c(2000, 2001, 2003))  # Valores contidos em um vetor (x %in% vetor)

Ames_Data |>
  filter(!Year_Built %in% c(2000, 2001, 2003)) # Valores N�O contidos em um vetor (!x %in% vetor)
  
Ames_Data |>                                      
  filter(between(Gr_Liv_Area, 2500, 3000))     # Entre (intervalo fechado)

Ames_Data |>
  filter(Gr_Liv_Area >= 2500 &
         Gr_Liv_Area <= 3000)

Ames_Data |>
  filter(near(Gr_Liv_Area, 2750, tol = 250))     # pr�ximo (intervalo aberto)

Ames_Data |>
  filter(near(Sale_Price,
              mean(Sale_Price, na.rm = T), 
              tol = sd(Sale_Price,  na.rm = T)/sqrt(length(Sale_Price)) 
              )
         ) # m�dia mais ou menos erro padr�o

Ames_Data %>% 
  filter(str_detect(Overall_Qual, pattern = "Good"))

tolower("gOoD")

Ames_Data %>% 
  filter(str_detect(tolower(Overall_Qual), pattern = "good"))

Ames_Data |>
  filter(Overall_Qual == "Very_Good", 
         (Foundation != "PConc" | Garage_Cars > 2)) # Multiplas condi��es


msleep |> 
  filter(is.na(conservation)) # filtrar linhas vazias

msleep |> 
  filter(!is.na(conservation))

### Filtragem por m�ltiplas colunas

Ames_Data[, 1:3] %>% 
  filter_all(any_vars(. > 2000)) # Uni�o

Ames_Data[,1:3] %>% 
  filter_all(all_vars(. > 2000)) # Interse��o

msleep %>% 
  filter_if(is.character, any_vars(is.na(.)))

msleep %>% 
  filter_if(is.numeric, any_vars(is.na(.)))

Ames_Data %>% 
  filter_at(1:3, all_vars(.> 2000)) # Uni�o

Ames_Data %>% 
  filter_at(vars(Gr_Liv_Area, Year_Built, Year_Remod_Add), all_vars(.> 2000)) # Uni�o

Ames_Data %>% 
  filter_at(vars(contains("Year")), all_vars(.> 2000))

### Ordena��o

Ames_Data %>% 
  arrange(Gr_Liv_Area)

Ames_Data %>% 
  arrange(desc(Gr_Liv_Area))

Ames_Data %>% 
  arrange(Year_Built)

Ames_Data %>% 
  arrange(Year_Built, Year_Remod_Add)