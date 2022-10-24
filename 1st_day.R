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

## Filtragem com operadores lógicos

Ames_Data %>%
  filter(Overall_Qual == "Good") # Qualidade do geral do imóvel boa

Ames_Data %>%
  filter(Gr_Liv_Area  > mean(Gr_Liv_Area , na.rm = TRUE)) # Maior que a média da variável

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade do geral do imóvel boa
           Total_Bsmt_SF > 2000) # Área do porão maior que 2000

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade do geral do imóvel boa
           !Total_Bsmt_SF > 2000) # Área do porão maior que 2000

Ames_Data %>%
  filter(Overall_Qual == "Good" & # Qualidade do geral do imóvel boa
           !Total_Bsmt_SF > 2000) # Área do porão maior que 2000

Ames_Data %>%
  filter(Overall_Qual == "Good" | # Qualidade do geral do imóvel boa
         Overall_Qual == "Very_Good") # Qualidade do geral do imóvel muito boa

Ames_Data %>%
  filter(xor(Gr_Liv_Area  > 2000,  # Área da sala de estar
             Lot_Area  > 10000)) # Área do lote

Ames_Data %>%
  filter(xor(Gr_Liv_Area  > 2000,  # Área da sala de estar
             Lot_Area  > 10000)) |> # Área do lote
  arrange (desc(Gr_Liv_Area)) 

Ames_Data |>
  filter(between(Gr_Liv_Area, 2500, 3000))

Ames_Data |>
  filter(Year_Built %in% c(2000, 2001, 2003))

msleep |> 
  filter(is.na(conservation))

msleep |> 
  filter(!is.na(conservation))
