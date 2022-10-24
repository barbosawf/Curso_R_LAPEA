library(tidyverse)

cols_Ames <- readRDS("selected_cols_Ames.rds")


AmesHousing::make_ames() |>
  select(Gr_Liv_Area, # �rea da sala de estar
         Year_Built, # Ano de constru��o
         Year_Remod_Add, # Ano de reforma
         Overall_Qual, # Qualidade do material geral e o acabamento da casa
         Foundation, # Tipo de funda��o
         Total_Bsmt_SF, # A�rea total do por�o
         Mas_Vnr_Type, # Tipo de folheado de alvenaria
         Lot_Area, # Tamanho do lote
         Garage_Cars, # Capacidade da garagem em n�mero de carros
         Sale_Price) -> Ames_Data # Pre�o de venda

saveRDS(Ames_Data, "Ames_Data.rds")

write_csv(Ames_Data, file = "Ames_Data.csv")
write_csv2(Ames_Data, file = "Ames_Data2.txt")
write_delim(Ames_Data, file = "Ames_Data_delim.txt", delim = "|")
write_tsv(mtcars, "mtcars.tsv")


read_delim("Ames_Data_delim.txt", delim = "|")
