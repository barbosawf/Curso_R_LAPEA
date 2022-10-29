library(tidyverse)

cols_Ames <- readRDS("selected_cols_Ames.rds")


AmesHousing::make_ames() |>
  select(Gr_Liv_Area, # Área da sala de estar
         Year_Built, # Ano de construção
         Year_Remod_Add, # Ano de reforma
         Overall_Qual, # Qualidade do material geral e o acabamento da casa
         Foundation, # Tipo de fundação
         Total_Bsmt_SF, # Aárea total do porão
         Mas_Vnr_Type, # Tipo de folheado de alvenaria
         Lot_Area, # Tamanho do lote
         Garage_Cars, # Capacidade da garagem em número de carros
         Sale_Price) -> Ames_Data # Preço de venda

saveRDS(Ames_Data, "Ames_Data.rds")

write_csv(Ames_Data, file = "Ames_Data.csv")
write_csv2(Ames_Data, file = "Ames_Data2.txt")
write_delim(Ames_Data, file = "Ames_Data_delim.txt", delim = "|")
write_tsv(mtcars, "mtcars.tsv")


read_delim("Ames_Data_delim.txt", delim = "|")


(
  rbcb::get_series(code = c(ipca = 433, selic = 4390)) %>%
    reduce(left_join, by = "date") %>%
    mutate(Data = date,
           IPCA = ipca,
           SELIC = selic, .keep = "none"
           ) %>%
    gather(Indicador, Valor, IPCA:SELIC) %>%
    arrange(Data) -> ipca_selic
) # ou usa 

plyr::join_all(rbcb::get_series(code = c(ipca = 433, selic = 4390)))

write_csv(ipca_selic, file = "ipca_selic.csv")

txhousing %>% 
  mutate(Data = as.Date(
    ISOdate(year = year, month = month, day = 1))) -> tx_housing

write_csv(tx_housing, file = "tx_housing.csv")

#links
#https://daranzolin.github.io/2016-12-10-join-list-dataframes/