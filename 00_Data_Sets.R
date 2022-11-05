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

df_ms <- tibble(
y = c(64, 71, 53, 67, 55, 58, 77, 57, 56, 51, 76, 68),
x1 = c(57, 59, 49, 62, 51, 50, 55, 48, 42, 42, 61, 57),
x2 = c(8, 10, 6, 11, 8, 7, 10, 9, 10, 6, 12, 9),
x3 = c(64, 100, 36, 121, 64, 49, 100, 81, 100, 36, 144, 81)
)

write_csv(df_ms, file = "df_ms.csv")


#dados<-read.table("dados extras/selecao modelos dados draper e smith 1981 usado apostila.txt",h=T)
df_ms_2 <- tibble(
y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7,
      72.5, 93.1, 115.9, 83.8, 113.3, 109.4),
x1 = c(7, 1, 11, 11,  7, 11,  3,  1,  2, 21,  1, 11, 10),
x2 = c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68),
x3 = c(6, 15,  8,  8,  6,  9, 17, 22, 18,  4, 23,  9,  8),
x4 = c(60, 52, 20, 47, 33, 22,  6, 44, 22, 26, 34, 12, 12))

write_csv(df_ms_2, file = "df_ms_2.csv")

#links
#https://daranzolin.github.io/2016-12-10-join-list-dataframes/