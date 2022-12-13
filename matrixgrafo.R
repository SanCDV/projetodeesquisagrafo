library(flextable)
library(GGally)
library(ggraph)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)
library(readxl)



EXP_2015_2022 <- read_excel("EXP_2015_2022.xlsx")

# load data
base <- rename(EXP_2015_2022, municipio = "Município", destino = "País", uf = 
                 "UF do Município", ano2015 = "2015 - Valor FOB (US$)", ano2016 =
                 "2016 - Valor FOB (US$)", ano2017 = "2017 - Valor FOB (US$)", ano2018 =
                 "2018 - Valor FOB (US$)", ano2019 = "2019 - Valor FOB (US$)", ano2020 =
                 "2020 - Valor FOB (US$)", ano2021= "2021 - Valor FOB (US$)", ano2022 = 
                 "2022 - Valor FOB (US$)")


# criar nova tabela de contingência
matrix <- base[c("municipio", "destino", "ano2015")]

# Carregamento dos dados
dados <- matrix

# Criação da matriz de correspondência com a função xtabs()
matrix_correspondencia2 <- xtabs(ano2015 ~ municipio + destino, data = dados)
matrix_correspondencia2

# Transformação da matriz de correspondência em uma tabela
tabela_correspondencia <- as.data.frame(matrix_correspondencia2)
tabela_correspondencia

# Instalação e carregamento do pacote dplyr
install.packages("dplyr")
library(dplyr)

# Cálculo da tabela de exportação com a função tapply
tabela_exportacao <- with(tabela_correspondencia,
                          data.frame(municipio = levels(tabela_correspondencia$municipio),
                                     ano2015 = tapply(tabela_correspondencia$ano2015, tabela_correspondencia$municipio, sum)))

# Exibição da tabela de exportação
tabela_exportacao


library(stplanr)
export2015<- od_to_odmatrix(dados, attrib = 3, name_orig = 1, name_dest = 2)
