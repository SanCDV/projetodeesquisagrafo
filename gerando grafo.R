# install packages
install.packages("flextable")
install.packages("GGally")
install.packages("ggraph")
install.packages("igraph")
install.packages("Matrix")
install.packages("network")
install.packages("quanteda")
install.packages("sna")
install.packages("tidygraph")
install.packages("tidyverse")
install.packages("tm")
install.packages("tibble")
install.packages("quanteda.textplots")
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
remotes::install_github("rlesur/klippy")

# activate packages
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
library(stplanr)
library(dplyr)


EXP_2015 <- read_excel("EXP_2015_2022.xlsx")

# load data
base <- rename(EXP_2015, municipio = "Município", destino = "País", uf = 
                 "UF do Município", ano2015 = "2015 - Valor FOB (US$)", ano2016 =
                 "2016 - Valor FOB (US$)", ano2017 = "2017 - Valor FOB (US$)", ano2018 =
                 "2018 - Valor FOB (US$)", ano2019 = "2019 - Valor FOB (US$)", ano2020 =
                 "2020 - Valor FOB (US$)", ano2021= "2021 - Valor FOB (US$)", ano2022 = 
                 "2022 - Valor FOB (US$)")


# criar nova tabela de contingência
matrix <- base[c("municipio", "destino", "ano2015")]
base$municipio <- as.character(base$municipio, base$destino, base$ano2015)


# Carregamento dos dados
dados <- matrix

# Criação da matriz de correspondência com a função xtabs()
matrix_correspondencia2 <- xtabs(ano2015 ~ municipio + destino, data = dados)
matrix_correspondencia2

# Transformação da matriz de correspondência em uma tabela
tabela_correspondencia <- as.data.frame(matrix_correspondencia2)
tabela_correspondencia

# Montando a base de dados
#2015 cidade/uf
export2015 <- od_to_odmatrix(base, attrib = 11, name_orig = 1, name_dest = 3)
export2015UF <- od_to_odmatrix(base, attrib = 11, name_orig = 2, name_dest = 3)

export2015 = as.character(as.numeric(export2015))
export2015 <- factor(c("name_origem", "nome_dest"))



..............................................................................

# criar nova tabela de contingência
cidades <- base[c("municipio")]
paises <- base[c("destino")]

origem = unique(cidades)
destino = unique(paises)

# Matriz de coocorrência
coocorrencia <-matrix(c (origem, destino), byrow = TRUE)
colnames(coocorrencia) = origem
rownames(coocorrencia) = destino

# Criando o objeto da rede
rede = graph_from_adjacency_matrix(coocorrencia, weighted = TRUE, mode = "upper")

# Plotando a rede
plot(rede, edge.width = E(rede)$weight * 0.5, vertex.label = V(rede)$name)


..............................................................................

library(igraph)
# criar nova tabela de contingência
cidades <- base[c("municipio")]
paises <- base[c("destino")]

origem = unique(cidades)
destino = unique(paises)

# Matriz de coocorrência
coocorrencia <-matrix(c (origem, destino), byrow = TRUE)
colnames(coocorrencia) = origem
rownames(coocorrencia) = destino

# Criando o objeto da rede
rede = graph_from_adjacency_matrix(coocorrencia, weighted = TRUE, mode = "upper")

# Plotando a rede
plot(rede, edge.width = E(rede)$weight * 0.5, vertex.label = V(rede)$name)

va <- export2015 %>%
  dplyr::mutate(municipio = rownames(.),
                destino = rowSums(.)) %>%
  dplyr::select(municipio, destino)
  
  