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


exporta <- read_excel("EXP_2015_2022.xlsx")

# load data
exporta <- rename(exporta, municipio = "Município", destino = "País", uf = 
                 "UF do Município", ano2015 = "2015 - Valor FOB (US$)", ano2016 =
                 "2016 - Valor FOB (US$)", ano2017 = "2017 - Valor FOB (US$)", ano2018 =
                 "2018 - Valor FOB (US$)", ano2019 = "2019 - Valor FOB (US$)", ano2020 =
                 "2020 - Valor FOB (US$)", ano2021= "2021 - Valor FOB (US$)", ano2022 = 
                 "2022 - Valor FOB (US$)")



# Montando a base de dados
#2015 cidade/uf
exporta2015muni <- od_to_odmatrix(exporta, attrib = 11, name_orig = 1, name_dest = 3)
exporta2015UF <- od_to_odmatrix(exporta, attrib = 11, name_orig = 2, name_dest = 3)

#degree
grau<-degree(exporta2015muni) #funcionando
grau <- is.data.frame(grau)

# crie um grafo bipartido
g <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(g)$color <- ifelse(V(g)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(g)

# Grau e distribuição de grau dos vértices

centr_eigen(
  g,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

g <- sample_pa(1000, m = 4)
centr_degree(g)$centralization
centr_clo(g, mode = "all")$centralization
centr_betw(g, directed = FALSE)$centralization
centr_eigen(g, directed = FALSE)$centralization

g0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
g1 <- make_star(10, mode = "undirected")
centr_eigen(g0)$centralization
centr_eigen(g1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(g, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(g))
centr_eigen(g, normalized = TRUE)$centralization


  
# Usaremos também o comando vertex.label=NA para especificar que não
#queremos imprimir labels nos vértices da rede e vertex.size para especificar
#o tamanho dos vértices.
library(sand)
exportaup <- upgrade_graph(g)
plot(exportaup, vertex.label=NA)
plot(exportaup, layout=layout_with_kk(exportaup), vertex.label=NA, vertex.size=10)

