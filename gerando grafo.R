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



# Montando a base de dados/matriz
#2015 cidade/uf
exporta2015muni <- od_to_odmatrix(exporta, attrib = 11, name_orig = 1, name_dest = 3)
exporta2015UF <- od_to_odmatrix(exporta, attrib = 11, name_orig = 2, name_dest = 3)


# crie um grafo bipartido
a <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(a)$color <- ifelse(V(a)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(a)

# Grau e distribuição de grau dos vértices

centr_eigen(
  a,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

a. <- sample_pa(1000, m = 4)
centr_degree(a)$centralization
centr_clo(a, mode = "all")$centralization
centr_betw(a, directed = FALSE)$centralization
centr_eigen(a, directed = FALSE)$centralization

a0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
a1 <- make_star(10, mode = "undirected")
centr_eigen(a0)$centralization
centr_eigen(a1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(a, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(a))
centr_eigen(a, normalized = TRUE)$centralization


  
# Usaremos também o comando vertex.label=NA para especificar que não
#queremos imprimir labels nos vértices da rede e vertex.size para especificar
#o tamanho dos vértices.
library(sand)
exportaup <- upgrade_graph(a)
plot(exportaup, vertex.label=NA)
plot(exportaup, layout=layout_with_kk(exportaup), vertex.label=NA, vertex.size=10)

#2016 cidade/uf
exporta2016muni <- od_to_odmatrix(exporta, attrib = 10, name_orig = 1, name_dest = 3)
exporta2016UF <- od_to_odmatrix(exporta, attrib = 10, name_orig = 2, name_dest = 3)


# crie um grafo bipartido
b <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(b)$color <- ifelse(V(b)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(b)

# Grau e distribuição de grau dos vértices

centr_eigen(
  b,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

b. <- sample_pa(1000, m = 4)
centr_degree(b)$centralization
centr_clo(b, mode = "all")$centralization
centr_betw(b, directed = FALSE)$centralization
centr_eigen(b, directed = FALSE)$centralization

b0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
b1 <- make_star(10, mode = "undirected")
centr_eigen(b0)$centralization
centr_eigen(b1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(b, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(b))
centr_eigen(b, normalized = TRUE)$centralization


#2017 cidade/uf
exporta2017muni <- od_to_odmatrix(exporta, attrib = 09, name_orig = 1, name_dest = 3)
exporta2017UF <- od_to_odmatrix(exporta, attrib = 09, name_orig = 2, name_dest = 3)


# crie um grafo bipartido
c <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(c)$color <- ifelse(V(c)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(c)

# Grau e distribuição de grau dos vértices

centr_eigen(
  c,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

c. <- sample_pa(1000, m = 4)
centr_degree(c)$centralization
centr_clo(c, mode = "all")$centralization
centr_betw(c, directed = FALSE)$centralization
centr_eigen(c, directed = FALSE)$centralization

c0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
c1 <- make_star(10, mode = "undirected")
centr_eigen(c0)$centralization
centr_eigen(c1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(c, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(c))
centr_eigen(c, normalized = TRUE)$centralization

#2018 cidade/uf
exporta2018muni <- od_to_odmatrix(exporta, attrib = 08, name_orig = 1, name_dest = 3)
exporta2018UF <- od_to_odmatrix(exporta, attrib = 08, name_orig = 2, name_dest = 3)


# crie um grafo bipartido
d <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(d)$color <- ifelse(V(d)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(d)

# Grau e distribuição de grau dos vértices

centr_eigen(
  d,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

d. <- sample_pa(1000, m = 4)
centr_degree(b)$centralization
centr_clo(d, mode = "all")$centralization
centr_betw(d, directed = FALSE)$centralization
centr_eigen(d, directed = FALSE)$centralization

d0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
d1 <- make_star(10, mode = "undirected")
centr_eigen(d0)$centralization
centr_eigen(d1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(d, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(d))
centr_eigen(d, normalized = TRUE)$centralization

#2016 cidade/uf
exporta2019muni <- od_to_odmatrix(exporta, attrib = 07, name_orig = 1, name_dest = 3)
exporta2019UF <- od_to_odmatrix(exporta, attrib = 07, name_orig = 2, name_dest = 3)


# crie um grafo bipartido
e <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(e)$color <- ifelse(V(e)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(e)

# Grau e distribuição de grau dos vértices

centr_eigen(
  e,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

e. <- sample_pa(1000, m = 4)
centr_degree(e)$centralization
centr_clo(e, mode = "all")$centralization
centr_betw(e, directed = FALSE)$centralization
centr_eigen(e, directed = FALSE)$centralization

e0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
e1 <- make_star(10, mode = "undirected")
centr_eigen(e0)$centralization
centr_eigen(e1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(e, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(e))
centr_eigen(e, normalized = TRUE)$centralization

#2020 cidade/uf
exporta2020muni <- od_to_odmatrix(exporta, attrib = 06, name_orig = 1, name_dest = 3)
exporta2020UF <- od_to_odmatrix(exporta, attrib = 06, name_orig = 2, name_dest = 3)


# crie um grafo bipartido
f <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(f)$color <- ifelse(V(f)$name %in% exporta$uf, "blue", "red")

# plote o grafo
plot(f)

# Grau e distribuição de grau dos vértices

centr_eigen(
  f,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

f. <- sample_pa(1000, m = 4)
centr_degree(f)$centralization
centr_clo(f, mode = "all")$centralization
centr_betw(f, directed = FALSE)$centralization
centr_eigen(f, directed = FALSE)$centralization

f0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
f1 <- make_star(10, mode = "undirected")
centr_eigen(f0)$centralization
centr_eigen(f1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(f, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(f))
centr_eigen(f, normalized = TRUE)$centralization

#2021 cidade/uf
exporta2021muni <- od_to_odmatrix(exporta, attrib = 05, name_orig = 1, name_dest = 3)
exporta2021UF <- od_to_odmatrix(exporta, attrib = 05, name_orig = 2, name_dest = 3)

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

g. <- sample_pa(1000, m = 4)
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

#2022 cidade/uf
exporta2022muni <- od_to_odmatrix(exporta, attrib = 04, name_orig = 1, name_dest = 3)
exporta2022UF <- od_to_odmatrix(exporta, attrib = 04, name_orig = 2, name_dest = 3)


# crie um grafo bipartido
h <- graph_from_data_frame(exporta, directed = FALSE )

# defina as cores dos vértices
V(h)$color <- ifelse(V(h)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(h)

# Grau e distribuição de grau dos vértices

centr_eigen(
  h,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE
)

h. <- sample_pa(1000, m = 4)
centr_degree(g)$centralization
centr_clo(g, mode = "all")$centralization
centr_betw(g, directed = FALSE)$centralization
centr_eigen(g, directed = FALSE)$centralization

h0 <- make_graph(c(2, 1), n = 10, dir = FALSE)
h1 <- make_star(10, mode = "undirected")
centr_eigen(h0)$centralization
centr_eigen(h1)$centralization

centr_eigen_tmax(graph = NULL, nodes = 0, directed = FALSE, scale = TRUE)

centr_eigen(h, normalized = FALSE)$centralization %>%
  `/`(centr_eigen_tmax(h))
centr_eigen(h, normalized = TRUE)$centralization



write.xlsx(exporta2016muni,file="exporta2016muni.xlsx")
