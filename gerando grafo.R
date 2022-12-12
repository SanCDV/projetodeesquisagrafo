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
library(gutenbergr)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)
library(read)
library(readxl)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load data
rom <- rename(EXP_2015_2017_20221025, municipio = "Município", destino = "País")


#  Agora transformamos essa tabela em uma matriz de coocorrência.

municipio <- crossprod(table(rom[1:2]))
diag(municipio) <- 0
brasil <- as.data.frame(municipio)

#   Para gerar grafos de rede dessa forma, definimos os nós e também podemos 
#adicionar informações sobre os nós que podemos usar posteriormente (como informações de frequência).

va <- brasil %>%
  dplyr::mutate(destino = rownames(.),
                entradas = rowSums(.)) %>%
  dplyr::select(destino, entradas) %>%
  dplyr::filter(!str_detect(destino, "SCENE"))

#  Agora, definimos as arestas , ou seja, as conexões entre os nós e, novamente, 
#podemos adicionar informações em variáveis ​​separadas que podemos usar posteriormente.

ed <- municipio %>%
  dplyr::mutate(from = rownames(.)) %>%
  tidyr::gather(to, Frequency, BALTHASAR:TYBALT) %>%
  dplyr::mutate(Frequency = ifelse(Frequency == 0, NA, Frequency))

# Gerar um objeto gráfico.

ig <- igraph::graph_from_data_frame(d=ed, vertices=va, directed = FALSE)

#  adicionaremos rótulos aos nós da seguinte maneira:

tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)

#  Quando agora plotamos nossa rede, ela se parece com a mostrada abaixo.

# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                alpha = .1) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 colour="gray10") +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

#  Usamos o número de ocorrências para definir o tamanho do vértice (ou tamanho do nó)

v.size <- V(tg)$Occurrences
# inspect
v.size

# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1) +
  geom_node_point(size=log(v.size)*2) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

# define colors (by family)
mon <- c("ABRAM", "BALTHASAR", "BENVOLIO", "LADY MONTAGUE", "MONTAGUE", "ROMEO")
cap <- c("CAPULET", "CAPULET’S COUSIN", "FIRST SERVANT", "GREGORY", "JULIET", "LADY CAPULET", "NURSE", "PETER", "SAMPSON", "TYBALT")
oth <- c("APOTHECARY", "CHORUS", "FIRST CITIZEN", "FIRST MUSICIAN", "FIRST WATCH", "FRIAR JOHN" , "FRIAR LAWRENCE", "MERCUTIO", "PAGE", "PARIS", "PRINCE", "SECOND MUSICIAN", "SECOND SERVANT", "SECOND WATCH", "SERVANT", "THIRD MUSICIAN")
# create color vectors
Family <- dplyr::case_when(sapply(tg, "[")$nodes$name %in% mon ~ "MONTAGUE",
                           sapply(tg, "[")$nodes$name %in% cap ~ "CAPULET",
                           TRUE ~ "Other")
# inspect colors
Family

# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight)) +
  geom_node_point(size=log(v.size)*2, 
                  aes(color=Family)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

# create a document feature matrix
romeo_dfm <- quanteda::as.dfm(romeo)
# create feature co-occurrence matrix
romeo_fcm <- quanteda::fcm(romeo_dfm)
# inspect data
head(romeo_fcm)

quanteda.textplots::textplot_network(romeo_fcm, 
                                     min_freq = .5, 
                                     edge_alpha = 0.5, 
                                     edge_color = "purple",
                                     vertex_labelsize = log(rowSums(romeo_fcm)),
                                     edge_size = 2)

dg <- ed %>%
  tidyr::drop_na()
dg <- dg[rep(seq_along(dg$Frequency), dg$Frequency), 1:2]
rownames(dg) <- NULL

names(igraph::degree(dgg))[which(igraph::degree(dgg) == max(igraph::degree(dgg)))]

igraph::betweenness(dgg)

names(igraph::betweenness(dgg))[which(igraph::betweenness(dgg) == max(igraph::betweenness(dgg)))]

names(igraph::closeness(dgg))[which(igraph::closeness(dgg) == max(igraph::closeness(dgg)))]

