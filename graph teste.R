library(igraph)

# leia o arquivo Excel
exporta <- read_excel("EXP_2015_2022.xlsx")

# renomeie as colunas
exporta <- rename(exporta, municipio = "Município", destino = "País", uf = 
                    "UF do Município", ano2015 = "2015 - Valor FOB (US$)", ano2016 =
                    "2016 - Valor FOB (US$)", ano2017 = "2017 - Valor FOB (US$)", ano2018 =
                    "2018 - Valor FOB (US$)", ano2019 = "2019 - Valor FOB (US$)", ano2020 =
                    "2020 - Valor FOB (US$)", ano2021= "2021 - Valor FOB (US$)", ano2022 = 
                    "2022 - Valor FOB (US$)")

# crie um grafo bipartido
g <- graph_from_data_frame(exporta, directed = FALSE)

# defina as cores dos vértices
V(g)$color <- ifelse(V(g)$name %in% exporta$municipio, "blue", "red")

# plote o grafo
plot(g)

# Grau e distribuição de grau dos vértices
d<- data_frame(degree(
  graph,
  v = V(graph),
  mode = c("all", "out", "in", "total"),
  loops = TRUE,
  normalized = FALSE)
) 

write.csv(exporta2015muni, "exemplomunicipio.csv", row.names = FALSE)
