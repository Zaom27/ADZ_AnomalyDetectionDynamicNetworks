library(igraph)
library(dplyr)
library(oddnet)
library(tsibble)
library(forcats)
#dat <- read_graph("~/../Desktop/DatosRedes/insecta-ant-colony5.edges")

datcol4 <- read.table("~/../Desktop/DatosRedes/insecta-ant-colony4.edges")




# datos originales --------------------------------------------------------


trescol <- datcol4 |> select(-V3) |> dplyr::rename(day = V4)
#Datos originales, pero agrupados
ant41_red_df <- trescol |> group_by(day)
# Separados por dias (41)
ant41_red_list <-  group_split(ant41_red_df)

ant41_red_df |> filter(day==10) |>
  igraph::graph_from_data_frame()  |>
    plot()
  
ant41_red_df |> filter(day==11) |>
  igraph::graph_from_data_frame()  |>
  plot()
ant41_red_df |> filter(day==12) |>
  igraph::graph_from_data_frame()  |>
  plot()

ant41_red_df |> filter(day==13) |>
  igraph::graph_from_data_frame()  |>
  plot()

networks <- list()
for (i in 1:length(ant41_red_list)) {
  gr <- graph_from_data_frame(ant41_red_list[[i]])
  networks[[i]] <- as_adjacency_matrix(gr)
  
}

anom <- anomalous_networks(networks,alpha = 0.05, fast = T)
anom
?anomalous_networks

plot(anom$outlier_probability)
plot(anom$kde)
plot(anom$lookde)
plot(anom$outlier_scores)
plot(anom$bandwidth)
plot(anom$data)
