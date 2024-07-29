library(igraph)
library(dplyr)
library(oddnet)
library(tsibble)
library(forcats)
library(readxl)
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


plot(anom$outlier_probability)
plot(anom$kde)
plot(anom$lookde)
plot(anom$outlier_scores)
plot(anom$bandwidth)
plot(anom$data)




# intento de color



### intento animacion 

# Leer los datos de las categorías de los nodos
Datos_Grupos_Colonia4 <- read_excel("data/Datos_Grupos_Colonia4.xlsx")

# Asegurarnos de que la columna de nodos y la de categorías estén correctamente nombradas
# Supongamos que las columnas son 'node' y 'category'
Datos_Grupos_Colonia4 <- Datos_Grupos_Colonia4 %>%
  rename(name = tag_id, category = Grupo)

# Verificar los nombres de los nodos en la lista de aristas
unique_nodes_in_edges <- unique(c(unlist(lapply(ant41_red_list, function(df) df$V1)), 
                                  unlist(lapply(ant41_red_list, function(df) df$V2))))

# Verificar los nombres de los nodos en el data frame de nodos
unique_nodes_in_vertices <- unique(Datos_Grupos_Colonia4$name)

# Verificar que todos los nodos en las aristas estén en el data frame de nodos
missing_nodes <- setdiff(unique_nodes_in_edges, unique_nodes_in_vertices)

# Añadir nodos faltantes con una categoría predeterminada
if (length(missing_nodes) > 0) {
  missing_nodes_df <- data.frame(name = missing_nodes, category = "unknown")
  Datos_Grupos_Colonia4 <- bind_rows(Datos_Grupos_Colonia4, missing_nodes_df)
}

library(igraph)

# Crear una lista para almacenar las redes en diferentes días
networks <- list()
edges_list <- list()

for (i in 1:length(ant41_red_list)) {
  # Crear el gráfico para el día i
  gr <- graph_from_data_frame(ant41_red_list[[i]], directed = FALSE, vertices = Datos_Grupos_Colonia4)
  # Guardar el gráfico en la lista
  networks[[i]] <- gr
  # Convertir el gráfico a data frame y añadir la información de tiempo
  edges_list[[i]] <- igraph::as_data_frame(gr, what = "edges") %>%
    mutate(time = unique(ant41_red_list[[i]]$day))
}

# Unir todos los data frames de enlaces en uno solo
edges_df <- bind_rows(edges_list)

# Crear el data frame de nodos con categorías
nodes <- igraph::as_data_frame(networks[[1]], what = "vertices")
# Asegurarnos de que el data frame de nodos tiene la columna de categoría
# Convertir la columna 'name' en 'nodes' a tipo 'character'
nodes <- nodes %>%
  mutate(name = as.character(name))

# Convertir la columna 'name' en 'Datos_Grupos_Colonia4' a tipo 'character'
Datos_Grupos_Colonia4 <- Datos_Grupos_Colonia4 %>%
  mutate(name = as.character(name))

# Realizar la unión de datos
nodes <- nodes %>%
  left_join(Datos_Grupos_Colonia4, by = "name")

library(ggraph)
library(gganimate)
library(tidygraph)
library(ggplot2)

# Convertir edges_df en un grafo para ggraph
graph_tbl <- as_tbl_graph(edges_df, directed = FALSE)

# Asegurarnos de que el data frame de nodos esté integrado en el grafo
graph_tbl <- graph_tbl %>%
  activate(nodes) %>%
  left_join(Datos_Grupos_Colonia4, by = "name")

# Crear el gráfico animado
p <- ggraph(graph_tbl, layout = 'fr') + 
  geom_edge_link(aes(frame = time), alpha = 0.8, color = 'grey') + 
  geom_node_point(aes(color = category), size = 5) + 
  scale_color_manual(values = c("F" = "red", "N" = "blue", "C" = "green", "unknown" = "grey")) + 
  theme_void()

# Añadir animación
animated_plot <- p + 
  transition_time(time) + 
  labs(title = "Día: {frame_time}")

# Guardar la animación como gif
animate(animated_plot, renderer = gifski_renderer("network_animation_2.gif"))




# Asegúrate de que el data frame `edges_df` tiene una columna de tiempo y selecciona solo 10 tiempos únicos
selected_times <- unique(edges_df$time) %>% head(10)

# Filtrar los datos de aristas para los 10 tiempos seleccionados
edges_df_subset <- edges_df %>%
  filter(time %in% selected_times)

# También asegúrate de que el data frame de nodos esté actualizado para los tiempos seleccionados
nodes_subset <- nodes %>%
  filter(name %in% unique(c(edges_df_subset$from, edges_df_subset$to)))

library(ggraph)
library(gganimate)
library(tidygraph)
library(ggplot2)

# Convertir el data frame filtrado en un grafo
graph_tbl_subset <- as_tbl_graph(edges_df_subset, directed = FALSE)

# Asegurarnos de que el data frame de nodos esté integrado en el grafo
graph_tbl_subset <- graph_tbl_subset %>%
  activate(nodes) %>%
  left_join(nodes_subset, by = "name")

library(ggraph)
library(gganimate)
library(tidygraph)
library(ggplot2)

# Verifica que el grafo tiene la columna 'category' en los nodos
graph_tbl_subset %>%
  activate(nodes) %>%
  head()

# Crear el gráfico animado con el subconjunto de datos
p_subset <- ggraph(graph_tbl_subset, layout = 'fr') + 
  geom_edge_link(aes(frame = time), alpha = 0.8, color = 'grey') + 
  geom_node_point(aes(color = category.x), size = 5) + 
  scale_color_manual(values = c("N" = "red", "F" = "blue", "C" = "green", "unknown" = "grey4")) + 
  theme_void()

# Añadir animación
animated_plot_subset <- p_subset + 
  transition_time(time) + 
  labs(title = "Día: {frame_time}")

# Guardar la animación como gif
animate(animated_plot_subset, renderer = gifski_renderer("network_animation_subset.gif"))