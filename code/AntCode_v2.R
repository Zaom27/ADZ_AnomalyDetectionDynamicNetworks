### LIBRERIAS ###

library(igraph)
library(dplyr)
library(oddnet)
library(tsibble)
library(forcats)
library(tidygraph)
library(tidyverse)
library(ggraph)
library(readxl)


### Carga de datos

getwd()
setwd("C:/Users/diego/Documents/ADZ_AnomalyDetectionDynamicNetworks")

## Redes de la colonia 4
datcol4 <- read.table("data/insecta-ant-colony4.edges")

## Grupos de la colonia 4
Datos_Grupos_Colonia4 <- read_excel("data/Datos_Grupos_Colonia4.xlsx")


## Ajustes de nombres de columnas 

colnames(datcol4) <- c("from", "to", "weight", "day")
colnames(Datos_Grupos_Colonia4) <- c("name","category")


## Creacion de categorias faltantes

# Eliminar nodos duplicados en el data frame de nodos
nodes_unique <- Datos_Grupos_Colonia4 %>%
  distinct(name, .keep_all = TRUE)

# Obtener todos los nodos presentes en las aristas
all_nodes <- unique(c(datcol4$from, datcol4$to))

# Verificar que todos los nodos están en el data frame de nodos
missing_nodes <- setdiff(all_nodes, nodes_unique$name)
if(length(missing_nodes) > 0) {
  cat("Nodos faltantes en el data frame de nodos:", missing_nodes, "\n")
}

# Crear un data frame de nodos faltantes con una categoría predeterminada
nodes_missing_df <- data.frame(name = missing_nodes, category = "unknown")

# Combinar el data frame de nodos originales con los nodos faltantes
nodes_complete <- bind_rows(nodes_unique, nodes_missing_df)

# Filtrar los datos de nodos para incluir solo los nodos presentes en las aristas
nodes_complete <- nodes_complete %>%
  filter(name %in% all_nodes)

# Crear el grafo usando igraph
graph <- graph_from_data_frame(d = datcol4, vertices = nodes_complete, directed = FALSE)


### Creacion de animacion de la red

### Librerías adicionales para animación ###

library(gganimate)

### Convertir a un objeto tidygraph ###
graph_tbl <- as_tbl_graph(graph)

### Asegurarse de que las columnas necesarias estén presentes ###
graph_tbl <- graph_tbl %>%
  activate(nodes) %>%
  mutate(category = as.factor(category)) %>%
  activate(edges) %>%
  mutate(day = as.numeric(day))

### Crear la animación ###
p <- graph_tbl %>%
  ggraph(layout = "fr") +  # Fruchterman-Reingold layout
  geom_edge_link(alpha = 0.7, color = "gray12") +
  geom_node_point(aes(color = category), size = 5) + 
  scale_color_manual(values = c("F" = "indianred", "N" = "navy", "C" = "gold3", "unknown" = "grey80")) +
  theme_void() + labs(title = 'Día: {frame_time}') +
  transition_time(day) +
  ease_aes('linear')

### Guardar la animación ###
anim <- animate(p, nframes = length(unique(datcol4$day)), fps = 5, width = 800, height = 600)
anim_save("network_animationPrueba.gif", anim)

### Plot de dias especificos
# Función para filtrar y preparar los datos para ggplot2 en un día específico
prepare_data_for_day <- function(graph_tbl, day) {
  # Filtrar las aristas del día específico
  edges_day <- graph_tbl %>%
    activate(edges) %>%
    filter(day == !!day) %>%
    as_tibble()
  
  # Convertir las columnas 'from' y 'to' a carácter si son numéricas
  if (is.integer(edges_day$from) || is.integer(edges_day$to)) {
    edges_day <- edges_day %>%
      mutate(across(c(from, to), as.character))
  }
  
  # Imprimir información de las aristas
  cat("Número de aristas para el día", day, ":", nrow(edges_day), "\n")
  if (nrow(edges_day) > 0) {
    print(head(edges_day))
  }
  
  # Filtrar los nodos conectados por las aristas seleccionadas
  connected_nodes <- unique(c(edges_day$from, edges_day$to))
  nodes_day <- graph_tbl %>%
    activate(nodes) %>%
    filter(name %in% connected_nodes) %>%
    as_tibble()
  
  # Imprimir información de los nodos
  cat("Número de nodos para el día", day, ":", nrow(nodes_day), "\n")
  if (nrow(nodes_day) > 0) {
    print(head(nodes_day))
  }
  
  # Comprobar si todos los nodos en las aristas están presentes en el conjunto de nodos
  missing_nodes <- setdiff(c(edges_day$from, edges_day$to), nodes_day$name)
  if (length(missing_nodes) > 0) {
    cat("Nodos en las aristas pero no en los nodos:", missing_nodes, "\n")
  }
  
  list(nodes = nodes_day, edges = edges_day)
}

# Plotear los datos de red para un día específico usando ggplot2
plot_network_for_day <- function(graph_tbl, day) {
  data_day <- prepare_data_for_day(graph_tbl, day)
  
  nodes_day <- data_day$nodes
  edges_day <- data_day$edges
  
  if (is.null(nodes_day) || is.null(edges_day) || nrow(nodes_day) == 0 || nrow(edges_day) == 0) {
    cat("No hay datos suficientes para el día", day, "\n")
    return(NULL)
  }
  
  # Crear layout usando igraph para las posiciones de los nodos
  graph_igraph <- graph_from_data_frame(d = edges_day, vertices = nodes_day, directed = FALSE)
  
  # Verificar si el grafo se creó correctamente
  if (gorder(graph_igraph) == 0) {
    cat("El grafo tiene 0 nodos, lo que indica un problema en la construcción del grafo.\n")
    return(NULL)
  }
  
  layout <- layout_with_fr(graph_igraph)
  
  nodes_day <- nodes_day %>%
    mutate(x = layout[, 1], y = layout[, 2])
  
  # Convertir 'from' y 'to' a carácter para la unión
  edges_day <- edges_day %>%
    left_join(nodes_day %>% select(name, x, y), by = c("from" = "name")) %>%
    rename(xstart = x, ystart = y) %>%
    left_join(nodes_day %>% select(name, x, y), by = c("to" = "name")) %>%
    rename(xend = x, yend = y)
  
  ggplot() +
    geom_segment(data = edges_day, aes(x = xstart, y = ystart, xend = xend, yend = yend), alpha = 0.7, color="gray12") +
    geom_point(data = nodes_day, aes(x = x, y = y, color = category), size = 5) +
    theme_void() + scale_color_manual(values = c("F" = "indianred", "N" = "navy", "C" = "gold3", "unknown" = "grey80")) +
    labs(title = paste('Red Dia:', day)) +
    scale_size(range = c(0.5, 2))
}

# Plotear la red para el día 34
plot_network_for_day(graph_tbl, 3)

###  Deteccion de anomalias

networks <- list()
for (i in 1:length(ant41_red_list)) {
  gr <- graph_from_data_frame(ant41_red_list[[i]])
  networks[[i]] <- as_adjacency_matrix(gr)
  
}

anom <- anomalous_networks(networks,alpha = 0.1)
anom

# Tema personalizado
mitema <- theme(
  plot.title = element_text(family = "sans", face = "bold", size = 18, vjust = 0.5, hjust = 0.5, color = "black"),
  axis.title.x = element_text(family = "sans", face = "bold", size = 14, vjust = 0.5, hjust = 0.5, color = "gray23"),
  axis.title.y = element_text(family = "sans", face = "bold", size = 14, vjust = 0.5, hjust = 0.5, color = "gray23"),
  axis.text.x = element_text(family = "sans", face = "italic", size = 12, vjust = 0.5, hjust = 0.5, color = "gray22"),
  axis.text.y = element_text(family = "sans", face = "italic", size = 12, vjust = 0.5, hjust = 0.5, color = "gray22"),
  panel.background = element_rect(fill = "gray95"),
  panel.grid.major = element_line(color = "gray85", size = 1),
  panel.grid.minor = element_line(color = "gray85", size = 0.5),
  legend.title = element_text(family = "sans", face = "bold", size = 12, color = "Black"),
  legend.text = element_text(family = "sans", face = "italic", size = 10, color = "gray25")
)

# Crear el gráfico
ggplot(data = NULL, aes(x = 1:41, y = prob_outlier)) +
  geom_point(color = "darkblue", size = 3.5, shape = 16) +
  geom_line(color = "navy", size = 0.7, linetype = 2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size=1) +
  labs(
    title = "Probabilidad Condicional de Outliers en la Red",
    x = "Tiempo (días)",
    y = "Probabilidad de Outlier"
  ) +
  mitema +
  scale_x_continuous(breaks = seq(1, 41, 2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme(plot.title = element_text(hjust = 0.5))



plot(anom$outlier_probability)
plot(anom$kde)
plot(anom$lookde)
plot(anom$outlier_scores)
plot(anom$bandwidth)
plot(anom$data)
