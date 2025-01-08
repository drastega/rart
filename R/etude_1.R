# Generative Art: Kandinsky-inspired Abstract Composition
library(ggplot2)
library(dplyr)

# Set parameters for Kandinsky-inspired art
set.seed(Sys.time())  # Unique seed for each run
n_layers <- sample(3:6, 1)  # Number of compositional layers
max_radius <- runif(1, 10, 20)
color_count <- sample(5:10, 1)  # More vibrant colors
shape_types <- c("circle", "triangle", "rectangle", "curve", "arc")
base_size <- runif(1, 1, 3)  # Base size for shapes

# Generate Kandinsky-inspired color palette
kandinsky_colors <- c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9", "#92A8D1",
                     "#955251", "#B565A7", "#009B77", "#DD4124", "#D65076")
color_palette <- colorRampPalette(sample(kandinsky_colors, color_count))

# Generate layered composition
data <- tibble()
for (layer in 1:n_layers) {
  n_shapes <- sample(5:15, 1)
  layer_data <- tibble(
    layer = layer,
    shape_id = 1:n_shapes,
    shape_type = sample(shape_types, n_shapes, replace = TRUE),
    x = runif(n_shapes, -max_radius * (layer/n_layers), max_radius * (layer/n_layers)),
    y = runif(n_shapes, -max_radius * (layer/n_layers), max_radius * (layer/n_layers)),
    size = runif(n_shapes, base_size * 0.8^(layer-1), base_size * 1.2^(layer-1)),
    angle = runif(n_shapes, 0, 2 * pi),
    color_value = runif(n_shapes),
    stroke_size = runif(n_shapes, 0.5, 2)
  )
  
  # Add layer-specific transformations
  if (layer > 1) {
    layer_data <- layer_data %>%
      mutate(
        x = x + rnorm(n(), 0, max_radius * 0.1),
        y = y + rnorm(n(), 0, max_radius * 0.1)
      )
  }
  
  data <- bind_rows(data, layer_data)
}

# Generate shape-specific geometries
shape_data <- data %>%
  group_by(shape_id, layer) %>%
  mutate(
    n_points = case_when(
      shape_type == "circle" ~ 100,
      shape_type == "triangle" ~ 3,
      shape_type == "rectangle" ~ 4,
      shape_type == "curve" ~ 50,
      shape_type == "arc" ~ 30,
      TRUE ~ 10
    )
  ) %>%
  uncount(n_points) %>%
  group_by(shape_id, layer) %>%
  mutate(
    point_id = row_number()
  ) %>%
  ungroup() %>%
  group_by(shape_id, layer, shape_type) %>%
  mutate(
    angle = case_when(
      shape_type == "circle" ~ seq(0, 2 * pi, length.out = n()),
      shape_type == "triangle" ~ seq(0, 2 * pi, length.out = 3),
      shape_type == "rectangle" ~ seq(0, 2 * pi, length.out = 5)[1:4],
      shape_type == "curve" ~ seq(-pi/2, pi/2, length.out = n()),
      shape_type == "arc" ~ seq(-pi/4, pi/4, length.out = n()),
      TRUE ~ seq(0, 2 * pi, length.out = n())
    )
  ) %>%
  mutate(
    x = x + size * cos(angle),
    y = y + size * sin(angle)
  )

# Create Kandinsky-inspired plot
p <- ggplot(shape_data, aes(x = x, y = y, group = interaction(shape_id, layer))) +
  geom_polygon(aes(fill = color_value), alpha = 0.8) +
  geom_path(aes(color = color_value), size = shape_data$stroke_size, lineend = "round") +
  scale_fill_gradientn(colors = color_palette(100)) +
  scale_color_gradientn(colors = color_palette(100)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.position = "none"
  ) +
  coord_fixed() +
  scale_size_identity() +
  facet_wrap(~layer, ncol = 1, scales = "free") +
  theme(strip.text = element_blank())

# Add musical rhythm elements
p <- p + geom_segment(
  data = shape_data %>% filter(shape_type %in% c("curve", "arc")),
  aes(xend = x + size * 0.2 * cos(angle), yend = y + size * 0.2 * sin(angle)),
  size = 1.5,
  lineend = "round"
)

p

# Save plot
ggsave("pics/kandinsky_art.png", p, width = 10, height = 10, dpi = 300)
