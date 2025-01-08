# Generative Art: Black Hole Spiral
library(ggplot2)
library(dplyr)

# Set parameters
n_points <- 10000
max_radius <- 10
spiral_tightness <- 0.5
color_palette <- colorRampPalette(c("black", "darkblue", "purple", "red", "orange"))

# Generate spiral points
set.seed(123)
data <- tibble(
  angle = runif(n_points, 0, 2 * pi),
  radius = sqrt(runif(n_points, 0, max_radius^2)),
  x = radius * cos(angle + spiral_tightness * radius),
  y = radius * sin(angle + spiral_tightness * radius),
  color = radius
)

# Create plot
p <- ggplot(data, aes(x = x, y = y, color = color)) +
  geom_point(size = 0.1, alpha = 0.8) +
  scale_color_gradientn(colors = color_palette(100)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    legend.position = "none"
  )

# Save plot
ggsave("pics/black_hole.png", p, width = 8, height = 8, dpi = 300)
