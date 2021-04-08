library(tidyverse)
library(rphylopic)

df <- data.frame(x = runif(1e4), y = runif(1e4))
df$clust <- kmeans(df, 1e3)$cluster
df <- df %>% group_by(clust) %>% mutate(min_x = min(x), min_y = min(y),
                                  max_x = max(x), max_y = max(y), n = n(),
                                  xc = mean(x), yc = mean(y))

# img <- image_data("e1997fdd-5f01-42f4-8415-7379854a7be6", size = "1024")[[1]]
# img <- image_data("1dda6a3d-e407-47fb-add8-fb6b0572f688", size = "1024")[[1]]
cat <- image_data("23cd6aa4-9587-4a2e-8e26-de42885004c9", size = 1024)[[1]]

ggplot(df, aes(x = x, y = y, color = as.factor(clust))) +
  geom_rect(aes(xmin = min_x, xmax = max_x, ymin = min_y, ymax = max_y, fill = clust)) +
  geom_point(aes(x = xc, y = yc), color = "grey50", size = 0.2) +
  scale_colour_viridis_d() + scale_fill_viridis_c() +
  add_phylopic(cat, color = "green", alpha = 0.5) +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("./pics/cat.png", width = 25, height = 18, units = "cm")
