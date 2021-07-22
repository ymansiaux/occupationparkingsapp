
gg <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                      colour = Species, group = Species)) +
  geom_line_interactive(aes(tooltip = Species, data_id = Species)) +
  scale_color_viridis_d() + 
  labs(title = "move mouse over lines")

x <- girafe(ggobj = gg, width_svg = 8, height_svg = 6,
            options = list(
              opts_hover_inv(css = "opacity:0.1;"),
              opts_hover(css = "stroke-width:2;")
            ))
x

b <- filter.(a, ident != "moyenne")
b <- b %>% mutate.(tooltip = as.character(glue::glue("Date : {.[,time]} \n
                                   Ident : {.[,nom]} \n
                                   Val : {.[,taux_occupation]}")))
glimpse(b)
library(ggiraph)
library(ggplot2)
gg <- ggplot(data = b, mapping = aes(x = time, y = taux_occupation, color = nom, group=nom)) + 
  geom_line_interactive(aes(tooltip=tooltip, data_id=ident)) + 
  # scale_color_manual(values = col_vector) +
  scale_fill_viridis_d() +
  # geom_line_interactive(data = a %>% filter.(ident == "moyenne"), mapping = 
                          # aes(x = time, y = taux_occupation, tooltip=taux_occupation, data_id=nom),  color = "black", lwd = 1.5) +
  theme_minimal() +
  theme(legend.position = "bottom")

x <- girafe(ggobj = gg, width_svg = 8, height_svg = 6,
            options = list(
              opts_hover_inv(css = "opacity:0.1;"),
              opts_hover(css = "stroke-width:2;")
            ))
x


