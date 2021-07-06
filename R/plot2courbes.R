plot2courbes<- function(r6_1, r6_2, timeStep) {
data_plot <-  r6_1$aggregated_data_by_some_time_unit %>% 
  mutate.(nom = paste0(nom, "_periode1")) %>% 
  bind_rows.(r6_2$aggregated_data_by_some_time_unit %>% 
               mutate.(nom = paste0(nom, "_periode2"))) %>% 
  mutate.(tooltip = as.character(
    glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf('%.2f', taux_occupation)}")
  )) %>% 
  mutate.(linetype = ifelse(ident == "moyenne", "dotted", "solid")) 

if(timeStep == "Jour") {
  data_plot <- data_plot %>% 
    mutate.(time = factor(lubridate::hour(time)))
} else if(timeStep == "Semaine") {
  data_plot <- data_plot %>% 
    mutate.(time = factor(lubridate::wday(time, label = TRUE)))
} else if(timeStep == "Mois") {
  data_plot <- data_plot %>% 
    mutate.(time = factor(lubridate::day(time)))
} else {
  data_plot <- data_plot %>% 
    mutate.(time = factor(lubridate::month(time, label = TRUE, abbr = FALSE)))
}

parkings_to_plot <- c("CUBPK49", "CUBPK76")

gg <- filter.(data_plot, ident %in% parkings_to_plot & ident != "moyenne") %>%  
  ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group=nom, linetype = nom)) + 
  geom_line_interactive(aes(data_id=ident), lwd = 1) + 
  geom_point_interactive(aes(tooltip=tooltip, data_id=ident))  +
  theme_minimal() +
  # theme(legend.position = "bottom") +
  
  geom_line_interactive(data = data_plot %>% filter.(ident == "moyenne"), 
                        mapping = aes(x = time, y = taux_occupation, tooltip=taux_occupation, data_id = ident, group = nom, color = nom),
                        lwd = 1.5) +
  scale_linetype_manual(
    "nom",
    values =
      unlist(
        with(
          distinct.(data_plot %>% 
                      filter.(ident %in% c("moyenne", parkings_to_plot)) %>%
                      select.(nom, linetype)),
          split(linetype, nom)))
  ) #+
  # A modifier quand on aura la palette bx metro
  # scale_color_manual(values = sample(colors(distinct = TRUE), length(parkings_to_plot)+1))


gg
}
