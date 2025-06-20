create_species_level_plot <- function(data, plot_title) {
  
  data$Class <- if_else(data$normalized_value<0, "Negative", "Positive")
  
  ggplot(data, aes(x = Species, y = normalized_value, fill = Class)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5)+
    labs(title = plot_title, x = "Species", y = "% Change") +
    scale_fill_manual(values = c(
      "Negative" = "#E76F51", 
      "Positive" = "#2FA4E7"
    )) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          legend.position = "none",
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))
  
}