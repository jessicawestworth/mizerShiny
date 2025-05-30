
comparedietmatrix <- function(unharvestedprojection, harvestedprojection, timerange){

  #THE TIMERANGE SHOULD BE X:Y
  dietunharv <- getDiet(unharvestedprojection@params,
                        n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                        n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                        n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                        proportion = TRUE) %>%
    as.table()%>%
    as.data.frame()%>%
    group_by(predator, prey)%>%
    summarise(Proportion=mean(Freq))

  dietharv <- getDiet(unharvestedprojection@params,
                      n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                      n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                      n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                      proportion = TRUE) %>%
    as.table()%>%
    as.data.frame()%>%
    group_by(predator, prey)%>%
    summarise(Proportion=mean(Freq))

  joindiet <- left_join(dietharv, dietunharv, by = c("prey", "predator"))%>%
    mutate(Difference = ((Proportion.x - Proportion.y) / Proportion.y) * 100) %>%  # Calculate percentage change
    select(predator, prey, Difference)%>%
    filter(!predator %in% ("Resource"),
           !prey %in% ("Resource"))

  dietplot <- ggplot(joindiet, aes(x = predator, y = prey, fill = Difference)) +
    geom_tile() +
    scale_fill_gradient2() +
    labs(x = "Predator",
         y = "Prey",
         fill = "Difference") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))


  return(dietplot)

}
