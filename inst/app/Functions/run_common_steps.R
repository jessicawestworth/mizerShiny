run_common_steps <- function(speciessim, time1, time2, progress, harvestedprojection=NULL,unharvestedprojection) {
  total_steps <- 7
  
  if (is.null(harvestedprojection)){
  progress$inc(amount = 1 / total_steps, message = "Running harvested projection...")
  harvestedprojection <- project(speciessim,
                                 effort = celticsim@initial_effort,
                                 t_max = time2 * 2)
  }
  # Step 4: Calculate size spectrum
  progress$inc(amount = 1 / total_steps, message = "Calculating size spectrum...")
  sizelevel <- plotSpectraRelative(harvestedprojection, unharvestedprojection, time1, time2)
  
  # Step 5: Calculate species level change
  progress$inc(amount = 1 / total_steps, message = "Calculating species level changes...")
  specieslevel <- plotSpeciesWithTimeRange(harvestedprojection, unharvestedprojection, time1, time2)
  
  # Step 6: Calculate guild level change
  progress$inc(amount = 1 / total_steps, message = "Calculating guild level changes...")
  guildlevel <- guildplot(harvestedprojection, unharvestedprojection, time1, time2, guildparams, celticsim)
  
  # Step 7: Compare diet matrices
  progress$inc(amount = 1 / total_steps, message = "Comparing diet matrices...")
  dietplot <- comparedietmatrix(harvestedprojection, unharvestedprojection, time1:time2)
  
  # Final step message
  progress$inc(amount = 1 / total_steps, message = "Finalising plotting functions...")
  
  # Return a list of the outputs.
  list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel, dietplot = dietplot)
}
