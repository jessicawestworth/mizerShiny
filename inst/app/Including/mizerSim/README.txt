For the sims included here, this is how they are generated.

unharvestedprojection <- project(celticsim, t_max = 200, effort = celticsim@initial_effort)
save(unharvestedprojection, file="unfishedprojection.RData")
save(unharvestedprojection, file="unharvestedprojection.RData")

They are saved as different despite being the same, as they are used in separate areas of the app - the single species and the fishery strategy sections. 
Both of these have different objectives, so the comparison simulation may need to be different in the future - the single species may not need to contain any fishing, for example.

However, loading your own mizerSim objects, make sure they are 202 years long. Users can select up to a 100 years on the sliders, this means that for one of the plots, 
it must plot 200 years. Across all of the plots, it plots the average of 2 years either side (so year 0,1,2, if choosing year 1). Therefore, when it is plotting for the 'species' 
plot, where it plots 2x into the future, it can plot (maxyear + 1)*2.