This folder contains everything about guilds and how I load it in. 

Overall, you are trying to get to a dataframe of column names 
Species - same as mizer
maxw - maximum weight for this feeding guild for this species
min - minimum weight for feeding guild for this species      
Feeding.guild - the feeding guild for this size of species and this weight. 

I have included, the data from Murray et al 2022 (guilds.txt), the same data in a cleaner format (guilds_cleaned.txt) and fishinfo, which relates common species names to their scientific.

Within the functions/guildplot.R code, there is the code I used to get from  guilds_cleaned.txt, using fishinfo, to get the guildparams_preprocessed so that you can load your own.
