If you want to load your own nutrition, or have your own data, it should just work to load into the app and should just plot ("should"). You will need to match the format I have 
here. Otherwise, the nutrition file here contains many species in the North East Atlantic, which you are able to use. The reference for this will be at the end, it has been 
culminated across many sources, provided by Pedro Warner at Cefas.

Match the file as such 
- plain CSV (comma separated), saved as UTF-8 or ISO88591
- column 1 is common_name, contains the common name for the species (each row a species)
- other columns contain nutrients - they are selected if they contain parentheses - as this signals its a nutrient by specifying units (g/100g)
- these nutrient columns must only contain numeric values

This will load the file in, but then there is the problem that the mizer names are usually different to names included in the nutrition files - so another file is required, 
which links the mizer species names to the common_names, found in the nutrition file. In the nutrtition Function in the Functions folder, there is a demonstration of how to get
from these files to the finished datasets.