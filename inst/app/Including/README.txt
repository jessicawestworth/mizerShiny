# Customizing mizerShiny

You can customize the default parameters and simulations used by mizerShiny by replacing the files in this directory.

## File Structure

- `mizerParam/params.rds` - The MizerParams object (fishery model parameters)
- `mizerSim/Unharvested/unharvestedprojection.rdata` - Single-species baseline simulation
- `mizerSim/Unfished/unfishedprojection.rdata` - Fishery strategy baseline simulation

## How to Replace Files

### Option 1: Direct File Replacement (Simplest)
Simply replace the files directly:

```r
# Replace parameters
saveRDS(my_params, "inst/app/Including/mizerParam/params.rds")

# Replace simulations
save(my_sp_sim, file = "inst/app/Including/mizerSim/Unharvested/unharvestedprojection.rdata")
save(my_fish_sim, file = "inst/app/Including/mizerSim/Unfished/unfishedprojection.rdata")

# Then run the app normally
shiny::runApp("inst/app")
```

### Option 2: Using the Launcher Function
Use the mizerShiny() function to temporarily or permanently replace files:

```r
# Temporary replacement (for this session only)
mizerShiny(params_file = "path/to/my/params.rds")

# Permanent replacement
mizerShiny(params_file = "path/to/my/params.rds", REPLACE = TRUE)
```

## Running the App

After replacing files, you can run the app in any of these ways:

1. **Directly**: `shiny::runApp("inst/app")`
2. **Using launcher**: `mizerShiny()`
3. **RStudio**: Click "Run App" button in the app.R file

## Notes

- Make sure your custom parameters have the same species list as the simulations
- The app will automatically regenerate simulations if there's a species mismatch
- Files are loaded from the package installation directory when using the installed version
