yieldplottingfunctions <- function(harvested, unharvested, timerange1, timerange2, plottype) {
  if (plottype == "singlefleet") {
    # Multiple gears case
    harv <- as.data.frame(as.table(getYieldGear(harvested)[c(timerange1, timerange2), ,])) %>%
      group_by(gear) %>%
      summarise(value = mean(Freq)) %>%
      subset(value > 0)
    
    # Same logic for unharvested
    unharv <- as.data.frame(as.table(getYieldGear(unharvested)[c(timerange1, timerange2), ,])) %>%
      group_by(gear) %>%
      summarise(value = mean(Freq)) %>%
      subset(value > 0)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = harv, labels = ~gear, values = ~value,
                           name = "harv", domain = list(row = 0, column = 0),
                           title = "Changed Strategy Yield")
    fig <- fig %>% add_pie(data = unharv, labels = ~gear, values = ~value,
                           name = "unharv", domain = list(row = 0, column = 1),
                           title = "Current Strategy Yield")
    
    fig <- fig %>% layout(showlegend = T,
                          grid = list(rows = 1, columns = 2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return(fig)
    
  } else if (plottype == "fleet") {
    # Single gear case
    harv <- as.data.frame(as.table(getYield(harvested)[c(timerange1, timerange2),])) %>%
      summarise(value = mean(Freq))
    
    unharv <- as.data.frame(as.table(getYield(unharvested)[c(timerange1, timerange2),])) %>%
      summarise(value = mean(Freq))
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = harv, labels = ~1, values = ~value,
                           name = "harv", domain = list(row = 0, column = 0),
                           title = "Changed Strategy Yield")
    fig <- fig %>% add_pie(data = unharv, labels = ~1, values = ~value,
                           name = "unharv", domain = list(row = 0, column = 1),
                           title = "Current Strategy Yield")
    
    fig <- fig %>% layout(showlegend = T,
                          grid = list(rows = 1, columns = 2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return(fig)
    
  } else if (plottype == "species") {
    
    harv <- as.data.frame(as.table(getYield(harvested)[c(timerange1, timerange2),])) %>%
      group_by(sp) %>%
      summarise(value = mean(Freq)) %>%
      rename(gear = sp) %>%
      subset(value > 0)
    
    unharv <- as.data.frame(as.table(getYield(unharvested)[c(timerange1, timerange2),])) %>%
      group_by(sp) %>%
      summarise(value = mean(Freq)) %>%
      rename(gear = sp) %>%
      subset(value > 0)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = harv, labels = ~gear, values = ~value,
                           name = "harv", domain = list(row = 0, column = 0),
                           title = "Changed Strategy Yield")
    fig <- fig %>% add_pie(data = unharv, labels = ~gear, values = ~value,
                           name = "unharv", domain = list(row = 0, column = 1),
                           title = "Current Strategy Yield")
    
    fig <- fig %>% layout(showlegend = T,
                          grid = list(rows = 1, columns = 2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return(fig)
    
  } else {
    stop("Invalid plot type specified")
  }
}