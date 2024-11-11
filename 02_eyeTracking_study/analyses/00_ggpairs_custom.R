
gg_pairs_custom <- function(xx,
                            columns = 1:ncol(xx),
                            map = NULL) {
  
  library(data.table)
  
  # CUSTOM PANELS -----------------------------------------------------------
  
  # scatterplot with loess model
  ggally_myscplot <- function(data, mapping, ...) {
    
    ggplot(data = data, mapping = mapping) +
      geom_point(alpha = .2) +
      geom_smooth(method = 'loess',
                  fill = 'pink') 
    
  }
  
  # correlation coefs and other data
  mycor <- function(data, mapping, sgnf = 3, ...) {
    
    xCol <- as.character(mapping[[1]][[2]])
    yCol <- as.character(mapping[[2]][[2]])
    
    d <- data.frame(x = data[[xCol]],
                    y = data[[yCol]])
    d <- d[complete.cases(d), ]
    
    rho <- round( cor.test(d$x, d$y, method = 's')$estimate, 2)
    r <- round( cor.test(d$x, d$y, method = 'p')$estimate, 2)
    # n <- nrow(d)
    
    pa <- paste0(c('rho = ', 'r = '), c(rho, r))
    
    loc <- data.table(x = c(.5, .5), 
                      y = c(.3, .6),
                      pa = pa)
    
    p <-  ggplot(data = loc, aes(x = x, y = y, label = pa)) + 
      xlim(0:1) + 
      ylim(0:1) + 
      theme(panel.background = element_rect(fill = "grey95"),  
            panel.grid = element_blank()) + 
      labs(x = NULL, y = NULL) +
      geom_text(color = "#8c8cc2")
    p
  }
  
  
  # THE PLOT ----------------------------------------------------------------
  if(is.null(map)) { # withouth additional mapping
    
    ggp_m <- ggpairs(data = xx,
                     columns = columns,
                     diag = list(discrete="barDiag", 
                                 continuous = wrap("densityDiag", alpha=0.5 )),
                     lower = list(continuous = ggally_myscplot,
                                  combo = "box_no_facet"),
                     upper = list(continuous = mycor))
    
  } else {
    ggp_m <- ggpairs(data = xx,
                     mapping = map,
                     columns = columns,
                     diag = list(discrete="barDiag", 
                                 continuous = wrap("densityDiag", alpha=0.5 )),
                     lower = list(continuous = ggally_myscplot,
                                  combo = "box_no_facet"),
                     upper = list(continuous = mycor))
  }
  
}
