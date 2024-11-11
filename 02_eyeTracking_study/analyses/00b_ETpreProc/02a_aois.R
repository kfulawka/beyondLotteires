# AOIS ---------------------------------------------

# function to convert psychopy coordinate system to 0-1 system
psyConv <- function(x) x/2 + .5

# coordinates of horizontal layout
xc <- psyConv(c(-.65, -.3, .3, .65))
yc <- psyConv(c(-.3, .3))

# screen parameters
x_s <- 1920
y_s <- 1080

# AOIs parameters:
# letters height was approx 4% of the screen height, AOI width will be set to 6%
h <- (.17 * y_s)/2
# probs width was max 8% of the screen width
pw <- (.12 * x_s)/2
# outcomes width was max 17% of the screen width
ow <- (.22 * x_s)/2

# stimulus coordinates (in pixels)
cors <- list(horizontal = list(x = xc * x_s, y = yc * y_s)
             , vertical = list(x = yc * x_s, y = xc * y_s))


# empty array for aoi's borders
aois <- array(NA
              , dim = c(8, 4, 4, 2)
              , dimnames = list(c('xa', 'pa', 'ya', 'qa', 'xb', 'pb', 'yb', 'qb')
                                , c('x0', 'x1', 'y0', 'y1')
                                , 1:4, c('vertical', 'horizontal')))

# orders of stimuli
st_ord <- matrix(c(  'xa', 'pa', 'ya', 'qa', 'xb', 'pb', 'yb', 'qb'
                   , 'ya', 'qa', 'xa', 'pa', 'yb', 'qb', 'xb', 'pb'
                   , 'pa', 'xa', 'qa', 'ya', 'pb', 'xb', 'qb', 'yb'
                   , 'qa', 'ya', 'pa', 'xa', 'qb', 'yb', 'pb', 'xb')
                 , nrow = 4, byrow = T)

# vertical layouts
for (ll in 1:4) {
  
  st <- st_ord[ll, ]
  
  for (i in 1:8) {
    if(st[i] %in% c('pa', 'qa', 'pb', 'qb') ) {
      ad = pw 
    } else {
      ad = ow
    }
    if ( i < 5  ) {
      aois[st[i],,ll,'vertical'] <- c(cors[['vertical']][[1]][1] - ad, cors[['vertical']][[1]][1] + ad
                                      , cors[['vertical']][[2]][i] - h, cors[['vertical']][[2]][i] + h) 
    } else {
      aois[st[i],,ll,'vertical'] <- c(cors[['vertical']][[1]][2] - ad, cors[['vertical']][[1]][2] + ad
                                      , cors[['vertical']][[2]][i-4] - h, cors[['vertical']][[2]][i-4] + h) 
    }
  }
}

# horizontal layouts
for (ll in 1:4) {
  
  st <- st_ord[ll, ]
  
  for (i in 1:8) {
    if(st[i] %in%  c('pa', 'qa', 'pb', 'qb') ) {
      ad = pw 
    } else {
      ad = ow
    }
    if ( i < 5  ) {
      aois[st[i],c(3,4,1,2),ll,'horizontal'] <- c(cors[['horizontal']][['y']][1] - h, cors[['horizontal']][['y']][1] + h
                                                  , cors[['horizontal']][['x']][i] - ad, cors[['horizontal']][['x']][i] + ad) 
    } else {
      aois[st[i],c(3,4,1,2),ll,'horizontal'] <- c(cors[['horizontal']][['y']][2] - h, cors[['horizontal']][['y']][2] + h
                                                  , cors[['horizontal']][['x']][i-4] - ad, cors[['horizontal']][['x']][i-4] + ad) 
    }
  }
}
dimnames(aois)[[4]] <- c(1, 2)
rm(x_s, y_s, h, pw, ow, cors, st_ord, ll, i, ad, st, xc, yc, psyConv)

