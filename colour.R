rgb.sample = function(v1){
  ecdf.v1 = ecdf(v1)
  v1.quantile = ecdf.v1(v1)
  red.sample = v1.quantile
  green.sample = v1.quantile
  blue.sample = v1.quantile
  
  red.sample[v1.quantile<1/6] = red.sample[v1.quantile<1/6]*6
  red.sample[v1.quantile>=1/6 & v1.quantile<2/6] = 1
  red.sample[v1.quantile>=2/6 & v1.quantile<1/2] = red.sample[v1.quantile>=2/6 & v1.quantile<1/2]*-6+3
  red.sample[v1.quantile>=1/2] = 0
  
  green.sample[v1.quantile<1/6] = 0
  green.sample[v1.quantile>=1/6 &v1.quantile<2/6] = green.sample[v1.quantile>=1/6 &v1.quantile<2/6]*6-1
  green.sample[v1.quantile>=2/6&v1.quantile<4/6] =1
  green.sample[v1.quantile>=4/6&v1.quantile<5/6] = green.sample[v1.quantile>=4/6&v1.quantile<5/6]*-6+5
  green.sample[v1.quantile>=5/6] = 0
  
  blue.sample[v1.quantile<1/2] = 0 
  blue.sample[v1.quantile>=3/6&v1.quantile<=4/6] = blue.sample[v1.quantile>=3/6&v1.quantile<=4/6]*6-3
  blue.sample[v1.quantile>=4/6&v1.quantile<=5/6] = 1
  blue.sample[v1.quantile>=5/6]=blue.sample[v1.quantile>=5/6]*-6+6
  
  colours = rgb(red = red.sample, green = green.sample, blue = blue.sample, alpha = 0.3)
  return(colours)
}
require(MASS)

MyParcoord = function(dat){
  # require colour.sample
  colours = colour.sample(dat[,1])
  parcoord(dat,
           col = rgb(red = colours$red.sample, 
                     blue = colours$blue.sample, 
                     green = colours$green.sample, 
                     alpha = 0.3), 
           lty = 1)}

