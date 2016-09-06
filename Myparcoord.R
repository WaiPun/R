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