getSymbols.CNHK = function(Symbols.name, from='2007-01-01'){
# Download the daliy shock prices 
# This is a modifer of getSymbol functions from quantmod
# The change is because the HK/SS/SZ exchange markets' symbols start with numbers
# Hong Kong = 0005.HK; Shenzhen = xxxxx.SZ; Shanghai = zxzzz.SS
# quantmod often assigns the variable by the symbols and it is not very convienent. 

yahoo.URL <- "http://real-chart.finance.yahoo.com/table.csv?"
to = Sys.Date()
from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])-1
from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])-1
to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])

download.file(paste(yahoo.URL,
                    "s=",Symbols.name,
                    "&d=",to.m,
                    "&e=",sprintf('%.2d',to.d),
                    "&f=",to.y,
                    "&g=d",
                    "&a=",from.m,
                    "&b=",sprintf('%.2d',from.d),
                    "&c=",from.y,
                    "&ignore=.csv",
                    sep=''),destfile=tmp,quiet=TRUE,method = "auto")

fr <- read.csv(tmp)
fr <- xts(as.matrix(fr[,-1]),
          as.Date(fr[,1]),
          #as.POSIXct(fr[,1], tz=Sys.getenv("TZ")),
          src='yahoo',updated=Sys.time())

Symbols.name = paste("s",Symbols.name,sep="")
colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                      c('Open','High','Low','Close','Volume','Adjusted'),
                      sep='.')
return(fr)}
