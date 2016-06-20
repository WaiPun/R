read.allsheets = function(excel.path, combine = F){

require(readxl)
no.sheets = length(excel_sheets(excel.path))
all.sheets = list()
    
# read all sheets
for (i in 1:no.sheets){
      Sheeti = read_excel(excel.path,i)
      all.sheets[[i]] = Sheeti}

# if "combine" wanted, rbind all
if(combine == T){
# initialise the first list
      combined = all.sheets[[1]]
      for(i in 2:no.sheets){
        combined = rbind(combined, all.sheets[[i]])}
      return(combined)
}else{return(all.sheets)}

}