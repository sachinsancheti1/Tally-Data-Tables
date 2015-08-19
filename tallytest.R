library(XML)
library(dplyr)
setwd("C:/Users/ariha/Desktop")
xmlfile = xmlParse("DayBook.xml")
xmltop = xmlRoot(xmlfile)

dt = xmlSize(xmltop[[2]][[1]][[2]])


camt=data.frame(numeric(),character())
for(i in 1:dt){
  # rt = xmlToDataFrame(xmltop[[2]][[1]][[2]][[i]])
  #   tt=rbind_all(list(tt,rt))
  camt = rbind(camt,data.frame(i,sapply(xmlChildren(xmltop[[2]][[1]][[2]][[i]]), xmlName)))
#   
}
View(camt)

tt = data.frame(na = "")
for(i in 1:dt){
  if(sapply(xmlChildren(xmltop[[2]][[1]][[2]][[i]]), xmlName)=="VOUCHER"){
    rt = xmlToDataFrame(xmltop[[2]][[1]][[2]][[i]])
    names(rt)=make.names(names(rt))
    tt=rbind_all(list(tt,rt))
  }
}
  View(tt)
  
shiny::runGitHub("Tally-Data-Tables","sachinsancheti1")
  