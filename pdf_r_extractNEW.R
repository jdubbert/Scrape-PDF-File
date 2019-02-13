library(tesseract)
library(data.table)
library(pdftools)
library(openxlsx)
library(readxl)

file <- pdftools::pdf_convert('ITU Mobile Broadband 2013.pdf', pages = 98,dpi = 600)
text <- tesseract::ocr(file)
text.split<-as.data.table(strsplit(text,'\n'))[-c(1:4),]

split2<-rep(NA)
for (i in c(1:nrow(text.split))){
  word<-text.split[[1]][i]
  start.pos<-regexpr("[.]{1}[0-9][[:space:]][0-9]",word,perl=TRUE)
  split2[(2*i)-1]<-paste(strsplit(word,split='')[[1]][1:(start.pos+1)],collapse="")
  split2[2*i]<-paste(strsplit(word,split='')[[1]][(start.pos+2):length(strsplit(word,split='')[[1]])],collapse="")
}
split2<-as.data.table(split2)

start.remove<-rep(NA)
for (i in c(1:nrow(split2))){
  word<-split2[[1]][i]
  start.pos<-regexpr("[[:space:]]{1}[a-zA-Z]\\w+\\D+",word,perl=TRUE)
  start.remove[i]<-paste(strsplit(word,split='')[[1]][(start.pos+1):length(strsplit(word,split='')[[1]])],collapse="")
}
start.remove<-data.table(start.remove)

write.xlsx(start.remove,"Pdf Data Split.xlsx")

dt<-as.data.table(read_excel("Pdf Data Split.xlsx"))
country.name<-rep(NA)
value<-rep(NA)
for (i in c(1:nrow(dt))){
  word<-dt[[1]][i]
  start.pos<-regexpr("[[:space:]]{1}[0-9]",word,perl=TRUE)
  value[i]<-paste(strsplit(word,split='')[[1]][(start.pos+1):length(strsplit(word,split='')[[1]])],collapse="")
  country.name[i]<-paste(strsplit(word,split='')[[1]][1:(start.pos-1)],collapse="")
}
final.dt<-data.table(country.name,value)
final.dt$value <- as.numeric(final.dt$value)
final.dt <- final.dt %>% arrange(desc(value))

write.xlsx(final.dt,"Pdf Data Split.xlsx")



dt<-as.data.table(read_excel("Pdf Data Split.xlsx"))
WriteXLS::WriteXLS(dt,"Pdf Data Split.xlsx",)


