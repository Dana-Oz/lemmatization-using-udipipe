library(tm)
library(NLP)
quran<-read.csv("quran_arabic.csv", stringsAsFactors = F, encoding = "UTF-8") #can be used in udpipe with or without locale arabic
#quran data cleaning
quran_clean<-function(text){
  text = removeNumbers(text)
  char = unlist(strsplit(text,""))
  char =  char[-grep("\\|",char)]
  return(paste(char, collapse= ""))
}
quran[,1]<-sapply(quran[,1],quran_clean,USE.NAMES = FALSE)
write.csv(quran, "clean_quran.txt", row.names = F, fileEncoding = "UTF-8") ##ENCODING PROBLEM
#Udipipe Lemmatization
#install.packages("udpipe")
library(udpipe)
udmodel <- udpipe_download_model(language = "arabic")
udmodel <- udpipe_load_model(file = udmodel$file_model)
get_lemma <- function(text)
{
  clean_text <- udpipe_annotate(udmodel, x = text)
  clean_text <- as.data.frame(clean_text, detailed = TRUE)
  if( any(is.na(clean_text$lemma)) == T)
  {
    return(paste(clean_text$lemma[-which(is.na(clean_text$lemma))], collapse = " "))
  }
  return(paste(clean_text$lemma, collapse = " "))
}
quran$lemma<-sapply(quran[,1], get_lemma)
orig_local<-Sys.getlocale(category = "LC_CTYPE") #"English_Israel.1252"
Sys.setlocale("LC_CTYPE", "arabic")
write.csv(quran$lemma, "quran_lemma.txt", row.names = F, fileEncoding = "UTF-8")
Sys.setlocale("LC_CTYPE", orig_local )