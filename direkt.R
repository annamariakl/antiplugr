#------------------------------------------------------
#Plan


# Tokenize
# vergleichen
# funktion die nach matches sucht Reihenfolge aber nicht Position vergleicht
# hilight

#Packages------------------------------
install.packages("quanteda")
install.packages("tm")
stitch("direkt.R")
#-------------------------------------------------------
# Test Texte
zit1 <-  "Nach Köhlmoos belegt 1Kön 12,25-30* eine Geschichtstheologie, 
die das „Trauma von Samaria und die Katastrophe Jerusalems“ interpretiert und 
dabei der Reichsteilung eine wichtige Rolle zuweist. In diesem Ereignis liegt der 
Grund für die Untergänge der beiden Staaten, die sich als „Bruderstaaten“ eigentlich 
nicht hätten trennen sollen."

orig1 <- "Aus der Textanalyse geht hervor, dass 1Kön 12,25-20* Teil eines
geschichtstheologischen Systems sind, das dazu dient, das Trauma von Samaria und
die Katastrophe Jerusalems zu verstehen und zu deuten. Die Reichsteilung spielt dabei 
als Grund der Unheilsgeschichte Israels eine wichtige Rolle. Es ist die Trennung der 
beiden Bruderstaaten voneinander, der sie beide in die Katastrophe führt."


# Köhlmoos, Melanie: Korrektes Zitieren, zuletzt geprüft am 13.08.2018.

#tokens-------------------------


# ------------mit tm package den corpus erstellen
#myCorpus_orig <- tm::Corpus(tm::VectorSource(vec_orig1)) 
#myCorpus_orig
# sieht jedes wort als dokument: nicht richtig



 directq <- function(zit1,orig1){
myCorpus_orig <- tm::Corpus(tm::VectorSource(c(zit1,orig1))) # Corpus erstellt

#stamming---------------------
myCorpus_orig_plain <- tm::tm_map(myCorpus_orig,tm::PlainTextDocument)

# warnung kann ignoriert werden eventuell ein Dataframe mit 
#df <- data.frame(doc_id = doc_ids, text = text, stringsAsFactors = FALSE)
#df_corpus <- Corpus(DataframeSource(df))
#inspect(df_corpus)
# vielleicht mit plain
myCorpus_orig_st2 <- tm::tm_map(myCorpus_orig_plain,tm::stemDocument)
# funktioniert


#--------Umwandlung in quanteda
 my_corpus_cuanteda <- quanteda::corpus(myCorpus_orig_st2)
# tokenize------
quan_stam_tok<- quanteda::tokens(my_corpus_cuanteda,remove_punct=T)
# funktioniert
# Problem ein dokument heißt meta ein anders heißt meta

# Vergleichen--------------
# beides sind Charactervectoren
#quan_stam_tok$content[1]# Auf einzelnen token zugreifen
# Funktion finden die Abfolgen in charaktervektoren vergleicht
 }

# funktion, die position der Übereinstimmung findet
# was macht die which funktion, wenn keins übereinstimmt
t
# muss mit any getestet werden

find.pos <- function (p,o){
  i<- 1
  pos_o <- list()
  while(i<= length(p)){
    if(any(p[i]== o)){
      pos_o[[i]] <- which(o==p[i])
    } else {
      pos_o[[i]] <-"keine Übereinstimmung mit dem Original"
    }
    i <- i+1
      
  }
  return(pos_o)
}

# test#+o <- c(4,4,4,3)
#p <- c(5,4,4,4)
#find.pos(p,o)
