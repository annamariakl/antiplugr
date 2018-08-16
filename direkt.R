#------------------------------------------------------
#Plan


# Tokenize
# vergleichen
# funktion die nach matches sucht Reihenfolge aber nicht Position vergleicht
# hilight

#Packages------------------------------
install.packages("quanteda")
install.packages("tm")

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
tok_zit1 <- quanteda::tokens(zit1, remove_numbers = TRUE, 
                         remove_punc = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, 
                         remove_separators = TRUE)

tok_orig1 <- quanteda::tokens(orig1, remove_numbers = TRUE, 
                         remove_punc = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, 
remove_separators = TRUE)
# kann nicht als Vector ausgegeben werden
rm(ok_orig1)

# Anzeigen 
tok_zit1
vec_zit1 <- as.vector(tok_zit1)
vec_orig1 <- as.vector(tok_orig1)

# ------------mit tm package den corpus erstellen
#myCorpus_orig <- tm::Corpus(tm::VectorSource(vec_orig1)) 
#myCorpus_orig
# sieht jedes wort als dokument: nicht richtig

myCorpus_orig <- tm::Corpus(tm::VectorSource(c(zit1,orig1))) # Corpus erstellt
myCorpus_orig
# funktioniert

#stamming---------------------
myCorpus_orig_st <- tm::tm_map(x= myCorpus_orig,FUN=tm::stemDocument)
#print(myCorpus_orig[1]$content)
myCorpus_orig_plain <- tm::tm_map(myCorpus_orig,tm::PlainTextDocument)
# warnung kann ignoriert werden eventuell ein Dataframe mit 
#df <- data.frame(doc_id = doc_ids, text = text, stringsAsFactors = FALSE)
#df_corpus <- Corpus(DataframeSource(df))
#inspect(df_corpus)
print(myCorpus_orig_st[1]$content)
# vielleicht mit plain
myCorpus_orig_st2 <- tm::tm_map(myCorpus_orig_plain,tm::stemDocument)
print(myCorpus_orig_st2[1]$content)
# funktioniert
str(myCorpus_orig_st2)
myCorpus_orig_st2

#--------Umwandlung in quanteda
 my_corpus_cuanteda <- quanteda::corpus(myCorpus_orig_st2)
# tokenize------
my_corpus_cuanteda
quan_stam_tok<- quanteda::tokens(my_corpus_cuanteda,remove_punct=T)
quan_stam_tok
# funktioniert
# Problem ein dokument heißt meta ein anders heißt meta

# Vergleichen--------------
str(quan_stam_tok$content)
str(quan_stam_tok$meta)
# beides sind Charactervectoren
quan_stam_tok$content[1]# Auf einzelnen token zugreifen
# Funktion finden die Abfolgen in charaktervektoren vergleicht

# erster Versuch match
?match
# test
4:6 %in% 1:10
# gibt keine position

# funktion, die position der Übereinstimmung findet

help(which)
3+2
  

