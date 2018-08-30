#' Compare a document to another document
#'
#' Compares a document to another document to find plagiarism.
#'
#' @param x File name/path of the PDF.
#' @param source File name/path of the source which should be
#' compared to the document x (source has to be in PDF format).
#' @param cos_sim Similarity parameter of the cosine distance. The output contains
#' sentences which have cosine similarity greater or equal 'cos_sim'. The default
#' is 0.6.
#'
#'
compare <- function(x, source, cos_sim = 0.6){

  # read documents
  documents <- c(x, source)
  text_documents <- sapply(documents, pdftools::pdf_text)

  # stem the documents
  documents_stem <- lapply(text_documents, stemDocument)

  # calculate the similarity for the whole documents


  # tokenize the documents
  documents_sen <- lapply(documents_stem, quanteda::tokens, what = "sentence",
                          remove_numbers = TRUE, remove_punc = TRUE,
                          remove_symbols = TRUE, remove_hyphens = TRUE,
                          remove_separators = TRUE)

  # lower the letters of the documents
  documents_sen_lower <- lapply(documents_sen, quanteda::tokens_tolower)

  # remove internal white space
  documents_sen_rem <- lapply(documents_sen_lower, function(x){
    gsub("\\s+", " ", x)
  })

  # assignment for output
  documents_sen_nums <- lapply(documents_sen, function(x){
    cumsum(lapply(x, length))
  })

  source_sen_nums1 <- sapply(documents_sen[[2]], length)
  source_sen_nums1 <- cumsum(c(documents_sen_nums[[1]][length(documents_sen_nums[[1]])],
                               source_sen_nums1))
  source_sen_nums2 <- source_sen_nums1[-1]

  # create corpus and document term matrix
  documents_corp <- tm::VCorpus(VectorSource(unlist(documents_sen_rem)))
  documents_dtm <- tm::DocumentTermMatrix(documents_corp,
                                          control = list(removePunctuation = TRUE,
                                                         stopwords=TRUE))

  documents_dtm <- as.matrix(documents_dtm)

  # prepare for comparison
  documents_sen <- as.vector(sapply(documents_sen_rem, length))

  # comparing with cosine similarity
  documents_sim <- list()
  for(i in 1:documents_sen[1]){
    documents_sim[[i]] <- apply(documents_dtm[-(1:documents_sen[1]), ], 1,
                                lsa::cosine, documents_dtm[i, ])
  }
  ## muss noch eleganter gelöst werden, noch viel zu langsam!!

  # select the sentences with a cosine similarity greater or equal 'cos_sim'
  sim_select <- lapply(documents_sim, function(x) {
    x[which(x >= cos_sim)]
  })

  # prepare the output

  # pages of the source
  sim_num <- lapply(sim_select, function(x){
    as.integer(names(x))
  })
  sim_num_un <- unlist(sim_num)

  pages_source <- findInterval(sim_num_un, c(1, source_sen_nums2))


  # pages of the document
  names(sim_num) <- 1:length(sim_num)
  sim_num_un_name <- unlist(sim_num)
  doc_sen <- as.integer(names(sim_num_un_name))

  pages_document <- findInterval(doc_sen, c(1, documents_sen_nums[[1]]))

  # sentence numbers of both documents
  sen_num_source <- as.integer(sim_num_un - source_sen_nums1[pages_source])
  documents_sen_nums2 <- c(0, as.vector(unlist(documents_sen_nums[[1]])))
  sen_num_document <- as.integer(doc_sen - documents_sen_nums2[pages_document])

  # prepare the output
  output <- tibble::tibble(cos_sim = unlist(sim_select), sen_x = documents_sen_rem[[1]][doc_sen],
                           page_x = pages_document, sen_num_x = sen_num_document,
                           page_source = pages_source, sen_num_source = sen_num_source)

  return(output)

}
