#' Search for similar sentences
#'
#'
#'
#' @param x File path of the PDF.
#' @param sen Sentence to be used to search in the text.
#' @param exact If you search for the exact sentence, the default is FALSE and the
#' cosine distance is used as similarity measurement.
#' @param cos_sim Similarity parameter of the cosine distance. The output contains
#' sentences which have cosine similarity greater or equal 'cos_sim'. The default
#' is 0.4.
#'

search_for <- function(x, sen, exact = FALSE, cos_sim = 0.4){

  # read in text with pdf_text() from the pdftools package
  text <- pdftools::pdf_text(x)

  # tokenize the text with tokenize() from the quanteda package
  text_sen <- quanteda::tokens(text, what = "sentence", remove_numbers = TRUE,
                               remove_punc = TRUE, remove_symbols = TRUE,
                               remove_hyphens = TRUE, remove_separators = TRUE)

  # assignment for ouput
  sen_nums <- cumsum(lapply(text_sen, length))

  # remove internal white space
  text_sen <- gsub("\\s+"," ",tok2)

  # if we are searching for the exact similar sentence
  if (exact == TRUE) {
    sen_loc <- lapply(sen, grep, text_sen)

    # prepare the output
    sen_loc_un <- unlist(sen_loc)
    pages <- findInterval(sen_loc_un, c(1, sen_nums))
    sen_num <- as.integer(sen_loc_un - sen_nums[pages - 1])
    output <- tibble::tibble(match = rep("perfect match", sapply(sen_loc, length)),
                             page = pages, sen_num = sen_num)

    return(output)

  } else {
    # prepare the text and the sentence for comparing:
    # add sentence to text vector
    text_sen[length(text_sen) + 1] <- sen
    # create corpus
    text_corp <- tm::VCorpus(VectorSource(text_sen))
    # create document term matrix
    text_dtm <- tm::DocumentTermMatrix(text_corp,
                                       control = list(removePunctuation = TRUE,
                                                      stopwords=TRUE))
    text_dtm <- as.matrix(text_dtm)

    # comparing with cosine similarity
    text_sim <- apply(text_dtm[-nrow(text_dtm), ], 1,
                      lsa::cosine, text_dtm[nrow(text_dtm), ])

    # select the sentences with a cosine similarity greater or equal 'cos_sim'
    sim_select <- text_sim[which(text_sim >= cos_sim)]

    # prepare the output
    sim_num <- as.integer(names(sim_select))
    pages <- findInterval(sim_num, c(1, sen_nums))
    sen_num <- as.integer(sim_num - sen_nums[pages - 1])
    output <- tibble::tibble(cos_sim = sim_select, page = pages,
                             sen_num = sen_num)
    return(output)

  }
}

