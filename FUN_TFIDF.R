## bind_tf_idf {tidytext}
function (tbl, term, document, n) 
{
  term <- quo_name(enquo(term))
  document <- quo_name(enquo(document))
  n_col <- quo_name(enquo(n))
  terms <- as.character(tbl[[term]])
  documents <- as.character(tbl[[document]])
  n <- tbl[[n_col]]
  doc_totals <- tapply(n, documents, sum)
  idf <- log(length(doc_totals)/table(terms))
  tbl$tf <- n/as.numeric(doc_totals[documents])
  tbl$idf <- as.numeric(idf[terms])
  tbl$tf_idf <- tbl$tf * tbl$idf
  if (any(tbl$idf < 0, na.rm = TRUE)) {
    rlang::warn(paste("A value for tf_idf is negative:\n", 
                      "Input should have exactly one row per document-term combination."))
  }
  tbl
}
