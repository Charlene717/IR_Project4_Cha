# https://www.rdocumentation.org/packages/text2vec/versions/0.6/topics/TfIdf
# NOT RUN {
data("movie_review")
N = 100
tokens = word_tokenizer(tolower(movie_review$review[1:N]))
dtm = create_dtm(itoken(tokens), hash_vectorizer())
model_tfidf = TfIdf$new()
dtm_tfidf = model_tfidf$fit_transform(dtm)
# }