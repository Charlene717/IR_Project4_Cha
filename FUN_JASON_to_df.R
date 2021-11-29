library(jsonlite)

JASON_to_df = function(input.datapath){
try({  
##### JASON to df #####
  # JASON.df <- data.frame(matrix(nrow = 0,ncol = 5))
  # colnames(JASON.df) <- c("CHAR.","WORD","SENT","Search Word","NO.")
  
  jason.all <- list()
  for (k in 1:length(input.datapath)) {
    jason1 <- fromJSON(input.datapath[k])
    jason.all <- rbind(jason.all,jason1)
    rm(jason1)
  }
  jason.all$tweet_text <- gsub('^"|"$','',jason.all$tweet_text) 
 # jason.all$tweet_text <- gsub( "\\..*http.?$",".",jason.all$tweet_text)
  
  jason.all["CHAR"]=0
  jason.all["WORD"]=0
  jason.all["SENT"]=0
  jason.all["Search Word"]=0
  jason.all["NO."]=0
  Text.All <- ""
  for (i in 1:length(jason.all[,1])) {
    try({
    jason.all[i,15] <- sum(nchar(jason.all[i,colnames(jason.all)=="tweet_text"], type = "chars", allowNA = T, keepNA = NA))
    
    Twtext.1P <- jason.all[i,colnames(jason.all)=="tweet_text"]
    Twtext.1P_df <- tibble(line = 1:length(Twtext.1P), text = Twtext.1P)
    Twtext.1P_df %>%
      unnest_tokens(word, text) -> Twtext.1P_df.Word
    
    jason.all[i,16] <- sapply(str_split(Twtext.1P, " "), length)
    jason.all[i,17] <- nsentence(Twtext.1P) # https://rdrr.io/cran/quanteda/man/nsentence.html
    jason.all[i,18] <- "NA" # jason.all[i,18] <- length(as.data.frame(Keyword.df)[,2])
    jason.all[i,19] <- i
    Text.All <- paste0(Text.All," ", Twtext.1P)
    })
  }
  JASON.df <- jason.all
})    
  Text.All_df <- tibble(line = 1:length(Text.All), text = Text.All)
  
  Text.All_df %>%
    unnest_tokens(word, text) %>% as.data.frame() -> Twtext.All_df.Word
  
  ##### Stemming (Porter's algorithm)#####
  
  
  ## Original
  Twtext.All_df.Word.C <- Twtext.All_df.Word  %>% count(word, sort = TRUE)
  Twtext.All_df.Word.C <- Twtext.All_df.Word.C[order(Twtext.All_df.Word.C$n, decreasing = TRUE),]
  Twtext.All_df.Word.C$word <-  factor(Twtext.All_df.Word.C$word, levels = Twtext.All_df.Word.C$word)
  
  
  ## Stemming
  Twtext.All_df.Word.Stem <- Twtext.All_df.Word %>% mutate(stem = wordStem(word))
  Twtext.All_df.Word.Stem.C <- Twtext.All_df.Word.Stem  %>% count(stem, sort = TRUE)
  Twtext.All_df.Word.Stem.C <- Twtext.All_df.Word.Stem.C[order(Twtext.All_df.Word.Stem.C$n, decreasing = TRUE),]
  Twtext.All_df.Word.Stem.C$stem <-  factor(Twtext.All_df.Word.Stem.C$stem, levels = Twtext.All_df.Word.Stem.C$stem)
  
  
  ## Remove the stop word
  Twtext.All_df.Word.Stem.RmSW <- Twtext.All_df.Word.Stem %>% anti_join(get_stopwords())
  Twtext.All_df.Word.Stem.RmSW.C <- Twtext.All_df.Word.Stem.RmSW  %>% count(stem, sort = TRUE)
  Twtext.All_df.Word.Stem.RmSW.C <- Twtext.All_df.Word.Stem.RmSW.C[order(Twtext.All_df.Word.Stem.RmSW.C$n, decreasing = TRUE),]
  Twtext.All_df.Word.Stem.RmSW.C$stem <-  factor(Twtext.All_df.Word.Stem.RmSW.C$stem, levels = Twtext.All_df.Word.Stem.RmSW.C$stem)
  
  
  



  # Put all result to output list  
  Output <- list()
  Output <- list(JASON.df,
                 Twtext.All_df.Word, Twtext.All_df.Word.C,
                 Twtext.All_df.Word.Stem, Twtext.All_df.Word.Stem.C,
                 Twtext.All_df.Word.Stem.RmSW,Twtext.All_df.Word.Stem.RmSW.C)
  
  names(Output) <- c("JASON.df",
                     "Abs.All_df.Word","Abs.All_df.Word.C",
                     "Abs.All_df.Word.Stem","Abs.All_df.Word.Stem.C",
                     "Abs.All_df.Word.Stem.RmSW","Abs.All_df.Word.Stem.RmSW.C")
  return(Output)
  
}
