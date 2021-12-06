# Analyzing word and document frequency: tf-idf
# https://www.tidytextmining.com/tfidf.html


## For Test
##### Paragraph to Sentence #####
# xmlTestPath = "D:/Dropbox/##_GitHub/0-R/IR_Project4_Cha/Database_TestXML/test1.xml"
xmlTestPath = "D:/Dropbox/##_GitHub/0-R/IR_Project4_Cha/Database_Cachexia/pmid-cancercach-set_1year100.xml"

Output_Sum <- XML_to_df(xmlTestPath)
XML.df <- Output_Sum[["XML.df"]]
XML.df <- XML.df[!is.na(XML.df$CHAR),]
XML.df <- XML.df[!XML.df$CHAR == 0,]


XML.df.Abs <- data.frame(PMID = XML.df$PMID ,Abstract = XML.df$Abstract)
XML.df.Abs1 <- XML.df.Abs[1,]

SumTest1 <- data.frame(matrix(nrow = 0,ncol = 3))
for (i in c(1:nrow(XML.df.Abs))) {
  Test <- SplitPara2Sent(XML.df.Abs[i,2])
  SumTest1 <- rbind(SumTest1,data.frame(PMID= XML.df.Abs[i,1], Line = seq(1,nrow(Test),by=1), Sent=Test))
  
}

SumTest1_1 <- mutate(SumTest1,PMIDLine=paste0(PMID,"_",Line))

##### Sentence to Word #####
SumTest2_3 <- data.frame(matrix(nrow = 0,ncol = 4))
for (j in c(1:nrow(SumTest1))) {
  SumTest2 <- SplitSent2Word(SumTest1[j,3])
  
  SumTest2_2 <- as.data.frame(table(SumTest2))
  colnames(SumTest2_2)[1] <- c("word") 
  ## Stemming & Remove stop word
  SumTest2_2 <- SumTest2_2 %>%
    mutate(stem = wordStem(word)) %>% 
    anti_join(get_stopwords()) %>% 
    count(stem, sort = TRUE)
  
  SumTest2_3 <- rbind(SumTest2_3,data.frame(PMID= SumTest1[j,1], Line = SumTest1[j,2], Word=SumTest2_2))
  
}
colnames(SumTest2_3)[c(3,4)] <- c("word","freq")

# ###############################################################################
# 
# ## Test Stemming & Remove stop word
# # https://smltar.com/stemming.html
# 
# ## Stemming
# SumTest2_4 <- SumTest2_3 %>%
#               mutate(stem = wordStem(word)) %>%
#               count(stem, sort = TRUE)
# ## Stemming & Remove stop word
# SumTest2_5 <- SumTest2_3 %>%
#               mutate(stem = wordStem(word)) %>% 
#               anti_join(get_stopwords()) %>%
#               count(stem, sort = TRUE)
# 
# ###############################################################################

##### TF-IDF #####
SumTest2_4 <- mutate(SumTest2_3,PMIDLine=paste0(PMID,"_",Line))
total_words <- SumTest2_4 %>% 
  group_by(PMID) %>% 
  summarise(total = sum(freq))  # https://github.com/tidyverse/dplyr/issues/505

SumTest2_4 <- left_join(SumTest2_4, total_words)
SumTest2_4 <- SumTest2_4[!is.na(SumTest2_4$word),]
SumTest2_4 <- SumTest2_4[!SumTest2_4$word=="",]

# library(ggplot2)
# ggplot(SumTest2_4, aes(freq/total, fill = PMID)) +
#   geom_histogram(show.legend = FALSE) +
#   #xlim(NA, 0.0009) +
#   facet_wrap(~PMID, ncol = 2, scales = "free_y")

PMID_tf_idf <- SumTest2_4 %>%
  bind_tf_idf(word, PMIDLine, freq)

PMID_tf_idfTest1 <- SumTest2_4 %>%
  TF_IDF(word, PMIDLine, freq)

PMID_tf_idfTest2 <- SumTest2_4 %>%
  TF_IDF(word, PMIDLine, freq, mode ="A")
PMID_tf_idfTest3 <- SumTest2_4 %>%
  TF_IDF(word, PMIDLine, freq, mode ="B")
PMID_tf_idfTest4 <- SumTest2_4 %>%
  TF_IDF(word, PMIDLine, freq, mode ="C")
PMID_tf_idfTest5 <- SumTest2_4 %>%
  TF_IDF(word, PMIDLine, freq, mode ="D")


PMID_tf_idf

PMID_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))



library(forcats)

PMID_tf_idf %>%
  group_by(PMID) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = PMID)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~PMID, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)



##### Count the score to decide the best Sentence #####
Keyword.df <- data.frame(word=c("Cachexia"),dist = c(100))
KeyWordW2v.df <- rbind(Keyword.df, dist_cachexia_5year_SRP)
KeyWordW2v.df$dist %>% as.numeric() -> KeyWordW2v.df$dist

## Word2Vector Ori
FindBestSent <- function(PMID_tf_idfTest1,KeyWordW2v.df){
      PMID_tf_idfOri <- left_join(PMID_tf_idfTest1,KeyWordW2v.df)
      PMID_tf_idfOri[is.na(PMID_tf_idfOri$dist),]$dist <- 0 
      PMID_tf_idfOri <- mutate(PMID_tf_idfOri,score=tf_idf*(1+dist))
      # PMID_tf_idfOri[is.na(PMID_tf_idfOri$score),]$score <- 0 
      PMID_tf_idfOri_Sum <- PMID_tf_idfOri %>% group_by(PMIDLine) %>% summarise(., avgscore=mean(score))
      PMID_tf_idfOri_Sum <- full_join(PMID_tf_idfOri_Sum, SumTest1_1)
      
      BestText.df3 <- PMID_tf_idfOri_Sum[PMID_tf_idfOri_Sum$avgscore == max(PMID_tf_idfOri_Sum$avgscore),]
      BestText3 <- as.character(BestText.df3$Text)
      
      BestTextinPMID.df3 <- PMID_tf_idfOri_Sum %>% group_by(PMID) %>% summarise(., maxscore=max(avgscore))
      BestTextinPMID3 <- PMID_tf_idfOri_Sum[PMID_tf_idfOri_Sum$avgscore %in% BestTextinPMID.df3$maxscore,]
      return(BestTextinPMID3)
}



BestTextinPMID_Ori <- FindBestSent(PMID_tf_idfTest1,KeyWordW2v.df)
BestTextinPMID_Ori <- BestTextinPMID_Ori[order(BestTextinPMID_Ori$avgscore, decreasing = T),]
BestTextinPMID_Ori <- data.frame(No=seq(1:nrow(BestTextinPMID_Ori)),BestTextinPMID_Ori)

BestTextinPMID_ModeA <- FindBestSent(PMID_tf_idfTest2,KeyWordW2v.df)
BestTextinPMID_ModeA <- BestTextinPMID_ModeA[order(BestTextinPMID_ModeA$avgscore, decreasing = T),]
BestTextinPMID_ModeA <- data.frame(No=seq(1:nrow(BestTextinPMID_ModeA)),BestTextinPMID_ModeA)

BestTextinPMID_ModeB <- FindBestSent(PMID_tf_idfTest3,KeyWordW2v.df)
BestTextinPMID_ModeB <- BestTextinPMID_ModeB[order(BestTextinPMID_ModeB$avgscore, decreasing = T),]
BestTextinPMID_ModeB <- data.frame(No=seq(1:nrow(BestTextinPMID_ModeB)),BestTextinPMID_ModeB)