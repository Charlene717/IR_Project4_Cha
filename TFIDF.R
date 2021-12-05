## For Test
##### Paragraph to Sentence #####
xmlTestPath = "D:/Dropbox/##_GitHub/0-R/IR_Project4_Cha/Database_TestXML/test1.xml"
Output_Sum <- XML_to_df(xmlTestPath)
XML.df <- Output_Sum[["XML.df"]]

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

# library(ggplot2)
# ggplot(SumTest2_4, aes(freq/total, fill = PMID)) +
#   geom_histogram(show.legend = FALSE) +
#   #xlim(NA, 0.0009) +
#   facet_wrap(~PMID, ncol = 2, scales = "free_y")

PMID_tf_idf <- SumTest2_4 %>%
  bind_tf_idf(word, PMIDLine, freq)

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
## Try1
PMID_tf_idf$word <- tolower(PMID_tf_idf$word)

PMID_tf_idf_Sum <- PMID_tf_idf %>% group_by(PMIDLine) %>% summarise(., avg=mean(tf_idf))
PMID_tf_idf_Sum <- full_join(PMID_tf_idf_Sum, SumTest1_1)

BestText.df <- PMID_tf_idf_Sum[PMID_tf_idf_Sum$avg == max(PMID_tf_idf_Sum$avg),]
BestText <- as.character(BestText.df$Text)

## Try2
KeyWordTest.df <- data.frame(word=c("covid19","level","import"),Weight = c(10,5,3))
PMID_tf_idf2 <- left_join(PMID_tf_idf,KeyWordTest.df)
PMID_tf_idf2 <- mutate(PMID_tf_idf2,score=tf_idf*Weight)
PMID_tf_idf2[is.na(PMID_tf_idf2$score),]$score <-0 
PMID_tf_idf_Sum2 <- PMID_tf_idf2 %>% group_by(PMIDLine) %>% summarise(., avgscore=mean(score))
PMID_tf_idf_Sum2 <- full_join(PMID_tf_idf_Sum2, SumTest1_1)


BestText.df2 <- PMID_tf_idf_Sum2[PMID_tf_idf_Sum2$avgscore == max(PMID_tf_idf_Sum2$avgscore),]
BestText2 <- as.character(BestText.df2$Text)

BestTextinPMID.df2 <- PMID_tf_idf_Sum2 %>% group_by(PMID) %>% summarise(., maxscore=max(avgscore))
BestTextinPMID2 <- PMID_tf_idf_Sum2[PMID_tf_idf_Sum2$avgscore %in% BestTextinPMID.df2$maxscore,]


# PMID_tf_idf_Sum2Max <- PMID_tf_idf2 %>% group_by(PMIDLine) %>% summarise(., maxscore=max(score))
