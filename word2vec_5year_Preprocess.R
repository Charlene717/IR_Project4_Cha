## https://github.com/mukul13/rword2vec
## https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html
## https://medium.com/broadhorizon-cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234


#############
# rm(list = ls()) # Clean variable

memory.limit(150000)

#####  Function setting ##### 
## Call function
source("FUN_XML_to_df.R") # Load file
source("FUN_tSNE.R")

library(rword2vec)
library(magrittr)
##### Load XML files #####
# Output_Sum_5year <- XML_to_df("D:/Ch_Bioinformatics Dropbox/Chang Charlene/##_GitHub/0-R/IR_Project4_Cha/Database_Cachexia/pmid-cancercach-set_5year1948.xml")
# Output_Sum_5year <- XML_to_df("C:/Users/user/Desktop/Database_Cachexia/pmid-cancercach-set_5year1948.xml")
Output_Sum_1year <- XML_to_df("C:/Users/user/Desktop/Database_Cachexia/pmid-cancercach-set_1year100.xml")

XML_df_1year <-  Output_Sum_1year[["XML.df"]]
XML_df_1year <- XML_df_1year[!is.na(XML_df_1year$CHAR),]
XML_df_1year <- XML_df_1year[!XML_df_1year$CHAR == 0,]

string_1year <- paste0(XML_df_1year$Abstract)
string_1year %>% 
            tibble(name = .) %>%
            filter(xfun::is_ascii(name)== T)

string_1year.df <- SplitSent2Word(string_1year)
colnames(string_1year.df)[1] <- c("word") 
string_1year.df$word <- as.character(string_1year.df$word)
## Stemming & Remove stop word
string_1year.df <- string_1year.df %>%
  mutate(stem = wordStem(word)) %>% 
  anti_join(get_stopwords()) 

# Remove punctuation 
library(stringr)
string_1year_SRP <- str_replace_all(string_1year, "[[:punct:]]", " ")  # SRP: string remove punctuation
# Stemming
Test <- wordStem(string_1year_SRP)

write.table(string_1year_SRP, file="string_1year_SRP.txt", sep="\t", col.names=T,
            eol = " ",
            row.names=T, quote=F)



##### word2vec SG #####
.rs.restartR()
### 1year_SRP
library(rword2vec)
ls("package:rword2vec")

model_1year_SRP = word2vec(train_file = "string_1year_SRP.txt",output_file = "vec_1year_SRP.bin",binary=1)

## Distance:
# file_name must be binary
dist_cachexia_1year_SRP = distance(file_name = "vec_1year_SRP.bin",search_word = "cachexia",num = 1000)
dist_Gene_1year_SRP = distance(file_name = "vec_1year_SRP.bin",search_word = "GDF15",num = 1000)
# dist_Gene_1year_SRP2 = distance(file_name = "vec_1year_SRP.bin",search_word = "gene",num = 1000)
# dist_Gene_1year_SRP3 = distance(file_name = "vec_1year_SRP.bin",search_word = "ZIP14",num = 1000)

## Word count:
# to count word occurences in input file
vocab_count(file_name="string_1year_SRP.txt",vocab_file="vocab.txt",min_count = 20)
Word_count=read.table("vocab.txt")
head(Word_count)

## Word analogy:
ana_cachexia_1year_SRP = word_analogy(file_name = "vec_1year_SRP.bin",search_words = "normal zinc cachexia",num = 1000)
ana_cachexia_1year_SRP2 = word_analogy(file_name = "vec_1year_SRP.bin",search_words = "healthy ZIP14 cachexia",num = 1000)
# ana_cachexia_1year_SRP3 = word_analogy(file_name = "vec_1year_SRP.bin",search_words = "normal cachexia ZIP14",num = 1000)
# ana_cachexia_1year_SRP4 = word_analogy(file_name = "vec_1year_SRP.bin",search_words = "cachexia ZIP14 normal",num = 1000)
# ana_cachexia_1year_SRP5 = word_analogy(file_name = "vec_1year_SRP.bin",search_words = "cachexia ZIP14 healthy",num = 1000)
# ana_cachexia_1year_SRP6 = word_analogy(file_name = "vec_1year_SRP.bin",search_words = "healthy CD8 cachexia",num = 1000)

## Bin to txt:
#convert .bin to .txt
bin_to_txt("vec_1year_SRP.bin","vec_1year_SRP.txt")
data_1year_SRP = as.data.frame(read.table("vec_1year_SRP.txt",skip=1))
data_1year_SRP[1,]

### DR_tSNE
data_1year_SRP_DR <- data_1year_SRP

data_1year_SRP_DR <- data_1year_SRP_DR[!(is.na(data_1year_SRP_DR$V1)),]
row.names(data_1year_SRP_DR) = data_1year_SRP_DR[,1]
data_1year_SRP_DR = data_1year_SRP_DR[,-1]

library(Rtsne)
library(magrittr)
library(ggplot2)
library(dplyr)

# https://jmonlong.github.io/Hippocamplus/2018/02/13/tsne-and-clustering/

KY_SG ="cachexia"
BIN_SG = "vec_1year_SRP.bin"
tsne_SG_plot<- tSNEPlot(KY_SG,BIN_SG,50)
tsne_SG_plot

KY_W2P ="cachexia"
BIN_W2P = "vec_1year_SRP_word2phrase.bin"
tsne_word2phrase_plot <- tSNEPlot(KY_W2P,BIN_W2P,50)
tsne_word2phrase_plot

###########

### Training word2phrase model:
word2phrase(train_file = "string_1year_SRP.txt",output_file = "string_1year_SRP_word2phrase.txt")

.rs.restartR()
### 1year_SRP
library(rword2vec)
ls("package:rword2vec")
# use this new text file to give word vectors
model_word2phrase = word2vec(train_file = "string_1year_SRP_word2phrase.txt",output_file = "vec_1year_SRP_word2phrase.bin",binary=1)

## Distance:
# file_name must be binary
dist_cachexia_1year_SRP_word2phrase = distance(file_name = "vec_1year_SRP_word2phrase.bin",search_word = "cachexia",num = 100)
dist_Gene_5yea_SRP_word2phrase = distance(file_name = "vec_1year_SRP_word2phrase.bin",search_word = "GDF15",num = 100)

# Word count:
# to count word occurences in input file
vocab_count(file_name="string_1year_SRP_word2phrase.txt",vocab_file="vocab_word2phrase.txt",min_count = 20)
Word_count_word2phrase = read.table("vocab_word2phrase.txt")
head(Word_count_word2phrase)

# Bin to txt:
#convert .bin to .txt
bin_to_txt("vec_1year_SRP_word2phrase.bin","vector_1year_SRP_word2phrase.txt")
data_1year_SRP_word2phrase=as.data.frame(read.table("vector_1year_SRP_word2phrase.txt",skip=1))
data_1year_SRP_word2phrase[1,]

### DR_tSNE
data_1year_SRP_word2phrase_DR <- data_1year_SRP_word2phrase 

data_1year_SRP_word2phrase_DR <- data_1year_SRP_word2phrase_DR[!(is.na(data_1year_SRP_word2phrase_DR$V1)),]
row.names(data_1year_SRP_word2phrase_DR) = data_1year_SRP_word2phrase_DR[,1]
data_1year_SRP_word2phrase_DR = data_1year_SRP_word2phrase_DR[,-1]

library(Rtsne)
library(magrittr)
library(ggplot2)
library(dplyr)

# https://jmonlong.github.io/Hippocamplus/2018/02/13/tsne-and-clustering/
Keyword ="cachexia"
dist_Keyword_word2phrase = distance(file_name = "vec_1year_SRP_word2phrase.bin",search_word = Keyword,num = 1000)

data_DR_word2phrase_K <- data_1year_SRP_word2phrase_DR[rownames(data_1year_SRP_word2phrase_DR) %in% c(as.character(dist_Keyword_word2phrase$word),Keyword),]

set.seed(1) # Fix the seed
tsne_word2phrase <- Rtsne(data_DR_word2phrase_K, perplexity = 50, pca = FALSE)

tsneP_word2phrase<- as.data.frame(tsne_word2phrase$Y)
row.names(tsneP_word2phrase) <- row.names(data_DR_word2phrase_K)

#
#dist_Keyword_word2phrase = distance(file_name = "vec_1year_SRP_word2phrase.bin",search_word = Keyword,num = 1000)

tsneP_word2phrase$SearchWord <- ifelse(row.names(tsneP_word2phrase) %in% c(as.character(dist_Keyword_word2phrase$word[1:50]),Keyword),
                     ifelse(row.names(tsneP_word2phrase) %in% Keyword,'SearchWord','SW_CosClose'),'Other')

# tsneP_word2phrase$SearchWord <- ifelse(row.names(tsneP_word2phrase) %in% c(as.character(dist_Keyword_word2phrase$word[1:50]),Keyword) ,'SearchWord','Other')
color <- c(SearchWord = "red",SW_CosClose = "#d538fc",Other = "gray")
tsne_word2phrase_plot2 <- tsneP_word2phrase%>%
  as.data.frame() %>%
  mutate(word = row.names(tsneP_word2phrase)) %>%
  ggplot(aes(x = V1, y = V2, label = word, col = SearchWord))+
  scale_color_manual(values = color) +
  geom_text(size = 3)
tsne_word2phrase_plot2






tsne_word2phrase_plot <- tsneP_word2phrase%>%
  as.data.frame() %>%
  mutate(word = row.names(tsneP_word2phrase)) %>%
  ggplot(aes(x = V1, y = V2, label = word))+
  geom_text(size = 3)
tsne_word2phrase_plot

## Monocle3




##### word2vec CROW #####

