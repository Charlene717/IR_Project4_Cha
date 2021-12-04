##### Separate the Paragraph to Sentence #####
SplitPara2Sent = function(XML.df.Abs1){
  
      # https://stackoverflow.com/questions/35304900/split-paragraph-into-sentences-in-r
      tmp <- strsplit(as.character(XML.df.Abs1), "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE) 
      # tmp <- str_split(XML.df.Abs1, " ")
      # tmp <- substr(XML.df.Abs1, " ")
      
      Output_Sent = tmp[[1]] %>% as.data.frame() #%>% as.character() %>% enc2utf8()
      
      # Remove <U+00A0>
      # https://stackoverflow.com/questions/41108617/remove-u00a0-from-values-in-columns-in-r
      Output_Sent <- as.data.frame(lapply(Output_Sent , function(x) {
        gsub("\u00A0", "", x) 
      }))
      colnames(Output_Sent) = "Text"

return(Output_Sent)
}

##### Separate the Sentence to Word #####
SplitSent2Word = function(XML.df.Abs1){
  ## https://cloud.tencent.com/developer/ask/44882
  #XML.df.Abs1 <- str_replace_all(XML.df.Abs1, "[[:punct:]]", " " ) 
  # https://www.codenong.com/21533899/
  XML.df.Abs1 <- gsub("[[:punct:]]","", XML.df.Abs1, ignore.case ="-" )
  tmp <- strsplit(as.character(XML.df.Abs1), " ", perl = TRUE) 
  # tmp <- substr(XML.df.Abs1, " ")
  
  Output_Sent = tmp[[1]] %>% as.data.frame() #%>% as.character() %>% enc2utf8()
  
  # Remove <U+00A0>
  # https://stackoverflow.com/questions/41108617/remove-u00a0-from-values-in-columns-in-r
  Output_Sent <- as.data.frame(lapply(Output_Sent , function(x) {
    gsub("\u00A0", "", x) 
  }))
  colnames(Output_Sent) = "Word"
  
  return(Output_Sent)
}

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

##### Sentence to Word #####
SumTest2_3 <- data.frame(matrix(nrow = 0,ncol = 4))
for (j in c(1:nrow(SumTest1))) {
  SumTest2 <- SplitSent2Word(SumTest1[j,3])
  
  SumTest2_2 <- as.data.frame(table(SumTest2))
  SumTest2_3 <- rbind(SumTest2_3,data.frame(PMID= SumTest1[j,1], Line = SumTest1[j,2], Word=SumTest2_2))
  
}
