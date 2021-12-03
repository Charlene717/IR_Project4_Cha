##### Separate the sentence #####

xmlTestPath = "D:/Dropbox/##_GitHub/0-R/IR_Project4_Cha/Database_TestXML/test1.xml"
Output_Sum <- XML_to_df(xmlTestPath)
XML.df <- Output_Sum[["XML.df"]]

XML.df.Abs <- data.frame(Abstract = XML.df$Abstract)
XML.df.Abs1 <- XML.df.Abs[1,]
# tmp <- str_split(XML.df.Abs1, " ")
# tmp <- substr(XML.df.Abs1, " ")

tmp <- strsplit(as.character(XML.df.Abs1), "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE) # https://stackoverflow.com/questions/35304900/split-paragraph-into-sentences-in-r


# XML.df.Abs.Sent <- sapply(strsplit(as.character(XML.df.Abs1), "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE), length)


SplitPara2Sent = function(XML.df.Abs1){
  
      # https://stackoverflow.com/questions/35304900/split-paragraph-into-sentences-in-r
      tmp <- strsplit(as.character(XML.df.Abs1), "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE) 
      Output_Sent = tmp[[1]] %>% as.data.frame() #%>% as.character() %>% enc2utf8()
      
      # Remove <U+00A0>
      # https://stackoverflow.com/questions/41108617/remove-u00a0-from-values-in-columns-in-r
      Output_Sent <- as.data.frame(lapply(Output_Sent , function(x) {
        gsub("\u00A0", "", x) 
      }))
      colnames(Output_Sent) = "Text"

return(Output_Sent)
}

Test2 <- SplitPara2Sent(XML.df.Abs[2,])






