xmlTestPath = "D:/Dropbox/##_GitHub/0-R/IR_Project4_Cha/Database_Cachexia/pmid-cancercach-set_1year100.xml"

xml1 <- xmlParse(xmlTestPath, encoding="UTF-8") %>% 
  xmlToList()
xml1[["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["ISOAbbreviation"]]


Output_Sum_Jl <- XML_to_df_Jl(xmlTestPath)
XML.df.Jl <-Output_Sum_Jl[["XML.df"]]



PathName <- getwd() ## Set output directroy
## Load files
dataJCR <- read.table(paste0(PathName,"/JournalHomeGrid_2019V2.csv"),  # 8j.F@I&W 
                      header=T,          # 8j.F$$*:2D$@&C!A'@,0Df&l&W:Y
                      sep=",")           # 1N3r895x,0$@9j2E89(SE*(z8j.F
colnames(dataJCR)[[3]] <- "Journal.Book"
dataJCR$Journal.Book <- toupper(dataJCR$Journal.Book)

dataPubMed <- read.csv(paste0(PathName,"/csv-cachexiaca-set2.csv"),  # 8j.F@I&W 
                      header=T,          # 8j.F$$*:2D$@&C!A'@,0Df&l&W:Y
                      sep=",")           # 1N3r895x,0$@9j2E89(SE*(z8j.F
dataPubMed <- data.frame(PMID=paste0('PMID: ',dataPubMed$PMID),dataPubMed)
dataPubMed$Journal.Book <- toupper(dataPubMed$Journal.Book)

dataPubMed2 <- left_join(dataPubMed,dataJCR)
dataPubMed2$Journal.Impact.Factor <- as.numeric(as.character(dataPubMed2$Journal.Impact.Factor))
dataPubMed2[dataPubMed2$Journal.Book== "J CACHEXIA SARCOPENIA MUSCLE",]$Journal.Impact.Factor <- 12.51
dataPubMed2[dataPubMed2$Journal.Book== "CELLS",]$Journal.Impact.Factor <- 4.366
dataPubMed2[dataPubMed2$Journal.Book== "CURR OPIN CLIN NUTR METAB CARE",]$Journal.Impact.Factor <- 3.775
dataPubMed2[dataPubMed2$Journal.Book== "LIFE (BASEL)",]$Journal.Impact.Factor <- 3.78
dataPubMed2[dataPubMed2$Journal.Book== "CANCERS (BASEL)",]$Journal.Impact.Factor <- 6.639
dataPubMed2[dataPubMed2$Journal.Book== "AM J HOSP PALLIAT CARE",]$Journal.Impact.Factor <- 1.655
dataPubMed2[dataPubMed2$Journal.Book== "ASIA PAC J ONCOL NURS",]$Journal.Impact.Factor <- 2.03
dataPubMed2[dataPubMed2$Journal.Book== "BMJ SUPPORT PALLIAT CARE",]$Journal.Impact.Factor <- 3.568
dataPubMed2[dataPubMed2$Journal.Book== "EUR J SURG ONCOL",]$Journal.Impact.Factor <- 4.42
dataPubMed2[dataPubMed2$Journal.Book== "J APPL PHYSIOL (1985)",]$Journal.Impact.Factor <- 3.531
dataPubMed2[dataPubMed2$Journal.Book== "CANCER MED",]$Journal.Impact.Factor <- 3.362
dataPubMed2[dataPubMed2$Journal.Book== "CLIN NUTR ESPEN",]$Journal.Impact.Factor <- 2.38

dataPubMed3 <- dataPubMed2[,c(1,7,16)]
SumTest2_5 <- left_join(SumTest2_4,dataPubMed3)


SumTest2_6 <- SumTest2_5[,c(5,3,4)]
#SumTest2_6$word <- tolower(SumTest2_6$word)
SumTest2_6_Matrix <- tidyr::spread(SumTest2_6,word,freq)
# SumTest2_6_MatrixTTT <- SumTest2_6_Matrix[,1000:1500]
SumTest2_6_Matrix[is.na(SumTest2_6_Matrix)] <-0

row.names(SumTest2_6_Matrix) <- SumTest2_6_Matrix[,1]
SumTest2_6_Matrix <- SumTest2_6_Matrix[,-1]
SumTest2_6_Matrix <- t(SumTest2_6_Matrix)
SumTest_Matrix <- SumTest2_6_Matrix[-1:-247,]
colnames(SumTest_Matrix) <- seq(1:ncol(SumTest_Matrix))

expression_matrix <- SumTest_Matrix
cell_metadata = SumTest2_5
cell_metadata <- cell_metadata %>% group_by(PMIDLine) %>% filter (! duplicated(PMIDLine)) 
TFIDFScore <- full_join(BestTextinPMID_Ori,BestTextinPMID_ModeA,by="PMIDLine")
TFIDFScore <- full_join(TFIDFScore,BestTextinPMID_ModeB,by="PMIDLine")
TFIDFScore <-TFIDFScore[,c(2,3,8,13)]
colnames(TFIDFScore) <- c("PMIDLine","avgScore_Ori","avgScore_ModeA","avgScore_ModeB")
cell_metadata <- left_join(cell_metadata,TFIDFScore)

Sentscore <- function(PMID_tf_idfTest1,KeyWordW2v.df){
  PMID_tf_idfOri <- left_join(PMID_tf_idfTest1,KeyWordW2v.df)
  PMID_tf_idfOri[is.na(PMID_tf_idfOri$dist),]$dist <- 0 
  PMID_tf_idfOri <- mutate(PMID_tf_idfOri,score=tf_idf*(1+dist))
  # PMID_tf_idfOri[is.na(PMID_tf_idfOri$score),]$score <- 0 
  PMID_tf_idfOri_Sum <- PMID_tf_idfOri %>% group_by(PMIDLine) %>% summarise(., score=mean(score))
  PMID_tf_idfOri_Sum <- full_join(PMID_tf_idfOri_Sum, SumTest1_1)
  
  # BestText.df3 <- PMID_tf_idfOri_Sum[PMID_tf_idfOri_Sum$avgscore == max(PMID_tf_idfOri_Sum$avgscore),]
  # BestText3 <- as.character(BestText.df3$Text)
  # 
  # BestTextinPMID.df3 <- PMID_tf_idfOri_Sum %>% group_by(PMID) %>% summarise(., maxscore=max(avgscore))
  # BestTextinPMID3 <- PMID_tf_idfOri_Sum[PMID_tf_idfOri_Sum$avgscore %in% BestTextinPMID.df3$maxscore,]
  return(PMID_tf_idfOri_Sum)
}

Sentscore_Ori <- Sentscore(PMID_tf_idfTest1,KeyWordW2v.df)
Sentscore_ModeA <- Sentscore(PMID_tf_idfTest2,KeyWordW2v.df)
Sentscore_ModeB <- Sentscore(PMID_tf_idfTest3,KeyWordW2v.df)
TFIDFScoreAll <- left_join(Sentscore_Ori,Sentscore_ModeA ,by="PMIDLine")
TFIDFScoreAll <- left_join(TFIDFScoreAll,Sentscore_ModeB ,by="PMIDLine")

TFIDFScoreAll <-TFIDFScoreAll[,c(1,2,6,10)]
colnames(TFIDFScoreAll) <- c("PMIDLine","Score_Ori","Score_ModeA","Score_ModeB")
TFIDFScoreAll$Score_Ori <- log10(1+TFIDFScoreAll$Score_Ori)
TFIDFScoreAll$Score_ModeA <- log10(1+TFIDFScoreAll$Score_ModeA)
TFIDFScoreAll$Score_ModeB <- log10(1+TFIDFScoreAll$Score_ModeB)

cell_metadata <- left_join(cell_metadata,TFIDFScoreAll)


gene_annotation = data.frame(gene_short_name=row.names(SumTest_Matrix))
row.names(gene_annotation) = row.names(SumTest_Matrix)

library(monocle3)
cds <- new_cell_data_set(expression_matrix,
                         cell_metadata = cell_metadata,
                         gene_metadata = gene_annotation)
cds <- preprocess_cds(cds, num_dim = 15)
plot_pc_variance_explained(cds)
cds <- reduce_dimension(cds)
plot_cells(cds)
plot_cells(cds, color_cells_by="Journal.Impact.Factor",cell_size = 3)
plot_cells(cds, color_cells_by="PMID",cell_size = 3,label_cell_groups = F)


cds <- reduce_dimension(cds, reduction_method="tSNE")
UMAP_Score_IF <-plot_cells(cds, reduction_method="UMAP", color_cells_by="Journal.Impact.Factor",
           cell_size = 3,scale_to_range=TRUE)%>% BeautifyggPlot(LegPos = c(0.1, 0.25))

UMAP_Score_IF <- UMAP_Score_IF + scale_colour_gradient2(low = "black", mid = "#750182", high = "#f25c90", 
                                                          guide = "colourbar",midpoint = 0, labs(fill = "Exp"))+
                                                          ggtitle("Impact.Factor")
UMAP_Score_IF

plot_cells(cds, reduction_method="tSNE", color_cells_by="avgScore_Ori",
           cell_size = 3,scale_to_range=TRUE)
plot_cells(cds, reduction_method="tSNE", color_cells_by="avgScore_ModeA",
           cell_size = 3,scale_to_range=TRUE)
plot_cells(cds, reduction_method="tSNE", color_cells_by="avgScore_ModeB",
           cell_size = 3,scale_to_range=TRUE)

# UMAP_Score_Ori
UMAP_Score_Ori <- plot_cells(cds, reduction_method="UMAP", color_cells_by="Score_Ori",
           cell_size = 3,scale_to_range=TRUE) %>% BeautifyggPlot(LegPos = c(0.1, 0.25))

UMAP_Score_Ori <- UMAP_Score_Ori + scale_colour_gradient2(low = "black", mid = "#1d91de", high = "#ff85e9", 
                         guide = "colourbar",midpoint = 0, labs(fill = "Exp"))+
                ggtitle("TF-IDF_ORi")
UMAP_Score_Ori

# UMAP_Score_ModeA
UMAP_Score_ModeA <- plot_cells(cds, reduction_method="UMAP", color_cells_by="Score_ModeA",
           cell_size = 3,scale_to_range=TRUE) %>% BeautifyggPlot(LegPos = c(0.1, 0.25))

UMAP_Score_ModeA <- UMAP_Score_ModeA + scale_colour_gradient2(low = "black", mid = "#1d91de", high = "#ff85e9", 
                                        guide = "colourbar",midpoint = 0, labs(fill = "Exp"))+
  ggtitle("TF-IDF_ModeA")
UMAP_Score_ModeA 

# UMAP_Score_ModeB
UMAP_Score_ModeB <-plot_cells(cds, reduction_method="UMAP", color_cells_by="Score_ModeB",
           cell_size = 3,scale_to_range=TRUE)%>% BeautifyggPlot(LegPos = c(0.1, 0.25))

UMAP_Score_ModeB <- UMAP_Score_ModeB + scale_colour_gradient2(low = "black", mid = "#1d91de", high = "#ff85e9", 
                                          guide = "colourbar",midpoint = 0, labs(fill = "Exp"))+
  ggtitle("TF-IDF_ModeB")
UMAP_Score_ModeB


grid.arrange(UMAP_Score_IF, UMAP_Score_Ori, UMAP_Score_ModeA ,UMAP_Score_ModeB , nrow = 2)
## Group cells into clusters
cds = cluster_cells(cds, resolution=1e-1)
plot_cells(cds,cell_size = 3)


plot_cells(cds,
           genes="cachexia",
           label_cell_groups=FALSE,
           show_trajectory_graph=FALSE,cell_size = 3,norm_method ="size_only")
plot_cells(cds,
           genes="cancer",
           label_cell_groups=FALSE,
           show_trajectory_graph=FALSE,cell_size = 3)

plot_cells(cds,
           genes="pancreatic",
           label_cell_groups=FALSE,
           show_trajectory_graph=FALSE,cell_size = 3,norm_method = "size_only")

