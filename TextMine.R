#--------------------------------

req.pcg <- function(pcg){
  # packages to be installed
  tbinst <- pcg[(!(pcg %in% installed.packages()[, "Package"]))|
                  (pcg %in% old.packages()[, "Package"])]  
  if (sum(tbinst %in% c("tmcn", "Rwordseg", "Rweibo"))>0){
    cntm <- tbinst[tbinst %in% c("tmcn", "Rwordseg", "Rweibo")]
    install.packages(cntm, 
                     repos = "http://R-Forge.R-project.org", 
                     type = "source")
  }else if(sum(tbinst == "Rgraphviz")>0){
    source("http://bioconductor.org/biocLite.R")
    biocLite("Rgraphviz")
  }else if (length(tbinst)){
    install.packages(tbinst, dependencies = T)  
  } 
  sapply(pcg, require, warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)  
}

all.pcg <- c("tm", "SnowballC", "qdap", "qdapDictionaries", "dplyr", 
             "RColorBrewer", "ggplot2", "scales", "wordcloud", "igraph",
             "Rweibo", "Rwordseg", "RWeka", "ggdendro")

req.pcg(all.pcg)

# ERROR: compilation failed for package 'tmcn'
# Warning in install.packages : package 'tmcn' is not available (for R version 3.2.0)

#--------------------------------

df <- read.csv("FO_Increased.csv")
df <- read.csv("FO_Dropped.csv")
df <- read.csv("FO_Same.csv")
df <- read.csv("FO_Total.csv")
df <- read.csv("Australia.csv")

i <- 1
i <- 2

sub_cont <- Corpus(VectorSource(df[complete.cases(df[, i]), i]))

sub_cont <- sub_cont %>% 
            tm_map(content_transformer(tolower)) %>%
            tm_map(removeWords, stopwords("english")[c(-81:-98, -160, -165:-167)])
# [160] "more"
# others: not

change <- content_transformer(function(x, from, to) gsub(from, to, x))
for(j in c(81:98, 166)) {
  sub_cont <- tm_map(sub_cont, change, stopwords("english")[j], "not")
}

sub_cont <- sub_cont %>% 
            tm_map(change, "blue oval", "blueoval") %>%
            tm_map(change, "loyal followers", "loyalfollower")


sub_cont <- sub_cont %>% 
            tm_map(removePunctuation) %>% 
            tm_map(stripWhitespace) %>%
            tm_map(stemDocument) %>%
            tm_map(removeNumbers)

# change words into original form
mat <- matrix(c(c("releas", "purchas", "websit", "territori", "specif", "peopl", "futur", "decid", "brochur", "pictur"), 
                c("release", "purchase", "website", "territory", "specify", "people", "future", "decide", "brochure", "picture")),
              nrow = 2, byrow = TRUE)
for(k in 1:ncol(mat)){
  sub_cont <- tm_map(sub_cont, change, mat[1, k], mat[2, k])  
}

sub_cont <- tm_map(sub_cont, removeWords, 
                   c("even", "still", "just", "will", "yet", "can", "much", "car", "ford", "also", "one", "vehicl"))

dtm <- DocumentTermMatrix(sub_cont)
tdm <- TermDocumentMatrix(sub_cont)
dim(dtm)
# inspect(dtm[1:5, 1:5])
freq <- colSums(as.matrix(dtm))
# length(freq)
ord <- order(freq, decreasing = TRUE)
# table(freq)
freq <- freq[ord]

wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Histogram of Frequency
subset(wf, freq > 2) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

comp <- function(words, mat){
  for(i in 1:length(words)){
    if(any(words[i] == mat[1, ])){
      words[i] <- mat[2, ][which(words[i] == mat[1, ])]
    }
  }
  return(words)
}
wf$word <- comp(wf$word, mat)

wf$word <- factor(wf$word, levels = wf[order(wf[,2], decreasing = FALSE), 1], ordered=T)

ggplot(subset(wf, freq > 25),aes(x= word, freq))    +
  geom_bar(stat = "identity")                       +
  coord_flip()                                      +
  ggtitle("Word Frequency > 25")                    +
  ylab("Frequency")                                 +
  xlab("Word")
# png("Dendrogram_db.png", width=12, height=8, units="in", res=300)
set.seed(123)
wordcloud(names(freq), freq, min.freq = 4, scale = c(5, .8), 
          random.order = FALSE, colors=brewer.pal(6, "Dark2"))

# Association plot
Attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE,
                          style = "invis", fontcolor = "white",
                          fillcolor = "red"),
                          edge = list(dir = "both", color = "darkblue", weight = 1.2))
plot(dtm,
     terms = findFreqTerms(dtm, lowfreq = 4),
     corThreshold = 0.2,
     attrs = Attrs, 
     weighting = TRUE)  

# Cluster Dendrogram:
# DistMat <- dist(scale(as.matrix(tdm)))
DistMat <- dist(scale(as.matrix(tdm)[order(rowSums(as.matrix(tdm)), decreasing = TRUE), ][1:35, ]))
fit <- hclust(DistMat) 
# method = "ward.D", "ward.D2", "single", "complete", "average"...
plot(fit)
ggdendrogram(fit)
# cut tree into k clusters
rect.hclust(fit, k = 6)
# rect.hclust(tree, k = NULL, which = NULL, x = NULL, h = NULL,
#             border = 2, cluster = NULL)

# kmeans

# findAssocs(dtm, "not", corlimit = 0.3)

#------------------------------------
df <- read.csv("FO_Compare.csv", head = FALSE)
df <- read.csv("FO_Compare2.csv", head = FALSE)

df <- sapply(df, as.character)
df <- df[, -1]
sub_cont <- Corpus(DataframeSource(df))

tdm6 <- as.matrix(tdm)
tdm6 <- tdm6[!rownames(tdm6) %in% c("new", "vehicl"), ]

colnames(tdm6) <- c("Drop1", "Drop2", "Increase1", "Increase2", "Same1", "Same2")

colnames(tdm6) <- c("Drop2", "Increase2", "Same2")

comparison.cloud(tdm6, random.order = F, max.words = Inf, title.size = 1.5)

commonality.cloud(tdm6, random.order=FALSE,
                   colors = brewer.pal(8, "Dark2"),
                   title.size=1.5)