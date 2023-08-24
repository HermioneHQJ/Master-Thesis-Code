setwd("/Users/hermionehe/Desktop/项目文件")

library(dplyr)
library(Matrix)
library(textmineR)
library(tidytext)
library(wordcloud)
library(NLP)
library(tm)
library(topicmodels)
library(reticulate)
library(stringr)
library(spatial)
library(rgdal)
library(proxy)
library(stringdist)
library(ggplot2)
library(GGally)
library(htmlwidgets)
library(magick)
library(tesseract)
library(exifr)
library(SnowballC)
library(knitr)

rules <- list(
  "s$" = "",
  "ing$" = "",
  "er$" = "",
  "est$" = "",
  "ies$" = "y",
  "men$" = "man",
  "ves$" = "f",
  "children$" = "child",
  "wn$" = ""
)
# Function to apply the rules and aggregate the words
aggregate_words <- function(word) {
  for (pattern in names(rules)) {
    if (grepl(pattern, word)) {
      return(gsub(pattern, rules[[pattern]], word))
    }
  }
  return(word)
}
##aggregate_words函数使用这些规则来对输入的单词进行处理。
###当一个单词匹配任何规则时，它会根据相应的规则进行替换。如果单词不匹配任何规则，它会原样返回。
###这个函数通常用于将单词转换为它们的基本形式或合并相似的单词，以便进行数据分析或文本处理。

## load objects with a new name
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}
  

###文本预处理
##它对文本数据进行一系列的预处理步骤：
###转换为小写字母、去除标点符号、去除数字、去除英文停用词（常见但没有实际意义的词汇，比如"the"、"is"等）、去除空格
preprocess_corpus <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  mywordsdf <- data.frame(word = character(), freq = numeric(), stringsAsFactors = FALSE)
  
  chunk_size <- 1000  # 每次处理的文本块大小
  

  for (i in seq(1, length(corpus), chunk_size)) {
    chunk <- corpus[i:min(i+chunk_size-1, length(corpus))]
    mytextdata <- TermDocumentMatrix(chunk)
    mtdmatrix <- as.matrix(mytextdata)  ###这一行将术语-文档矩阵转换为标准矩阵格式，以便处理词频。
    mywords <- sort(rowSums(mtdmatrix), decreasing = TRUE)  ###计算词块中每个术语（单词）的单词出现次数总和，并根据词频按递减顺序排序  
    mywordsdf <- rbind(mywordsdf, data.frame(word = names(mywords), freq = mywords)) ###使用 rbind 函数将当前语块的词频信息添加到 mywordsdf 数据框架中，从而有效地追加词频
    gc()  ###垃圾回收函数 (gc()) 手动释放内存
  }
  
  mywordsdf <- aggregate(freq ~ word, data = mywordsdf, FUN = sum)  
### 合并每个块的结果
###这一行汇总每个语块中的词频，并将其合并为一个数据帧。
###aggregate 函数用于按 "词 "列对数据进行分组，并计算每个词的词频总和。
###这一步非常重要，因为在循环过程中，每个词块的词频都是单独累积的，而现在我们要得到的是整个语料库中每个词的总词频。
  
  set.seed(412) 
  return(mywordsdf) ###函数返回处理后的 mywordsdf 数据帧，其中包含整个文本语料库的词频汇总
}

### here instead directly process corspus 
process_corpus_words <- function(text) {
  try(corpus<- Corpus(VectorSource(text)),silent=T) ## silenziate wrning to life a better life
  try( corpus <- tm_map(corpus, tolower),silent=T) ## silenziate wrning to life a better life
  try(corpus <- tm_map(corpus, removePunctuation),silent=T) ## silenziate wrning to life a better life
  try(corpus <- tm_map(corpus, removeNumbers),silent=T) ## silenziate wrning to life a better life
  try(corpus <- tm_map(corpus, removeWords, stopwords("english")),silent=T) ## silenziate wrning to life a better life
  try(corpus <- tm_map(corpus, stripWhitespace),silent=T) ## silenziate wrning to life a better life
  # Extract individual words
  corpus <- unlist(strsplit(as.character(corpus), "\\s+"))
  
  # Remove empty and short words
  corpus <- corpus[nchar(corpus) > 1]
  
  # Remove non-alphabetic words
  corpus <- corpus[grepl("^[a-zA-Z]+$", corpus)]
  
  mytextdata <- TermDocumentMatrix (corpus)
  mtdmatrix <- as.matrix (mytextdata)
  mywords <- sort( rowSums( mtdmatrix ),decreasing=TRUE)
  mywordsdf <- data.frame (word = names (mywords ),freq=mywords)
  set.seed( 412 )# for reproducibility
  return(mywordsdf)
  
  gc(reset=T)
  
}
## 载入语料库
TW_obj <- read.csv("TW_object_detection_results_long_format.csv")

##### 生成词云 ######
## 处理文本数据
TW_text <- TW_obj$text
mywordsdf <- preprocess_corpus(TW_text)

png("after setting the pipeline wordcloud/TW_image description blip and gpt.png", width = 8, height = 6, units = "in", res = 300)wordcloud (words = mywordsdf$word, freq = mywordsdf$freq
           ,min.freq = 0 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(3, 0.6))  

dev.off()


##### WORDCLOUD GPT DESCRIPTIONS ###########
## Start wordcloud text caption 
### all descriptions
TW_text <- Corpus(VectorSource(TW_obj[TW_obj$method=="Text_gpt2", "text"]))
mywordsdf<-preprocess_corpus (TW_text)

png("after setting the pipeline wordcloud/TW_image description gpt.png", width = 8, height = 6, units = "in", res = 300)

wordcloud (words = mywordsdf$word, freq = mywordsdf$freq
           ,min.freq = 0 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(3, 0.6))  

dev.off()

##### WORDCLOUD BLIP DESCRIPTIONS ###########
## Start wordcloud text caption 
### all descriptions
TW_text <- Corpus(VectorSource(TW_obj[TW_obj$method=="Caption_blip", "text"]))
mywordsdf<-preprocess_corpus (TW_text)

png("after setting the pipeline wordcloud/TW_image description blip.png", width = 8, height = 6, units = "in", res = 300)

wordcloud (words = mywordsdf$word, freq = mywordsdf$freq
           ,min.freq = 0 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(3, 0.6)) 
dev.off()

### LABELS NOW 处理标签相关的词云

lab_agree<-c("a")
for (i in unique(TW_obj$image_path)) {
  #  i="http://pbs.twimg.com/media/Fufg8OVWAAIiRhx.jpg"
  a<-(TW_obj[TW_obj$image_path==i & TW_obj$method=="YOLOS" & TW_obj$score>0.5,"label"])
  b<-(TW_obj[TW_obj$image_path==i & TW_obj$method=="FasterRCNN" & TW_obj$score>0.5,"label"])
  lab_agree<-c(lab_agree,intersect (a,b))
  print(i)
}
lab_agree<-lab_agree[-1]
Becky_text <- Corpus(VectorSource(lab_agree))
mywordsdf<-preprocess_corpus (Becky_text)

png("after setting the pipeline wordcloud/TW_image description labels 0.5 agreement.png", width = 12, height = 10, units = "in", res = 300)

wordcloud(words = mywordsdf$word, freq = log(mywordsdf$freq), min.freq = 0,
          max.words = 200, random.order = FALSE,
          rot.per = 0.25, colors = brewer.pal(8, "Dark2"), scale = c(2, 0.5))


dev.off()

#### LABELS >0.75 FASTER

lab_FAST<-c("a")
for (i in unique(TW_obj$image_path)) {
  #  i="/content/drive/MyDrive/Colab Notebooks/Becky Govanhill Photos/g.1.3 mary_0016.jpg"
  #a<-(TW_obj[TW_obj$image_path==i & TW_obj$method=="YOLOS" & TW_obj$score>0.5,"label"])
  b<-unique(TW_obj[TW_obj$image_path==i & TW_obj$method=="FasterRCNN" & TW_obj$score>0.75,"label"])
  lab_FAST<-c(lab_FAST,b)
  print(i)
}
lab_FAST<-lab_FAST[-1]
Becky_text <- Corpus(VectorSource(lab_FAST))
mywordsdf<-preprocess_corpus (Becky_text)

png("after setting the pipeline wordcloud/TW_image description Faster CNN 0.75.png", width = 12, height = 10, units = "in", res = 300)

wordcloud (words = mywordsdf$word, freq = log(mywordsdf$freq)
           ,min.freq = 0 ,max.words=200 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(2, 0.5))  
dev.off()

### now with YOLOS-small
lab_YOLOS<-c("a")
for (i in unique(TW_obj$image_path)) {
  #  i="/content/drive/MyDrive/Colab Notebooks/Becky Govanhill Photos/g.1.3 mary_0016.jpg"
  #a<-(TW_obj[TW_obj$image_path==i & TW_obj$method=="YOLOS" & TW_obj$score>0.5,"label"])
  b<-unique(TW_obj[TW_obj$image_path==i & TW_obj$method=="YOLOS" & TW_obj$score>0.75,"label"])
  lab_YOLOS<-c(lab_YOLOS,b)
  print(i)
}
lab_YOLOS<-lab_YOLOS[-1]
Becky_text <- Corpus(VectorSource(lab_YOLOS))
mywordsdf<-preprocess_corpus (Becky_text)

png("after setting the pipeline wordcloud/TW_image description YOLOS 0.75.png", width = 12, height = 10, units = "in", res = 300)

wordcloud (words = mywordsdf$word, freq = log(mywordsdf$freq)
           ,min.freq = 0 ,max.words=200 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(2, 0.5))  
dev.off()

### RE-DO ANALYSES WITHOUTH NOT REAL WORLD IMAGES #############################
##### THIS PROCEDURE IS MORE EFFECTIVE TO IDENTIFY IMAGES WE DO NOT NEED, SEARCHING FOR OBJECTS/words ALREADY IDENTIFIED ####################

TW_obj<-read.csv("TW_object_detection_results_long_format.csv")
load("WS_becky_folder-path.Rda")


a<-unique(TW_obj[grepl("logo",TW_obj$text),"image_path"])
a1<-TW_obj[TW_obj$image_path %in% a, ]
sum(na.omit(a1$score)>=0.7) 
###共有 207 个与 "logo" 相关的目标检测结果，并且其中有 207 个标签的得分大于等于 0.7


### these words log do not refer (generally) to real images 
TW_obj[grepl("advertisement",TW_obj$text),]
TW_obj[grepl("advertising",TW_obj$text),]
TW_obj[grepl("album",TW_obj$text),]
TW_obj[grepl("app",TW_obj$text),]
TW_obj[grepl("article",TW_obj$text),]
TW_obj[grepl("audi",TW_obj$text),]
TW_obj[grepl("beyonce",TW_obj$text),] ##查找TW_obj$text中是否包含了"beyonce"这个关键词，返回一个逻辑向量
TW_obj[grepl("bible",TW_obj$text),]
TW_obj[grepl("cancer",TW_obj$text),]
TW_obj[grepl("nirvana",TW_obj$text),]
TW_obj[grepl("birmingham",TW_obj$text),]
TW_obj[grepl("map",TW_obj$text),]
TW_obj[grepl("aireonais",TW_obj$text),]
TW_obj[grepl("poster",TW_obj$text),] 
TW_obj[grepl("camilla",TW_obj$text),]
TW_obj[grepl("charles",TW_obj$text),]
TW_obj[grepl("cardiff",TW_obj$text),]
TW_obj[grepl("nintendo",TW_obj$text),]
TW_obj[grepl("cuzyyretchin",TW_obj$text),]
TW_obj[grepl("dontdeff",TW_obj$text),]
TW_obj[grepl("newspaper",TW_obj$text),]
TW_obj[grepl("advertisement",TW_obj$text),]
TW_obj[grepl("eurovision",TW_obj$text),]
TW_obj[grepl("event",TW_obj$text),] 
TW_obj[grepl("screenshot",TW_obj$text),]
TW_obj[grepl("festival",TW_obj$text),]
TW_obj[grepl("flyer",TW_obj$text),]
TW_obj[grepl("film",TW_obj$text),]
TW_obj[grepl("gaga",TW_obj$text),]
TW_obj[grepl("germany",TW_obj$text),]
TW_obj[grepl("gin",TW_obj$text),]
TW_obj[grepl("google",TW_obj$text),]
TW_obj[grepl("gps",TW_obj$text),]
TW_obj[grepl("greyhounds",TW_obj$text),]
TW_obj[grepl("gtr",TW_obj$text),]
TW_obj[grepl("toyota",TW_obj$text),]
TW_obj[grepl("gun",TW_obj$text),]
TW_obj[grepl("hot",TW_obj$text),]
TW_obj[grepl("screen shot",TW_obj$text),]
TW_obj[grepl("party",TW_obj$text),]
TW_obj[grepl("hydra",TW_obj$text),]
TW_obj[grepl("icons",TW_obj$text),]
TW_obj[grepl("images",TW_obj$text),]
TW_obj[grepl("inch",TW_obj$text),]
TW_obj[grepl("laptop",TW_obj$text),] ## this is controvrtial, there are some real images though
TW_obj[grepl("inch",TW_obj$text),]
TW_obj[grepl("intel core",TW_obj$text),]
TW_obj[grepl("iphone",TW_obj$text),]
TW_obj[grepl("leonard",TW_obj$text),]
TW_obj[grepl("liverpool",TW_obj$text),]
TW_obj[grepl("logos",TW_obj$text),]
TW_obj[grepl("london",TW_obj$text),]
TW_obj[grepl("magazine",TW_obj$text),]
TW_obj[grepl("match",TW_obj$text),] ## there are no matches in the WS
TW_obj[grepl("message",TW_obj$text),]
TW_obj[grepl("michael",TW_obj$text),]
TW_obj[grepl("movie",TW_obj$text),]
TW_obj[grepl("nhs",TW_obj$text),]
TW_obj[grepl("nick",TW_obj$text),]
TW_obj[grepl("nokia",TW_obj$text),]
TW_obj[grepl("paypal",TW_obj$text),]
TW_obj[grepl("pfj",TW_obj$text),]
TW_obj[grepl("podcast",TW_obj$text),]
TW_obj[grepl("portland",TW_obj$text),]
TW_obj[grepl("queen",TW_obj$text),]
TW_obj[grepl("readslearn",TW_obj$text),]
TW_obj[grepl("readsthe",TW_obj$text),]
TW_obj[grepl("readsthere",TW_obj$text),]
TW_obj[grepl("referee",TW_obj$text),]
TW_obj[grepl("screenshote",TW_obj$text),]
TW_obj[grepl("shark",TW_obj$text),]
TW_obj[grepl("shas",TW_obj$text),]
TW_obj[grepl("shmyatscho",TW_obj$text),]
TW_obj[grepl("simpsons",TW_obj$text),]
TW_obj[grepl("skull",TW_obj$text),]
TW_obj[grepl("sneakers",TW_obj$text),]
TW_obj[grepl("soccer",TW_obj$text),]
TW_obj[grepl("soldiers",TW_obj$text),]
TW_obj[grepl("sony",TW_obj$text),]
TW_obj[grepl("spell",TW_obj$text),]
TW_obj[grepl("soccer",TW_obj$text),]
TW_obj[grepl("starfish",TW_obj$text),]
TW_obj[grepl("suit",TW_obj$text),] 
TW_obj[grepl("starfish",TW_obj$text),]
TW_obj[grepl("yoga",TW_obj$text),]
TW_obj[grepl("yin",TW_obj$text),]
TW_obj[grepl("yoo",TW_obj$text),]
TW_obj[grepl("microphones",TW_obj$text),]
TW_obj[grepl("anemone",TW_obj$text),]
TW_obj[grepl("tweet",TW_obj$text), ] 
TW_obj[grepl("yeah",TW_obj$text), ] 

words_MEME <- c(
  
  "microphones","words","avatar","coaster", "boxing","wwe","wrestler","wrestling","yoo","yoga","advertisement", "advertising", "album", "app", "article", "audi", "beyonce", "bible", "cancer",
  "nirvana", "birmingham", "map", "aireonais", "poster", "camilla", "charles", "cardiff", "nintendo",
  "cuzyyretchin", "dontdeff", "newspaper", "advertisement", "eurovision", "event", "screenshot",
  "festival", "flyer", "film", "gaga", "germany", "gin", "google", "gps", "greyhounds", "gtr",
  "toyota", "gun", "hot", "screen shot", "party", "hydra", "icons", "images", "inch", "laptop",
  "inch", "intel core", "iphone", "leonard", "liverpool", "logos", "london", "magazine", "match",
  "message", "michael", "movie", "nhs", "nick", "nokia", "paypal", "pfj", "podcast", "portland",
  "queen", "readslearn", "readsthe", "readsthere", "referee", "screenshote", "shark", "shas","tweet",
  "shmyatscho", "simpsons", "skull", "sneakers", "soccer", "soldiers", "sony", "spell", "soccer","xbox","colorful striped wallpaper",
  "starfish", "suit", "starfish", "logo","wasi07", "zame","ufc","uks","und","tweet","twitter","tuesday","albert","anemone"
) 

a <- TW_obj[grepl(paste(words_MEME, collapse = "|"), TW_obj$text), ]

###TAKING ONLY BLIP FOR NOW. IS THE ONE I TRUST THE MOST 
wordfreq_WS<-(WS_becky[ WS_becky$method=="BLIP",]) ##选择了使用 BLIP 方法的结果
wordfreq_WS<-(wordfreq_WS[!duplicated(wordfreq_WS$image_path) ,]) ##选取唯一单一的图片，去除重复的图像描述
length(unique(WS_becky$image_path)) ## i get 640  ### the size match
length(unique(wordfreq_WS$image_path)) ## i get 640  ### the size match
##验证了在选择唯一图像描述后数据的大小匹配，确保没有重复的图像描述，确保在选择唯一的图像描述后，数据集的一致性。

words_MEME <- paste(words_MEME, collapse = "|")

### 删除非真实世界图片的描述
wordfreq_WS_pre <- wordfreq_WS[!grepl(words_MEME, wordfreq_WS$caption) ,]
dim(wordfreq_WS_pre)
## FROM 640 IT DROPS TO  553 

wordfreq_TW<-(TW_obj[ TW_obj$method=="Caption_blip",]) ##选择使用 Caption_blip 方法的结果，并将结果存储在 wordfreq_TW 变量中
wordfreq_TW<-(wordfreq_TW[!duplicated(wordfreq_TW$image_path) ,]) ##选取唯一单一的图片，去除重复的图像描述
length(unique(TW_obj$image_path)) ## i get 7175 
length(unique(wordfreq_TW$image_path)) ## i get 7175
###两个数据集中的图像路径是相同且唯一的

#从 wordfreq_TW 数据集中删除包含在 words_MEME 中定义的关键词的文本行
wordfreq_TW_pre <- wordfreq_TW[!grepl(words_MEME, wordfreq_TW$text) ,]
dim(wordfreq_TW_pre)
## FROM 7175 IT DROPS TO 5039

##上述代码已经删除非现实世界相关的部分
##现在合并两个数据集并且处理同义词
wordfreq_WS= process_corpus_words(VectorSource(wordfreq_WS_pre[,c("caption")])) 
##函数将处理 'caption' 列中的文本，并返回一个数据框（mywordsdf）包含单词频率
wordfreq_TW= process_corpus_words(VectorSource(wordfreq_TW_pre[,c("text")])) 
##函数将处理 'text' 列中的文本，并返回一个数据框（mywordsdf）包含单词频率

sortedAllAlph<-sort(unique(c(wordfreq_WS$word ,wordfreq_TW$word)))

combined_word_freq<- as.data.frame(cbind(sortedAllAlph, freq_TW=rep(0),freq_WS=rep(0)))

### adding freq of the words 

for (word in combined_word_freq$sortedAllAlph) {
  if (word %in% wordfreq_TW$word) {
    combined_word_freq[combined_word_freq$sortedAllAlph == word, "freq_TW"] <- wordfreq_TW[wordfreq_TW$word == word, "freq"]
  }
  if (word %in% wordfreq_WS$word) {
    combined_word_freq[combined_word_freq$sortedAllAlph == word, "freq_WS"] <- wordfreq_WS[wordfreq_WS$word == word, "freq"]
  }
}

combined_word_freq$freq_TW <- as.numeric(combined_word_freq$freq_TW)
combined_word_freq$freq_WS <- as.numeric(combined_word_freq$freq_WS)

print(combined_word_freq)

## NOW MERGE DATASETS AND THEN THE SYNONYMS
colsums_result <- colSums(combined_word_freq[, 2:3])
print(colsums_result)

### now we can merge duplicates ## eg. play, playing 
# Apply the aggregation function to each word
aggregated_words <- sapply(combined_word_freq$sortedAllAlph, aggregate_words) ### this is the hand made one
combined_word_freq$aggregated_words<-aggregated_words ### ok this work, however, for example across became acros

### snowball methods for collapsing words to a common route ### advances iin stemming
###词干化
combined_word_freq$clustered_porter<-wordStem(combined_word_freq$sortedAllAlph, language = "porter") 
combined_word_freq$clustered_eng<-wordStem(combined_word_freq$sortedAllAlph, language = "english")

apply(combined_word_freq[,c("aggregated_words","clustered_porter","clustered_eng")],2, function(x){length(unique(x))} )
apply(combined_word_freq[,c("aggregated_words","clustered_porter","clustered_eng")],2, function(x){sum(duplicated(x))} ) ## i have more duplicated in the clustered eng 

### check the best aggregator 
dupl=(combined_word_freq[duplicated(combined_word_freq$clustered_eng),"clustered_eng"]) 
unique(dupl) 
##rapid check 
a<-combined_word_freq[(combined_word_freq$clustered_eng) %in% dupl,]

combined_word_freq$FINAL_WORDS<-combined_word_freq$sortedAllAlph 

### First duplicate the original 
### THIS WAS NOT WORKING I NEED THE COMPLETE COPY
combined_word_freq [(combined_word_freq$clustered_eng) %in% a$clustered_eng,"FINAL_WORDS"]<-combined_word_freq[(combined_word_freq$clustered_eng) %in% a$clustered_eng,"clustered_eng"] ## for the duplicated take them both. 

## aggregate frequencies 
aggregated_TW <- aggregate(freq_TW ~ FINAL_WORDS, combined_word_freq, sum) 

length(unique(combined_word_freq$FINAL_WORDS))
dim(combined_word_freq)
dim(combined_word_freq)[1]-length(unique(dupl) ) 
aggregated_WS <- aggregate(freq_WS ~ FINAL_WORDS, combined_word_freq, sum)

#### MERGING THESE TWO datasets, they have the meme words removed, compiled frequencies, then "synonyms"
aggregated_word_frequencies<-merge(aggregated_WS,aggregated_TW)
length(unique(aggregated_word_frequencies$FINAL_WORDS))
# Calculate normalized word frequencies
aggregated_word_frequencies$freq_WS_NORM <- aggregated_word_frequencies$freq_WS / dim(wordfreq_WS_pre)[1]
aggregated_word_frequencies$freq_TW_NORM <- aggregated_word_frequencies$freq_TW / dim(wordfreq_TW_pre)[1] 

#### HERE ALL PLURALS AND SINGULARS AS WELL AS HAVE BEEN MERGED. 
###  HOWEVER 

#### FIRST EXPLORATION FOR ONLY OBSERVED FREQUENCIES
aggregated_word_frequencies[,2:5]<- apply(aggregated_word_frequencies[,2:5],2,as.numeric)

# Perform chi-squared test
chi2_test <- chisq.test(aggregated_word_frequencies[aggregated_word_frequencies$freq_WS>2 & aggregated_word_frequencies$freq_TW>2,c("freq_TW","freq_WS")])

# Print the test result
print(chi2_test) 
##该卡方检验结果表明 "freq_TW" 和 "freq_WS" 的频率分布在统计上存在显著差异，
##它们之间不是完全相同的，可能在某些方面存在显著性差异

##选取满足 "freq_WS" 和 "freq_TW" 值都大于2的行，并提取这些行中 "freq_WS_NORM" 列的数据
aggregated_word_frequencies[aggregated_word_frequencies$freq_WS>2 & aggregated_word_frequencies$freq_TW>2,c("freq_WS_NORM", "freq_TW_NORM")]
# 绘制直方图并修改标题
hist(aggregated_word_frequencies[aggregated_word_frequencies$freq_WS>2 & aggregated_word_frequencies$freq_TW>2,c("freq_WS_NORM", "freq_TW_NORM")], 
     main = "Histogram of Freq_WS&TW_NORM", 
     xlab = "freq_WS&TW_NORM > 2", ylab = "Frequency")

wilcox.test(aggregated_word_frequencies[aggregated_word_frequencies$freq_WS>2 & aggregated_word_frequencies$freq_TW>2,c("freq_WS")],aggregated_word_frequencies[aggregated_word_frequencies$freq_WS>2 & aggregated_word_frequencies$freq_TW>2,c("freq_TW")], paired=T)


dim(aggregated_word_frequencies[aggregated_word_frequencies$freq_WS>0,])

dim(aggregated_word_frequencies[aggregated_word_frequencies$freq_TW>0,])

##数据的稀疏度度量
dim(wordfreq_TW_pre)[1] 
##这个比例表示wordfreq_TW_pre数据框中非零值占所有值的比例
dim(wordfreq_WS_pre)[1]
