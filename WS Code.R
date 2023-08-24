
### set directory & load packages & functions ##############
## directory
setwd("/Users/hermionehe/Desktop/项目文件")

## packages 
library(leaflet)
library(sf)
library(geosphere)
library(tidytext)
library(vader)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(topicmodels)
library(RColorBrewer)
library(reticulate)
library(dplyr)
library(stringr)
library(ggplot2)
library(NLP)




### function pre-process wordcloud 
preprocess_corpus <- function(text) {
  
  corpus <- tm_map(text, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  mytextdata <- TermDocumentMatrix (corpus)
  mtdmatrix <- as.matrix (mytextdata)
  mywords <- sort( rowSums( mtdmatrix ),decreasing=TRUE)
  mywordsdf <- data.frame (word = names (mywords ),freq=mywords)
  set.seed( 412 )# for reproducibility
  return(mywordsdf)
} 


### UPLOADING THE IMAGE DESCRIPTIONS #################
## For now it is all we have at some point I will need to upload them in a loop 
WS_becky<-read.csv("WS_image_analysis_results_long_format.csv")

### this now have the new format 
## EXTRACTING FILENAME 
### MKAE THE NAMES COMPARABLE GETTING ONLY THE FILENAME
WS_becky$file_list<-rep(NA) ## the new column of the file name in R
WS_becky$Folder<-rep(NA) ## the new column of the file name in R

###提取文件名和文件夹名，并将它们存储在数据框WS_becky的相应列中的每一行。
for (i in 1:dim(WS_becky)[1]) {
  #i=1
  
  # Example string
  my_string <- as.character(WS_becky$image_path[i])
  
  # Extract the letters, numbers, and symbols between the last '/' and '.' in the string (excluding '/')
  #pattern <- "/([^/]+\\.[A-Za-z0-9]+)$"  # updated regular expression pattern
  #matches <- regmatches(my_string, regexpr(pattern, my_string))
  #result <- ifelse(length(matches) > 0, sub("^/(.*)$", "\\1", matches[[1]]), "")
  
  # Print the result
  #print(result)
  extracted_text <- str_extract(my_string, "(?<=/)[^/]+(?=/[^/]+$)")
  file_name <- str_extract(my_string, "(?<=\\\\)[^\\\\]+$")
  folder_name <- str_extract(my_string, "(?<=\\\\)[^\\\\]+(?=\\\\[^\\\\]+$)")
  
  print(file_name)
  WS_becky[i,"file_list"]<-file_name
  WS_becky[i,"Folder"]<-folder_name
}

## as we know some objects are repeated and this can make analyses more difficult 
### save this for performing the chi square in twitter script
save(WS_becky,file="WS_becky_folder-path.Rda")

# Create a function to preprocess the text
preprocess_text <- function(text) {
  # Convert to lowercase
  text <- tm_map(text, tolower)
  
  # Remove punctuation
  text <- tm_map(text, removePunctuation)
  
  # Remove numbers
  text <- tm_map(text, removeNumbers)
  
  # Remove stopwords
  text <- tm_map(text, removeWords, stopwords("english"))
  
  # Strip whitespace
  text <- tm_map(text, stripWhitespace)
  
  return(text)
}

# Create a function to create the corpus
create_corpus <- function(data) {
  # Combine "text" and "caption" columns into a single text column
  text <- paste(data$text, data$caption, sep = " ")
  
  # Create a temporary data frame
  temp_df <- data.frame(text)
  
  # Convert the text to Corpus with exception handling
  tryCatch({
    corpus <- Corpus(VectorSource(temp_df$text))
    corpus <- preprocess_text(corpus)
  }, error = function(e) {
    # Print the error message
    print(paste("Error:", e$message))
    
    # Return an empty corpus
    corpus <- Corpus(VectorSource(character(0)))
  })
  
  return(corpus)
}
# Create the corpus using exception handling
Becky_text <- create_corpus(WS_becky)

##### WORDCLOUD BOTH DESCRIPTIONS ######
## Start wordcloud text caption 
mywordsdf<-preprocess_corpus (Becky_text)

# Save the plot as a PNG file
png("after setting the pipeline wordcloud/WS_image description blip and gpt.png", width = 8, height = 6, units = "in", res = 300)

# Code to generate the word cloud plot goes here

wordcloud (words = mywordsdf$word, freq = mywordsdf$freq
           ,min.freq = 0 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(3, 0.6))  

dev.off()

### LABELS NOW ##########

## First take the unique of labels agreement ######
lab_agree<-c("a")
for (i in unique(WS_becky$image_path)) {
  #  i=unique(WS_becky$image_path)[4]
  a<-(WS_becky[WS_becky$image_path==i & WS_becky$method=="YOLOS" & WS_becky$score>0.5,"label"])
  b<-(WS_becky[WS_becky$image_path==i & WS_becky$method=="Faster R-CNN" & WS_becky$score>0.5,"label"])
  lab_agree<-c(lab_agree,intersect (a,b))
  
}
lab_agree<-lab_agree[-1]
Becky_text <- Corpus(VectorSource(lab_agree))
mywordsdf<-preprocess_corpus (Becky_text)

png("after setting the pipeline wordcloud/WS_image description labels 0.5 agreement.png", width = 8, height = 6, units = "in", res = 300)

wordcloud (words = mywordsdf$word, freq = log(mywordsdf$freq)
           ,min.freq = 0 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(3, 0.6))  ### ok it worked also with my fake dataset.

dev.off()

#### LABELS >0.75 FASTER

lab_FAST<-c("a")
for (i in unique(WS_becky$image_path)){
  #  i="/content/drive/MyDrive/Colab Notebooks/Becky Govanhill Photos/g.1.3 mary_0016.jpg"
  #a<-(WS_becky[WS_becky$image_path==i & WS_becky$method=="YOLOS" & WS_becky$score>0.5,"label"])
  b<-unique(WS_becky[WS_becky$image_path==i & WS_becky$method=="Faster R-CNN" & WS_becky$score>0.75,"label"])
  lab_FAST<-c(lab_FAST,b)
  
}
lab_FAST<-lab_FAST[-1]
Becky_text <- Corpus(VectorSource(lab_FAST))
mywordsdf<-preprocess_corpus (Becky_text)

png("after setting the pipeline wordcloud/WS_image description Faster CNN 0.75.png", width = 8, height = 6, units = "in", res = 300)

wordcloud (words = mywordsdf$word, freq = log(mywordsdf$freq)
           ,min.freq = 0 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(3, 0.6))  ### ok it worked also with my fake dataset.
dev.off()

### now with YOLOS-small
lab_YOLOS<-c("a")
for (i in unique(WS_becky$image_path)) {
  #  i="/content/drive/MyDrive/Colab Notebooks/Becky Govanhill Photos/g.1.3 mary_0016.jpg"
  #a<-(WS_becky[WS_becky$image_path==i & WS_becky$method=="YOLOS" & WS_becky$score>0.5,"label"])
  b<-unique(WS_becky[WS_becky$image_path==i & WS_becky$method=="YOLOS" & WS_becky$score>0.75,"label"])
  lab_YOLOS<-c(lab_YOLOS,b)
  
}
lab_YOLOS<-lab_YOLOS[-1]
Becky_text <- Corpus(VectorSource(lab_YOLOS))
mywordsdf<-preprocess_corpus (Becky_text)

png("after setting the pipeline wordcloud/WS_image description YOLOS 0.75.png", width = 8, height = 6, units = "in", res = 300)

wordcloud (words = mywordsdf$word, freq = log(mywordsdf$freq)
           ,min.freq = 0 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2"),scale = c(3, 0.6))  ### ok it worked also with my fake dataset.

dev.off()

install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)

# Calculate label frequencies
label_frequencies <- table(WS_becky$label)

# Sort the frequencies in descending order
sorted_frequencies <- sort(label_frequencies, decreasing = TRUE)

# Create a summary table with unique labels
summary_table <- data.frame(labels = unique(names(sorted_frequencies)), frequencies = sorted_frequencies)

# Format the table using kable()
formatted_table <- kable(summary_table, align = "c", caption = "Label Frequencies") %>%
  kable_styling()

# Print the formatted table
print(formatted_table)


    