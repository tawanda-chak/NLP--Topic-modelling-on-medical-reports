#LDA topic_model on AI generated Medical reports
data<- read.delim("patient_data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8") # here can add , fileEncoding = "UTF-8"
#colnames(data)
View(data)

library(tm)
library(topicmodels)
library(LDAvis)
library("stringr")
#Install the following
library(knitr)
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable) 
# addition
library(stringr)
library(quanteda)
library(textstem)
library(stringr)
library(dplyr)

# Create a corpus from the documents
corpus <- Corpus(VectorSource(data$Signs.and.Symptoms))
# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

inspect(corpus) #to inspect the corpus

# Create a document-term matrix (DTM)
dtm <- DocumentTermMatrix(corpus)
### code to show ordered dtm 
dtm
inspect(dtm)
### end of ordered dtm


# Convert DTM to a matrix of word frequencies
word_freq <- colSums(as.matrix(dtm))
# Sort the words by frequency
sorted_word_freq <- sort(word_freq, decreasing = TRUE)
# Plot the top 20 most frequent words
n<-50
top_words <- head(sorted_word_freq, n)
sorted_word_freq
head(sorted_word_freq,9)

wordcloud(names(top_words), freq = top_words, scale=c(3,0.5), min.freq = 1, max.words = 50, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
# Plot a bar chart of word frequencies
barplot(top_words, las = 2, col = "lightblue", main = "Top Most Frequent Words", xlab = "Words", ylab = "Frequency")
### end of word frequency plots

dim(dtm)
# DTM and the metadata
sel_idx <- slam::row_sums(dtm) > 0
dtm <- dtm[sel_idx, ]
data <- data[sel_idx, ] ##Take note here

# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# number of topics
K<-5
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

ncol(dtm) # lengthOfVocab

# topics are probability distributions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms

rowSums(beta)            # rows in beta sum to 1

nrow(dtm)

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics

rowSums(theta)[1:10]     # rows in theta sum to 1

#the 10 most likely terms within the term probabilities beta of 
terms(topicModel, 5)

exampleTermData <- terms(topicModel, 10)
exampleTermData[, 1:3]

#For the next steps, we want to give the topics more 
#descriptive names than just numbers. 
#Therefore, we simply concatenate the five most likely terms of 
#each topic to a string that represents a pseudo-name for each topic.

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# visualize topics as word cloud
topicToViz <- 2 # change for your own topic of interest
topicToViz <- grep('Sickness', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

##correction of the above line
# Extract the probabilities of each of the terms
probabilities <- tmResult$terms[topicToViz,]
# Remove NA values
probabilities <- probabilities[!is.na(probabilities)]
# Check if the vector is not empty
if(length(probabilities) > 0) {
  # Sort the probabilities in decreasing order
  top40terms <- sort(probabilities, decreasing=TRUE)[1:40]
  words <- names(top40terms)
  # Visualize the terms as a word cloud
  mycolors <- brewer.pal(8, "Dark2")
  wordcloud(words, top40terms, random.order = FALSE, color = mycolors)
} else {
  cat("No valid probabilities found.")
}


####****
exampleIds <- c(4, 10, 28)
lapply(corpus[exampleIds], as.character) 

exampleIds <- c(4, 10, 28)
print(paste0(exampleIds[1], ": ", substr(content(corpus[[exampleIds[1]]]), 0, 400), '...'))
print(paste0(exampleIds[2], ": ", substr(content(corpus[[exampleIds[2]]]), 0, 400), '...'))
print(paste0(exampleIds[3], ": ", substr(content(corpus[[exampleIds[3]]]), 0, 400), '...'))


##### Alterations
# Get the number of topics
num_topics <- ncol(theta)
# Define topicNames based on the number of topics
topicNames <- paste("Topic", 1:num_topics)
# Now proceed with the rest of your code
# Get topic proportions from example documents
topicProportionExamples <- theta[exampleIds,]
####

N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)
####

# see alpha from previous model
attr(topicModel, "alpha") 
#The new alpha=0.2
topicModel2 <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))

tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # reset topicnames

#Now visualize the topic distributions in the three documents again. 
#What are the differences in the distribution structure?
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)
# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")

#to bring the topics into a certain order.
nrow(dtm) ## size of collection or rows
#Approach 1
#We sort topics according to their probability within the entire collection:
# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nrow(dtm)  # mean probabilities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order  

#or
#to 5 decimal places
soP <- sort(topicProportions, decreasing = TRUE)
paste(round(soP, 5), ":", names(soP)) 

#Approach 2
#We count how often a topic appears as a primary topic within a paragraph
#This method is also called Rank-1.
countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nrow(dtm)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)

#or inline
so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
paste(so, ":", names(so))

##Filtering documents __useless line
topicToFilter <- 5  # you can set this manually ...
# ... or have it selected by a term in the topic name (e.g. 'children')
topicToFilter <- grep('sickness', topicNames)[1] 
topicThreshold <- 0.2
selectedDocumentIndexes <- which(theta[, topicToFilter] >= topicThreshold)
filteredCorpus <- corpus[selectedDocumentIndexes]
# show length of filtered corpus
filteredCorpus



####* 
#data$Year <- paste0(substr(data$Year, 1, 4))
#View(textdata)
colnames(data)
# get mean topic proportions per decade
topic_proportion_per_year <- aggregate(theta, by = list(year = data$Year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year)[2:(K+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_year, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "topic")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
######### ****************

