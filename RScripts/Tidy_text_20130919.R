#install.packages("tidytext","widyr","tidyr","dplyr","ggplot2","wordcloud","reshape2","widyr","SnowballC")
#Ref:  Julia Silge and David Robinson, Text Mining with R: A Tidy Approach, https://www.tidytextmining.com/index.html
install.packages("textdata")
#For sentiment analysis

# 1. READING AND CONVERTING TEXT DATA INTO TIDY FORMAT
library(dplyr)
#A powerful library with select, filter, mutate, arrange, join functions for tidytext, https://www.rdocumentation.org/packages/dplyr/versions/0.7.8
#A tibble is a modern class of data frame within R, available in the dplyr
sd <- read.table("slumdog.txt",encoding = "UTF-8") #Reading text data file into R workspace, Unicode (or Universal Coded Character Set) Transformation Format – 8-bit.
View(sd)
colnames(sd)[1] <- 'text' #renaming review column as text to use 'tm'
sd$text1 = as.character(sd$text) #converts text to charatcter type
sdtidy<-tibble(line=1:nrow(sd),text=sd$text1) 
# converts R table to dataframe tibble
View(sdtidy)

library(tidytext)
#tidy text format is a table with one-token-per-row; works with packags such as dplyr,tidyr,ggplot2 etc
sdtidy1 <- sdtidy %>% unnest_tokens(word,text)
#https://www.rdocumentation.org/packages/tidytext/versions/0.3.0/topics/unnest_tokens
View(sdtidy1)
sdtidy1 %>% count(word,sort=TRUE)
# %>% : pipe operator

# 2. STOP-WORDS
data(stop_words) #loads stop_words from tidytext library
View(stop_words)
sdtidy2<-sdtidy1 %>% anti_join(stop_words) #using pipe operator %>%, left entity, typically a table, is input to the right function; anti_join removes common rows
sdtidy2 %>% count(word,sort=TRUE) #counts a column and display

# 2.1 Adding Stop-Words
custom_stop_words <- bind_rows(data_frame(word=c("film","movie"),lexicon=c("custom")),stop_words) #inserting two rows into stop_words and creating it as a seprate table
sdtidy3<-sdtidy1 %>% anti_join(custom_stop_words)
sdtidy3 %>% count(word,sort=TRUE) 

# 2.2 Removing Stop-Words
custom_stop_words<- filter(custom_stop_words,word!="i")
sdtidy4<-sdtidy1 %>% anti_join(custom_stop_words)
sdtidy4 %>% count(word,sort=TRUE) 
# 3. STEMMING
library(SnowballC) #for stemming function
sdtidy3 %>% mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE)
#https://smltar.com/stemming.html
#mutate() in deplyr adds new variables and preserves existing ones;
#love count increases from 20 to 35

# 4. PLOTTING FREQUENT WORDS (without stemming)
library(ggplot2)
#An elegant graphical tool in R 
sdtidy3 %>% count(word,sort=TRUE) %>% filter(n>20) %>%
  mutate(word=reorder(word,n)) %>%  # reorder (generic function) word based on valu of n. Mutate (deplyr) creates a variable based on existing variables
  ggplot(aes(word,n)) +geom_col() +xlab("Words")+ylab("Frequency")+coord_flip()
#aesthetics, aes(x,y); type of graph, geom; geom_col() meaning height proportional to y value, xlab and ylab axes labels, and graph flipped
#count function creates a new column "n"

# 4.1 Wordcloud
library(wordcloud)
#For creating worldcloud from text
sdtidy3 %>% count(word,sort=TRUE) %>% with(wordcloud(word,n,max.words=50))

# 5. IMPORTANCE OF WORDS(tf-idf)
sdtidy1_words <- sdtidy1 %>% count(word,sort=TRUE) #creates n column on sdtidy1
#ggplot(sdtidy1_words,aes(n/sum(n))) + geom_histogram(show.legend=FALSE) + xlim(NA,0.009)
#View(sdtidy1_words); n: particular word count; sum(n): total number of words
#graph shows large proportion of infrequent words; long tailed distribution 

# 5.1 Zipf's Law
tf_rank <- sdtidy1_words %>% mutate(rank=row_number(),tf=n/sum(n))
#row number is same as rank as words have been sorted 
View(tf_rank)
ggplot(tf_rank,aes(rank,tf)) +geom_line(size=1.1,alpha=0.8,show.legend=FALSE) + scale_x_log10() +scale_y_log10()
#grah will show flat starting top line indicating many commonly used words, known as Zipf's law
sdtidy5<-sdtidy1  %>% group_by(line) %>% count(word,sort=TRUE) %>% ungroup() 
#Here each line contains a thread/post in a discussion; a line is thus a document      
sd_tf_idf<-sdtidy5 %>% bind_tf_idf(word,line,n) %>% arrange(desc(tf_idf)) #bind_tf_idf is a tidytext function, inserts idf and tf-idf columns
View(sd_tf_idf)
#tf is specific to a document (n/sum(n) of a document); idf=log(N/nt); N -no of docs; nt, number of documents where the term appears

# 6. BIGRAM ANALYSES
sdbigram <- sdtidy %>% unnest_tokens(bigram,text,token="ngrams",n=2) #bigram will be variable name, unnest will be ngram tyoe with n=2
#https://www.rdocumentation.org/packages/tidytext/versions/0.3.0/topics/unnest_tokens
sdbigram
sdbigram %>% count(bigram,sort=TRUE) # many common words like "of the"

# 6.1 Counting Co-Occurences
library(tidyr)
#Enables n-gram manipulation using functions such as separate, unite, unnest 
sdbigram_split<-sdbigram %>% separate(bigram,c("word1","word2"),sep=" ") #separats pairs into individual features, separate is a 
sdbigram_split_filter<-sdbigram_split %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
#Removes stop-words from each feature
#https://www.tutorialspoint.com/r/r_operators.htm, %in% retains row values not in LHS, in RHS
sdbigram_split_filter %>% count(word1,word2,sort=TRUE)
sdbigram_join_filter <- sdbigram_split_filter %>% unite(bigram,word1,word2,sep=" ") #Unite words into pairs
sdbigram_join_filter %>% count(bigram,sort=TRUE)

#Checks the associate of an unparliamenmtary word!
sdbigram_split_filter %>% filter(word2=="shit") %>% count(line,word1,sort=TRUE) #types of shit in different documents, [street] 

# 6.2 Pair-Wise Correlation
# here words need not be in sequence, as in ngram
library(widyr)
#For pairwise correlations; (how many times if X then Y in different documents)
pairs<- sdtidy2 %>% pairwise_count(word,line,sort=TRUE)
pairs
pairs %>% filter(item1=="love") #items that pairs with love, similar to document wise count for 'shit'
pair_corr<- sdtidy2 %>% group_by(word) %>% filter(n()>=10) %>% pairwise_cor(word,line,sort=TRUE)
pair_corr # phi-coefficient
pair_corr %>% filter(item1=="danny")

# 7. SENTIMENT ANALYSIS
#[install and include "textdata" for sentiment lexicons, if required. For this environment, "tidytext" works]
nrcjoy<-get_sentiments("nrc") %>% filter(sentiment=="anger")
sdtidy2 %>% inner_join(nrcjoy) %>% count(word,sort=TRUE)
#uses nrc lexicon and lists words pertaining to anger
afinn<- sdtidy2 %>% inner_join(get_sentiments("afinn")) %>% group_by(line)
afinn
#document wise sentiment scores for words based on afinn lexicon (-5 to +5)
afinn_overall<- sdtidy2 %>% inner_join(get_sentiments("afinn")) %>%summarise(sentiment=sum(value))
afinn_overall

wordbysenti <- sdtidy2 %>% inner_join(get_sentiments("bing")) %>% 
  count(word,sentiment,sort=TRUE)
wordbysenti # uses bing lexicon to classify words into positive and negative and count them

wordbysenti %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
  mutate(word=reorder(word,n)) %>% ggplot(aes(word,n))+
  geom_col()+facet_wrap(~sentiment,scales="free_y")+coord_flip()
# plots top 10 +ve and -ve words; facet_wrap generates two grids based on sentiment positive and negative

# 7.1 Comparative Wordcloud
library(reshape2)
sdtidy2 %>% inner_join(get_sentiments("bing")) %>% 
  count(word,sentiment,sort=TRUE) %>% acast(word~sentiment,value.var="n",fill=0) %>% 
  comparison.cloud(colors=c("#F8766D", "#00BFC4"),max.words=100)

#Haven't Censored any word!







