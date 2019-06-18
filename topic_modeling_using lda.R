rm(list=ls())
library(dplyr)
library(topicmodels)
library(tidytext)
library(ggplot2)

data <- read.csv("tdm.csv", stringsAsFactors = F, row.names = 1)
data <- readr::read_csv("tdm.csv")

data1 <- t(data)
rownames(data1) <- NULL

dim(data)

a <- apply(data1,1,sum)
data2 <- data1[-which(a == 0),]
rm(data1)

set.seed(12345) 
sampling <- sample(1:as.integer(nrow(data2)*0.8),replace = FALSE)
train_data <- data2[sampling,]
test_data <- data2[-sampling,]


perplexity_df <- data.frame(train=numeric(), test=numeric())
topics <- c(2:30)
burnin = 100
iter = 1000
keep = 50

set.seed(12345)
for (i in topics){
  
  lda_result <- LDA(train_data, k = i, method = "Gibbs",
                control = list(burnin = burnin, iter = iter, keep = keep) )
  perplexity_df[i,1] <- perplexity(lda_result, newdata = train_data)
  perplexity_df[i,2]  <- perplexity(lda_result, newdata = test_data) 
}

#perplexity 시각화
per_vis <- ggplot(data=perplexity_df, aes(x= as.numeric(row.names(perplexity_df)))) + labs(y="Perplexity",x="Number of topics") + ggtitle("Perplexity of hold out  and training data")

per_vis <- per_vis + geom_line(aes(y=test), colour="red")
per_vis <- per_vis + geom_line(aes(y=train), colour="green")
per_vis


#정한 topic 개수로 토픽모델링
set_topic_lda <- LDA(data2, k=15, seed=12345)
topics <- tidy(set_topic_lda, matrix='beta')
top_terms <- topics %>% group_by(topic) %>% top_n(20, beta) %>% ungroup() %>% arrange(topic, -beta)


#시각화
top_terms %>% filter(topic == 1 | topic == 2 | topic == 3 | topic == 4 | topic == 5) %>% 
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ topic, scales="free") +
  coord_flip() +
  theme(axis.text.y=element_text(family=""))

top_terms %>% 
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ topic, scales="free") +
  coord_flip() +
  theme(axis.text.y=element_text(family=""))
