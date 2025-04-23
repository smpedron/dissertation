## Structural Topic Model, Redpilling Paper (Prolific Sample)
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())
source("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/codes/rp_datacleaner.r")

##### Structural Topic Model #####

## Question: How do you feel about the idea of living in a community where most people share similar racial backgrounds?

topic_model <- rp_data
topic_model$open_ended <- ifelse(topic_model$open_ended == "", NA, topic_model$open_ended)
topic_model <- topic_model %>% 
  filter(!is.na(open_ended))

library(stm)
set.seed(5456231)

## Removing phrases
topic_model$open_ended <- gsub("i think", "", tolower(topic_model$open_ended))
topic_model$open_ended <- gsub("like", "", tolower(topic_model$open_ended))
topic_model$open_ended <- gsub("i don't know", "", tolower(topic_model$open_ended))

## Processing data
process <- textProcessor(topic_model$open_ended, metadata = topic_model)

## Document output
output <- prepDocuments(process$documents, process$vocab, process$meta, 
                        lower.thresh = 66) # Take words that are included in at least 20 responses as they may be more meaningful

docs <- output$documents
vocab <- output$vocab
meta <- output$meta

## Searching for right number of topics
topicnumbers <- c(4, 6, 8, 10) 
topicresult <- searchK(docs,vocab, topicnumbers, 
                       prevalence =~ race_commitment + exp_condition, 
                       data = meta)


plot(topicresult) # going with 6 topics

## Topic model
topic_model_community <- stm(output$documents, output$vocab, K = 6, data = output$meta, init.type = "Spectral", seed = 5456231)

## Validating
## 1: checking word prevalence
labelTopics(topic_model_community, n = 6)
plot.STM(topic_model_community, type = "summary", labeltype="prob") # prevalence of topics
# 6, 2, 4, 5, 1, 3

## 2: Reading examples 
findThoughts(topic_model_community, texts = substr(topic_model$open_ended, 1, 400)[as.numeric(names(output$documents))], 
             n = 10, topics = 1)$docs[[1]] # comfortable but limited personal and social growth
findThoughts(topic_model_community, texts = substr(topic_model$open_ended, 1, 400)[as.numeric(names(output$documents))], 
             n = 20, topics = 2)$docs[[1]] # strong preference for diversity
findThoughts(topic_model_community, texts = substr(topic_model$open_ended, 1, 400)[as.numeric(names(output$documents))], 
             n = 20, topics = 3)$docs[[1]] # indifferent as long as certain conditions are met: law abiding citizens, respectful, neighborhood isn't fully white
findThoughts(topic_model_community, texts = substr(topic_model$open_ended, 1, 400)[as.numeric(names(output$documents))], 
             n = 15, topics = 4)$docs[[1]] # Against homogeneity for cultural exchange
findThoughts(topic_model_community, texts = substr(topic_model$open_ended, 1, 400)[as.numeric(names(output$documents))], 
             n = 15, topics = 5)$docs[[1]] # Comfort and belonging in homogeneous communities
findThoughts(topic_model_community, texts = substr(topic_model$open_ended, 1, 400)[as.numeric(names(output$documents))], 
             n = 15, topics = 6)$docs[[1]] # prefer it because it promotes community


## Topic prevalence based on race commitment
topic_labels <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6")

## Effects Estimate
estimate1 <- estimateEffect(1:6 ~ race_commitment, topic_model_community, metadata = meta, uncertainty = "Global")
summary(estimate1) # no sig diff

estimate2 <- estimateEffect(1:6 ~ exp_condition, topic_model_community, metadata = meta, uncertainty = "Global")
summary(estimate2) # no sig diff

## plots
plot.estimateEffect(estimate1, 
                    covariate = "race_commitment", 
                    topics = 1:6, 
                    model =  topic_model_community, 
                    method = "difference", 
                    cov.value1 = "High", 
                    cov.value2 = "Low", 
                    xlab = "Change in topic likelihood when respondent has low commitment to their racial identity", 
                    main = "Difference in Topic Prevalence by Racial Identity Commitment", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.06, 0.06))

plot.estimateEffect(estimate2, 
                    covariate = "exp_condition", 
                    topics = 1:6, 
                    model = topic_model_community, 
                    method = "difference", 
                    cov.value1 = "TreatmentA-SoftSell", 
                    cov.value2 = "Control", 
                    xlab = "Change in topic likelihood when in Soft Sell (vs. Control)", 
                    main = "Topic Prevalence: Soft-Sell vs. Control", 
                    labeltype = "custom", 
                    custom.labels = topic_labels)

plot.estimateEffect(estimate2, 
                    covariate = "exp_condition", 
                    topics = 1:6, 
                    model = topic_model_community, 
                    method = "difference", 
                    cov.value1 = "TreatmentB-HardSell", 
                    cov.value2 = "Control", 
                    xlab = "Change in topic likelihood when in Hard Sell (vs. Control)", 
                    main = "Topic Prevalence: Hard-Sell vs. Control", 
                    labeltype = "custom", 
                    custom.labels = topic_labels)

plot.estimateEffect(estimate2, 
                    covariate = "exp_condition", 
                    topics = 1:6, 
                    model = topic_model_community, 
                    method = "difference", 
                    cov.value1 = "TreatmentC-ProDiversity", 
                    cov.value2 = "Control", 
                    xlab = "Change in topic likelihood when in Pro-Diversity (vs. Control)", 
                    main = "Topic Prevalence: Pro-Diversity vs. Control", 
                    labeltype = "custom", 
                    custom.labels = topic_labels)


#
