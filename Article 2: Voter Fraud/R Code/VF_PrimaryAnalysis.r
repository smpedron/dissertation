## Primary Analysis File, Voter Fraud Paper (Prolific Sample)
## Conjoint Analysis + Topic Model
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())
source("C:/Users/steph/Documents/Courses/PhD/Papers/VF Stuff/R/VF_datacleaner.r")

################## AMCE/ACIE ESTIMATES ################## 

## Conjoint Design - incorporating profile restrictions in the estimation
attribute_list <- list()
attribute_list[["gender"]] <-c("Male", "Female")
attribute_list[["education"]] <- c("College degree", "High school diploma", "Less than high school")
attribute_list[["country"]] <-  c("Mexico", "USA", "China", "Germany", "Syria", "Guatemala")
attribute_list[["citizen"]] <- c("Yes", "No")
attribute_list[["englishprof"]] <- c("Fluent English", "Broken English")
attribute_list[["conviction"]] <- c("Prior conviction (not voting related)", "No prior conviction")

constraint_list <- list()
constraint_list[[1]] <- list()
constraint_list[[1]][["country"]] <- c("USA")
constraint_list[[1]][["englishprof"]] <- c("Broken English")
constraint_list[[2]] <- list()
constraint_list[[2]][["country"]] <- c("USA")
constraint_list[[2]][["citizen"]] <- c("No")

vf <- makeDesign(type='constraints', attribute.levels=attribute_list, constraints=constraint_list)


## Intra-respondent Reliability
## IRR 2 already recoded in Qualtrics so that Profile 2 in IRR2 = Profile 1 in IRR1
## New var: if not identical 0, if identical 1, then averaging over all respondents
prolific_data$irr_new <- as.integer(prolific_data$irr1 == prolific_data$irr2)
prolific_data$irr_avg <- mean(prolific_data$irr_new, na.rm = T)
overall_avg <- 0.7057257 # IRR is 0.7057257

## Basic AMCE first
## Adjusting baselines
baselines <- list()
baselines$country <- "USA"
baselines$englishprof <- "Fluent English"
basic <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen,
              data = prolific_data, respondent.id = "id", baselines = baselines, design = vf)

## AMCE and ACIEs
## Prior convict interactions
combo <- amce(Chosen ~ gender + country + education + englishprof + conviction + country*conviction + conviction*citizen, 
              data = prolific_data, respondent.id = "id", baselines = baselines, design = vf)

## Citizenship interactions
## Readjusting baselines because I can't interact citizen with USA as baseline
baselines <- list()
baselines$country <- "Guatemala"
baselines$englishprof <- "Fluent English"
citizen_combo <- amce(Chosen ~ gender + country + education + englishprof + conviction + country*citizen + education*citizen, 
                      data = prolific_data, respondent.id = "id", baselines = baselines, design = vf)


## Need to extract to adjust standard errors using IRR
extract <- summary(basic)[[1]]
extract$`Std. Err` <- extract$`Std. Err` * overall_avg
extract$lower <- extract$Estimate - 1.96 * extract$`Std. Err`
extract$upper <- extract$Estimate + 1.96 * extract$`Std. Err`
extract$Level <- factor(extract$Level, levels = extract$Level[order(extract$Attribute)])
extract$Level %<>% 
  plyr::mapvalues(
    c("Yes", "Prior conviction (not voting related)",
      "China", "Germany", "Guatemala", "Mexico", "Syria", "High school diploma", "Less than high school",
      "Broken English", "Male"),
    c("US Citizen", "Prior conviction", 
      "China", "Germany", "Guatemala", "Mexico", "Syria", "HS diploma", "Less than HS",
      "Broken English", "Male")
  ) %>%
  as.vector()
extract$Level <- factor(extract$Level, levels = extract$Level[order(extract$Attribute)])

extract2 <- summary(combo)[[1]]
extractx <- summary(combo)[[2]]
extract2 <- rbind(extract2, extractx)
extract2$`Std. Err` <- extract2$`Std. Err` * overall_avg
extract2$lower <- extract2$Estimate - 1.96 * extract2$`Std. Err`
extract2$upper <- extract2$Estimate + 1.96 * extract2$`Std. Err`
extract2$Level %<>% 
  plyr::mapvalues(
    c("Yes", "Yes:Prior conviction (not voting related)", "Prior conviction (not voting related)", "Prior conviction (not voting related):China", 
      "Prior conviction (not voting related):Germany", "Prior conviction (not voting related):Guatemala",
      "Prior conviction (not voting related):Mexico", "Prior conviction (not voting related):Syria",
      "China", "Germany", "Guatemala", "Mexico", "Syria", "High school diploma", "Less than high school",
      "Broken English", "Male"),
    c("US Citizen", "US Citizen: Prior conviction", "Prior conviction", "China:Prior conviction", "Germany:Prior conviction", 
      "Guatemala:Prior Conviction", "Mexico:Prior Conviction", "Syria:Prior Conviction",
      "China", "Germany", "Guatemala", "Mexico", "Syria", "HS diploma", "Less than HS",
      "Broken English", "Male")
  ) %>%
  as.vector()
extract2$Level <- factor(extract2$Level, levels = extract2$Level[order(extract2$Attribute)])

extract3 <- summary(citizen_combo)[[1]]
extractx <- summary(citizen_combo)[[2]]
extract3 <- rbind(extract3, extractx)
extract3$`Std. Err` <- extract3$`Std. Err` * overall_avg
extract3$lower <- extract3$Estimate - 1.96 * extract3$`Std. Err`
extract3$upper <- extract3$Estimate + 1.96 * extract3$`Std. Err`
extract3$Level %<>% 
  plyr::mapvalues(
    c("Yes", "Prior conviction (not voting related)", "Yes:China", "Yes:Germany", "Yes:Mexico", 
      "Yes:Syria", "Yes:USA", "Yes:High school diploma", "Yes:Less than high school",
      "China", "Germany", "Mexico", "USA", "Syria", "High school diploma", "Less than high school",
      "Broken English", "Male"),
    c("US Citizen", "Prior conviction", "China:US Citizen", "Germany:US Citizen", "Mexico:US Citizen",
      "Syria:US Citizen", "USA:US Citizen", "HS Diploma:US Citizen", "Less than HS:US Citizen",
      "China", "Germany", "Mexico", "USA", "Syria", "HS diploma", "Less than HS",
      "Broken English", "Male")
  ) %>%
  as.vector()
extract3 <- extract3[-16,]
extract3$Level <- factor(extract3$Level, levels = extract3$Level[order(extract3$Attribute)])


## COEF PLOTS
ggplot(extract, aes(x = Estimate, y = Level, color = Attribute)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "AMCE", y = "", title = "No Interactions", color = "Baseline") +
  scale_color_manual(values = c("deepskyblue", "darkmagenta", "chartreuse3", "darkgoldenrod2", "blueviolet", "deeppink"),
                     labels = c("Non-citizen", "No prior conviction", "USA", "College Degree", "Fluent English", "Female")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_minimal()


ggplot(extract2, aes(x = Estimate, y = Level, color = Attribute)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "AMCE/ACIE", y = "", title = "Prior Conviction Interactions", color = "Baseline") +
  scale_color_manual(values = c("deepskyblue", "darkmagenta", "chartreuse3", "darkgoldenrod2", 
                                "blueviolet", "deeppink", "blue3", "darkgreen"),
                     labels = c("Non-citizen", "Non-citizen:No Prior Conviction", "Prior Conviction", 
                                "USA: No Prior Conviction", "USA", "College Degree", "Fluent English", "Female")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_minimal()

ggplot(extract3, aes(x = Estimate, y = Level, color = Attribute)) + # citizen:USA not here cause NAN
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "AMCE/ACIE", y = "", title = "Citizenship Interactions", color = "Baseline") +
  scale_color_manual(values = c("deepskyblue", "darkmagenta", "chartreuse3", "darkgoldenrod2", 
                                "blueviolet", "deeppink", "blue3", "darkgreen"),
                     labels = c("Non-citizen", "Guatemala:Non-citizen", "College Degree:Non-citizen", 
                                "No Prior Conviction", "Guatemala", "College Degree", "Fluent English", "Female")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_minimal()

#

################## TOPIC MODEL ################## 

## Questions in Survey:
## vf_thoughts: What do you think about when you think about voter fraud?
## vf_happens: How do you personally think voter fraud happens during an election?

library(stm)
set.seed(9189654)

## Take unique ids only
topic_model <- original_data %>% 
  distinct(id, .keep_all = TRUE)

topic_model$pid7 %<>% 
  plyr::mapvalues(
    c(1, 2, 3, 4, 5, 6, 7),
    c("Democrat", "Democrat", "Democrat", "Independent", "Republican", "Republican", "Republican")
  ) %>% 
  as.vector()

## Removing phrases
topic_model$vf_thoughts <- gsub("i think", "", tolower(topic_model$vf_thoughts))
topic_model$vf_happens <- gsub("i think", "", tolower(topic_model$vf_happens))

## Processing data
process <- textProcessor(topic_model$vf_thoughts, metadata = topic_model)
process_2 <- textProcessor(topic_model$vf_happens, metadata = topic_model)

## Document output
output <- prepDocuments(process$documents, process$vocab, process$meta, 
                        lower.thresh = 38) 
output2 <- prepDocuments(process_2$documents, process_2$vocab, process_2$meta, 
                         lower.thresh = 38) 
# Let's take words that are included in at least 38 (~5% of) responses as they may be more meaningful
# Don't need ultra uncommmon words

docs <- output$documents
vocab <- output$vocab
meta <- output$meta

docs2 <- output2$documents
vocab2 <- output2$vocab
meta2 <- output2$meta

## Searching for right number of topics
topicnumbers <- c(4, 6, 8, 10) # trying out a lot of topics
topicresult <- searchK(docs,vocab, topicnumbers, 
                       prevalence =~ pid7, 
                       data = meta)
topicresult2 <- searchK(docs2,vocab2, topicnumbers, 
                        prevalence =~ pid7, 
                        data = meta2)

plot(topicresult) # let's go with 6 for both
plot(topicresult2)

## Topic model
topic_model_vfthoughts <- stm(output$documents, output$vocab, K = 6, data = output$meta, init.type = "Spectral", seed = 9189654)
topic_model_vfhappens <- stm(output2$documents, output2$vocab, K = 6, data = output2$meta, init.type = "Spectral", seed = 564321) 

## Validating
## 1: checking word prevalence
labelTopics(topic_model_vfthoughts, n = 6)
plot.STM(topic_model_vfthoughts, type = "summary", labeltype="prob") # prevalence of topics

labelTopics(topic_model_vfhappens, n = 6)
plot.STM(topic_model_vfhappens, type = "summary", labeltype="prob") 

## VF THOUGHTS
## 2: Reading examples (alter n as needed)
t1 <- findThoughts(topic_model_vfthoughts, texts = substr(topic_model$vf_thoughts, 1, 400)[as.numeric(names(output$documents))], 
                   n = 6, topics = 1)$docs[[1]]
t2 <- findThoughts(topic_model_vfthoughts, texts = substr(topic_model$vf_thoughts, 1, 400)[as.numeric(names(output$documents))], 
                   n = 6, topics = 2)$docs[[1]]
t3 <- findThoughts(topic_model_vfthoughts, texts = substr(topic_model$vf_thoughts, 1, 400)[as.numeric(names(output$documents))], 
                   n = 4, topics = 3)$docs[[1]]
t4 <- findThoughts(topic_model_vfthoughts, texts = substr(topic_model$vf_thoughts, 1, 400)[as.numeric(names(output$documents))], 
                   n = 4, topics = 4)$docs[[1]]
t5 <- findThoughts(topic_model_vfthoughts, texts = substr(topic_model$vf_thoughts, 1, 400)[as.numeric(names(output$documents))], 
                   n = 5, topics = 5)$docs[[1]]
t6 <- findThoughts(topic_model_vfthoughts, texts = substr(topic_model$vf_thoughts, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 6)$docs[[1]]

# plots of the examples
plotQuote(t1, width = 30, text.cex = 0.7, main = "Topic 1") # It is a serious crime that should be punished
plotQuote(t2, width = 30, text.cex = 0.7, main = "Topic 2") # Most that do commit voter fraud are caught
plotQuote(t3, width = 30, text.cex = 0.8, main = "Topic 3") # Occurs through mail-in ballots
plotQuote(t4, width = 30, text.cex = 0.9, main = "Topic 4") # A corrupt system and people trying to illegal steal an election
plotQuote(t5, width = 30, text.cex = 0.7, main = "Topic 5") # It occurs rarely
plotQuote(t6, width = 30, text.cex = 1, main = "Topic 6") # It is a lie perpetuated by Trump

## VF HAPPENS
## 2: Reading examples (alter n as needed)
t1x <- findThoughts(topic_model_vfhappens, texts = substr(topic_model$vf_happens, 1, 400)[as.numeric(names(output2$documents))], 
                    n = 3, topics = 1)$docs[[1]]
t2x <- findThoughts(topic_model_vfhappens, texts = substr(topic_model$vf_happens, 1, 400)[as.numeric(names(output2$documents))], 
                    n = 3, topics = 2)$docs[[1]]
t3x <- findThoughts(topic_model_vfhappens, texts = substr(topic_model$vf_happens, 1, 400)[as.numeric(names(output2$documents))], 
                    n = 3, topics = 3)$docs[[1]]
t4x <- findThoughts(topic_model_vfhappens, texts = substr(topic_model$vf_happens, 1, 400)[as.numeric(names(output2$documents))], 
                    n = 3, topics = 4)$docs[[1]]
t5x <- findThoughts(topic_model_vfhappens, texts = substr(topic_model$vf_happens, 1, 400)[as.numeric(names(output2$documents))], 
                    n = 3, topics = 5)$docs[[1]]
t6x <- findThoughts(topic_model_vfhappens, texts = substr(topic_model$vf_happens, 1, 400)[as.numeric(names(output2$documents))], 
                    n = 3, topics = 6)$docs[[1]]

# plots of the examples
plotQuote(t1x, width = 30, text.cex = 0.9, main = "Topic 1") # They do not believe it does or that it is extremely rare
plotQuote(t2x, width = 30, text.cex = 1, main = "Topic 2") # Poll workers counting incorrectly on purpose or changing legal votes
plotQuote(t3x, width = 30, text.cex = 1, main = "Topic 3") # Someone assumes another’s identity
plotQuote(t4x, width = 30, text.cex = 1, main = "Topic 4") # Mail-in ballots and casting multiple ballots
plotQuote(t5x, width = 30, text.cex = 1, main = "Topic 5") # Electronic voting systems
plotQuote(t6x, width = 30, text.cex = 1, main = "Topic 6") # infrequent AND is used by Republicans as a scare tactic


## Checking out prevalence based on some respondent demographics
topic_labels <- c("T1", "T2", "T3", "T4", "T5", "T6")

## Code lines: 198-204 need to be altered as needed for these
## Effects Estimate (VF THOUGHTS)
## CONCLUSION: No differences in topic prevalence by respondent demographics
estimate1 <- estimateEffect(1:6 ~ race2, topic_model_vfthoughts, metadata = meta, uncertainty = "Global")
summary(estimate1) ## effect of being white on topic prevalence

estimate2 <- estimateEffect(1:6 ~ respondent_gender, topic_model_vfthoughts, metadata = meta, uncertainty = "Global")
summary(estimate2) ## effect of being male on topic prevalence

estimate3 <- estimateEffect(1:6 ~ pid7, topic_model_vfthoughts, metadata = meta, uncertainty = "Global")
summary(estimate3)

estimate4 <- estimateEffect(1:6 ~ ideo5, topic_model_vfthoughts, metadata = meta, uncertainty = "Global")
summary(estimate4)

estimate5 <- estimateEffect(1:6 ~ educ, topic_model_vfthoughts, metadata = meta, uncertainty = "Global")
summary(estimate5)

## plotting estimates
plot.estimateEffect(estimate1, 
                    covariate = "race2", 
                    topics = 1:6, 
                    model =  topic_model_vfthoughts, 
                    method = "difference", 
                    cov.value1 = "White", 
                    cov.value2 = "Non-White", 
                    xlab = "Change in topic likelihood when respondent is White", 
                    main = "Difference between Whites and Non-Whites in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

plot.estimateEffect(estimate2, 
                    covariate = "respondent_gender", 
                    topics = 1:6, 
                    model =  topic_model_vfthoughts, 
                    method = "difference", 
                    cov.value1 = "Female", 
                    cov.value2 = "Male", 
                    xlab = "Change in topic likelihood when respondent is Female", 
                    main = "Difference between Females and Males in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

plot.estimateEffect(estimate3, 
                    covariate = "pid7", 
                    topics = 1:6, 
                    model =  topic_model_vfthoughts, 
                    method = "difference", 
                    cov.value1 = "Republican", 
                    cov.value2 = "Democrat", 
                    xlab = "Change in topic likelihood when respondent is Republican", 
                    main = "Difference between Republicans and Democrats in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

plot.estimateEffect(estimate4, 
                    covariate = "ideo5", 
                    topics = 1:6, 
                    model =  topic_model_vfthoughts, 
                    method = "difference", 
                    cov.value1 = "Conservative", 
                    cov.value2 = "Liberal", 
                    xlab = "Change in topic likelihood when respondent is Conservative", 
                    main = "Difference between Conservatives and Liberals in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

## Effects Estimate (VF HAPPENS)
estimate1 <- estimateEffect(1:6 ~ race2, topic_model_vfhappens, metadata = meta2, uncertainty = "Global")
summary(estimate1)

estimate2 <- estimateEffect(1:6 ~ respondent_gender, topic_model_vfhappens, metadata = meta2, uncertainty = "Global")
summary(estimate2) 

estimate3 <- estimateEffect(1:6 ~ pid7, topic_model_vfhappens, metadata = meta2, uncertainty = "Global")
summary(estimate3) # Topic 4 and 6

estimate4 <- estimateEffect(1:6 ~ ideo5, topic_model_vfhappens, metadata = meta2, uncertainty = "Global")
summary(estimate4)

estimate5 <- estimateEffect(1:6 ~ educ, topic_model_vfhappens, metadata = meta2, uncertainty = "Global")
summary(estimate5)


## plotting estimates
plot.estimateEffect(estimate1, 
                    covariate = "race2", 
                    topics = 1:6, 
                    model =  topic_model_vfhappens, 
                    method = "difference", 
                    cov.value1 = "White", 
                    cov.value2 = "Non-White", 
                    xlab = "Change in topic likelihood when respondent is White", 
                    main = "Difference between Whites and Non-Whites in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

plot.estimateEffect(estimate2, 
                    covariate = "respondent_gender", 
                    topics = 1:6, 
                    model =  topic_model_vfhappens, 
                    method = "difference", 
                    cov.value1 = "Female", 
                    cov.value2 = "Male", 
                    xlab = "Change in topic likelihood when respondent is Female", 
                    main = "Difference between Females and Males in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

plot.estimateEffect(estimate3, 
                    covariate = "pid7", 
                    topics = 1:6, 
                    model =  topic_model_vfhappens, 
                    method = "difference", 
                    cov.value1 = "Republican", 
                    cov.value2 = "Democrat", 
                    xlab = "Change in topic likelihood when respondent is Republican", 
                    main = "Difference between Republicans and Democrats in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

plot.estimateEffect(estimate4, 
                    covariate = "ideo5", 
                    topics = 1:6, 
                    model =  topic_model_vfhappens, 
                    method = "difference", 
                    cov.value1 = "Conservative", 
                    cov.value2 = "Liberal", 
                    xlab = "Change in topic likelihood when respondent is Conservative", 
                    main = "Difference between Conservatives and Liberals in Topic Prevalence", 
                    labeltype = "custom", 
                    custom.labels = topic_labels, 
                    xlim=c(-0.125, 0.05))

