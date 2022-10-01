# Load required libraries
library("LexisNexisTools")
library("dplyr")
library("quanteda")
library("stm")
library("wordcloud")
library("igraph")
library("stminsights")
library("tidyverse")
library("ggplot2")

# Read documents
setwd("")
LNToutput = lnt_read("./docx_Relevant", convert_date = TRUE)

# Creating the data frame
df <- right_join(LNToutput@meta, LNToutput@articles, by = "ID")
df <- dplyr::filter(df, !is.na(Date))

# Transform date to numeric
df$Date <- as.numeric(df$Date)

# Performing the STM
processed <- textProcessor(df$Article, metadata = df)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 20, prevalence =~  s(Date),
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)

# Get common words of the topics
labelTopics(First_STM, c(1:20))

# Display STM topics ordered by expected topic proportions
topicNames <- c("Topic 1","Topic 2 (IBM Watson)","Topic 3","Topic 4 (Mental health)","Topic 5","Topic 6",
                "Topic 7 (Artificial Intelligence)","Topic 8","Topic 9 (Health monitoring)","Topic 10","Topic 11 (Investment)","Topic 12",
                "Topic 13","Topic 14","Topic 15","Topic 16","Topic 17",
                "Topic 18","Topic 19 (Covid-19)","Topic 20")

par(bty="n",col="grey40",lwd=5)
plot.STM(First_STM, type = "summary", custom.labels = "", topic.names = topicNames)

# Plot word distribution in selected topics
tidy_stm <- tidytext::tidy(First_STM)

tidy_stm_filter <- filter(tidy_stm, topic == 2 |
                            topic == 4 |
                            topic == 7 |
                            topic == 9 |
                            topic == 11 |
                            topic == 19)

tidy_stm_filter %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
labs(x = NULL, y = expression(beta))

# Find thoughts of stm
findThoughts(model = First_STM, texts = df$Article, topics = 2, n = 10)

# Topic correlation
topicCorr(First_STM, method = c("simple", "huge"), cutoff = 0.01, verbose = TRUE)

# Estimate the effect of the date
First_STM_Time_Effect <- estimateEffect(formula = 1:20 ~ s(Date), stmobj = First_STM, metadata = out$meta, 
                                        uncertainty = "Global")

# Plot the effect of the date for Topic 2, 4 and 9
par(lwd = 2)
plot.estimateEffect(First_STM_Time_Effect, "Date", method = "continuous", topics = c(2, 4, 9), 
                    model = First_STM, printlegend = FALSE, xaxt = "n", xlab = "Time", ylim=c(-.2, 0.8), 
                    linecol = c("purple", "blue", "orange"))
yearseq = seq(from = as.Date("1980-01-01"), 
              to = as.Date("2022-07-01"), by = "year")
yearnames = as.numeric(format(as.Date(yearseq), "%Y"))
axis(1,at = as.numeric(yearseq), labels = yearnames)
legend("topright", legend = c("IBM Watson", "Mental health", "Health monitoring"), col=c("purple", "blue", "orange"), lty=1)

# Plot the effect of the date for Topic 7 and 11
par(lwd = 2)
plot.estimateEffect(First_STM_Time_Effect, "Date", method = "continuous", topics = c(7, 11), 
                    model = First_STM, printlegend = FALSE, xaxt = "n", xlab = "Time", ylim=c(-.2, 0.8), 
                    linecol = c("blue", "orange"))
yearseq = seq(from = as.Date("1980-01-01"), 
              to = as.Date("2022-07-01"), by = "year")
yearnames = as.numeric(format(as.Date(yearseq), "%Y"))
axis(1,at = as.numeric(yearseq), labels = yearnames)
legend("topright", legend = c("Artificial Intelligence", "Investment"), col=c("blue", "orange"), lty=1)

# Plot the effect of the date for Topic 19
par(lwd = 2)
plot.estimateEffect(First_STM_Time_Effect, "Date", method = "continuous", topics = 19, 
                    model = First_STM, printlegend = FALSE, xaxt = "n", xlab = "Time", ylim=c(-.2, 0.45), 
                    linecol = "blue")
yearseq = seq(from = as.Date("1980-01-01"), 
              to = as.Date("2022-07-01"), by = "year")
yearnames = as.numeric(format(as.Date(yearseq), "%Y"))
axis(1,at = as.numeric(yearseq), labels = yearnames)
legend("topright", legend = "Covid-19", col="blue", lty=1)
