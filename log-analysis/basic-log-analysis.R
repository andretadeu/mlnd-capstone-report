# Author: Andre Tadeu de Carvalho
# Date: 18 February 2018
# Basic data analysis of OpenAI Baselines executions of DQN and A2C

library('ggplot2')

minNGamesWindow <- function(data_table, col_name, n) {
  result <- rep(0, dim(data_table)[[1]])
  window_size <- n %/% 2
  for (i in 1:dim(data_table)[[1]]) {
    begin <- if (i <= window_size) 1 else i - window_size
    end <- if (i > dim(data_table)[[1]] - window_size) dim(data_table)[[1]] else i + window_size
    result[[i]] <- min(data_table[begin:end, col_name])
  }
  result
}

maxNGamesWindow <- function(data_table, col_name, n) {
  result <- rep(0, dim(data_table)[[1]])
  window_size <- n %/% 2
  for (i in 1:dim(data_table)[[1]]) {
    begin <- if (i <= window_size) 1 else i - window_size
    end <- if (i > dim(data_table)[[1]] - window_size) dim(data_table)[[1]] else i + window_size
    result[[i]] <- max(data_table[begin:end, col_name])
  }
  result
}

meanNGamesWindow <- function(data_table, col_name, n) {
  result <- rep(0, dim(data_table)[[1]])
  window_size <- n %/% 2
  for (i in 1:dim(data_table)[[1]]) {
    begin <- if (i <= window_size) 1 else i - window_size
    end <- if (i > dim(data_table)[[1]] - window_size) dim(data_table)[[1]] else i + window_size
    result[[i]] <- mean(data_table[begin:end, col_name])
  }
  result
}

medianNGamesWindow <- function(data_table, col_name, n) {
  result <- rep(0, dim(data_table)[[1]])
  window_size <- n %/% 2
  for (i in 1:dim(data_table)[[1]]) {
    begin <- if (i <= window_size) 1 else i - window_size
    end <- if (i > dim(data_table)[[1]] - window_size) dim(data_table)[[1]] else i + window_size
    result[[i]] <- median(data_table[begin:end, col_name])
  }
  result
}

sdNGamesWindow <- function(data_table, col_name, n) {
  result <- rep(0, dim(data_table)[[1]])
  window_size <- n %/% 2
  for (i in 1:dim(data_table)[[1]]) {
    begin <- if (i <= window_size) 1 else i - window_size
    end <- if (i > dim(data_table)[[1]] - window_size) dim(data_table)[[1]] else i + window_size
    result[[i]] <- sd(data_table[begin:end, col_name])
  }
  result
}

readMultipleCSVs <- function(logs_path) {
  files <- list.files(path = logs_path, pattern = '^[0-9]+\\.monitor\\.csv')
  data_frames <- list()
  cnt <- 1
  for (f in files) {
    df <- read.csv(paste(logs_path, f, sep = '/'), skip = 1, header = TRUE)
    df$Executor <- cnt
    df$Entry <- 1:dim(df)[[1]]
    data_frames[[cnt]] <- df
    cnt <- cnt + 1
  }
  result <- data_frames[[1]]
  for (i in 2:length(data_frames)) {
    result <- rbind(result, data_frames[[i]])
  }
  result
}

extractMetricsBetweenMultipleAgents <- function(data_table) {
  maxNumEntries <- max(data_table$Entry)
  entry <- 1:maxNumEntries
  numAgents <- rep(0, maxNumEntries)
  agentMinRew <- rep(0, maxNumEntries)
  agentMinNumGames <- rep(0, maxNumEntries)
  agentMaxRew <- rep(0, maxNumEntries)
  agentMaxNumGames <- rep(0, maxNumEntries)
  agentMeanRew <- rep(0, maxNumEntries)
  agentMeanNumGames <- rep(0, maxNumEntries)
  agentMedianRew <- rep(0, maxNumEntries)
  agentMedianNumGames <- rep(0, maxNumEntries)
  agentSDRew <- rep(0, maxNumEntries)
  agentSDNumGames <- rep(0, maxNumEntries)
  result <- data.frame(entry, agentMinRew, agentMinNumGames, agentMaxRew,
                       agentMaxNumGames, agentMeanRew, agentMeanNumGames,
                       agentMedianRew, agentMedianNumGames, agentSDRew,
                       agentSDNumGames)
  for (i in 1:maxNumEntries) {
    entry <- data_table[data_table$Entry == i,]
    result[i, "numAgents"] <- dim(entry)[[1]]
    result[i, "agentMinRew"] <- min(entry[, "SumRewards"])
    result[i, "agentMinNumGames"] <- min(entry[, "NumGames"])
    result[i, "agentMaxRew"] <- max(entry[, "SumRewards"])
    result[i, "agentMaxNumGames"] <- max(entry[, "NumGames"])
    result[i, "agentMeanRew"] <- mean(entry[, "SumRewards"])
    result[i, "agentMeanNumGames"] <- mean(entry[, "NumGames"])
    result[i, "agentMedianRew"] <- median(entry[, "SumRewards"])
    result[i, "agentMedianNumGames"] <- median(entry[, "NumGames"])
    result[i, "agentSDRew"] <- sd(entry[, "SumRewards"])
    result[i, "agentSDNumGames"] <- sd(entry[, "NumGames"])
  }
  result
}

# Raising statistics for monitor data from DQN
monitor_dqn <- read.csv('../openai-logs/deepq/atari/monitor.csv', skip = 1, header = TRUE)
colnames(monitor_dqn) <- c("SumRewards", "NumGames", "Time")
monitor_dqn$Entry <- 1:dim(monitor_dqn)[[1]]
monitor_dqn$`MinRewards` <- minNGamesWindow(monitor_dqn, 'SumRewards', 100)
monitor_dqn$`MinNumGames` <- minNGamesWindow(monitor_dqn, 'NumGames', 100)
monitor_dqn$`MaxRewards` <- maxNGamesWindow(monitor_dqn, 'SumRewards', 100)
monitor_dqn$`MaxNumGames` <- maxNGamesWindow(monitor_dqn, 'NumGames', 100)
monitor_dqn$`MeanRewards` <- meanNGamesWindow(monitor_dqn, 'SumRewards', 100)
monitor_dqn$`MeanNumGames` <- meanNGamesWindow(monitor_dqn, 'NumGames', 100)
monitor_dqn$`MedianRewards` <- medianNGamesWindow(monitor_dqn, 'SumRewards', 100)
monitor_dqn$`MedianNumGames` <- medianNGamesWindow(monitor_dqn, 'NumGames', 100)
monitor_dqn$`SDRewards` <- sdNGamesWindow(monitor_dqn, 'SumRewards', 100)
monitor_dqn$`SDNumGames` <- sdNGamesWindow(monitor_dqn, 'NumGames', 100)

monitor_dqn2 <- read.csv('../openai-logs/deepq/atari/0/monitor.csv', skip = 1, header = TRUE)
colnames(monitor_dqn2) <- c("SumRewards", "NumGames", "Time")
monitor_dqn2$Entry <- 1:dim(monitor_dqn2)[[1]]
monitor_dqn2$`MinRewards` <- minNGamesWindow(monitor_dqn2, 'SumRewards', 100)
monitor_dqn2$`MinNumGames` <- minNGamesWindow(monitor_dqn2, 'NumGames', 100)
monitor_dqn2$`MaxRewards` <- maxNGamesWindow(monitor_dqn2, 'SumRewards', 100)
monitor_dqn2$`MaxNumGames` <- maxNGamesWindow(monitor_dqn2, 'NumGames', 100)
monitor_dqn2$`MeanRewards` <- meanNGamesWindow(monitor_dqn2, 'SumRewards', 100)
monitor_dqn2$`MeanNumGames` <- meanNGamesWindow(monitor_dqn2, 'NumGames', 100)
monitor_dqn2$`MedianRewards` <- medianNGamesWindow(monitor_dqn2, 'SumRewards', 100)
monitor_dqn2$`MedianNumGames` <- medianNGamesWindow(monitor_dqn2, 'NumGames', 100)
monitor_dqn2$`SDRewards` <- sdNGamesWindow(monitor_dqn2, 'SumRewards', 100)
monitor_dqn2$`SDNumGames` <- sdNGamesWindow(monitor_dqn2, 'NumGames', 100)

# Raising statistics for monitor data from DQN
monitor_dqn_nd <- read.csv('../openai-logs/deepq/atari/1/monitor.csv', skip = 1, header = TRUE)
colnames(monitor_dqn_nd) <- c("SumRewards", "NumGames", "Time")
monitor_dqn_nd$Entry <- 1:dim(monitor_dqn_nd)[[1]]
monitor_dqn_nd$`MinRewards` <- minNGamesWindow(monitor_dqn_nd, 'SumRewards', 100)
monitor_dqn_nd$`MinNumGames` <- minNGamesWindow(monitor_dqn_nd, 'NumGames', 100)
monitor_dqn_nd$`MaxRewards` <- maxNGamesWindow(monitor_dqn_nd, 'SumRewards', 100)
monitor_dqn_nd$`MaxNumGames` <- maxNGamesWindow(monitor_dqn_nd, 'NumGames', 100)
monitor_dqn_nd$`MeanRewards` <- meanNGamesWindow(monitor_dqn_nd, 'SumRewards', 100)
monitor_dqn_nd$`MeanNumGames` <- meanNGamesWindow(monitor_dqn_nd, 'NumGames', 100)
monitor_dqn_nd$`MedianRewards` <- medianNGamesWindow(monitor_dqn_nd, 'SumRewards', 100)
monitor_dqn_nd$`MedianNumGames` <- medianNGamesWindow(monitor_dqn_nd, 'NumGames', 100)
monitor_dqn_nd$`SDRewards` <- sdNGamesWindow(monitor_dqn_nd, 'SumRewards', 100)
monitor_dqn_nd$`SDNumGames` <- sdNGamesWindow(monitor_dqn_nd, 'NumGames', 100)


# Raising statistics for monitor data from A2C
monitor_a2c <- readMultipleCSVs('../openai-logs/a2c/atari')
colnames(monitor_a2c) <- c("SumRewards", "NumGames", "Time", "Executor", "Entry")
a2c_metrics <- extractMetricsBetweenMultipleAgents(monitor_a2c)

monitor_a2c_0 <- readMultipleCSVs('../openai-logs/a2c/atari/0')
colnames(monitor_a2c_0) <- c("SumRewards", "NumGames", "Time", "Executor", "Entry")
a2c_metrics_0 <- extractMetricsBetweenMultipleAgents(monitor_a2c_0)

monitor_a2c_1 <- readMultipleCSVs('../openai-logs/a2c/atari/1')
colnames(monitor_a2c_1) <- c("SumRewards", "NumGames", "Time", "Executor", "Entry")
a2c_metrics_1 <- extractMetricsBetweenMultipleAgents(monitor_a2c_1)

monitor_a2c_2 <- readMultipleCSVs('../openai-logs/a2c/atari/2')
colnames(monitor_a2c_2) <- c("SumRewards", "NumGames", "Time", "Executor", "Entry")
a2c_metrics_2 <- extractMetricsBetweenMultipleAgents(monitor_a2c_2)

monitor_a2c_3 <- readMultipleCSVs('../openai-logs/a2c/atari/3')
colnames(monitor_a2c_3) <- c("SumRewards", "NumGames", "Time", "Executor", "Entry")
a2c_metrics_3 <- extractMetricsBetweenMultipleAgents(monitor_a2c_3)

monitor_a2c_4 <- readMultipleCSVs('../openai-logs/a2c/atari/4')
colnames(monitor_a2c_4) <- c("SumRewards", "NumGames", "Time", "Executor", "Entry")
a2c_metrics_4 <- extractMetricsBetweenMultipleAgents(monitor_a2c_4)

monitor_a2c_5 <- readMultipleCSVs('../openai-logs/a2c/atari/5')
colnames(monitor_a2c_5) <- c("SumRewards", "NumGames", "Time", "Executor", "Entry")
a2c_metrics_5 <- extractMetricsBetweenMultipleAgents(monitor_a2c_5)

# Graphs
# DQN
png(file = 'dqn-mean-summed-rewards.png', width = 800, height = 600, units = "px")
dqn_mean_summed_rew <- ggplot() +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MeanRewards`, colour = "Mean")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MinRewards`, colour = "Minimum")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MaxRewards`, colour = "Maximum")) +
  ylab("Summed rewards in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Mean of all summed rewards in 100 entries window")
print(dqn_mean_summed_rew)
dev.off()

png(file = 'dqn-median-summed-rewards.png', width = 800, height = 600, units = "px")
dqn_median_summed_rew <- ggplot() +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MedianRewards`, colour = "Median")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MinRewards`, colour = "Minimum")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MaxRewards`, colour = "Maximum")) +
  ylab("Summed rewards in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Median of all summed rewards in 100 entries window")
print(dqn_median_summed_rew)
dev.off()

png(file = 'dqn-sd-summed-rewards.png', width = 800, height = 600, units = "px")
dqn_sd_summed_rew <- ggplot(data = monitor_dqn, aes(x = Entry, y = `SDRewards`)) + geom_line(stat = 'identity') +
  ylab("Summed rewards in 100 entries window") +
  ggtitle("Standard deviance of all summed rewards in 100 entries window")
print(dqn_sd_summed_rew)
dev.off()

png(file = 'dqn-mean-ngp.png', width = 800, height = 600, units = "px")
dqn_mean_ngp <- ggplot() +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MeanNumGames`, colour = "Mean")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MinNumGames`, colour = "Minimum")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MaxNumGames`, colour = "Maximum")) +
  ylab("Number of games played in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Mean of the number of games played in 100 entries window")
print(dqn_mean_ngp)
dev.off()

png(file = 'dqn-median-ngp.png', width = 800, height = 600, units = "px")
dqn_median_ngp <- ggplot() +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MedianNumGames`, colour = "Median")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MinNumGames`, colour = "Minimum")) +
  geom_line(data = monitor_dqn, aes(x = Entry, y = `MaxNumGames`, colour = "Maximum")) +
  ylab("Number of games played in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Median of the number of games played in 100 entries window")
print(dqn_median_ngp)
dev.off()

png(file = 'dqn-sd-ngp.png', width = 800, height = 600, units = "px")
dqn_sd_ngp <- ggplot(data = monitor_dqn, aes(x = Entry, y = `SDNumGames`)) + geom_line(stat = 'identity') +
  ylab("Number of games played in 100 entries window") +
  ggtitle("Standard deviance of the number of games played in 100 entries window")
print(dqn_sd_ngp)
dev.off()

# DQN - Non dueling
png(file = 'dqn-nd-mean-summed-rewards.png', width = 800, height = 600, units = "px")
dqn_nd_mean_summed_rew <- ggplot() +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MeanRewards`, colour = "Mean")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MinRewards`, colour = "Minimum")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MaxRewards`, colour = "Maximum")) +
  ylab("Summed rewards in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Mean of all summed rewards in 100 entries window")
print(dqn_nd_mean_summed_rew)
dev.off()

png(file = 'dqn-nd-median-summed-rewards.png', width = 800, height = 600, units = "px")
dqn_nd_median_summed_rew <- ggplot() +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MedianRewards`, colour = "Median")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MinRewards`, colour = "Minimum")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MaxRewards`, colour = "Maximum")) +
  ylab("Summed rewards in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Median of all summed rewards in 100 entries window")
print(dqn_nd_median_summed_rew)
dev.off()

png(file = 'dqn-nd-sd-summed-rewards.png', width = 800, height = 600, units = "px")
dqn_nd_sd_summed_rew <- ggplot(data = monitor_dqn_nd, aes(x = Entry, y = `SDRewards`)) + geom_line(stat = 'identity') +
  ylab("Summed rewards in 100 entries window") +
  ggtitle("Standard deviance of all summed rewards in 100 entries window")
print(dqn_nd_sd_summed_rew)
dev.off()

png(file = 'dqn-nd-mean-ngp.png', width = 800, height = 600, units = "px")
dqn_nd_mean_ngp <- ggplot() +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MeanNumGames`, colour = "Mean")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MinNumGames`, colour = "Minimum")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MaxNumGames`, colour = "Maximum")) +
  ylab("Number of games played in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Mean of the number of games played in 100 entries window")
print(dqn_nd_mean_ngp)
dev.off()

png(file = 'dqn-nd-median-ngp.png', width = 800, height = 600, units = "px")
dqn_nd_median_ngp <-ggplot() +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MedianNumGames`, colour = "Median")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MinNumGames`, colour = "Minimum")) +
  geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `MaxNumGames`, colour = "Maximum")) +
  ylab("Number of games played in 100 entries window") + theme(legend.title = element_blank()) +
  ggtitle("Median of the number of games played in 100 entries window")
print(dqn_nd_median_ngp)
dev.off()

png(file = 'dqn-nd-sd-ngp.png', width = 800, height = 600, units = "px")
dqn_nd_sd_ngp <- geom_line(data = monitor_dqn_nd, aes(x = Entry, y = `SDNumGames`)) + geom_line(stat = 'identity') +
  ylab("Number of games played in 100 entries window") +
  ggtitle("Standard deviance of the number of games played in 100 entries window")
print(dqn_nd_sd_ngp)
dev.off()


# A2C
png(file = 'a2c-mean-summed-rewards.png', width = 800, height = 600, units = "px")
a2c_mean_summed_rew <- ggplot() +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMeanRew`, colour = "Mean")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMinRew`, colour = "Minimum")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMaxRew`, colour = "Maximum")) +
  xlab("Entry") + ylab("Summed rewards obtained for all agents") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean of all summed rewards obtained for all agents")
print(a2c_mean_summed_rew)
dev.off()

png(file = 'a2c-median-summed-rewards.png', width = 800, height = 600, units = "px")
a2c_median_summed_rew <- ggplot() +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMedianRew`, colour = "Median")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMinRew`, colour = "Minimum")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMaxRew`, colour = "Maximum")) +
  xlab("Entry") + ylab("Summed rewards obtained for all agents") +
  theme(legend.title = element_blank()) +
  ggtitle("Median of all summed rewards obtained for all agents")
print(a2c_median_summed_rew)
dev.off()

png(file = 'a2c-sd-summed-rewards.png', width = 800, height = 600, units = "px")
a2c_sd_summed_rew <- ggplot(data = a2c_metrics, aes(x = entry, y = `agentSDRew`)) + geom_line(stat = 'identity') +
  xlab("Entry") + ylab("Standard deviance of all agents's summed rewards.") +
  ggtitle("Evolution of the standard deviance of all agents' summed rewards.")
print(a2c_sd_summed_rew)
dev.off()

png(file = 'a2c-mean-ngp.png', width = 800, height = 600, units = "px")
a2c_mean_ngp <- ggplot() +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMeanNumGames`, colour = "Mean")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMinNumGames`, colour = "Minimum")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMaxNumGames`, colour = "Maximum")) +
  xlab("Entry") + ylab("Number of games played for all agents") +
  theme(legend.title = element_blank()) +
  ggtitle("Evolution of the number of games played for all agents")
print(a2c_mean_ngp)
dev.off()

png(file = 'a2c-median-ngp.png', width = 800, height = 600, units = "px")
a2c_median_ngp <- ggplot() +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMedianNumGames`, colour = "Median")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMinNumGames`, colour = "Minimum")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMaxNumGames`, colour = "Maximum")) +
  xlab("Entry") + ylab("Number of games played for all agents") +
  theme(legend.title = element_blank()) +
  ggtitle("Evolution of the number of games played for all agents")
print(a2c_median_ngp)
dev.off()

png(file = 'a2c-sd-ngp.png', width = 800, height = 600, units = "px")
a2c_sd_ngp <- ggplot(data = a2c_metrics, aes(x = entry, y = `agentSDNumGames`)) + geom_line(stat = 'identity') +
  xlab("Entry") + ylab("Standard deviance of the number of games played by all agents.") +
  ggtitle("Evolution of the standard deviance of the number of games played by all agents.")
print(a2c_sd_ngp)
dev.off()

png(file = 'a2c-lnlstm-mean-summed-rewards.png', width = 800, height = 600, units = "px")
a2c_mean_summed_rew <- ggplot() +
  geom_line(data = a2c_metrics_5, aes(x = entry, y = `agentMeanRew`, colour = "Mean")) +
  geom_line(data = a2c_metrics_5, aes(x = entry, y = `agentMinRew`, colour = "Minimum")) +
  geom_line(data = a2c_metrics_5, aes(x = entry, y = `agentMaxRew`, colour = "Maximum")) +
  xlab("Entry") + ylab("Summed rewards obtained for all agents") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean of all summed rewards obtained for all agents")
print(a2c_mean_summed_rew)
dev.off()

png(file = 'a2c-vs-humans-mean.png', width = 800, height = 600, units = 'px')
a2c_vs_humans_mean <- ggplot() +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMeanRew`, colour = "Mean")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMaxRew`, colour = "Maximum")) +
  geom_hline(yintercept = 580.9) + 
  annotate("text", 300, 580.9, vjust = -1, label = "Average of the best human performance") +
  xlab("Entry") + ylab("Summed rewards obtained for all agents") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean of all summed rewards obtained for all agents")
print(a2c_vs_humans_mean)
dev.off()

png(file = 'a2c-vs-humans-median.png', width = 800, height = 600, units = 'px')
a2c_vs_humans_median <- ggplot() +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMedianRew`, colour = "Median")) +
  geom_line(data = a2c_metrics, aes(x = entry, y = `agentMaxRew`, colour = "Maximum")) +
  geom_hline(yintercept = 494) + 
  annotate("text", 300, 494, vjust = -1, label = "Average of the best human performance") +
  xlab("Entry") + ylab("Summed rewards obtained for all agents") +
  theme(legend.title = element_blank()) +
  ggtitle("Mean of all summed rewards obtained for all agents")
print(a2c_vs_humans_median)
dev.off()