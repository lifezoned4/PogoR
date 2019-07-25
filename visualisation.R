library(ggplot2)

plotMon <- function(name, IVs) {
  singleMon <- monData[which(monData$name == name),]

  maxProduct <- max(singleMon$StatProduct)
  startAt <- round(min(singleMon$StatProduct)/maxProduct, 3) * 100
  
  print(IVs)
  
  combinationsStatProds <- foreach(IV = iter(IVs, by='row'), .combine=rbind)%do% {
    return(singleMon[which(singleMon$IV.Attack == IV[,"AtkIV"] &
                                   singleMon$IV.Defense == IV[, "DefenseIV"] &
                                   singleMon$IV.HP == IV[, "HpIV"]),][['StatProduct']])
  }
  
  gainLineMin <- min(combinationsStatProds)/maxProduct * 100 
  gainLineMax <- max(combinationsStatProds)/maxProduct* 100 
  
  print(paste(gainLineMin, gainLineMax))
  
  color <- singleMon$Level
  minLevel <- min(singleMon$Level)
 
  p <- ggplot(singleMon, aes(x=StatProduct/maxProduct*100, fill=color)) + geom_histogram(bins = 100) 
  foreach(comb = iter(combinationsStatProds, by='row'), .combine=rbind) %do% {
    gain <- comb / maxProduct * 100 
    p <- p + geom_vline(xintercept=gain, color='black', linetype="dashed")
  }
  p +  geom_vline(xintercept=gainLineMin, color='red', linetype="dashed") +
    geom_vline(xintercept=gainLineMax, color='green', linetype="dashed") +
    labs(title=paste(name, "min:", paste(startAt, "%", sep=''), minLevel), x="StatProduct", y = "Count")  
}

combinations <- list(
                  c(1, 15, 9),
                  c(7, 10, 9),
                  c(12, 6, 9),
                  c(15, 4, 9),
                  c(9, 9, 8),
                  c(12, 7, 8)
                 )

df_combinations <- data.frame()
df_combinations <- rbind(df_combinations, combinations)
names(df_combinations) <- c("AtkIV", "DefenseIV", "HpIV")

plotMon('Gengar', df_combinations)
