library(readr)
library(foreach)
library(doParallel)
library(iterators)

RawBaseStats <- read_csv("RawBaseStats.csv")
RawCPM <- read_csv("RawCPM.csv")

cl <- makeCluster(8)
registerDoParallel(cl)

clusterExport(cl, 'RawCPM')
clusterExport(cl, 'RawBaseStats')

IVspreadLoadCurrent <- function(mon) {
  library(readr)
  
  filename <- paste('out/', mon[['#']], '_', mon[['JoinedName']], '_ranks.csv', sep='')
  if(file.exists(filename)) {
    monData <- read_csv(filename)
    return(monData)
  }
}

IVspreadRanks <- function(mon) {
  library(foreach)
  library(iterators)
  
  Atk <- mon[['Attack']]
  Def <- mon[['Defense']]
  HP <- mon[['HP']]
  
  print(mon[['JoinedName']])
  
  IVCombinations = expand.grid(0:15, 0:15, 0:15)
  names(IVCombinations) <- c('IV.Attack', 'IV.Defense', 'IV.HP')
  IVCombinations$Attack <- IVCombinations$IV.Attack + Atk
  IVCombinations$Defense <- IVCombinations$IV.Defense + Def
  IVCombinations$HP <- IVCombinations$IV.HP + HP
  
  ret <- foreach(current=iter(IVCombinations, by='row'), .combine=rbind) %dopar%  {
    level <- 0.5
    repeat {
      level <- level + 0.5 
      currentCPM <- RawCPM[RawCPM$'Level' == level,][['CP Multiplier']]
      currentCP <- (currentCPM ** 2 * current[['Attack']] * sqrt(current[['Defense']]) * sqrt(current[['HP']])) / 10
      currentCP <- floor(currentCP)
      if(currentCP > 1500) {
        level <- level - 0.5
        break
      }
      if(level == 40) {
        level <- 40
        break
      }
    }
    cpm <-  RawCPM[RawCPM$'Level' == level,][['CP Multiplier']]
    cp <-  (cpm ** 2 * current[['Attack']] * sqrt(current[['Defense']]) * sqrt(current[['HP']])) / 10
    r <- c(current[['IV.Attack']], current[['IV.Defense']], current[['IV.HP']], current[['Attack']] * cpm, current[['Defense']] * cpm, current[['HP']] * cpm, current[['Attack']]*current[['Defense']]*current[['HP']] * cpm ** 3, floor(cp), level)
    names(r) <- c('IV.Attack', 'IV.Defense' , 'IV.HP', 'Attack', 'Defense', 'HP', 'StatProduct', 'CP', 'Level')
    return(r)
  }
  
  data <- data.frame(ret)
  
  minSP <- min(data$'StatProduct')
  maxSP <- max(data$'StatProduct')
  
  gains <- foreach(current=iter(data, by='row'), .combine=rbind) %dopar% {
    g <- (current[['StatProduct']] - minSP) / (maxSP - minSP)
    names(g) <- c('Gain')
    return(g)
  }
  
  ranks <- cbind(data, gains)
  ranks <- ranks[order(-ranks$'StatProduct'),]

  filename = paste('out/', mon[['#']], '_', mon[['JoinedName']], '_ranks.csv', sep='')
  print(c('writing:', filename))
  
  write.csv(ranks, file = filename)
  
  return(ranks)
}

foreach

monData <- foreach(mon=iter(RawBaseStats, by='row'), .combine=rbind) %dopar% {
  data <- IVspreadLoadCurrent(mon)
  if(nrow(data) == 4096) print(paste('Cached:', mon[['JoinedName']]))
  else data <- IVspreadRanks(mon)
  return(cbind(data.frame(name = mon[['JoinedName']]), data))
}

stopCluster(cl)
