DF.Structure <- function(TIME, ExposureValue, IND.amount, Hr.amount, ...)
{
  ST.WS = list()
  #for (h in 1:(length(YearDates)*24))
  for (h in 1:Hr.amount)
  {
    for (i in seq(1,IND.amount))
    {
      if (i == 1) # creating the base
      {
        ST.DF = NULL
        ST.DF = data.frame(unlist(TIME[[i]])[h])
        ST.DF = cbind(ST.DF, data.frame(ExposureValue[[i]][h]), 1)
        colnames(ST.DF) = c("TIME", "EXP", "IND")
      } else {
        ST.DF = rbind(ST.DF, ST.DF[1,])
        ST.DF$EXP[i] = ExposureValue[[i]][h]
        ST.DF$IND[i] = i
      }
    }
    class(ST.DF$TIME) = class(TIME.P[[1]][[1]])
    ST.WS[[h]] = ST.DF
  }
  ST.DF.WS = data.table::rbindlist(ST.WS)
  return(ST.DF.WS)
}

DF.Stats <- function(ST.DF.WS, ...)
{
  # calculating mean, min and max and standard deviation
  mean.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), mean)))
  min.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), min)))
  max.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), max)))
  sd.EXP = data.frame (with (ST.DF.WS, tapply(EXP, factor(TIME), sd)))
  
  stats.EXP = cbind(mean.EXP, min.EXP, max.EXP, sd.EXP)
  names(stats.EXP) = c("meanEXP", "minEXP", "maxEXP", "sdEXP")
  stats.EXP$TIME = rownames(stats.EXP)
  
  #class(stats.EXP$TIME) = class(TIME.P[[1]][[1]])
  
  return(stats.EXP)
}