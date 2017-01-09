TimeDifference <- function(x, ...) # Method 2: read from uncompressed (larger) file
{
  start.time = Sys.time()
  
  OUT = paste(x)
  
  end.time = Sys.time()
  time.taken = end.time - start.time
  print(time.taken)
  return(OUT)
}