# Module for extracting bz2-files
# Copyright (C) 2017 William Schuch
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages <- c("R.utils", "gtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(R.utils)
library(gtools)

ExtractBZ2 <- function(pol, PolDir, StartHour = 1, EndHour = length(YearDates)*24, ...)
{
  # check if the extracted .txt's already exist
  txt.lst = list.files(path = PolDir, pattern = paste0("[0-9]*_[0-9]*_", toupper(pol), ".txt"))
  txt.lst2 = NA
  for (s in seq_along(txt.lst))
  {
    txt.lst2[s] = !grepl(x = txt.lst[s], pattern = ".bz2")
  }
  txt.lst = txt.lst[txt.lst2]
  txt.dr = file.path(PolDir, txt.lst)
  txt.dr = mixedsort(txt.dr)
  
  if (!(length(txt.lst) == (EndHour - StartHour + 1)))
  {
    bz2.lst = list.files(path = PolDir, pattern = paste0("[0-9]*_[0-9]*_", toupper(pol), ".txt.bz2"))
    bz2.lst = mixedsort(bz2.lst) # fixes string order 1 10 11 -> 1 2 3
    bz2.lst = bz2.lst[StartHour:EndHour] # subset
    
    bz2.dr = file.path(PolDir, bz2.lst)
    
    txt.lst = gsub(x = bz2.lst, pattern = ".bz2", replacement = "")
    txt.dr = file.path(PolDir, txt.lst)
    
    for (d in seq_along(StartHour:EndHour))
    {
      if (!file.exists(txt.dr[d]))
      {
        print(paste("Extracting", bz2.dr[d]))
        bunzip2(bz2.dr[d], txt.dr[d], remove = FALSE, skip = TRUE)
      }
    }
  }
  return(txt.dr)
}