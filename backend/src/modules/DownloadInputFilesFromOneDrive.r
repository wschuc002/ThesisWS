# Module for downloading input files from OneDrive:
# https://wageningenur4-my.sharepoint.com/personal/william_schuch_wur_nl
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

## Check for required packages and install them (incl dependencies) if they are not installed yet.
list.of.packages <- c("curl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(curl)

DownloadInputFilesFromOneDrive <- function(Filenames, KEY.InputData, ...)
{
  print(paste0("Files to download:"))
  print(paste0(Filenames))
  
  for (f in Filenames)
  {
    Source = "https://wageningenur4-my.sharepoint.com/personal/william_schuch_wur_nl"
    DocID = KEY.InputData$DocID[KEY.InputData$Filename == f]
    AuthKey = KEY.InputData$AuthKey[KEY.InputData$Filename == f]
    
    url.onedrive = paste0(Source, "/_layouts/15/download.aspx?docid=", DocID, "&authkey=", AuthKey)
    
    if (is.na(KEY.InputData$dir.CountryRegion[KEY.InputData$Filename == f]))
    {
      out.dir = file.path("..", "data")
    } else
    {
      out.dir = file.path("..", "data", KEY.InputData$dir.CountryRegion[KEY.InputData$Filename == f])
    }
    
    if (!dir.exists(out.dir))
    {
      dir.create(out.dir)
      print(paste("Created:", out.dir))
    }

    file.dir = file.path(out.dir, f)
    
    if (!file.exists(file.dir))
    {
      curl_download(url.onedrive, destfile = file.dir)
      print(paste("Downloaded:", f, "from:", Source, "to:", out.dir))
    } else
    {
      print(paste(file.dir, "already exists."))
    }
  }
}