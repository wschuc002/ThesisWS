# Module for downloading input files from irceline ftp:
# @ftp.irceline.be
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
list.of.packages <- c("utils")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the packages
library(utils)

DownloadInputFilesFromIrcelineFTP <- function(Filenames, ftp.pwd, out.dir, pol, ...) # currently only working for pollutant data
{
  print(paste0("Files to download:"))
  print(paste0(Filenames))
  
  if (!dir.exists(out.dir)) { dir.create(out.dir) }
  
  outpol.dir = file.path(out.dir, toupper(pol))
  if (!dir.exists(outpol.dir)) { dir.create(outpol.dir) }
  
  if (!all(file.exists(file.path(outpol.dir, Filenames))))
  {
    for (f in Filenames)
    {
      url.irceline = paste0("ftp://", ftp.pwd, "@ftp.irceline.be/rioifdm/2015_v421/", pol, "/", f)
      
      if (!file.exists(file.path(outpol.dir, f)))
      {
        #download.file(url = url.irceline, destfile = file.path(out.dir, paste0(year.active, MonthDayHour, "_", toupper(pol), ".txt.bz2")))
        download.file(url = url.irceline, destfile = file.path(outpol.dir, f))
      } else
      {
        print(paste0(file.path(outpol.dir, f), " already exists."))
      }
    }
  } else
  {
    print(paste0("All the hi-res air quality files for ", toupper(pol), " already exist."))
  }
}
