# Module for download input data from Google Drive
# Copyright (C) 2016 William Schuch
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


# ## Check for required packages and install them (incl dependencies) if they are not installed yet.
# list.of.packages = c("data.table", "devtools", "RGoogleDocs")
# new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# # For packages not available in CRAN, but are on Github:
# if(length(new.packages)) devtools::install_github("duncantl/RGoogleDocs")
# 

# ## Load the package(s)
# library(RGoogleDocs)


download.AQNL <- function(zip_airNL_in, ... )
  {
    setwd("../data/RIVM2")
    url = "https://drive.google.com/drive/folders/0B5dbtjRcWbwiU0x2SlRneEpOTlE"
    download.file(url, zip_airNL_in, method = "wget")
    setwd("../../src")
  }

unzip.AQNL <- function(zip_airNL_in, ... )
  {
    zip_in = file.path("..", "data", "RIVM2", zip_airNL_in)
    tiff = file.path("..", "data", "RIVM2", "20161108_vandaag_no2_00.tiff")
    # Check if input data is available
    if (!file.exists(tiff))
      {
        unzip(zip_in, exdir= file.path("..", "data", "RIVM2"))
      }
  }