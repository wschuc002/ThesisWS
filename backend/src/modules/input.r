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
# list.of.packages = c("devtools", "RGoogleDocs", "rgoogledata")
# new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# # For packages not available in CRAN, but are on Github:
# if(length(new.packages)) devtools::install_github("duncantl/RGoogleDocs")


## Load the package(s)
library(RGoogleDocs)
library(RCurl)
#library(rgoogledata)

base.GD = "https://docs.google.com/uc?export=download&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM&confirm="

#url1 = paste0(base.GD, "ONuX")
url1 = paste0(base.GD, confirmID)
OUT1 = getURL(url1, ssl.verifypeer = FALSE)
#confirmID = substr(OUT1, 34562, 34565) # false, gives same confirmID
confirmID = substr(OUT1, 36149, 36152)
confirmID

download.file(url= paste0("http://docs.google.com/uc?export=download&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM&confirm=",confirmID),
              destfile= "C:/git/ThesisWS/backend/data/RIVM2/test4564564586.zip", method='auto')

browseURL(url1, browser = getOption("Firefox"),
          encodeIfNeeded = FALSE)

## My test file is here: https://docs.google.com/file/d/0B1bNxpx4XS8dTG01Z3pvRjVYZHc/edit?usp=sharing
dl_from_GoogleD <- function(output, key, confirmID){
  require(RCurl)
  bin <- getBinaryURL(paste0("https://docs.google.com/uc?export=download&id=", key, "&confirm=", confirmID), ssl.verifypeer = FALSE)
  con <- file(output, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste(output, "read into", getwd())))
}

test <- dl_from_GoogleD(output = "klootzak.7z",
                        key = "0B5dbtjRcWbwiS1NJNGZrMlJObHM",
                        confirmID = "tUZl")



gpasswd = "mysecretpassword"
auth = getGoogleAuth("kay.cichini @ gmail.com", gpasswd)
con = getGoogleDocsConnection(auth)

CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
docs <- getDocs(con, cainfo = CAINFO)

# get file references
hrefs <- lapply(docs, function(x) return(x@access["href"]))
keys <- sub(".*/full/.*%3A(.*)", "\\1", hrefs)
types <- sub(".*/full/(.*)%3A.*", "\\1", hrefs)

# make urls (for url-scheme see: http://techathlon.com/download-shared-files-google-drive/)
# put format parameter for other output formats!
pdf_urls <- paste0("https://docs.google.com/uc?export=download&id=", keys)
doc_urls <- paste0("https://docs.google.com/document/d/", keys, "/export?format=", "txt")

pdf_urls = paste0("https://docs.google.com/uc?export=download&confirm=tUZl&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM")


# download documents with your browser
gdoc_ids <- grep("document", types)
lapply(gdoc_ids, function(x) shell.exec(doc_urls[x]))

pdf_ids <- grep("7z", types, ignore.case = T)
lapply(pdf_ids, function(x) shell.exec(pdf_urls[x]))





  
 
temp <- tempfile()
download.file("https://docs.google.com/uc?export=download&confirm=tUZl&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM", temp)
con <- unz(temp, "test.zip")
dat <- read.table(con, header=T, skip=2, sep=",")
unlink(temp)


library(RCurl)
library(curl)
download.file(url='http://s3.amazonaws.com/tripdata/201307-citibike-tripdata.zip',
              destfile= "test.zip", method='curl')

download.file(url='http://docs.google.com/uc?export=download&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM&confirm=iI_9',
              destfile= "C:/git/ThesisWS/backend/data/RIVM2/test456456456.zip", method='curl')


options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))   
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
x <- getURL(URL)
download.file(url= URL, destfile= "test3.csv", method='auto')

URL2 <- "http://docs.google.com/uc?export=download&confirm=tUZl&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM"
download.file(url= URL2, destfile= "test3.zip", method='curl')




# set workdrive
setwd("C:/git/ThesisWS/backend/data/RIVM2")

# open a temporary directory to store data
temp <- tempdir()

# assuming each zip file is identified uniquely through a number   
for (i in 1:(1))
     {
     
     # define the url	
     url <- paste("https://docs.google.com/uc?export=download&confirm=tUZl&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM", i, sep = "")
     url <- paste("https://docs.google.com/uc?export=download&confirm=tUZl&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM")
     
     file <- basename(url)
     file <- "sdfsdf"
     
     # download the zip file and store in the "file" object 
     download.file(url, file)
     
     # unzip the file and store in the temp folder
     gunzip(file, exdir = temp)
}

# unlink the temporary directory
unlink(temp)



myZip.phase1 <- getURL("https://docs.google.com/uc?&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM&export=download&confirm=lJOb", ssl.verifypeer = FALSE)

# pattern <- "([c][o][n][f][i][r][m][%])[0-9]*[A-z]*"
# pattern <- "[a-z]"
# pattern <- "(confirm%3D)([A-z]*)"

url = "http://docs.google.com/uc?id=0B5dbtjRcWbwiRm1tVVUzbkhUekE&export=download&confirm=i1Z5"
url = myZip.phase2
download.file("http://docs.google.com/uc?&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM&export=download&confirm=1KZD", destfile = "C:/git/ThesisWS/backend/data/RIVM2/test000.7z", method = "auto")

confirmID = substr(myZip.phase1, 34562, 34565)
myZip.phase2 = paste("https://docs.google.com/uc?&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM&export=download&confirm=", confirmID, sep = "")
myZip.phase2

download.file(myZip.phase2, destfile = "C:/git/ThesisWS/backend/data/RIVM2/test000.7z", method = "curl")


temp <- tempfile()
download.file("http://docs.google.com/uc?export=download&confirm=oORw&id=0B5dbtjRcWbwiRm1tVVUzbkhUekE",temp, method = "curl")
con <- unz(temp, "a1.dat")
data <- matrix(scan(con),ncol=4,byrow=TRUE)
unlink(temp)




download.URL = "https://docs.google.com/uc?export=download&confirm=tUZl&id=0B5dbtjRcWbwiS1NJNGZrMlJObHM"
download.AQNL <- function(download.URL, ...)
  {
    setwd("../data/RIVM2")
    #url = "https://drive.google.com/drive/folders/0B5dbtjRcWbwiU0x2SlRneEpOTlE"
    #url = "https://drive.google.com/file/d/0B5dbtjRcWbwiSU9tOUQ0TUxZR0E"
    url = download.URL
    download.file(url, destfile = file.path("..", "data", "RIVM2"), method = "wget")
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