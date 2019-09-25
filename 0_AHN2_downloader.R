"
@author: Zsofia Koma, UvA
Aim: This script download the required AHN2 data from the pdok website
"

# Set working directory
workingdirectory="D:/Koma/Paper1/Revision/" ## set this directory where you would like to put your las files
setwd(workingdirectory)

# Set filenames and dwnload and unzip the required dataset
req_tile=list("02gz2","02hz1","06en2","06fn1")

for (tile in req_tile){
  download.file(paste("http://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_","gefilterd/g",tile,".laz.zip",sep=""),
                paste("g",tile,".laz.zip",sep=""))
  download.file(paste("http://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_","uitgefilterd/u",tile,".laz.zip",sep=""),
                paste("u",tile,".laz.zip",sep=""))
  
  unzip(paste("g",tile,".laz.zip",sep=""))
  unzip(paste("u",tile,".laz.zip",sep=""))
}

zipped <- dir(path=workingdirectory, pattern=".laz.zip")
file.remove(zipped)