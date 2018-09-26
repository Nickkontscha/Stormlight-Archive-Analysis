
# The code is basicly the same for each book. 
# There are a few differences with their XML trees which have to be handled seperately

# A .epub is bascily a zipped file format. One can unpack it to get one file for each chapter
# This functions parses the text of a chapter. I noticed that the text of a chapter always starts with one o
# of two different classes (depending on if it is cursive or not).
# A challange was to actually get the text of relevant pages only. Most chapters have some texts which give
# the world more background info. Some other pages are maps, pictures or other explainations which are irrelevant
# for this project. 
textOfChapter <- function(file){
  xml_data <- htmlTreeParse(file, useInternal = TRUE)
  whichNode <- "//p[@class='calibre_2']//text() | //p[@class='calibre_3']//text()"
  text <- unlist(xpathApply(xml_data, whichNode, xmlValue))
  return(text)
}

# The structure of the files unzipped .epub is helpfull here. 
# Every book has a table of contents. It gives us a lot of usefull information
# like the name of the chapter, the chapter id/number and a link to the corresponding chapter file
getTableOfContents <- function(toc_path){
  xml_data <- htmlTreeParse(toc_path, useInternal = TRUE)
  # get all text that have a node a
  text <- unlist(xpathApply(xml_data, "//a", xmlValue))
  # There is an emptry <a></a> for whatever reason
  text <- text[2:length(text)]
  # get the attributes of them
  links <- unlist(xpathApply(xml_data, "//a", xmlAttrs))
  # we only want the attribute href
  links <- links[names(links) == "href"]
  df_toc <- data.frame(matrix(ncol = 3, nrow = length(links)))
  names(df_toc) <- c("file", "chapter", "nr")
  
  df_toc$file <- links
  df_toc$chapter <- text
  
  df_toc$file <- gsub("#.*","",df_toc$file)
  df_toc$file <- paste("data/way_of_kings/", df_toc$file, sep = "")
  df_toc$nr <- gsub("(.*):.*","\\1",df_toc$chapter)
  df_toc$chapter <- gsub(".*: ","",df_toc$chapter)
  df_toc$chapter <- tolower(df_toc$chapter)
  #df_toc <- na.omit(df_toc)
  
  for(i in 1:nrow(df_toc)){
    if(nchar(df_toc$nr[i]) > 4){
      df_toc$nr[i] <- NA
    }
  }
  df_toc$nr <- gsub("I", "1", df_toc$nr)
  df_toc$nr <- gsub("i", "1", df_toc$nr)
  
  return(df_toc)
}


toc_wok_path <- "data/way_of_kings/index_split_002.html"
WayOfKings <- getTableOfContents(toc_wok_path)
WayOfKings$text <- NA
for(k in 1:nrow(WayOfKings)){
  text <- textOfChapter(WayOfKings$file[k])
  text <- paste(text, collapse = " ")
  WayOfKings$text[k] <- text
}
rm(k, text)

WayOfKings$chapter <- gsub("[[:punct:]]", "", WayOfKings$chapter)
rm(getTableOfContents, textOfChapter, toc_wok_path)

