
# See comments in loadData_wok.R

getTableOfContents <- function(toc_path){
  xml_data <- htmlTreeParse(toc_path, useInternal = TRUE)
  # get all text that have a node a
  text <- unlist(xpathApply(xml_data, "//a", xmlValue))
  # get the attributes of them
  links <- unlist(xpathApply(xml_data, "//a", xmlAttrs))
  # we only want the attribute href
  links <- links[names(links) == "href"]
  df_toc <- data.frame(matrix(ncol = 3, nrow = length(links)))
  names(df_toc) <- c("file", "chapter", "nr")
  
  df_toc$file <- links
  df_toc$chapter <- text
  
  df_toc$file <- gsub("#.*","",df_toc$file)
  df_toc$file <- paste("data/words_of_radiance/text/", df_toc$file, sep = "")
  df_toc$nr <- gsub("(.*)\\..*","\\1",df_toc$chapter)
  df_toc$nr <- gsub("\\s", "", df_toc$nr)
  df_toc$chapter <- gsub(".*\\. ","",df_toc$chapter)
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


textOfChapter <- function(file){
  xml_data <- htmlTreeParse(file, useInternal = TRUE)
  whichNode <- "//p[@class='co']//text() | //p[@class='tx']//text()"
  text <- unlist(xpathApply(xml_data, whichNode, xmlValue))
  return(text)
}



toc_wor_path <- "data/words_of_radiance/text/part0005.html"
WordsOfRadiance <- getTableOfContents(toc_wor_path)
WordsOfRadiance$text <- NA
for(k in 1:nrow(WordsOfRadiance)){
  text <- textOfChapter(WordsOfRadiance$file[k])
  text <- paste(text, collapse = " ")
  WordsOfRadiance$text[k] <- text
}

rm(k, text, toc_wor_path, getTableOfContents, textOfChapter)

















