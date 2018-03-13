# read arguments from command line
args <- commandArgs(trailingOnly = T)

# file to process
infile <- args[1]

# ngram size
n <- as.numeric(args[2])

# location of output file
outpath <- args[3]


# check if file exists and path exists
if (!file.exists(infile)) {
  stop('[ERROR]: File does not exists \n')
}

if (!file.exists(outpath)) {
  cat('[WARNING]: Path for output file does not exists \n')
  cat('[WARNING]: Creating path \n')
  dir.create(outpath, showWarnings = F)
  if (file.exists(outpath)) {
    cat('[INFO]: The location of the path to output file will be in: \n')
    cat(paste0('[INFO]: ', normalizePath(dirname(outpath)), '/', outpath, '\n'))
  }
}

# check if n is valid 
if (is.na(n) | !(n > 1 & n <= 6)) {
  stop('[ERROR]: Invalid n. n should be a number and must be from 1 to 6')
}

cat('[INFO]: Starting the program processing \n')
# load libraries
library(dplyr)
library(magrittr)
library(stringr)
library(readr)
library(XML)
library(parallel)

# functions

# taggedNgram reads the 1-gram words data frame
# returns a data frame of n-grams tagged with the line number and timestamp of the first word
taggedNgram <- function(words.df, n) {
    dat <- mclapply(1:(nrow(words.df)-n+1), mc.cores=8, FUN =
             function(x){
		
               c(id=words.df$id[x],
		 ngram = paste(words.df$word[x:(x+n-1)] %>%
                                str_trim(),
                              collapse=" "), 
                 linenumber = words.df$linenumber[x], 
                 timestamp = words.df$timestamp[x])
             }) %>%
    {do.call('rbind', .)} %>%
    as.data.frame(stringsAsFactors = F)
  cat(paste0('[INFO]: Finished processing ', words.df$id[1], '\n'))
  return(dat)
}


# parseTranscript reads the df containing transcripts
# takes the xml_body column
# returns a list of data frame
# each data frame corresponds to a transcript converted to 1-grams
# each row is a word from the transcript and tagged with linenumber and timestamp
parseTranscriptDf <- function(df) {
  mclapply(1:nrow(df), mc.cores=8,
        FUN=
          function(x) {
            # create id
            id = paste0(df$UniqueIdentifier[x], 
                        '--',
                        as.character(df$startDateTime[x]) %>%
                          gsub('[[:punct:][:space:]]', '-', .))
            cat(paste0('[INFO]: Processing document: ', id, '\n'))
            
            # get only the transcript body
            str_extract(df$xml_body[x], '<StationText>([[:print:][:space:]]*?)</StationText>') %>%
              str_replace_all('[[:space:]]', ' ') %>%
              xmlToList() %>%
              # process the text in a given transcript
              # take the text, line number, timestamp
              lapply(FUN=
                       function(y){
			# create a one-one gram and associate the time
                         words <- y[[1]][[1]] %>%
                           {gsub("\\[.*?\\]", "", .)} %>%
                           {gsub("[a-zA-Z0-9]*?:", "", ., perl=T)} %>%
                           str_replace_all("[^'-[:alnum:] ]", ' ') %>%
                           str_trim() %>%
			   strsplit('\\s+') %>%
                           unlist

                         if (length(words) ==  0) {
                           return(NULL)
			 } 
                         else {
                         data.frame(id = id,
                                    words = words, 
                                    linenumber = rep(y[[2]][[1]], n=length(words)),
                                    timestamp = rep(y[[2]][[2]], n=length(words)),
                                    stringsAsFactors = F) %>%
			 return()
                         }
                       }) %>%
              do.call('rbind', .)
          })
}


# main
t <- proc.time()
cat(paste0('[INFO]: Reading transcript csv: ', infile, '\n'))
transcripts.df <- read_csv(infile)

cat(paste0('[INFO]: Parsing the transcript \n'))
words.df <- parseTranscriptDf(transcripts.df)


ngrams.df.list <- lapply(words.df, FUN = function(x) {
  cat(paste0('[INFO]: Converting ', x$id[1], ' to ', n, '-gram \n'))
  return(taggedNgram(x, n))
})

  # bulk of the processing happens here
assign(paste0('processed_' ,n, 'gram_', gsub('[/].*[/]|[.]csv|-', '_', infile)), ngrams.df.list)



# save processed file as an .RDS file
saveRDS(ngrams.df.list, 
     file=paste0(outpath, '/', 'processed_' ,n, 'gram_', gsub('[/].*[/]|[.]csv|-', '_', infile, perl = T), '.RDS'))

print(proc.time()-t)



