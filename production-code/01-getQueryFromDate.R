library(RMySQL)
library(parallel)
library(magrittr)
library(dplyr)
library(stringr)

# read arguments from command line
args <- commandArgs(trailingOnly = T)

# target_dt
target_dt = args[1]
target_dt_plus_one = (as.Date(target_dt) + 1) %>% as.character()

# ngram size
ngram_size <- as.numeric(args[2])

# location of output file
outpath <- args[3]


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
if (is.na(ngram_size) | !(ngram_size > 1 & ngram_size <= 6)) {
  stop('[ERROR]: Invalid n. n should be a number and must be from 1 to 6')
}


# connect to MySQL database
con <- dbConnect(MySQL(),
	user = 'TextLab',
	password = 'Carlo!and!Duong=house',
	host = 'localhost',
	dbname = 'textsim'
	)



#### HELPER FUNCTIONS

# list.in creates the necessary string needed for the WHERE x IN type of SQL queries
# returns a string (e.g. "(segment1, segment2, segment3)")
list.in <- function(list) {
	paste(list, collapse=",")
}

# textlines_to_1gram reads the textlines dataframe
# sanitizes text by removing punctuations
# returns data frame of word
textlines_to_1gram <- function(df) {
	mclapply(1:nrow(df), mc.cores=8, FUN = function(row) {
		phrase = df$LineText[row]
		#  create a one-one gram and associate the time
        words <- enc2utf8(phrase) %>%						# try to resolve encoding issues
        	{gsub("\\[.*?\\]", "", .)} %>% 					# remove words enclosed in brackets
            {gsub("[a-zA-Z0-9]*?:", "", ., perl=T)} %>% 	# remove the likes of [Reporter]:
            str_replace_all("(?<=[[:alnum:]])-*(?=[[:alnum:]])", "") %>% # remove dash in dashed words
            str_replace_all("[^'[:alnum:] ]", "") %>% 		# remove other characters except alnum and '
            str_trim("both") %>%							# remove padding
			strsplit('\\s+') %>%							# split by spaces
            unlist()										# get vectors

        # check if empty
        if (length(words) ==  0) {
        	# cat("EMPTY")
        	data.frame(Segment_ID = character(0),
                words = character(0), 
                LineNumber = character(0),
                LineDateTime = character(0),
                stringsAsFactors = F) %>%
            return()
		} 
        else {
            data.frame(Segment_ID = rep(df$Segment_ID[row], times=length(words)),
                words = words, 
                LineNumber = rep(df$LineNumber[row], times=length(words)),
                LineDateTime = rep(df$LineDateTime[row], times=length(words)),
                stringsAsFactors = F) %>%
            return()
		}
	}) %>%
	{do.call('rbind', .)} %>%
	return()
}

# tagged_ngram reads the 1-gram words data frame
# returns a data frame of n-grams tagged with the line number and timestamp of the first word
tagged_ngram <- function(words.df, n) {
    dat <- mclapply(1:(nrow(words.df)-n+1), mc.cores=8, FUN =
             function(row){
			# create the row elements	
            c(Segment_ID = words.df$Segment_ID[row],
		 	  ngram = paste(words.df$words[row:(row+n-1)] %>%
              			        str_trim(),
                            collapse=" "), 
              LineNumber = words.df$LineNumber[row], 
              LineDateTime = words.df$LineDateTime[row])
             }) %>%
    		{do.call('rbind', .)} %>%
    		as.data.frame(stringsAsFactors = F)
  return(dat)
}


#### MAIN ####


# get relevant segment ids
query_ids <- sprintf("SELECT DISTINCT Segment_ID FROM textlines WHERE LineDateTime >= '%s' and  LineDateTime <= '%s';", target_dt, target_dt_plus_one)

sprintf("[QUERYING]: %s", query_ids)
query_ids_results <- dbGetQuery(con, query_ids)$Segment_ID

print("[INFO]: Finished querying the segments in the selected date")


# get textlints of the segment ids
query_textlines <- sprintf("SELECT * FROM textlines WHERE Segment_ID IN (%s)", list.in(query_ids_results))

sprintf("[QUERYING]: %s", query_textlines)
query_textlines_results <- dbGetQuery(con, query_textlines)
str(query_textlines_results)

head(query_textlines_results)
print("[INFO]: Finished querying the texlines for all segments in the selected date")

# get 1-gram df
textlines_1gram <- textlines_to_1gram(query_textlines_results) 
str(textlines_1gram)

head(textlines_1gram)

# get 6-gram df
textlines_6gram <- tagged_ngram(textlines_1gram, ngram_size)


# save the file
saveRDS(textlines_1gram, file=paste0(outpath, '/', target_dt, '_processed_' ,1, 'gram', '.RDS'))
saveRDS(textlines_6gram, file=paste0(outpath, '/', target_dt, '_processed_' ,ngram_size, 'gram', '.RDS'))


