library(magrittr)
library(dplyr)
library(tidytext)
library(dbscan)
library(tm)
library(readr)
library(stringr)
library(RMySQL)
library(igraph)
library(jsonlite)
library(parallel)
library(lubridate)

# read arguments from command line
args <- commandArgs(trailingOnly = T)

# RDS file containing the 6-gram df for the requested date
infile = args[1]
segment_date = infile %>% str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")

# location of output file
outpath <- args[2]



if (!file.exists(outpath)) {
  cat('[WARNING]: Path for output file does not exists \n')
  cat('[WARNING]: Creating path \n')
  dir.create(outpath, showWarnings = F)
  if (file.exists(outpath)) {
    cat('[INFO]: The location of the path to output file will be in: \n')
    cat(paste0('[INFO]: ', normalizePath(dirname(outpath)), '/', outpath, '\n'))
  }
}

# check for debug command
if (length(args) > 2) {
  if (args[3] == "debug") {
    isDebug = T
  }
}  else {
  isDebug = F
}

#### HELPER FUNCTIONS ####

# where_in_vect2string creates the necessary string needed for the WHERE x IN type of SQL queries
# returns a string (e.g. "(segment1, segment2, segment3)")
where_in_vect2string <- function(list) {
  paste(list, collapse=",")
}


# get ngrams specified by the index
# returns a vector of ngrams
get_stories_ngram_vector <- function(ngramdata, id_to_search, first, last) {
  # list of ngrams
  ngramdata %>%
    dplyr::filter(Segment_ID == id_to_search) %$%
    ngram[first:last]
}

# get ngrams specified by the index and transform the ngrams into text
# returns the text of the story
get_stories_ngram_text <- function(ngramdata, id_to_search, first, last) {
  vector <- get_stories_ngram_vector(ngramdata, id_to_search, first, last)   # get list of ngrams as vector
  vector[-length(vector)] %>%           # take all the 6 grams except the last one
    lapply(FUN = function(x) {
      strsplit(x, ' ') %>%                # split the 6 gram
        unlist() %>%                      # turn to vector
        .[1]                              # take the first element (aka the first word)
    }) %>%
    paste(collapse = " ") %>%             # combine the first words together
    paste(vector[length(vector)]) # add the last ngram
}

# get stories in segments where the ngrams of that stories was found in 5 or more other texts
# also, a story must contains more than 10 ngrams
# the parameters/threshold here are only for exploration (not necessarily the optimal values)
# returns a data frame of stories
get_stories <- function(segment_df, id) {

  # get the subset relevant to the specific segment
  segment_df <- segment_df %>% filter(Segment_ID == id)
  station <- segment_df$station[1]

  # get count-index data for the ngrams in the dataframe
  mat <- data.frame(index = 1:nrow(segment_df),
                    count_score = segment_df$count %>%
                          cut(breaks = c(-Inf, 2, 5, Inf),    # break the counts into categories
                              labels = c(0, 0.9, 1)) %>%      # 0 - 0:2, 0.9 - 3:4, 1 - 5:hi
                          as.character() %>%                  # turn factors to character
                          as.numeric()) %>%                   # turn character to numeric
          na.omit() %>%                                       # remove NA if they exist
          filter(count_score > 0) %>%                         # remove those ngrams with low counts
          as.matrix()                                         # turn to matrix for dbscan

  if (nrow(mat) == 0) {
    return(NULL)
  }

  else {
    db <- dbscan(mat, 20, 5)

    group <- data.frame(x = mat[,1],
                        y = mat[,2],
                        cluster = db$cluster) %>%
            filter(cluster!=0)
  
    if (nrow(group) == 0) {
      return(NULL)
    } else {
      stories_tagging <- segment_df %>%
                            mutate(index = 1:n()) %>%
                            merge(group, by.x = "index", by.y = "x", all = F)

      stories <- stories_tagging %>%
                    filter(cluster != 0) %>%
                    group_by(cluster) %>%
                    arrange(index) %>%
                    summarise(Segment_ID = first(id),
                              station = first(station),
                              story_ID = paste(id, first(cluster)),
                              first = ifelse(min(index) - 15 < 0, 0, min(index) - 15),    # adjust the window
                              last = max(index),
                              start_line = min(LineNumber),
                              end_line = max(LineNumber),
                              start_timestamp = first(LineDateTime),
                              end_timestamp = last(LineDateTime),
                              length = (last-first)+6) %>%
                    select(-cluster) %>%
                    filter(length > 10 & length < 150) 

      if (nrow(stories) == 0) {
        return(NULL)
      } 
      # get text of each stories
      text <- lapply(1:nrow(stories), function(row) {
                        get_stories_ngram_text(segment_df, stories$Segment_ID[row], stories$first[row], stories$last[row])
              }) %>%
              unlist() 
      stories$text <- text
      
      return(stories)
    }
  }
}

# calculate the proportion of the intersection of two ngram vectors wrt to the first ngram vector
# NOTE: not symmetric
# returns a double from 0 to 1
stories_similarity <- function(ngram_story1, ngram_story2) {
  sizeIntersection <- length(intersect(ngram_story1, ngram_story2))
  similarity <- sizeIntersection/length(ngram_story1)
  return(similarity)
}


calculate_similarity_matrix <- function(stories, all_stories_ngrams) {
  # get similarity metric between two stories
  indices <- expand.grid(i = 1:nrow(stories),
                        j = 1:nrow(stories))

  mclapply(1:nrow(indices), mc.cores=8, function(ij) {
    print(paste("[INFO]: Calculating similarity between", indices$i[ij], indices$j[ij]))
    # if i and j are equal, return NA
    if (indices$i[ij] == indices$j[ij]) {
      return(NA)
    } else {
      ngram_story1 <- all_stories_ngrams[[ indices$i[ij] ]]
      ngram_story2 <- all_stories_ngrams[[ indices$j[ij] ]]

      return(stories_similarity(ngram_story1, ngram_story2))
    }
    
    }) %>%
  unlist() %>%
  matrix(nrow = nrow(stories), ncol = nrow(stories), byrow = F) %>%
  return()
}



# query video files for the stories identified
# returns a data frame of videos file infos
get_video_files <- function(con, Segment_IDs) {
  query_videofiles <- sprintf("SELECT * from video WHERE Segment_ID IN (%s);", 
                              where_in_vect2string(Segment_IDs))
  cat(sprintf("[INFO]: Querying videos using the following query \n%s\n" ,query_videofiles))
  query_videofiles_results <- dbGetQuery(con, query_videofiles)
  return(query_videofiles_results)
}

# finds the closest video that contains the story
# returns a string containing the location of the video file
get_closest_video <- function(story_segment_ID, story_timestamp, video_df){
  ### PLEASE ADD ERROR HANDLING PLEASEEEE!!!!
  story_timestamp <- ymd_hms(story_timestamp, tz = "UTC")
  
  vids <- video_df %>%
    filter(Segment_ID == story_segment_ID) %>%
    {cbind(., data.frame(story_timestamp = rep(story_timestamp, nrow(.))))} %>%
    mutate(search = ymd_hms(StartTime, tz = "UTC"),
           diff =  difftime(story_timestamp, search)) %>%           # find a video that is shortly before the story timestamp
    filter(diff > 0) %>%
    arrange(diff) 
  
  if (nrow(vid) == 0) {
    return("FileNotFound.mp4")
  } else {
    vids %>%
      {paste(.[1,2], .[1,1], sep="/")} %>%    # paste the location folder and the filename
      str_trim("both") %>%
      return()
  }
    
}

# calls the system cp commande to copy the relevant video files to the public_web tmpVideo folder
# returns nothing
copy_videos_to_publicweb <- function(directories) {
  
  # form the following command
  # cp /path/to/{file1,file2,...} /path/to/move
  
  directories %>% 
      paste(collapse=",") %>% 
      {sprintf("{%s}", .)} %>% 
      {sprintf("cp /data/1/wesmediafowler/TVEyesTasks/Campaigns/%s /data/1/wesmediafowler/public_web/sites/tmpVideo", .)} %>%
      system()
}


#### MAIN ####


  #### READING DATA AND CONNECTION TO DATABASE

# read the RDS file
ngram_basic <- readRDS(infile)

# create a subset of the data when debug is requested
if (isDebug) {
  sample_segment_ID<- ngram_basic$Segment_ID %>% unique %>% sample(300)
  ngram_basic <- ngram_basic %>%
      filter(Segment_ID %in% sample_segment_ID)
}

# read credentials
creds <- read_lines(".pass")
# connect to MySQL database
con <- dbConnect(MySQL(),
  user = creds[1],
  password = creds[2],
  host = 'localhost',
  dbname = 'textsim'
  )

  #### PROCESSING THE STORIES DATA FRAME

ids_to_query <- ngram_basic$Segment_ID %>% unique()

# query the stations of Segment_ID
query_stations <- sprintf("SELECT id, station FROM segment WHERE id IN (%s)", where_in_vect2string(ids_to_query))

sprintf("[QUERYING]: %s", query_stations)
query_stations_results <- dbGetQuery(con, query_stations)

print("[INFO]: Finished querying the stations of the Segment_IDs")
head(query_stations_results)

# add index to the ngram sequence of each transcript
ngram_index <- ngram_basic %>%
  group_by(Segment_ID) %>%
  mutate(index = 1:n()) %>%
  ungroup()

# count the number of times a particular ngram appears in all of the transcripts in a given day
count_ngrams <- ngram_basic %>%
  merge(query_stations_results, by.x="Segment_ID", by.y="id") %>%
  group_by(station) %>%
  select(station, ngram) %>%
  distinct() %>%
  group_by(ngram) %>%
  summarise(count = n()) %>%
  ungroup()

# combine count information to the ngram dataframe with index
# then add the station details too
ngram_data <- merge(ngram_index, count_ngrams, by="ngram") %>%
  arrange(Segment_ID, index) %>%
  merge(query_stations_results, by.x="Segment_ID", by.y="id", all = F)





# # structure of the stories data frame
# stories <- data.frame(Segment_ID = integer(0),
#                       station = character(0),
#                       story_ID = character(0),
#                       first = integer(0),
#                       last = integer(0),
#                       start_line = integer(0),
#                       end_line = integer(0),
#                       start_timestamp = integer(0),
#                       end_timestamp = integer(0),
#                       length = integer(0)
#                       )


# take Segment_IDs to iterate through
Segment_IDs <- ngram_data$Segment_ID %>% unique()

# get all stories
stories_list <- mclapply(Segment_IDs, mc.cores=8, function(id) {
  get_stories(ngram_data, id)
  })

stories <- do.call('rbind', stories_list)

# get all ngrams for all the stories for scoring later
all_stories_ngrams <- mclapply(1:nrow(stories), mc.cores=8, function(row) {
          get_stories_ngram_vector(ngram_data, 
                                  stories$Segment_ID[row], 
                                  stories$first[row], 
                                  stories$last[row])
  })




# get similarity metric between two stories
similarity_scores <- calculate_similarity_matrix(stories, all_stories_ngrams)


# create stories adj mat based on a threshold
stories_adj_mat <- similarity_scores > 0.7


stories_graph <- graph_from_adjacency_matrix(stories_adj_mat, mode = "directed")
cluster <- clusters(stories_graph, mode = "weak")$membership
stories$cluster <- cluster %>% unlist()                   # it returns a list sometimes

pdf(sprintf("%s/plot.pdf", outpath))
plot(stories_graph, mode = "weak", edge.arrow.size=0.1,
     vertex.size=5,
     vertex.color=cluster,
     vertex.label=NA)
dev.off()


# get entire video data for relevant stations
stories_segment_ID <-  stories$Segment_ID %>% unique()
print(stories_segment_ID)
videofiles_df <- get_video_files(con, stories_segment_ID)


# add the filename of the closest video file
stories_videofiles <- mclapply(1:nrow(stories), mc.cores=8, function(row) {
  get_closest_video(stories$Segment_ID[row], stories$start_timestamp[row], videofiles_df)
  }) %>% unlist()

stories$location <- stories_videofiles
stories$url <- stories_videofiles %>% str_extract("[A-Z0-9]*\\.mp4$")



#### PREPARING THE SEGMENTS FOR EXPORT ####

stories_export <- stories[order(stories$cluster), ]


#### GET VIDEO FILES TO TRANSFER TO TMPVIDEO ####
stories_export %$%
  copy_videos_to_publicweb(location)


stories_group <- stories_export %>%
  group_by(cluster) %>%
  arrange(desc(length)) %>%
  summarise(sample_story = text[1],
            num_transcripts=n()) %>%
  arrange(desc(num_transcripts))




# creating the JSON output
json <- list()
for (i in 1:nrow(stories_group)) {
  cluster_to_process = stories_group$cluster[i]
  segment_list <- split(stories[stories$cluster == cluster_to_process, ] , 
                        seq(nrow(stories[stories$cluster == cluster_to_process, ]))) %>%
    lapply(function(story) {return(story %>% as.list())}) %>% 
    unname() %>% 
    list() # cover with a "master list"
  json[[i]] <- c("cluster_message"=stories_group$sample_story[i], "stations"=segment_list)
}
json <- list("date"=segment_date, "clusters"=json)

json %>% 
  jsonlite::toJSON(pretty = T, auto_unbox = T) %>% 
  write_lines(sprintf('%s/%s_extracted_%s.json', outpath, segment_date, Sys.Date()))


save.image(sprintf("processing_%s", segment_date))


