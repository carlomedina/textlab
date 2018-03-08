if !(require(RMySQL)) {
	install.packages("RMySQL")
	library(RMySQL)
}

library(RMySQL)


# helper functions
list.in <- function(list) {
	paste(list, collapse=",")
}

textlines_to_1gram <- function(df, )


con <- dbConnect(MySQL(),
	user = 'TextLab',
	password = 'Carlo!and!Duong=house',
	host = 'localhost',
	dbname = 'textsim'
	)

# target_dt
target_dt = '2017-07-25'
target_dt_plus_one = '2017-07-26'


# get relevant segment ids
query_ids <- sprintf("SELECT DISTINCT segment_id FROM textlines WHERE linedatetime >= '%s' and  linedatetime <= '%s' LIMIT 10;", target_dt, target_dt_plus_one)

query_ids_results <- dbGetQuery(con, query_ids)$segment_id


# get textlints of the segment ids
query_textlines <- sprintf("SELECT * FROM textlines WHERE segment_id IN (%s)", list.in(query_ids_results))

query_textlines_results <- dbGetQuery(con, query_textlines)


