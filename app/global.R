# load library of targerts
load("../data/environment-10-17.2017.RData")
TARGETNGRAMSDF = ngrams
MIN_VALUE_TARGET_ID <- min(TARGETNGRAMSDF$id)
MAX_VALUE_TARGET_ID <- max(TARGETNGRAMSDF$id)
