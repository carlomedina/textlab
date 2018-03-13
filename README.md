# TextLab: Story Similarity in TV Transcripts
Work in collaboration with the Wesleyan Media Project

## Some definitions
### Segment
A full transcript of a given news show. It includes a portions of transcripts 'pre-show', and 'post-show', as well as the transcripts from adds. They are usually generated automatically (STT-generator)

### Story 
A subsection of a segment that talks about a specific topic (i.e. a news anchor talking about the weather, a sound bite about a local news occurrence)

## Introduction
The aim of this project is to identify stories that are similar across different segments across different stations. 

## Method
The current approach is based on some n-gram heuristics that we identified.

### Pre-processing 
The transcripts for a given day is queried from a MariaDB server. The transcripts are pre-processed by removing speech tags (e.g. "[Reporter]:"), removing punctuations (e.g. all punctuations, hyphens in hyphenated words are removed, only quotes in contractions are kept). The resulting transcipts are then converted to n-grams, with the associated line number and timestamp for the n-gram. In our case, n=6.

### Story Identification
The counts for each unique n-gram in the entire corpus for the day is calculated. The counts were used to score the n-gram in the transcript. The count-index graph of the segment can then be plotted. One can notice that some subsections of segments that have low n-gram counts (close to one), while others have high n-gram counts. These subsections of segments are the candidates for our stories since they indicate that the sequence of n-gram are found in another segment. The entire process is demonstrated graphically below.

To obtain the story indices, DBSCAN was used on the count-index data to find the groups/cluster. To help with the clustering, the following conversion was made to the value of count: if 1>=count, then count=0, else if 2<=count<5, then count=0.9, else count=1. The indices of the stories are then collected (min index and max index per cluster).

### Story Matching
The stories that were identified were compared pairwise and a similarity score is calculated. The similarity score is a pseudo-Jaccard Index that is not symmetric. Let A, B be the n-gram vector of story A and story B. The similarity score of A to B is $S_{AB} = \frac{|A \cap B|}{|A|}$. The reason for this is to obtain a directed graph later on.

The process of pairwise comparison yields a matrix of similarity scores. The matrix is used to form the network of stories where two stories are connected (with direction) if their similarity score is above a threshold. Network clustering is then form to group similar stories together.
