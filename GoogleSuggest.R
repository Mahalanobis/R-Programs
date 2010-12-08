# modified as Richie Cotton's comment -> http://www.r-chart.com/2010/07/what-search-engines-think-people-want.html

googleSuggest = function(query = '', .opts = list())
{
  require(XML)
  require(RCurl)
  query <- curlEscape(query)
  the_url <- paste('http://google.com/complete/search?output=toolbar&q=', query, sep='')
  webpage <- getURL(the_url, .opts=.opts)
  doc = xmlTreeParse(webpage, useInternal=TRUE)

  search_string <- sapply(getNodeSet(doc, "//CompleteSuggestion/suggestion"), function(elt){xmlGetAttr(elt, "data")})
  count <- sapply(getNodeSet(doc, "//CompleteSuggestion/num_queries"), function(elt){xmlGetAttr(elt, "int")})
  dfr <- data.frame(search_string = search_string, count = count, stringsAsFactors = FALSE)
  dfr$count <- as.numeric(dfr$count)
  dfr
}

# This function iterating through to get each letter of the alphabet... 
# for example
#   http://google.com/complete/search?output=toolbar&q=how%20can%20i%a
#   http://google.com/complete/search?output=toolbar&q=how%20can%20i%b
#   ...
#
# I especially like the concise syntax for iterating through the letters of the alphabet
# http://stackoverflow.com/questions/1439513/creating-a-sequential-list-of-letters-with-r
#

ggoogleSuggestAll = function(query = '')
{
queries <- paste(query, letters)
results_list <- lapply(queries, googleSuggest)
dfr <- do.call(rbind, results_list)
dfr <- dfr[order(dfr$count, decreasing = TRUE), ]
dfr
}