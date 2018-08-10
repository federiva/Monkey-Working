load("R/global_environment_post_1.RData")
# Style following https://google.github.io/styleguide/Rguide.xml
# Packages to be used
library(ape)
library(seqinr)
library(rentrez)

# Printing out the available databases
entrez_dbs()
# Look into the taxonomy database of NCBI for the entry Caviidae
taxonomy.search <- entrez_search("taxonomy", "Hystricomorpha")
# The NCBI taxonomy id of Caviidae 
ncbi.taxon.id <- taxonomy.search$ids
# Look for the mitochondrial sequences matching control region in Hystricomorpha
query.hystricomorpha <- c("txid33550[Organism:exp] AND gene_in_mitochondrion[PROP] AND complete genome")
# Using entrez_search()
search.hystricomorpha <- entrez_search(db = "nuccore", term = query.hystricomorpha, retmax=1000)
#We get 92 hits
summary_hystricomorpha <- entrez_summary(db = "nuccore", id = search.hystricomorpha$ids)  


titles_hystricomorpha <- extract_from_esummary(summary_hystricomorpha, c("slen","gi","organism","taxid", "uid"))

# exploring the titles of the results
FilteringTitles <- function(x, filter.by, operation = "OR") {
  # x is an object from entrez_search
  # filter.by is a string with the words used to filter the titles
  # The function returns a dataframe of rentrez ncbi ids and titles
  # operation is a string which has the logical operation to be performed during the filtering process. It could be OR/AND/NOT. The default value is OR
  output.titles <- c()
  output.ids <- c()
  # subsetting the data if we have more than 100 results
  if (length(x$ids) > 100) {
    # Get the indexes to subset the chunks of data
    lower.margin <- seq(from = 1, to = length(x$ids), by = 100)
    upper.margin <- c(seq(from = lower.margin[2]-1, to = length(x$ids), by = 100),length(x$ids))
    # For each of the indexes: Subset the data; Get the summaries, Extract the titles
    for (i in c(1:length(lower.margin))) {
      cat("Getting the summary", i, "of", length(lower.margin), "...")
      rentrez.summary.tmp <- entrez_summary("nuccore", x$ids[lower.margin[i]:upper.margin[i]])
      cat(" OK.\nWaiting 2 seconds\n")
      Sys.sleep(2) #wait two secs until get the new summary
      # Extracting the titles from the rentrez summary object
      titles.tmp <- extract_from_esummary(rentrez.summary.tmp, "title")
      title.counter <- 1
      ids.titles.tmp <- names(titles.tmp)
      for (each.title in titles.tmp) {
        is.true.and <- TRUE
        or.test.counter <- 0
        is.true.not <- FALSE
        id.title <- ids.titles.tmp[title.counter]
        for (each.term in filter.by) {
          if (operation == "AND") {
            # If we found that one of the terms is not found then it breaks the loop to the next word
            if (!(grepl(pattern = each.term, x = each.title, ignore.case = T))) {
              is.true.and <- FALSE
            }
          } else if (operation == "OR") {
              if (!(grepl(pattern = each.term, x = each.title, ignore.case = T))) {
              # or.test.counter adds 1 if the term is not found in the title, when the counter is equal to the amount of terms used in the test for every title, then it means that no word was found.
                or.test.counter <- or.test.counter + 1
              }
          } else if (operation == "NOT") {
              if (!(grepl(pattern = each.term, x = each.title, ignore.case = T))){
                is.true.not <- TRUE
              } else {
                is.true.not <- FALSE
                break
              }
            }
        }
        if (operation == "AND") {
          if (is.true.and) {
            output.titles <- c(each.title, output.titles)
            output.ids <- c(id.title, output.ids)
          }
        } else if (operation == "OR") {
          if (or.test.counter < length(filter.by)) {
            output.titles <- c(each.title, output.titles)
            output.ids <- c(id.title, output.ids)
          }
        } else if (operation == "NOT") {
          if (is.true.not) {
            output.titles <- c(each.title, output.titles)
            output.ids <- c(id.title, output.ids)
          }
        }
        title.counter <- title.counter + 1
      }
    }
  }
  output.data.frame <- data.frame(output.ids, output.titles)
  colnames(output.data.frame) <- c("ids", "titles")
  return(output.data.frame)
}  

filtered.titles.hys <- FilteringTitles(custom.control.region.search.hystricomorpha, c("control"))

filtered.titles.hys <- FilteringTitles(custom.control.region.search.hystricomorpha, filter.by = c("partial"), operation = "NOT")

str(filtered.titles.hys)

  
save.image("R/global_environment_post_1.RData")



