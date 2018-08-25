FilterParsedGBFF <- function (path.to.parsed.gbff.csv, path.to.child.taxids.csv) {
  parsed.gbff <- read.csv(file= path.to.parsed.gbff.csv)
  child.taxids <- read.csv(file= path.to.child.taxids.csv, sep="|", header = T)
  result.data.frame <- subset(parsed.gbff, taxonId %in% child.taxids$tax_id)
  cat("Writing filtered.csv")
  write.csv(result.data.frame, "csv_files/filtered.csv",quote = F, row.names = F)
  return(droplevels(result.data.frame))
}