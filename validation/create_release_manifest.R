# Run from the development root after final build and validation.
files <- c(
  "msma_4.0.tar.gz",
  "msma.Rcheck/00check.log",
  "validation/msma_3.2_vs_4.0_compatibility_test.html"
)
missing <- files[!file.exists(files)]
if (length(missing)) stop("Missing release artifacts: ", paste(missing, collapse = ", "))
md5 <- tools::md5sum(files)
out <- data.frame(file = names(md5), md5 = unname(md5), stringsAsFactors = FALSE)
utils::write.table(out, "validation/RELEASE-MANIFEST.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
print(out)


