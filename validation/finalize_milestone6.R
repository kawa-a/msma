# Finalize local Milestone 6 evidence after compatibility and revdep checks.
root <- if (file.exists("src.r")) normalizePath(".") else normalizePath("..")
compat_html <- file.path(root, "validation", "msma_3.2_vs_4.0_compatibility_test.html")
compat_tar <- file.path(root, "validation", "msma_3.2.tar.gz")
check_log <- file.path(root, "msma.Rcheck", "00check.log")
mand_logs <- c(
  file.path(root, "validation", "mand-revdep", "mand.Rcheck", "00check.log"),
  file.path(root, "mand.Rcheck", "00check.log")
)
mand_logs <- mand_logs[file.exists(mand_logs)]
if (!length(mand_logs)) stop("Missing mand reverse-dependency check log")
if (length(mand_logs) > 1L) {
  mand_logs <- mand_logs[which.max(file.info(mand_logs)$mtime)]
}
mand_log <- mand_logs[1L]
required <- c(compat_html, compat_tar, check_log, mand_log)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing audit artifacts: ", paste(missing, collapse = ", "))
html <- paste(readLines(compat_html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (!grepl("Failed: 0", gsub("<[^>]+>", " ", html), fixed = TRUE)) stop("Compatibility report is not Failed: 0")
msma_log <- readLines(check_log, warn = FALSE)
if (!any(trimws(msma_log) == "Status: OK")) stop("msma R CMD check is not Status: OK")
mand_lines <- readLines(mand_log, warn = FALSE)
if (any(grepl("^Status:.*(ERROR|WARNING)", mand_lines))) stop("mand audit has ERROR or WARNING")
files <- c("msma_4.0.tar.gz", "msma.Rcheck/00check.log",
           "validation/msma_3.2.tar.gz",
           "validation/msma_3.2_vs_4.0_compatibility_test.html",
           sub(paste0("^", normalizePath(root, winslash = "/"), "/?"), "",
               normalizePath(mand_log, winslash = "/")))
paths <- file.path(root, files)
md5 <- tools::md5sum(paths)
out <- data.frame(file = files, md5 = unname(md5), stringsAsFactors = FALSE)
utils::write.table(out, file.path(root, "validation", "RELEASE-MANIFEST.tsv"),
                   sep = "\t", row.names = FALSE, quote = FALSE)
print(out)
cat("Milestone 6 local audit evidence is complete.\n")


