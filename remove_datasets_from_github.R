# Remove dataset files from GitHub while keeping local copies.
#
# How to run in RStudio:
# 1. Open this file.
# 2. Click Source.
#
# What this does:
# - Finds tracked dataset files in this repository.
# - Removes them from Git tracking with `git rm --cached`, so your local files stay.
# - Adds data-file patterns to .gitignore so the files do not get re-added later.
# - Commits the cleanup.
# - Pushes the commit to GitHub.
#
# Important:
# This removes datasets from the current GitHub branch going forward. It does not
# erase datasets from old Git commit history. If your data policy requires old
# commits to be scrubbed too, the repository history must be rewritten separately.

push_to_github <- TRUE
commit_message <- "Remove datasets from repository"

dataset_extensions <- c(
  "csv", "tsv",
  "xls", "xlsx", "xlsm",
  "rds", "rda", "rdata",
  "sav", "dta", "sas7bdat",
  "parquet", "feather", "arrow",
  "db", "sqlite", "sqlite3"
)

dataset_directories <- c(
  "data",
  "datasets",
  "raw_data",
  "raw-data",
  "processed_data",
  "processed-data"
)

git <- function(args, allow_failure = FALSE) {
  command <- paste("git", paste(shQuote(args), collapse = " "))
  message("\n$ ", command)

  output <- system2("git", args = shQuote(args), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }

  if (length(output) > 0) {
    message(paste(output, collapse = "\n"))
  }

  if (!allow_failure && status != 0L) {
    stop("Git command failed: ", command, call. = FALSE)
  }

  invisible(output)
}

repo_root <- git(c("rev-parse", "--show-toplevel"))[1]
setwd(repo_root)
message("Repository: ", repo_root)

tracked_files <- git(c("ls-files"))

file_extension <- function(path) {
  name <- basename(path)
  has_extension <- grepl("\\.", name)
  extension <- ifelse(has_extension, sub("^.*\\.([^.]+)$", "\\1", name), "")
  tolower(extension)
}

is_in_dataset_directory <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)
  vapply(
    parts,
    function(path_parts) any(tolower(path_parts) %in% tolower(dataset_directories)),
    logical(1)
  )
}

dataset_files <- tracked_files[
  file_extension(tracked_files) %in% dataset_extensions |
    is_in_dataset_directory(tracked_files)
]

dataset_files <- sort(unique(dataset_files))

if (length(dataset_files) == 0) {
  message("No tracked dataset files were found.")
} else {
  message("\nDataset files that will be removed from GitHub but kept locally:")
  message(paste(" -", dataset_files, collapse = "\n"))

  chunks <- split(dataset_files, ceiling(seq_along(dataset_files) / 50))
  for (chunk in chunks) {
    git(c("rm", "--cached", "--", chunk))
  }
}

ignore_lines <- c(
  "",
  "# Dataset files are not committed because of data permissions.",
  "*.csv",
  "*.tsv",
  "*.xls",
  "*.xlsx",
  "*.xlsm",
  "*.rds",
  "*.rda",
  "*.RData",
  "*.sav",
  "*.dta",
  "*.sas7bdat",
  "*.parquet",
  "*.feather",
  "*.arrow",
  "*.db",
  "*.sqlite",
  "*.sqlite3",
  "data/",
  "datasets/",
  "raw_data/",
  "raw-data/",
  "processed_data/",
  "processed-data/"
)

gitignore_path <- file.path(repo_root, ".gitignore")
existing_ignore_lines <- if (file.exists(gitignore_path)) {
  readLines(gitignore_path, warn = FALSE)
} else {
  character()
}

new_ignore_lines <- ignore_lines[!(ignore_lines %in% existing_ignore_lines)]
if (length(new_ignore_lines) > 0) {
  writeLines(c(existing_ignore_lines, new_ignore_lines), gitignore_path)
  message("\nUpdated .gitignore with dataset file patterns.")
} else {
  message("\n.gitignore already contains the dataset file patterns.")
}

files_to_stage <- c(".gitignore", "remove_datasets_from_github.R")
files_to_stage <- unique(files_to_stage[file.exists(files_to_stage)])
git(c("add", "--", files_to_stage))

staged_changes <- git(c("diff", "--cached", "--name-only", "--diff-filter=ACMRTD"))

if (length(staged_changes) == 0) {
  message("\nNo Git changes to commit.")
} else {
  message("\nStaged changes that will be committed:")
  message(paste(" -", staged_changes, collapse = "\n"))

  git(c("commit", "-m", commit_message))

  if (isTRUE(push_to_github)) {
    current_branch <- git(c("rev-parse", "--abbrev-ref", "HEAD"))[1]
    git(c("push", "origin", current_branch))
    message("\nDone. Dataset files were removed from the current GitHub branch.")
  } else {
    message("\nDone. Commit created locally. Set push_to_github <- TRUE to push.")
  }
}

message("\nReminder: old commits may still contain these files until history is rewritten.")
