# One-time: link local project to GitHub and push (run from project root).
# In RStudio: source("connect_github.R")

root <- if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) {
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
setwd(root)

remote_url <- "https://github.com/DataFirst-AFLEARN/egra-egma.git"

if (!dir.exists(".git")) {
  system2("git", c("init"), stdout = TRUE, stderr = TRUE)
}

remotes <- tryCatch(
  system2("git", c("remote", "-v"), stdout = TRUE, stderr = TRUE),
  error = function(e) character()
)
has_origin <- any(grepl("^origin\\s", remotes))

if (!has_origin) {
  system2("git", c("remote", "add", "origin", remote_url), stdout = TRUE, stderr = TRUE)
  message("Added remote origin: ", remote_url)
} else {
  system2("git", c("remote", "set-url", "origin", remote_url), stdout = TRUE, stderr = TRUE)
  message("Updated remote origin: ", remote_url)
}

system2("git", c("branch", "-M", "main"), stdout = TRUE, stderr = TRUE)

message(
  "\nNext steps in Terminal (or RStudio Git pane):\n",
  "  git add .\n",
  "  git commit -m \"Initial AFLearn EGRA/EGMA study rounds site\"\n",
  "  git push -u origin main\n",
  "\nThen on GitHub: Settings → Pages → branch main → folder /docs\n",
  "Site URL: https://datafirst-aflearn.github.io/egra-egma/\n"
)
