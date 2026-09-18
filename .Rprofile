# REMEMBER to restart R after you modify and save this file!

# First, execute the global .Rprofile if it exists. You may configure blogdown
# options there, too, so they apply to any blogdown projects. Feel free to
# ignore this part if it sounds too complicated to you.
if (file.exists("~/.Rprofile")) {
  base::sys.source("~/.Rprofile", envir = environment())
}

# Now set options to customize the behavior of blogdown for this project. Below
# are a few sample options; for more options, see
# https://bookdown.org/yihui/blogdown/global-options.html
options(
  # to automatically serve the site on RStudio startup, set this option to TRUE
  blogdown.serve_site.startup = FALSE,
  # to disable knitting Rmd files on save, set this option to FALSE
  blogdown.knit.on_save = FALSE,
  blogdown.knit.on_save = FALSE,     
  blogdown.author = "Alexander Gamerdinger",  
  blogdown.ext = ".Rmarkdown",     
  blogdown.subdir = "blog",
  blogdown.server.verbose = TRUE,
  # Build to Markdown, not HTML. Hugo 0.158+ refuses .html files in
  # content/ unless security.allowContent is opened up, and .markdown
  # output avoids that entirely.
  blogdown.method = 'markdown'
)

# fix Hugo version
# Must match what `hugo version` reports in the terminal. Since 0.158 the
# macOS build is a .pkg rather than a .tar.gz, so blogdown::install_hugo()
# cannot fetch it. Install with `brew install hugo` instead, then set the
# number below to whatever `hugo version` prints. Anything from 0.158 up
# works with the theme.
options(blogdown.hugo.version = "0.165.0")
