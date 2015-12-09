## Script to generate web pages (markdown format) that include the R code for auxiliary scripts
## Shouldn't be run on a regular basis and care is needed.
## Assumes it runs from _tools dir

filenames <- c("fijiquakes", "rSpecialMC", "rSpecialNS",
               "shortestpath", "short.output", "startup")
extra_text <- c("chapter 6", "chapter 12", "chapter 12", "chapter 17", "chapters 16+17", "all chapters")
rawgithub <- "https://raw.githubusercontent.com/spatstat/book/gh-pages/_includes/chapter-code/R/"
for(i in seq_along(filenames)){
  rawfile <- paste0(rawgithub, filenames[i], ".R")
  writeLines(
    c("---",
      "layout: chapter-code",
      paste0("title: ", filenames[i], ".R"),
      "---",
      "",
      paste("## Auxiliary script", filenames[i]),
      paste0("The auxiliary script `", filenames[i],
             ".R` below is loaded in the R code for ",
             extra_text[i], "."),
      "The chapter R code assumes that the script is placed in a subdirectory 'R' of the current working directory.",
      paste0("You can download the script <a href='", rawfile, "' target=_blank>here</a>."),
      "",
      "{% highlight r %}",
      paste0("{% include chapter-code/R/", filenames[i], ".R %}"),
      "{% endhighlight %}"),
    paste0("../chapter-code/R/", filenames[i], ".md"))
}
