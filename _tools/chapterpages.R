## Script to generate web pages (markdown format) that include the R code for each chapter
## Shouldn't be run on a regular basis and care is needed.
## Assumes it runs from _tools dir

chap <- 1:17
chap0 <- formatC(chap, width = 2, flag = "0")
filenames <- list.files("../_includes/chapter-code/", pattern = "[0-1][0-9].*")
rawgithub <- "https://raw.githubusercontent.com/spatstat/book/gh-pages/_includes/chapter-code/"
for(i in seq_along(chap)){
  rawfile <- paste0(rawgithub, filenames[i])
  writeLines(
    c("---",
      "layout: chapter-code",
      paste("title: R code for chapter", chap[i]),
      "---",
      "",
      paste("## R code for chapter", chap[i]),
      paste0("Below is the R code used to generate results and figures in chapter ", chap[i], "."),
      paste("The code is in a rather raw format extracted from the book manuscript files",
            "-- please read the [instructions for use](instructions.html) if you haven't done so yet."),
      paste0("You can download the script <a href='", rawfile, "' target=_blank>here</a>."),
      "",
      "{% highlight r %}",
      paste0("{% include chapter-code/", filenames[i], " %}"),
      "{% endhighlight %}"),
    paste0("../chapter-code/chapter", chap0[i], ".md"))
}
