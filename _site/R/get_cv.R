# Obtain cv.pdf
download.file("https://raw.githubusercontent.com/etiennebacher/CV_perso/master/cv.pdf", "_site/cv.pdf")
download.file("https://raw.githubusercontent.com/etiennebacher/good-practices/master/good-practices.pdf", "_site/good-practices.pdf")

# Make the button "CV" open in a new tab
index_html <- readLines("_site/index.html")
index_html <- gsub('href="cv.pdf"', 'href="cv.pdf" target = "blank"', index_html)
index_html <- gsub('href="good-practices.pdf"', 'href="good-practices.pdf" target = "blank"', index_html)
cat(index_html, sep = "\n", file = "_site/index.html")
