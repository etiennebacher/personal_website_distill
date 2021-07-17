# Obtain cv.pdf
download.file("https://raw.githubusercontent.com/etiennebacher/CV_perso/master/cv.pdf", "_site/cv.pdf")

# Make the button "CV" open in a new tab
index_html <- readLines("_site/index.html")
index_html <- gsub('href="cv.pdf"', 'href="cv.pdf" target = "blank"', index_html)
cat(index_html, file = "_site/index.html")