# render_test <- function(outname)
# {
#   time = Sys.time()
#   rmarkdown::render("C:/Users/nfrgu/Documents/Activity Generator/RmdTest.Rmd", output_file = outname, params = list(key = FALSE, time = time))
#   rmarkdown::render("C:/Users/nfrgu/Documents/Activity Generator/RmdTest.Rmd", output_file = cat(outname, " key"), params = list(key = TRUE, time = time))
# }
# 
# render_test("outtest")

render_test2 <- function(outname, qlist)
{
  time = Sys.time()
  rmarkdown::render("C:/Users/nfrgu/Documents/Activity Generator/Question Function.Rmd", output_file = outname, params = list(key = FALSE, time = time, qlist = qlist))
}

render_test2("testdoc", list(c(1, 0), 3, c(4,0), 5, 6, 7))
