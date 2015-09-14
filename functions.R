# Convert special characters to international format
to.plain <- function(s) {

   # 1 character substitutions
   old1 <- "šžþàáâãäåçèéêëìíîïðñòóôõöùúûüý"
   new1 <- "szyaaaaaaceeeeiiiidnooooouuuuy"
   s1 <- chartr(old1, new1, s)

   # 2 character substitutions
   old2 <- c("œ", "ß", "æ", "ø")
   new2 <- c("oe", "ss", "ae", "oe")
   s2 <- s1
   for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)

   s2
}  # adapted from http://stackoverflow.com/questions/17517319/r-replacing-foreign-characters-in-a-string

