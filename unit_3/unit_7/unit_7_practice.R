inHome <- c(3, 4, 1, 1, 1, 3, 3, 6, 5, 1, 4, 5, 4, 4, 3, 6, 7, 7, 7, 8)
outHome <- c(7, 6, 7, 8, 7, 6, 5, 6, 4, 2, 5, 4, 3, 6, 7, 5, 4, 3, 8, 7)

t.test(inHome, outHome, paired = FALSE)

nibbles <- c(9, 3, 1, 6, 5, 7, 8, 3, 10, 3, 5, 2, 9, 6, 2, 5, 8, 1, 6, 3)
wribbles <- c(4, 7, 6, 8, 7, 7, 8, 6, 7, 8, 9, 8, 7, 3, 6, 7, 6, 5, 5, 6)

t.test(nibbles, wribbles, paired = TRUE, alternative = c("less"))
