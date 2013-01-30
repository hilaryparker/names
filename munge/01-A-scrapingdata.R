# Gathering data from SSA website using getNames function #
# ranks will be the same from each iteration, only take ranks from the first one

tmp <- getNames(year.ind=seq(1880,2011),number="p",female=TRUE)
female.percents <- tmp[[1]]
female.ranks <- tmp[[2]]
ProjectTemplate::cache("female.percents")
ProjectTemplate::cache("female.ranks")

tmp <- getNames(year.ind=seq(1880,2011),number="p",female=FALSE)
male.percents <- tmp[[1]]
male.ranks <- tmp[[2]]
ProjectTemplate::cache("male.percents")
ProjectTemplate::cache("male.ranks")

tmp <- getNames(year.ind=seq(1880,2011),number="n",female=TRUE)
female.nums <- tmp[[1]]
ProjectTemplate::cache("female.nums")

tmp <- getNames(year.ind=seq(1880,2011),number="n",female=FALSE)
male.nums <- tmp[[1]]
ProjectTemplate::cache("male.nums")
