library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)

x <- str_split(txt[9], "\n")
class(x)
length(x)

s <- x[[1]]
class(s)
length(s)

s <- s %>% str_trim()
s[1]

header_index <- str_which(s, "2015")[1]
header_index

header <- s[header_index] %>% str_split("\\s+", simplify = TRUE)
month <- header[1]
header <- header[-1]
month
header[3]

tail_index <- str_which(s, "Total")
tail_index

n <- str_count(s, "\\d+")
sum(n==1)

out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

s <- str_remove_all(s, "[^\\d\\s]")

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

the_names = c("day", header)
tab <- s %>% as.data.frame() %>% setNames(the_names) %>% mutate_all(as.numeric)
mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2017"[20:30])

tab <- tab %>% gather(year, deaths, -day) %>% mutate(deaths = as.numeric(deaths))

tab %>% 
  filter(year != 2018) %>%
  ggplot(aes(day,deaths,color=year)) +
  geom_line() +
  geom_vline(xintercept = 20)
