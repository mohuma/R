library(tidyverse)

# import a web page into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

# extract all "table" nodes into an object
tab <- h %>% html_nodes("table")
tab <- tab[[2]]

# convert HTML table to a data frame
tab <- tab %>% html_table
class(tab)

# update the column header
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

# guacamole recipe page from Food Network
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
directions <- h %>% html_nodes(".o-Method__m-Step") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients, directions)
guacamole

# general function to get the recipe
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  directions <- h %>% html_nodes(".o-Method__m-Step") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients, directions = directions))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")



library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

html_text(nodes[[1]])

html_table(nodes[[4]])

sapply(nodes[1:4], html_table)

sapply(nodes, html_table)

html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])

tab_1 <- html_table(nodes[[10]]) %>% setNames(c("No", "Team", "Payroll", "Average")) %>% select(Team, Payroll, Average) %>% filter(Team != "Team")
tab_2 <- html_table(nodes[[19]]) %>% setNames(c("Team", "Payroll", "Average")) %>% filter(Team != "Team")
full_join(tab_1, tab_2, by = "Team")

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
col_names <- c("Team", "Payroll", "Average")
tab_1 <- tab_1[-1, -1]
tab_2 <- tab_2[-1,]
names(tab_2) <- col_names
names(tab_1) <- col_names
full_join(tab_1,tab_2, by = "Team")


library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table") %>% html_table(fill = TRUE)
