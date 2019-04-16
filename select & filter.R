
exam %>% select(math)

exam %>% select(class, math, english)

exam %>% select(-math)

exam %>% select(-math, -english)

exam %>% filter(class == 1) %>% select(english)

# test select & filter ####

# q1 ####

mpg_test <- mpg %>% select(class, cty)
head(mpg_test)

# q2 ####

class_suv <- mpg_test %>% filter(class == "suv")
class_compact <- mpg_test %>% filter(class == "compact")
mean(class_suv$cty)
mean(class_compact$cty)


