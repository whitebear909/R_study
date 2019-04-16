exam %>% filter(math > 50)

exam %>% filter(math < 50)

exam %>% filter(english >= 80)

exam %>% filter(english <= 80)

exam %>% filter(class ==1 & math >= 50)

exam %>% filter(english >=90 | math >= 90)

exam %>% filter(class ==1 | class == 3 | class ==5)

exam %>% filter(class %in% c(1,3,5))

df_class1 <- exam %>% filter(class ==1)
df_class2 <- exam %>% filter(class ==2)

mean(df_class1$math)
mean(df_class2$math)

# test ####
# q1 ####
df_displ4 <- mpg %>% filter(displ <= 4)
df_displ5 <- mpg %>% filter(displ >= 5)

table(df_displ4$displ)
table(df_displ5$displ)

mean(df_displ4$hwy) 
mean(df_displ5$hwy)

# q2 ####
df_manufacturer_audi <- mpg %>% filter(manufacturer == "audi")
df_manufacturer_toyota <- mpg %>% filter(manufacturer == "toyota")

table(df_manufacturer_audi$manufacturer)
table(df_manufacturer_toyota$manufacturer)

mean(df_manufacturer_audi$cty) 
mean(df_manufacturer_toyota$cty)

# q3 ####
df_manufacturer3 <- mpg %>% filter(manufacturer %in% c("chevrolet","ford","honda"))
mean(df_manufacturer3$hwy) 

table(df_manufacturer3$manufacturer)
