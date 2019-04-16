
# test 1 ####
df_test_raw = as.data.frame(ggplot2::midwest)
df_test_new = df_test_raw

summary(df_test_new)
head(df_test_new)

# test 2 ####
df_test_new <- rename(df_test_new, total = poptotal)
df_test_new <- rename(df_test_new, asian = popasian)

head(df_test_new)
dim(df_test_new)
str(df_test_new)

# test 3 ####
df_test_new$asian_percentage <- round(df_test_new$asian / df_test_new$total *100,1)
df_test_new$asian_percentage <- df_test_new$asian / df_test_new$total *100

df_test_new$asian_percentage

# test 4 ####
df_test_new$asian_percentage_compare <- ifelse(mean(df_test_new$asian_percentage) < df_test_new$asian_percentage, "large", "small")

mean(df_test_new$asian_percentage)
str(df_test_new)

# test 5 ####
table(df_test_new$asian_percentage_compare)
qplot(df_test_new$asian_percentage_compare)

