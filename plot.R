
ggplot(data = mpg, aes(x=displ,y=hwy)) +
  geom_point() +
  xlim(3,6) + 
  ylim(10,30) +
  

ggplot(data = mpg, aes(x=cty,y=hwy)) +
  geom_point() 

ggplot(data = df_midwest_copy, aes(x=poptotal,y=popasian)) +
  geom_point() +
  xlim(0,500000)+
  ylim(0,10000)

ggplot(data = economics, aes(x = date, y = unemploy)) + 
  geom_line()

ggplot(data = economics, aes(x = date, y = psavert)) + 
  geom_line()

ggplot(data = mpg, aes(x=drv)) +geom_bar()

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

df_mpg  

ggplot(data = df_mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) + geom_col()

str(mpg)

df_data <- mpg %>% 
  filter(class == "suv") %>%  
  group_by(manufacturer, class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

ggplot(data = df_data, aes(x = reorder(manufacturer, -mean_cty), y=mean_cty)) +
  geom_col()

ggplot(data = mpg, aes(x = class)) + geom_bar()  

# boxplot ####
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

ggplot(data = mpg %>% filter(class %in% c("compact", "subcompact", "suv")), aes(x = class, y = cty)) + geom_boxplot()

ggplot(data)


http://www.ggplot2-exts.org/gallery/

  이미지 검색, 방문, 코드 복사  
  https://www.google.com/search?q=r+map+graph&rlz=1C1SQJL_koKR838KR838&source=lnms&tbm=isch&sa=X&ved=0ahUKEwii8s2I-tjhAhUKZt4KHYN0BjkQ_AUIDigB&biw=1920&bih=888
