
pacman::p_load(dplyr, lme4, MuMIn)

before = read.csv('rhino-mynah-before-data.csv')
before$status = as.factor("before")
before.cleaned = before %>%
  filter(!session.no %in% c("1", "4"))

after = read.csv('rhino-mynah-after-data.csv')
after.cleaned = after %>%
  filter(!session.no %in% c("13", "27", "49"))


after$status = as.factor('after')
data = rbind(before.cleaned,after.cleaned)

data = data %>%
  rowwise() %>%
  mutate(feed.birds = sum(feed.on, feed.not.on, not.feed.on))

data2 = data %>%
  select(session.no, Weather, rhino.no, food.presence, food.presence.index,
         Interval, feed.on, not.feed.on, feed.not.on, not.feed.not.on, mynah.no,
         status, feed.birds) 

data3 = data2 %>%
  group_by(status, session.no) %>%
  dplyr::summarise(total.feed = sum(feed.birds),
                   total.mynah = sum(mynah.no),
                   prop.feed = total.feed/total.mynah,
         prop.not.feed = 1 - prop.feed,
         weather = first(Weather),
         rhino.no = max(rhino.no),
         food.presence = first(food.presence),
         food.presence.index = max(food.presence.index)) 

data3$prop.feed[is.na(data3$prop.feed)] = 0


data3 %>%
  group_by(status) %>%
  summarise(prop.correl = cor(prop.feed, total.mynah),
            absolute.correl = cor(total.feed, total.mynah),
            c2 = cor(total.feed, prop.feed))
# before: 0.81, after: 0.80
# total birds highly correlated to proportion of feeding bird (regardless of position)
# so can use either as a response.

m1 = glmer(prop.feed ~ 1 + (1|status),
           data = data3,
           family = binomial(link='logit'))


m2 = glmer(prop.feed ~ weather + (1|status),
           data = data3,
           family = binomial(link='logit'))


m3 = glmer(prop.feed ~ rhino.no + (1|status),
           data = data3,
           family = binomial(link='logit'))

m4 = glmer(prop.feed ~ rhino.no + food.presence.index + (1|status),
           data = data3,
           family = binomial(link='logit'))

model.sel(m1, m2, m3, m4)

# linear, birds feed
m1 = lm(total.feed ~ 1,
           data = data3)


m2 = lm(total.feed ~ weather,
           data = data3)


m3 = lm(total.feed ~ rhino.no,
           data = data3)

m4 = lm(total.feed ~ rhino.no + food.presence.index,
           data = data3)

m5 = lm(total.feed ~ status,
        data = data3)

m6 = lm(total.feed ~ status + rhino.no,
        data = data3)

model.sel(m1, m2, m3, m4, m5, m6)

# glm, logit
m1 = glm(prop.feed ~ 1,
        data = data3,
        family = binomial(link = "logit"))


m2 = glm(prop.feed ~ weather,
        data = data3,
        family = binomial(link = "logit"))


m3 = glm(prop.feed ~ rhino.no,
        data = data3,
        family = binomial(link = "logit"))

m4 = glm(prop.feed ~ rhino.no + food.presence.index,
        data = data3,
        family = binomial(link = "logit"))

m5 = glm(prop.feed ~ status,
        data = data3,
        family = binomial(link = "logit"))

m6 = glm(prop.feed ~ status + rhino.no,
        data = data3,
        family = binomial(link = "logit"))

model.sel(m1, m2, m3, m4, m5, m6)



           