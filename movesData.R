require('RCurl', quiet = T)
require('jsonlite', quiet = T)
require('ggplot2', quiet = T)

places.daily.url <- 'https://api.moves-app.com/api/1.1/user/places/daily?'
activities.daily.url <- 'https://api.moves-app.com/api/1.1/user/activities/daily?'
profile.url <- 'https://api.moves-app.com/api/1.1/user/profile?'

# get oauth token
token <- readline("Enter your authorization token if you have one already, otherwise just enter pls: ")
if (nchar(token) == 0) {
  client.id <- readline("Enter your client id: ")
  client.secret <- readline("Enter your client secret: ")
  url <- paste('https://api.moves-app.com/oauth/v1/authorize?response_type=code&client_id=',
               client.id,
               '&scope=activity%20location', sep = '')
  browseURL(url)
  
  auth.code <- readline("Enter the authorization code you got from browser: ")
  
  url <- paste('https://api.moves-app.com/oauth/v1/access_token?grant_type=authorization_code&code=',
               auth.code, '&client_id=', client.id, '&client_secret=', client.secret, sep = '')
  token <- fromJSON(postForm(url, a = '', binary = F))$access_token
}

# get user profile info
url <- paste(profile.url, 'access_token=', token, sep = '')
profile <- fromJSON(getURL(url))
first.date <- as.Date(profile$profile$firstDate, '%Y%m%d')
to <- as.Date(first.date + 30, '%Y%m%d')

# get daily activities and locations from user registration date to today
from <- first.date
places <- list()
activities <- list()
while(from < Sys.Date()) {
  if (to > Sys.Date()) to <- Sys.Date()
  fromFmt <- format(from, '%Y%m%d')
  toFmt <- format(to, '%Y%m%d')
  url <- paste(places.daily.url, 'from=', fromFmt, '&to=', toFmt, '&access_token=', token, sep = '')
  places <- rbind(places, fromJSON(getURL(url)))
  
  url <- paste(activities.daily.url, 'from=', fromFmt, '&to=', toFmt, '&access_token=', token, sep = '')
  activities <- rbind(activities, fromJSON(getURL(url)))
  
  from <- to + 1
  to <- to + 30
}

# analyze lunch time on week days
office.hours <- 0
home.hours <- 0
lunch.time <- list()
in.office <- list()
work.days <- 0
home.days <- 0
visit.loc <- c()
for (i in 1 : dim(places)[1]) {
  dt <- as.POSIXlt(strptime(places[i, 1], '%Y%m%d'))
  
  if ((weekdays(dt) %in% c('Saturday', 'Sunday')))
    next
  
  start <- strptime(places[i, 'segments'][[1]]$startTime[places[i, 'segments'][[1]]$place$type == 'home'], '%Y%m%dT%H%M%S')
  end <- strptime(places[i, 'segments'][[1]]$endTime[places[i, 'segments'][[1]]$place$type == 'home'], '%Y%m%dT%H%M%S')
  start[start < dt] <- dt
  end[end > dt + 3600 * 24] <- dt + 3600 * 24
  hours <- sum(as.numeric(difftime(end, start, unit = 'hours')))
  if (hours > 0) home.days <- home.days + 1
  home.hours <- home.hours + hours

  start <- strptime(places[i, 'segments'][[1]]$startTime[places[i, 'segments'][[1]]$place$type == 'work'], '%Y%m%dT%H%M%S')
  end <- strptime(places[i, 'segments'][[1]]$endTime[places[i, 'segments'][[1]]$place$type == 'work'], '%Y%m%dT%H%M%S')
  start[start < dt] <- dt
  end[end > dt + 3600 * 24] <- dt + 3600 * 24
  hours <- sum(as.numeric(difftime(end, start, unit = 'hours')))
  if (hours > 0) work.days <- work.days + 1
  office.hours <- office.hours + hours
  
  if (hours > 0)
    in.office <- rbind(in.office, data.frame(date = dt,
                            office.hours = hours))
  
  if (length(end) > 0) {
    # treat the first time I leave the office as lunch time
    if (end[1] < dt + 3600 * 16  && end[1] > dt + 3600 * 11)
      lunch.time <- rbind(lunch.time, data.frame(date = dt, 
                          lunch.time = as.numeric(difftime(end[1], dt, unit = 'hours'))))  
  }
  
  visit.loc <- c(visit.loc, places[i, 'segments'][[1]]$place$name)
}

office.pct <- office.hours / work.days
home.pct <- home.hours / home.days

# plot lunch time
c <- ggplot(lunch.time, aes(date, lunch.time))
print(c + stat_smooth(span = 0.5, size = 1.2) + geom_point())

# plot office hours
c <- ggplot(in.office, aes(date, office.hours))
print(c + stat_smooth(span = .5, size = 1.2) + geom_point())

sort(table(visit.loc))

walk.distance <- 0
cycle.distance <- 0
run.distance <- 0
days <- 0
for (i in 1 : dim(activities)[1]) {
  activity <- activities[i, 'summary'][[1]]
  if (sum(activity$distance, na.rm = T) > 0) {
    days <- days + 1
    walk.distance <- walk.distance + sum(activity$distance[activity$activity == 'walking'])
    cycle.distance <- cycle.distance + sum(activity$distance[activity$activity == 'cycling'])
    run.distance <- run.distance + sum(activity$distance[activity$activity == 'running'])
  }
}

cat(paste('Walking distance per day:', round(walk.distance / days), 'meters\n'))
cat(paste('Cycling distance per day:', round(cycle.distance / days), 'meters\n'))
cat(paste('Running distance per day:', round(run.distance / days), 'meters\n'))

