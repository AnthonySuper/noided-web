module OptBeer.Routes where

import Noided.Pathname

homePath :: PathTemplate '[]
homePath = PathEnd

newUserPath :: PathTemplate '[]
newUserPath = "users" :/ "new" :/ PathEnd

newSessionPath :: PathTemplate '[]
newSessionPath = "sessions" :/ "new" :/ PathEnd

usersPath :: PathTemplate '[]
usersPath = "users" :/ PathEnd

sessionsPath :: PathTemplate '[]
sessionsPath = "sessions" :/ PathEnd

logoutPath :: PathTemplate '[]
logoutPath = "logout" :/ PathEnd
