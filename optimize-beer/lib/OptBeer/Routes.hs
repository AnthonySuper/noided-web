module OptBeer.Routes where

import Noided.Pathname

homePath :: PathTemplate '[]
homePath = PathEnd

newUserPath :: PathTemplate '[]
newUserPath = "users" :/ "new" :/ PathEnd

usersPath :: PathTemplate '[]
usersPath = "users" :/ PathEnd
