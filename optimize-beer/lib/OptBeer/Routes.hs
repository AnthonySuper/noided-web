module OptBeer.Routes where

import Noided.Pathname

newUserPath :: PathTemplate '[]
newUserPath = "users" :/ "new" :/ PathEnd

usersPath :: PathTemplate '[]
usersPath = "users" :/ PathEnd
