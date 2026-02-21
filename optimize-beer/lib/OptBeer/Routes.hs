module OptBeer.Routes where

import Noided.Pathname

newUserPath :: PathTemplate '[]
newUserPath = "users" :/ "new" :/ PathEnd
