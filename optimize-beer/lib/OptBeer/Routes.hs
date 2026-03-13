module OptBeer.Routes where

import Noided.Pathname
import OptBeer.Type.OrganizationIdent

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

organizationsPath :: PathTemplate '[]
organizationsPath = "organizations" :/ PathEnd

newOrganizationPath :: PathTemplate '[]
newOrganizationPath = "organizations" :/ "new" :/ PathEnd

showOrganizationPath :: PathTemplate '[OrganizationIdent]
showOrganizationPath = "organizations" :/ capPiece :/ PathEnd

createItemPath :: PathTemplate '[OrganizationIdent]
createItemPath = "organizations" :/ capPiece :/ "items" :/ PathEnd
