module OptBeer.Routes where

import Noided.Pathname
import OptBeer.DB.Ids.ItemId
import OptBeer.DB.Ids.RecipeId
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

itemsPath :: PathTemplate '[OrganizationIdent]
itemsPath = "organizations" :/ capPiece :/ "items" :/ PathEnd

createItemPath :: PathTemplate '[OrganizationIdent]
createItemPath = itemsPath

newItemPath :: PathTemplate '[OrganizationIdent]
newItemPath = "organizations" :/ capPiece :/ "items" :/ "new" :/ PathEnd

recipesPath :: PathTemplate '[OrganizationIdent]
recipesPath = "organizations" :/ capPiece :/ "recipes" :/ PathEnd

createRecipePath :: PathTemplate '[OrganizationIdent]
createRecipePath = recipesPath

newRecipePath :: PathTemplate '[OrganizationIdent]
newRecipePath = "organizations" :/ capPiece :/ "recipes" :/ "new" :/ PathEnd

showItemPath :: PathTemplate '[ItemId]
showItemPath = "items" :/ capPiece :/ PathEnd

editItemPath :: PathTemplate '[ItemId]
editItemPath = "items" :/ capPiece :/ "edit" :/ PathEnd

updateItemPath :: PathTemplate '[ItemId]
updateItemPath = showItemPath
