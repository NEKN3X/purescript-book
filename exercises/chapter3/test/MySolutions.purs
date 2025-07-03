module Test.MySolutions where

import Prelude
import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry = _.address.street >>> eq street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = filter filterEntry >>> not null
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq entriesAreDuplicate
  where
  entriesAreDuplicate :: Entry -> Entry -> Boolean
  entriesAreDuplicate e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
