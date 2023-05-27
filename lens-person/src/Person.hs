{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Person
  ( Address (..),
    Born (..),
    Name (..),
    Person (..),
    bornStreet,
    renameStreets,
    setBirthMonth,
    setCurrentStreet,
  )
where

import Control.Lens
import Data.Time.Calendar (Day, fromGregorian, toGregorian)

data Name = Name
  { _foreNames :: String,
    _surName :: String
  }

makeLenses ''Name

data Address = Address
  { _street :: String,
    _houseNumber :: Int,
    _place :: String,
    _country :: String
  }

makeLenses ''Address

data Born = Born
  { _bornAt :: Address,
    _bornOn :: Day
  }

makeLenses ''Born

data Person = Person
  { _name :: Name,
    _born :: Born,
    _address :: Address
  }

makeLenses ''Person

bornStreet :: Born -> String
bornStreet b = b ^. (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet st = address . street .~ st

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = born . bornOn .~ newbd $ person
  where
    (y, _, d) = toGregorian (person ^. (born . bornOn))
    newbd = fromGregorian y month d

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (born . bornAt . street) f . over (address . street) f
