{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/assets StaticR Static appStatic

/ HomeR GET

/auth   AuthR   Auth   getAuth
/api/cats CatsR GET
|]
