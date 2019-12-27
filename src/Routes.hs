{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/assets StaticR Static appStatic

/ HomeR GET

/client   AuthR   Auth   getAuth
/api/data DataR GET
|]