{-# LANGUAGE DeriveGeneric #-}

module Types.ProgramInfo where

import GHC.Generics (Generic)
import qualified Data.Yaml as Yaml

instance Yaml.ToJSON Type

data Type = Type
    { version :: String
    , namespace :: String
    , name :: String
    , url :: String
    , notice :: String
    }
    deriving (Eq, Show, Generic)
