{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Data.Aeson.With (
  withJSON
, withValue
, withStringField
, withArrayField
, withObjectField
, withNumberField
, withBoolField
, withNullField
)where

import           Control.Lens
import           Data.Aeson      as A
import           Data.Aeson.Lens
import qualified Data.HashMap.Lazy     as HML
import           Data.Scientific
import           Data.Text
import qualified Data.Vector      as V

-- | Union two JSON values together.
withJSON :: (ToJSON a) => a -> Value -> Value
withJSON x (Object obj) = Object $ HML.union obj y
  where Object y = toJSON x
withJSON _ _ =  error "Can ony add a new TOJSON object to objects"

-- | Add  Null field to a JSON value.
withNullField :: Text -> Value -> Value
withNullField f = _Object . at f ?~ Null

-- | Add a String field to a JSON value.
withStringField :: Text -> Text -> Value -> Value
withStringField f v =  _Object  . at f ?~ String v

-- | Add an Array field to a JSON value.
withArrayField :: ToJSON a => Text -> [a] -> Value -> Value
withArrayField f v = _Object . at f ?~ Array (V.fromList (toJSON <$> v))

-- | Add an Value field to a JSON value.
withValue :: ToJSON a => Text -> a -> Value -> Value
withValue f v = _Object . at f ?~ (toJSON v)

-- | Add an Number field to a JSON value.
withNumberField :: Text -> Scientific -> Value -> Value
withNumberField f v = _Object . at f ?~ Number v

-- | Add a Bool field to a JSON value.
withBoolField :: Text -> Bool -> Value -> Value
withBoolField f v = _Object . at f ?~ Bool v

-- | Add an Object field to a JSON value.
withObjectField :: Text -> Object -> Value -> Value
withObjectField f v = _Object . at f ?~ Object v
