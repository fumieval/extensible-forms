{-# LANGUAGE GADTs, Rank2Types, TypeFamilies #-}
module Web.Forms.Extensible.Field where

import Control.Lens
import Data.Attoparsec.ByteString.Char8 hiding ((<?>))
import qualified Data.ByteString.Char8 as B
import Data.Extensible
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Blaze.Html (Html)
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Data.HashMap.Strict as HM

class FormStyle t where
  withLabel :: proxy t -> Html -> Html -> Html
  formText :: proxy t
    -> Text -- ^ placeholder
    -> Maybe Text
    -> Html
  formSelect :: proxy t
    -> Text -- ^ placeholder
    -> [Text] -- ^ candidates
    -> Maybe Text -- ^ default
    -> Html

data FormField style a = FormField
  { formAsHtml :: Html
  , formValidate :: B.ByteString -> Either FormValidation a
  }

instance Wrapper FormField where
  type Repr FormField a = FormField a
  _Wrapper = id

class DefaultField a where
  defaultField :: FormStyle style => FormField style a

instance DefaultField Text where
  Text = FormField


testForm :: FormStyle t => RecordOf (Form t) xs

{-
data FormField a b = FormField
  { ffLabel :: Maybe Html
  , ffDefault :: Maybe a
  , ffInput :: FormInput a
  , ffPostValidation :: a -> Either FormValidation b
  }



(<?>) :: FormField a -> Html -> FormField a
ff <?> l = ff { ffLabel = Just l }

data FormInput x where
  Text :: Text -> FormInput Text
  Select :: Text -> [Text] -> FormInput Text

parseInput :: FormInput a -> B.ByteString -> Either FormValidation a
parseInput (Text _) bs = Right $ T.decodeUtf8 bs
parseInput (Select _ xs) bs = case parseOnly (decimal <* endOfInput) bs of
  Left e -> Left $ ParseError $ T.pack e
  Right i
    | i >= 1 && i <= length xs -> Right $! xs !! pred i
    | otherwise -> Left SelectOutOfRange

parseField :: FormField a -> B.ByteString -> Either FormValidation a
parseField (FormField _ _ inp val) bs = parseInput inp bs >>= val

data FormValidation = FormMissingParam !B.ByteString
  | ParseError !T.Text
  | SelectOutOfRange
  | FieldMustNotBeEmpty
  deriving Show

parseForm :: Forall (KeyIs KnownSymbol) xs
  => RecordOf FormField xs
  -> [(B.ByteString, B.ByteString)]
  -> RecordOf (Either FormValidation) xs
parseForm rec params = htabulateFor (Proxy :: Proxy (KeyIs KnownSymbol))
  $ \k -> Field $ let name = B.pack $ symbolVal $ proxyAssocKey k
    in case HM.lookup name m of
      Just bs -> parseField (getField $ hlookup k rec) bs
      Nothing -> Left $ FormMissingParam name
  where
    m = HM.fromList params

type Form = RecordOf FormField

buildForm :: Forall (KeyIs KnownSymbol) xs
  => (forall a. Text -> FormField a -> Html)
  -> RecordOf FormField xs
  -> Html
buildForm toHtml = hfoldMapWithIndexFor (Proxy :: Proxy (KeyIs KnownSymbol))
  (\k -> toHtml (T.pack $ symbolVal $ proxyAssocKey k) . getField)

instance DefaultField Text where
  defaultField = FormField Nothing Nothing (Text "") Right

defaultForm :: Forall (KeyValue KnownSymbol DefaultField) xs
  => RecordOf FormField xs
defaultForm = htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol DefaultField))
  $ \k -> Field $ defaultField
    <?> fromString (symbolVal $ proxyAssocKey k)
-}
