{-# LANGUAGE TypeFamilies #-}
module Web.Forms.Extensible where

import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html5 as B
import qualified Data.ByteString as B
import Data.Extensible
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Data.Optional
import GHC.TypeLits

type Form = RecordOf Input

text :: Text
  -> Input Text
text ph = Input
  { inputToHtml = H.input
    H.! H.type_ "text"
    H.! H.placeholder (H.textValue ph)
  , parseValue = pure . T.decodeUtf8
  }

select :: [Text] -> Input Text
select xs = Input
  { inputToHtml = H.select $ mapM_ (H.option . H.text) xs
  , parseValue = pure . T.decodeUtf8
  }

data Input a = Input
  { inputToHtml :: H.Html
  , parseValue :: B.ByteString -> Either String a
  }

instance Wrapper Input where
  type Repr Input a = Input a
  _Wrapper = id

simpleHtml :: Forall (KeyIs KnownSymbol) xs => Form xs -> H.Html
simpleHtml = hfoldMap (inputToHtml . getField)
