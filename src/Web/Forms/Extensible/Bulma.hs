{-# LANGUAGE GADTs #-}
module Web.Forms.Extensible.Bulma where

import Prelude hiding (div)
import Control.Monad
import Web.Forms.Extensible.Field hiding (input)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text, pack)
import Debug.Trace

data Bulma

instance FormStyle Bulma where
  labelField lbl b = div ! class_ "field" $ do
    H.label ! class_ "label" $ lbl
    b
  formText _ ph m'def = div ! class_ "control" $ do
    input
      ! class_ "input"
      ! type_ "text"
      ! name (textValue fname)
      ! foldMap (value . textValue) m'def
      ! placeholder (textValue ph)
  formSelect _ ph xs m'def = div ! class_ "control" $ div ! class_ "select"
    $ select ! name (textValue fname) $ do
      option ! value "0" $ text ph
      forM_ (zip [1..] xs) $ \(i, v) -> option
        ! value (textValue $ pack $ show i)
        ! (if Just v == m'def then selected "selected" else mempty)
        $ text v
