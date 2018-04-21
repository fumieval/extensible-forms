{-# LANGUAGE OverloadedLabels, TypeFamilies #-}

module Main where

import Web.Forms.Extensible
import Data.Extensible
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

type TestFields =
  [ "firstName" >: Text
  , "lastName" >: Text
  , "gender" >: Text
  ]

testForm :: Form TestFields
testForm = #firstName @= text "First Name"
  <: #lastName @= text "Last Name"
  <: #gender @= select ["Male", "Female", "Other"]
  <: nil

main = putStrLn $ renderHtml $ simpleHtml testForm
