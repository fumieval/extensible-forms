{-# LANGUAGE OverloadedLabels, TypeFamilies, QuasiQuotes #-}

module Main where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Web.Forms.Extensible
import Web.Forms.Extensible.Bulma
import Control.Lens
import Data.Extensible
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import Text.Hamlet

type TestFields =
  [ "firstName" >: Text
  , "lastName" >: Text
  , "gender" >: Text
  ]

testForm :: Form TestFields
testForm = defaultForm
  & #gender . formInput
    .~ Select ["Select gender", "Male", "Female", "Other"]

main = runEnv 8080 $ \req sendResp -> do
  (params, _) <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
  let result = T.pack $ show $ htraverse (fmap (Field . Identity) . getField) $ parseForm testForm params
  sendResp $ responseBuilder status200 [] $ Utf8.renderHtmlBuilder [shamlet|<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Hello Bulma!</title>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.0/css/bulma.min.css">
    <script defer src="https://use.fontawesome.com/releases/v5.0.7/js/all.js">
  <body>
  <section class="section">
    <div>
      #{result}
    <div class="container">
      <h1 class="title">
        Hello World
      <p class="subtitle">
        My first website with <strong>Bulma</strong>!
      <form action="/" method="post">
        #{buildForm buildBulma testForm}
        <div class="control">
          <button class="button is-primary">Submit</button>|]
