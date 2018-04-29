{-# LANGUAGE OverloadedLabels, TypeFamilies, QuasiQuotes #-}

module Main where

import Web.Forms.Extensible
import Web.Forms.Extensible.Bulma
import Control.Lens
import Data.Extensible
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
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

main = putStrLn $ renderHtml [shamlet|<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Hello Bulma!</title>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.0/css/bulma.min.css">
    <script defer src="https://use.fontawesome.com/releases/v5.0.7/js/all.js">
  <body>
  <section class="section">
    <div class="container">
      <h1 class="title">
        Hello World
      <p class="subtitle">
        My first website with <strong>Bulma</strong>!
      #{form}|]
  where
    form = buildForm buildBulma testForm
