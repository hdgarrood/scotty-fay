{-# LANGUAGE OverloadedStrings #-}
module TestJQuery where

import Prelude
import Fay.Text
import JQuery

test :: Fay JQuery
test = select ("#an-id" :: Text)
