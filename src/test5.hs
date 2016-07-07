{-# LANGUAGE NamedFieldPuns #-}

module Test5 where

data D = D { field1 :: Int, field2::String } deriving (Show)

x = D { field1=3, field2="ok" }

f D{field1} = field1 + 11
