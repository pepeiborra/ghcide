{-# LANGUAGE TypeFamilies #-}
module A(Cls, Fam, foo) where

foo = ()

type family Fam a :: *

class Cls a
