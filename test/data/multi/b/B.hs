{-# LANGUAGE TypeFamilies #-}
module B(module B) where
import A
qux = foo

data B = B

type instance Fam B = B

instance Cls B
