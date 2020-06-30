{-# LANGUAGE FlexibleInstances #-}
module Development.IDE.Types.Progress where

class HasProgress k where
    hasProgress :: k -> Bool

instance {-# OVERLAPPABLE #-} HasProgress k where
    hasProgress _ = True
