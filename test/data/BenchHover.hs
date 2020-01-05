{- HLINT ignore -}
{-# OPTIONS -Wmissing-signatures #-}

-- obviated type signature on purpose (for diagnostics)
void x = x >> return ()

test :: IO ()
test = do
    let a = 'a'
    void $ return $ a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:a:[]