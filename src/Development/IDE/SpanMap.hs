module Development.IDE.SpanMap (SpanMap, genSpanMap, spansAtPoint) where

import           Control.DeepSeq
import qualified Data.IntervalMap.FingerTree as IM
import           Development.IDE.Spans.Type
import           Language.Haskell.LSP.Types

newtype SpanMap = SpanMap (IM.IntervalMap Position SpanInfo)

instance NFData SpanMap where rnf x = seq x ()

instance Show SpanMap where show _ = "SPAN MAP"

genSpanMap :: [SpanInfo] -> SpanMap
genSpanMap = SpanMap . foldMap f
  where
    f x@SpanInfo {..} = IM.singleton
      (IM.Interval (Position spaninfoStartLine spaninfoStartCol)
                  (Position spaninfoEndLine spaninfoEndCol)
      )
      x

-- | Seaches for all the spans around the given point
spansAtPoint :: Position -> SpanMap -> [SpanInfo]
spansAtPoint p (SpanMap im) = snd <$> IM.search p im
