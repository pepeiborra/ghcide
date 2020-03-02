
module Development.IDE.Plugin(Plugin(..), codeActionPlugin, codeActionPluginWithRules) where

import           Data.Default
import           Development.IDE.LSP.Server
import           Development.Shake

import           Development.IDE.Core.Rules
import qualified Language.Haskell.LSP.Core     as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types    (List(..), CodeActionParams(..), CAResult, CodeActionContext,
                                                Range, ResponseError, TextDocumentIdentifier)


data Plugin c = Plugin
    {pluginRules   :: Rules ()
    ,pluginHandler :: PartialHandlers c
    }

instance Default (Plugin c) where
    def = Plugin mempty def

instance Semigroup (Plugin c) where
    Plugin x1 y1 <> Plugin x2 y2 = Plugin (x1<>x2) (y1<>y2)

instance Monoid (Plugin c) where
    mempty = def


codeActionPlugin :: (LSP.LspFuncs c -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin c
codeActionPlugin = codeActionPluginWithRules mempty

codeActionPluginWithRules :: Rules () -> (LSP.LspFuncs c -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin c
codeActionPluginWithRules rr f = Plugin rr $ PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeActionHandler = withPartialResponse _partialResultToken NotPrCodeAction RspCodeAction g <> LSP.codeActionHandler x
    }
    where
      g lsp state (CodeActionParams a b c _ _) = fmap nonEmptyList <$> f lsp state a b c
      nonEmptyList [] = Nothing
      nonEmptyList  x = Just (List x)
