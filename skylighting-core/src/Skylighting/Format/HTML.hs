module Skylighting.Format.HTML (
      formatHtmlInline
    , formatHtmlBlock
    , styleToCss
    ) where

import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Skylighting.Types
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Skylighting.Format.HTML.CSS
import qualified Clay.Render as C

-- | Format tokens using HTML spans inside @code@ tags. For example,
-- A @KeywordTok@ is rendered as a span with class @kw@.
-- Short class names correspond to 'TokenType's as follows:
-- 'KeywordTok'        = @kw@,
-- 'DataTypeTok'       = @dt@,
-- 'DecValTok'         = @dv@,
-- 'BaseNTok'          = @bn@,
-- 'FloatTok'          = @fl@,
-- 'CharTok'           = @ch@,
-- 'StringTok'         = @st@,
-- 'CommentTok'        = @co@,
-- 'OtherTok'          = @ot@,
-- 'AlertTok'          = @al@,
-- 'FunctionTok'       = @fu@,
-- 'RegionMarkerTok'   = @re@,
-- 'ErrorTok'          = @er@,
-- 'ConstantTok'       = @cn@,
-- 'SpecialCharTok'    = @sc@,
-- 'VerbatimStringTok' = @vs@,
-- 'SpecialStringTok'  = @ss@,
-- 'ImportTok'         = @im@,
-- 'DocumentationTok'  = @do@,
-- 'AnnotationTok'     = @an@,
-- 'CommentVarTok'     = @cv@,
-- 'VariableTok'       = @va@,
-- 'ControlFlowTok'    = @cf@,
-- 'OperatorTok'       = @op@,
-- 'BuiltInTok'        = @bu@,
-- 'ExtensionTok'      = @ex@,
-- 'PreprocessorTok'   = @pp@,
-- 'AttributeTok'      = @at@,
-- 'InformationTok'    = @in@,
-- 'WarningTok'        = @wa@.
-- A 'NormalTok' is not marked up at all.
formatHtmlInline :: FormatOptions -> [SourceLine] -> Html
formatHtmlInline opts = wrapCode opts
                      . mconcat . intersperse (toHtml "\n")
                      . map (mapM_ (tokenToHtml opts))

-- | Format tokens as an HTML @pre@ block. Each line is wrapped in an a
-- element with the class ‘source-line’. If line numbering
-- is selected, the surrounding pre is given the class ‘numberSource’,
-- and the resulting html will display line numbers thanks to the included
-- CSS.  See the documentation for 'formatHtmlInline' for information about how
-- tokens are encoded.
formatHtmlBlock :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlock opts ls =
  H.div ! A.class_ (toValue "sourceCode") $
  H.pre ! A.class_ (toValue $ Text.unwords classes)
        $ wrapCode opts
        $ mconcat . intersperse (toHtml "\n")
        $ zipWith (sourceLineToHtml opts) [startNum..] ls
  where  classes = Text.pack "sourceCode" :
                   [Text.pack "numberSource" | numberLines opts] ++
                   [x | x <- containerClasses opts
                      , x /= Text.pack "sourceCode"]
         startNum = LineNo $ startNumber opts

wrapCode :: FormatOptions -> Html -> Html
wrapCode opts h = H.code ! A.class_ (toValue $ Text.unwords
                                             $ Text.pack "sourceCode"
                                               : codeClasses opts) $ h

-- | Each line of source is wrapped in an (inline-block) anchor that makes
-- subsequent per-line processing (e.g. adding line numnbers) possible.
sourceLineToHtml :: FormatOptions -> LineNo -> SourceLine -> Html
sourceLineToHtml opts lno cont =
  (if lineAnchors opts
      then H.a   ! A.class_ sourceLine
                 ! A.id lineNum
                 ! A.href lineRef
                 ! dataAttrib
      else H.a   ! A.class_ sourceLine
                 ! A.id lineNum
                 ! dataAttrib) $ mapM_ (tokenToHtml opts) cont
  where  sourceLine = toValue "sourceLine"
         lineNum = toValue prefixedLineNo
         lineRef = toValue ('#':prefixedLineNo)
         prefixedLineNo = Text.unpack (lineIdPrefix opts) <> show (lineNo lno)
         dataAttrib = H.dataAttribute (fromString "line-number")
                          (toValue (show (lineNo lno)))

tokenToHtml :: FormatOptions -> Token -> Html
tokenToHtml _ (NormalTok, txt)  = toHtml txt
tokenToHtml opts (toktype, txt) =
  if titleAttributes opts
     then sp ! A.title (toValue $ show toktype)
     else sp
   where sp = H.span ! A.class_ (toValue $ short toktype) $ toHtml txt

-- | Returns CSS for styling highlighted code according to the given style.
styleToCss :: Style -> String
styleToCss = TL.unpack . C.renderWith config [] . styleToCss'
  where
    config = C.compact
      { C.rbrace = fromString "}\n"
      , C.comments = True
      }
