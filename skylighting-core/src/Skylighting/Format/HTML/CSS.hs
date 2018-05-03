{-# LANGUAGE
    OverloadedStrings
  , NoImplicitPrelude
  #-}

module Skylighting.Format.HTML.CSS
    ( module Skylighting.Format.HTML.CSS
    ) where

import Prelude hiding (div, span, (**))
import Clay (Css)
import Clay hiding (backgroundColor, reverse)
import Clay.Stylesheet (CommentText(..))
import qualified Clay as C
import qualified Clay.Media as CM
import qualified Clay.Text as CT
import Data.List (sort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Skylighting.Types

styleToCss' :: Style -> Css
styleToCss' f = do
    divspec
    numberspec
    colorspec
    linkspec
    mapM_ toCss (sort (Map.toList (tokenStyles f)))

   where colorspec = div # sourceCode ?
           case (defaultColor f, backgroundColor f) of
             (Nothing, Nothing) -> pure ()
             (Just c, Nothing)  -> do
               color $ fromColor c
             (Nothing, Just c)  -> do
               C.backgroundColor $ fromColor c
             (Just c1, Just c2) -> do
               color $ fromColor c1
               C.backgroundColor $ fromColor c2

         numberspec = do
          pre # numberSource ? do
            marginLeft . em $ 3
            pure () `maybe` (\c -> borderLeft solid (px 1) $ fromColor c) $ lineNumberColor f

            paddingLeft . px $ 4

            a # sourceLine ? do
              position relative
              left . em $ (-4)

              before & do
                content . attrContent $ "data-line-number"
                position relative
                left . em $ (-1)
                "Set width on pseudo element" `commenting` display inlineBlock
                textAlign . alignSide $ sideRight
                verticalAlign vAlignBaseline
                border none nil none
                pointerEvents allEvents
                userSelect none
                sym2 padding nil (px 4)
                width . em $ 4
                pure () `maybe` (\c -> C.backgroundColor $ fromColor c) $ lineNumberBackgroundColor f
                pure () `maybe` (\c -> color $ fromColor c) $ lineNumberColor f

         divspec = do
          a # sourceLine ? do
            display inlineBlock
            lineHeight . unitless $ 1.25
            pointerEvents none
            color inherit
            textDecoration inherit

            empty & do
              "Correct empty line height" `commenting` do
                height . em $ 1.2

          star # sourceCode ? do
            "Needed for line numbers to be displayed" `commenting` do
              overflow visible

          -- TODO reduce comment duplication (needs new clay release)
          div # sourceCode ? do
            "Collapse neighbours correctly" `commenting` do
              sym2 margin (em 1) nil
          pre # sourceCode ? do
            "Collapse neighbours correctly" `commenting` do
              sym margin nil

          code # sourceCode ? do
            whiteSpace CT.pre
            "Needed for contents to be position: relative" `commenting` do
              position relative

          query CM.screen [] $ do
            div # sourceCode ? do
              overflowX auto

          query CM.print [] $ do
            code # sourceCode ? do
              whiteSpace preWrap
            a # sourceLine ? do
              textIndent . indent . em $ (-1)
              paddingLeft . em $ 1

         linkspec = query CM.screen [] $ do
          a # sourceLine # before ? do
            textDecoration underline

         numberSource :: Refinement
         numberSource = ".numberSource"
         sourceCode :: Refinement
         sourceCode = ".sourceCode"
         sourceLine :: Refinement
         sourceLine = ".sourceLine"

toCss :: (TokenType, TokenStyle) -> Css
toCss (t,tf) = do
        code ** span # shortR t ? do
          showTokenType t `commenting` do
            colorspec
            backgroundspec
            weightspec
            stylespec
            decorationspec
  where colorspec = pure () `maybe` (color . fromColor) $ tokenColor tf
        backgroundspec = pure () `maybe` (C.backgroundColor . fromColor) $ tokenBackground tf
        weightspec = if tokenBold tf then fontWeight bold else pure ()
        stylespec  = if tokenItalic tf then fontStyle italic else pure ()
        decorationspec = if tokenUnderline tf then textDecoration underline else pure ()
        shortR = byClass . short
        showTokenType t' = CommentText . Text.pack $ case reverse (show t') of
                             'k':'o':'T':xs -> reverse xs
                             _              -> ""

short :: TokenType -> Text
short KeywordTok        = "kw"
short DataTypeTok       = "dt"
short DecValTok         = "dv"
short BaseNTok          = "bn"
short FloatTok          = "fl"
short CharTok           = "ch"
short StringTok         = "st"
short CommentTok        = "co"
short OtherTok          = "ot"
short AlertTok          = "al"
short FunctionTok       = "fu"
short RegionMarkerTok   = "re"
short ErrorTok          = "er"
short ConstantTok       = "cn"
short SpecialCharTok    = "sc"
short VerbatimStringTok = "vs"
short SpecialStringTok  = "ss"
short ImportTok         = "im"
short DocumentationTok  = "do"
short AnnotationTok     = "an"
short CommentVarTok     = "cv"
short VariableTok       = "va"
short ControlFlowTok    = "cf"
short OperatorTok       = "op"
short BuiltInTok        = "bu"
short ExtensionTok      = "ex"
short PreprocessorTok   = "pp"
short AttributeTok      = "at"
short InformationTok    = "in"
short WarningTok        = "wa"
short NormalTok         = ""

