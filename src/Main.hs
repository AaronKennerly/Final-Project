{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import FinalProject

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _result :: String 
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | Add'   | Sub'   | Mul'  | Div' | Exp'
  | Gal'   | Quart' | Pint' | Cup' | FlOz'
  | Doll'  | Euro'  | Pound'| Pesos'
  | Seven' | Eight' | Nine'
  | Four'  | Five'  | Six'
  | One'   | Two'   | Three'
  | Zero'  | Dot'   | LPar' | RPar'
  | Calc'  | Clear'
  | LCon'  | MCon'
 deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack_ [childSpacing_ 15] [
      label "This is the front-end for a calculator.",
      label $ "Enter a Expression: " <> showt (model ^. result),
      spacer,
      hstack [
        spacer,
        button "Gallon" Gal', spacer, button "Quart" Quart', spacer,
        button "Pint" Pint', spacer, button "Cup" Cup', spacer,
        button "Fl oz" FlOz'
      ],
      hstack [
        spacer,
        button "Dollars" Doll', spacer, button "Euros" Euro', spacer,
        button "Pound" Pound', spacer, button "Pesos" Pesos'
      ],
      hstack [
        spacer,
        button "+" Add', spacer, button "-" Sub', spacer,
        button "*" Mul', spacer, button "/" Div', spacer,
        button "^" Exp'
      ],
      hstack [
        spacer,
        button "7" Seven', spacer, button "8" Eight', spacer,
        button "9" Nine'
      ],
      hstack [
        spacer,
        button "4" Four', spacer, button "5" Five', spacer,
        button "6" Six'
      ],
      hstack [
        spacer,
        button "1" One', spacer, button "2" Two', spacer,
        button "3" Three'
      ],
      hstack [
        spacer,
        button "0" Zero', spacer, button "." Dot', spacer,
        button "(" LPar', spacer, button ")" RPar'
      ],
      hstack [
        spacer,
        button "Calculate" Calc', spacer, button "Clear" Clear'
      ],
      hstack [
        spacer,
        button "Liquid Con" LCon', spacer, button "Curr Con" MCon' 
      ]
    ] `styleBasic` [padding 25]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  Add'   -> [Model (model & result %~ (++ "+"))]
  Sub'   -> [Model (model & result %~ (++ "-"))]
  Mul'   -> [Model (model & result %~ (++ "*"))]
  Div'   -> [Model (model & result %~ (++ "/"))]
  Exp'   -> [Model (model & result %~ (++ "^"))]
  Gal'   -> [Model (model & result %~ (++ "G"))]
  Quart' -> [Model (model & result %~ (++ "Q"))]
  Pint'  -> [Model (model & result %~ (++ "P"))]
  Cup'   -> [Model (model & result %~ (++ "C"))]
  FlOz'  -> [Model (model & result %~ (++ "F"))]
  Doll'  -> [Model (model & result %~ (++ "$"))]
  Euro'  -> [Model (model & result %~ (++ "E"))]
  Pound' -> [Model (model & result %~ (++ "K"))]
  Pesos' -> [Model (model & result %~ (++ "D"))]
  Zero'  -> [Model (model & result %~ (++ "0"))]
  One'   -> [Model (model & result %~ (++ "1"))]
  Two'   -> [Model (model & result %~ (++ "2"))]
  Three' -> [Model (model & result %~ (++ "3"))]
  Four'  -> [Model (model & result %~ (++ "4"))]
  Five'  -> [Model (model & result %~ (++ "5"))]
  Six'   -> [Model (model & result %~ (++ "6"))]
  Seven' -> [Model (model & result %~ (++ "7"))]
  Eight' -> [Model (model & result %~ (++ "8"))]
  Nine'  -> [Model (model & result %~ (++ "9"))]
  Dot'   -> [Model (model & result %~ (++ "."))]
  LPar'  -> [Model (model & result %~ (++ "("))]
  RPar'  -> [Model (model & result %~ (++ ")"))]
  Clear' -> [Model (model & result .~ "")]
  Calc'  -> case model ^. result of
                "" -> [Model (model & result .~ "Error need input")]
                input -> let output = repl [] input
                          in [Model (model & result .~ output)]
  LCon'  -> case model ^. result of
                "" -> [Model (model & result .~ "Error need input")]
                input -> let output = lConvert (lexCon input)
                          in [Model (model & result .~ output)]
  MCon'  -> case model ^. result of
                "" -> [Model (model & result .~ "Error need input")]
                input -> let output = mConvert (lexCon input)
                          in [Model (model & result .~ output)]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Calculator",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel ""
