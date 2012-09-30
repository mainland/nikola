--
-- This code is adapted from Ben Lippmeier's Gloss library
--

module GUI.Commands where

import qualified Data.Map as Map
import qualified Graphics.UI.GLUT as GLUT

data Key = CharKey     Char
         | SpecialKey  SpecialKey
         | MouseButton MouseButton
  deriving (Show, Eq, Ord)

data MouseButton = LeftButton
                 | MiddleButton
                 | RightButton
                 | WheelUp
                 | WheelDown
                 | AdditionalButton Int
  deriving (Show, Eq, Ord)

data KeyState = Down
              | Up
  deriving (Show, Eq, Ord)

data SpecialKey = KeyUnknown
                | KeySpace
                | KeyEsc
                | KeyF1
                | KeyF2
                | KeyF3
                | KeyF4
                | KeyF5
                | KeyF6
                | KeyF7
                | KeyF8
                | KeyF9
                | KeyF10
                | KeyF11
                | KeyF12
                | KeyF13
                | KeyF14
                | KeyF15
                | KeyF16
                | KeyF17
                | KeyF18
                | KeyF19
                | KeyF20
                | KeyF21
                | KeyF22
                | KeyF23
                | KeyF24
                | KeyF25
                | KeyUp
                | KeyDown
                | KeyLeft
                | KeyRight
                | KeyTab
                | KeyEnter
                | KeyBackspace
                | KeyInsert
                | KeyNumLock
                | KeyBegin
                | KeyDelete
                | KeyPageUp
                | KeyPageDown
                | KeyHome
                | KeyEnd
                | KeyPad0
                | KeyPad1
                | KeyPad2
                | KeyPad3
                | KeyPad4
                | KeyPad5
                | KeyPad6
                | KeyPad7
                | KeyPad8
                | KeyPad9
                | KeyPadDivide
                | KeyPadMultiply
                | KeyPadSubtract
                | KeyPadAdd
                | KeyPadDecimal
                | KeyPadEqual
                | KeyPadEnter
  deriving (Show, Eq, Ord)

data Modifiers = Modifiers
    { shift :: KeyState
    , ctrl  :: KeyState
    , alt   :: KeyState
    }
  deriving (Show, Eq, Ord)

data Command = CRestore

             | CTranslate
             | CRotate

             -- bump zoom
             | CBumpZoomOut
             | CBumpZoomIn

             -- bump translate
             | CBumpLeft
             | CBumpRight
             | CBumpUp
             | CBumpDown

             -- bump rotate
             | CBumpClockwise
             | CBumpCClockwise
  deriving (Show, Eq, Ord)

type Commands = Map.Map Command [(Key, Maybe Modifiers)]

commands :: Commands
commands =
    Map.fromList defaultCommands
  where
    defaultCommands :: [(Command, [(Key, Maybe Modifiers)])]
    defaultCommands =
        [ (CRestore, [(CharKey 'r', Nothing)])

        , (CTranslate, [ (MouseButton LeftButton, Just (Modifiers { shift = Up
                                                                  , ctrl  = Up
                                                                  , alt   = Up }))
                       ])

        , (CRotate, [ (MouseButton RightButton, Nothing)
                    , (MouseButton LeftButton , Just (Modifiers { shift = Up
                                                                , ctrl  = Down
                                                                , alt   = Up }))
                    ])

        -- bump zoom
        , (CBumpZoomOut, [ (MouseButton WheelDown,   Nothing)
                         , (SpecialKey  KeyPageDown, Nothing)
                         ])
        , (CBumpZoomIn,  [ (MouseButton WheelUp,     Nothing)
                         , (SpecialKey  KeyPageUp,   Nothing)
                         ])

        -- bump translate
        , (CBumpLeft,  [(SpecialKey KeyLeft,  Nothing)])
        , (CBumpRight, [(SpecialKey KeyRight, Nothing)])
        , (CBumpUp,    [(SpecialKey KeyUp,    Nothing)])
        , (CBumpDown,  [(SpecialKey KeyDown,  Nothing)])

        -- bump rotate
        , (CBumpClockwise,  [(SpecialKey KeyHome, Nothing)])
        , (CBumpCClockwise, [(SpecialKey KeyEnd,  Nothing)])
        ]

isCommand :: Commands -> Command -> Key -> Modifiers -> Bool
isCommand commands cmd key keyMods =
    case Map.lookup cmd commands of
      Nothing      -> False
      Just matches -> or $ map (go key keyMods) matches
  where
    go :: Key -> Modifiers -> (Key, Maybe Modifiers) -> Bool
    go key _ (cmdKey, Nothing) =
        key == cmdKey

    go key keyMods (cmdKey, Just cmdKeyMods) =
        key == cmdKey && keyMods == cmdKeyMods

glutKeyToKey :: GLUT.Key -> Key
glutKeyToKey (GLUT.Char '\32')                            = SpecialKey KeySpace
glutKeyToKey (GLUT.Char '\13')                            = SpecialKey KeyEnter
glutKeyToKey (GLUT.Char '\9')                             = SpecialKey KeyTab
glutKeyToKey (GLUT.Char '\ESC')                           = SpecialKey KeyEsc
glutKeyToKey (GLUT.Char '\DEL')                           = SpecialKey KeyDelete
glutKeyToKey (GLUT.Char c)                                = CharKey c
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF1)                 = SpecialKey KeyF1
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF2)                 = SpecialKey KeyF2
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF3)                 = SpecialKey KeyF3
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF4)                 = SpecialKey KeyF4
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF5)                 = SpecialKey KeyF5
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF6)                 = SpecialKey KeyF6
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF7)                 = SpecialKey KeyF7
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF8)                 = SpecialKey KeyF8
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF9)                 = SpecialKey KeyF9
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF10)                = SpecialKey KeyF10
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF11)                = SpecialKey KeyF11
glutKeyToKey (GLUT.SpecialKey GLUT.KeyF12)                = SpecialKey KeyF12
glutKeyToKey (GLUT.SpecialKey GLUT.KeyLeft)               = SpecialKey KeyLeft
glutKeyToKey (GLUT.SpecialKey GLUT.KeyUp)                 = SpecialKey KeyUp
glutKeyToKey (GLUT.SpecialKey GLUT.KeyRight)              = SpecialKey KeyRight
glutKeyToKey (GLUT.SpecialKey GLUT.KeyDown)               = SpecialKey KeyDown
glutKeyToKey (GLUT.SpecialKey GLUT.KeyPageUp)             = SpecialKey KeyPageUp
glutKeyToKey (GLUT.SpecialKey GLUT.KeyPageDown)           = SpecialKey KeyPageDown
glutKeyToKey (GLUT.SpecialKey GLUT.KeyHome)               = SpecialKey KeyHome
glutKeyToKey (GLUT.SpecialKey GLUT.KeyEnd)                = SpecialKey KeyEnd
glutKeyToKey (GLUT.SpecialKey GLUT.KeyInsert)             = SpecialKey KeyInsert
glutKeyToKey (GLUT.SpecialKey GLUT.KeyNumLock)            = SpecialKey KeyNumLock
glutKeyToKey (GLUT.SpecialKey GLUT.KeyBegin)              = SpecialKey KeyBegin
glutKeyToKey (GLUT.SpecialKey GLUT.KeyDelete)             = SpecialKey KeyDelete
glutKeyToKey (GLUT.SpecialKey (GLUT.KeyUnknown _))        = SpecialKey KeyUnknown
glutKeyToKey (GLUT.MouseButton GLUT.LeftButton)           = MouseButton LeftButton
glutKeyToKey (GLUT.MouseButton GLUT.MiddleButton)         = MouseButton MiddleButton
glutKeyToKey (GLUT.MouseButton GLUT.RightButton)          = MouseButton RightButton
glutKeyToKey (GLUT.MouseButton GLUT.WheelUp)              = MouseButton WheelUp
glutKeyToKey (GLUT.MouseButton GLUT.WheelDown)            = MouseButton WheelDown
glutKeyToKey (GLUT.MouseButton (GLUT.AdditionalButton i)) = MouseButton (AdditionalButton i)

glutKeyStateToKeyState :: GLUT.KeyState -> KeyState
glutKeyStateToKeyState GLUT.Down = Down
glutKeyStateToKeyState GLUT.Up   = Up

glutModifiersToModifiers :: GLUT.Modifiers -> Modifiers
glutModifiersToModifiers (GLUT.Modifiers a b c) =
    Modifiers (glutKeyStateToKeyState a)
              (glutKeyStateToKeyState b)
              (glutKeyStateToKeyState c)
