module Config where

import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))

data Config = Config
    { confSize   :: Last Int
    , confLimit  :: Last Int
    , confNikola :: Last Bool
    , confBench  :: Last Bool
    , confHelp   :: Last Bool
    }
  deriving (Eq, Show)

instance Monoid Config where
    mempty  = emptyConfig
    mappend = appendConfig

defaultConfig :: Config
defaultConfig = Config
    { confSize   = ljust 512
    , confLimit  = ljust 255
    , confNikola = ljust False
    , confBench  = ljust False
    , confHelp   = ljust False
    }

emptyConfig :: Config
emptyConfig = Config
    { confSize   = mempty
    , confLimit  = mempty
    , confNikola = mempty
    , confBench  = mempty
    , confHelp   = mempty
    }

appendConfig :: Config -> Config -> Config
appendConfig a b = Config
    { confSize   = app confSize a b
    , confLimit  = app confLimit a b
    , confNikola = app confNikola a b
    , confBench  = app confBench a b
    , confHelp   = app confHelp a b
    }
  where
    app f = mappend `on` f

ljust :: a -> Last a
ljust = Last . Just

fromLJust :: (Config -> Last a)
          -> Config
          -> a
fromLJust fld f =
    case fld f of
      Last Nothing  -> fromLJust fld defaultConfig
      Last (Just a) -> a
