module Color
    ( module E
    , withColor
    , withColor'
    ) where

import           Data.String         (IsString (..))
import           System.Console.ANSI as E (Color (..), ColorIntensity (..))
import qualified System.Console.ANSI as ANSI
import           Universum

withColor' :: (IsString s, Monoid s) => (ColorIntensity, Color) -> s -> s
withColor' (intensity, color) text = mconcat
    [ fromString $
      ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color]
    , text
    , fromString $
      ANSI.setSGRCode [ANSI.Reset]
    ]

withColor :: (IsString s, Monoid s) => Color -> s -> s
withColor color = withColor' (Vivid, color)

