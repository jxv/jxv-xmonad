module JxvXMonad
  ( runIO
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Default (def)
import Data.String (IsString)
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

runIO :: IO ()
runIO = do
  let bar = "xmobar" :: String
  let current = xmobarColor "red" "" . wrap "[" "]" :: WorkspaceId -> String
  let prettyPrinting = xmobarPP { ppCurrent = current }
  let config = def { focusedBorderColor = "red", terminal = "urxvt", modMask = mod4Mask }
  xmonad config

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask} = (modMask, xK_b)
