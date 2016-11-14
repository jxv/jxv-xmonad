module JxvXMonad
  ( Bar(..)
  , XMonadConfigModified(..)
  , XMonadConfig(..)
  , PrettyPrinting(..)
  , Wires(..)
  , main
  , runIO
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Default (def)
import Data.String (IsString)
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

newtype Bar = Bar String
  deriving (Show, Eq, IsString)

newtype XMonadConfigModified = XMonadConfigModified (XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))))

newtype XMonadConfig = XMonadConfig (XConfig (Choose Tall (Choose (Mirror Tall) Full)))

newtype PrettyPrinting = PrettyPrinting PP

instance Show PrettyPrinting where
  show (PrettyPrinting _) = "PrettyPrinting <PP>"

class Monad m => Wires m where
  runXMonadConfig :: Bar -> PrettyPrinting -> (XConfig Layout -> (KeyMask, KeySym)) -> XMonadConfig -> m XMonadConfigModified
  runXMonad :: XMonadConfigModified -> m ()

main :: Wires m => m ()
main = do
  let bar = "xmobar" :: Bar
  let current = xmobarColor "red" "" . wrap "[" "]" :: WorkspaceId -> String
  let prettyPrinting = PrettyPrinting $ xmobarPP { ppCurrent = current } :: PrettyPrinting
  let myConfig = XMonadConfig $ def { focusedBorderColor = "red", terminal = "urxvt", modMask = mod4Mask } :: XMonadConfig
  config <- runXMonadConfig bar prettyPrinting toggleStrutsKey myConfig
  runXMonad config

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask} = (modMask, xK_b)

newtype JxvXMonad a = JxvXMonad (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: JxvXMonad a -> IO a
runIO (JxvXMonad m) = m

instance Wires JxvXMonad where
  runXMonadConfig = runXMonadConfig'
  runXMonad = runXMonad'

runXMonadConfig' :: MonadIO m => Bar -> PrettyPrinting -> (XConfig Layout -> (KeyMask, KeySym)) -> XMonadConfig -> m XMonadConfigModified
runXMonadConfig' (Bar bar) (PrettyPrinting pp) toggleStrutsKey (XMonadConfig config) = liftIO $ XMonadConfigModified <$> statusBar bar pp toggleStrutsKey config

runXMonad' :: MonadIO m => XMonadConfigModified -> m ()
runXMonad' (XMonadConfigModified config) = liftIO $ xmonad config
