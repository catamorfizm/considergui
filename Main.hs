import LuaParser
import Data.List
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade
import Data.IORef
import Control.Exception
import Control.Monad
import System.Directory
import Data.Maybe
import qualified Data.ByteString.Char8 as C8

settingsFile = ".considerguirc"
gladeFile = "gui.glade"

defaultSettings = [("folder", "wow"), ("account", "a1")]
accountFolder d = d++"/WTF/Account"
dbFile d a = accountFolder d ++ "/" ++ a ++ "/SavedVariables/Considerater.lua"

data GUI = GUI { guiW1 :: Window
               , guiConfigD
               , guiHelpD
               , guiStatD :: Dialog
               , guiQuitTB
               , guiHelpTB
               , guiConfigTB :: ToolButton
               , guiAccountCB
               , guiToonCB
               , guiProfCB :: ComboBox
               , guiEditProfB
               , guiHelpCloseB :: Button
               }

type Settings = IORef [(String, String)]

data State = S { stateGUI :: GUI, settings :: Settings, db :: IORef (Maybe LuaExpr) }

main = do
  initGUI
  gui <- loadGlade                -- load glade XML
  set <- newIORef []
  db  <- newIORef Nothing
  let s = S gui set db
  loadSettings s                  -- load saved settings
  connectGUI s                    -- setup signals
  widgetShow (guiW1 (stateGUI s)) -- show main window
  mainGUI
  return ()

lsDir dir = filter ((/= ".") . take 1) `fmap` getDirectoryContents dir

saveSettings (S { settings = set }) = do
  v <- get set
  C8.writeFile settingsFile (C8.pack $ show v)

loadSettings (S { settings = set }) =
  handle ((\ _ -> return ()) :: SomeException -> IO ()) $ do
    v <- (read . C8.unpack) `fmap` C8.readFile settingsFile
    set $= v

getSetting (S { settings = set }) name = do
  v <- get set
  return . fromJust
         $ lookup name v `mplus`
           lookup name defaultSettings `mplus`
           Just ""
setSetting (s@S { settings = set }) name v = do
  set $~ (((name, v):) . filter ((/=name) . fst))
  saveSettings s

loadDB s = handle failureCase $ do
  wow  <- getSetting s "folder"
  acnt <- getSetting s "account"
  e_lua <- parseLuaFile (dbFile wow acnt)
  case e_lua of
    Left err  -> print err >> failureCase undefined
    Right (Assign _ lua) -> analyze lua >> db s $= Just lua >> return True
  where failureCase :: SomeException -> IO Bool
        failureCase _ = db s $= Nothing >> return False

analyze (Arr a) = do
  case lookup (Str "profileKeys") a of
    Just (Arr pkeys) -> print (map fst pkeys)
    _ -> putStrLn "Mal-formed profileKeys"
  return ()
analyze _ = putStrLn "Mal-formed saved variables"


loadGlade :: IO GUI
loadGlade = do
  builder <- builderNew         -- GTKBuilder format
  builderAddFromFile builder gladeFile
  let find cast = builderGetObject builder cast
  [w1] <- mapM (find castToWindow) ["window1"]
  [configD, statD, helpD] <- mapM (find castToDialog)
    ["configdialog", "statdialog", "helpdialog"]
  [quitTB,configTB, helpTB] <- mapM (find castToToolButton)
    ["quittoolbutton", "configtoolbutton", "helptoolbutton"]
  [accountCB, toonCB, profCB] <- mapM (find castToComboBox)
    ["accountcombobox", "tooncombobox", "profcombobox"]
  [editProfB, helpCloseB] <- mapM (find castToButton)
    ["editprofbutton", "helpclosebutton"]
  return $ GUI { guiW1         = w1
               , guiConfigD    = configD
               , guiHelpD      = helpD
               , guiStatD      = statD
               , guiQuitTB     = quitTB
               , guiConfigTB   = configTB
               , guiHelpTB     = helpTB
               , guiAccountCB  = accountCB
               , guiToonCB     = toonCB
               , guiProfCB     = profCB
               , guiEditProfB  = editProfB
               , guiHelpCloseB = helpCloseB
               }

connectGUI s = do
  let gui = stateGUI s

  -- quit buttons
  onDestroy (guiW1 gui) mainQuit
  onToolButtonClicked (guiQuitTB gui) mainQuit

  -- options
  onToolButtonClicked (guiConfigTB gui) (showConfigWin s)
  
  -- comboboxes  
  guiAccountCB gui `on` changed $ comboBoxChangeFun "account" (guiAccountCB gui)
  guiToonCB gui `on` changed $ comboBoxChangeFun "toon" (guiToonCB gui)

  onClicked (guiEditProfB gui) (widgetShow (guiStatD gui))

  -- help
  onToolButtonClicked (guiHelpTB gui) (widgetShow (guiHelpD gui))
  onClicked (guiHelpCloseB gui) (widgetHide (guiHelpD gui))

  refreshMainWin s

  where
    comboBoxChangeFun settingName cb = do
      m_a  <- comboBoxGetActiveText cb
      olda <- getSetting s settingName
      case m_a of
        Just a | a /= olda -> setSetting s settingName a >> refreshMainWin s
        _ -> return ()

  
showConfigWin s = do
  let gui = stateGUI s
  widgetShow (guiConfigD gui)
  return ()

refreshMainWin s = do
  let gui = stateGUI s
  loadSettings s
  loadDB s

  comboBoxTextClear (guiAccountCB gui)
  comboBoxTextClear (guiToonCB gui)
  comboBoxTextClear (guiProfCB gui)

  wow <- getSetting s "folder"
  whenIO (doesDirectoryExist (accountFolder wow)) $ do
    accounts <- lsDir (accountFolder wow)
    mapM_ (comboBoxAppendText (guiAccountCB gui)) accounts
    account <- getSetting s "account"
    case findIndex (==account) accounts of
      Just i  -> comboBoxSetActive (guiAccountCB gui) i
      Nothing -> return ()

    whenIO (isJust `fmap` get (db s)) $ do
      Arr pkeys <- ((|-> "profileKeys") . fromJust) `fmap` get (db s)
      let toons = mapMaybe (\ (k, _) -> case k of Str s -> Just s; _ -> Nothing) pkeys
      mapM_ (comboBoxAppendText (guiToonCB gui)) toons
      toon <- getSetting s "toon"
      case findIndex (==toon) toons of
        Nothing -> return ()
        Just i  -> do
          comboBoxSetActive (guiToonCB gui) i
          Str profKey <- ((|-> toon) . (|-> "profileKeys") . fromJust) `fmap` get (db s)
          Arr profs <- ((|-> "profiles") . (|-> profKey) . (|-> "profiles") . fromJust) `fmap` get (db s)
          print profs



comboBoxTextClear cb = cellLayoutClear cb >> comboBoxSetModelText cb

whenIO condIO thenIO = condIO >>= (\ b -> if b then thenIO else return ())

Arr a |-> str = maybe (Arr []) id (lookup (Str str) a)

x $= v = writeIORef x v
x $~ f = (f `fmap` readIORef x) >>= writeIORef x
get x = readIORef x
