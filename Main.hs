 ---------------------------------------------------------------------------
 -- This program is free software: you can redistribute it and/or modify  --
 -- it under the terms of the GNU General Public License as published by  --
 -- the Free Software Foundation, either version 3 of the License, or     --
 -- (at your option) any later version.                                   --
 --                                                                       --
 -- This program is distributed in the hope that it will be useful,       --
 -- but WITHOUT ANY WARRANTY; without even the implied warranty of        --
 -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
 -- GNU General Public License for more details.                          --
 --                                                                       --
 -- You should have received a copy of the GNU General Public License     --
 -- along with this program.  If not, see <http://www.gnu.org/licenses/>. --
 ---------------------------------------------------------------------------
import Prelude hiding (catch)
import LuaParser
import Data.List
import Data.Char
import Graphics.UI.Gtk hiding (get)
import Data.IORef
import Control.Exception
import Control.Monad
import Control.Concurrent
import System.Directory
import System.FilePath
import System.Process
import System.IO
import Data.Maybe
import qualified Data.ByteString.Char8 as C8

appName = "ConsiderGUI"
settingsFile = "settings"
gladeFile = "gui.glade"
maxWeight = 25
regionCodes = ["us","eu","tw","cn"]
progressUpdateInterval = 100 -- msec

defaultSettings = [ ("region", "us")
                  , ("simspec", "primary")
                  , ("simcOptions", unlines ["iterations=10000\nthreads=1\nskill=0.8"]) ]
accountFolder d = d </> "WTF" </> "Account"
dbFile d a = accountFolder d </> a </> "SavedVariables" </> "Considerater" <.> "lua"

data GUI = GUI { guiW1 :: Window
               , guiConfigD
               , guiHelpD
               , guiStatD
               , guiSimcD
               , guiNewProfD :: Dialog
               , guiQuitTB
               , guiHelpTB
               , guiConfigTB :: ToolButton
               , guiAccountCB
               , guiToonCB
               , guiProfCB
               , guiRegionCB
               , guiCopyFromCB
               , guiSimSpecCB :: ComboBox
               , guiEditProfB
               , guiHelpCloseB
               , guiSimcB
               , guiSimExecB
               , guiSimStopB
               , guiCfgOkB
               , guiCfgCancelB
               , guiStatOkB
               , guiStatCancelB
               , guiSimCancelB
               , guiSimApplyB
               , guiNewProfB
               , guiDelProfB :: Button
               , guiSimSB :: Statusbar
               , guiStatScales :: [(String, HScale)]
               , guiHelpTV
               , guiSimcOutTV
               , guiSimcOptTV :: TextView
               , guiWoWFolderFC
               , guiSimCProgFC :: FileChooser
               , guiNewProfE :: Entry
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
  wow <- getSetting s "folder"
  when (null wow) $ do
    -- show a "first time" message if folder not set
    flip timeoutAdd 100 $ do
      m_folder <- guessFolder
      msg <- case m_folder of
        Just folder -> setSetting s "folder" folder >>
                       return checkGuessMsg
        Nothing     -> return firstTimeMsg
      d <- messageDialogNew (Just (guiW1 (stateGUI s)))
                            [DialogModal]
                            MessageInfo ButtonsOk msg
      dialogRun d
      widgetDestroy d
      showConfigDialog s
      return False
    return ()
  mainGUI
  return ()

-- list of dir contents
lsDir dir = filter ((/= ".") . take 1) `fmap` getDirectoryContents dir
  `catch` ((\ _ -> return []) :: SomeException -> IO [String])

-- figure out application settings folder
appSettingsFolder = do
  f <- getAppUserDataDirectory appName
  createDirectoryIfMissing True f
  return f
  `catch` ((\ _ -> return "") :: SomeException -> IO FilePath)

-- save/load/get/set for persistent settings
saveSettings (S { settings = set }) = do
  f <- appSettingsFolder
  v <- get set
  C8.writeFile (f </> settingsFile) (C8.pack $ show v)

loadSettings (S { settings = set }) =
  handle ((\ _ -> return ()) :: SomeException -> IO ()) $ do
    f <- appSettingsFolder
    v <- (read . C8.unpack) `fmap` C8.readFile (f </> settingsFile)
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

-- save/load DB for the ConsideraterDB
loadDB s = handle failureCase $ do
  wow  <- getSetting s "folder"
  acnt <- getSetting s "account"
  e_lua <- parseLuaFile (dbFile wow acnt)
  case e_lua of
    Left err  -> print err >> failureCase undefined
    Right (Assign _ lua) -> db s $= Just lua >> return True
  where failureCase :: SomeException -> IO Bool
        failureCase _ = db s $= Nothing >> return False

saveDB s = handle failureCase $ do
  wow  <- getSetting s "folder"
  acnt <- getSetting s "account"
  m_e <- get (db s)
  case m_e of
    Nothing -> return False
    Just e  -> do
      C8.writeFile (dbFile wow acnt) (C8.pack $ show (Assign "ConsideraterDB" e))
      return True
  where failureCase :: SomeException -> IO Bool
        failureCase _ = db s $= Nothing >> return False

-- read the GUI widgets and setup GUI record
loadGlade :: IO GUI
loadGlade = do
  builder <- builderNew         -- GTKBuilder format
  builderAddFromFile builder gladeFile
  let find cast = builderGetObject builder cast
  [w1] <- mapM (find castToWindow) ["window1"]
  [configD, statD, helpD, simcD, newProfD] <- mapM (find castToDialog)
    ["configdialog", "statdialog", "helpdialog", "simcdialog", "newprofdialog"]
  [quitTB,configTB, helpTB] <- mapM (find castToToolButton)
    ["quittoolbutton", "configtoolbutton", "helptoolbutton"]
  [accountCB, toonCB, profCB, regionCB,
   copyFromCB, simSpecCB] <- mapM (find castToComboBox)
    [ "accountcombobox", "tooncombobox", "profcombobox"
    , "regioncombobox", "copyfromcombobox", "simspeccombobox" ]
  [ editProfB, helpCloseB, statOkB, statCancelB, 
    cfgOkB, cfgCancelB, simcB, simExecB, simStopB,
    simCancelB, simApplyB, newProfB, delProfB ]
    <- mapM (find castToButton)
    [ "editprofbutton", "helpclosebutton", "statok", "statcancel"
    , "configok", "configcancel", "simcbutton", "simexecbutton"
    , "simstopbutton", "simcancelbutton", "simapplybutton"
    , "newprofbutton", "delprofbutton" ]
  hscales <- forM stathscales $ \ (scaleName, statName) -> do
    s <- find castToHScale scaleName
    rangeSetRange s 0 maxWeight
    rangeSetIncrements s 0.1 1.0
    return (statName, s)
  [simcOptTV, simcOutTV, helpTV] <- mapM (find castToTextView)
    ["simoptionstextview", "simoutputtextview", "helptextview"]
  [folderFC, simcFC] <- mapM (find castToFileChooser)
    ["wowfolderfilechooser", "simcfilechooser"]
  [simSB] <- mapM (find castToStatusbar)
    ["simstatusbar"]
  [newProfE] <- mapM (find castToEntry)
    ["newprofentry"]
  return $ GUI { guiW1          = w1
               , guiConfigD     = configD
               , guiHelpD       = helpD
               , guiStatD       = statD
               , guiSimcD       = simcD
               , guiNewProfD    = newProfD
               , guiQuitTB      = quitTB
               , guiConfigTB    = configTB
               , guiHelpTB      = helpTB
               , guiAccountCB   = accountCB
               , guiToonCB      = toonCB
               , guiProfCB      = profCB
               , guiRegionCB    = regionCB
               , guiCopyFromCB  = copyFromCB
               , guiSimSpecCB   = simSpecCB
               , guiEditProfB   = editProfB
               , guiNewProfB    = newProfB
               , guiDelProfB    = delProfB
               , guiHelpCloseB  = helpCloseB
               , guiSimcB       = simcB
               , guiSimExecB    = simExecB
               , guiSimStopB    = simStopB
               , guiSimCancelB  = simCancelB
               , guiSimApplyB   = simApplyB
               , guiStatOkB     = statOkB
               , guiStatCancelB = statCancelB
               , guiCfgOkB      = cfgOkB
               , guiCfgCancelB  = cfgCancelB
               , guiStatScales  = hscales
               , guiHelpTV      = helpTV
               , guiSimcOutTV   = simcOutTV
               , guiSimcOptTV   = simcOptTV
               , guiWoWFolderFC = folderFC
               , guiSimCProgFC  = simcFC
               , guiSimSB       = simSB
               , guiNewProfE    = newProfE
               }

-- configure the GUI with handlers and fill in various widgets
connectGUI s = do
  let gui = stateGUI s

  -- quit buttons
  guiW1 gui `onDestroy` mainQuit
  guiQuitTB gui `onToolButtonClicked` mainQuit

  -- options
  ff <- fileFilterNew
  fileFilterSetName ff "simc"
  fileFilterAddPattern ff "simc"
  fileFilterAddPattern ff "simc.exe"
  fileChooserSetFilter (guiSimCProgFC gui) ff
  guiConfigTB gui `onToolButtonClicked` showConfigDialog s
  guiCfgCancelB gui `onClicked` widgetHide (guiConfigD gui)
  guiCfgOkB gui `onClicked` do
    m_folder <- fileChooserGetCurrentFolder (guiWoWFolderFC gui)
    m_simc <- fileChooserGetFilename (guiSimCProgFC gui)
    when (isJust m_folder) $ setSetting s "folder" (fromJust m_folder)
    when (isJust m_simc) $ setSetting s "simc" (fromJust m_simc)
    sanityCheckFolder s
    refreshMainWin s
    widgetHide (guiConfigD gui)
  guiConfigD gui `windowSetTransientFor` guiW1 gui
  comboBoxTextClear (guiRegionCB gui)
  mapM_ (comboBoxAppendText (guiRegionCB gui)) regionCodes
  region <- getSetting s "region"
  case findIndex (==region) regionCodes of
    Just i  -> comboBoxSetActive (guiRegionCB gui) i
    Nothing -> return ()
  guiRegionCB gui `on` changed $ comboBoxChangeFun "region" (guiRegionCB gui)

  -- comboboxes  
  guiAccountCB gui `on` changed $ comboBoxChangeFun "account" (guiAccountCB gui)
  guiToonCB gui `on` changed $ comboBoxChangeFun "toon" (guiToonCB gui)
  guiProfCB gui `on` changed $ comboBoxChangeFun "prof" (guiProfCB gui)

  -- new profile button
  guiNewProfB gui `onClicked` do
    entrySetText (guiNewProfE gui) ""
    comboBoxTextClear (guiCopyFromCB gui)
    profList <- comboBoxGetModelText (guiProfCB gui) >>= listStoreToList
    mapM_ (comboBoxAppendText (guiCopyFromCB gui)) profList
    resp <- dialogRun (guiNewProfD gui)
    when (resp == ResponseOk) $ do
      name <- entryGetText (guiNewProfE gui)
      when (not (null name) && name `notElem` profList) $ do
        copy <- comboBoxGetActiveText (guiCopyFromCB gui)
        setSetting s "prof" name
        copyStats s copy
        refreshMainWin s
    widgetHide (guiNewProfD gui)
  guiNewProfD gui `windowSetTransientFor` guiW1 gui
  -- edit profile buttons
  guiEditProfB gui `onClicked` do
    prof <- getSetting s "prof"
    unless (null prof) $ showStatDialog s
    widgetSetSensitive (guiW1 gui) False
  guiStatCancelB gui `onClicked` widgetHide (guiStatD gui)
  guiStatOkB gui `onClicked` do
    saveStats s
    widgetHide (guiStatD gui)
  guiStatD gui `onHide` do
    widgetSetSensitive (guiW1 gui) True
  guiStatD gui `windowSetTransientFor` guiW1 gui
  -- del profile button
  guiDelProfB gui `onClicked` do
    prof <- getSetting s "prof"
    unless (null prof) $ do
      delD <- messageDialogNew (Just (guiW1 gui)) []
                               MessageQuestion ButtonsYesNo
                               ("Delete "++prof++"?")
      resp <- dialogRun delD
      when (resp==ResponseYes) $ do
        delStats s
        refreshMainWin s
      widgetDestroy delD
  -- simc buttons
  guiSimcB gui `onClicked` do
    simcOptions <- getSetting s "simcOptions"
    optTB <- textViewGetBuffer (guiSimcOptTV gui)
    textBufferSetText optTB simcOptions    
    widgetShow (guiSimcD gui)
  guiSimExecB gui `onClicked` runSimC s
  guiSimCancelB gui `onClicked` widgetHide (guiSimcD gui)
  widgetSetSensitive (guiSimApplyB gui) False
  guiSimApplyB gui `onClicked` do
    m_sf <- parseScaleFactors s
    case m_sf of
      Nothing -> return ()
      Just sf -> setScaleFactors s sf
    widgetHide (guiSimcD gui)
  comboBoxTextClear $ guiSimSpecCB gui
  mapM_ (comboBoxAppendText (guiSimSpecCB gui)) ["primary", "secondary"]
  spec <- getSetting s "simspec"
  comboBoxSetActive (guiSimSpecCB gui) (if spec == "primary" then 0 else 1)
  guiSimcD gui `windowSetTransientFor` guiStatD gui

  -- help
  guiHelpTB gui `onToolButtonClicked` widgetShow (guiHelpD gui)
  guiHelpCloseB gui `onClicked` widgetHide (guiHelpD gui)
  helpTB <- textViewGetBuffer (guiHelpTV gui)
  textBufferSetText helpTB helpText
  guiHelpD gui `windowSetTransientFor` guiW1 gui

  refreshMainWin s

  where
    comboBoxChangeFun settingName cb = do
      m_a  <- comboBoxGetActiveText cb
      olda <- getSetting s settingName
      case m_a of
        Just a | a /= olda -> setSetting s settingName a >> refreshMainWin s
        _ -> return ()

-- clear and refill the widgets in the main window
refreshMainWin s = do
  let gui = stateGUI s
  loadSettings s
  loadDB s

  comboBoxTextClear (guiAccountCB gui)
  comboBoxTextClear (guiToonCB gui)
  comboBoxTextClear (guiProfCB gui)

  widgetSetSensitive (guiNewProfB gui) False
  widgetSetSensitive (guiEditProfB gui) False
  widgetSetSensitive (guiDelProfB gui) False

  wow <- getSetting s "folder"
  whenIO (doesDirectoryExist (accountFolder wow)) $ do
    accounts <- lsDir (accountFolder wow)
    forM_ accounts $ comboBoxAppendText (guiAccountCB gui)
    account <- getSetting s "account"
    case findIndex (==account) accounts of
      Just i  -> comboBoxSetActive (guiAccountCB gui) i
      Nothing -> setSetting s "account" "" >> setSetting s "toon" "" >> setSetting s "prof" ""

    whenIO (isJust `fmap` get (db s)) $ do
      Arr pkeys <- ((|-> "profileKeys") . fromJust) `fmap` get (db s)
      let toons = mapMaybe (\ (k, _) -> case k of Str s -> Just s; _ -> Nothing) pkeys
      forM_ toons $ comboBoxAppendText (guiToonCB gui)
      toon <- getSetting s "toon"
      case findIndex (==toon) toons of
        Nothing -> setSetting s "toon" "" >> setSetting s "prof" ""
        Just i  -> do
          widgetSetSensitive (guiNewProfB gui) True
          comboBoxSetActive (guiToonCB gui) i
          Str profKey <- (flip derefExpr [Str "profileKeys", Str toon] . fromJust) `fmap` get (db s)
          Arr profs <- (flip derefExpr [Str "profiles", Str profKey, Str "profiles"] . fromJust) `fmap` get (db s)
          let profNames = sort . flip map profs $ \ (Str name, _) -> name
          forM_ profNames $ comboBoxAppendText (guiProfCB gui)
          prof <- getSetting s "prof"
          case findIndex (==prof) profNames of
            Nothing -> setSetting s "prof" ""
            Just i  -> do
              comboBoxSetActive (guiProfCB gui) i
              widgetSetSensitive (guiEditProfB gui) True
              widgetSetSensitive (guiDelProfB gui) True

-- fill and show the configuration/preferences dialog
showConfigDialog s = do
  let gui = stateGUI s
  folder <- getSetting s "folder"
  fileChooserSetCurrentFolder (guiWoWFolderFC gui) folder
  simc <- getSetting s "simc"
  unless (null simc) $ fileChooserSetFilename (guiSimCProgFC gui) simc >> return ()
  dialogRun (guiConfigD gui)
  return ()

-- delete current profile
delStats s = do
  let gui = stateGUI s
  whenIO (isJust `fmap` get (db s)) $ do
    toon <- getSetting s "toon"
    prof <- getSetting s "prof"
    when (not (null toon) && not (null prof)) $ do
      d <- fromJust `fmap` get (db s)
      let Str profKey = derefExpr d [Str "profileKeys", Str toon]
      db s $~ \ (Just e) -> Just $ pruneExpr e [Str "profiles", Str profKey, Str "profiles", Str prof]
      saveDB s
      setSetting s "prof" ""
      return ()

-- create new profile, optionally cloned from profile
copyStats s copyFrom = do
  let gui = stateGUI s
  whenIO (isJust `fmap` get (db s)) $ do
    toon <- getSetting s "toon"
    prof <- getSetting s "prof"
    when (not (null toon) && not (null prof)) $ do
      d <- fromJust `fmap` get (db s)
      let Str profKey = derefExpr d [Str "profileKeys", Str toon]
      let srcTable = case copyFrom of
                       Just srcName -> derefExpr d [Str "profiles", Str profKey, Str "profiles", Str srcName]
                       Nothing      -> Arr []
      db s $~ \ (Just e) -> Just $ modifyExpr e [Str "profiles", Str profKey, Str "profiles", Str prof] srcTable
      saveDB s
      return ()

-- show the edit profile dialog for current profile
showStatDialog s = do
  let gui = stateGUI s
  whenIO (isJust `fmap` get (db s)) $ do
    toon <- getSetting s "toon"
    prof <- getSetting s "prof"
    when (not (null toon) && not (null prof)) $ do
      Str profKey <- (flip derefExpr [Str "profileKeys", Str toon] . fromJust) `fmap` get (db s)
      profs <- (flip derefExpr [Str "profiles", Str profKey, Str "profiles"] . fromJust) `fmap` get (db s)
      let Arr ptable = profs |-> prof
      forM_ (guiStatScales gui) $ \ (stat, scale) ->
        case lookup (Str stat) ptable of
          Just (Num x) -> rangeSetValue scale x
          Nothing      -> rangeSetValue scale 0
      -- check if simc is present
      getSetting s "simc" >>= doesFileExist >>= widgetSetSensitive (guiSimcB gui)
      widgetShow (guiStatD gui)

-- save the changes made in the edit profile dialog
saveStats s = do
  let gui = stateGUI s
  whenIO (isJust `fmap` get (db s)) $ do
    toon <- getSetting s "toon"
    prof <- getSetting s "prof"
    when (not (null toon) && not (null prof)) $ do
      Str profKey <- (flip derefExpr [Str "profileKeys", Str toon] . fromJust) `fmap` get (db s)
      newTable <- (concat `fmap`) . forM (guiStatScales gui) $ \ (stat, scale) -> do
        x <- rangeGetValue scale
        if x /= 0 then return [(Str stat, Num x)] else return []
      db s $~ \ (Just e) -> Just $ modifyExpr e [Str "profiles", Str profKey, Str "profiles", Str prof] (Arr newTable)
      saveDB s
      return ()

-- normalize a list of scale factors aka weights
normScaleFactors sf = map (\ (n, w) -> (n, w*factor)) sf
  where factor = maxWeight / maximum (map snd sf)

-- set the weights in the edit profile dialog
setScaleFactors s sf =
  forM_ sf $ \ (n, w) -> do
    case lookup n simchscales of
      Nothing     -> return ()
      Just hsName -> do
        let Just hs = lookup hsName (guiStatScales (stateGUI s))
        rangeSetValue hs w

-- parse scale factors from the simulation output
parseScaleFactors s = do
  let gui = stateGUI s
  tb  <- textViewGetBuffer (guiSimcOutTV gui)
  end <- textBufferGetEndIter tb
  res <- textIterBackwardSearch end "Scale Factors:" [] Nothing
  case res of
    Nothing     -> return Nothing
    Just (_, e) -> do
      ls <- lines `fmap` textBufferGetText tb e end False
      let strs = drop 1 . words . last . filter (any isAlpha) $ ls
      return . Just . normScaleFactors .
        flip map strs $ \ str ->
          let (n, w) = break (=='=') str in (n, read (drop 1 w))

-- parse string of form "Toon - Server"
toonNameServer t = (head $ words t, last $ words t)

-- run the simulation, setup handlers for following progress
runSimC s = do
  let gui = stateGUI s
  cid     <- statusbarGetContextId (guiSimSB gui) "runsim"
  tb      <- textViewGetBuffer (guiSimcOutTV gui)
  textBufferSetText tb ""
  cursor  <- textBufferGetInsert tb
  let scrollToEnd = textViewScrollToMark (guiSimcOutTV gui) cursor 0 (Just (1, 0))
  region  <- getSetting s "region"
  toon    <- getSetting s "toon"
  spec    <- maybe "primary" id `fmap` comboBoxGetActiveText (guiSimSpecCB gui)
  let (char, serv) = toonNameServer toon
  let armory = "armory="++region++","++serv++","++char++"|"++spec
  simc    <- getSetting s "simc"
  optTB   <- textViewGetBuffer (guiSimcOptTV gui)
  optBeg  <- textBufferGetStartIter optTB
  optEnd  <- textBufferGetEndIter optTB
  options <- textBufferGetText optTB optBeg optEnd False
  setSetting s "simcOptions" options
  setSetting s "simspec" spec
  widgetSetSensitive (guiSimApplyB gui) False
  statusbarPop (guiSimSB gui) cid
  statusbarPush (guiSimSB gui) cid "Running (this may take a while)..."
  let csf = "calculate_scale_factors=1"
  (_, Just hout, _, pid) <- createProcess (proc simc (armory:csf:lines options))
                                          { std_out = CreatePipe }
  widgetSetSensitive (guiSimStopB gui) True
  widgetSetSensitive (guiSimExecB gui) False
  guiSimStopB gui `onClicked` do
    widgetSetSensitive (guiSimStopB gui) False
    widgetSetSensitive (guiSimExecB gui) True
    statusbarPop (guiSimSB gui) cid
    terminateProcess pid
  let progress = handle ((\ _ -> return False) :: SomeException -> IO Bool) $ do
        code <- getProcessExitCode pid
        case code of
          Just _  -> do
            rest <- C8.unpack `fmap` C8.hGetContents hout
            textBufferInsertAtCursor tb rest
            scrollToEnd
            widgetSetSensitive (guiSimStopB gui) False
            widgetSetSensitive (guiSimExecB gui) True
            statusbarPop (guiSimSB gui) cid
            m_sf <- parseScaleFactors s
            case m_sf of
              Nothing -> widgetSetSensitive (guiSimApplyB gui) False >>
                         statusbarPush (guiSimSB gui) cid "Failure"
              Just _  -> widgetSetSensitive (guiSimApplyB gui) True >>
                         statusbarPush (guiSimSB gui) cid "Success"
            return False
          Nothing -> do
            s <- C8.unpack `fmap` C8.hGetNonBlocking hout 64
            unless (null s) $ do
              textBufferInsertAtCursor tb s
              scrollToEnd
            return True
  timeoutAdd progress progressUpdateInterval
  return ()

-- try to guess a valid folder
guessFolder =
  (lookup True . flip zip possibleLocations) `fmap`
  mapM doesDirectoryExist possibleLocations

-- show a quick dialog box with an error message
showErrMsg msg = do
  d <- messageDialogNew Nothing
                        [DialogModal]
                        MessageError ButtonsClose msg
  dialogRun d
  widgetDestroy d

-- do some basic checks on specified folder, show error messages if it
-- fails any of the checks
sanityCheckFolder s = do
  wow <- getSetting s "folder"
  accounts <- if null wow then return [] else lsDir (accountFolder wow)
  case accounts of
    [] -> showErrMsg "Could not find any accounts in specified Warcraft folder."
    as -> whenIO ((not . or) `fmap` mapM (doesFileExist . dbFile wow) as) $ do
            showErrMsg "Unable to detect presence of Considerater Addon in specified Warcraft folder."

-- clear a Text-only combobox (should be part of Gtk2hs IMO)
comboBoxTextClear cb = cellLayoutClear cb >> comboBoxSetModelText cb

-- convenience functions
whenIO condIO thenIO = condIO >>= (\ b -> if b then thenIO else return ())
Arr a |-> str = maybe (Arr []) id (lookup (Str str) a)
x $= v = writeIORef x v
x $~ f = (f `fmap` readIORef x) >>= writeIORef x
get x = readIORef x

-- table of widget names mapped to internal stat names
stathscales =
  [ ("strhscale", "STR")
  , ("agihscale", "AGI")
  , ("stahscale", "STA")
  , ("inthscale", "INT")
  , ("spihscale", "SPI")
  , ("mashscale", "MASTERY_RATING")
  , ("dpshscale", "DPS")
  , ("sphscale", "SPELL_DMG")
  , ("hithscale", "MELEE_HIT_RATING")
  , ("crithscale", "MELEE_CRIT_RATING")
  , ("hastehscale", "MELEE_HASTE_RATING")
  , ("exphscale", "EXPERTISE_RATING")
  , ("penhscale", "SPELLPEN")
  , ("maxdmghscale", "MAX_DAMAGE")
  , ("armorhscale", "ARMOR")
  , ("bonusarmorhscale", "ARMOR_BONUS")
  , ("dodgehscale", "DODGE_RATING")
  , ("parryhscale", "PARRY_RATING")
  , ("resilhscale", "RESILIENCE_RATING")
  , ("regenhscale", "HEALTH_REG")
  , ("firehscale", "FIRE_RES")
  , ("naturehscale", "NATURE_RES")
  , ("frosthscale", "FROST_RES")
  , ("shadowhscale", "SHADOW_RES")
  , ("arcanehscale", "ARCANE_RES") ]

-- table of SimulationCraft stat names mapped to our stat names
simchscales =
  [ ("Str"      , "STR")                 
  , ("Agi"      , "AGI")                 
  , ("Sta"      , "STA")                 
  , ("Int"      , "INT")                 
  , ("Spi"      , "SPI")                 
  , ("Mastery"  , "MASTERY_RATING")      
  , ("Wdps"     , "DPS")                 
  , ("SP"       , "SPELL_DMG")           
  , ("Hit"      , "MELEE_HIT_RATING")    
  , ("Crit"     , "MELEE_CRIT_RATING")   
  , ("Haste"    , "MELEE_HASTE_RATING")  
  , ("Exp"      , "EXPERTISE_RATING")    
  , ("Armor"    , "ARMOR")               
  , ("Dodge"    , "DODGE_RATING")        
  , ("Parry"    , "PARRY_RATING") ]

helpText = unlines
  [ "ConsiderGUI Help\n"
  , "The purpose of this tool is to help you manage your profiles while out-of-game."
  , "The first step is to configure the tool by selecting Preferences and setting your"
  , "Warcraft Folder to the appropriate location if it is not already correct. If you"
  , "intend to use SimulationCraft then be sure to set that to the simc.exe file that"
  , "you installed.  Finally be sure that your game region is set correctly.\n"
  , "In the main window, you can select your playing account from the dropdown box, then"
  , "the specific character you want to work with, and finally the profile list will"
  , "become active.  At this point you may choose a profile and edit or delete it.  You"
  , "may also create a new profile, with the option of copying settings from an existing"
  , "profile.\n"
  , "The edit profile screen allows you to adjust stat weights, or you may opt to run"
  , "SimulationCraft to automatically adjust your stat weights.  If you have downloaded"
  , "and installed SimulationCraft, and configured the location of simc.exe, then you can"
  , "click on the \"Run SimulationCraft\" button.  This will bring up a dialog with some"
  , "default options and an \"Execute\" button.  Click that button to start the simulation."
  , "It will take some time, and it will display the output in the lower view.  You may"
  , "click \"Stop\" if you want to end the simulation.   Once it succeeds then the \"Apply\""
  , "button will become available.  Clicking this will automatically set stat weights"
  , "according to the results of the simulation.  Remember to click \"Ok\" after reviewing"
  , "the changes to the weights to save them permanently.\n"
  , "SimulationCraft is a program with many advanced options, therefore, the options can"
  , "be edited as plain text, line by line, in the provided space.  The default options"
  , "should suffice for beginners, so if you are unsure, you can just click \"Execute\" at"
  , "this screen.  SimulationCraft gets all of its character-specific data from the"
  , "Warcraft Armory, so it is only as good as that information is." ]

firstTimeMsg = "Please configure the Warcraft Folder before proceeding."
checkGuessMsg = "Please confirm that the Warcraft Folder is set correctly."

-- list of folders to try
possibleLocations = [ joinDrive "C:\\" ("Program Files (x86)" </> "World of Warcraft")
                    , joinDrive "D:\\" ("Program Files (x86)" </> "World of Warcraft")
                    , joinDrive "C:\\" ("Program Files" </> "World of Warcraft")
                    , joinDrive "D:\\" ("Program Files" </> "World of Warcraft")
                    , joinDrive "C:\\" "World of Warcraft"
                    , joinDrive "D:\\" "World of Warcraft" ]
