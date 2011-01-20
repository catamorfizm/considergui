import LuaParser
import Data.List
import Graphics.UI.Gtk hiding (get)
import Data.IORef
import Control.Exception
import Control.Monad
import Control.Concurrent
import System.Directory
import System.Process
import System.IO
import Data.Maybe
import qualified Data.ByteString.Char8 as C8

settingsFile = ".considerguirc"
gladeFile = "gui.glade"
maxWeight = 25
regionCodes = ["us","eu","tw","cn"]

defaultSettings = [ ("folder", "wow")
                  , ("account", "a1")
                  , ("region", "us")
                  , ("simcOptions", unlines ["iterations=10000"]) ]
accountFolder d = d++"/WTF/Account"
dbFile d a = accountFolder d ++ "/" ++ a ++ "/SavedVariables/Considerater.lua"

data GUI = GUI { guiW1 :: Window
               , guiConfigD
               , guiHelpD
               , guiStatD
               , guiSimcD :: Dialog
               , guiQuitTB
               , guiHelpTB
               , guiConfigTB :: ToolButton
               , guiAccountCB
               , guiToonCB
               , guiProfCB
               , guiRegionCB :: ComboBox
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
               , guiSimApplyB :: Button
               , guiSimSB :: Statusbar
               , guiStatScales :: [(String, HScale)]
               , guiHelpTV
               , guiSimcOutTV
               , guiSimcOptTV :: TextView
               , guiWoWFolderFC
               , guiSimCProgFC :: FileChooser
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
  [configD, statD, helpD, simcD] <- mapM (find castToDialog)
    ["configdialog", "statdialog", "helpdialog", "simcdialog"]
  [quitTB,configTB, helpTB] <- mapM (find castToToolButton)
    ["quittoolbutton", "configtoolbutton", "helptoolbutton"]
  [accountCB, toonCB, profCB, regionCB] <- mapM (find castToComboBox)
    ["accountcombobox", "tooncombobox", "profcombobox", "regioncombobox"]
  [ editProfB, helpCloseB, statOkB, statCancelB, 
    cfgOkB, cfgCancelB, simcB, simExecB, simStopB,
    simCancelB, simApplyB ]
    <- mapM (find castToButton)
    [ "editprofbutton", "helpclosebutton", "statok", "statcancel"
    , "configok", "configcancel", "simcbutton", "simexecbutton"
    , "simstopbutton", "simcancelbutton", "simapplybutton" ]
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
  return $ GUI { guiW1          = w1
               , guiConfigD     = configD
               , guiHelpD       = helpD
               , guiStatD       = statD
               , guiSimcD       = simcD
               , guiQuitTB      = quitTB
               , guiConfigTB    = configTB
               , guiHelpTB      = helpTB
               , guiAccountCB   = accountCB
               , guiToonCB      = toonCB
               , guiProfCB      = profCB
               , guiRegionCB    = regionCB
               , guiEditProfB   = editProfB
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
               }

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
  guiConfigTB gui `onToolButtonClicked` do
    folder <- getSetting s "folder"
    fileChooserSetCurrentFolder (guiWoWFolderFC gui) folder
    simc <- getSetting s "simc"
    unless (null simc) $ fileChooserSetFilename (guiSimCProgFC gui) simc >> return ()
    dialogRun (guiConfigD gui)
    return ()
  guiCfgCancelB gui `onClicked` widgetHide (guiConfigD gui)
  guiCfgOkB gui `onClicked` do
    m_folder <- fileChooserGetCurrentFolder (guiWoWFolderFC gui)
    m_simc <- fileChooserGetFilename (guiSimCProgFC gui)
    when (isJust m_folder) $ setSetting s "folder" (fromJust m_folder)
    when (isJust m_simc) $ setSetting s "simc" (fromJust m_simc)
    widgetHide (guiConfigD gui)
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

  -- edit profile buttons
  guiEditProfB gui `onClicked` showStatDialog s
  guiStatCancelB gui `onClicked` widgetHide (guiStatD gui)
  guiStatOkB gui `onClicked` do
    saveStats s
    widgetHide (guiStatD gui)
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

  -- help
  guiHelpTB gui `onToolButtonClicked` widgetShow (guiHelpD gui)
  guiHelpCloseB gui `onClicked` widgetHide (guiHelpD gui)
  helpTB <- textViewGetBuffer (guiHelpTV gui)
  textBufferSetText helpTB helpText

  refreshMainWin s

  where
    comboBoxChangeFun settingName cb = do
      m_a  <- comboBoxGetActiveText cb
      olda <- getSetting s settingName
      case m_a of
        Just a | a /= olda -> setSetting s settingName a >> refreshMainWin s
        _ -> return ()

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
    forM_ accounts $ comboBoxAppendText (guiAccountCB gui)
    account <- getSetting s "account"
    case findIndex (==account) accounts of
      Just i  -> comboBoxSetActive (guiAccountCB gui) i
      Nothing -> return ()

    whenIO (isJust `fmap` get (db s)) $ do
      Arr pkeys <- ((|-> "profileKeys") . fromJust) `fmap` get (db s)
      let toons = mapMaybe (\ (k, _) -> case k of Str s -> Just s; _ -> Nothing) pkeys
      forM_ toons $ comboBoxAppendText (guiToonCB gui)
      toon <- getSetting s "toon"
      case findIndex (==toon) toons of
        Nothing -> return ()
        Just i  -> do
          comboBoxSetActive (guiToonCB gui) i
          Str profKey <- (flip derefExpr [Str "profileKeys", Str toon] . fromJust) `fmap` get (db s)
          Arr profs <- (flip derefExpr [Str "profiles", Str profKey, Str "profiles"] . fromJust) `fmap` get (db s)
          let profNames = flip map profs $ \ (Str name, _) -> name
          forM_ profNames $ comboBoxAppendText (guiProfCB gui)
          prof <- getSetting s "prof"
          case findIndex (==prof) profNames of
            Nothing -> return ()
            Just i  -> comboBoxSetActive (guiProfCB gui) i

showStatDialog s = do
  let gui = stateGUI s
  whenIO (isJust `fmap` get (db s)) $ do
    toon <- getSetting s "toon"
    prof <- getSetting s "prof"
    when (not (null prof) && not (null prof)) $ do
      Str profKey <- (flip derefExpr [Str "profileKeys", Str toon] . fromJust) `fmap` get (db s)
      profs <- (flip derefExpr [Str "profiles", Str profKey, Str "profiles"] . fromJust) `fmap` get (db s)
      let Arr ptable = profs |-> prof
      forM_ (guiStatScales gui) $ \ (stat, scale) ->
        case lookup (Str stat) ptable of
          Just (Num x) -> rangeSetValue scale x
          Nothing      -> rangeSetValue scale 0
      widgetShow (guiStatD gui)

saveStats s = do
  let gui = stateGUI s
  whenIO (isJust `fmap` get (db s)) $ do
    toon <- getSetting s "toon"
    prof <- getSetting s "prof"
    when (not (null prof) && not (null prof)) $ do
      Str profKey <- (flip derefExpr [Str "profileKeys", Str toon] . fromJust) `fmap` get (db s)
      newTable <- forM (guiStatScales gui) $ \ (stat, scale) -> do
        x <- rangeGetValue scale
        return (Str stat, Num x)
      db s $~ \ (Just e) -> Just $ modifyExpr e [Str "profiles", Str profKey, Str "profiles", Str prof] (Arr newTable)
      saveDB s
      return ()

normScaleFactors sf = map (\ (n, w) -> (n, w*factor)) sf
  where factor = maxWeight / maximum (map snd sf)

setScaleFactors s sf =
  forM_ sf $ \ (n, w) -> do
    case lookup n simchscales of
      Nothing     -> return ()
      Just hsName -> do
        let Just hs = lookup hsName (guiStatScales (stateGUI s))
        rangeSetValue hs w

parseScaleFactors s = do
  let gui = stateGUI s
  tb  <- textViewGetBuffer (guiSimcOutTV gui)
  end <- textBufferGetEndIter tb
  res <- textIterBackwardSearch end "Scale Factors:" [] Nothing
  case res of
    Nothing     -> return Nothing
    Just (_, e) -> do
      strs <- (drop 1 . words) `fmap` textBufferGetText tb e end False
      return . Just . normScaleFactors .
        flip map strs $ \ str ->
          let (n, w) = break (=='=') str in (n, read (drop 1 w))

toonNameServer t = (head $ words t, last $ words t)

runSimC s = do
  let gui = stateGUI s
  cid     <- statusbarGetContextId (guiSimSB gui) "runsim"
  tb      <- textViewGetBuffer (guiSimcOutTV gui)
  textBufferSetText tb ""
  cursor  <- textBufferGetInsert tb
  let scrollToEnd = textViewScrollToMark (guiSimcOutTV gui) cursor 0 (Just (1, 0))
  region  <- getSetting s "region"
  toon    <- getSetting s "toon"
  let (char, serv) = toonNameServer toon
  let armory = "armory="++region++","++serv++","++char
  simc    <- getSetting s "simc"
  optTB   <- textViewGetBuffer (guiSimcOptTV gui)
  optBeg  <- textBufferGetStartIter optTB
  optEnd  <- textBufferGetEndIter optTB
  options <- textBufferGetText optTB optBeg optEnd False
  setSetting s "simcOptions" options
  widgetSetSensitive (guiSimApplyB gui) False
  statusbarPop (guiSimSB gui) cid
  statusbarPush (guiSimSB gui) cid "Running"
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
  timeoutAdd progress 100
  return ()

comboBoxTextClear cb = cellLayoutClear cb >> comboBoxSetModelText cb

whenIO condIO thenIO = condIO >>= (\ b -> if b then thenIO else return ())

Arr a |-> str = maybe (Arr []) id (lookup (Str str) a)

x $= v = writeIORef x v
x $~ f = (f `fmap` readIORef x) >>= writeIORef x
get x = readIORef x

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

helpText = "ConsiderGUI v1.0\n\nHelp\n"
