import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import System.IO

myWorkspaces = ["1:main", "2:web", "3:chat"] ++ map show [4..9]

myManageHook = composeAll
    [ className =? "Firefox" --> doShift "2:web"
    , className =? "Pidgin" --> doShift "3:chat"
    , resource  =? "desktop_window" --> doIgnore]

myLayout = avoidStruts tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 0.6

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
    xmonad $ defaultConfig {
        workspaces         = myWorkspaces,
        terminal           = "termite",
        focusFollowsMouse  = False,
        normalBorderColor  = "#000000",
        focusedBorderColor = "#dd0000",
        borderWidth        = 2,

        logHook            = dynamicLogWithPP xmobarPP
                                { ppOutput = hPutStrLn xmproc },

        manageHook         = myManageHook <+> manageDocks,
        layoutHook         = smartBorders $ myLayout
    } `additionalKeys`
        [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
