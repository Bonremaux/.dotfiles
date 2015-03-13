import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import System.IO
import XMonad.Hooks.ManageHelpers

myWorkspaces = ["1:term", "2:web", "3:code", "4:chat"] ++ map show [5..9]

myManageHook = composeAll
    [ className =? "Pidgin" --> doShift "4:chat"
    , resource  =? "desktop_window" --> doIgnore
    , isFullscreen --> doFullFloat]

myLayout = smartBorders (avoidStruts tiled ||| Mirror tiled ||| Full)
           where tiled = Tall 1 (3/100) (5/9)

modm = mod4Mask

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
    xmonad $ defaultConfig {
        workspaces         = myWorkspaces,
        terminal           = "urxvt",
        focusFollowsMouse  = True,
        clickJustFocuses   = False,
        normalBorderColor  = "#000000",
        focusedBorderColor = "#ee9a00",
        modMask            = modm,

        logHook            = dynamicLogWithPP xmobarPP
                                { ppOutput = hPutStrLn xmproc },

        manageHook         = myManageHook <+> manageDocks,
        layoutHook         = myLayout
    } `additionalKeys`
        [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")

        , ((0, xK_Print), spawn "scrot")
        ]
