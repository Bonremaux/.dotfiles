Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = True
       , hideOnStart = False
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Weather "EGPF" ["-t","<station>: <tempC>C",
                                          "-L","18","-H","25",
                                          "--normal","green",
                                          "--high","red",
                                          "--low","lightblue"] 36000
                    , Run Network "enp4s1" ["-L","0","-H","32",
                                          "--normal","green","--high","red"] 10
                    --, Run Cpu ["-L","3","-H","50",
                    --           "--normal","green","--high","red"] 10
                    , Run MultiCpu [
                        "-t", "Cpu: <total0> <total1>",
                        "-L", "30",
                        "-H", "60",
                        "-h", "#FFB6B0",
                        "-l", "#CEFFAC",
                        "-n", "#FFFFCC",
                        "-w", "3"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% %multicpu% | %memory% * %swap% | %enp4s1% } { <fc=#ee9a00>%date%</fc>| %EGPF%"
       }
