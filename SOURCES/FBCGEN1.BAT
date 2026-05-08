rem
rem Programmgenerator fuer Programm fbc
rem
pause
copy fbcouc.for %1.for
edit %1.for
rem
rem    Schleife zum Compilieren
rem
echo šbersetze %1
\fbcq\for\bin\for1 %1,,%1;
if errorlevel 4 goto :error
echo  Pass Two
\fbcq\for\bin\pas2
if errorlevel 4 goto :error
echo Linken
\fbcq\for\bin\link c:fbc+c:fbcalgo+c:fbcalgf+c:fbccomm+c:fbcopti+c:fbcgraf+c:%1;
\fbcq\for\bin\exepack fbc.exe fbc.exe
rename altmath.lib math.lib
echo Ende Linker
goto :fertig
:error
echo Abbruch des Compilers wegen Fehlermeldungen
:fertig
