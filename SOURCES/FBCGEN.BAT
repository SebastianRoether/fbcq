rem
rem Programmgenerator fuer Programm fbc
rem
echo Bitte in Laufwerk a: formatierte Diskette einlegen
pause
copy fbcouc.for %1.for
edi2 %1.for
rem
rem    Schleife zum Compilieren
rem
echo Kopiere Datei %.for auf die Ramdisk
copy %1.for b:
b:
echo šbersetze %1
for1 %1,,%1;
if errorlevel 4 goto :error
echo  Pass Two
pas2
if errorlevel 4 goto :error
del b:%1.for
copy %1.obj c:
del %1.obj
del %1.lst
echo Linken
link c:fbc+c:fbcalgo+c:fbcalgf+c:fbccomm+c:fbcopti+c:fbcgraf+c:%1;
exepack fbc.exe a:fbc.exe
del b:*.exe
copy c:\msfor\8087.lib b:
del b:math.lib
rename 8087.lib math.lib
link c:fbc+c:fbcalgo+c:fbcalgf+c:fbccomm+c:fbcopti+c:fbcgraf+c:%1;
copy c:\msfor\altmath.lib b:
del b:math.lib
rename altmath.lib math.lib
exepack fbc.exe a:fbcf.exe
del b:*.exe
echo Ende Linker
goto :fertig
:ende
goto :fertig
:error
echo Abbruch des Compilers wegen Fehlermeldungen
:fertig
c:
del %1.obj
copy c:*.fbc a:
echo Ende der Generierung fuer Kundencode %1
