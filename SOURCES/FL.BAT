echo off
rem
rem    Schleife zum Compilieren
rem
:beginn
if %1 ==/ goto :laden
if not exist %1.for goto :ende
echo Kopiere Datei %1.for auf die Ramdisk
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
copy %1.lst c:
del %1.lst
shift
c:
goto :beginn
:laden
shift
if not exist %1list.lnk goto :ende
b:
echo Linken aus File %1list.lnk
link < c:%1list.lnk
copy *.exe c:
del b:*.exe
echo Ende Linker
goto :fertig
:ende
echo Kein weiteres File %1 vorhanden
goto :fertig
:error
echo Abbruch des Compilers wegen Fehlermeldungen
:fertig
c:
echo on
