$DEBUG
$STORAGE:2
      subroutine fbcouc(iuser,iowner,icnmin,icnmax)
c
c     iuser - Anwender-Code
c     iowner- Wartungs-Code
c     icnmin- Anzahl MenÅaufrufe vor Codeeingabe
c     icnmax- Maximale Anzahl von MenÅaufrufen bis Codeingabe
c
      iowner=1000-30000-305
      icnmin=1
      icnmax=2
      iuser=9100+451
      return
      end
