$DEBUG
$STORAGE:2
      program fbc
c
c     Hauptprogramm zur Reglerparameteroptimierung
c
c     Zweck: Verwaltungsprogramm fuer ein IBM-PC-gestuetztes
c     Reglerentwurfsverfahren.
c
c     Nach Eingabe von Wertetripeln gemessener Sprungantworten gene-
c     riert das Programm die Kurve durch numerische Interpolation neu.
c     Mithilfe eines Identifikationsverfahrens wird anschliessend ein
c     mathematisches Prozessmodell auf der Basis der z-Uebertragungs-
c     funktion erzeugt.
c     Der Anwender hat nun die Wahl, entweder einen eigenen Regler in
c     Simulation zu erproben oder einen neuen Regler zu entwerfen. Die-
c     ser nach dem bekannten quadratischen Integralkriterium (Gewichtung
c     auch der Stellenergie) optimale Standardregler kann in Simulation
c     ueberprueft und dann auf den Proze· uebertragen werden.
c
c     Es werden unterstuetzt:
c       - Regelstrecken mit proportionalem und integrierendem Verhalten,
c       - Entwurf von Reglern ohne Oeffnen des Regelkreises,
c       - Optimierung einzelner Regelparameter separat,
c       - Totzeitverhalten bis zu 20% des Zeitausschnittes,
c       - Nichtminimales Phasenverhalten (Allpass) hoehrer Ordnung,
c       - Eine separat speicherbare (und lesbare) Datenbasis,
c       - Semigrafische Simulationen,
c       - Druckerausgabe aller Bildschirminhalte via Betriebssystem.
c
c
c     Beschreibung der wesentlichen Variablen:
c
c     lth    - Maximalanzahl simultan identifizierbarer Proz.-parameter
c     nki    - Maximalanzahl einlesbarer Wertetripel
c     lmax   - Maximale Koeffizientenanzahl der diskreten Prozess-
c              Uebertragungsfunktion (entspricht der Ordnung lmax-1)
c     idmax  - Maximale Prozesstotzeit
c     nkm    - Maximalanzahl der interpolierten Stuetzstellen
c     nregma - Maximalanzahl der Regelparameter
c
c     sprung - Vektor mit Sprungerregungs-Messwerten
c     sprant - Vektor mit Sprungantwort-Messwerten
c     time   - Vektor mit Relativzeitwerten
c     uk     - Vektor mit interpolierten Sprungerregungs-Sequenzen
c     yk     - Vektor mit interpolierter Sprungantwort
c     ur     - Vektor mit simulierten Regler-Stellsequenzen
c     yr     - Vektor mit simulierten Prozessantwortsequenzen
c
c     phi    - Messdatenvektor fuer Identifikation
c     co     - Vektor mit Cosinus-Rotationswerten (Identifikation)
c     so     - Vektor mit Sinus-Rotationswerten (Identifikation)
c     rh     - Informationsmatrix der Identifikation
c     theta  - Vektor mit Identifikationsparametern
c
c     x0     - Vektor mit zu optimierenden Regelparametern
c     d      - Vektor mit Optimierungsparametern
c     dopt   - Vektor mit Optimierungs-Startparametern
c     hs     - Vektor mit Start-Stuetzstellenweiten fuer Optimierung
c     regpar - Vektor mit zeitdiskreten Regelparametern
c
c     ci     - Character 'J' indiziert integrierende Regelstrecke
c     cs     - Character 'J' indiziert sprungfoermige Anregung
c     cd     - Character 'J' indiziert sprungfaehige Prozessantwort
c
c     Aufgerufene Unterprogramme:
c     adclr  - Loescht den IBM-PC-Bildschirm
c     cbnput - Eingabe von Bezeichnungen
c     cinput - Eingabe von  physikalischen Wertzuordnungen
c     cisdin - Abfrage nach Ja/Nein-Antworten
c     rinput - Eingabe von Real-Zahlen
c     xyinpu - Editieren von gemessenen Wertetripeln
c     minmax - Berechnung von Zeichnungsparametern
c     xintpl - Interpolation eingelesener Daten
c     xkurve - Semigrafische ausgabe von Vektoren
c     xlsqr  - Least-Squares-Identifikation
c     xsimul - Prozess-Simulation
c     pause  - Pause mit blinkender Anzeige
c     rtwait - Beendet Bildschirmausgabe und wartet
c     xrkonv - Skalierung von Optimierungsparametern
c     rminkg - Konjugierte-Gradienten-Minimierung
c     itaekr - Berechnung des quadratischen Regelflaechenkriteriums
c     rkodis - Berechnung zeitdiskreter aus PID-Regelparametern
c     diskio - Ausgabe und Einlesen Speichermedium
c     fbcint - Bildschirmausgabe einer Kurzanleitung
c     fbcouc - Passwort-Initialisierung
c
c     Autor: Friedemann Roether
c     Erste Version: 01.02.1986
c     Letzte Aenderung: 21.06.1987
c     Versionsstand: 1.10
c
c
      parameter(lth=11,nki=20,lmax=6,idmax=12,nkm=61,nregma=4)
      implicit real*8(a-h,o-z)
      dimension sprung(nki),sprant(nki),time(nki)
      dimension ur(nkm),yr(nkm),yk(nkm),uk(nkm)
      dimension phi(lth),co(lth),so(lth),theta(lth)
      dimension x0(121),d(7),hs(nregma),regpar(nregma),dopt(6)
      character*1 ja,ci,cs,cd
      character*30 cbez(3),cb1,cuser
      character*46 csta
      character*4 ctyu(3),cdum
      integer name(10)
      logical isema,icusto
      common/itaek/ru,vk,n1,id,nrop,dt,irk(nregma),regkon(nregma),
     *a(lmax),b(lmax),qr(nregma),pr(nregma),se,su,ci
      external itaekr
c
c     Vorkonfigurierte Datenbasis
c
      data len,npoly,n,nk,kmax,nou/7,3,3,61,10,1/
      data csta/'Instabiler Proze·. Bitte Eingabedaten prÅfen.!'/
      data cbez/'Tiefpa·-Proze· mit Totzeit    ',
     *'Proze·-Ausgang                ','Proze·-Eingang                '/
      data ctyu/'s   ','V   ','V   '/
      data dopt/1.d-7,1.d-3,1.d-3,1.d-2,.2d00,.05d00/
      data uanf,uend,yanf,yend/5.d00,6.d00,10.d00,11.d00/
      data time/0.d00,4.d00,14.d00,24.d00,39.d00,49.d00,60.d00,13*0.d00/
      data sprung/20*6.d00/
      data sprant/10.d0,10.d0,10.293,10.65,10.9,10.958,10.982,13*11.d00/
      data theta/-2.48824,2.05387,-.56203,0.,4.62d-3,1.69d-3,-2.73d-3,
     *4*0.d0/
      data regpar/1.22650,22.93540,0.d00,0.d00/
      data name/98,121,32,82,111,101,116,104,101,114/
      data cs,cd/'J','N'/
      data job,icnt/0,0/
      data isema,icusto/.false.,.false./
      ci='N'
      id=4
      ru=0.2d00
c
c     Definition des Anwender- und User-Codes
c
      call fbcouc(iusval,isycod,icnmin,icnmax)
      icusto=.false.
      go to 200
c
c     Anzeige des Hauptmenues
c
 100  continue
      call adclr
      write(*,105)
 105  format(//
     *15x,'        FeedBackController - RechnergestÅtzter         '/
     *15x,'   Entwurf von P/PI/PID/PD-T1/PID-T1 -  Proze·reglern  '//
     *15x,'        Version 1.10   Copyright F. Roether 1987       '///
     *15x,'   Manuelle Eingabe von Proze·daten_________________1  '/
     *15x,'   Simuliere Anregung/Antwort ungeregelter Proze·___2  '/
     *15x,'   Eingabe/Modifikation von Regelparametern_________3  '/
     *15x,'   Simuliere Stell/Regelgrî·e geregelter Proze·_____4  '/
     *15x,'   Parameteroptimierung (quadratische GÅtefunktion)_5  '/
     *15x,'   Sichten der aktuellen Proze·- und Regelparameter_6  '/
     *15x,'   Anzeigen, Drucken und Speichern der Datenbasis___7  '/
     *15x,'   Einlesen der Datenbasis von Diskette/Festplatte__8  '/
     *15x,'   Kurzanleitung FeedBackController_________________9  '/
     *15x,'   Programmende____________________________________10  '/)
 110  continue
      icnt=icnt+1
      write(*,120)
 120  format(
     *15x,'   Bitte gewÅnschte Zahl eingeben:'\)
      read(*,'(i6)',err=3000) job
      if(job.le.0.or.job.gt.10) go to 3000
      if(.not.isema.and.job.ge.2.and.job.le.7) then
         call pause('Bitte mit Funktion (1) oder (8) Daten einlesen.!')
         go to 110
      end if
      go to (150,400,600,800,1000,1200,1400,1600,1800,2000),job
c
c     Handeingabe wichtiger Prozesscharakteristika
c
 150  continue
      call adclr
      call cbnput(' Bez. Regelkreis ',cbez(1))
      call cbnput(' Bez. Antwortfkt.',cbez(2))
      call cbnput(' Bez. Anregung   ',cbez(3))
      call cinput(' Zeitachsen-Skalierung in Einheit  ',ctyu(1))
      call cinput(' Physikal. Einheit Prozessantwort  ',ctyu(2))
      call cinput(' Physikal. Einheit Prozessanregung ',ctyu(3))
      call cisdin('Besitzt Prozess integrierendes Verhalten  (J/N)',ci)
      call cisdin('Sprunganregung direkt auf Streckeneingang (J/N)',cs)
      call cisdin('Ist auch die  Sprungantwort  sprungfÑhig  (J/N)',cd)
      if(cd.eq.'J') then
         write(*,'(a55)')
     *   ' ** Noch keine UnterstÅtzung sprungfÑhiger Prozesse ** '
         cd='N'
      end if
      call rinput('Anfangswert Proze·antwort  (in',ctyu(2),yanf,1)
      call rinput('Anfangswert Proze·anregung (in',ctyu(3),uanf,1)
c
c     Eingabe der Prozess-Messwertetripel
c
      call xyinpu(time,sprant,sprung,len,nki,ctyu)
      if(len.lt.4) then
         call pause('Fehlermeldung: Zuwenig StÅtzstellen!')
         go to 100
      end if
      if(.not.icusto) then
         write(*,'(a61)')
     *   ' ** Im Demomodus liegt Wertetripel 4 mit  XX,10.65,6 fest. **'
         sprung(4)=6.d00
         sprant(4)=10.65d00
      end if
      if(ci.eq.'J'.and.cs.eq.'J') then
         yend=sprant(len)
      else
         call rinput('Endwert Proze·antwort      (in',ctyu(2),yend,1)
      end if
      call rinput('Endwert Proze·anregung     (in',ctyu(3),uend,1)
c
c     Ende der Eingabe. Beginn der Datenbearbeitung.
c
 200  continue
c
c     Berechne Begrenzungen fuer Semigrafik
c
      call minmax(yu,wy,yo,uu,wu,uo,yanf,yend,uanf,uend,
     *sprant,sprung,len,ci,cs,if)
      if(if.ne.0) go to 100
c
c     Interpoliere Sprunganregung mit kubischen Splines, es sei
c     denn, es handelt sich um eine sprungfoermige Groesse
c
      npoly=3
      if(cs.eq.'J') npoly=0
      call xintpl(dt,uk,nk,time,sprung,len,npoly,nou)
c
c     Interpoliere Sprungantwort mit kubischen Splines
c
      npoly=3
      call xintpl(dt,yk,nk,time,sprant,len,npoly,nou)
c
c     Zeichne Kurve
c
      if(job.eq.1)
     *call xkurve(yk,uk,nk,time,sprant,sprung,len,yu,wy,yo,uu,wu,uo,
     *yanf,uanf,cbez,ctyu,0)
c
c     Berechnung des Gleichwerts und der Proze·-Verstaerkung
c
      if(uend.eq.uanf.or.(cs.eq.'N'.and.ci.eq.'J')) then
         pver=yend-yanf
      else
         pver=(yend-yanf)/(uend-uanf)
      end if
      if(ci.ne.'J') then
         pglw=yend-pver*uend
      else
         pglw=yanf
      end if
c
c     Bestimmung der Modellparameter
c
      n1=n+1
      if(job.eq.1) then
         call adclr
         write(*,'(/17x,a37)') 'Proze·identifikation. Bitte warten...'
c
c        Bestimmung der Totzeit
c
         id=0
         do 230 i=1,nk
            if(uk(i).ne.uanf) then
               if(yk(i).eq.yanf) then
                  id=id+1
               else
                  go to 235
               end if
            end if
 230     continue
 235     continue
         id=min0(id-1,idmax)
         id=max0(id,0)
c
c        Identifikation
c
         info=0
         call xlsqr(theta,n,yk,uk,nk,yanf,uanf,pglw,pver,id,x0,lth,phi,
     *   ci,rho,co,so,info)
      end if
      if(job.le.1) then
         do 240 i=2,n1
            a(i)=theta(i-1)
            b(i)=theta(n+i)
 240     continue
         b(1)=theta(n+1)
         a(1)=1.d00
      end if
c
c     Berechnung der Verstaerkung des identifizierten Prozesses
c
      suma=0.d00
      sumb=0.d00
      sumc=0.d00
      do 250 i=1,n1
         suma=suma+a(i)
         sumb=sumb+b(i)
         sumc=sumc+suma
 250  continue
      if(ci.eq.'N') then
         vk=sumb/suma
         pgl=yend-vk*uend
      else
         vk=sumb/(dt*sumc)
         pgl=vk*uanf
      end if
      if(vk.eq.0.d00) then
         call pause('Fehlermeldung:Streckentyp nicht zulÑssig.!')
         go to 100
      end if
c
c     Setze Indikator: Datenbasis fertig berechnet
c
      isema=.true.
      nrop=0
c
c     Nimm Regelparameter-Denormierung vor
c
      call xrkonv(regpar,regkon,vk,dt,ierr,1)
      go to 100
c
c     Simuliere und zeichne gemessenen Prozess
c
 400  continue
      call xsimul(yr,uk,uk,nk,uanf,uend,
     *a,b,pver,pglw,n,pr,qr,npr,nqr,id,ci,cs,0,nou,ifail)
      if(ifail.ne.0) then
         call pause(csta)
         go to 110
      end if
      call xkurve(yr,uk,nk,time,sprant,sprung,len,yu,wy,yo,uu,wu,uo,
     *yanf,uanf,cbez,ctyu,0)
      go to 100
c
c     Eingabe der Regler-Initialisierungen
c
 600  continue
      call adclr
c
c     Denormiere interne Regelparameter zwecks Aenderung von Hand
c
      call xrkonv(regpar,regkon,vk,dt,ierr,0)
 605  continue
      write(*,610)
 610  format(' Laplacedarstellung des Reglers (Siehe Kurzanleitung):'//
     *       '   y(s) = KP * (1+1/(TN*s)+TV*s/(1+T1*s)) * (w(s)-x(s))'//
     *       ' Bitte Startparameter KP,TN,TV,T1 fÅr Regler eingeben:'/)
      cb1(1:25)='(1) Prop.-Beiwert KP (in '
      cb1(26:29)=ctyu(2)
      cb1(30:30)='/'
      call rinput(cb1,ctyu(3),regpar(1),1)
      if(regpar(1).eq.0.d00) then
         write(*,'(a40)') ' Bitte KP verschieden von Null wÑhlen.  '
         go to 605
      end if
 620  continue
      call rinput('(2) Nachstellzeit       TN (in',ctyu(1),regpar(2),0)
      if(regpar(2).eq.0.d00.and.ci.ne.'J') then
         write(*,'(a40)') ' Bei diesem Streckentyp mu· TN > 0 sein.'
         go to 620
      end if
      call rinput('(3) Vorhaltezeit        TV (in',ctyu(1),regpar(3),0)
 630  continue
      call rinput('(4) Vorhalt-Verzîgerung T1 (in',ctyu(1),regpar(4),0)
      if(regpar(3).eq.0.d00) regpar(4)=0.d0
      if(regpar(4).lt.dt.and.regpar(4).ne.0.d00) then
         write(*,'(a5,,f10.4,a20)') ' T1 >',dt,' oder T1 = 0 wÑhlen.'
         go to 630
      end if
 660  continue
      write(*,670)
 670  format(/' Welche der vier Parameter (1,2,3 oder 4) sollen zur'/
     *' Optimierung markiert werden (Zahlen durch Kommas trennen)?:'\)
      read(*,'(4i2)',err=660) (irk(i),i=1,nregma)
c
c     Kennzeichne zu optimierende Regelparameter
c
      index=0
      nrop=nregma
      do 675 i=1,nregma
         if(irk(i).le.0.or.irk(i).gt.nregma) then
            nrop=nrop-1
         else
            index=index+1
            irk(index)=irk(i)
         end if
 675  continue
c
c     Teste, ob Startwerte zulaessig
c
      call xrkonv(regpar,regkon,vk,dt,ierr,1)
      if(nrop.eq.0) go to 100
      do 702 i=1,nrop
         x0(i)=regkon(irk(i))
 702  continue
      call itaekr(x0,f0,li,iz)
      if(li.ne.0) then
c
c     Startwerte nicht zulaessig
c
 705     write(*,710)
 710     format(/
     *' Der aktuelle Regler stabilisiert den Proze· nicht.'/
     *' Sollen automatisch Startwerte bestimmt werden (J/N)?'\)
         read(*,'(a1)',err=705) ja
         if(ja.eq.'j'.or.ja.eq.'J') then
c
c    Suche nach stabilisierenden Startwerten
c
            if(regpar(3).gt.0.d00) regpar(3)=dt
            if(regpar(4).gt.0.d00) regpar(4)=2.*dt
            regpar(1)=1.d00/vk
      if(regpar(2).gt.0.d0.or.ci.eq.'N')regpar(2)=(time(len)-time(1))/2.
            izs=0
 720        continue
            call xrkonv(regpar,regkon,vk,dt,ierr,1)
            do 730 i=1,nrop
               x0(i)=regkon(irk(i))
 730        continue
            call itaekr(x0,f0,li,iz)
            if(li.eq.1) then
               if(izs.gt.20) then
                  call pause('Keine stabilisierenden Werte gefunden!')
                  go to 100
               end if
               regpar(1)=regpar(1)/2.d00
               regpar(2)=regpar(2)*2.d00
               izs=izs+1
               go to 720
            end if
         end if
      end if
      go to 100
c
c     Simuliere und zeichne geschlossenen Regelkreis
c
 800  continue
      call rkodis(pr,qr,npr,nqr,regkon(1)/vk,regkon(2),regkon(3),
     *regkon(4))
      call xsimul(yr,ur,uk,nk,uanf,uend,
     *a,b,pver,pglw,n,pr,qr,npr,nqr,id,ci,cs,1,nou,ifail)
      if(ifail.ne.0) then
         call pause(csta)
         go to 110
      end if
      call xkurve(yr,ur,nk,time,sprant,sprung,len,yu,wy,yo,uu,wu,uo,
     *yanf,uanf,cbez,ctyu,1)
      go to 100
c
c     Fuehre Regelparameteroptimierung durch
c
 1000 continue
      if(regpar(1).eq.0.d00) then
          call pause('Regler ohne Proportionalbeiwert nicht zulÑssig!')
          go to 110
      end if
c
c     Konsistenzpruefung: Verringere Satz von Optimierungsparametern,
c     wenn noetig
c
      i=1
 1005 continue
      if(regkon(irk(i)).eq.0.d00) then
         i1=i+1
         if(nrop.ge.i1) then
            do 1010 j=i1,nrop
               irk(j-1)=irk(j)
 1010       continue
         end if
         nrop=nrop-1
      else
         i=i+1
      end if
      if(i.le.nrop) go to 1005
      if(nrop.le.0) then
         call pause('Bitte mit (3) Optimierungsparameter markieren.!')
         go to 110
      end if
 1020 continue
c
c     Eingabe Gewichtungsfaktor r
c
      call adclr
      write(*,1025)
 1025 format(
     *' Wie soll im quadratischen GÅtekriterium (Proze·verst. KS)'//
     *3x,'     ÏÙ'/
     *3x,' J =  ≥ (1-r) (w(Ï)-x(t))˝  +  r KS˝ (y(Ï)-y(t))˝  dt'/
     *3x,'     ¯ı'//
     *' die Stellgrî·engewichtung r bemessen werden?')
      cdum='1   '
      call rinput('Stellgewichtung (Min: 0   Max:',cdum,ru,0)
      ru=dmin1(1.d00,ru)
      write(*,'()')
c
c     Besetze Startwerte fuer Optimierung
c
      do 1028 i=1,4
         d(i)=dopt(i)
 1028 continue
      d(6)=dopt(5)
      d(7)=dopt(6)
      do 1030 i=1,nrop
         x0(i)=regkon(irk(i))
         hs(i)=dmax1(dabs(d(3)*x0(i)),d(3))
 1030 continue
c
c     Minimierung nach dem Konjugierten-Gradienten-Verfahren
c
      call rminkg(itaekr,nrop,d,hs,kmax,x0,f0,iz,ifail,nou)
c
c     Pruefe und uebernehme Ergebnis
c
      if(ifail.eq.1) then
         call pause(csta)
         go to 100
      end if
      if(ifail.eq.0)
     *write(*,1035) 'Die Parameteroptimierung ist abgeschlossen.     '
      if(ifail.eq.2)
     *write(*,1035) 'Evtl. Verbesserungen durch Wiederholung mîglich.'
      if(ifail.eq.3)
     *write(*,1035) 'Keine merkliche  Verbesserung in Tastrichtungen.'
 1035 format(/1x,a48)
      ifail=0
      do 1040 i=1,nrop
         irki=irk(i)
         if(irki.gt.1.and.((x0(i).lt.0.d00.or.x0(i).gt.600.)
     *   .or.(irki.eq.4.and.x0(i).lt.0.8))) then
            ifail=1
            regkon(irki)=0.d00
         else
            regkon(irki)=x0(i)
         end if
 1040 continue
      if(ifail.eq.1) then
         write(*,1050)
 1050    format(
     *   ' Die Regelstruktur ist nicht sinnvoll und wurde modifiziert.'/
     *   ' Bitte Startwerte ÅberprÅfen (3) und erneut optimieren (5).')
         if(regkon(3).eq.0.d00) regkon(4)=0.d00
      end if
      call xrkonv(regpar,regkon,vk,dt,ierr,0)
      call rtwait
      go to 100
c
c     Bildschirmausgabe von Regelparametern
c
 1200 continue
      call adclr
      write(*,1205) cbez(1)
 1205 format(' Regelkreis: ',a30//)
      totzei=id*dt
      write(*,1207) totzei,ctyu(1)
 1207 format(
     *' Streckentotzeit     TT        : ',f10.4,' ',a4)
      if(ci.ne.'J') then
      write(*,1210) vk,ctyu(2),ctyu(3),pgl,ctyu(2)
 1210 format(
     *' StreckenverstÑrkung KS        : ',f10.4,' ',a4,' / ',a4/
     *' Gleichanteil x0 der Regelgrî·e: ',f10.4,' ',a4/)
      else
      write(*,1212) vk,ctyu(2),ctyu(3),ctyu(1),pgl,ctyu(2),ctyu(1)
 1212 format(
     *' Integrationsbeiwert Regelgrî·e: ',f10.4,' ',a4,' / ',a4,'*',a4/
     *' Kompensierter Gleichanteil    : ',f10.4,' ',a4,' / ',a4/)
      end if
      write(*,1215) regpar(1),ctyu(3),ctyu(2)
 1215 format(' Ausgabe der aktuellen Reglerkennwerte:'//
     *' Proportionalbeiwert KP        : ',f10.4,' ',a4,' / ',a4)
      if(regpar(2).gt.0.d00) write(*,1220) regpar(2),ctyu(1)
 1220 format(
     *' Nachstellzeit       TN        : ',f10.4,' ',a4)
      write(*,1230) regpar(3),ctyu(1),regpar(4),ctyu(1)
 1230 format(
     *' Vorhaltezeit        TV        : ',f10.4,' ',a4/
     *' Vorhalt-Verzîgerung T1        : ',f10.4,' ',a4//)
      write(*,'(a33,f10.4)') ' Gewichtung Stellaufwand       : ',ru
      call rtwait
      go to 100
c
c     Schreibe Daten auf Speichermedium
c
 1400 continue
      call diskio(cbez,ctyu,ci,cs,cd,yanf,uanf,len,time,sprant,sprung,
     *yend,uend,pgl,vk,nk,n,id,a,b,rho,regpar,ru,dopt,kmax,nou,1,if)
      go to 100
c
c     Lese Daten von Speichermedium
c
 1600 continue
      if(.not.icusto) then
         call pause('Lesen in Demomodus nicht mîglich!')
         go to 110
      else
      call diskio(cbez,ctyu,ci,cs,cd,yanf,uanf,len,time,sprant,sprung,
     *yend,uend,pgl,vk,nk,n,id,a,b,rho,regpar,ru,dopt,kmax,nou,0,if)
c
c     Konsistenzpruefung
c
         len=min0(len,nki)
         len=max0(len,0)
         nk=min0(nk,nkm)
         nk=max0(nk,11)
         id=min0(id,idmax)
         id=max0(id,0)
         n=min0(n,lmax-1)
         n=max0(n,1)
         if(if.eq.0) go to 200
         go to 100
      end if
c
c     Bildschirmausgabe einer Kurzeinfuehrung
c
 1800 continue
      call fbcint
      go to 100
c
c     Programmabbruch
c
 2000 continue
      write(*,2010)
 2010 format(17x,' Programmabbruch bedeutet Verlust der Daten, falls'/
     *17x,' nicht vorher mit Funktion (7) abgespeichert wurde.'//
     *17x,' Wirklich abbrechen (J/N)?'\)
      read(*,'(a1)',err=2000) ja
      if(ja.ne.'j'.and.ja.ne.'J') go to 100
      go to 9999
c
c     Benutzervalidierung
c
 3000 continue
      call lclear
      if((icnt.ge.icnmin.and.icnt.le.icnmax).and.
     *(job.eq.iusval.or.job.eq.isycod)) then
         cuser(1:30)='FeedBackController  1.10 1987!'
         if(job.eq.isycod) then
            do 3010 i=1,10
               cuser(19+i:19+i)=char(name(i))
 3010       continue
            write(*,'(17x,a5,i6)') ' UIC:',iusval
         else
            write(*,'(17x,a13)') ' Validierung.'
         end if
         call pause(cuser)
         icusto=.true.
         len=0
         isema=.false.
      end if
      go to 110
 9999 continue
      call adclr
      stop
      end
