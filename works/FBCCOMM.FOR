$DEBUG
$STORAGE:2
c     Unterprogrammsammlung FBCCOMM.FOR
c
c     Kommunikationsprogramme fuer Programm FBC (FeedBackController)
c     Roether,1987
c
c
c
      subroutine diskio(cbez,ctyu,ci,cs,cd,yanf,uanf,len,t,y,u,
     *yend,uend,pglw,pver,nk,n,id,a,b,rho,q,r,dopt,kmax,nou,job,if)
c
c     Zweck: Transfer der Datenbank zwischen Speichermedium und
c     dem Programm fbc (Reglerparameteroptimierung)
c
c     Eingabeparameter:
c     job   - 0:Lese Datenfile, 1:Schreibe Datenfile
c     nou   - Ausgabeeinheit fuer Systemmeldungen
c     kmax  - Maximalanzahl von Minimierungsschritten (Optimierung)
c     dopt  - Optimierungsparametervektor (Optimierung)
c     r     - Gewichtungsfaktor des ITAE-Kriteriums
c     q     - Vektor mit Regelparametern
c     rho   - Identifikations-Fehler
c     b     - Koeffizientenvektor fuer Zaehlerpolynom Strecke
c     a     - Koeffizientenvektor fuer Nennerpolynom Strecke
c     id    - Totzeit Strecke
c     n     - Systemordnung
c     nk    - Anzahl der aequidistanten interpolierten Kurvenwerte
c     pver  - Prozessverstaerkung
c     pglw  - Prozessgleichwert y0
c     uend  - Endwert der Stellerregung
c     yend  - Endwert der Systemantwort
c     u     - Vektor mit gemessenen Stellwerten
c     y     - Vektor mit gemessenen Prozesswerten
c     t     - Vektor mit abgelesenen Relativzeiten
c     len   - Anzahl der eingelesenen Stuetzstellen
c     uanf  - Anfangswert der Stellerregung
c     yanf  - Anfangswert der Systemantwort
c     cd    - Character-Wert (Indiziert sprungfaehige Strecke)
c     cs    - Character-Wert (Indiziert sprungfoermige Erregung)
c     ci    - Character-Wert (Indiziert integrierendes Verhalten)
c     ctyu  - Character-Vektor mit physikalisBhen Bezeichnungen
c     cbez  - Vektor mit Bezeichnungen
c
c     Autor:Friedemann Roether
c     Letzte Aenderung: 19.05.1987
c
      implicit real*8 (a-h,o-z)
      dimension t(len),u(len),y(len),a(*),b(*),q(4),dopt(6)
      character*20 cdum,name
      character*4 ctyu(3),cdu4
      character*1 ci,cs,cd
      character*2 blank,bldum
      logical ex
      character*30 cbez(3)
      character*40 kopf,kopfz
      character*16 cdu16
      data kopfz/' FBC (FeedBackController) , Roether 1987'/
      data blank/'  '/
      data nds/8/
      if=0
      n1=n+1
      dt=(t(len)-t(1))/(nk-1)
c
c     File-Definition
c
 100  continue
      name='                    '
      write(*,110)
 110  format(/17x,' Name des Datenfiles :'\)
      read(*,'(a20)',err=100) name
c
c     Ueberpruefe und ergaenze eingegebenen Namen
c
      do 115 i=1,16
         if(name(i:i).eq.' '.or.name(i:i).eq.'.') then
            nfbc=i
            go to 120
         end if
 115  continue
      nfbc=17
 120  continue
      if(nfbc.le.1) go to 1000
      if(name(1:3).eq.'con'.or.name(1:3).eq.'prn'
     *.or.name(1:3).eq.'CON'.or.name(1:3).eq.'PRN'.and.
     *(name(4:4).eq.' '.or.name(4:4).eq.':')) then
         if(job.ne.1) go to 100
         name(4:20)=':                '
         ex=.false.
         go to 125
      end if
      name(nfbc:nfbc+3)='.fbc'
      nend=nfbc+4
      if(nend.le.20) then
         do 123 i=nend,20
            name(i:i)=' '
 123     continue
      end if
      inquire(file=name,exist=ex)
 125  continue
      if(.not.ex) then
         if(name(4:4).ne.':') write(*,130) name
 130     format(17x,' Bisher kein File: ',a20,' vorhanden.')
         if(job.eq.0) go to 100
         open(nds,file=name,status='new',form='formatted')
      else
         open(nds,file=name,status='old',form='formatted')
      end if
      if(name(4:4).ne.':') rewind nds
      if(ex) then
         read(nds,'(a40)',err=999) kopf
         if(kopf.ne.kopfz) then
            write(*,140)
 140        format(17x,' Kein FBC-Datenfile!')
            close(nds)
            go to 100
         end if
      else
         write(nds,'(a40)') kopfz
      end if
c
c     Beginne mit Datentransfer
c     1. Prozesscharakteristika und Messdaten
c
      if(job.eq.0) then
         read (nds,'(a2)',err=999) bldum
         read (nds,'(a20,a30)',err=999) cdum,cbez(1)
         read (nds,'(a20,a30)',err=999) cdum,cbez(2)
         read (nds,'(a20,a30)',err=999) cdum,cbez(3)
         read (nds,'(a20,3a4)',err=999) cdum,(ctyu(i),i=1,3)
         read (nds,'(a20,a1 )',err=999) cdum,ci
         read (nds,'(a20,a1 )',err=999) cdum,cs
         read (nds,'(a20,a1 )',err=999) cdum,cd
         read (nds,'(a20,i5)',err=999) cdum,len
         read (nds,'(a2)',err=999) bldum
         read (nds,'(a40)',err=999) kopf
         read (nds,'(a18,2(g14.6,a4))',err=999) cdum,yanf,cdu4,uanf,cdu4
      else
         write(nds,'(a2)') blank
         write(nds,'(a20,a30)') ' Prozessbezeichnung:',cbez(1)
         write(nds,'(a20,a30)') ' Bezeichn. Antwort :',cbez(2)
         write(nds,'(a20,a30)') ' Bezeichn. Anregung:',cbez(3)
         write(nds,'(a20,3a4)') ' Physikal.Einheiten:',(ctyu(i),i=1,3)
         write(nds,'(a20,a1 )') ' Integralanteil    ?',ci
         write(nds,'(a20,a1 )') ' Sprunganregung    ?',cs
         write(nds,'(a20,a1 )') ' Proze· sprungfÑhig?',cd
         write(nds,'(a20,i5)')  ' Anzahl Wertetripel:',len
         write(nds,'(a2)') blank
         write(nds,'(3a18)')'   Zeitmarke      ','   Prozessantwort ',
     *   '   Prozessanregung'
         write(nds,'(a18,2(g14.6,a4))') '   Anfangswerte  :',
     *   yanf,ctyu(2),uanf,ctyu(3)
      end if
      do 200 i=1,len
         if(job.eq.0) then
            read(nds,'(3(g14.6,a4))',err=999) t(i),cdu4,y(i),cdu4,u(i),
     *      cdu4
         else
            write(nds,'(3(g14.6,a4))') t(i),ctyu(1),y(i),ctyu(2),u(i),
     *   ctyu(3)
         end if
 200  continue
      if(job.eq.0) then
         read(nds,'(a18,2(g14.6,a4))',err=999) cdum,yend,cdu4,uend,cdu4
         read(nds,'(a2)',err=999) bldum
         read(nds,'(a20,g14.6)', err=999) cdum,pglw
         read(nds,'(a20,g14.6)', err=999) cdum,pver
      else
         write(nds,'(a18,2(g14.6,a4))') '   Endwerte      :',
     *   yend,ctyu(2),uend,ctyu(3)
         write(nds,'(a2)') blank
         if(ci.ne.'J') then
            write(nds,'(a20,g14.6,a1,a4)')
     *    ' Gleichwertanteil  :',pglw,' ',ctyu(2)
            write(nds,'(a20,g14.6,a1,a4,a3,a4)')
     *    ' Verstaerkung      :',pver,' ',ctyu(2),' / ',ctyu(3)
         else
            write(nds,'(a20,g14.6,a1,a4,a3,a4)')
     *    ' Komp. Gleichanteil:',pglw,' ',ctyu(2),' / ',ctyu(1)
            write(nds,'(a20,g14.6,a1,a4,a3,a4,a1,a4)')
     * ' Integrationsbeiw. :',pver,' ',ctyu(2),' / ',ctyu(3),'*',ctyu(1)
         end if
      end if
      if(name(1:4).eq.'con:'.or.name(1:4).eq.'CON:') then
         write(nds,'(a2)') blank
         call rtwait
      end if
c
c     2. Modelldaten
c
      if(job.eq.0) then
         read(nds,'(a2)') bldum
         read(nds,'(a40)') kopf
         read(nds,'(a20,i5)',   err=999) cdum,nk
         read(nds,'(a20,g14.6)',err=999) cdum,dtt
         read(nds,'(a20,i5)',   err=999) cdum,id
         read(nds,'(a20,i5)',   err=999) cdum,n
         read(nds,'(a20,g14.6)',err=999) cdum,rho
         read(nds,'(a40)') kopf
         read(nds,'(a40)') kopf
         n1=n+1
      else
         write(nds,'(a2)') blank
         write(nds,'(a40)')   ' Zeitdiskretes Prozessmodell:           '
         write(nds,'(a20,i5)')    ' Anzahl Punkte     :',nk
         write(nds,'(a20,g14.6,a4)') ' Abtastzeit        :',dt,ctyu(1)
         write(nds,'(a20,i5)')    ' Totzeiten         :',id
         write(nds,'(a20,i5)')    ' Prozessordnung    :',n
         write(nds,'(a20,g14.6)') ' Fehler LS-SchÑtzg.:',rho
         write(nds,'(a40)') ' Z-Uebertragungskoeffizienten B(z)/A(z):'
         write(nds,'(3a18)')' Polynom-Position ','   Zaehler B(i)   ',
     *   '   Nenner A(i)    '
      end if
      do 300 i=1,n1
         if(job.eq.0) then
            read(nds,'(a16,i2,2(g16.8,a2))',err=310) cdu16,bldum,
     *      b(i),bldum,a(i),bldum
         else
            write(nds,'(a16,i2,2(g16.8,a2))') ' Koeffizient Nr.',i,
     *      b(i),blank,a(i),blank
         end if
 300  continue
      ierr=0
      go to 320
 310  continue
      ierr=1
 320  continue
c
c     3. Reglerdaten
c
      if(job.eq.0) then
         if(ierr.ne.1) read(nds,'(a2)',err=999) bldum
         read(nds,'(a40)',err=999) kopf
         read(nds,'(a4,g12.4,3(a4,g12.4,a4))',err=999)
     *   cdu4,q(1),cdu4,q(2),cdu4,cdu4,q(3),cdu4,cdu4,q(4),cdu4
         read(nds,'(a20,g12.4)',err=999) cdum,r
      else
         write(nds,'(a2)') blank
         write(nds,'(a40)') ' Reglerkenngroessen:                    '
         write(nds,'(a4,g12.4,3(a4,g12.4,a4))')
     *   ' KP:',q(1),' TN:',q(2),ctyu(1),' TV:',q(3),ctyu(1),' T1:',
     *   q(4),ctyu(1)
         write(nds,'(a20,g12.4)') ' Stellgewichtung r :',r
      end if
c
c     4. Optimierungsparameter
c
      if(job.eq.0) then
         read(nds,'(a2)') bldum
         read(nds,'(a40)',err=999) kopf
         read(nds,'(6g12.4)',err=999) (dopt(i),i=1,6)
         read(nds,'(a20,i5)',   err=999) cdum,kmax
         read(nds,'(a20,i5)',   err=999) cdum,nou
      else
         write(nds,'(a2)') blank
         write(nds,'(a40)') ' Parameter der Minimierungsprozedur:    '
         write(nds,'(6g12.4)') (dopt(i),i=1,6)
         write(nds,'(a20,i5)')    ' Max. Optim.-Stufen:',kmax
         write(nds,'(a20,i5)')    ' Ausgabeoption     :',nou
      end if
      close(nds,status='keep')
      if(name(1:4).eq.'con:'.or.name(1:4).eq.'CON:') call rtwait
 900  continue
      return
 999  call pause(' Lesefehler!')
 1000 if=1
      return
      end
c
c
c
      subroutine cinput(charac,cha)
c
c     Zweck: Handeingabe einer Sequenz aus 4 Zeichen
c
c     Eingabe:
c     charac - Bezeichnung (35Zeichen)
c     cha    - Bisherige Zeichenkette
c
c     Ausgabe:
c     cha    - Neue Zeichenkette
c
c     Autor:Friedemann Roether
c     Letzte Aenderung:15.05.1987
c
      character*35 charac
      character*4 cha,cdum
  10  continue
      write(*,'(a35,a1,a4,a2\)') charac,'(',cha,')?'
      read(*,'(a4)',err=10) cdum
      call lclear
      if(cdum.ne.'   ') then
         cha=cdum
         go to 10
      end if
      write(*,'(a35,a1,a4)')  charac,':',cha
      return
      end
c
c
c
      subroutine rinput(charac,cpval,wert,job)
c
c     Zweck:Handeingabe einer Real-Zahl. Ist sie verschieden von Null,
c     dann wird sie dem eingelesenen Wert zugewiesen.
c
c     Eingabe:
c     charac  - Character-Bezeichnung (30 Zeichen)
c     cpval   - Physikalische Bezeichnung (4 Zeichen)
c     wert    - Bisheriger Wert
c     job     - 0: Keine Werte kleiner Null werden eingelesen
c
c     Ausgabe:
c     wert    - Neuer Wert
c
c     Autor: Friedemann Roether
c     Letzte Aenderung: 25.05.1987
c
      real*8 wert,wert1,wert2
      character*30 charac
      character*4 cpval
      character*1 c
 10   continue
      wert2=wert
      write(*,'(a1,a30,a1,a4,a5,f10.4,a2\)') ' ',charac,' ',cpval,
     *')   (',wert,'):'
      read(*,'(g18.10)',err=20) wert1
      go to 30
 20   continue
      c='J'
      call cisdin('Wert behalten <J> oder zu Null setzen <N> (J/N)',c)
      if(c.eq.'N') wert=0.d00
      call lclear
      go to 40
 30   continue
      if(wert1.ne.0.d0) then
       wert=wert1
       if(dabs(wert).lt.1.d-5.and.wert.ne.0.d00) wert=dnint(1.d10*wert)
       if(job.eq.0.and.wert1.lt.0.d00) wert=0.d00
      end if
 40   continue
      call lclear
      if(wert2.ne.wert) go to 10
      write(*,'(a1,a30,a1,a4,a5,f10.4)') ' ',charac,' ',cpval,
     *'):   ',wert
      return
      end
c
c
c
      subroutine cbnput(charac,cha)
c
c     Zweck: Eingabe einer laengeren Bezeichnung (30 Zeichen)
c
c     Eingabe:
c     charac - Vorspann
c     cha    - Bisheriger Textstring
c
c     Ausgabe:
c     cha    - Neuer Textstring
c
c     Autor: Friedemann Roether
c     Letzte Aenderung:15.05.1987
c
      character*17 charac
      character*30 cha,cdum
 10   continue
      write(*,'(a17,a2,a30,a1\)') charac,': ',cha,'?'
      read(*,'(a30)',err=10) cdum
      call lclear
      if(cdum(1:4).ne.'    ') then
         cha=cdum
         go to 10
      end if
      write(*,'(a17,a2,a30)') charac,': ',cha
      return
      end
c
c
c
      subroutine cisdin(text,c)
c
c     Zweck: Handeingabe einer Ja/Nein Antwort.
c
c     Eingabe:
c     text  - Ausgabetext (character*47)
c     c     - Bisheriger Character 'J' oder 'N'
c
c     Ausgabe:
c     c     - Neuer Character
c
c     Autor: Friedemann Roether
c     Letzte Aenderung:15.05.1987
c
      character*47 text
      character*1 c,ja
 10   continue
      write(*,'(a1,a47,a1,a1,a1\)') ' ',text,' ',c,'?'
      read(*,'(a1)',err=10) ja
      if(ja.eq.'j'.or.ja.eq.'J') c='J'
      if(ja.eq.'n'.or.ja.eq.'N') c='N'
      call lclear
      write(*,'(a1,a47,a1,a1,a1)') ' ',text,'?',' ',c
      return
      end
c
c
c
      subroutine lclear
c
c     Zweck: Loeschen der gerade beschriebenen Zeile und Positionieren
c     vor die Zeile
c
c     Autor:Friedemann Roether
c     Letzte Aenderung:12.06.87
c
      write(*,'(1x,11a\)') char(27),'[1A',char(27),'[K',char(27),'[1A'
      return
      end
c
      subroutine xyinpu(x,y,z,len,nki,ctyu)
c
c     Zweck: Eingabe gemessener Prozess-Wertetripel
c     Werte kînnen editiert, gelîscht und Åberschrieben werden.
c
c     Eingabe:
c     ctyu  - Character-Vektor (*4) mit physikalischen Einheiten
c     len   - Anzahl bisher eingegebener Wertetripel
c     nki   - Maximalanzahl der Wertetripel
c     x,y,z - Bisheriger Vektoren x(Zeit), y(Sprungantwort) und
c             z (Sprunganregung)
c
c     Ausgabe:
c     len   - Neue Wertetripel-Anzahl
c     x,y,z - Neue Vektoren
c
c     Autor: Friedemann Roether
c     Letzte Aenderung:15.05.1987
c
      implicit real*8(a-h,o-z)
      character*4 ctyu(3)
c
c     Zweck: Eingabe von len Datentripeln x(i),y(i),z(i)
c
      dimension x(nki),y(nki),z(nki)
c
      if(len.eq.0) go to 9
      icnt=0
c
c     Ausgabe bisher eingelesener Wertetripel
c
 2    continue
      call adclr
      write(*,5)
 5    format(' Ausgabe bisher eingelesener Wertetripel:')
      do 7 i=1,len
         write(*,8) i,x(i),ctyu(1),y(i),ctyu(2),z(i),ctyu(3)
 8       format(1x,i3,' -tes Tripel:',3(G14.6,A4))
 7    continue
 9    continue
      if(icnt.ne.1) write(*,30)
 30   format(
     *' Bitte Åbertragen Sie nun etwa 8 (mindestens 4 ) Wertetripel'/
     *' (Relativzeit, Sprungantwort, Proze·eingangsgrî·e) mîglichst'/
     *' exakt. Jedes Wertetripel ist durch Kommata zu trennen.     '/
     *' Beispiel fÅr eine Eingabe: 40,2.45,1.333 <RETURN>'/)
 40   continue
      len1=len+1
      if(len1.gt.nki) write(*,'(a31)') ' Max.Anz. Wertetripel erreicht.'
      write(*,50) len1
 50   format(' ',i3,' -tes Tripel:'\)
      read(*,60,err=40) time,sprant,sprung
 60   format(3g18.10)
      if(dabs(time).lt.1.d-5.and.time.ne.0.d00) time=time*1.d10
      if(dabs(sprant).lt.1.d-5.and.sprant.ne.0.d00) sprant=sprant*1.d10
      if(dabs(sprung).lt.1.d-5.and.sprung.ne.0.d00) sprung=sprung*1.d10
      if(time.eq.0.d0.and.sprant.eq.0.d0.and.sprung.eq.0.d0) then
         if(len.eq.0) then
c
c     Erstes eingegebenes Wertetripel
c
            x(1)=0.d00
            y(1)=0.d00
            z(1)=0.d00
            len=1
            i=1
            go to 40
         else
            icnt=icnt+1
            if(icnt.eq.1) go to 2
            if(icnt.eq.2) go to 999
         end if
      else
         call lclear
         write(*,8) len1,time,ctyu(1),sprant,ctyu(2),sprung,ctyu(3)
      end if
      icnt=0
c
c     Einsortieren der eingegebenen Werte
c
      if(len.eq.0) then
         len=1
         i=1
         go to 100
      end if
      do 70 i=1,len
         if(sngl(x(i))-sngl(time)) 70,100,80
 70   continue
      if(len1.gt.nki) go to 2
c
c     Neues Tripel ist letztes Tripel
c
      len=len+1
      i=len
      go to 100
c
c     Werte nach Position i nach hinten verschieben
c
 80   continue
      if(len1.gt.nki) go to 2
      i1=len-i+1
      do 90 j=1,i1
         j1=len-j+1
         x(j1+1)=x(j1)
         y(j1+1)=y(j1)
         z(j1+1)=z(j1)
 90   continue
      len=len+1
c
c     Wert an Position i einsortieren
c
 100  continue
      if(sprung.ne.0.d0.or.sprant.ne.0.d0
     *.or.sngl(x(i)).ne.sngl(time))  then
         x(i)=time
         y(i)=sprant
         z(i)=sprung
      else
c
c     Wert an Position i ausketten
c
         i1=i
         do 110 j=i1,len
            x(j)=x(j+1)
            y(j)=y(j+1)
            z(j)=z(j+1)
 110     continue
         len=len-1
      end if
      go to 40
 999  continue
      return
      end
c
c
c
      subroutine pause(text)
c
c     Zweck: Blinkende Anzeige eines Meldetextes
c
c     Eingabeparameter:
c     text - Character-String bis zur Laenge 50
c
c     Eingabeparameter:
c     text - Character der Laenge max 50
c
c     Autor:Friedemann Roether
c     Letzte Aenderung: 15.05.1987
c
      character*1 text(*)
      do 10 i=1,50
         if(text(i).eq.'!') go to 20
 10   continue
      go to 100
 20   continue
      i1=i-1
      write(*,'(1x,4a\)') char(27),'[5m'
      write(*,'(/a20\)') '                 ** '
      do 30 i=1,i1
         write(*,'(a1\)') text(i)
 30   continue
      write(*,'(a3,1x,4a\)') ' **',char(27),'[0m'
c
c     Pausenschleife
c
      do 40 i=1,15000
      do 35 j=1,2000   
      k = i+1
 35   continue
 40   continue
      write(*,'(1x,11a\)') char(27),'[1A',char(27),'[K',char(27),'[3A'
      call lclear
 100  continue
      return
      end
c
c
c
      subroutine rtwait
c
c     Zweck: Warten auf Druecken der Return-Taste
c
c     Autor:Friedemann Roether
c     Letzte Aenderung: 15.05.1987
c
      character*1 c
      call cline(57,1,1)
  5   write(*,10)
 10   format('Weiter mit <RETURN>.'\)
      read(*,20,err=5) c
 20   format(A1)
      return
      end
c
c
c
      subroutine fbcint
c
c     Zweck: Ausgabe eines Bildschirmkommentars im Program fbc
c     Regelparameter-Optimierung
c
c     Eingabeparameter:
c     Keine
c
c     Ausgabeparameter:
c     Keine
c
c     Autor: Friedemann Roether
c     Erste Version:   12.04.1987
c     Letzte Aenderung:15.05.1987
c
      call adclr
      write(*,10)
 10   format(
     *17x'Kurzanleitung FeedBackController (FBC)'//
     *17x'Zweck:  FeedBackController ist ein Programm fÅr den rechner-'/
     *17x'gestÅtzten interaktiven Entwurf von PI/PD/PDT1/PID/PIDT1-Reg'/
     *17x'lern auf der Basis gemessener Sprungantwort-VerlÑufe. Es be-'/
     *17x'inhaltet stÅtzpunktorientierte Me·dateneingabe,Reglerentwurf'/
     *17x'und Regelparameter-Optimierungen am Proze·modell,  grafische'/
     *17x'Simulation und Archivierung der erzielten Ergebnisse.'//
     *17x'Vorgehensweise:'//
     *17x'1. Sprunganregung auf den interessierenden  Proze· bzw.  den'/
     *17x'   Sollwert eines vorhandenen Reglers aufbringen.Messung der'/
     *17x'   Stell- und der Regelgrî·e aufzeichnen (z.B. durch Proze·-'/
     *17x'   schreiber) und sorgfÑltig mit Funktion (1) Åbertragen.'//
     *17x'2. Mit Funktion (2) prÅfen, ob das intern berechnete Proze·-'/
     *17x'   modell mit den gemessenen FunktionsverlÑufen harmoniert.'//
     *17x'3. Vorbelegung eines Reglers mit Funktion (3). Merke:Zu Null'/
     *17x'   gesetzte Parameter werden weder bei  Simulation  noch bei'/
     *17x'   Optimierung berÅcksichtigt und geben damit die Regelstruk')
      call rtwait
      call adclr
      write(*,20)
 20   format(
     *17x'   tur vor (PI-,PID-,etc.). Beliebige Parameter  kînnen  zur'/
     *17x'   Optimierung markiert werden - die restlichen,auch die von'/
     *17x'   Null verschiedenen Parameter bleiben konstant.'//
     *17x'4. Grafische Simulation des geschlossenen Regelkreises   (4)'/
     *17x'   mit der aktuellen Regelstruktur. Dient zur Kontrolle  der'/
     *17x'   Startwerte bzw. der mit (5) erzielten Ergebnisse.'//
     *17x'5. Funktion (5) initiiert die Parameteroptimierung im  Sinne'/
     *17x'   des quadratischen Integralkriteriums.Die Wahl der Gewich-'/
     *17x'   tung r (0-1) ergibt hart bis weich eingreifende Regler.'//
     *17x'6. Ausgabe aller Proze·/Regelparameter auf  Schirm mit (6).'//
     *17x'7. Ausgabe der internen Datenbasis wahlweise auf ein  Daten-'/
     *17x'   file, auf den Bildschirm ( con: ) oder den  Systemdrucker'/
     *17x'   ( prn: ) mit Funktion (7).'//
     *17x'8. Einlesen einer externen Datenbasis mit (8).'//
     *17x'9. Ausgabe dieser Kurzanleitung mit (9).'//
     *17x'10.Programmabbruch.')
      call rtwait
      call adclr
      write(*,25)
 25   format(
     *17x'Folgende Regelkreisstruktur wird zugrundegelegt:        '//
     *17x'Soll-                  Stell                 ≥ Gleich   '/
     *17x'wert w       ⁄ƒƒƒƒƒƒƒø grî·e y ⁄ƒƒƒƒƒƒƒƒø    ≥ wert x0  '/
     *17x' ƒƒƒ>Oƒƒƒƒƒƒ>≥  GR   ≥ƒƒƒƒƒƒƒƒ>≥   GS   ≥ƒƒƒ>Oƒƒƒƒƒƒƒø  '/
     *17x'   - ≥       ¿ƒƒƒƒƒƒƒŸ         ¿ƒƒƒƒƒƒƒƒŸ  +         ≥  '/
     *17x'     ≥        Regler          Regelstrecke           ≥  '/
     *17x'     ≥                                               ≥  '/
     *17x'     ≥                        Regelgrî·e x           ≥  '/
     *17x'     ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ  '//
     *17x'Die Strecke wird im Laplacebereich (L.-Parameter s=p+jw)'/
     *17x'folgenderma·en beschrieben:                             '/
     *17x'        x(s) = GS(s)*y(s) + x0                          '/
     *17x'Der Regler besitzt die Laplacedarstellung:              '/
     *17x'        y(s) = GR(s)*(w(s)-x(s))                        '/
     *17x'Er setzt sich aus den bekannten Einzeltermen zusammen:  '/
     *17x'       GR(s) = KP*(1+1/(TN*s)+TV*s/(1+T1*s))            '//
     *17x'KP - Proportionalbeiwert  TN - Nachstellzeit            '/
     *17x'TV - Vorhaltezeit         T1 - Vorhalt-Verzîgerung      '//
     *17x'x0 bedeutet den stellwertunabhÑngigen Gleichanteil. ')
      call rtwait
      call adclr
      write(*,30)
 30   format(
     *17x'Wichtige Hinweise:'//
     *17x'1. Alle Bildschirminhalte - auch grafische - kînnen mit den'/
     *17x'   Tasten <SHIFT> <PRTSC> (gleichzeitig drÅcken) auf  einem'/
     *17x'   vollkompatiblen Systemdrucker ausgedruckt werden.'//
     *17x'2. Die auf Platte abgespeicherte Datenbasis ist lesbar. Mit'/
     *17x'   einem Editor lassen sich alle Daten modifizieren.'//
     *17x'3. Ein Wertbelegungsvorschlag vom Programm wird mit<RETURN>'/
     *17x'   bestÑtigt. Wichtig: Belegung mit 0 durch "N" initiieren.'//
     *17x'4. Die derzeitige Programmversion unterstÅtzt proportionale'/
     *17x'   und integraltermbehaftete, aber (noch) keine sprungfÑhi-'/
     *17x'   gen Antwortprozesse.'//
     *17x'5. Die Prozesstotzeit darf nicht mehr als  20%  (12 Punkte)'/
     *17x'   des eingegebenen Zeitausschnitts betragen.')
      call rtwait
      return
      end
