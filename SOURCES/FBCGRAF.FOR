$STORAGE:2
c     Unterprogrammsammlung FBCGRAF.FOR
c
c     Grafikprogrramme fuer Programm FBC (FeedBackController)
c     Roether,1987
c
c
c
      subroutine xkurve(yk,uk,nk,time,sprant,sprung,len,yu,wy,yo,uu,wu,
     *uo,yanf,uanf,cbez,ctyu,job)
c
c     Zweck: Zeichnen von regelungstechnischen Kurvenverl„ufen auf den
c     IBM-PC-Grafikschirm. Bemerkung: Der Treiber ANSI.SYS muss unter
c     MS-DOS installiert sein
c
c     Eingabe:
c     job   - 0: Ohne Regelung, 1: Mit Regelungstext
c     ctyu  - Character-Vektor mit physikalischen Bezeichnungen
c     cbez  - Character-Vektor mit Bildueberschrift
c     uanf  - Erster Wert der Stellfunktion
c     yanf  - Erster Wert der Antwortfunktion
c     uo    - Obere Begrenzung der Stellkurve
c     wu    - Einschwingwert der Stellkurve
c     uu    - Untere Begrenzung der Stellkurve
c     yo    - Obere Begrenzung der Antwortkurve
c     wy    - Einschwingwert der Antwortkurve
c     yu    - Untere Begrenzung der Antwortkurve
c     len   - Anzahl der gemessenen Wertetripel
c     sprung- Vektor mit gemessener Stellerregung
c     sprant- Vektor mit gemessener Sprungantwort
c     time  - Vektor mit Zeitmarken
c     nk    - Anzahl zu zeichnender Punkte
c     yk    - Simulierte Antwortkurve
c     uk    - Interpolierte Stellgroesse
c
c     Unterprogramme:
c     swnset- Setzt Bildschirmrahmen
c     swndis- Zeichnet Bildschirmrahmen
c     adnum - Zeichnet eine Gleitkommazahl auf den Bildschirm
c     adclr - Loescht den Bildschirm
c     cline - Setzt den Cursor an bestimmte Stelle
c     sdset - Setzt Grafikpunkt
c     sdhlin- Schreibt horizontale Linie
c     rtwait- Stoppt Bildschirm-Ausgabe
c
c     Autor: Friedemann Roether
c     Letzte Aenderung: 22.05.1987
c
      implicit real*8(a-h,o-z)
      character*30 cbez(3)
      character*4 ctyu(3)
      dimension yk(nk),uk(nk),time(len),sprung(len),sprant(len)
c
c     Loesche Bildschirm, zeichne und beschrifte Rahmen
c
      call adclr
      call swnset(time(1),time(len),yu,yo,16,76,4,24,1)
      call swndis(ctyu(1),ctyu(2),1)
      call adnum(6,ipy(sngl(yanf),1),sngl(yanf))
      call adnum(6,ipy(sngl(wy),1),sngl(wy))
      call cline(16,25,1)
      write(*,'(a30,a30)') cbez(1),cbez(2)
      call sdhlin(wy,45,1)
      call cline(16,2,1)
      if(job.eq.0) then
         write(*,10)
 10      format('Systemantwort ( * Eingabe, + interpoliert/simuliert)')
      else
         write(*,20)
 20      format('Systemantwort mit Regelung (+ simuliert)  ')
      end if
c
c     Zeichne Antwortkurve
c
      t=time(1)
      dt=(time(len)-time(1))/(nk-1)
      do 30  ik=1,nk
         call sdset(t,yk(ik),43,1)
         t=t+dt
 30   continue
      if(job.eq.0) then
         do 40 ik=1,len
            call sdset(time(ik),sprant(ik),42,1)
 40      continue
      end if
      call rtwait
c
c     Loesche Bildschirm, zeichne und beschrifte Rahmen
c
      call adclr
      call swnset(time(1),time(len),uu,uo,16,76,4,24,2)
      call swndis(ctyu(1),ctyu(3),2)
      call adnum(6,ipy(sngl(uanf),2),sngl(uanf))
      call adnum(6,ipy(sngl(wu),2),sngl(wu))
      call cline(16,25,1)
      write(*,'(a30,a30)') cbez(1),cbez(3)
      call cline(16,2,1)
      if(job.eq.0) then
         write(*,50)
 50      format('Systemanregung ( = Handeingabe, - interpoliert)')
      else
         write(*,60)
 60      format('Regler-Stellfunktion ( - simuliert)')
      end if
c
c     Zeichne Stellkurve
c
      t=time(1)
      call sdset(time(1),uanf,45,2)
      do 70 ik=1,nk
         call sdset(t,uk(ik),45,2)
         t=t+dt
 70   continue
      if(job.eq.0) then
         call sdset(time(1),uanf,61,2)
         do 80 ik=1,len
            call sdset(time(ik),sprung(ik),61,2)
 80      continue
      end if
      call rtwait
      return
      end
c
c
c
      subroutine minmax(yu,wy,yo,uu,wu,uo,yanf,yend,uanf,uend,
     *sprant,sprung,len,ci,cs,if)
c
c     Zweck: Berechne Minima und Maxima fuer adaequate Kurvenbe-
c     schriftung.
c
c     Eingabe:
c     cs    - Character-Wert ('J':Stellerregung ist Sprungfunktion)
c     ci    - Character-Wert ('J':Strecke ist integrierend
c     len   - Laenge der Vektoren sprung und sprant
c     sprung- Vektor mit gemessener Sprungerregung
c     sprant- Vektor mit gemessener Sprungantwort
c     uend  - Endwert der Sprungerregung
c     uanf  - Anfangswert der Sprungerregung
c     yend  - Endwert der Sprungantwort
c     yanf  - Anfangswert der Sprungantwort
c
c     Ausgabe:
c     uo    - Oberer Grenzwert der Stellkurve
c     wu    - Endwert der Stellkurve
c     uu    - Unterer Grenzwert der Stellkurve
c     yo    - Oberer Grenzwert der Antwortkurve
c     wy    - Endwert der Antwortkurve
c     yu    - Unterer Grenzwert der Antwortkurve
c     if    - Fehlerparameter: Skalierungseingabe nicht konsistent
c
c     Autor: Friedemann Roether
c     Letzte Aenderung: 22.5.1987
c
      implicit real*8(a-h,o-z)
      dimension sprung(len),sprant(len)
      character*1 ci,cs
      if=0
c
c     Berechne Minima und Maxima der Kurve
c
      ymax=-1.d30
      ymin=1.d30
      umax=-1.d30
      umin=1.d30
      do 10 i=1,len
         ymax=dmax1(ymax,sprant(i))
         ymin=dmin1(ymin,sprant(i))
         umax=dmax1(umax,sprung(i))
         umin=dmin1(umin,sprung(i))
 10   continue
      ymax=dmax1(ymax,yend)
      ymax=dmax1(ymax,yanf)
      ymin=dmin1(ymin,yend)
      ymin=dmin1(ymin,yanf)
      umax=dmax1(umax,uend)
      umax=dmax1(umax,uanf)
      umin=dmin1(umin,uend)
      umin=dmin1(umin,uanf)
      if(umin.eq.umax) then
         call pause(' Fehler: Keine Bewegung der Sprungerregung!')
         if=1
         return
      end if
      if(ymin.eq.ymax) then
         call pause(' Fehler: Keine Bewegung der Prozeáantwort!')
         if=2
         return
      end if
c
c     Berechnung sinnvoller Kurvenbegrenzungen
c
      y1=yanf
      wy=yend
      y2=(wy-yanf)*5./4.+yanf
      u1=uanf
      if(ci.eq.'J') u1=-(uend-uanf)+uanf
      wu=uend
      u2=(uend-uanf)*4.+uanf
      uu=dmin1(u1,u2)
      uu=dmin1(uu,umin)
      uo=dmax1(u1,u2)
      uo=dmax1(uo,umax)
      yu=dmin1(y1,y2)
      yu=dmin1(yu,ymin)
      yo=dmax1(y1,y2)
      yo=dmax1(yo,ymax)
      return
      end
c
c     Grafische Funktionsbibliothek
c     Geeignet fuer semigrafische Ansteuerung des IBM-PC-BIldschirms
c     Roether/8/85
c
      subroutine adclr
c
c     Zweck: Loescht Bildschirm
c
      write(*,'(1x,3a\)') char(27),'[2J'
      return
      end
c
c
c
      subroutine adhlin(ix1,ix2,iy,itype)
c
c     Zweck:
c     Zeichnet horizontale linie auf alpha-bildschirm vom punkt ix1,iy
c     zum punkt ix2,iy
c
c     Eingabe:
c     ix1,iy - Erstes Koordinatenpaar
c     ix2,iy - Zweites Koordinatenpaar
c     itype  - ASCII-character nr. (2-255)
c
      character*1 cha(80),charac
c
      ianz=ix2-ix1+1
      charac=char(itype)
      do 10 i=1,ianz
         cha(i)=charac
 10   continue
      iy1=26-iy
      j2= int(iy1/10)
      j1= iy1-j2*10
      i2= int(ix1/10)
      i1= ix1-i2*10
      write (*,'(1x,2a,2i1,a,2i1,a,80a)') char(27),'[',j2,j1,';',
     *i2,i1,'f',(cha(i),i=1,ianz)
c
      return
      end
c
c
c
      subroutine adnum(ix,iy,zahl)
c
c     Zweck: Setzt real-zahl im format f9.3  an
c     die stelle ix,iy auf alpha-bildschirm
c     pos. (1,1) entspricht linke untere ecke!
c
c     Eingabe:
c     ix    - X-Position auf Bildschirm
c     iy    - Y-Position auf Bildschirm
c     itype - ASCII-Character (1-255)
c
      iy1=26-iy
      j2= int(iy1/10)
      j1= iy1-j2*10
      i2= int(ix/10)
      i1= ix-i2*10
      write (*,'(1x,2a,2i1,a,2i1,a,f9.3\)') char(27),'[',j2,j1,';',
     *                                  i2,i1,'f',zahl
c
      return
      end
c
c
c
      subroutine adset(ix,iy,itype)
c
c     Zweck: Setzt den ASCII-Character Nr. itype
c     an die stelle ix,iy auf alpha-bildschirm
c     pos. (1,1) entspricht linke untere ecke!
c     eingabe: ix,iy,itype
c
c     Eingabe:
c     ix   - X-Wert
c     iy   - Y-Wert
c     itype- ASCII-Character (1-255)
c
      iy1=26-iy
      j2= int(iy1/10)
      j1= iy1-j2*10
      i2= int(ix/10)
      i1= ix-i2*10
      write (*,'(1x,2a,2i1,a,2i1,2a)') char(27),'[',j2,j1,';',
     *                                  i2,i1,'f',char(itype)
      return
      end
c
c
c
      subroutine advlin(ix,iy1,iy2,itype)
c
c     Zweck:
c     zeichnet vertikale linie auf alpha-bildschirm vom punkt ix1,iy
c     zum punkt ix2,iy
c
c     Eingabe:
c     ix,iy1: Erstes Koordinatenpaar
c     ix,iy2: Zweites Koordinatenpaar
c     itype - ASCII-Character (1-255)
c
      integer dy
      dy=iy2-iy1
      jj=1
      if(dy.lt.0) jj=-1
      do 10 j=iy1,iy2,jj
10    call adset(ix,j,itype)
      return
      end
c
c
c
      subroutine cline(ix,iy,job)
c
c     ZwecK:Setzt cursor an die stelle ix,iy und
c     l”scht den rest der zeile iy ab pos. ix
c
c     Eingabe:
c     ix,iy: Koordinaten
c     job -  0:Loesche Rest der Zeile
c            1:Positioniere nur
c
      iy1=26-iy
      j2= int(iy1/10)
      j1= iy1-j2*10
      i2= int(ix/10)
      i1= ix-i2*10
      if(job.eq.0) then
      write (*,'(1x,2a,2i1,a,2i1,3a\)') char(27),'[',j2,j1,';', 
     *                                  i2,i1,'f',char(27),'[K'
      else
      write (*,'(1x,2a,2i1,a,2i1,a\)') char(27),'[',j2,j1,';',
     *                                  i2,i1,'f'
      end if
      return
      end
c
c
c
      function ipx(x,i)
c
c     Zweck:Graphik-skalierung einer x-koordinate
c
c     Eingabe:
c     i  - Nummer der Kurve
c     x  - x-Wert
c
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      ipx=ifix((x-xu(i))/(xo(i)-xu(i))*(io(i)-iu(i))+0.5)+iu(i)
      return
      end
c
c
c
      function ipy(y,i)
c
c     Zweck:Graphik-skalierung einer y-koordinate
c
c     Eingabe:
c     i  - Nummer der Kurve
c     y  - y-Wert
c
c
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      ipy=ifix((y-yu(i))/(yo(i)-yu(i))*(jo(i)-ju(i))+0.5)+ju(i)
      return
      end
c
c
c
      subroutine pxyskl(ix,iy,x,y,i)
c
c     Zweck:
c     Berechnet die koordinaten des fensters ix,iy
c     aus den virtuellen koordinaten x,y
c
      ix=ipx(x,i)
      iy=ipy(y,i)
      return
      end
c
c
c
      subroutine pxytst(iok,ix,iy,i)
c
c     testet, ob sich bildschirmkoordinaten innerhalb eines
c     erlaubten bildschirmfensters befinden
c     gleichzeitig werden die koordinaten ix,iy abgespeichert
c
c     eingabeparameter:
c     ix,iy,i - siehe library-vereinbarungen
c
c     ausgabeparameter:
c     iok     - iok=0 zeichnen nicht erlaubt
c               iok=1 zeichnen erlaubt
c
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      iok=0
      if(ix.lt.iu(i).or.ix.gt.io(i)) go to 10
      if(iy.lt.ju(i).or.iy.gt.jo(i)) go to 10
      iok=1
 10   ip(i)=ix
      ip(i+9)=iy
      return
      end
c
c
c
      subroutine sdhlin(y,itype,i)

c     zeichnet eine horizontale linie ueber den definitionsbereich
c     in der hoehe y in das i-te koordinatensystem 
c
c     eingabe:  y     real-gr”áe
c               i     koordinatensystem
c               itype ascii-character nr. (2-255)
c
      real*8 y
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      ix1=ipx(xo(i),i)
      ix2=ipx(xu(i),i)
      iy =ipy(sngl(y),i)
      call adhlin(ix2,ix1,iy,itype)
      return
      end
c
c
c
      subroutine sdvlin(x,itype,i)
c
c     zeichnet eine vertikale linie ueber den wertebereich
c     an der stelle x in das i-te koordinatensystem 
c
c     eingabe:  x     real-gr”áe
c               i     koordinatensystem
c               itype ascii-character nr. (2-255)
c
      real*8 x
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      iy1=ipy(yo(i),i)
      iy2=ipy(yu(i),i)
      ix =ipx(sngl(x),i)
      call advlin(ix,iy1,iy2,itype)
      return
      end
c
c
c
      subroutine sdset(x,y,itype,i)
c
c     zeichnet einen punkt nach x,y im i-ten koordinatensystem

c     eingabe:  x,y   real-gr”áe
c               i     koordinatensystem
c
      real*8 x,y
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      call pxyskl(ix,iy,sngl(x),sngl(y),i)
      call pxytst(iok,ix,iy,i)
      if(iok.eq.1) call adset(ix,iy,itype)
      if(iy.gt.jo(i)) call adset(ix,jo(i),30)
      if(iy.lt.ju(i)) call adset(ix,ju(i),31)
      return
      end
c
c
c
      subroutine ssquar(i)
c
c     zeichnet einen rahmen um definitions- und wertebereich
c     im i-ten koordinatensystem
c
c     eingabe:  i      koordinatensystem
c
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      call advlin(iu(i),jo(i),ju(i),179)
      call advlin(io(i),jo(i),ju(i),179)
      call adhlin(iu(i),io(i),ju(i),196)
      call adhlin(iu(i),io(i),jo(i),196)
      call adset(iu(i),ju(i),197)
      call adset(iu(i),jo(i),194)
      call adset(io(i),jo(i),191)
      call adset(io(i),ju(i),180)
      return
      end
c
c
c
      subroutine swndis(cx,cy,i)
c
c     zeichnet i-tes koordinatensystem und setzt skalierung
c
c     eingabe:  i      koordinatensystem
c               iscal  <0 : min/max-werte in unterster zeile
c                      >=0: skalierung an den ecken
c
      character*4 cx,cy
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)

      if(iu(i).lt.io(i).and.ju(i).lt.jo(i)) then
       call ssquar(i)
       if(yu(i)*yo(i).lt.0.0) call sdhlin(0.0d0,196,i)
       if(xu(i)*xo(i).lt.0.0) call sdvlin(0.0d0,179,i)
c
         call adnum(iu(i)-5,ju(i)-1,xu(i))
         call adnum(io(i)-8,ju(i)-1,xo(i))
         call adnum(iu(i)-10,ju(i),yu(i))
         call adnum(iu(i)-10,jo(i),yo(i))
         call cline(io(i)-16,ju(i)-1,1)
         write(*,'(a4)') cx
         call cline(iu(i)-6,jo(i)-2,1)
         write(*,'(a4)') cy
c
      endif
      return
      end
c
c
c
      subroutine swnset(xmi,xma,ymi,yma,ixmi,ixma,iymi,iyma,i)
c
c     setzt die parameter der kurve i
c     hierbei ist das koordinatensystem folgendermassen definiert:
c     xmi  - untere begrenzung (virtuelles system) horizontale
c     xma  - obere        #             #              #
c     ymi  - untere       #             #          vertikale
c     yma  - obere        #             #              #
c     ixmi - untere       #    (bildschirm-koord.) horizontale
c     ixma - obere        #             #              #
c     iymi - untere       #             #          vertikale
c     iyma - obere        #             #              #
c
c
      real*8 xmi,xma,ymi,yma
      common/p/xu(9),xo(9),yu(9),yo(9),iu(9),io(9),ju(9),jo(9),ip(21)
      xu(i)=sngl(xmi)
      xo(i)=sngl(xma)
      yu(i)=sngl(ymi)
      yo(i)=sngl(yma)
      iu(i)=ixmi
      io(i)=ixma
      ju(i)=iymi
      jo(i)=iyma
      ip(i)=ixmi
      ip(i+9)=iymi
      return
      end
