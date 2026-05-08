$DEBUG
$STORAGE:2
c     Unterprogrammsammlung FBCALGO.FOR
c
c     Algorithmen fuer Programm FBC (FeedBackController)
c     Roether,1987
c
c
c
      subroutine itaekr(x0,seu,li,iz)
c
c
c     Analytische Berechnung des quadratischen Integralkriteriums
c     mithilfe eines schnellen Algorithmus von Astrîm (1971)
c     in doppelter Genauigkeit
c
c     seu = Summe( y(i)*y(i)+r*Kp*u(i)*u(i)) i=1,unendlich
c     mit dem Gewichtungsfaktor r und der Streckenverstaerkung Kp
c
c     Eingabeparameter:
c     x0   - Vektor mit zu optimierenden Parametern
c     iz   - Anzahl bisheriger Funktionsaufrufe
c
c     Ausgabeparameter:
c     li    - 1, falls Berechnung nicht zulaessig
c     seu   - Wert des Integralkriteriums
c     iz    - um den Wert 1 inkrementierter Funktionsaufruf
c
c     Unterprogramme
c     pmp    - Polynommultiplikation
c     padd   - Polynomaddition
c     parsev - Berechnung der Varianz eines Prozesses
c
c     Autor:Friedemann Roether
c     Erste    Version: 12.02.81
c     Letzte Aenderung: 15.05.87
c
      implicit real*8(a-h,o-z)
      character*1 ci
      dimension x0(*),bz(22),az(22),as(22),h(22)
c
c     Der Common-Bereich teilt sich wie folgt auf:
c     r     -  Guetekriterium zwischen Null und Eins
c     vk    -  Regelstreckenverstaerkung
c     n     -  Ordnung der Regelstrecke
c     id    -  Totzeit der Regelstrecke
c     nrop  - Anz. simultan veraenderbarer Regelparameter (Optimierung)
c     dt    - Abtastzeit
c     irk   - Zeigervektor auf veraenderbare Regelparameter
c     regkon- Vektor mit zeitkontinuierlichen Regelparametern
c     a     - Nennerpolynomkoeffizienten der Regelstrecke
c     b     - Zaehlerpolynomkoeffizienten der Regelstrecke
c     qr    - Zaehlerkoeffizienten des diskreten Reglers
c     pr    - Nennerkoeffizienten des diskreten Reglers
c     se    - Regelabweichung
c     su    - Stellabweichung
c     ci    - Character-Wert: 'J':bedeutet: Strecke ist integrierend
c
      common/itaek/r,vk,n,id,nrop,dt,irk(4),regkon(4),
     *a(6),b(6),qr(4),pr(4),se,su,ci
c
c     Reset des Fehlerindikators
c
      li=0
c
c     Berechne diskrete Reglerwerte
c
      diff=0.
      do 5 i=1,nrop
         regkon(irk(i))=x0(i)
         if(irk(i).eq.4.and.x0(i).lt.1.) diff=1.-x0(i)
 5    continue
      call rkodis(pr,qr,npr,nqr,regkon(1)/vk,regkon(2),regkon(3),
     *regkon(4))
c
c     Setze B(z)=NS(z)*NR(z)
c
      call pmp(bz,nbz,a,n,pr,npr)
c
c     Berechne ZR(z)*ZS(z)
c
      call pmp(h,nh,b,n,qr,nqr)
c
c     Berechne z**-d * ZR(z)*ZS(z)
c
      if(id.ne.0) then
         do 10 i=1,nh
            l=nh-i+1
 10      h(l+id)=h(l)
         do 15 i=1,id
            h(i)=0.d00
 15      continue
         nh=nh+id
      end if
c
c     Berechne A(z) = NS(z)*NR(z) + ZR(z)*ZS(z)*z**-d
c
      call padd(az,naz,h,nh,bz,nbz)
c
c     Loeschen nichtbelegter Werte in B
c
      if(naz.gt.nbz) then
         nbz1=nbz+1
         do 30 i=nbz1,naz
            bz(i)=0.d00
 30      continue
      end if
c
c     Berechne NS(z)*ZR(z)
c
      call pmp(h,nh,qr,nqr,a,n)
c
c     Berechnung der Varianz der Stellenergie
c     Berechne-1/k * A(z)
c
      if(ci.ne.'J') then
         call pmp(as,nas,az,naz,-1./vk,1)
c
c        Berechne C(z) = (NS(z)*ZR(z)-1/k*A(z))
c
         call padd(h,nh,h,nh,as,nas)
      end if
c
c     Dividiere B(z) und C(z) durch 1-z**-1
c     (Rueckrechnung auf Sprunganregung)
c
      do 50 i=2,nh
          h(i)=h(i)+h(i-1)
 50   continue
      h(nh)=0.d00
      do 70 i=2,nbz
         bz(i)=bz(i)+bz(i-1)
 70   continue
      bz(nbz)=0.d00
c
c     Berechne Varianz
c
      call parsev(se1,su1,az,bz,h,naz,as,li)
      if(li.eq.0) then
         se=se1
         su=su1*vk*vk
         seu=(1.d00-r)*se+r*su+diff*diff*10.
      end if
      iz=iz+1
      return
      end
c
c
c
      SUBROUTINE XLSQR(THETA,N,YK,UK,NK,YANF,UANF,YG,V,ID,RH,LTH,PHI,
     *CI,RHO,CO,SO,IN)
C
C     ZWECK: LEAST-SQUARES-IDENTIFIKATION MIT LINEAREN RANDBEDINGUNGEN
C     UND PSEUDO-REKURSIVER ORTHOGONALZERLEGUNG
C     VGL. LINPACK USERS GUIDE
C
C     EINGABEPARAMETER:
C     IN    - 0:SYSTEM OHNE, 1: SYSTEM MIT DURCHGRIFF
C     SO    - VEKTOR MIT DREHUNGSPARAMETERN (SINUS)
C     CO    - VEKTOR MIT DREHUNGSPARAMETERN (COSINUS)
C     RHO   - QUADRATISCHER FEHLER DER SCHAETZUNG
C     CI    - CHARACTER*1 (INDIZIERT INTEGRIERENDES VERHALTEN
C     PHI   - MESSDATENVEKTOR
C     LTH   - MAXIMALDIMENSION DES SCHAETZPROBLEMS
C     RH    - INFORMATIONSMATRIX DER DIMENSION LTH*LTH
C     ID    - PROZESSTOTZEIT
C     V     - PROZESS-VERSTAERUNGSFAKTOR
C     YG    - GLEICHWERT Y0
C     UANF  - ANFANGSWERT DER STELLANREGUNG
C     YANF  - ANFANGSWERT DER SYSTEMANTWORT
C     NK    - ANZAHL AUSZUWERTENDER PUNKTE
C     UK    - VEKTOR MIT STELLERREGUNGEN
C     YK    - VEKTOR MIT SYSTEMANTWORTEN
C     N     - PROZESSORDNUNG
C
C     AUSGABEPARAMETER:
C     THETA - VEKTOR MIT IDENTIFIZIERTEN PARAMETERN
C     IN    - 0:NORMALER LAUF, 1: SINGULAERE INFORMATIONSMATRIX
C
C     UNTERPROGRAMME:
C     SCHUD - REKURSIVE ORTHOGONALZERLEGUNG (GIVENS-ROTATION)
C     STRSL - GLEICHUNGSLOESUNG MIT DREICKSMATRIX
C
C     AUTOR:FRIEDEMANN ROETHER
C     LETZTE AENDERUNG: 16.05.1987
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 CI
      DIMENSION THETA(*),PHI(*),YK(NK),UK(NK),CO(*),SO(*)
      DIMENSION RH(LTH,*)
      JOB=IN
      IN=0
      NTH=N+N
C
C     VERGROESSERE SCHAETZPROBLEM, FALLS SYSTEMDURCHGRIFF VORHANDEN
C
      IF(JOB.EQ.1) THEN
         NTH1=NTH+1
      ELSE
         NTH1=NTH
         THETA(NTH+1)=0.D00
      END IF
C
C     INITIALISIERE DATENVEKTOREN
C
      RHO=0.D00
      DO 10 I=1,NTH1
         PHI(I)=0.D00
         SO(I)=0.D00
         CO(I)=0.D00
         THETA(I)=0.D00
         DO 20 J=1,NTH1
            RH(I,J)=0.D00
 20      CONTINUE
 10   CONTINUE
C
C     AUFBAU DES SCHAETZGLEICHUNGSSYSTEMS
C
      DO 100 IK=1,NK
         DO 30 I=1,N
            IKI=IK-I
            YIKI=YANF
            IF(IKI.GT.0) YIKI=YK(IKI)
            PHI(I)=YG-YIKI
            IKI=IKI-ID
            UIKI=UANF
            IF(IKI.GT.0) UIKI=UK(IKI)
            IF(CI.EQ.'J') UIKI=UIKI-UANF
            PHI(I+N)=UIKI
 30      CONTINUE
         IF(JOB.EQ.1) THEN
            IKI=IK-ID
            UIKI=UANF
            IF(IKI.GT.0) UIKI=UK(IKI)
            IF(CI.EQ.'J') UIKI=UIKI-UANF
            PHI(NTH1)=UIKI
         END IF
         CALL SCHUD(RH,LTH,NTH1,PHI,THETA,LTH,1,YK(IK)-YG,RHO,CO,SO)
 100  CONTINUE
C
C     LOESE SCHAETZGLEICHUNGSSYSTEM
C
      CALL STRSL1(RH,LTH,NTH1,THETA,0,IN)
      IF(V.EQ.0.D00) GO TO 190
C
C     BERECHNE REDUZIERTE LEAST-SQUARES-LOESUNG
C     UNTER DER RANDBEDINGUNG LY=R
C
      IF(CI.NE.'J') THEN
C
C     RANDBEDINGUNG BEI INTEGRIERENDEM VERHALTEN
C     SUMME ALLER NENNERKOEFFIZIENTEN GLEICH NULL
C
         EPS=V
         DO 160 I=1,N
            PHI(I)=V
            PHI(I+N)=-1.
            EPS=EPS+V*THETA(I)-THETA(I+N)
 160     CONTINUE
         IF(JOB.EQ.1) THEN
            EPS=EPS-THETA(NTH1)
            PHI(NTH1)=-1.D00
         END IF
      ELSE
C
C     RANDBEDINGUNG BEI VORGEGEBENER VERSTAERKUNG
C     SUMME DER ZAEHLERKOEFFIZ.=SUMME NENNERKOEFF.*VERSTAERKUNG
C
         EPS=1.D0
         DO 165 I=1,N
            PHI(I)=1.D00
            PHI(I+N)=0.D0
            EPS=EPS+THETA(I)
 165     CONTINUE
         IF(JOB.EQ.1) PHI(NTH1)=0.D00
      END IF
      CALL STRSL1(RH,LTH,NTH1,PHI,1,IN)
      SUM=0.D00
      DO 170 I=1,NTH1
         SUM=SUM+PHI(I)*PHI(I)
 170  CONTINUE
      EPS=EPS/SUM
      CALL STRSL1(RH,LTH,NTH1,PHI,0,IN)
      DO 180 I=1,NTH1
         THETA(I)=THETA(I)-PHI(I)*EPS
 180  CONTINUE
C
C     ENTZERRE LOESUNG
C
 190  CONTINUE
      DUM=THETA(NTH+1)
      DO 200 I=1,N
         I1=NTH-I+1
         THETA(I1+1)=THETA(I1)
 200  CONTINUE
      THETA(N+1)=DUM
      RETURN
      END
c
c
c
      SUBROUTINE STRSL1(T,LDT,N,B,JOB,INFO)
C
C     ZWECK: LOESUNG DES GLEICHUNGSSYSTEMS T*X=B, WOBEI T
C     EINE OBERE DREIECKSMATRIX IST
C
C     EINGABEPARAMETER:
C     B    - VEKTOR B DER RECHTEN SEITE
C     N    - DIMENSION VON B UND ZEILEN/SPALTENZAHL VON T
C     LDT  - FUEHRENDE DIMENSION VON T
C     T    - OBERE DREIECKSMATRIX (BERECHNET MITTELS SCHUD)
C
C     AUSGABEPARAMETER:
C     B    - LOESUNG X
C
C     AUTOR:LINPACK,1979
C
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER LDT,N,INFO
      DIMENSION T(LDT,N),B(N)
C
C     STRSL1 SOLVES THE SYSTEM TX=B,WHERE T IS UPPER TRIANGULAR
C     SEE LINPACK FOR DETAILS
C     MODIFIED: T MUST BE UPPER TRIANGULAR
C     JOB=0: SOLVE TX=B
C     JOB=1: SOLVE TRANS(T)X=B
C
      INTEGER J,JJ
C
C     CHECK FOR ZERO DIAGONAL ELEMENTS
C
      DO 10 INFO=1,N
C ....EXIT
      IF(T(INFO,INFO).EQ.0.D0) GO TO 150
 10   CONTINUE
      INFO=0
      IF(JOB.EQ.1) GO TO 110
C
C     SOLVE TX=B FOR UPPER TRIANGULAR
C
 50   CONTINUE
      B(N)=B(N)/T(N,N)
      IF(N.LT.2) GO TO 70
      DO 60 JJ=2,N
         J=N-JJ+1
         TEMP=-B(J+1)
         CALL SAXPY1(J,TEMP,T(1,J+1),B(1))
         B(J)=B(J)/T(J,J)
 60   CONTINUE
 70   CONTINUE
      GO TO 140
C
C     SOLVE TRANS(T)*X=B FOR UPPER TRIANGULAR
C
 110  CONTINUE
      B(1)=B(1)/T(1,1)
      IF(N.LT.2) GO TO 130
      DO 120 J=2,N
         J1=J-1
         DO 105 K=1,J1
            B(J)=B(J)-T(K,J)*B(K)
 105     CONTINUE
         B(J)=B(J)/T(J,J)
 120  CONTINUE
 130  CONTINUE
 140  CONTINUE
 150  CONTINUE
      RETURN
      END
c
c
c
      SUBROUTINE SAXPY1(N,SA,SX,SY)
C
C     ZWECK: BERECHNET SY:=SY+SA*SX
C
C     EINGABEPARAMETER:
C     SY    - VEKTOR SY
C     SX    - VEKTOR SX
C     SA    - SKALAR SA
C     N     - DIMENSION DER VEKTOREN
C
C     AUSGABEPARAMETER:
C     SY    - NEUER VEKTOR
C
C     AUTOR: BLAS AUS LINPACK,1979
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SX(N),SY(N)
      IF(N.LE.0) RETURN
      IF(SA.EQ.0.0D0) RETURN
      DO 10 I=1,N
 10   SY(I)=SY(I)+SA*SX(I)
      RETURN
      END
c
c
c
      subroutine rkodis(pr,qr,npr,nqr,rkp,tit0,tdt0,t1t0)
c
c     Zweck: Umrechnung der Parameter zeitkontinuierlicher
c     Standardregler in entsprechende zeitdiskrete Regler
c
c     Zeitkontinuierlicher Regler:
c     u(s)=Kp(1+td*s/(1+t1*s)+ti/s) * e(s)
c
c     Zeitdiskreter Regler:
c     u(k)=pr(2)*u(k-1)+pr(3)*u(k-2) +
c         +qr(1)*e(k)+qr(2)*e(k-1)+qr(3)*e(k-2)
c
c     u -Reglerausgang, e - Regelabweichung, s - Laplaceoperator
c
c     Eingabe:
c     t1t0  - Verzoegerungszeit t1/t0 (t0 - Abtastzeit)
c     tdt0  - Differenzierzeit  td/t0
c     tit0  - Integrationszeit t0/ti
c     rkp   - Reglerverstaerkung
c
c     Ausgabe:
c     nqr   - Laenge der Regler-Zaehlerpolynoms (zeitdiskret)
c     npr   - Laenge des Regler-Nennerpolynoms (zeitdiskret)
c     qr    - Regler- Zaehlerkoeffizienten
c     pr    - Regler- Nennerkoeffizienten
c
c     Bemerkung: Die Abtastzeit t0 sollte klein gegenÅber den
c     Zeitkonstanten ti,td und t1 sein, da die Berechnung auf
c     einer numerischen Approximation beruht.
c
c     Autor: Friedemann Roether
c     Erste Version:  12.02.1982
c     Letzte Aenderung: 15.05.1987
c
      implicit real*8(a-h,o-z)
      dimension pr(*),qr(*)
      data zero,one,two/0.d00,1.d00,2.d00/
c
c     Umrechnung in adaequate Kehrwerte
c
      pr(1)=one
      if(t1t0.ne.zero) then
         tdt1=tdt0/t1t0
         t0t1=one/t1t0
         eat0t1=dexp(-t0t1)
         if(tit0.ne.zero) then
c
c           PI(DT1)-Regler (z-Transformierte)
c
            t0ti=one/tit0
            npr=3
            nqr=3
            qr(1)=(one+t0ti+tdt1*eat0t1)*rkp
            qr(2)=(-one-eat0t1*(t0ti+two*tdt1+one))*rkp
            qr(3)=(one+tdt1)*eat0t1*rkp
            pr(2)=-one-eat0t1
            pr(3)=eat0t1
         else
c
c           P(DT1)-Regler (z-Transformierte)
c
            npr=2
            nqr=2
            qr(1)=(one+tdt1*eat0t1)*rkp
            qr(2)=-(one+tdt1)*eat0t1*rkp
            pr(2)=-eat0t1
         end if
      else
         if(tit0.ne.zero) then
c
c           PI-,PID-Regler (Numerische Approximation)
c
            t0ti=one/tit0
            npr=2
            nqr=3
            qr(1)=(one+tdt0)*rkp
            qr(2)=(-one-two*tdt0+t0ti)*rkp
            qr(3)=tdt0*rkp
            pr(2)=-one
         else
c
c           P-,PD-Regler (Numerische Approximation)
c
            npr=1
            nqr=2
            qr(1)=(one+tdt0)*rkp
            qr(2)=-tdt0*rkp
         end if
      end if
      return
      end
c
c
c
      subroutine xrkonv(regpar,regkon,v,dt,ierr,job)
c
c     Zweck: Normierung und Denormierung zeitkontinuierlicher
c     Regelparameter auf Prozesssverstaerkung (Regelverstaerkung)
c     bzw. Abtastzeit (Integrations-,Differenzier-,Verzoegerungszeit)
c
c
c     Eingabe:
c     job    - 0: Normierung, 1: Denormierung
c     dt     - Abtastzeit
c     v      - Prozessverstaerkung
c     regkon - Vektor mit normierten Regelparametern
c     regpar - Vektor mit denormierten Regelparametern
c
c     Ausgabe:
c     job=0: regpar wird berechnet
c     job=1: regkon wird berechnet
c     ierr   - Besitzt den Wert 1, falls die Normierungsparameter
c              nicht konsistent sind
c
c     Autor: Friedemann Roether
c     Erste Version: 02.02.1987
c     Letzte Aenderung: 15.05.1987
c
      implicit real*8(a-h,o-z)
      dimension regpar(*),regkon(*)
      ierr=0
      if(v.eq.0.d00.or.dt.le.0.d00) then
         ierr=1
         return
      end if
      if(job.eq.0) then
         regpar(1)=regkon(1)/v
         regpar(2)=regkon(2)*dt
         regpar(3)=regkon(3)*dt
         regpar(4)=regkon(4)*dt
      else
         regkon(1)=regpar(1)*v
         regkon(2)=regpar(2)/dt
         regkon(3)=regpar(3)/dt
         regkon(4)=regpar(4)/dt
      end if
      return
      end
c
c
c
      subroutine xintpl(dt,yk,nk,x,y,nx,npoly,nou)
c
c     Zweck: Numerische Interpolation eingelesener Daten
c     Falls npoly=3 angegeben wird, erfolgt kubische Spline-
c     interpolation einer stetigen Kurve
c
c     Eingabeparameter:
c
c     npoly   - Ordnung des Interpolationspolynoms 0,1 oder 3
c     nou     - Ausgabeeinheit fuer interpolierte Werte
c     nx      - Anzahl der StÅtzstellen
c     y       - Funktionswerte an den StÅtzstellen
c     x       - StÅtzstellen
c     nk      - Anzahl der aequidistanten Punkte
c
c     Ausgabeparameter:
c
c     yk      - Vektor mit aequidistanten Stuetzstellen
c     dt      - Zeitabstand
c
c     Autor: Friedemann Roether
c     Erste Version: 2.1.1987
c     Letzte Aenderung: 15.05.1987
c
      implicit real*8(a-h,o-z)
      dimension fpl(6),dx(6),yk(nk),x(nx),y(nx),s(40),a(40)
c
c     Berechne Anfang und Ende der Spline-Interpolation
c
      nx1=nx-1
      do 5 i=2,nx1
         if(y(i).ne.y(i+1)) then
            ixanf=i
            go to 8
         end if
 5    continue
      ixanf=nx1
 8    continue
      do 10 i=nx1,2,-1
         if(y(i).ne.y(i+1)) then
            ixend=i
            go to 12
         end if
 10   continue
      ixend=2
 12   continue
      ixend1=ixend-1
      ixanf1=ixanf+1
      iabst=ixend-ixanf
      if(iabst.le.2) go to 50
      ygr0=(y(ixanf)-y(ixanf-1))/(x(ixanf)-x(ixanf-1))
      ygrn=(y(nx)-y(nx1))/(x(nx)-x(nx1))
c
c     Aufbau des tridiagonalen Gleichungssystems zur Bestimmung
c     der Splinekoeffizienten s(i)
c
      hi=x(ixanf1)-x(ixanf)
      s(ixanf)=hi/3.d00
      a(ixanf)=hi/6.d00
      do 15 i=ixanf1,ixend1
         him1=hi
         hi=x(i+1)-x(i)
         skal=s(i-1)/him1
         s(i)=a(i-1)-2.*skal*(hi+him1)
         a(i)=-skal*hi
 15   continue
      skal=s(ixend1)/hi
      s(ixend)=a(ixend1)-2.*skal*hi
c
c     Aufloesung des Gleichungssystems
c
      s(ixend)=(ygrn-(y(ixend)-y(ixend1))/hi)/s(ixend)
      do 20 i=ixend1,ixanf1,-1
         him1=x(i)-x(i-1)
         bi=(y(i+1)-y(i))/hi-(y(i)-y(i-1))/him1
         s(i)=(bi-a(i)*s(i+1))/s(i)
         hi=him1
 20   continue
      bi=(y(ixanf1)-y(ixanf))/hi-ygr0
      s(ixanf)=(bi-a(ixanf)*s(ixanf1))/s(ixanf)
c
c     Interpolationsschleife
c
 50   continue
      t=x(1)
      dt=(x(nx)-x(1))/(nk-1)
      ix=1
      do 100 i=1,nk
         if(t.ge.x(ix).and.ix.lt.nx) ix=ix+1
         if(npoly.le.0) then
c
c           Rechteckinterpolation
c
            yk(i)=y(ix-1)
         else
         if(ix.le.ixanf.or.ix.gt.ixend.or.npoly.eq.1.or.iabst.le.2) then
c
c           Trapezinterpolation
c
            yk(i)=((y(ix)-y(ix-1))/(x(ix)-x(ix-1)))*(t-x(ix-1))+y(ix-1)
         else
c
c           Kubische Spline-Interpolation
c
            hi=x(ix)-x(ix-1)
            hid6=hi/6.d00
            him6=hi*6.d00
            d1=t-x(ix)
            d2=t-x(ix-1)
            sx=-(s(ix-1)/him6)*d1*d1*d1+(s(ix)/him6)*d2*d2*d2
            sx=sx+(y(ix)/hi-s(ix)*hid6)*d2-(y(ix-1)/hi-s(ix-1)*hid6)*d1
            yk(i)=sx
            if(dabs(d2).lt.dabs(d1)*1.d-3) yk(i)=y(ix-1)
         end if
         end if
         if(nou.ge.2) write(*,'(2(a6,g14.6))') ' Zeit:',t,' Wert:',yk(i)
         t=t+dt
 100  continue
      yk(nk)=y(nx)
      return
      end
c
c
c
      subroutine xsimul(yr,ur,wk,nk,uanf,uend,a,b,v,yglw,n,pr,qr,
     *npr,nqr,id,ci,cs,job,nou,ifail)
c
c     Zweck: Simulation des Antwortverhaltens eines diskreten
c     Eingroessenprozesses ohne und mit Regelung
c
c     Eingabe:
c     nou    - Ausgabeeinheit
c     job    - 0:Keine Regelung  1: Regelung
c     cs     - Character-Variable, 'J' bedeutet: Prozess sprungfaehig
c     ci     - Character-Variable, 'J' bedeutet: Prozess integrierend
c     id     - Totzeit des Prozesses
c     nqr    - Laenge des Regler-Zaehlerpolynoms
c     npr    - Laenge des Regler-Nennerpolynoms
c     qr     - Regler-Zaehlerpolynom
c     pr     - Regler-Nennerpolynom
c     n      - Prozessordnung
c     yglw   - Gleichwert y0 auf Prozessausgang
c     v      - Prozessverstaerkung
c     b      - Strecken-Zaehlerpolynomkoeffizienten
c     a      - Strecken-nennerpolynomkoeffizienten
c     uanf   - Anfangswert der Anregung
c     uend   - Endwert der Anregung
c     nk     - Anzahl der zu simulierenden Punkte
c     wk     - Anregungssequenz
c
c     Ausgabe:
c     ur     - Vektor mit Simulation des Reglerausgangs
c     yr     - Vektor mit Simulation des Streckenausgangs
c     ifail  - 0: Normale Simulation, 1:instabil
c
c     Autor: Friedemann Roether
c     Erste Version: 12.02.1982
c     Letzte Aenderung:15.05.1987
c
      implicit real*8(a-h,o-z)
      character*1 ci,cs
      dimension yr(nk),ur(nk),wk(nk),a(*),b(*),pr(npr),qr(nqr)
      ifail=0
      n1=n+1
      yanf=v*uanf
      range=v*(uend-uanf)
      if(range.eq.0.d00) range=v
      range=dabs(range)
      rmax=300.d0*range
      if(ci.eq.'J') yanf=0.d00
c
c     Beginn der Simulationsschleife
c
      do 10 i=1,nk
        yr(i) = 0
 10   continue

      do 300 i=1,nk
c
c     Berechne Prozessvorhersage
c
         sum=0.d00
         do 250 j=2,n1
            k=i-j+1
            if(k.le.0) then
               yrk=yanf
            else
               yrk=yr(k)
            end if
            k=k-id
            if(k.le.0) then
               urk=uanf
               if(ci.eq.'J'.and.job.eq.1) urk=0.d00
            else
               urk=ur(k)
            end if
            if(ci.eq.'J'.and.job.eq.0) urk=urk-uanf
            sum=sum-a(j)*yrk+b(j)*urk
C            write(*,240) i,j,k,sum
C 240        format('  i,j,k,sum:',i4,i4,i4,g8.2)
C            write(*,245) (a(l),l=1,4)
C            write(*,245) (b(l),l=1,4)
C 245        format(g8.2,g8.2,g8.2,g8.2)
C            call rtwait()
 250     continue
         if(dabs(sum).gt.rmax) then
            ifail=1
            return
         end if
         yr(i)=sum
         if(job.eq.1) then
c
c        Berechne Reglervorhersage
c
            if(ci.ne.'J') then
               sum=qr(1)*(v*wk(i)-yr(i))
               if(cs.eq.'N') sum=qr(1)*(v*uend-yr(i))
            else
               sum=qr(1)*(v*(wk(i)-uanf)-yr(i))
               if(cs.eq.'N') sum=qr(1)*(v-yr(i))
            end if
            do 280 j=2,nqr
               k=i-j+1
               if(k.le.0) then
                  ek=0.d00
                  urk=uanf
                  if(ci.eq.'J') urk=0.d00
               else
                  urk=ur(k)
                  if(ci.ne.'J') then
                     ek=v*wk(k)-yr(k)
                     if(cs.eq.'N') ek=v*uend-yr(k)
                  else
                     ek=v*(wk(k)-uanf)-yr(k)
                     if(cs.eq.'N') ek=v-yr(k)
                  end if
               end if
               prj=0.d00
               if(j.le.npr) prj=pr(j)
               sum=sum-prj*urk+qr(j)*ek
 280        continue
            ur(i)=sum
         end if
 300  continue
c
c     Ende der Simulationsschleife. Bei Bedarf Ausgabe
c     Korrektur durch Aufadditon des Gleichwertes
c
      do 400 i=1,nk
         i1=i-1
         yr(i)=yr(i)+yglw
         if(ci.eq.'J'.and.job.eq.1) ur(i)=ur(i)+uanf
         if(nou.ge.2) write(*,'(a9,i2,a6,g14.6,a6,g14.6)')
     *   ' Schritt:',i1,' y(k):',yr(i),' u(k):',ur(i)
 400  continue
      return
      end
