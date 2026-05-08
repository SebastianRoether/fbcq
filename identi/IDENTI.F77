      PROGRAM IDENTI
C
C     ZWECK:
C     HAUPTPROGRAMM ZUR VERWALTUNG ZYKLISCH ANFALLENDER PROZESSDATEN,
C     EINSCHLIESSLICH IDENTIFIKATION DYNAMISCHER SYSTEME
C     PROJEKT: IDENTIFIKATION MIT PC'S
C
C     EINGABEPARAMETER:
C     SIEHE COMMON-VEREINBARUNG IN BLOCK-DATA-ROUTINE
C
C     AUSGABEPARAMETER:
C     SIEHE COMMON-VEREINBARUNG IN BLOCK-DATA-ROUTINE
C
C     AUFGERUFENE UNTERPROGRAMME:
C     SINPUT  - PARAMETEREINGABE
C     XGRAPH  - GRAFISCHE AUSGABE
C     XIDENT  - IDENTIFIKATION
C
C     BEARBEITER: ROETHER
C     LETZTE AENDERUNG: 23/07/86
C
C
      DIMENSION V(10),NREG(11,2)     
C
C     VARIABLEN-VEREINBARUNGEN
C
      PARAMETER(NDAMA=2,NADMA=8,NADDAX=10,LENMA=1000)
      LOGICAL EXSTAT
      REAL FPAR,PMIN(NADDAX),PMAX(NADDAX)
      INTEGER*4 IZ,IZSAVE,IZMOLD,IMW(NADDAX)
      INTEGER*2 ITIME,ICNEND,NADU,NDAU,LEN,ITYPMW,IVMA(NDAMA),  
     *NRDM(NDAMA),NSEQ(NDAMA),NRSB(NDAMA),IPL(NADDAX),NPL
      INTEGER*2 JOBPL,ISCALE(NADMA)
      INTEGER*2 ICNT,NPOINT,IONLFL
      INTEGER*2 IDAT(NADDAX,LENMA)
      INTEGER*2 NADMAX,NDAMAX,IADMIN,IADMAX,IDAMIN,IDAMAX,LENMAX
      CHARACTER*20 TEXT(NADDAX),TBUF(1)
      CHARACTER*6 PBEZ(NADDAX)
      CHARACTER*12 DNAME
C
C     COMMON-VEREINBARUNGEN
C
      COMMON/KONFIG/ITIME,ICNEND,NADU,NDAU,LEN,ITYPMW,IMW,IVMA,
     *NRDM,NSEQ,NRSB,IPL,NPL,JOBPL,ISCALE,FPAR,PMIN,PMAX
      COMMON/STATUS/IZ,ICNT,NPOINT,IONLFL,IZMOLD
      COMMON/ROHDAT/IDAT          
      COMMON/GRENZW/NADMAX,NDAMAX,IADMIN,IADMAX,IDAMIN,IDAMAX,LENMAX,
     *NPMAX
      COMMON/PTEXTE/TEXT,PBEZ
      DATA IUNKO,IUNDT,IUNPR/8,9,7/
C
C     ENDE DES VORSPANNS
C
      NADDA=NADU+NDAU
C
C     HAUPTMENUE
C
 100  WRITE(*,110)
 110  FORMAT(' Hauptprogramm zur Parameteridentifikation'/    
     */' Programmabbruch_____________________________________1' 
     */' Vorbesetzungen  relevanter Funktionsparameter_______2' 
     */' Ausgabe relevanter Funktionsparameter_______________3' 
     */' Datenkommunikation mit Festplatte___________________4' 
     */' Start der Wandlerlese- und Schreibroutinen__________5' 
     */' Abbruch der Wandlerlese- und Schreibroutinen________6' 
     */' Zyklische Wertausgabe der Kanaele___________________7'
     */' Grafikausgabe_______________________________________8'  
     */' Identifikation______________________________________9'/
     */' Ihre Eingabe:')   
      READ(*,*,ERR=100) JOB
      IF(JOB.LE.0.OR.JOB.GT.9) GO TO 100
      GO TO(1000,2000,3000,4000,5000,6000,7000,8000,9000),JOB
C
C     PROGRAMMABBRUCH
C
 1000 CONTINUE
      IF(IONLFL.EQ.1) CALL INTUHC
C
C     PROGRAMMABBRUCH           
C
      GO TO 9999
C
C     INITIALISIERUNG RELEVANTER PARAMETER
C
 2000 CONTINUE
      WRITE(*,2010)
 2010 FORMAT(' Bitte Vorbesetzungen neu definieren:'   
     */,' Die Wahl 2,3,4,5,6 erzeugt einen Zeiger-Reset!'/
     */,' Zurueck ins Hauptmenue____________________________1'
     */,' Anzahl der AD-Wandlerkanaele______________________2'
     */,' Anzahl der DA-Wandlerkanaele______________________3'
     */,' Festlegung der Uhreninterrupts und Multiplikators_4'
     */,' Festlegung der Anregungssignale an DA-Kan{len_____5' 
     */,' Anzahl der gespeicherten Daten____________________6' 
     */,' Festlegung der Mittelwertbildungen________________7'
     */,' Wahl der Text}berschriften f}r gezeichnete Kan{le_8' 
     */,' Festlegung physikalischer Skalierungsbereiche_____9' 
     */,' Ihre Wahl:')
      READ(*,*,ERR=2000) JOB2
      IF(JOB2.LE.0.OR.JOB2.GT.9) GO TO 2000
      GO TO(2100,2200,2300,2400,2500,2600,2700,2800,2900),JOB2
 2100 CONTINUE
      GO TO 100
C
C     AD-WANDLERKANAELE
C
 2200 CONTINUE
      CALL SINPUT(NADU,' Anzahl der AD-Wandlerkanaele: ')
      GO TO 2980
C
C     DA-WANDLERKANAELE
C
 2300 CONTINUE
      CALL SINPUT(NDAU,' Anzahl der DA-Wandlerkanaele: ')
      GO TO 2980
C
C     INTERRUPT-ABFRAGESIGNAL AN PERIPHERIE
C
 2400 CONTINUE
      CALL SINPUT(ITIME,' Zeitinterrupt 1,10 oder 600 (*100msec): ')
      CALL SINPUT(ICNEND,' Multiplikator (Produkt => Abtastzeit): ')
      GO TO 2000
C
C     VORBELEGUNG VON DA-KANALSEQUENZEN
C
 2500 CONTINUE
      IF(NDAU.LE.0) GO TO 9910
      CALL SINPUT(IC,' Welcher Kanal?:')
      IF(IC.GT.2) GO TO 2500
      WRITE(*,2510) LEN
 2510 FORMAT(/' Welche Signalform (Laenge',I5,')?'
     */,' Rechtecksignal____________________________________1'
     */,' Pseudobinaeres Signal_____________________________2' 
     */,' Selbstgeneriertes Signal__________________________3'/
     */,' Ihre Eingabe:')
      READ(*,*,ERR=2500) JOB21
      IF(JOB21.LE.0.OR.JOB21.GT.3) GO TO 2500
      IC1=IC+NADMAX
      IF(JOB21.EQ.3) GO TO 2585
      VM=10.*FLOAT(IMW(IC1))/FLOAT(IDAMAX-IDAMIN)
      CALL SRNPUT(VM,' Mittelwert in Volt:')
      IDAMW=IFIX(VM/10.*(IDAMAX-IDAMIN))
      IMW(IC1)=IDAMW
      VM=10.*FLOAT(IVMA(IC))/FLOAT(IDAMAX-IDAMIN)
      CALL SRNPUT(VM,' Amplitude in Volt:')
      IDAMV=IFIX(VM/10.*(IDAMAX-IDAMIN))
      IVMA(IC)=IDAMV
      IDAMI=IDAMW-IDAMV     
      IDAMA=IDAMW+IDAMV
      CALL SINPUT(NSEQ(IC),' Sequenzlaenge bezogen auf Abtastzeit:')
      GO TO(2520,2550),JOB21
C
C     RECHTECKSIGNAL 
C
 2520 CONTINUE                 
      IDAT(IC1,1)=IDAMA
      NRDM(IC)=0
      NRSB(IC)=0
      DO 2530 I=2,LEN 
         IF(MOD(I,NSEQ(IC)).EQ.1.OR.NSEQ(IC).EQ.1) THEN       
            IF(IDAT(IC1,I-1).EQ.IDAMA) THEN     
               IDAT(IC1,I)=IDAMI
            ELSE                              
               IDAT(IC1,I)=IDAMA
            END IF
         ELSE
            IDAT(IC1,I)=IDAT(IC1,I-1)
         END IF
 2530 CONTINUE
      GO TO 2000
C
C     VORBESETZUNG MIT PSEUDOBINAEREN RAUSCHSIGNALEN
C     1. INITIALISIERUNG DER SCHIEBEREGISTER 
C
 2550 CONTINUE
      CALL SINPUT(NRDM(IC),' L{nge des Schieberegisters:')
      CALL SINPUT(NRSB(IC),' Index des r}ckgekoppelten Registers:')
      NRDMIC=NRDM(IC)
      DO 2560 I=1,NRDMIC
         NREG(I,IC)=IDAMI
 2560 CONTINUE
      NREG(1,IC)=IDAMA
      NREG(5,IC)=IDAMA
      NREG(9,IC)=IDAMA
      NREG(10,IC)=IDAMA
C
C     2. ABFRAGE DER SCHIEBEREGISTER & SCHIEBEN
C
      IDAMW2=2*IDAMW
      DO 2580 I=1,LEN
         IDAT(IC1,I)=NREG(NRDMIC,IC)
         IF(MOD(I,NSEQ(IC)).EQ.1.OR.NSEQ(IC).EQ.1) THEN      
            N1=IDAMI
            IF(NREG(NRSB(IC),IC)+NREG(NRDMIC,IC).NE.IDAMW2) N1=IDAMA 
            N2=NRDMIC-1
            DO 2575 K=1,N2
               N3=NRDMIC-K
               NREG(N3+1,IC)=NREG(N3,IC)
 2575       CONTINUE
            NREG(1,IC)=N1
         END IF
 2580 CONTINUE
      GO TO 2000
C
C     SELBSTGENERIERTES SIGNAL
C
 2585 CONTINUE
      ISTART=1
      IWOLD=IDAMIN
 2590 CONTINUE
      CALL SINPUT(IEND,' Abtastzeitpunkt:')
      WERT=10.*FLOAT(IWOLD)/FLOAT(IDAMAX-IDAMIN)
      CALL SRNPUT(WERT,' Zugeh|riger Wert in Volt:')     
      IWERT=IFIX(WERT/10.*(IDAMAX-IDAMIN))
      IABST=IEND-ISTART   
      IF(IEND.LE.LEN) THEN
         DO 2595 I=ISTART,IEND  
            STEIGU=FLOAT(IWERT-IWOLD)/FLOAT(IABST)              
            IDAT(IC1,I)=IWOLD+INT(STEIGU*IABST)   
 2595    CONTINUE
         ISTART=IEND
         IWOLD=IWERT
         IF(IEND.LT.LEN) GO TO 2590
      END IF
      GO TO 2000
C
C     EINLESEN DER DATENFELDLAENGE
C
 2600 CONTINUE
      CALL SINPUT(LEN,' Laenge des Datenfeldes:')
      GO TO 2000
C
C     FESTLEGUNG DER MITTELWERTBILDUNGEN
C
 2700 CONTINUE
      WRITE(*,2705)
 2705 FORMAT(/' Wie soll die Mittelwertbildung erfolgen?'//
     *' Keine Mittelwertbildung ______________________________0'/
     *' Explizite Vorgabe von Mittelwerten____________________1'/
     *' Berechnung der Mittelwerte aufgrund der Messhistorie__2'/
     *' On-line-Berechnung der Mittelwerte: Neustart__________3')
      ITYPMO=ITYPMW
      CALL SINPUT(ITYPMW,' Ihre Wahl:')
      IF(ITYPMW.LT.0.OR.ITYPMW.GT.4) GO TO 2700
      IF(ITYPMW.EQ.0) THEN       
         DO 2707 I=1,NADU
            IMW(I)=IADMIN
 2707    CONTINUE
         GO TO 2000
      END IF
      GO TO(2710,2720,2730),ITYPMW
 2710 CONTINUE
      IF(NADU.LE.0) GO TO 9910
      DO 2715 I=1,NADU 
      WRITE(*,2712) I
 2712 FORMAT(' AD-Kanal',I2)
      RMW=10.*FLOAT(IMW(I))/FLOAT(IADMAX-IADMIN)
      IZSAVE=IZ
      IF(ITYPMO.EQ.3) RMW=RMW/FLOAT(IZSAVE-IZMOLD)
      CALL SRNPUT(RMW,' Mittelwert in Volt:')
      IMW(I)=IFIX(RMW/10.*(IADMAX-IADMIN))
 2715 CONTINUE
      GO TO 2000
C
C     VORGABE AUFGRUND DER MESSHISTORIE
C
 2720 CONTINUE
      CALL SINPUT(IMWLAG,' Wieviele Punkte aus Messhistorie:')
      NPSAVE=NPOINT-IMWLAG
      DO 2729 J=1,NADU
         IMW(J)=0
         DO 2728 I=1,IMWLAG
            IMW(J)=IMW(J)+IDAT(J,IPLAG(NPSAVE,I,LEN))
 2728    CONTINUE
         IMW(J)=IMW(J)/IMWLAG
 2729 CONTINUE
      GO TO 2000
C
C     ONLINE-BERECHNUNG:Neustart
C
 2730 CONTINUE
      DO 2735 I=1,NADU
         IMW(I)=0          
 2735 CONTINUE
      IZMOLD=IZ
      GO TO 2000
C
C     TEXTEINGABE FUER BILDUEBERSCHRIFTEN
C
 2800 CONTINUE
      IF(NADDA.LE.0) GO TO 9910
      DO 2850 I=1,NADU  
      WRITE(*,2810) I,TEXT(I)
 2810 FORMAT(' AD-Kanal',I2,/' Bisherige Ueberschrift:',A20,/
     *' Neue Ueberschrift (Mit ! abschliessen):')
      READ(*,2875,ERR=2800) TEXT(I)
      WRITE(*,2820) PBEZ(I)
 2820 FORMAT(' Physikalische Einheit bisher:',A6,/
     *' Neue Bezeichnung:')
      READ(*,2825) PBEZ(I)
 2825 FORMAT(A6)
 2850 CONTINUE
 2860 CONTINUE
      DO 2880 I=1,NDAU
      WRITE(*,2870) I,TEXT(NADMAX+I)
 2870 FORMAT(' DA-Kanal',I2,/' Bisherige Ueberschrift:',A20,/
     *' Neue Ueberschrift (Mit ! abschliessen):')
      READ(*,2875,ERR=2860) TEXT(NADMAX+I)
 2875 FORMAT(A20)   
      WRITE(*,2820) PBEZ(NADMAX+I)
      READ(*,2825) PBEZ(NADMAX+I)
 2880 CONTINUE
      GO TO 2000
C
C     PHYSIKALISCHE MESSBEREICHE
C
 2900 CONTINUE
      NADDA=NADU+NDAU
      DO 2950 I=1,NADDA
         WRITE(*,2910) I
 2910    FORMAT(' Kanal:',I3)
         CALL SRNPUT(PMIN(I),' Minimum: ')
         CALL SRNPUT(PMAX(I),' Maximum: ')              
 2950 CONTINUE
      GO TO 2000
C
C     RESETS
C
 2980 CONTINUE
C
C     RESET DER GRAFIKDEFINITIONEN
C
      NADDA=NADU+NDAU
      NPL=NADDA     
      DO 2990 I=1,NPL
      IF(I.LE.NADU) IPL(I)=I
      IF(I.GT.NADU) IPL(I)=NADMAX+I-NADU
 2990 CONTINUE
      GO TO 2000
C
C     AUSGABE RELEVANTER PARAMETER
C
 3000 CONTINUE
      GO TO 100
C
C     KOMMUNIKATION MIT DISK
C
 4000 CONTINUE
      WRITE(*,4010)
 4010 FORMAT(/' Submen} Datenkommunikation mit Platte:'//
     *' R}ckkehr ins Hauptmen}__________________________________0'/
     *' Lese Konfigurierungsdaten von Platte____________________1'/
     *' Lese Mess-und Konfigurierungsdaten von Platte___________2'/
     *' Schreibe Konfigurierungsdaten auf Platte________________3'/
     *' Schreibe Mess- und Konfigurierungsdaten auf Platte______4'/ 
     *' Lese Messdatenfile von Honeywell H-TMS-3000_____________5'/
     *' Ihre Wahl:')
      READ(*,*,ERR=4000) JOB41
      IF(JOB41.LT.0.OR.JOB41.GT.5) GO TO 4000
      IF(JOB41.EQ.0) GO TO 100
      DNAME='            '
      WRITE(*,4020) 
 4020 FORMAT(' Bitte vollst{ndigen Dateinamen eingeben:')
      READ(*,4030,ERR=4000) DNAME
 4030 FORMAT(A12)
C     INQUIRE(DNAME,EXIST=EXSTAT)
C     IF(.NOT.EXSTAT.AND.JOB41.LT.3) GO TO 9940
      IUND=IUNKO
      IF(JOB41.EQ.2.OR.JOB41.EQ.4.OR.JOB41.EQ.5) IUND=IUNDT
      IF(JOB41.EQ.5) THEN
         OPEN(UNIT=IUND,FORM='FORMATTED',STATUS='UNKNOWN',FILE=DNAME,
     *   IOSTAT=IERRFL,ERR=9940)
      ELSE
         OPEN(UNIT=IUND,FORM='UNFORMATTED',STATUS='UNKNOWN',FILE=DNAME,
     *   IOSTAT=IERRFL,ERR=9940)
      END IF
      IF(IERRFL.NE.0) GO TO 9940
      REWIND IUND
      GOTO(4100,4200,4300,4400,4500), JOB41
C
C     LESE KONFIGURIERUNG VON PLATTE
C
 4100 CONTINUE
      CALL DREAD(IUNKO,1,IERR,LENMAX)
      IF(IERR.EQ.1) GO TO 9940
      CLOSE(IUNKO)
      IONLFL=0
      GO TO 4000
C
C     LESE DATENSATZ AUF PLATTE
C
 4200 CONTINUE
      CALL DREAD(IUNDT,2,IERR,LENMAX)
      IF(IERR.EQ.1) GO TO 9940
      CLOSE(IUNDT)
      GO TO 4000
C
C     SCHREIBE KONFIGURIERUNG AUF PLATTE
C
 4300 CONTINUE
      CALL DWRITE(IUNKO,1,IERR,LENMAX)
      IF(IERR.EQ.1) GO TO 9940
      CLOSE(IUNKO)
      GO TO 4000
C
C     SCHREIBE DATENSATZ AUF PLATTE
C
 4400 CONTINUE
      CALL DWRITE(IUNDT,2,IERR,LENMAX)
      IF(IERR.EQ.1) GO TO 9940 
      CLOSE(IUNDT)
      GO TO 4000
C
C     LESE HTMS-DATEN
C
 4500 CONTINUE
      CALL SINPUT(KANAL,' Name des Zielkanals fuer Messdaten:')
      CALL SRNPUT(PMIN(KANAL),' Untere Bereichsgrenze:')       
      CALL SRNPUT(PMAX(KANAL),' Obere Bereichsgrenze:')
      IF(KANAL.LE.NADMAX)CALLSINPUT(ISCALE(KANAL),' Skalierungsfaktor:')
      CALL SINPUT(ISHIF,' Verschiebungsfaktor (0-100):')
      CALL SINPUT(IDEZI,' Dezimierungsfaktor (1-10):')
      IF(IDEZI.LE.1) IDEZI=1
      READ(IUND,4510,ERR=9940,END=9940) TBUF
      READ(IUND,4510,ERR=9940,END=9940) TBUF
 4510 FORMAT(A1)
      RMAXI=PMAX(KANAL)
      RMINI=PMIN(KANAL)
      RDIF=RMAXI-RMINI
      IF(KANAL.LE.NADMAX) THEN
         IF(ISCALE(KANAL).GT.0) RDIF=RDIF*ISCALE(KANAL)           
         IF(ISCALE(KANAL).LT.0) RDIF=RDIF/IABS(ISCALE(KANAL))
      END IF
      IF(ISHIF.GT.0) THEN
         DO 4515 I=1,ISHIF
            READ(IUND,4520,ERR=9940,END=4550) J,W1,W2
 4515    CONTINUE
      END IF  
      DO 4540 I=1,LEN
         SUMME=0.
         DO 4530 K=1,IDEZI
            READ(IUND,4520,ERR=9940,END=4550) J,W1,W2
 4520       FORMAT(I6,E14.5,E14.5)
         SUMME=SUMME+W2
 4530    CONTINUE
         W2=SUMME/FLOAT(IDEZI)
         IF(KANAL.LE.NADMAX) THEN
            IDAT(KANAL,I)=NINT(((W2-RMINI)/RDIF)*(FLOAT(IADMAX-IADMIN)))
         ELSE
            IDAT(KANAL,I)=NINT(((W2-RMINI)/RDIF)*(FLOAT(IDAMAX-IDAMIN)))
         END IF
 4540 CONTINUE
      CLOSE(IUND)
      GO TO 4000
 4550 CONTINUE
      WRITE(*,4560) I 
 4560 FORMAT(' Datenende vorzeitig erreicht bei Z{hler ',I5)
      CLOSE(IUND)
      GO TO 4000
C
C     START DER WANDLERPERIPHERIE
C
C 
C     RESET DES FLAGS UND DES INTERRUPT-PAUSEN-ZAEHLERS
C
 5000 CONTINUE
C
C     RESET DER COUNTER 
C
      IZ=0
      IZMOLD=0
      NPOINT=0
      ICNT=0
C
C     INITIALISIERE UHR
C
      IF(IONLFL.EQ.1) CALL INTUHC
      MASK=4 
      IF(ITIME.EQ.1) MASK=2
      IF(ITIME.EQ.10) MASK=4
      IF(ITIME.EQ.600) MASK=8
      CALL INTUHO(MASK)  
      IONLFL=1
      GO TO 100
C
C     SCHLIESSEN DER WANDLERPERIPHERIE
C
 6000 CONTINUE
      IF(IONLFL.EQ.1) CALL INTUHC 
      IONLFL=0
      GO TO 100
C     
C     ZYKLISCHE MESSDATENAUSGABE
C
 7000 CONTINUE
      IF(IONLFL.NE.1) GO TO 9920
C
C     
C     BEGINN DER WARTESCHLEIFE
C     ONLINE-AUSWERTUNG
C
      NADDA=NADU+NDAU
      NADU1=NADU+1
      IZSAVE=IZ
 7040 CONTINUE
      IF(IZ.EQ.IZSAVE) GO TO 7040
      IF(IRAWIO().NE.0) GO TO 100
      NPSAVE=NPOINT
      IZSAVE=IZ
      IF(NADU.GE.1) THEN
         DO 7050 I=1,NADU
            V(I)=10.*FLOAT(IDAT(I,NPSAVE))/FLOAT(IADMAX-IADMIN)
 7050    CONTINUE
      END IF
      IF(NDAU.GE.1) THEN
         DO 7070 I=NADU1,NADDA
            I1=I+NADMAX-NADU
            V(I)=10.*FLOAT(IDAT(I1,NPSAVE))/FLOAT(IDAMAX-IDAMIN)
 7070    CONTINUE
      END IF
      WRITE(*,7100) IZSAVE,(V(I),I=1,NADDA)
 7100 FORMAT(' Schritt:',I5,' Spg. (V):',10F7.3)
      GO TO 7040                         
C
C     GRAFIKAUSGABE
C
 8000 CONTINUE
      CALL XGRAPH
      GO TO 100
C
C     IDENTIFIKATION
C
 9000 CONTINUE
      CALL XIDEN
      GO TO 100
C
C     PROGRAMMENDE
C
 9910 CONTINUE
      WRITE(*,9915)
 9915 FORMAT(' **Fehlermeldung: Keine DA-Wandler angemeldet** ')
      GO TO 100
 9920 CONTINUE
      WRITE(*,9925)
 9925 FORMAT(' **Fehlermeldung: Interrupt-Routinen nicht gestartet** ')
      GO TO 100
 9940 CONTINUE
      WRITE(*,9945)
 9945 FORMAT(' **Fehlermeldung: Disketten-Lese oder Schreibfehler** ') 
      GO TO 100
 9999 CONTINUE
      STOP 
      END
C
C
C
      SUBROUTINE SINPUT(IWERT,AUSGAB)
C
C     ZWECK:EINGABE EINES INTEGER-WERTES
C
C     EINGABEPARAMETER:
C     IWERT  - BISHERIGER WERT
C     AUSGAB - TEXT ZUR AUSGABE AUF DEM BILDSCHIRM
C
C     AUSGABEPARAMETER:
C     IWERT  - AKTUALISIERTER WERT
C
C     ROETHER/7/86
C
      CHARACTER*1 AUSGAB(80)
      DO 10 I=1,80
      IF(AUSGAB(I).EQ.':') GO TO 20
 10   CONTINUE
      RETURN
 20   NEND=I 
 25   CONTINUE
      WRITE(*,30) (AUSGAB(I),I=1,NEND)       
 30   FORMAT(80A1)
      WRITE(*,40) IWERT
 40   FORMAT(/' Bisheriger Wert:',I5,'   Neuer Wert:')  
      READ(*,*,ERR=25) IWERT
      WRITE(*,50) IWERT    
 50   FORMAT(/' Gelesen:',I5/)
      RETURN
      END
C
C
C
      SUBROUTINE SRNPUT(RWERT,AUSGAB)
C
C     ZWECK:
C     EINGABE EINES REAL-WERTES
C
C     EINGABEPARAMETER:
C     RWERT  - BISHERIGER WERT
C     AUSGAB - TEXT ZUR AUSGABE AUF DEM BILDSCHIRM
C
C     AUSGABEPARAMETER:
C     RWERT  - AKTUALISIERTER WERT
C
C     ROETHER/7/86
C
      CHARACTER*1 AUSGAB(80)
      DO 10 I=1,80
      IF(AUSGAB(I).EQ.':') GO TO 20
 10   CONTINUE
      RETURN
 20   NEND=I
 25   CONTINUE
      WRITE(*,30) (AUSGAB(I),I=1,NEND)
 30   FORMAT(80A1)
      WRITE(*,40) RWERT
 40   FORMAT(/' Bisheriger Wert:',F9.3,'  Neuer Wert:')
      READ(*,*,ERR=25) RWERT
      WRITE(*,50) RWERT
 50   FORMAT(/' Gelesen:',F9.3/)
      RETURN
      END
C
C
C
      SUBROUTINE DREAD(IUN,JOB,IERR,LEN)
      INTEGER IV1(56),IV2(9),IV3(10,1000),IV4(432)
      CHARACTER*20 T(10)
      COMMON/KONFIG/IV1        
      COMMON/STATUS/IV2    
      COMMON/ROHDAT/IV3       
      COMMON/PTEXTE/T
      COMMON/MODELL/IV4
C
C     
C
      IERR=0
      IF(JOB.EQ.1) THEN
        READ(IUN,ERR=1000)(IV1(I),I=1,56),(IV2(I),I=1,9),(T(I),I=1,10),
     *  (IV4(I),I=1,432)
      ELSE
        READ(IUN,ERR=1000)(IV1(I),I=1,56),(IV2(I),I=1,9),(T(I),I=1,10),
     *  (IV4(I),I=1,432),((IV3(I,J),I=1,10),J=1,LEN)     
      END IF
      GO TO 9999
 1000 IERR=1
 9999 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE DWRITE(IUN,JOB,IERR,LEN)
      INTEGER IV1(56),IV2(9),IV3(10,1000),IV4(432)
      CHARACTER*20 T(10)
      COMMON/KONFIG/IV1     
      COMMON/STATUS/IV2    
      COMMON/ROHDAT/IV3        
      COMMON/PTEXTE/T
      COMMON/MODELL/IV4
C
C
C
      IERR=0
      IF(JOB.EQ.1) THEN
        WRITE(IUN,ERR=1000)(IV1(I),I=1,56),(IV2(I),I=1,9),(T(I),I=1,10),
     *  (IV4(I),I=1,432)
      ELSE
        WRITE(IUN,ERR=1000)(IV1(I),I=1,56),(IV2(I),I=1,9),(T(I),I=1,10),
     *  (IV4(I),I=1,432),((IV3(I,J),I=1,10),J=1,LEN)
      END IF
      GO TO 9999
 1000 IERR=1
 9999 CONTINUE
      RETURN  
      END
