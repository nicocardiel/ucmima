C------------------------------------------------------------------------------
C Version 26-September-2022                                      file: ucmima.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This program is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
C
C Este programa lee imagenes FITS generadas por la camara Pictor 216 del
C Departamento de Astrofisica de la Universidad Complutense de Madrid.
C
C------------------------------------------------------------------------------
C
        PROGRAM UCMIMA
        IMPLICIT NONE
C
        CHARACTER*255 READC
        INTEGER READILIM
        REAL READF
        INTEGER TRUEBEG,TRUELEN
C Si se cambia algun parametro, verificar si aparece tambien en alguna rutina
        INTEGER MAXNAXIS
        PARAMETER(MAXNAXIS=2)                !valor maximo admisible para NAXIS
        INCLUDE 'dimensions.inc'
C
        INTEGER I,J
        INTEGER NB,NBLOCAL
        INTEGER NAXIS(0:MAXNAXIS)                               !NAXIS=NAXIS(0)
        INTEGER NAXISBUFF(0:MAXNAXIS)                           !NAXIS=NAXIS(0)
        INTEGER NS1,NS2,NC1,NC2
        INTEGER MNS1,MNS2,MNC1,MNC2
        INTEGER L1,L2,IXC1,IXC2,IYC1,IYC2
        INTEGER LNC1,LNC2,LNS1,LNS2
        INTEGER LADO                         !tama\~{n}o del cuadrado de medida
        INTEGER ILUT
        INTEGER JUST
        REAL DATAMIN,DATAMAX
        REAL DATAMINBUFF,DATAMAXBUFF
        REAL IMAGEN(NXMAX,NYMAX)
        REAL IMAGENBUFF(NXMAX,NYMAX)
        REAL XC,YC,FCTE
        REAL XMIN,XMAX,YMIN,YMAX
        REAL BG,FG,TR(6)
        REAL FMEAN,FSIGMA,FMEDIAN
        CHARACTER*1 CH,COUT,COPER,CLUT
        CHARACTER*5 CNC1,CNC2,CNS1,CNS2,CLADO
        CHARACTER*6 CJUST
        CHARACTER*80 FITSFILE,FITSFILEBUFF,CDUMMY
        LOGICAL LFIRSTIMAGE
        LOGICAL LBEXIST
        LOGICAL LZOOM,LMEDIR
        LOGICAL LNULL(NXMAX,NYMAX),ANYNULL,ANYNULLBUFF
C
        EXTERNAL SYSTEMFUNCTION
        INTEGER SYSTEMFUNCTION
        INTEGER ISYSTEM
C
        COMMON/BLKIMAGEN/IMAGEN
        COMMON/BLKLNULL/LNULL,ANYNULL
        COMMON/BLKDATMINMAX/DATAMIN,DATAMAX
        COMMON/BLKNAXIS/NAXIS
        COMMON/BLKFITSFILE/FITSFILE
        COMMON/BLKESTADISTICA/FMEAN,FSIGMA,FMEDIAN
C------------------------------------------------------------------------------
        TR(1)=0.
        TR(2)=1.
        TR(3)=0.
        TR(4)=0.
        TR(5)=0.
        TR(6)=1.
C
        JUST=0
        IF(JUST.EQ.0)THEN
          CJUST='JUST=0'
        ELSE
          CJUST='JUST=1'
        END IF
C
        LFIRSTIMAGE=.TRUE.           !la proxima imagen es la primera en leerse
        LZOOM=.FALSE.                    !indica si la imagen actual es un zoom
C
        ILUT=3
        CLUT='3'
        LADO=21
        CLADO='21'
        COPER=' '
C------------------------------------------------------------------------------
C Abrimos el dispositivo grafico
        CALL RPGBEGOK('/XSERVE',0)
ccc     CALL RPGBEGOK('/ps',1)
        CALL PGSCF(1)
        CALL PGSCR(2,1.0,0.5,0.5)
        CALL PGSCR(3,0.6,1.0,0.6)
        CALL PGSCR(4,0.4,0.4,1.0)
        CALL PGSCR(5,0.6,1.0,1.0)
        CALL PGSCR(7,1.0,1.0,0.4)
        CALL PALETTE(ILUT)
        CALL BUTTSBR(0.00,1.000,0.05,0.95)
        CALL BUTTSPR(0.04,0.625,0.09,0.75)
        CALL BUTTSXB(8)
        CALL BUTTSYB(15)
C------------------------------------------------------------------------------
C Dibujamos botones y texto sobre botones:
        CALL PGSVP(0.0,1.0,0.0,1.0)
        CALL PGSWIN(0.0,1.0,0.0,1.0)
        CALL BIGTEXT(1,1)
        CALL BIGTEXT(1,2)
        CALL BIGTEXT(1,3)
        CALL BIGTEXT(1,4)
        CALL BIGTEXT(1,5)
C
C Fichero y Salir
        CALL BUTTON( 1,'[l]oad',     0)
        CALL BUTTON( 2,'[s]ave', 0)
        CALL BUTTON( 2,'[s]ave', 3)
        CALL BUTTON( 9,'e[x]it',    0)
C Zoom y restaurar
        CALL BUTTON( 3,'[z]oom',  0)
        CALL BUTTON( 3,'[z]oom',  3)
        CALL BUTTON(11,'[w]hole',0)
        CALL BUTTON(11,'[w]hole',3)
C LUT
        CALL BUTTON( 4,'[l]ut '//CLUT, 0)
        CALL BUTTON( 4,'[l]ut '//CLUT, 3)
C JUST
        CALL BUTTON(12,CJUST,  0)
C Medir
        CALL BUTTON( 5,'[m]easure', 0)
        CALL BUTTON( 5,'[m]easure', 3)
        CALL BUTTON(13,CLADO,  0)
        CALL BUTTON(13,CLADO,  3)
C Calculadora
        CALL BUTTON( 6,'+',        0)
        CALL BUTTON( 7,'-',        0)
        CALL BUTTON( 8,'image',   0)
        CALL BUTTON(14,'/',        0)
        CALL BUTTON(15,'x',        0)
        CALL BUTTON(16,'constant',0)
        CALL BUTTON( 6,'+',        3)
        CALL BUTTON( 7,'-',        3)
        CALL BUTTON( 8,'image',   3)
        CALL BUTTON(14,'/',        3)
        CALL BUTTON(15,'x',        3)
        CALL BUTTON(16,'constant',3)
C Background, Foreground, MINMAX
        CALL BUTTON(70,'       BG:',-6)
        CALL BUTTON(78,'       FG:',-6)
        CALL BUTTSCH(0.8)
        CALL BUTTON(71,'Background',0)
        CALL BUTTON(71,'Background',3)
        CALL BUTTON(79,'Foreground',0)
        CALL BUTTON(79,'Foreground',3)
        CALL BUTTSCH(1.0)
        CALL BUTTON(72,'Min[,]Max',  0)
        CALL BUTTON(72,'Min[,]Max',  3)
        CALL BUTTON(80,'[h]istog.',0)
        CALL BUTTON(80,'[h]istog.',3)
C------------------------------------------------------------------------------
C mensaje indicando como comenzar a trabajar
        CALL HELPTEXT('Select "Load" to read a FITS image, '//
     +   '"EXIT" to finish the program')
C------------------------------------------------------------------------------
C inicialmente mostramos la caja de imagen vacia
        CALL RPGENV(0.,1.,0.,1.,JUST,-2)
        CALL PGBOX('BC',0.0,0,'BC',0.0,0)
        CALL SHOWFITSFILE('NO IMAGE LOADED',DATAMIN,DATAMAX)
C------------------------------------------------------------------------------
C comprobamos si se ha pulsado un boton
20      CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
        CALL IFBUTTON(XC,YC,NB)
C buscamos aceleradores
        IF(CH.EQ.'l')THEN
          NBLOCAL=1
        ELSEIF(CH.EQ.'s')THEN
          NBLOCAL=2
        ELSEIF(CH.EQ.'z')THEN
          NBLOCAL=3
        ELSEIF(CH.EQ.'u')THEN
          NBLOCAL=4
        ELSEIF(CH.EQ.'m')THEN
          NBLOCAL=5
        ELSEIF(CH.EQ.'x')THEN
          NBLOCAL=9
        ELSEIF(CH.EQ.'w')THEN
          NBLOCAL=11
        ELSEIF(CH.EQ.',')THEN
          NBLOCAL=72
        ELSEIF(CH.EQ.'h')THEN
          NBLOCAL=80
        ELSE
          NBLOCAL=0
        END IF
        IF((NBLOCAL.NE.0).AND.(CH.NE.' '))THEN
          CALL BUTTQEX(NBLOCAL,LBEXIST)
          IF(LBEXIST) NB=NBLOCAL
        END IF
C------------------------------------------------------------------------------
        IF(NB.EQ.0)THEN
          IXC1=NINT(XC)
          IYC1=NINT(YC)
          IF((IXC1.GE.NC1).AND.(IXC1.LE.NC2).AND.
     +       (IYC1.GE.NS1).AND.(IYC1.LE.NS2))THEN
            WRITE(CNC1,'(I5)') IXC1
            LNC1=TRUEBEG(CNC1)
            WRITE(CNS1,'(I5)') IYC1
            LNS1=TRUEBEG(CNS1)
            WRITE(CDUMMY,*) IMAGEN(IXC1,IYC1)
            L1=TRUEBEG(CDUMMY)
            L2=TRUELEN(CDUMMY)
            CDUMMY='Pixel X='//CNC1(LNC1:5)//', Y='//
     +       CNS1(LNS1:5)//', Flux='//CDUMMY(L1:L2)
            CALL HELPTEXT(CDUMMY(1:TRUELEN(CDUMMY)))
          END IF
C..............................................................................
        ELSEIF(NB.EQ.1)THEN
          CALL BUTTON(1,'[l]oad',5)
          CALL BIGTEXT(0,1)
          WRITE(*,200)
          WRITE(*,101) '* List of FITS files in current directory '//
     +     '(*.fts, *.fits):'
          ISYSTEM=SYSTEMFUNCTION('ls *.fts *.fits')
          WRITE(*,200)
          CALL HELPTEXT('Introduce the name of the file to be '//
     +     'loaded')
C cuando leemos una imagen, reseteamos la mascara de pixels no validos
          DO I=1,NYMAX
            DO J=1,NXMAX
              LNULL(J,I)=.FALSE.
            END DO
          END DO
          CALL LEEFITS
          IF(ANYNULL)THEN
            WRITE(*,101)'***************************************'
            WRITE(*,101)'* the image contains undefined pixels *'
            WRITE(*,101)'***************************************'
          END IF
          CALL HELPTEXT(' ')
C
          IF(LFIRSTIMAGE)THEN    !Si es la primera imagen, establecemos limites
            NC1=1
            NC2=NAXIS(1)
            NS1=1
            NS2=NAXIS(2)
          END IF
C
          IF(LFIRSTIMAGE)THEN    !Si es la primera imagen, establecemos limites
            CALL ESTADISTICA(NC1,NC2,NS1,NS2,0.,0.) !Estadis. en toda la imagen
            BG=FMEAN-FSIGMA
            IF(BG.LT.DATAMIN) BG=DATAMIN
            BG=ANINT(BG)
            FG=FMEAN+FSIGMA
            IF(FG.GT.DATAMAX) FG=DATAMAX
            FG=ANINT(FG)
            IF(BG.EQ.FG)THEN
              BG=BG-1.
              FG=FG+1.
            END IF
            CALL BUTTSCH(0.8)
            WRITE(CDUMMY,*) BG
            L1=TRUEBEG(CDUMMY)
            L2=TRUELEN(CDUMMY)
            CALL BUTTON(71,CDUMMY(L1:L2),0)
            WRITE(CDUMMY,*) FG
            L1=TRUEBEG(CDUMMY)
            L2=TRUELEN(CDUMMY)
            CALL BUTTON(79,CDUMMY(L1:L2),0)
            CALL BUTTSCH(1.0)
            XMIN=REAL(NC1)-0.5
            XMAX=REAL(NC2)+0.5
            YMIN=REAL(NS1)-0.5
            YMAX=REAL(NS2)+0.5
            CALL RPGERASW(0.00,0.64,0.045,0.80,0)
            CALL RPGENV(XMIN,XMAX,YMIN,YMAX,JUST,-2)
            CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          END IF
C
          CALL SHOWFITSFILE('Imagen: '//FITSFILE(1:TRUELEN(FITSFILE)),
     +     DATAMIN,DATAMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C
          IF(LFIRSTIMAGE)THEN          !la proxima ya no sera la primera imagen
            LFIRSTIMAGE=.FALSE.
            CALL BUTTON( 2,'[s]ave', 0)
            CALL BUTTON( 3,'[z]oom',  0)
            CALL BUTTON( 4,'[l]ut '//CLUT, 0)
            CALL BUTTON( 5,'[m]easure',    0)
            CALL BUTTON(11,'[w]hole',    0)
            CALL BUTTON(13,CLADO,      0)
            CALL BUTTON( 6,'+',        0)
            CALL BUTTON( 7,'-',        0)
            CALL BUTTON(14,'/',        0)
            CALL BUTTON(15,'x',        0)
            CALL BUTTON(72,'Min[,]Max',  0)
            CALL BUTTON(80,'[h]istog.',0)
          END IF
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
          CALL BIGTEXT(1,1)
C
          CALL BUTTON(1,'[l]oad',0)
C..............................................................................
        ELSEIF(NB.EQ.2)THEN
          CALL BUTTON( 2,'[s]ave', 5)
          CALL BIGTEXT(0,1)
          WRITE(*,200)
          WRITE(*,101) '* List of FITS files in current directory '//
     +     '(*.fts, *.fits):'
          ISYSTEM=SYSTEMFUNCTION('ls *.fts *.fits')
          WRITE(*,200)
          CALL HELPTEXT('Introduce the name of the file where the '//
     +     'current FITS image will be saved')
          CALL ESCFITS
          CALL SHOWFITSFILE('Image: '//FITSFILE(1:TRUELEN(FITSFILE)),
     +     DATAMIN,DATAMAX)
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
          CALL BIGTEXT(1,1)
          CALL BUTTON( 2,'[s]ave', 0)
C..............................................................................
        ELSEIF(NB.EQ.3)THEN
          CALL BUTTON( 3,'[z]oom',  5)
          CALL BIGTEXT(0,2)
          CALL HELPTEXT('Select the two corners of the rectangle '//
     +     'to be zoomed')
C
          CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
          IXC1=INT(XC+0.5)
          IF(IXC1.LT.NC1) IXC1=NC1
          IF(IXC1.GT.NC2) IXC1=NC2
          IYC1=INT(YC+0.5)
          IF(IYC1.LT.NS1) IYC1=NS1
          IF(IYC1.GT.NS2) IYC1=NS2
          WRITE(CDUMMY,*)IXC1,IYC1
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL HELPTEXT('Cursor location '//CDUMMY(L1:L2))
C
          CALL PGSCI(5)
          CALL RPGBAND(2,0,REAL(IXC1),REAL(IYC1),XC,YC,CH)
          CALL PGSCI(1)
          IXC2=INT(XC+0.5)
          IF(IXC2.LT.NC1) IXC2=NC1
          IF(IXC2.GT.NC2) IXC2=NC2
          IYC2=INT(YC+0.5)
          IF(IYC2.LT.NS1) IYC2=NS1
          IF(IYC2.GT.NS2) IYC2=NS2
C
          IF((IXC1.EQ.IXC2).AND.(IYC1.EQ.IYC2))THEN
            CALL HELPTEXT(
     +       'ERROR: invalid rectangle corners')
            CALL BUTTON( 3,'[z]oom',  0)
            GOTO 20
          END IF
C
          NC1=MIN0(IXC1,IXC2)
          NC2=MAX0(IXC1,IXC2)
          NS1=MIN0(IYC1,IYC2)
          NS2=MAX0(IYC1,IYC2)
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
C
          XMIN=REAL(NC1)-0.5
          XMAX=REAL(NC2)+0.5
          YMIN=REAL(NS1)-0.5
          YMAX=REAL(NS2)+0.5
          CALL RPGERASW(0.00,0.64,0.045,0.80,0)
          CALL RPGENV(XMIN,XMAX,YMIN,YMAX,JUST,-2)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL SHOWFITSFILE('Image: '//FITSFILE(1:TRUELEN(FITSFILE)),
     +     DATAMIN,DATAMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C
          WRITE(CNC1,'(I5)') NC1
          LNC1=TRUEBEG(CNC1)
          WRITE(CNC2,'(I5)') NC2
          LNC2=TRUEBEG(CNC2)
          WRITE(CNS1,'(I5)') NS1
          LNS1=TRUEBEG(CNS1)
          WRITE(CNS2,'(I5)') NS2
          LNS2=TRUEBEG(CNS2)
          CDUMMY='Zoom from X='//CNC1(LNC1:5)//', Y='//CNS1(LNS1:5)//
     +              ', to X='//CNC2(LNC2:5)//', Y='//CNS2(LNS2:5)//
     +              '.'
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL HELPTEXT(CDUMMY(L1:L2))
C
          LZOOM=.TRUE.
          CALL BUTTON(11,'[w]hole',0)
          CALL BIGTEXT(1,2)
          CALL BUTTON( 3,'[z]oom',  0)
C..............................................................................
        ELSEIF(NB.EQ.4)THEN
          CALL BUTTON( 4,'[l]ut '//CLUT,    5)
          CALL BIGTEXT(0,3)
          ILUT = ILUT + 1
          IF(ILUT.GT.5) ILUT = 1
          WRITE(CLUT,'(I1)') ILUT
          CALL PALETTE(ILUT)
C
          CALL RPGERASW(0.00,0.64,0.045,0.80,0)
          CALL RPGENV(XMIN,XMAX,YMIN,YMAX,JUST,-2)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL SHOWFITSFILE('Image: '//FITSFILE(1:TRUELEN(FITSFILE)),
     +     DATAMIN,DATAMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C
          CALL BIGTEXT(1,3)
          CALL BUTTON( 4,'[l]ut '//CLUT,    0)
C..............................................................................
        ELSEIF(NB.EQ.5)THEN
          CALL BUTTON( 5,'[m]easure',    5)
          CALL BIGTEXT(0,4)
          LMEDIR=.FALSE.                   !de momento aun no hemos medido nada
C desactivamos todos los demas botones
          CALL BUTTON( 1,'[l]oad',     3)
          CALL BUTTON( 2,'[s]ave', 3)
          CALL BUTTON( 9,'e[x]it',    3)
          CALL BUTTON( 3,'[z]oom',  3)
          CALL BUTTON( 4,'[l]ut '//CLUT,  3)
          CALL BUTTON(11,'[w]hole',3)
          CALL BUTTON(13,CLADO      ,3)
          CALL BUTTON( 6,'+',        3)
          CALL BUTTON( 7,'-',        3)
          CALL BUTTON(14,'/',        3)
          CALL BUTTON(15,'x',        3)
          CALL BUTTON(72,'Min[,]Max',  3)
          CALL BUTTON(80,'[h]istog.',3)
          CALL BUTTSCH(0.8)
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(71,CDUMMY(L1:L2),3)
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(79,CDUMMY(L1:L2),3)
          CALL BUTTSCH(1.0)
C medimos
          CALL HELPTEXT('MOUSE: measure with left button,'//
     +     ' finish with right buttton')
50        CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
          IF(CH.EQ.'X') GOTO 52
          IXC1=NINT(XC)
          IYC1=NINT(YC)
          IF((IXC1.GE.NC1).AND.(IXC1.LE.NC2).AND.
     +       (IYC1.GE.NS1).AND.(IYC1.LE.NS2))THEN
            WRITE(CNC1,'(I5)') IXC1
            LNC1=TRUEBEG(CNC1)
            WRITE(CNS1,'(I5)') IYC1
            LNS1=TRUEBEG(CNS1)
            WRITE(CDUMMY,*) IMAGEN(IXC1,IYC1)
            L1=TRUEBEG(CDUMMY)
            L2=TRUELEN(CDUMMY)
            CDUMMY='Central pixel: X='//
     +       CNC1(LNC1:5)//', Y='//CNS1(LNS1:5)//
     +       ', Flux='//CDUMMY(L1:L2)
            CALL HELPTEXT(CDUMMY(1:TRUELEN(CDUMMY)))
            MNC1=IXC1-LADO/2
            MNC2=IXC1+LADO/2
            MNS1=IYC1-LADO/2
            MNS2=IYC1+LADO/2
            IF((MNC1.GE.NC1).AND.(MNC2.LE.NC2).AND.
     +         (MNS1.GE.NS1).AND.(MNS2.LE.NS2))THEN
              LMEDIR=.TRUE.
              CALL PGSCI(7)
              CALL PGMOVE(REAL(MNC1),REAL(MNS1))
              CALL PGDRAW(REAL(MNC1),REAL(MNS2))
              CALL PGDRAW(REAL(MNC2),REAL(MNS2))
              CALL PGDRAW(REAL(MNC2),REAL(MNS1))
              CALL PGDRAW(REAL(MNC1),REAL(MNS1))
              CALL PGSCI(1)
              CALL ESTADISTICA(MNC1,MNC2,MNS1,MNS2,BG,FG)
              CALL PGIMAGSMALL(MNC1,MNC2,MNS1,MNS2,FG,BG,1)
            ELSE
            END IF
          END IF
          GOTO 50
52        IF(LMEDIR)THEN     !recuperamos la imagen completa con region de zoom
            CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          END IF
C reactivamos todos los demas botones
          CALL BUTTON( 1,'[l]oad',     0)
          CALL BUTTON( 2,'[s]ave', 0)
          CALL BUTTON( 9,'e[x]it',    0)
          CALL BUTTON( 3,'[z]oom',  0)
          CALL BUTTON( 4,'[l]ut '//CLUT,  0)
          IF((LZOOM).OR.(LMEDIR)) CALL BUTTON(11,'[w]hole',0)
          CALL BUTTON(13,CLADO      ,0)
          CALL BUTTON( 6,'+',        0)
          CALL BUTTON( 7,'-',        0)
          CALL BUTTON(14,'/',        0)
          CALL BUTTON(15,'x',        0)
          CALL BUTTON(72,'Min[,]Max',  0)
          CALL BUTTON(80,'[h]istog.',0)
          CALL BUTTSCH(0.8)
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(71,CDUMMY(L1:L2),0)
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(79,CDUMMY(L1:L2),0)
          CALL BUTTSCH(1.0)
C
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
          CALL BIGTEXT(1,4)
          CALL BUTTON( 5,'[m]easure',    0)
C..............................................................................
        ELSEIF((NB.EQ.6).OR.(NB.EQ.7).OR.(NB.EQ.14).OR.(NB.EQ.15))THEN
          IF(COPER.NE.' ') GOTO 20
          CALL BIGTEXT(0,5)
          IF(NB.EQ. 6)THEN
            CALL BUTTON( 6,'+',        5)
            COPER='+'
          ELSEIF(NB.EQ. 7)THEN
            CALL BUTTON( 7,'-',        5)
            COPER='-'
          ELSEIF(NB.EQ.14)THEN
            CALL BUTTON(14,'/',        5)
            COPER='/'
          ELSEIF(NB.EQ.15)THEN
            CALL BUTTON(15,'x',        5)
            COPER='x'
          END IF
C se activan solo estos dos
          CALL BUTTON( 8,'image',   0)
          CALL BUTTON(16,'constant',0)
C desactivamos todos los demas botones
          CALL BUTTON( 1,'[l]oad',     3)
          CALL BUTTON( 2,'[s]ave', 3)
          CALL BUTTON( 9,'e[x]it',    3)
          CALL BUTTON( 3,'[z]oom',  3)
          CALL BUTTON( 4,'[l]ut '//CLUT,  3)
          CALL BUTTON( 5,'[m]easure',    3)
          CALL BUTTON(11,'[w]hole',3)
          CALL BUTTON(13,CLADO      ,3)
          IF(COPER.NE.'+') CALL BUTTON( 6,'+',        3)
          IF(COPER.NE.'-') CALL BUTTON( 7,'-',        3)
          IF(COPER.NE.'/') CALL BUTTON(14,'/',        3)
          IF(COPER.NE.'x') CALL BUTTON(15,'x',        3)
          CALL BUTTON(72,'Min[,]Max',  3)
          CALL BUTTON(80,'[h]istog.',3)
          CALL BUTTSCH(0.8)
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(71,CDUMMY(L1:L2),3)
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(79,CDUMMY(L1:L2),3)
          CALL BUTTSCH(1.0)
C pedimos si se va a utilizar una segunda imagen o una constante
          CALL HELPTEXT('Select whether the operation is going '//
     +     'to use an additional image or a constant')
C..............................................................................
        ELSEIF((NB.EQ.8).OR.(NB.EQ.16))THEN
          IF(NB.EQ.8)THEN
            CALL BUTTON( 8,'image',   5)
            CALL HELPTEXT('Introduce the name of the additional '//
     +       'image in the console')
            DO I=1,NAXIS(2)
              DO J=1,NAXIS(1)
                IMAGENBUFF(J,I)=IMAGEN(J,I)
              END DO
            END DO
            NAXISBUFF(1)=NAXIS(1)
            NAXISBUFF(2)=NAXIS(2)
            DATAMINBUFF=DATAMIN
            DATAMAXBUFF=DATAMAX
            FITSFILEBUFF=FITSFILE
            ANYNULLBUFF=ANYNULL
C en este caso no reseteamos la mascara de pixels no validos porque queremos
C mantener la mascara actual (a la que se a\~{n}adiran los pixels no validos
C de la imagen que vamos a leer a continuacion)
            CALL LEEFITS
            IF(ANYNULL)THEN
              WRITE(*,101)'***************************************'
              WRITE(*,101)'* the image contains undefined pixels *'
              WRITE(*,101)'***************************************'
            END IF
            FITSFILE=FITSFILEBUFF               !restauramos el nombre original
            IF((NAXIS(1).NE.NAXISBUFF(1)).OR.
     +         (NAXIS(2).NE.NAXISBUFF(2)))THEN
              WRITE(*,101)'ERROR: the additional image has different '//
     +         'dimensions.'
              WRITE(*,*)
              WRITE(*,100)'Press <RETURN> to continue...'
              READ(*,*)
              NAXIS(1)=NAXISBUFF(1)
              NAXIS(2)=NAXISBUFF(2)
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGENBUFF(J,I)
                END DO
              END DO
              DATAMIN=DATAMINBUFF
              DATAMAX=DATAMAXBUFF
              ANYNULL=ANYNULLBUFF
              GOTO 82
            END IF
            IF(ANYNULL)THEN
            ELSE
              ANYNULL=ANYNULLBUFF
            END IF
            IF(COPER.EQ.'+')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGENBUFF(J,I)+IMAGEN(J,I)
                END DO
              END DO
            ELSEIF(COPER.EQ.'-')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGENBUFF(J,I)-IMAGEN(J,I)
                END DO
              END DO
            ELSEIF(COPER.EQ.'/')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IF(IMAGEN(J,I).EQ.0.0)THEN
                    LNULL(J,I)=.TRUE.
ccc                 WRITE(*,101)'ERROR: division por cero. '//
ccc     +               'Operacion abortada.'
ccc                 WRITE(*,*)
ccc                 WRITE(*,100)'Pulsar <RETURN> para continuar...'
ccc                 READ(*,*)
ccc                 DO II=1,NAXIS(2)
ccc                   DO JJ=1,NAXIS(1)
ccc                     IMAGEN(JJ,II)=IMAGENBUFF(JJ,II)
ccc                   END DO
ccc                 END DO
ccc                 DATAMIN=DATAMINBUFF
ccc                 DATAMAX=DATAMAXBUFF
ccc                 GOTO 82
                  END IF
                END DO
              END DO
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGENBUFF(J,I)/IMAGEN(J,I)
                END DO
              END DO
            ELSEIF(COPER.EQ.'x')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGENBUFF(J,I)*IMAGEN(J,I)
                END DO
              END DO
            END IF
          ELSEIF(NB.EQ.16)THEN
            CALL BUTTON(16,'constant',5)
            CALL HELPTEXT('Introduce constant value in console')
80          FCTE=READF('Constant','@')
            IF((COPER.EQ.'/').AND.(FCTE.EQ.0.0))THEN
              WRITE(*,101)'ERROR: division by cero avoided!'
              WRITE(*,101)'Introduce a new constant value:'
              GOTO 80
            END IF
            IF(COPER.EQ.'+')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGEN(J,I)+FCTE
                END DO
              END DO
            ELSEIF(COPER.EQ.'-')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGEN(J,I)-FCTE
                END DO
              END DO
            ELSEIF(COPER.EQ.'/')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGEN(J,I)/FCTE
                END DO
              END DO
            ELSEIF(COPER.EQ.'x')THEN
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IMAGEN(J,I)=IMAGEN(J,I)*FCTE
                END DO
              END DO
            END IF
          END IF
C
          DATAMIN=1.E30
          DATAMAX=-1.E30
          DO I=1,NAXIS(2)
            DO J=1,NAXIS(1)
              IF(.NOT.LNULL(J,I))THEN
                IF(IMAGEN(J,I).LT.DATAMIN) DATAMIN=IMAGEN(J,I)
                IF(IMAGEN(J,I).GT.DATAMAX) DATAMAX=IMAGEN(J,I)
              END IF
            END DO
          END DO
          L2=TRUELEN(FITSFILE)
          IF(FITSFILE(L2-2:L2).EQ.'...')THEN
            FITSFILE=FITSFILE(1:L2)//'.'
          ELSE
            FITSFILE=FITSFILE(1:L2)//COPER//'...'
          END IF
C redibujamos el resultado
82        CONTINUE
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,0.,0.)  !Estad. con todos los pixels
          BG=FMEAN-FSIGMA
          IF(BG.LT.DATAMIN) BG=DATAMIN
          BG=ANINT(BG)
          FG=FMEAN+FSIGMA
          IF(FG.GT.DATAMAX) FG=DATAMAX
          FG=ANINT(FG)
          IF(BG.EQ.FG)THEN
            BG=BG-1.
            FG=FG+1.
          END IF
          CALL BUTTSCH(0.8)
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(71,CDUMMY(L1:L2),0)
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(79,CDUMMY(L1:L2),0)
          CALL BUTTSCH(1.0)
          XMIN=REAL(NC1)-0.5
          XMAX=REAL(NC2)+0.5
          YMIN=REAL(NS1)-0.5
          YMAX=REAL(NS2)+0.5
          CALL RPGERASW(0.00,0.64,0.045,0.80,0)
          CALL RPGENV(XMIN,XMAX,YMIN,YMAX,JUST,-2)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL SHOWFITSFILE('Image: '//FITSFILE(1:TRUELEN(FITSFILE)),
     +     DATAMIN,DATAMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C desactivamos estos dos seguro
          CALL BUTTON( 8,'image',   0)
          CALL BUTTON(16,'constant',0)
          CALL BUTTON( 8,'image',   3)
          CALL BUTTON(16,'constant',3)
C activamos los que estaban desactivados
          CALL BUTTON( 1,'[l]oad',     0)
          CALL BUTTON( 2,'[s]ave', 0)
          CALL BUTTON( 9,'e[x]it',    0)
          CALL BUTTON( 3,'[z]oom',  0)
          CALL BUTTON( 4,'[l]ut '//CLUT,  0)
          CALL BUTTON( 5,'[m]easure',    0)
          IF(LZOOM) CALL BUTTON(11,'[w]hole',0)
          CALL BUTTON(13,CLADO      ,0)
          CALL BUTTON( 6,'+',        0)
          CALL BUTTON( 7,'-',        0)
          CALL BUTTON(14,'/',        0)
          CALL BUTTON(15,'x',        0)
          CALL BUTTON(72,'Min[,]Max',  0)
          CALL BUTTON(80,'[h]istog.',0)
          COPER=' '         !evita repetir pulsar boton de operacion "apretado"
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
          CALL BIGTEXT(1,5)
C..............................................................................
        ELSEIF(NB.EQ.9)THEN
          CALL BUTTON(9,'e[x]it',5)
          CALL BIGTEXT(0,1)
          CALL HELPTEXT('This option ends the program execution')
          COUT=READC('Do you really want to exit (y/n)','n','yn')
          IF(COUT.EQ.'y') GOTO 90
          IF(LFIRSTIMAGE)THEN
            CALL HELPTEXT('Select "Load" to read '//
     +       'a FITS image, "EXIT" to finish the program')
          ELSE
            CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +       //' MEASURE, CALCULATOR or change image cuts')
          END IF
          CALL BIGTEXT(1,1)
          CALL BUTTON(9,'e[x]it',0)
C..............................................................................
        ELSEIF(NB.EQ.11)THEN
          CALL BUTTON(11,'[w]hole',5)
          CALL BIGTEXT(0,2)
C
          NC1=1
          NC2=NAXIS(1)
          NS1=1
          NS2=NAXIS(2)
          CALL HELPTEXT(' ')
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
C
          XMIN=REAL(NC1)-0.5
          XMAX=REAL(NC2)+0.5
          YMIN=REAL(NS1)-0.5
          YMAX=REAL(NS2)+0.5
          CALL RPGERASW(0.00,0.64,0.045,0.80,0)
          CALL RPGENV(XMIN,XMAX,YMIN,YMAX,JUST,-2)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL SHOWFITSFILE('Image: '//FITSFILE(1:TRUELEN(FITSFILE)),
     +     DATAMIN,DATAMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C
          LZOOM=.FALSE.
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
          CALL BIGTEXT(1,2)
          CALL BUTTON(11,'[w]hole',0)
!         CALL BUTTON(11,'[w]hole',3)
C..............................................................................
        ELSEIF(NB.EQ.12)THEN
          CALL BUTTON(12,CJUST,  5)
          IF(JUST.EQ.0)THEN
            JUST=1
            CJUST='JUST=1'
          ELSE
            JUST=0
            CJUST='JUST=0'
          END IF
          CALL RPGERASW(0.00,0.64,0.045,0.80,0)
          CALL RPGENV(XMIN,XMAX,YMIN,YMAX,JUST,-2)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL SHOWFITSFILE('Image: '//FITSFILE(1:TRUELEN(FITSFILE)),
     +     DATAMIN,DATAMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL BUTTON(12,CJUST,  0)
C..............................................................................
        ELSEIF(NB.EQ.13)THEN
          CALL BUTTON(13,CLADO,  5)
          CALL BIGTEXT(0,3)
          CALL HELPTEXT('This option allows to modify '//
     +     'the size of the measure square')
          LADO=READILIM('Side of the measure square (odd number)',
     +     CLADO,1,NAXIS(2))
          IF(MOD(LADO,2).EQ.0)THEN
            IF(LADO.EQ.NAXIS(2))THEN
              LADO=LADO-1
            ELSE
              LADO=LADO+1
            END IF
          END IF
          WRITE(CDUMMY,*)LADO
          CALL RMBLANK(CDUMMY,CDUMMY,L1)
          CLADO=CDUMMY(1:L1)
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
          CALL BIGTEXT(1,3)
          CALL BUTTON(13,CLADO,  0)
C..............................................................................
        ELSEIF(NB.EQ.71)THEN
          WRITE(CDUMMY,*) BG
          CALL HELPTEXT('This option allows to modify the image '//
     +     'background')
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTSCH(0.8)
          CALL BUTTON(71,CDUMMY(L1:L2),5)
          CALL BUTTSCH(1.0)
          BG=READF('Background',CDUMMY(L1:L2))
C
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTSCH(0.8)
          CALL BUTTON(71,CDUMMY(L1:L2),0)
          CALL BUTTSCH(1.0)
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
C..............................................................................
        ELSEIF(NB.EQ.72)THEN
          CALL BUTTON(72,'Min[,]Max',  5)
C
          BG=IMAGEN(NC1,NS1)
          FG=BG
          DO I=NS1,NS2
            DO J=NC1,NC2
              IF(IMAGEN(J,I).LT.BG) BG=IMAGEN(J,I)
              IF(IMAGEN(J,I).GT.FG) FG=IMAGEN(J,I)
            END DO
          END DO
C
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C
          CALL BUTTSCH(0.8)
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(71,CDUMMY(L1:L2),0)
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(79,CDUMMY(L1:L2),0)
          CALL BUTTSCH(1.0)
C
          CALL BUTTON(72,'Min[,]Max',  0)
C..............................................................................
        ELSEIF(NB.EQ.79)THEN
          WRITE(CDUMMY,*) FG
          CALL HELPTEXT('This option allows to modify the image '//
     +     'foreground')
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTSCH(0.8)
          CALL BUTTON(79,CDUMMY(L1:L2),5)
          CALL BUTTSCH(1.0)
          FG=READF('Foreground',CDUMMY(L1:L2))
C
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTSCH(0.8)
          CALL BUTTON(79,CDUMMY(L1:L2),0)
          CALL BUTTSCH(1.0)
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
C..............................................................................
        ELSEIF(NB.EQ.80)THEN
          CALL BUTTON(80,'[h]istog.',5)
          CALL HELPTEXT('Seleccionar histograma limits with the '//
     +     'help of the mouse')
C desactivamos todos los demas botones
          CALL BUTTON( 1,'[l]oad',     3)
          CALL BUTTON( 2,'[s]ave', 3)
          CALL BUTTON( 9,'e[x]it',    3)
          CALL BUTTON( 3,'[z]oom',  3)
          CALL BUTTON( 4,'[l]ut '//CLUT,  3)
          CALL BUTTON(11,'[w]hole',3)
          CALL BUTTON( 5,'[m]easure',    3)
          CALL BUTTON(13,CLADO      ,3)
          CALL BUTTON( 6,'+',        3)
          CALL BUTTON( 7,'-',        3)
          CALL BUTTON(14,'/',        3)
          CALL BUTTON(15,'x',        3)
          CALL BUTTON(72,'Min[,]Max',  3)
          CALL BUTTSCH(0.8)
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(71,CDUMMY(L1:L2),3)
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(79,CDUMMY(L1:L2),3)
          CALL BUTTSCH(1.0)
C seleccionamos region del histograma
          CALL ZOOMHISTOG(BG,FG)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          CALL PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,0)
          CALL ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
          CALL HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
C reactivamos todos los demas botones
          CALL BUTTON( 1,'[l]oad',     0)
          CALL BUTTON( 2,'[s]ave', 0)
          CALL BUTTON( 9,'e[x]it',    0)
          CALL BUTTON( 3,'[z]oom',  0)
          CALL BUTTON( 4,'[l]ut '//CLUT,  0)
          IF(LZOOM) CALL BUTTON(11,'[w]hole',0)
          CALL BUTTON( 5,'[m]easure',    0)
          CALL BUTTON(13,CLADO      ,0)
          CALL BUTTON( 6,'+',        0)
          CALL BUTTON( 7,'-',        0)
          CALL BUTTON(14,'/',        0)
          CALL BUTTON(15,'x',        0)
          CALL BUTTON(72,'Min[,]Max',  0)
          CALL BUTTSCH(0.8)
          WRITE(CDUMMY,*) BG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(71,CDUMMY(L1:L2),0)
          WRITE(CDUMMY,*) FG
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL BUTTON(79,CDUMMY(L1:L2),0)
          CALL BUTTSCH(1.0)
C
          CALL BUTTON(80,'[h]istog.',0)
          CALL HELPTEXT('Select option: FILE, ZOOM, LUT,'
     +     //' MEASURE, CALCULATOR or change image cuts')
C..............................................................................
        END IF
        GOTO 20
C------------------------------------------------------------------------------
90      CALL PGEND
C------------------------------------------------------------------------------
        STOP
C
100     FORMAT(A,$)
101     FORMAT(A)
200     FORMAT(79('-'))
        END
C
C******************************************************************************
C Muestra el texto de ayuda
        SUBROUTINE HELPTEXT(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER OLD_COLOR
        REAL XV1,XV2,YV1,YV2
        REAL XW1,XW2,YW1,YW2
C------------------------------------------------------------------------------
C almacenamos region de dibujo actual
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL PGQCI(OLD_COLOR)
C definimos la region de dibujo del texto de ayuda
        CALL PGSVP(0.0,1.0,0.0,1.0)
        CALL PGSWIN(0.0,1.0,0.0,1.0)
        CALL PGSCI(4)
        CALL PGRECT(0.00,1.00,0.005,0.045)
        CALL PGSCI(1)
        CALL PGPTEXT(0.02,0.015,0.0,0.0,CADENA)
C recuperamos region de dibujo inicial
        CALL PGSVP(XV1,XV2,YV1,YV2)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
        CALL PGSCI(OLD_COLOR)
        END
C
C******************************************************************************
C Escribe la etiqueta con el nombre del fichero
        SUBROUTINE SHOWFITSFILE(CADENA,DATAMIN,DATAMAX)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
        REAL DATAMIN,DATAMAX
C
        INTEGER         TRUEBEG,TRUELEN     !character manipulation
C
        INTEGER L1,L2
        REAL XV1,XV2,YV1,YV2
        REAL XW1,XW2,YW1,YW2
        CHARACTER*50 CDUMMY
C------------------------------------------------------------------------------
C almacenamos region de dibujo actual
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)
C definimos la region de dibujo del texto
        CALL PGSVP(0.0,1.0,0.0,1.0)
        CALL PGSWIN(0.0,1.0,0.0,1.0)
        CALL PGSCI(0)
        CALL PGRECT(XV1,XV2,YV2+0.01,0.82)
        CALL PGSCI(3)
        CALL PGPTEXT(0.5*(XV1+XV2),YV2+0.02,0.0,0.5,CADENA)
        IF(CADENA(1:21).EQ.'THERE IS NO IMAGE')THEN
          CALL PGSCI(1)
        ELSE
          CALL PGSCI(2)
          WRITE(CDUMMY,*) DATAMIN
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL PGPTEXT(XV1,YV2+0.02,0.0,0.0,CDUMMY(L1:L2))
          WRITE(CDUMMY,*) DATAMAX
          L1=TRUEBEG(CDUMMY)
          L2=TRUELEN(CDUMMY)
          CALL PGPTEXT(XV2,YV2+0.02,0.0,1.0,CDUMMY(L1:L2))
          CALL PGSCI(1)
        END IF
C recuperamos region de dibujo inicial
        CALL PGSVP(XV1,XV2,YV1,YV2)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
        END
C
C******************************************************************************
C Calcula estadistita del rectangulo definido por NC1,NC2,NS1,NS2, 
C considerando solo los pixels con se\~{n}al entre BG y FG, por un lado, y
C considerando todos los pixels, por otro (si BG=FG=0.0, entonces la
C estadistica se realiza solo una vez con todos los pixels de la imagen).

        SUBROUTINE ESTADISTICA(NC1,NC2,NS1,NS2,BG,FG)
        IMPLICIT NONE
        INTEGER NC1,NC2,NS1,NS2
        REAL BG,FG
C
        INTEGER         TRUEBEG,TRUELEN     !character manipulation
C
        INCLUDE 'dimensions.inc'
C
        INTEGER I,J,K
        INTEGER NPIX,NPIXBIS
        INTEGER L1,L2,L1BIS,L2BIS,DL,LS
        REAL IMAGEN(NXMAX,NYMAX),PIXEL(NXMAX*NYMAX)
        REAL FMEAN,FSIGMA,FMEDIAN
        REAL FMEANBIS,FSIGMABIS,FMEDIANBIS
        REAL FMEAN0,FMEDIAN1
        REAL XV1,XV2,YV1,YV2
        REAL XW1,XW2,YW1,YW2
        REAL OLD_CH
        CHARACTER*50 CDUMMY,CDUMMYBIS
        LOGICAL LNULL(NXMAX,NYMAX),ANYNULL
        LOGICAL ANYNULL1,ANYNULL2
C
        COMMON/BLKIMAGEN/IMAGEN
        COMMON/BLKLNULL/LNULL,ANYNULL
        COMMON/BLKESTADISTICA/FMEAN,FSIGMA,FMEDIAN
C------------------------------------------------------------------------------
        ANYNULL1=.FALSE.
        ANYNULL2=.FALSE.
        K=0
        DO I=NS1,NS2
          DO J=NC1,NC2
            IF((BG.EQ.0.0).AND.(FG.EQ.0.0))THEN
              IF(.NOT.LNULL(J,I))THEN
                K=K+1
                PIXEL(K)=IMAGEN(J,I)
              ELSE
                ANYNULL1=.TRUE.
              END IF
            ELSE
              IF(.NOT.LNULL(J,I))THEN
                IF((IMAGEN(J,I).GE.BG).AND.(IMAGEN(J,I).LE.FG))THEN
                  K=K+1
                  PIXEL(K)=IMAGEN(J,I)
                END IF
              ELSE
                ANYNULL1=.TRUE.
              END IF
            END IF
          END DO
        END DO
        NPIX=K
C------------------------------------------------------------------------------
        IF(NPIX.EQ.0)THEN
          FMEAN=0.
          FSIGMA=0.
          FMEDIAN=0.
        ELSE
          FMEAN=FMEAN0(NPIX,PIXEL,FSIGMA)
          FMEDIAN=FMEDIAN1(NPIX,PIXEL)
        END IF
C------------------------------------------------------------------------------
        IF((BG.EQ.0.0).AND.(FG.EQ.0.0))THEN
          FMEANBIS=FMEAN
          FSIGMABIS=FSIGMA
          FMEDIANBIS=FMEDIAN
          ANYNULL2=ANYNULL1
          NPIXBIS=NPIX
        ELSE
          K=0
          DO I=NS1,NS2
            DO J=NC1,NC2
              IF(.NOT.LNULL(J,I))THEN
                K=K+1
                PIXEL(K)=IMAGEN(J,I)
              ELSE
                ANYNULL2=.TRUE.
              END IF
            END DO
          END DO
          NPIXBIS=K
          FMEANBIS=FMEAN0(NPIXBIS,PIXEL,FSIGMABIS)
          FMEDIANBIS=FMEDIAN1(NPIXBIS,PIXEL)
        END IF
C------------------------------------------------------------------------------
C almacenamos region de dibujo actual
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL PGQCH(OLD_CH)
C------------------------------------------------------------------------------
C definimos nueva region de dibujo para mostrar resultado de la estadistica
        CALL PGSVP(0.00,1.00,0.00,1.00)
        CALL PGSWIN(0.00,1.00,0.00,1.00)
        CALL PGSCH(0.8)
C
        CALL PGSCI(14)
        CALL PGRECT(0.65,1.00,0.67,0.82)
        CALL PGSCI(1)
C
        CALL PGSCI(1)
        CALL PGPTEXT(0.87,0.80,0.0,1.0,'pixels in [BG,FG]')
        CALL PGPTEXT(0.99,0.80,0.0,1.0,'all pixels')
C
        CALL PGSCI(5)
        CALL PGPTEXT(0.66,0.77,0.0,0.0,'Npixels:')
        WRITE(CDUMMY,*) NPIX
        CALL RMBLANK(CDUMMY,CDUMMY,L1)
        CALL PGSCI(7)
        IF(ANYNULL1)THEN
          CALL PGPTEXT(0.87,0.77,0.0,1.0,'('//CDUMMY(1:L1)//')')
        ELSE
          CALL PGPTEXT(0.87,0.77,0.0,1.0,CDUMMY(1:L1))
        END IF
        WRITE(CDUMMY,*) NPIXBIS
        CALL RMBLANK(CDUMMY,CDUMMY,L1)
        CALL PGSCI(3)
        IF(ANYNULL2)THEN
          CALL PGPTEXT(0.99,0.77,0.0,1.0,'('//CDUMMY(1:L1)//')')
        ELSE
          CALL PGPTEXT(0.99,0.77,0.0,1.0,CDUMMY(1:L1))
        END IF
C
        CALL PGSCI(5)
        CALL PGPTEXT(0.66,0.74,0.0,0.0,'Mean')
        WRITE(CDUMMY,*) FMEAN
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        CALL PGSCI(7)
        CALL PGPTEXT(0.87,0.74,0.0,1.0,CDUMMY(L1:L2))
        WRITE(CDUMMYBIS,*) FMEANBIS
        L1BIS=TRUEBEG(CDUMMYBIS)
        L2BIS=TRUELEN(CDUMMYBIS)
        CALL PGSCI(3)
        CALL PGPTEXT(0.99,0.74,0.0,1.0,CDUMMYBIS(L1BIS:L2BIS))
C
        CALL PGSCI(5)
        CALL PGPTEXT(0.66,0.71,0.0,0.0,'Std.Dev.:')
        WRITE(CDUMMY,*) FSIGMA
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        CALL PGSCI(7)
        CALL PGPTEXT(0.87,0.71,0.0,1.0,CDUMMY(L1:L2))
        WRITE(CDUMMYBIS,*) FSIGMABIS
        L1BIS=TRUEBEG(CDUMMYBIS)
        L2BIS=TRUELEN(CDUMMYBIS)
        CALL PGSCI(3)
        CALL PGPTEXT(0.99,0.71,0.0,1.0,CDUMMYBIS(L1BIS:L2BIS))
C
        CALL PGSCI(5)
        CALL PGPTEXT(0.66,0.68,0.0,0.0,'Median:')
        WRITE(CDUMMY,*) FMEDIAN
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        CALL PGSCI(7)
        CALL PGPTEXT(0.87,0.68,0.0,1.0,CDUMMY(L1:L2))
        WRITE(CDUMMYBIS,*) FMEDIANBIS
        L1BIS=TRUEBEG(CDUMMYBIS)
        L2BIS=TRUELEN(CDUMMYBIS)
        CALL PGSCI(3)
        CALL PGPTEXT(0.99,0.68,0.0,1.0,CDUMMYBIS(L1BIS:L2BIS))
C
        CALL PGSCI(1)
        CALL PGSCH(OLD_CH)
C------------------------------------------------------------------------------
C mostramos la misma estadistica en la ventana de texto
        WRITE(*,101)'=========================================='
        WRITE(*,101)'           pixels in [BG,FG]    all pixels'
        WRITE(*,101)'           =================   ==========='
C
        WRITE(*,'(A14,4X,I10,4X,I10)') 'Npixels:      ',NPIX,NPIXBIS
C
        WRITE(*,100) 'Mean:         '
        WRITE(*,100) '   '
        WRITE(CDUMMY,*) FMEAN
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        DL=L2-L1+1
        IF(DL.LT.10)THEN
          DO LS=DL+1,10
            WRITE(*,100) ' '
          END DO
        END IF
        WRITE(*,100) CDUMMY(L1:L2)
        WRITE(CDUMMY,*) FMEANBIS
        WRITE(*,100) '   '
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        DL=L2-L1+1
        IF(DL.LT.10)THEN
          DO LS=DL+1,10
            WRITE(*,100) ' '
          END DO
        END IF
        WRITE(*,101) CDUMMY(L1:L2)
C
        WRITE(*,100) 'Std.Dev.:     '
        WRITE(*,100) '   '
        WRITE(CDUMMY,*) FSIGMA
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        DL=L2-L1+1
        IF(DL.LT.10)THEN
          DO LS=DL+1,10
            WRITE(*,100) ' '
          END DO
        END IF
        WRITE(*,100) CDUMMY(L1:L2)
        WRITE(CDUMMY,*) FSIGMABIS
        WRITE(*,100) '   '
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        DL=L2-L1+1
        IF(DL.LT.10)THEN
          DO LS=DL+1,10
            WRITE(*,100) ' '
          END DO
        END IF
        WRITE(*,101) CDUMMY(L1:L2)
C
        WRITE(*,100) 'Median:       '
        WRITE(*,100) '   '
        WRITE(CDUMMY,*) FMEDIAN
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        DL=L2-L1+1
        IF(DL.LT.10)THEN
          DO LS=DL+1,10
            WRITE(*,100) ' '
          END DO
        END IF
        WRITE(*,100) CDUMMY(L1:L2)
        WRITE(CDUMMY,*) FMEDIANBIS
        WRITE(*,100) '   '
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        DL=L2-L1+1
        IF(DL.LT.10)THEN
          DO LS=DL+1,10
            WRITE(*,100) ' '
          END DO
        END IF
        WRITE(*,101) CDUMMY(L1:L2)
        WRITE(*,101)'------------------------------------------'
C------------------------------------------------------------------------------
C recuperamos region de dibujo inicial
        CALL PGSVP(XV1,XV2,YV1,YV2)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C Dibuja un histograma de la se�al en el rectangulo definido por NC1,NC2,
C NS1,NS2, con limites en X entre BG y FG
        SUBROUTINE HISTOGRAMA(NC1,NC2,NS1,NS2,BG,FG)
        IMPLICIT NONE
C
        INTEGER NC1,NC2,NS1,NS2
        REAL BG,FG
C
        INCLUDE 'dimensions.inc'
        INTEGER NBINMAX
        PARAMETER(NBINMAX=100)                 !numero de bins en el histograma
C
        INTEGER I,J,K
        INTEGER NPIX(NBINMAX)
        INTEGER ISUM
        INTEGER NBIN
        REAL IMAGEN(NXMAX,NYMAX),DBIN,DBIN2
        REAL XPLOT(NBINMAX)
        REAL YGAUSS(NBINMAX),AMP
        REAL XV1,XV2,YV1,YV2
        REAL XW1,XW2,YW1,YW2
        REAL XMIN,XMAX,YMIN,YMAX,DY
        REAL FMEAN,FSIGMA,FMEDIAN
        REAL OLD_CH
C
        COMMON/BLKIMAGEN/IMAGEN
        COMMON/BLKHLIMITS/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKESTADISTICA/FMEAN,FSIGMA,FMEDIAN
C------------------------------------------------------------------------------
!       IF(INT(ABS(FG-BG)).LT.NBINMAX)THEN
!         NBIN=INT(ABS(FG-BG))
!         IF(NBIN.EQ.0) NBIN=1
!       ELSE
!         NBIN=NBINMAX
!       END IF
        NBIN=NBINMAX
C calculamos el histograma
        DO K=1,NBIN
          NPIX(K)=0
        END DO
        DBIN=(FG-BG)/REAL(NBIN)                            !anchura de cada bin
        DO K=1,NBIN
          XPLOT(K)=BG+REAL(K-1)*DBIN
        END DO
C
        DO I=NS1,NS2
          DO J=NC1,NC2
            K=NINT((IMAGEN(J,I)-BG)/DBIN)+1
            IF((K.GE.1).AND.(K.LE.NBIN)) NPIX(K)=NPIX(K)+1
          END DO
        END DO
C calculamos los limites para dibujar el histograma
        XMIN=BG
        XMAX=FG
C
        YMIN=0.
        YMAX=NPIX(1)
        DO K=2,NBIN
          IF(NPIX(K).GT.YMAX) YMAX=NPIX(K)
        END DO
        IF(YMAX.EQ.0.0) YMAX=1.0
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/20.
C------------------------------------------------------------------------------
C almacenamos region de dibujo actual
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL PGQCH(OLD_CH)
C------------------------------------------------------------------------------
C borramos histograma previo
        CALL RPGERASW(0.68,1.00,0.045,0.34,0)
C definimos nueva region de dibujo para el histograma
        CALL PGSVP(0.73,1.00,0.09,0.32)
        CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
        CALL PGSCH(0.7)
        CALL PGBOX('BCTSN',0.0,0,'BCTSN',0.0,0)
        CALL PGSCH(OLD_CH)
        CALL PGSCI(7)
        DO K=1,NBIN
          CALL PGMOVE(XPLOT(K)-0.5*DBIN,0.0)
          CALL PGDRAW(XPLOT(K)-0.5*DBIN,REAL(NPIX(K)))
          CALL PGDRAW(XPLOT(K)+0.5*DBIN,REAL(NPIX(K)))
          CALL PGDRAW(XPLOT(K)+0.5*DBIN,0.0)
        END DO
C dibujamos gaussiana
        IF(FSIGMA.GT.0.0)THEN
          ISUM=0
          DO K=1,NBIN
            ISUM=ISUM+NPIX(K)
          END DO
          AMP=REAL(ISUM*DBIN)/(FSIGMA*SQRT(2.*3.141593))
          DBIN2=(FG-BG)/REAL(NBINMAX)
          DO K=1,NBINMAX
            XPLOT(K)=BG+REAL(K-1)*DBIN2
          END DO
          DO K=1,NBINMAX
            YGAUSS(K)=AMP*EXP(-(XPLOT(K)-FMEAN)*(XPLOT(K)-FMEAN)/
     +       (2.*FSIGMA*FSIGMA))
          END DO
          CALL PGSCI(2)
          CALL PGLINE(NBINMAX,XPLOT,YGAUSS)
        END IF
        CALL PGSCI(1)
C------------------------------------------------------------------------------
C recuperamos region de dibujo inicial
        CALL PGSVP(XV1,XV2,YV1,YV2)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
        END
C
C******************************************************************************
C Hace un zoom en el histograma
        SUBROUTINE ZOOMHISTOG(BG,FG)
        IMPLICIT NONE
        REAL BG,FG
C
        INTEGER         TRUEBEG,TRUELEN     !character manipulation
C
        INTEGER L1,L2,NN1,NN2
        REAL XV1,XV2,YV1,YV2
        REAL XW1,XW2,YW1,YW2
        REAL XMIN,XMAX,YMIN,YMAX
        REAL XC,YC
        REAL XC1,XC2
        CHARACTER*1 CH
        CHARACTER*50 CDUMMY
C
        COMMON/BLKHLIMITS/XMIN,XMAX,YMIN,YMAX
C------------------------------------------------------------------------------
C almacenamos region de dibujo actual
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)
C------------------------------------------------------------------------------
C definimos la region de dibujo para el histograma
        CALL PGSVP(0.73,1.00,0.09,0.32)
        CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
C
        CALL HELPTEXT('Seleccionar histograma limits with the '//
     +   'help of the mouse')
        CALL PGSCI(5)
        CALL RPGBAND(6,0,0.,0.,XC,YC,CH)
        XC1=XC
        CALL PGSCI(1)
        IF(XC1.LT.XMIN) XC1=XMIN
        IF(XC1.GT.XMAX) XC1=XMAX
        WRITE(CDUMMY,*) XC1
        L1=TRUEBEG(CDUMMY)
        L2=TRUELEN(CDUMMY)
        CALL HELPTEXT('Cursor X='//CDUMMY(L1:L2)//
     +   '. Press the mouse again...')
        CALL PGSCI(5)
        CALL RPGBAND(4,0,XC,0.,XC,YC,CH)
        XC2=XC
        CALL PGSCI(1)
        IF(XC2.LT.XMIN) XC2=XMIN
        IF(XC2.GT.XMAX) XC2=XMAX
        BG=AMIN1(XC1,XC2)
        FG=AMAX1(XC2,XC2)
        CALL HELPTEXT(' ')
C------------------------------------------------------------------------------
C recuperamos region de dibujo inicial
        CALL PGSVP(XV1,XV2,YV1,YV2)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
C
        END
C
C******************************************************************************
C ITYPE=0: dibuja imagen completa en peque\~{n}o, con region de zoom marcada en 
C otro color. 
C ITYPE=1: dibuja region en la que se esta midiendo, marcandose en otro color
C los pixels que no se utilizan en la estadistica.
        SUBROUTINE PGIMAGSMALL(NC1,NC2,NS1,NS2,FG,BG,ITYPE)
        IMPLICIT NONE
        INTEGER NC1,NC2,NS1,NS2
        REAL BG,FG
        INTEGER ITYPE
C
        INTEGER MAXNAXIS
        PARAMETER(MAXNAXIS=2)                !valor maximo admisible para NAXIS
        INCLUDE 'dimensions.inc'
C
        INTEGER I,J
        INTEGER NAXIS(0:MAXNAXIS)                               !NAXIS=NAXIS(0)
        REAL IMAGEN(NXMAX,NYMAX)
        REAL TR(6)
        REAL XV1,XV2,YV1,YV2
        REAL XW1,XW2,YW1,YW2
        REAL XMIN,XMAX,YMIN,YMAX
        LOGICAL LNULL(NXMAX,NYMAX),ANYNULL
C
        COMMON/BLKIMAGEN/IMAGEN
        COMMON/BLKNAXIS/NAXIS
        COMMON/BLKLNULL/LNULL,ANYNULL
C------------------------------------------------------------------------------
        TR(1)=0.
        TR(2)=1.
        TR(3)=0.
        TR(4)=0.
        TR(5)=0.
        TR(6)=1.
C
        IF(ITYPE.EQ.0)THEN
          XMIN=REAL(1)-0.5
          XMAX=REAL(NAXIS(1))+0.5
          YMIN=REAL(1)-0.5
          YMAX=REAL(NAXIS(2))+0.5
        ELSE
          XMIN=REAL(NC1)-0.5
          XMAX=REAL(NC2)+0.5
          YMIN=REAL(NS1)-0.5
          YMAX=REAL(NS2)+0.5
        END IF
C almacenamos region de dibujo actual
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)
C------------------------------------------------------------------------------
C definimos nueva region de dibujo
        CALL PGSVP(0.75,1.00,0.48,0.65)
        CALL RPGERASW(0.74,1.00,0.47,0.66,0)
        IF(ITYPE.EQ.0)THEN
          CALL PGWNAD(XMIN,XMAX,YMIN,YMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,1,NAXIS(1),1,NAXIS(2),FG,BG,TR)
        ELSE
          CALL PGWNAD(XMIN,XMAX,YMIN,YMAX)
          CALL PGIMAG(IMAGEN,NXMAX,NYMAX,NC1,NC2,NS1,NS2,FG,BG,TR)
          DO I=NS1,NS2
            DO J=NC1,NC2
              IF(IMAGEN(J,I).GT.FG)THEN
                CALL PGSCI(2)
                CALL PGMOVE(REAL(J)-0.5,REAL(I)-0.5)
                CALL PGDRAW(REAL(J)+0.5,REAL(I)+0.5)
                CALL PGMOVE(REAL(J)-0.5,REAL(I)+0.5)
                CALL PGDRAW(REAL(J)+0.5,REAL(I)-0.5)
              ELSEIF(IMAGEN(J,I).LT.BG)THEN
                CALL PGSCI(5)
                CALL PGMOVE(REAL(J)-0.5,REAL(I)-0.5)
                CALL PGDRAW(REAL(J)+0.5,REAL(I)+0.5)
                CALL PGMOVE(REAL(J)-0.5,REAL(I)+0.5)
                CALL PGDRAW(REAL(J)+0.5,REAL(I)-0.5)
              END IF
              IF(LNULL(J,I))THEN
                CALL PGSCI(6)
                CALL PGMOVE(REAL(J)-0.5,REAL(I)-0.5)
                CALL PGDRAW(REAL(J)+0.5,REAL(I)+0.5)
                CALL PGMOVE(REAL(J)-0.5,REAL(I)+0.5)
                CALL PGDRAW(REAL(J)+0.5,REAL(I)-0.5)
                CALL PGMOVE(REAL(J),REAL(I)-0.5)
                CALL PGDRAW(REAL(J),REAL(I)+0.5)
                CALL PGMOVE(REAL(J)-0.5,REAL(I))
                CALL PGDRAW(REAL(J)+0.5,REAL(I))
              END IF
            END DO
          END DO
          CALL PGSCI(1)
        END IF
        CALL PGBOX('BC',0.0,0,'BC',0.0,0)
        IF(ITYPE.EQ.0)THEN
          IF((NC1.NE.1).OR.(NC2.NE.NAXIS(1)).OR.
     +       (NS1.NE.1).OR.(NS2.NE.NAXIS(2)))THEN
            CALL PGSCI(3)
            CALL PGMOVE(REAL(NC1),REAL(NS1))
            CALL PGDRAW(REAL(NC1),REAL(NS2))
            CALL PGDRAW(REAL(NC2),REAL(NS2))
            CALL PGDRAW(REAL(NC2),REAL(NS1))
            CALL PGDRAW(REAL(NC1),REAL(NS1))
            CALL PGSCI(1)
          END IF
        END IF
C------------------------------------------------------------------------------
C recuperamos region de dibujo inicial
        CALL PGSVP(XV1,XV2,YV1,YV2)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
C------------------------------------------------------------------------------
        END
C
C******************************************************************************
C
        SUBROUTINE BIGTEXT(IMODE,ILABEL)
        IMPLICIT NONE
        INTEGER IMODE,ILABEL
C
        INTEGER OLD_CI
        REAL OLD_CH
        REAL XV1,XV2,YV1,YV2
        REAL XW1,XW2,YW1,YW2
C------------------------------------------------------------------------------
C almacenamos region de dibujo actual
        CALL PGQVP(0,XV1,XV2,YV1,YV2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL PGQCH(OLD_CH)
        CALL PGQCI(OLD_CI)
C------------------------------------------------------------------------------
        CALL PGSVP(0.0,1.0,0.0,1.0)
        CALL PGSWIN(0.0,1.0,0.0,1.0)
        IF(IMODE.EQ.0)THEN
          CALL PGSCI(7)
        ELSE
          CALL PGSCI(3)
        END IF
C
        CALL PGSCH(1.5)
        IF(ILABEL.EQ.1)THEN
          CALL PGPTEXT(0.1250,0.96,0.0,0.5,'FILE')
        ELSEIF(ILABEL.EQ.2)THEN
          CALL PGPTEXT(0.3125,0.96,0.0,0.5,'ZOOM')
        ELSEIF(ILABEL.EQ.3)THEN
          CALL PGPTEXT(0.4375,0.96,0.0,0.5,'LUT')
        ELSEIF(ILABEL.EQ.4)THEN
          CALL PGPTEXT(0.5625,0.96,0.0,0.5,'MEASURE')
        ELSEIF(ILABEL.EQ.5)THEN
          CALL PGPTEXT(0.8125,0.96,0.0,0.5,'CALCULATOR')
        END IF
C------------------------------------------------------------------------------
C recuperamos region de dibujo inicial
        CALL PGSVP(XV1,XV2,YV1,YV2)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
        CALL PGSCI(OLD_CI)
        CALL PGSCH(OLD_CH)
C
        END
C
C******************************************************************************
C Subrutina para leer una image FITS
        SUBROUTINE LEEFITS
        IMPLICIT NONE
C
        CHARACTER*255   READC               !read character
        INTEGER         TRUELEN
C
        INTEGER MAXNAXIS
        PARAMETER(MAXNAXIS=2)                !valor maximo admisible para NAXIS
        INCLUDE 'dimensions.inc'
C
        INTEGER JROW(NXMAX)
        INTEGER I,J,L
        INTEGER FIRSTPIX
        INTEGER BITPIX,NAXIS(0:MAXNAXIS)
        INTEGER ISTATUS,IREADWRITE,IUNIT
        INTEGER BLOCKSIZE,NULLVAL
        INTEGER NKEYS,NSPACE,NFOUND
        REAL IMAGEN(NXMAX,NYMAX),FROW(NXMAX)
        REAL DATAMIN,DATAMAX
        DOUBLE PRECISION DROW(NXMAX)
        CHARACTER*50 COMMENT
        CHARACTER*80 FITSFILE,CLINEA
        LOGICAL LOGFILE,ANYNULL,LANYNULL
        LOGICAL LROW(NXMAX),LNULL(NXMAX,NYMAX)
C
        COMMON/BLKIMAGEN/IMAGEN !imagen FITS leida en formato REAL
        COMMON/BLKLNULL/LNULL,ANYNULL   !mascara que indica si existen NaN, etc.
        COMMON/BLKDATMINMAX/DATAMIN,DATAMAX !maximo y minimo en la imagen
        COMMON/BLKNAXIS/NAXIS   !dimensiones
        COMMON/BLKFITSFILE/FITSFILE !nombre de la imagen
C------------------------------------------------------------------------------
C inicializamos variables
        ISTATUS=0               !controla posibles errores durante la ejecucion
        IREADWRITE=0                      !la imagen se abrira en modo READONLY
        NULLVAL=-999
        LANYNULL=.FALSE.
C------------------------------------------------------------------------------
C pedimos nombre de la imagen
10      FITSFILE=READC('Name of the image','@','@')
        INQUIRE(FILE=FITSFILE,EXIST=LOGFILE)
        IF(LOGFILE)THEN
        ELSE
          WRITE(*,100)'ERROR: the file "'
          WRITE(*,100)FITSFILE(1:TRUELEN(FITSFILE))
          WRITE(*,101)'" does not exist.'
          WRITE(*,*)
          WRITE(*,100)'Press <RETURN> to continue...'
          READ(*,*)
          GOTO 10
        END IF
C localizamos un numero de unidad de fichero no utilizada
ccc     CALL FTGIOU(IUNIT,ISTATUS)
        IUNIT=80 !ojo, IUNIT=99 entra en conflicto con el Postcript y es
                 !precisamente este numero el que toma esta funcion
C abrimos el fichero
        CALL FTOPEN(IUNIT,FITSFILE,IREADWRITE,BLOCKSIZE,ISTATUS)
C determinamos el numero de keywords en la cabecera y las mostramos
        CALL FTGHSP(IUNIT,NKEYS,NSPACE,ISTATUS)
        DO I=1,NKEYS
          CALL FTGREC(IUNIT,I,CLINEA,ISTATUS)
          L=TRUELEN(CLINEA)
          WRITE(*,101)CLINEA(1:L)
        END DO
        IF(ISTATUS.EQ.0)THEN                                  !todo ha ido bien
          WRITE(*,101)'END'
          WRITE(*,*)
        END IF
C leemos BITPIX
        CALL FTGKYJ(IUNIT,'BITPIX',BITPIX,COMMENT,ISTATUS)
C comprobamos que NAXIS=2
        CALL FTGKYJ(IUNIT,'NAXIS',NAXIS(0),COMMENT,ISTATUS)
        IF(NAXIS(0).GT.MAXNAXIS)THEN
          WRITE(*,100)'FATAL ERROR: NAXIS >'
          WRITE(*,*)MAXNAXIS
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C leemos NAXIS1 y NAXIS2 [notar que el quinto parametro es NAXIS(1) en lugar
C de NAXIS para asi recuperar NAXIS(1) y NAXIS(2)]
        CALL FTGKNJ(IUNIT,'NAXIS',1,2,NAXIS(1),NFOUND,ISTATUS)
        IF(NAXIS(1).GT.NXMAX)THEN
          WRITE(*,101)'* FATAL ERROR in subroutine LEEFITS:'
          WRITE(*,101)'NAXIS(1) > NXMAX'
          STOP
        END IF
        IF(NAXIS(2).GT.NYMAX)THEN
          WRITE(*,101)'* FATAL ERROR in subroutine LEEFITS:'
          WRITE(*,101)'NAXIS(2) > NYMAX'
          STOP
        END IF
C leemos la imagen
        IF(BITPIX.EQ.16)THEN
          DO I=1,NAXIS(2)
            FIRSTPIX=(I-1)*NAXIS(1)+1
            CALL FTGPFJ(IUNIT,1,FIRSTPIX,NAXIS(1),JROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS(1)
              IMAGEN(J,I)=REAL(JROW(J))
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSEIF(BITPIX.EQ.32)THEN
          DO I=1,NAXIS(2)
            FIRSTPIX=(I-1)*NAXIS(1)+1
            CALL FTGPFE(IUNIT,1,FIRSTPIX,NAXIS(1),FROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS(1)
              IMAGEN(J,I)=FROW(J)
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
ccc       CALL FTG2DE(IUNIT,1,NULLVAL,NXMAX,NAXIS(1),NAXIS(2),
ccc     +     IMAGEN,ANYNULL,ISTATUS)
        ELSEIF(BITPIX.EQ.-32)THEN
          DO I=1,NAXIS(2)
            FIRSTPIX=(I-1)*NAXIS(1)+1
            CALL FTGPFE(IUNIT,1,FIRSTPIX,NAXIS(1),FROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS(1)
              IMAGEN(J,I)=FROW(J)
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
ccc       CALL FTG2DE(IUNIT,1,NULLVAL,NXMAX,NAXIS(1),NAXIS(2),
ccc     +     IMAGEN,ANYNULL,ISTATUS)
        ELSEIF(BITPIX.EQ.-64)THEN
          DO I=1,NAXIS(2)
            FIRSTPIX=(I-1)*NAXIS(1)+1
            CALL FTGPFD(IUNIT,1,FIRSTPIX,NAXIS(1),DROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS(1)
              IMAGEN(J,I)=REAL(DROW(J))
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSE
          WRITE(*,100)'FATAL ERROR in subroutine LEEFITS: BITPIX ='
          WRITE(*,*) BITPIX
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C cerramos el fichero
        CALL FTCLOS(IUNIT,ISTATUS)
C liberamos el numero de unidad del fichero utilizado
        CALL FTFIOU(IUNIT,ISTATUS)
C calculamos maximo y minimo
        DATAMIN=1.E30
        DATAMAX=-1.E30
        DO I=1,NAXIS(2)
          DO J=1,NAXIS(1)
            IF(.NOT.LNULL(J,I))THEN
              IF(IMAGEN(J,I).LT.DATAMIN) DATAMIN=IMAGEN(J,I)
              IF(IMAGEN(J,I).GT.DATAMAX) DATAMAX=IMAGEN(J,I)
            END IF
          END DO
        END DO
C chequeamos si se ha producido algun error
        IF(ISTATUS.GT.0)THEN
          CALL PRINTERROR(ISTATUS)
        END IF
        ANYNULL=LANYNULL                  !basta que haya ocurrido una sola vez
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
C******************************************************************************
C Subrutina para escribir una image FITS
        SUBROUTINE ESCFITS
        IMPLICIT NONE
C
        CHARACTER*255   READC               !read character
        INTEGER         TRUELEN
C
        INTEGER MAXNAXIS
        PARAMETER(MAXNAXIS=2)                !valor maximo admisible para NAXIS
        INCLUDE 'dimensions.inc'
C
        INTEGER BITPIX,NAXIS(0:MAXNAXIS)
        INTEGER ISTATUS,IUNIT
        INTEGER BLOCKSIZE,NULLVAL
        REAL IMAGEN(NXMAX,NYMAX)
        CHARACTER*80 FITSFILE
        LOGICAL LOGFILE
        LOGICAL SIMPLE,EXTEND
C
        COMMON/BLKIMAGEN/IMAGEN
        COMMON/BLKNAXIS/NAXIS
        COMMON/BLKFITSFILE/FITSFILE
C------------------------------------------------------------------------------
C inicializamos variables
        ISTATUS=0               !controla posibles errores durante la ejecucion
        NULLVAL=-999
        BLOCKSIZE=1           !normalmente usaremos un valor de 1 (=2880 bytes)
C------------------------------------------------------------------------------
C pedimos nombre de la imagen
10      FITSFILE=READC('Name of the image','@','@')
        INQUIRE(FILE=FITSFILE,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          WRITE(*,100)'ERROR: the file "'
          WRITE(*,100)FITSFILE(1:TRUELEN(FITSFILE))
          WRITE(*,101)'" already exists. Choose a different name.'
          WRITE(*,*)
          WRITE(*,100)'Press <RETURN> to continue...'
          READ(*,*)
          GOTO 10
        ELSE
          WRITE(*,100)'Saving image...'
        END IF
C localizamos un numero de unidad de fichero no utilizada
ccc     CALL FTGIOU(IUNIT,ISTATUS)
        IUNIT=82 !ojo, IUNIT=99 entra en conflicto con el Postcript y es
                 !precisamente este numero el que toma esta funcion
C creamos un nuevo fichero FITS vacio
        CALL FTINIT(IUNIT,FITSFILE,BLOCKSIZE,ISTATUS)
C inicializamos los parametros
        SIMPLE=.TRUE.
        BITPIX=-32
        EXTEND=.FALSE.
C escribimos los keywords indispensables
        CALL FTPHPR(IUNIT,SIMPLE,BITPIX,NAXIS(0),NAXIS(1),0,1,
     +   EXTEND,ISTATUS)
C salvamos la imagen
        IF(BITPIX.EQ.-32)THEN
          CALL FTP2DE(IUNIT,1,NXMAX,NAXIS(1),NAXIS(2),IMAGEN,ISTATUS)
        ELSE
          WRITE(*,100)'FATAL ERROR in subroutine ESCFITS: BITPIX ='
          WRITE(*,*) BITPIX
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C cerramos el fichero
        CALL FTCLOS(IUNIT,ISTATUS)
C liberamos el numero de unidad del fichero utilizado
        CALL FTFIOU(IUNIT,ISTATUS)
C chequeamos si se ha producido algun error
        IF(ISTATUS.GT.0)THEN
          CALL PRINTERROR(ISTATUS)
        END IF
        WRITE(*,101)'  ...OK'
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        SUBROUTINE PRINTERROR(ISTATUS)
C Print out the FITSIO error messages to the user
        INTEGER ISTATUS
        CHARACTER ERRTEXT*30,ERRMESSAGE*80
C Check if status is OK (no error); if so, simply return
        IF(ISTATUS.LE.0) RETURN
C Get the text string which describes the error
        CALL FTGERR(ISTATUS,ERRTEXT)
        WRITE(*,'(A,$)')'FITSIO Error Status = '
        WRITE(*,*)ISTATUS
        WRITE(*,'(A)')ERRTEXT
C Read and print out all the error messages on the FITSIO stack
        CALL FTGMSG(ERRMESSAGE)
        DO WHILE(ERRMESSAGE.NE.' ')
          WRITE(*,'(A)') ERRMESSAGE
          CALL FTGMSG(ERRMESSAGE)
        END DO
        END
