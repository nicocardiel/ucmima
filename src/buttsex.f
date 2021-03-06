C------------------------------------------------------------------------------
C Version 26-November-1996                                      File: buttsex.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es or fjg@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE BUTTSEX(NBUT,LEXIST)
C
C Input: NBUT,LEXIST
C
C Set whether the asked button is active (currently available) or not.
C
C INTEGER NBUT -> button number
C LOGICAL LEXIST -> .TRUE. if the button is active, .FALSE. otherwise
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTSEX(NBUT,LEXIST)
        IMPLICIT NONE
	INTEGER NBUT
	LOGICAL LEXIST
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
	IF((NBUT.LT.1).OR.(NBUT.GT.MAX_NBUTT))THEN
	  WRITE(*,101)'ERROR: button number out of limits in '//
     +     'subroutine BUTTSEX.'
	  LEXIST=.FALSE.
	  RETURN
	END IF
	EXIST_BUTT(NBUT)=LEXIST
101	FORMAT(A)
        END
