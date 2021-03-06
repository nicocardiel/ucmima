C------------------------------------------------------------------------------
C Version 26-November-1996                                      File: buttsbr.f
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
C SUBROUTINE BUTTSBR(X1,X2,Y1,Y2)
C
C Input: X1,X2,Y1,Y2
C
C Set the button region limits.
C
C REAL X1 -> x-coordinate of the left hand edge of the button region viewport,
C      in normalized device coordinates
C REAL X2 -> x-coordinate of the right hand edge of the button region viewport,
C      in normalized device coordinates
C REAL Y1 -> y-coordinate of the bottom edge of the button region viewport,
C      in normalized device coordinates
C REAL Y2 -> y-coordinate of the top edge of the button region viewport,
C      in normalized device coordinates
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTSBR(X1,X2,Y1,Y2)
        IMPLICIT NONE
	REAL X1,X2,Y1,Y2
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
	X3VPORT=X1
	X4VPORT=X2
	Y3VPORT=Y1
	Y4VPORT=Y2
        END
