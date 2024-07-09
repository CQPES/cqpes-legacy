C All common blocks are moved based on original code and 
C variables and arrays are in module
C Some parts of the code has to be modified to use module
C Jingjing Zheng, April 2014
C
      SUBROUTINE prepot
C
C   System:          H3
C   Functional Form: Double many-body expansion
C   Common Name:     DMBE
C   Reference:       A. J. C. Varandas, F. B. Brown, C. A. Mead, 
C                    D. G. Truhlar, and N. C. Blais
C                    J. Chem. Phys. 86, 6258 (1987).
C
C   PREPOT must be called once before any call to POT.
C   The potential parameters are included through the subprogram
C   BLOCK DATA H3.  
C   The coordinates, potential energy for the ground electronic state, and 
C   the derivatives of the potential energy for the ground electronic state 
C   with respect to the coordinates are passed through the common block POTCM:
C                  /POTCM/ R(3), ENERGY, DEDR(3).
C   This potential energy function calculates the energy and the 
C   derivatives for the ground electronic state and for the first
C   excited electronic state.  The information for the ground 
C   electronic state is passed in POTCM.  The energy and the 
C   derivatives of the energy for the first electronic state are
C   passed through the common block POT2CM:
C                  /POT2CM/ POTE2, DH3U, COUP(3).
C   All information passed through the common blocks POTCM and 
C   POT2CM are in hartree atomic units.  
C   The the flags that indicate what calculations should be carried out in 
C   the potential routine are passed through the common block POTCCM:
C                  /POTCCM/ NSURF, NDER, NDUM(8)
C   where:
C        NSURF = 0 => ground electronic state
C        NSURF = 1 => ground and first electronic state
C        NDER  = 0 => no derivatives should be calculated
C        NDER  = 1 => calculate first derivatives
C        NDUM  - these 8 integer values can be used to flag options
C                within the potential; in this potential these options 
C                are not used.
C
C***********************************************************************
C     Calculates double many-body expansion of the H3 potentials.
C        R1, R2, R3 - interatomic distances.
C        POTE1 - energy of surface 1 (ground electronic state).
C        DEDR  - derivatives for surface 1.
C        POTE2 - energy of surface 2 (excited electronic state).
C        DH3U  - derivatives for surface 2.
C        COUP  - nonadiabatic coupling.
C        Note:  COUP not yet coded, and derivatives require further
C          checking for isosceles and near-isosceles geometries.
C        Note also:  For more efficient calculation of POTE1, remove
C          the derivatives.
C        This subprogram is in hartree atomic units.
C***********************************************************************
C
      use potcm3
      use h3potcm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DLEP(3),DLEP2(3),DV2(3),DV3(3),DVA(3),DC3(3)
C
C   Set up the constants for this potential; NSURF indicates which surface
C   will be used for computing the energy, NDER indicates whether or not 
C   the derivatives should be calculated, and DASY is the energy of the 
C   system at the reactant asymptote.
C
         PARAMETER (DASY  = 0.174474112D0)
C
         NSURF = 0
         NDER  = 1
C
C   Echo which surface is being used to unit 6.
C
         WRITE (6, 1000)
         IF (NSURF .EQ. 1) WRITE (6, 1100)
C
1000     FORMAT(/,2X,T5,'Prepot has been called for H + HH',
     *          /,2X,T5,'DMBE potential energy surface',
     *          /,2X,T5,'The energy for the ground electronic state ',
     *                  'is calculated.')
1100     FORMAT(/,2X,T5,'The energy for the first electronic state',
     *                  ' is also calculated.')
      RETURN
      END
C*****
C
      SUBROUTINE POT
      use potcm3
      use h3potcm
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DLEP(3),DLEP2(3),DV2(3),DV3(3),DVA(3),DC3(3)
      PARAMETER (DASY  = 0.174474112D0)
!     module potcm3
!      integer :: nsurf, nder, ndum(8)
!      real(8) :: r(3),energy, dedr(3)
!     end module potcm3


C
C   Initialize the variables used for the energy terms and the arrays used
C   for storing the derivatives.
 
      R1 = R(1)
      R2 = R(2)
      R3 = R(3)

         POTE1 = 0.D0
         POTE2 = 0.D0
         DO 10 I = 1, 3
               DEDR(I) = 0.D0
               DH3U(I) = 0.D0
10       CONTINUE
C
C Set up symmetry coordinates and their derivatives
C
      CALL H3COOR (R1,R2,R3)
C
C Set up 2-body correlation energies, dispersion damping terms, and
C    their derivatives
C
      CALL H3COR2
C
C Get Leps potentials and derivatives
C
      CALL H3LEPS (R1,R2,R3,VLEPS,VLEPS2,DLEP,DLEP2)
C
C Get VA term and its derivatives
C
      CALL H3VA (R1,R2,R3,VA,DVA)
C
C Get VII term and its derivatives
C
      CALL H3VII (R1,R2,R3,VII,DV2)
C
C Get VIII term and its derivatives
C
      CALL H3VIII (R1,R2,R3,VIII,DV3)
C
C Get 3-body correlation and its derivative
C
      CALL H3COR3 (R1,R2,R3,CE3,DC3)
C
C 2-body correlation term
C
      CE2 = CORRS(1)+CORRS(2)+CORRS(3)
C
      VC = VA+VII+VIII+CE2+CE3+DASY
      POTE1 = VLEPS+VC
      IF (NSURF .EQ. 1) POTE2 = VLEPS2+VC
      IF (NDER .EQ. 1) THEN
          DO 20 I = 1, 3
                T = DVA(I)+DV2(I)+DV3(I)+DCORRS(I)+DC3(I)
                DEDR(I) = DLEP(I)+T
                IF (NSURF .EQ. 1) DH3U(I) = DLEP2(I)+T
20        CONTINUE
      ENDIF

      ENERGY = POTE1
C
      RETURN
      END
C*****
C
C     BLOCK DATA H3
C
C***********************************************************************
C     Data for the DMBE H3 surface.
C***********************************************************************
C
C     use h3potcm
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C
C     DATA RHOL / 2.4848D0 /
C     DATA ALPH0,ALPH1,BET0,BET1 / 25.9528D0,1.1868D0,15.7381D0,
C    *                             0.09729D0 /
C     DATA ALPHA5,BETA1,BETA2,BETA3 / 8.2433033D-3,0.53302897D0,       
C    *   0.39156612D-1,0.69996945D0 /
C     DATA ALPH2 / 4.735364D-1 /
C     DATA AL0,AL1,AL2,AL3 / 0.45024472D+1,-0.62467617D+1,0.40966542D+1,
C    *                       0.21813012D+1 /
C     DATA AZ2 / 4.453649D-4 /
C     DATA CD0,CD1 / 6.333404D-3,-1.726839D-3 /
C     DATA CHH / 6.499027D0,1.243991D+2,3285.8D0 /
C     DATA CK0 / -1.372843D-1,-1.638459D-1,-1.973814D-1 /
C     DATA CK1 / 1.011204D0,9.988099D-1,9.399411D-1 /
C     DATA HFD,HFA1,HFA2,HFA3,HFGAM / 0.15796326D0,2.1977034D0,         
C    *   1.2932502D0,0.64375666D0,2.1835071D0 /
C     DATA H2RM,H2R0,H2RMT / 1.401D0,6.928203D0,7.82D0 /
C     DATA SQRT3 / 1.73205080756887D0 /
C     DATA XPAR / -0.9286579D-2,0.2811592D-3,-0.4665659D-5,0.20698D-7,
C    *            0.2903613D+2,-0.2934824D+1,0.7181886D0,-0.3753218D0,
C    *            -0.1114538D+1,0.2134221D+1,-0.4164343D0,0.2022584D0,
C    *            -0.4662687D-1,-0.4818623D+2,0.2988468D0 /
C     END
C*****
C
      SUBROUTINE H3ACCT (R,ETRIP,DETR)
C
C***********************************************************************
C     Computes the HFACE (Hartree-Fock-approximate correlation energy)
C     potential for the lowest triplet state of H2 [A.J.C. Varandas &
C     J. Brandao Mol. Phys.,45,1982,857] without the 2-body correlation
C     energy.
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA A,B1,B2,B3,B4 / 0.448467D0,-0.056687D0,0.246831D0,
     *  -0.018419D0,0.000598D0 /
C
      IF (R.GT.1.0D+8) THEN
         ETRIP = 0.0D0
         DETR = 0.0D0
      ELSE
         T = R*(B1+R*(B2+R*(B3+R*B4)))
         ETRIP = A*EXP(-T)/R
C
C  Derivative
C
         T = B1+R*(2.0D0*B2+R*(3.0D0*B3+R*4.0D0*B4))
         DETR = -ETRIP*(T+1.0D0/R)
      ENDIF
      RETURN
      END
C*****
C
      SUBROUTINE H3COOR (R1,R2,R3)
C
C**********************************************************************
C   Calculates D3H symmetry coordinates and derivatives
C**********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3COCM/ PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,
C    *   DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3
C
      PER = R1+R2+R3
      PER2 = PER*PER
C
C   QCOORD and its derivatives
C
      R12 = R1*R1
      R22 = R2*R2
      R32 = R3*R3
      QCOORD = R12+R22+R32
      DQ1 = 2.0D0*R1
      DQ2 = 2.0D0*R2
      DQ3 = 2.0D0*R3
C
C   RHO and its derivatives
C
      RHO = SQRT(QCOORD/3.0D0)
      T = 1.0D0/(6.0D0*RHO)
      DRHO1 = T*DQ1
      DRHO2 = T*DQ2
      DRHO3 = T*DQ3
C
C   S, CPHI3 (cos(phi3)), and their derivatives
C
      GAMMA = 2.0D0*R12-R22-R32
      GAM2 = GAMMA*GAMMA
      DGM1 = 2.0D0*DQ1
      DGM2 = -DQ2
      DGM3 = -DQ3
      BETA = SQRT3*(R22-R32)
      BET2 = BETA*BETA
      DBT1 = 0.0D0
      DBT2 = SQRT3*DQ2
      DBT3 = -SQRT3*DQ3
      T12 = BET2+GAM2
      T1 = SQRT(T12)
      S = T1/QCOORD
      S2 = S*S
      IF (S.EQ.0.0D0) THEN
         DS1 = 0.0D0
         DS2 = 0.0D0
         DS3 = 0.0D0
C
C   For S=0, CPHI3 and its derivative should not be used anywhere but
C      set to zero anyway.
C
         CPHI3 = 0.0D0
         DCPHI1 = 0.0D0
         DCPHI2 = 0.0D0
         DCPHI3 = 0.0D0
      ELSE
         DS1 = S*((BETA*DBT1+GAMMA*DGM1)/T12-DQ1/QCOORD)
         DS2 = S*((BETA*DBT2+GAMMA*DGM2)/T12-DQ2/QCOORD)
         DS3 = S*((BETA*DBT3+GAMMA*DGM3)/T12-DQ3/QCOORD)
         T2 = 1.0D0/(T1*T12)
         CPHI3 = GAMMA*(3.0D0*BET2-GAM2)*T2
         T3 = 3.0D0*BETA*(3.0D0*GAM2-BET2)*T2/T12
         DCPHI1 = T3*(GAMMA*DBT1-BETA*DGM1)
         DCPHI2 = T3*(GAMMA*DBT2-BETA*DGM2)
         DCPHI3 = T3*(GAMMA*DBT3-BETA*DGM3)
      ENDIF
      RETURN
      END
C*****
C
      SUBROUTINE H3COR2
C
C***********************************************************************
C     Calculates 2-body correlation energies for singlet and triplet
C     states of H2, the damping factors for the dispersion terms, and
C     their 1st derivatives
C***********************************************************************

      use potcm3
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /POTCM/ RR(3),POTEI,DEDR(3)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3CRCM/ CORRS(3),DCORRS(3),CORRT(3),DCORRT(3),DAMP(3,3),  
C    *   DDAMP(3,3)

C
C   Loop over three coordinates
C
      DO 20 J = 1, 3
C        R = RR(J)
         CORRS(J) = 0.0D0
         DCORRS(J) = 0.0D0
         CORRT(J) = 0.0D0
         DCORRT(J) = 0.0D0
C        T1 = 1.0D0/R
         T1 = 1.0D0/R(J)
         T = T1*T1
         T2 = T*T
         T1 = T2*T1
         NEXP = 4
C
C      Loop over terms in dispersion expansion
C
         DO 10 ID = 1, 3
            NEXP = NEXP+2
            T2 = T2*T
            T1 = T1*T
C
C     singlet
C
C           CALL H3DAMP (R,NEXP,H2RM,H2R0,D,DD)
            CALL H3DAMP (R(J),NEXP,H2RM,H2R0,D,DD)
C
C     store damping factors (including CHH coeff and 1/R**NEXP) for
C        later use in computing the 3-body correlation energy.
C
            DAMP(ID,J) = CHH(ID)*D*T2
            DDAMP(ID,J) = CHH(ID)*(DD*T2-DBLE(NEXP)*D*T1)
            CORRS(J) = CORRS(J)-DAMP(ID,J)
            DCORRS(J) = DCORRS(J)-DDAMP(ID,J)
C
C     triplet
C
C           CALL H3DAMP (R,NEXP,H2RMT,H2R0,D,DD)
            CALL H3DAMP (R(J),NEXP,H2RMT,H2R0,D,DD)
            CORRT(J) = CORRT(J)-CHH(ID)*D*T2
            DCORRT(J) = DCORRT(J)-CHH(ID)*(DD*T2-DBLE(NEXP)*D*T1)
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
C*****
C
      SUBROUTINE H3COR3 (R1,R2,R3,CE3,DC3)
C
C***********************************************************************
C     Calculates 3-body correlation energy and its 1st derivatives
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DC3(3)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3CRCM/ CORRS(3),DCORRS(3),CORRT(3),DCORRT(3),DAMP(3,3),  
C    *   DDAMP(3,3)
C
      CE3 = 0.0D0
      DC3(1) = 0.0D0
      DC3(2) = 0.0D0
      DC3(3) = 0.0D0
C
C   Loop over terms in dispersion expansion; dispersion damping factors
C      are computed in H3COR2 and passed through COMMON /H3CRCM/.
C
      DO 10 ID = 1, 3
         CALL H3G (R1,CK0(ID),CK1(ID),H2RM,G1,GD1)
         CALL H3G (R2,CK0(ID),CK1(ID),H2RM,G2,GD2)
         CALL H3G (R3,CK0(ID),CK1(ID),H2RM,G3,GD3)
         CALL H3H (R1,CK1(ID),H1,HD1)
         CALL H3H (R2,CK1(ID),H2,HD2)
         CALL H3H (R3,CK1(ID),H3,HD3)
         T = 1.0D0-0.5D0*(G2*H3+G3*H2)
         T1 = T*DAMP(ID,1)
         T1D1 = T*DDAMP(ID,1)
         T1D2 = -0.5D0*(GD2*H3+G3*HD2)*DAMP(ID,1)
         T1D3 = -0.5D0*(G2*HD3+GD3*H2)*DAMP(ID,1)
         T = 1.0D0-0.5D0*(G3*H1+G1*H3)
         T2 = T*DAMP(ID,2)
         T2D1 = -0.5D0*(G3*HD1+GD1*H3)*DAMP(ID,2)
         T2D2 = T*DDAMP(ID,2)
         T2D3 = -0.5D0*(GD3*H1+G1*HD3)*DAMP(ID,2)
         T = 1.0D0-0.5D0*(G1*H2+G2*H1)
         T3 = T*DAMP(ID,3)
         T3D1 = -0.5D0*(GD1*H2+G2*HD1)*DAMP(ID,3)
         T3D2 = -0.5D0*(G1*HD2+GD2*H1)*DAMP(ID,3)
         T3D3 = T*DDAMP(ID,3)
         CE3 = CE3+T1+T2+T3
         DC3(1) = DC3(1)+T1D1+T2D1+T3D1
         DC3(2) = DC3(2)+T1D2+T2D2+T3D2
         DC3(3) = DC3(3)+T1D3+T2D3+T3D3
   10 CONTINUE
      RETURN
      END
C*****
C
      SUBROUTINE H3DAMP (R,N,RM,R0,D,DD)
C
C***********************************************************************
C     Calculates damping function for the nth dispersion coefficient and
C     its 1st derivative
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C
      A = ALPH0/DBLE(N)**ALPH1
      B = BET0*EXP(-BET1*DBLE(N))
      DENOM = RM+2.5D0*R0
      X = 2.0D0*R/DENOM
      POL = X*(A+X*B)
      T = EXP(-POL)
      T1 = 1.0D0-T
      T2 = T1**(N-1)
      D = T1*T2
      DPOL = A+2.0D0*B*X
      DD = (DBLE(2*N)/DENOM)*DPOL*T*T2
      RETURN
      END
C*****
C
      SUBROUTINE H3G (R,CK0X,CK1X,RM,G,GD)
C
C***********************************************************************
C     Calculates G function and its 1st derivative
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      T = CK0X*EXP(-CK1X*(R-RM))
      G = 1.0D0+T
      GD = -CK1X*T
      RETURN
      END
C*****
C
      SUBROUTINE H3H (R,ET,H,HD)
C
C***********************************************************************
C     Calculates H function and its 1st derivative.
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      T = ET*R
      SGNT = 1.0D0
      IF (T.LT.0.0D0) THEN
         T = -T
         SGNT = -1.0D0
      ENDIF
      T = EXP(-T)
      T2 = T*T
      T1 = 1.0D0/(1.0D0+T2)
      HYSEC = 2.0D0*T*T1
      HYTAN = SGNT*(1.0D0-T2)*T1
      T1 = HYTAN**5
      H = HYTAN*T1
      HD = 6.0D0*ET*T1*HYSEC*HYSEC
      RETURN
      END
C*****
C
      SUBROUTINE H3H2HF (R,VHF,DVHF)
C
C***********************************************************************
C     Calculates extended-Hartree-Fock curve for ground singlet state of
C     H2 [A.J.C. Varandas & J.D. Silva, J. Chem. Soc. Faraday II
C     (submitted)] and 1st derivative of 2-body extended-Hartree-Fock
C     curve for the ground-singlet state of H2.
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C
      DR = R-H2RM
      T = EXP(-HFGAM*DR)
      VHF = -HFD*(1.0D0+DR*(HFA1+DR*(HFA2+DR*HFA3)))*T
      DVHF = -HFGAM*VHF-HFD*(HFA1+DR*(2.0D0*HFA2+3.0D0*DR*HFA3))*T
      RETURN
      END
C*****
C
      SUBROUTINE H3LEPS (R1,R2,R3,VLEPS,VLEPS2,DLEP,DLEP2)
C
C***********************************************************************
C     Calculates 3-body extended-Hartree-Fock energy defined by a
C     LEPS-type function.
C***********************************************************************
C   VLEPS2 IS LEPS LOWER SURFACE.
C   VLEPS2 IS LEPS UPPER SURFACE.
C   DLEP(3) ARE LEPS LOWER SURFACE DERIVATIVES
C   DLEP2(3) "   "   UPPER    "       "
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DLEP(3),DLEP2(3)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3CRCM/ CORRS(3),DCORRS(3),CORRT(3),DCORRT(3),DAMP(3,3),  
C    *   DDAMP(3,3)
C
C  Get HF potentials for the ground singlet state of H2
C
      CALL H3H2HF (R1,SNG1,SNG1D1)
      CALL H3H2HF (R2,SNG2,SNG2D2)
      CALL H3H2HF (R3,SNG3,SNG3D3)
C
C  Compute switching function for the triplet function and
C     its derivatives
C
      CALL H3SWIT (R1,R2,R3,F,DF1,DF2,DF3)
      OMF = 1.0D0-F
C
C  Compute the H2 triplet energies
C
      CALL H3ACCT (R1,AT1,ATD1)
C
C     Add in triplet and subtract singlet 2-body correlation terms
C
      AT1 = AT1+CORRT(1)-CORRS(1)
      ATD1 = ATD1+DCORRT(1)-DCORRS(1)
      CALL H3TRIP (R1,WE1,WED1)
      T1 = F*WE1+OMF*AT1
      FTERM = WE1-AT1
      T1D1 = FTERM*DF1+F*WED1+OMF*ATD1
      T1D2 = FTERM*DF2
      T1D3 = FTERM*DF3
      CALL H3ACCT (R2,AT2,ATD2)
C
C     Add in triplet and subtract singlet 2-body correlation terms
C
      AT2 = AT2+CORRT(2)-CORRS(2)
      ATD2 = ATD2+DCORRT(2)-DCORRS(2)
      CALL H3TRIP (R2,WE2,WED2)
      T2 = WE2*F+OMF*AT2
      FTERM = WE2-AT2
      T2D1 = FTERM*DF1
      T2D2 = FTERM*DF2+F*WED2+OMF*ATD2
      T2D3 = FTERM*DF3
      CALL H3ACCT (R3,AT3,ATD3)
C
C     Add in triplet and subtract singlet 2-body correlation terms
C
      AT3 = AT3+CORRT(3)-CORRS(3)
      ATD3 = ATD3+DCORRT(3)-DCORRS(3)
      CALL H3TRIP (R3,WE3,WED3)
      T3 = WE3*F+OMF*AT3
      FTERM = WE3-AT3
      T3D1 = FTERM*DF1
      T3D2 = FTERM*DF2
      T3D3 = FTERM*DF3+F*WED3+OMF*ATD3
C
C  Construct LEPS potential
C
      Q1 = 0.5D0*(SNG1+T1)
      Q2 = 0.5D0*(SNG2+T2)
      Q3 = 0.5D0*(SNG3+T3)
      DQ1D1 = 0.5D0*(SNG1D1+T1D1)
      DQ1D2 = 0.5D0*T1D2
      DQ1D3 = 0.5D0*T1D3
      DQ2D1 = 0.5D0*T2D1
      DQ2D2 = 0.5D0*(SNG2D2+T2D2)
      DQ2D3 = 0.5D0*T2D3
      DQ3D1 = 0.5D0*T3D1
      DQ3D2 = 0.5D0*T3D2
      DQ3D3 = 0.5D0*(SNG3D3+T3D3)
      EX1 = 0.5D0*(SNG1-T1)
      EX2 = 0.5D0*(SNG2-T2)
      EX3 = 0.5D0*(SNG3-T3)
      DJ1D1 = 0.5D0*(SNG1D1-T1D1)
      DJ1D2 = -DQ1D2
      DJ1D3 = -DQ1D3
      DJ2D1 = -DQ2D1
      DJ2D2 = 0.5D0*(SNG2D2-T2D2)
      DJ2D3 = -DQ2D3
      DJ3D1 = -DQ3D1
      DJ3D2 = -DQ3D2
      DJ3D3 = 0.5D0*(SNG3D3-T3D3)
      F1 = (EX1-EX2)**2
      F2 = (EX2-EX3)**2
      F3 = (EX3-EX1)**2
      QSUM = Q1+Q2+Q3
      EXCH = SQRT(0.5D0*(F1+F2+F3))
      VLEPS = QSUM-EXCH
      VLEPS2 = QSUM+EXCH
      XTERM1 = 2.0D0*EX1-EX2-EX3
      XTERM2 = 2.0D0*EX2-EX1-EX3
      XTERM3 = 2.0D0*EX3-EX1-EX2
      QTERM = DQ1D1+DQ2D1+DQ3D1
      TEXCH = EXCH
      IF (TEXCH.LE.0.0D0) TEXCH = 1.0D0
      XTERM = 0.5D0/TEXCH*(XTERM1*DJ1D1+XTERM2*DJ2D1+XTERM3*DJ3D1)
      DLEP(1) = QTERM-XTERM
      DLEP2(1) = QTERM+XTERM
      QTERM = DQ1D2+DQ2D2+DQ3D2
      XTERM = 0.5D0/TEXCH*(XTERM1*DJ1D2+XTERM2*DJ2D2+XTERM3*DJ3D2)
      DLEP(2) = QTERM-XTERM
      DLEP2(2) = QTERM+XTERM
      QTERM = DQ1D3+DQ2D3+DQ3D3
      XTERM = 0.5D0/TEXCH*(XTERM1*DJ1D3+XTERM2*DJ2D3+XTERM3*DJ3D3)
      DLEP(3) = QTERM-XTERM
      DLEP2(3) = QTERM+XTERM
      RETURN
      END
C*****
C
      SUBROUTINE H3SWIT (R1,R2,R3,SWITCH,DF1,DF2,DF3)
C
C***********************************************************************
C     Calculates switching term for the Hartree-Fock component of the
C     diatomic triplet state function.  The notation of Thompson et al.
C     (J. Chem. Phys., 82,5597,1985; and references therein) is used
C     throughout.
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3COCM/ PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,
C    *   DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3
C
      T1 = -AZ2*QCOORD
      IF (S.EQ.0.0D0) THEN
         SWITCH = EXP(T1*QCOORD)
         T1 = 2.0D0*T1*SWITCH
         DF1 = T1*DQ1
         DF2 = T1*DQ2
         DF3 = T1*DQ3
      ELSE
         T2 = 1.0D0+S*S2*CPHI3
         SWITCH = EXP(T1*QCOORD*T2)
         T1 = T1*SWITCH
         TDQ = 2.0D0*T2
         T2 = S2*QCOORD
         TDS = 3.0D0*CPHI3*T2
         TDC = S*T2
         DF1 = T1*(TDQ*DQ1+TDS*DS1+TDC*DCPHI1)
         DF2 = T1*(TDQ*DQ2+TDS*DS2+TDC*DCPHI2)
         DF3 = T1*(TDQ*DQ3+TDS*DS3+TDC*DCPHI3)
      ENDIF
      RETURN
      END
C*****
C
      SUBROUTINE H3TRIP (R,TRIP,TRIPD)
C
C***********************************************************************
C     Calculates effective diatomic triplet state curve.
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C
      PREX = AL0+R*(AL1+R*AL2)
      XP = EXP(-AL3*R)
      TRIP = PREX*XP
      TRIPD = ((AL1+2.0D0*AL2*R)-AL3*PREX)*XP
      RETURN
      END
C*****
C
      SUBROUTINE H3VA (R1,R2,R3,VA,DVA)
C
C***********************************************************************
C     Calculates Va correction energy and its 1st derivative.
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DVA(3)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3COCM/ PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,
C    *   DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3
C
      T = ALPHA5*PER2
      EXPVA = EXP(-T*PER)
      IF (EXPVA.EQ.0.0D0) THEN
         VA = 0.0D0
         DVA(1) = 0.0D0
         DVA(2) = 0.0D0
         DVA(3) = 0.0D0
      ELSE
         VA = 0.0D0
         DV = 0.0D0
         T3 = (R1-R2)*(R2-R3)*(R3-R1)
         T1 = T3*T3
         T2 = 1.0D0
         T4 = 2.0D0
         DO 10 J = 1, 4
            T2 = T2*T1
            VA = VA+XPAR(J)*T2
            DV = DV+T4*XPAR(J)*T3
            T3 = T3*T1
            T4 = T4+2.0D0
   10    CONTINUE
         VA = VA*EXPVA
         T1 = DV*EXPVA
         T2 = 3.0D0*T*VA
         DVA(1) = T1*(R2-R3)*(R3+R2-2.0D0*R1)-T2
         DVA(2) = T1*(R3-R1)*(R1+R3-2.0D0*R2)-T2
         DVA(3) = T1*(R1-R2)*(R1+R2-2.0D0*R3)-T2
      ENDIF
      RETURN
      END
C*****
C
      SUBROUTINE H3VII (R1,R2,R3,E,DV2)
C
C***********************************************************************
C     Calculates VII correction energy and its 1st derivative.
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3COCM/ PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,
C    *   DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3
      DIMENSION DV2(3)
C
C   Compute B1 function.
C
      R1I = 1.0D0/R1
      R2I = 1.0D0/R2
      R3I = 1.0D0/R3
      COS1 = 0.5D0*R2I*R3I*(R12-R22-R32)
      COS2 = 0.5D0*R1I*R3I*(R22-R12-R32)
      COS3 = 0.5D0*R1I*R2I*(R32-R12-R22)
      WB = 1.0D0+COS1+COS2+COS3
      WB2 = WB*WB
C
C   WB derivatives
C
      WB1P = (R1*R3I-1.0D0)*R2I-R3I-(COS2+COS3)*R1I
      WB2P = (R2*R3I-1.0D0)*R1I-R3I-(COS1+COS3)*R2I
      WB3P = (R3*R2I-1.0D0)*R1I-R2I-(COS1+COS2)*R3I
C
C   EB1 term
C
      EXP1 = EXP(-BETA1*PER)
      EXP3 = EXP(-BETA3*PER)
      EB1T = (XPAR(5)+XPAR(6)*PER)*EXP1
      EB3T = (XPAR(14)+XPAR(15)*PER2)*EXP3
      EB1 = WB*(EB1T+EB3T)
C
C   EB1 derivatives
C
      EB1PR = WB*(-BETA1*EB1T-BETA3*EB3T+XPAR(6)*EXP1+2.0D0*PER*XPAR(15)
     *   *EXP3)
      EB1PWB = EB1T+EB3T
      EB1P1 = EB1PWB*WB1P+EB1PR
      EB1P2 = EB1PWB*WB2P+EB1PR
      EB1P3 = EB1PWB*WB3P+EB1PR
C
C   EB2 term
C
      T1 = BETA2*PER
      EXP2 = EXP(-T1*PER)
      EB2 = WB2*(XPAR(7)+WB*(XPAR(8)+WB*XPAR(9)))*EXP2
C
C   EB2 derivatives
C
      EB2PWB = WB*(2.0D0*XPAR(7)+WB*(3.0D0*XPAR(8)+WB*4.0D0*XPAR(9)))*  
     *   EXP2
      EB2PR = -2.0D0*T1*EB2
      EB2P1 = EB2PWB*WB1P+EB2PR
      EB2P2 = EB2PWB*WB2P+EB2PR
      EB2P3 = EB2PWB*WB3P+EB2PR
C
C   EB4 term
C      EB4A
C
      T2 = XPAR(10)*EXP1
      T3 = WB*XPAR(11)*EXP2
      EB4A = WB*(T2+T3)
C
C      EB4A derivatives
C
      EB4APW = T2+2.0D0*T3
      EB4APR = -WB*(BETA1*T2+2.0D0*T1*T3)
      EB4AP1 = EB4APW*WB1P+EB4APR
      EB4AP2 = EB4APW*WB2P+EB4APR
      EB4AP3 = EB4APW*WB3P+EB4APR
C
C      EB4B
C
      T2 = XPAR(12)*EXP1
      T3 = XPAR(13)*EXP2
      EB4B = WB*(T2+T3)
C
C      EB4B derivatives
C
      EB4BPW = T2+T3
      EB4BPR = -WB*(BETA1*T2+2.0D0*T1*T3)
      EB4BP1 = EB4BPW*WB1P+EB4BPR
      EB4BP2 = EB4BPW*WB2P+EB4BPR
      EB4BP3 = EB4BPW*WB3P+EB4BPR
C
      DR12 = R1-R2
      DR23 = R2-R3
      DR31 = R3-R1
      EQ = DR12*DR12+DR23*DR23+DR31*DR31
      EQP1 = 2.0D0*(DR12-DR31)
      EQP2 = 2.0D0*(-DR12+DR23)
      EQP3 = 2.0D0*(-DR23+DR31)
      RI = R1I+R2I+R3I
      EB4 = EB4A*RI+EB4B*EQ
C
C   EB4 derivatives
C
      EB4P1 = EB4AP1*RI-EB4A/R12+EB4BP1*EQ+EB4B*EQP1
      EB4P2 = EB4AP2*RI-EB4A/R22+EB4BP2*EQ+EB4B*EQP2
      EB4P3 = EB4AP3*RI-EB4A/R32+EB4BP3*EQ+EB4B*EQP3
C
      E = EB1+EB2+EB4
      DV2(1) = EB1P1+EB2P1+EB4P1
      DV2(2) = EB1P2+EB2P2+EB4P2
      DV2(3) = EB1P3+EB2P3+EB4P3
      RETURN
      END
C*****
C
      SUBROUTINE H3VIII (R1,R2,R3,VIII,DV3)
C
C**********************************************************************
C     Calculates VIII correction energy and it 1st derivatives
C***********************************************************************
C
      use h3potcm1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DV3(3)
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,  
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C     COMMON /H3COCM/ PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,
C    *   DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3
C
      IF (S.EQ.0.0D0) THEN
         VIII = 0.0D0
         DV3(1) = 0.0D0
         DV3(2) = 0.0D0
         DV3(3) = 0.0D0
      ELSE
         T1 = RHO-RHOL
         T5 = ALPH2*T1
         EXPV3 = EXP(-T5*T1)
         IF (EXPV3.EQ.0.0D0) THEN
            VIII = 0.0D0
            DV3(1) = 0.0D0
            DV3(2) = 0.0D0
            DV3(3) = 0.0D0
         ELSE
            T1 = S2*CPHI3
            T2 = 1.0D0+S*T1
            T3 = S2*T2
            T4 = CD0+CD1*RHO
            VIII = T3*T4*EXPV3
            TDS = (2.0D0*S*T2+3.0D0*S2*T1)*T4
            TDCPH = S2*S*S2*T4
            TDRHO = T3*(CD1-2.0D0*T5*T4)
            DV3(1) = (TDS*DS1+TDCPH*DCPHI1+TDRHO*DRHO1)*EXPV3
            DV3(2) = (TDS*DS2+TDCPH*DCPHI2+TDRHO*DRHO2)*EXPV3
            DV3(3) = (TDS*DS3+TDCPH*DCPHI3+TDRHO*DRHO3)*EXPV3
         ENDIF
      ENDIF
      RETURN
      END
C*****

C     module h3potcm
C     real(8) :: r1,r2,r3,POTE1,DEDR(3)
C     integer :: nsurf, nder, ndum(8)
C     real(8) :: pote2,dh3u(3),coup(3)

C     real(8) :: ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)

C     real(8) :: PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,
C    *   DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3
C     real(8) :: CORRS(3),DCORRS(3),CORRT(3),DCORRT(3),DAMP(3,3),
C    *   DDAMP(3,3)

C     DATA RHOL / 2.4848D0 /
C     DATA ALPH0,ALPH1,BET0,BET1 / 25.9528D0,1.1868D0,15.7381D0,
C    *                             0.09729D0 /
C     DATA ALPHA5,BETA1,BETA2,BETA3 / 8.2433033D-3,0.53302897D0,       
C    *   0.39156612D-1,0.69996945D0 /
C     DATA ALPH2 / 4.735364D-1 /
C     DATA AL0,AL1,AL2,AL3 / 0.45024472D+1,-0.62467617D+1,0.40966542D+1,
C    *                       0.21813012D+1 /
C     DATA AZ2 / 4.453649D-4 /
C     DATA CD0,CD1 / 6.333404D-3,-1.726839D-3 /
C     DATA CHH / 6.499027D0,1.243991D+2,3285.8D0 /
C     DATA CK0 / -1.372843D-1,-1.638459D-1,-1.973814D-1 /
C     DATA CK1 / 1.011204D0,9.988099D-1,9.399411D-1 /
C     DATA HFD,HFA1,HFA2,HFA3,HFGAM / 0.15796326D0,2.1977034D0,         
C    *   1.2932502D0,0.64375666D0,2.1835071D0 /
C     DATA H2RM,H2R0,H2RMT / 1.401D0,6.928203D0,7.82D0 /
C     DATA SQRT3 / 1.73205080756887D0 /
C     DATA XPAR / -0.9286579D-2,0.2811592D-3,-0.4665659D-5,0.20698D-7,
C    *            0.2903613D+2,-0.2934824D+1,0.7181886D0,-0.3753218D0,
C    *            -0.1114538D+1,0.2134221D+1,-0.4164343D0,0.2022584D0,
C    *            -0.4662687D-1,-0.4818623D+2,0.2988468D0 /
C     end
C     COMMON /POTCM/ R1,R2,R3,POTE1,DEDR(3)
C     COMMON /POT2CM/ POTE2,DH3U(3),COUP(3)
C     COMMON /POTCCM/ NSURF, NDER, NDUM(8)
C
C  Common block for H3 potential parameters, set in BLOCK DATA H3
C
C     COMMON /H3DMCM/ ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1
C    *   ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1,
C    *   HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)
C
C  Common block for coordinates (computed in H3COOR)
C
C     COMMON /H3COCM/ PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,
C    *   DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3
C
C  Common block for 2-body correlation energies and damping factors
C     (computed in H3COR2)
C
C     COMMON /H3CRCM/ CORRS(3),DCORRS(3),CORRT(3),DCORRT(3),DAMP(3,3),
C    *   DDAMP(3,3)

