c -----------------------------------------------------------------------------
c   SURFACE 4C
c ------------------------------------------------------------------------
c.. The O(3P) + HCl --> OH + Cl MRCI+Q/cc-pvtz/SEC triplet surface of 
c.. B. Ramachandran, J. Senekowitsch and R.E. Wyatt. 

c.. This is OHCl RSW number 4C.

c.. This surface is based on a fit to ab initio data at the MRCI level using 
c.. vtz basis sets. The Davidson corrected energies (MRCI+Q) were scaled in 
c.. a manner very similar to the SEC procedure of Brown and Truhlar 
c.. [Chem. Phys. Lett. 117, 307 (1985)] prior to fitting them to the model.

c.. The rms error of the fit is 0.70 kcal/mol. The estimated reaction barrier 
c.. for the forward reaction = 9.78 kcal/mol (0.424 eV). 
c.. Reaction enthalpy = +0.98 kcal/mol (endothermic) with ZPE.
c ----------------------------------------------------------------------------
c  USEAGE:

c  All arguments are passed through the common block /potcm/. On input,
c  On Input:
c       r(1) = R(O-H)
c       r(2) = R(H-Cl)
c       r(3) = R(Cl-O).
c
c On Output:
c       energy* = Energy in Hartree w.rti to the asymptotic O + HCl minimum.
c       dedr   = Derivatives of the potential w.r. to r(1), r(2), and r(3).

c * If energy with respect to the three separate atom limit is required,
c   see below.

c  NOTE:
c    Before any actual potential energy calculations are made, a single
c    call to prepot must be made:
c       call prepot

c    This computes the asymptotic O + HCl minimum energy and saves it. Later,
c    the potential energy is computed by calling pot:
c       call pot

c ---------------------------------------------------------------------------
      subroutine prepot
       use potcm3

        implicit double precision (a-h, o-z)

c       common /potcm/  r(3), energy, dedr(3)
c       common /potccm/ nsurf, nder, ndum(8)

        save e0

        external diat, VCI

        nsurf=1
        nder=1

c.. Energy of the asymptotic HCl minimum:
        e0 = diat(2, 2.41080042280D0, dum)

        write(6,*) 'PREPOT has been called for the OHCl RSW surface 4c.'
        write(6,*) 'Version date February 15, 1999.  Final version.'
        write(6,*)

        return

        entry pot

c.. Potential with the three-separate atom limit set equal to zero:
c            energy = VCI(r(1), r(2), r(3), dedr)

c.. Potential with the asymptotic HCl minimum set equal to zero:
             energy = VCI(r(1), r(2), r(3), dedr) - e0

        return
        end
c ----------------------------------------------------------------------------
        double precision function VCI (R1, R2, R3, dR)

c.. Evaluate the potential at the given values of R1, R2 and theta.
c.. On input: R1 = R(O-H); R2 = R(H-Cl); theta = O-H-Cl
c.. On output, VCI has the value of the potential in Hartrees, with zero
c.. set equal to the energy of the three separate atom system.
c ----------------------------------------------------------------------------
        implicit double precision (a-h,o-z)

        dimension ip(112), jp(112), kp(112)
        dimension p(112), dR(3), Xp(0:7), Yp(0:7), Zp(0:7)

        save ip,jp,kp,p,b1,re1,b2,re2
        external CFG

	data eps/1.0D-28/
        data (ip(i),i=1,112)/
     . 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     . 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     . 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,
     . 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4,
     . 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5,
     . 5, 6, 6, 6, 6, 6, 6/
        data (jp(i),i=1,112)/
     . 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3,
     . 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 1, 1, 1, 1, 1, 1, 1, 2, 2,
     . 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 1, 1, 1,
     . 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 1, 1, 1,
     . 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 1, 1, 1, 1, 2, 2, 2, 3, 3,
     . 4, 1, 1, 1, 2, 2, 3/
        data (kp(i),i=1,112)/
     . 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5,
     . 0, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 0, 1, 2, 3, 4, 5, 6, 0, 1,
     . 2, 3, 4, 5, 0, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 0, 1, 0, 1, 2,
     . 3, 4, 5, 0, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 0, 1, 0, 0, 1, 2,
     . 3, 4, 0, 1, 2, 3, 0, 1, 2, 0, 1, 0, 0, 1, 2, 3, 0, 1, 2, 0, 1,
     . 0, 0, 1, 2, 0, 1, 0/
        data (p(i),i=1,112)/
     . -0.870523095250D-02,  0.400356765754D+00,  0.303606482023D+00,
     . -0.409616357451D-03,  0.948189559622D-02,  0.922719829127D-01,
     . -0.109172041200D+00, -0.856827066938D-01, -0.355266217526D+00,
     . -0.766109767725D+00, -0.829022280292D+00, -0.421680942152D+00,
     . -0.752169575585D-01, -0.550864446303D-01, -0.117573103101D-01,
     .  0.406581482956D+00,  0.573698082811D+00,  0.494427437923D+00,
     .  0.233362243972D+00,  0.492650558830D-01,  0.113892625246D-01,
     . -0.154676093992D+00, -0.241386442954D+00, -0.140669466432D+00,
     . -0.320022622696D-01, -0.239946585190D-02,  0.188972654546D-01,
     .  0.538893604152D-01,  0.248300149748D-01,  0.105621190160D-02,
     .  0.347842352143D-03, -0.471075369050D-02, -0.215729831360D-02,
     .  0.366892972667D+00, -0.111568553643D+01, -0.585326502872D+00,
     .  0.679897165818D-01,  0.153756984419D+00, -0.817992778908D-01,
     . -0.176704322921D-01,  0.364219704804D+00,  0.118505974406D+01,
     .  0.870141697306D+00,  0.266833715563D+00, -0.838634251257D-01,
     . -0.149998633272D-01, -0.501293198269D+00, -0.441461978966D+00,
     . -0.296168161587D+00, -0.139033873742D+00, -0.125459008043D-01,
     .  0.140717672318D+00,  0.104051824434D+00,  0.250285055267D-01,
     .  0.107234607521D-01,  0.180180457716D-02, -0.208449947438D-01,
     . -0.364825637734D-03, -0.396159835158D-02,  0.163634086994D-02,
     . -0.812935356520D+00,  0.104768770128D+01,  0.300318825328D+00,
     . -0.589953284070D-01, -0.871636548717D-01,  0.221940814313D-01,
     .  0.192735280683D+00, -0.996266495093D+00, -0.397756630609D+00,
     . -0.871782035648D-01,  0.211001010218D-01,  0.228345525656D+00,
     .  0.235376432933D+00,  0.848597458792D-01,  0.153550904565D-01,
     . -0.827238664103D-01, -0.145618258495D-01, -0.230803140781D-02,
     .  0.408746307820D-03,  0.147387253989D-02,  0.213510155850D-02,
     .  0.669875529913D+00, -0.423548490808D+00, -0.983196840777D-02,
     .  0.271724319514D-01,  0.222427003967D-01, -0.407102097129D+00,
     .  0.406782569876D+00,  0.667630561224D-01,  0.162137391173D-01,
     .  0.137598057033D-01, -0.787804704261D-01, -0.106508547645D-01,
     .  0.233008904857D-01,  0.674772493425D-03, -0.371085846306D-02,
     . -0.234889374566D+00,  0.722967890636D-01, -0.221679107703D-01,
     . -0.362591104822D-02,  0.174426632527D+00, -0.730342398552D-01,
     .  0.803320640414D-03, -0.332360049130D-01,  0.106662543758D-01,
     .  0.141898236904D-02,  0.293726812253D-01, -0.386746784948D-02,
     .  0.277622976556D-02, -0.215729949344D-01,  0.419154763367D-02,
     .  0.362797649049D-02/
        data b1, re1/ 0.132568728000D+01,  0.182694367620D+01 /
	data b2, re2/ 0.107872541250D+01,  0.241080042280D+01 /
	data b3, re3 / 0.132282603750D+01, 0.296617200000D+01 /


c.. Diatomic potentials (and derivatives):
        VCI = diat(1, R1, dR(1)) + diat(2, R2, dR(2))
     .		+ diat (3, R3, dR(3))

c.. Coordinates for expanding the three-body potential [coordinates of
c.. Aguado and Paniagua with a minor modification, Ref. J. Chem. Phys.
c.. 96, 1265 (1990)]:
        X = R1*dexp(-b1*(R1-re1))
        Y = R2*dexp(-b2*(R2-re2))
        Z = CFG(R1, R2, R3)

c.. Add a small constant to Z to avoid floating point overflow at 90 deg.
c.. where cosine is zero. (In principle, the singularity will be removed
c.. by the cosine terms in the numerator of the expansion.)

        dR1 = (1.d0/(Z*R2) - 1.d0/R1)
        dR2 = (1.d0/(Z*R1) - 1.d0/R2)
        dR3 = R3/(R1*R2*Z)
        dT1 = (1.d0/R1-b1)
        dT2 = (1.d0/R2-b2)

        Xp(0)=1.d0
        Yp(0)=1.d0
        Zp(0)=1.d0
        do i=1,7
            Xp(i)=Xp(i-1)*X
            Yp(i)=Yp(i-1)*Y
            Zp(i)=Zp(i-1)*Z
        enddo

c.. Compute the potential:
        do n = 1,112
            term=p(n)*Xp(ip(n))*Yp(jp(n))*Zp(kp(n))
            VCI=VCI+term
            dR(1)=dR(1)+term*(dble(ip(n))*dT1+dble(kp(n))*dR1)
            dR(2)=dR(2)+term*(dble(jp(n))*dT2+dble(kp(n))*dR2)
            dR(3)=dR(3)-term*dble(kp(n))*dR3
        enddo

        return
        end
c ----------------------------------------------------------------------------
      double precision function diat (id, r, dr)

c.. Computes and returns the diatomic potential at the provided value of the
c.. bond order coordinate. The model used is that of Garcia and Lagana,
c.. which may be called the "extended Morse" model.
c.. Ref.: Mol. Phys. 56, 621 (1985)].
c ----------------------------------------------------------------------------
      implicit real*8 (a-h, o-z)
      dimension p1(7), p2(7), p3(7)

      save p1, p2, p3
c.. The OH molecule parameters:
      data p1/2.69104191D0,-2.54552087D0,1.01791599D0,-0.16343704D0,
     .  0.16981780D0,1.76758304D0,1.8269436762D0/
c.. The HCl molecule parameters:
      data p2/2.69104191D0,-2.54552086D0,1.01791599D0,-0.16343704D0,
     .  0.16968172D0,1.43830055D0,2.4108004228D0/
c.. The ClO molecule parameters:
      data p3/2.69104191D0,-2.54552086D0,1.01791599D0,-0.16343704D0,
     .  0.10301553D0,1.76376805D0,2.966172D0/

        diat = 0.d0
        dr = 0.d0

	if (id .eq. 1) then
c.. Diatomic potential function of OH
	    x = dexp(-p1(6)*(r-p1(7)))
	    do i = 1,4
		term = p1(i)*(x**i)
		diat = diat + term
		dr = dr + dble(i)*term
	    enddo
	    diat = -p1(5)*diat
	    dr = p1(5)*p1(6)*dr
	else if (id. eq. 2) then
c.. Diatomic potential function of HCl
	    x = dexp(-p2(6)*(r-p2(7)))
	    do i = 1,4
		term = p2(i)*(x**i)
		diat = diat + term
		dr = dr + dble(i)*term
	    enddo
	    diat = -p2(5)*diat
	    dr = p2(5)*p2(6)*dr
	else if (id. eq. 3) then
c.. Diatomic potential function of ClO
	    x = dexp(-0.5d0*p3(6)*(r-p3(7)))
	    term = p3(2)*(x**2)+p3(4)*(x**4)
	    dr = 2.0d0*p3(2)*(x**2) + 4.0d0*p3(4)*(x**4)
	    diat = -p3(5)*term
	    dr = 0.5d0*p3(5)*p3(6)*dr
	endif

      return
      end
c-----------------------------------------------------------------------------
      double precision function CFG(a,b,c)

c Given three sides of a triangle A,B,C, find the cosine of the angle A-B-C.
c-----------------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      a2=a**2
      b2=b**2
      c2=c**2
      if (a2.eq.0.0d0) then
        if (b2-c2.lt.0.0d0) then
          x=-1.0d0
        else
          x=1.0d0
        endif
      else if (b2.eq.0.0d0) then
        if (a2-c2.lt.0.0d0) then
          x=-1.0d0
        else
          x=1.0d0
        endif
      else
        x=(a2+b2-c2)/(2.d0*a*b)
        if (x.gt.1.0d0) x=1.0d0
        if (x.lt.-1.0d0) x=-1.0d0
      endif
      CFG=x
      return
      end
