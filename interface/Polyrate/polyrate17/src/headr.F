C***********************************************************************
C  HEADR
C***********************************************************************
C
      SUBROUTINE headr
      use perconparam, only : FU6
C
C     PRINTS OUT PROGRAM HEADER TO UNIT FU6
C
C     CALLED BY:
C                MAIN
C
C
C   INCLUDE FILE ADDED 15/09.11
C   FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      WRITE (FU6,1000)
      WRITE (FU6,1100)
      WRITE (FU6,1200)
      WRITE (FU6,1300)
      WRITE (FU6,1400)
      WRITE (FU6,1500)
      WRITE (FU6,1600)
      WRITE (FU6,1700)
      WRITE (FU6,1800)
      WRITE (FU6,1300)
      WRITE (FU6,1900)  
      WRITE (FU6,2000)
      WRITE (FU6,1000)
C
C
      RETURN
C
 1000 FORMAT(/,80(1H*))

 1100 FORMAT(/3X,'POLYRATE17 revision C (Oct. 6, 2018)',/)

 1200 FORMAT(3X,'Jingjing Zheng, Junwei Lucas Bao, Ruben Meana-Paneda,',
     *      /3X,'Shuxia Zhang, Benjamin J. Lynch, Jose C. Corchado,',
     *      /3X,'Yao-Yuan Chuang, Patton L. Fast, Wei-Ping Hu,',
     *      /3X,'Yi-Ping Liu, Gillian C. Lynch, Kiet A. Nguyen,',
     *      /3X,'Charles F. Jackels, Antonio Fernandez Ramos,',
     *      /3X,'Benjamin A. Ellingson, Vasilios S. Melissas,',
     *      /3X,'Jordi Villa, Ivan Rossi, Elena L. Coitino,',
     *      /3X,'Jingzhi Pu, and Titus V. Albu',/)

 1300 FORMAT(3X,'Department of Chemistry, Chemical Theory Center, ',
     *      /3X,'and Supercomputing Institute, '
     *      /3X,'University of Minnesota, Minneapolis, Minnesota',/)

 1400 FORMAT(3X,'Artur Ratkiewicz',
     *      /3X,'Institute of Chemistry, University of Bialystok, ',
     *          'Poland',/)

 1500 FORMAT(3X,'Rozeanne Steckler',
     *      /3X,'Northwest Alliance for Computational Science &',
     *          ' Engineering',
     *      /3X,'Oregon State University, Corvallis, Oregon',/)

 1600 FORMAT(3X,'Bruce C. Garrett',
     *      /3X,'Environmental Molecular Sciences Laboratory',
     *      /3X,'Pacific Northwest Laboratory, Richland, Washington',/)

 1700 FORMAT(3X,'Alan D. Isaacson',
     *      /3X,'Department of Chemistry, Miami University, ',
     *          'Oxford, Ohio',/)

 1800 FORMAT(3X,'and Donald G. Truhlar')    
 1900 FORMAT(3X,'A program for the analysis of reactants,', 
     *          ' products, transition states, and',/3X,
     *          'generalized transition states, and for the',   
     *          ' calculation of variational',/3X, 
     *          'transition state theory rate constants and', 
     *          ' multidimensional semiclassical',/,3X,'tunneling ', 
     *          'probabilities, including direct dynamics.',//) 

 2000 FORMAT(3X,'Copyright 1988-2017',
     *      /3X,'Donald G. Truhlar and ',
     *          'Regents of the University of Minnesota')
C
      END
