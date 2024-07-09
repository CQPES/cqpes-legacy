C
C   Demonstrates invalid ID handling in inteface with pointer checking  
C This prorgam prints a few random numbers, frees the stream, and then 
C tries to use the stream again.                                       
C

C do not uncomment the following line 
#ifndef CHECK_POINTERS
#define CHECK_POINTERS		
#endif

       program invalid_IDf
       implicit none

#include "sprng_f.h"

       integer streamnum, nstreams, seed, i, junk
       SPRNG_POINTER stream
       real*8 rn

C--- reading in a generator type
       integer gtype
#include "genf_types_menu.h"
       print *,'Type in a generator type (integers: 0,1,2,3,4,5):  '
       read *, gtype
C---

       streamnum = 0
       nstreams = 1
       seed = 985456376
       
C  initialize stream
       stream = init_sprng(gtype,streamnum,nstreams,seed,SPRNG_DEFAULT)
       print *, 'Printing information about new stream'
       junk = print_sprng(stream)

       print *, 'Printing 3 double precision numbers [0,1): '
       do 100 i = 1, 3
          rn = sprng(stream)
          print *, rn
 100   continue

       junk = free_sprng(stream)

      print *, ' Expect a SPRNG error message on the use'
      print *, '  of an invalid stream ID '
       rn = sprng(stream)
       print *, ' sprng returns ', rn,
     &  ' on being given an invalid stream ID'


       end
