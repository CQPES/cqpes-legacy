/****************************************************************************/
/*               ____Demonstrates the use make_sprng_seed____               */
/* 'make_sprng_seed' is used to produce a new seed each time the program is */
/* run. Then a few random numbers are printed.                              */
/****************************************************************************/

#include <stdio.h>

/* Uncomment the following line to get the interface with pointer checking */
/*#define CHECK_POINTERS                                                   */
 
#include "sprng.h"              /* SPRNG header file                       */

main()
{
  int streamnum, nstreams, seed, *stream, i;
  double rn;
  int j;
  int gtype;  /*---    */


  /*--- reading in a generator type */
#include "gen_types_menu.h"
  printf("Type in a generator type (integers: 0,1,2,3,4,5):  ");
  scanf("%d", &gtype);
 
/*  int rng_type_ary[] = {SPRNG_LFG, SPRNG_LCG, SPRNG_LCG64, SPRNG_CMRG,\
	        SPRNG_MLFG, SPRNG_PMLCG};

for(j = 0; j < 6; j++){
*/
  /************************** Initialization *******************************/

  streamnum = 0;
  nstreams = 1;

  seed = make_sprng_seed();	/* make new seed each time program is run  */

  stream = init_sprng(gtype, \
		  streamnum,nstreams,seed,SPRNG_DEFAULT); /*initialize stream*/
  printf(" Printing information about new stream\n");
  print_sprng(stream);

  /************************ print random numbers ***************************/

  printf(" Printing 3 random numbers in [0,1):\n");
  for (i=0;i<3;i++)
  {
    rn = sprng(stream);		/* generate double precision random number */
    printf("%f\n", rn);
  }

  free_sprng(stream);		/* free memory used to store stream state  */
/*
}
*/
}
