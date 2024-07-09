/***************************************************************************/
/*            ____Demonstrates the use of sprng and isprng____             */
/* A random number stream is initialized and a few random double precision */
/* numbers and a few integers are printed.                                 */
/***************************************************************************/

#include <stdio.h>

/* Uncomment the following line to get the interface with pointer checking */
/*#define CHECK_POINTERS                                                   */
 
#include "sprng.h"  /* SPRNG header file                                   */

#define SEED 985456376


struct rngen
{
	int rng_type;
}

main()
{
  int streamnum, nstreams, *stream;
  double rn;
  int irn;
  int i, j;
  int gtype;  /*---    */


  /*--- reading in a generator type */
#include "gen_types_menu.h"
  printf("Type in a generator type (integers: 0,1,2,3,4,5):  ");
  scanf("%d", &gtype);


/*---
  int rng_type_ary[] = {SPRNG_LFG, SPRNG_LCG, SPRNG_LCG64, SPRNG_CMRG,\
	  SPRNG_MLFG, SPRNG_PMLCG};

  for(j = 0; j < 7; j++){
*/
/****************** Initialization values ****************************/
           
 		streamnum = 0;
 		nstreams = 1;

  		stream = init_sprng(gtype, \
				streamnum,nstreams,SEED,SPRNG_DEFAULT); 
														/* initialize stream */
  		printf("\n --------------------------------------------------------\n");
  		printf(" Print information about new stream:\n");
  		print_sprng(stream);	
    printf("rng_type is %d\n",((struct rngen *)stream)->rng_type);

  		/*********************** print random numbers ************************/
		
  		printf(" Printing 3 random numbers in [0,1):\n");
  		for (i=0;i<3;i++)
  		{
    		rn = sprng(stream);	/* generate a double precision random number */
    		printf("%f\n",rn);
  		}
		
  		printf(" Printing 3 random integers in [0,2^31):\n");
  		for (i=0;i<3;i++)
  		{
    		irn = isprng(stream);/* generate an integer random number */
    		printf("%16d\n",irn);
  		}
    	printf("rng_type is %d\n",((struct rngen *)stream)->rng_type);

  		/*************************** free memory *****************************/

  		free_sprng(stream);  /* free memory used to store stream state */
/*---  
}
*/
}
