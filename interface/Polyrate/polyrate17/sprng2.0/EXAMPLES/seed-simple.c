/****************************************************************************/
/*               ____Demonstrates the use make_sprng_seed____               */
/* 'make_sprng_seed' is used to produce a new seed each time the program is */
/* run. Then a few random numbers are printed.                              */
/****************************************************************************/

#include <stdio.h>

#define SIMPLE_SPRNG		/* simple interface                         */
#include "sprng.h"              /* SPRNG header file                        */



main()
{
  int i, seed;
  double rn;
  int gtype;  /*---    */


  /*--- reading in a generator type */
#include "gen_types_menu.h"
  printf("Type in a generator type (integers: 0,1,2,3,4,5):  ");
  scanf("%d", &gtype);


  seed = make_sprng_seed();	/* make new seed each time program is run   */

  init_sprng(gtype,seed,SPRNG_DEFAULT);	/* initialize stream                        */
  printf(" Printing information about new stream\n");
  print_sprng();

  printf(" Printing 3 random numbers in [0,1):\n");
  for (i=0;i<3;i++)
  {
    rn = sprng();		/* generate double precision random number */
    printf("%f\n", rn);
  }

}
