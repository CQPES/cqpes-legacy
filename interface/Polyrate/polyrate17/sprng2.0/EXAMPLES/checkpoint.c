/****************************************************************************/
/*                ____Demonstrates checkpointing____                        */
/* In a new run, this program initializes a random number stream and prints */
/* a few random numbers. Finally it packs the state of the stream into a    */
/* file. In a continuation of an old run this program reads in the state of */
/* a stream from a file, prints a few random numbers, and stores the final  */
/* state of the stream into a file.                                         */
/****************************************************************************/


#include <stdio.h>

/* Uncomment the following line to get the interface with pointer checking  */
/*#define CHECK_POINTERS                                                    */
 
#include "sprng.h"             /* SPRNG header file                         */

#define SEED 985456376


main(int argc, char *argv[])
{
  int *stream, i, size, nstreams, streamnum;
  double rn;
  FILE *fp;
  char buffer[MAX_PACKED_LENGTH], outfile[80], infile[80], *bytes;
  int j;
  
  int rng_type_ary[] = {SPRNG_LFG, SPRNG_LCG, SPRNG_LCG64, SPRNG_CMRG,\
	        SPRNG_MLFG, SPRNG_PMLCG};
  int gtype;  /*---    */
  
  /****************** Initialization values *********************************/
            
  streamnum = 0;
  nstreams = 1;
  
  /*--- reading in a generator type */
#include "gen_types_menu.h"
  printf("Type in a generator type (integers: 0,1,2,3,4,5):  ");
  scanf("%d", &gtype);
  
  /*********************** Initialize streams *******************************/

  printf("Enter name of file to store final state of the stream:\n");
  scanf("%s", outfile);
  printf("Enter name of file to read from:\n\t(enter 9 for a new run)\n");
  scanf("%s", infile);
  
  if(infile[0] == '9')		/* initialize stream the first time         */
    stream = init_sprng(gtype, \
			streamnum,nstreams,SEED,SPRNG_DEFAULT);
  else           		/* read stream state from file afterwards   */
  {
    fp = fopen(infile,"r");
    fread(&size,1,sizeof(int),fp);
    fread(buffer,1,size,fp);
	printf("Before unpack\n");
    stream = unpack_sprng(buffer);
	printf("After unpack\n");
    fclose(fp);
  }
  
  /*********************** print random numbers *****************************/
            
  printf(" Printing 5 random numbers in [0,1): \n");
  
  for(i=0; i<5; i++)
  {
    rn = sprng(stream);	        /* generate double precision random number  */
    printf("%d  %f\n", i+1, rn);
  }
  
  /************************* store stream state *****************************/
            
  size = pack_sprng(stream,&bytes); /* pack stream state into an array      */
  fp = fopen(outfile,"w");	/* open file to store stream state          */
  if(fp == NULL)
  {
    fprintf(stderr,"Could not open file %s for writing\nCheck path or permissions\n", outfile);
    exit(1);
  }
  fwrite(&size,1,sizeof(int),fp); /* store # of bytes required for storage  */
  fwrite(bytes,1,size,fp);      /* store stream state                       */
  fclose(fp);

  /*************************** free memory **********************************/
            
  free(bytes);			/* free memory needed to store stream state */
  free_sprng(stream);           /* free memory used to store stream state   */
/*}*/
}
