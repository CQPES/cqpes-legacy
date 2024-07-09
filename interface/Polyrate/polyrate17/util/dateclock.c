#include <stdio.h>

/* the shell_date function retrieves the UNIX 'date' command's output */
/* by Jason Goeppinger (goepping@s1.msi.umn.edu) 10/10/91 */

/* int shdate(char *result) */
int shdate_(result)
char *result;
{
  FILE *fp;
  int status = -1;
  sscanf("Time/date unavailable","%[A-Z a-z0-9:/]",result);

  if((fp = popen("/bin/date","r")) != NULL)
  {
    fscanf (fp,"%[A-Z a-z0-9:/]",result);
    pclose(fp);
    status = 0;
  }
  return status;
}
 
