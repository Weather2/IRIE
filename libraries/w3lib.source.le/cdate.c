#include <time.h>
void cdate(idate,itime)
int *idate;
int *itime;
{
 struct tm *timestruct;
 time_t clock;
 time(&clock);
 timestruct = localtime(&clock);
 idate[0] = timestruct->tm_mon + 1;
 idate[1] = timestruct->tm_mday;
 idate[2] = timestruct->tm_year;
	itime[2] = timestruct->tm_sec;
	itime[1] = timestruct->tm_min;
	itime[0] = timestruct->tm_hour;
  return;
}
