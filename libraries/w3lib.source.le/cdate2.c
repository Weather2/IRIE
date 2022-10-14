#include <time.h>
void cdate2(idate,itime)
int *idate;
int *itime;
{
 struct tm *timestruct;
 time_t clock;
 time(&clock);
 timestruct = localtime(&clock);
 idate[0] = timestruct->tm_mon + 1;
 idate[1] = timestruct->tm_mday;
 idate[2] = timestruct->tm_year; /* years since 1900 */
 idate[3] = timestruct->tm_wday + 1;
 idate[4] = timestruct->tm_yday + 1;
	itime[2] = timestruct->tm_sec;
	itime[1] = timestruct->tm_min;
	itime[0] = timestruct->tm_hour;
  return;
}
