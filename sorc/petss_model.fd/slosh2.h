#ifndef SLOSH2_H
#define SLOSH2_H
#define PROGRAM_VERSION "4.11a"
#define PROGRAM_DATE "06/05/2013"
#ifndef PROGRAM_COMMENT
#define PROGRAM_COMMENT ""
#endif

#include "halotype.h"
#include <stdio.h>
#include "tideutil.h"         /* Tide functions. */
#include "rex.h"

/* MY_MAX_PATH is 256 + 1 (null chacater) */
#ifndef MY_MAX_PATH
 #define MY_MAX_PATH 257
#endif
/* Typically defined in /MinGW-3.4.2/include/windef.h as 260. */
/* Needs to agree between C and intrface.f so easier to set at 257. */

#define WNDU 200

/*
#define DOUBLE_FORTRAN
*/
/* #define BAS_X 600 (see halotype.h) */
/* #define BAS_Y 600 (see halotype.h) */

/*
typedef struct {
  double sec;
  int day, hour, min, month, year, tday;
} time_type;
*/

/* Fortran intrface variables... */
typedef struct {
#ifdef DOUBLE_FORTRAN
  double zb[BAS_Y][BAS_X];
  double hb[BAS_Y][BAS_X];
  double hb_max[BAS_Y][BAS_X];
  double hb_old[BAS_Y][BAS_X];
  double tide[BAS_Y][BAS_X];
  double tide_V1max[BAS_Y][BAS_X]; /* Used only for Tide Version 1,2 */
/* Instantaneous Track information...*/
  double storm_lat, storm_lon;
  double wspeed, wdirect, delp, size2;
  int    mask[BAS_Y][BAS_X];    /* mask for tide only runs */
/*  char rexBuff[BAS_X*BAS_Y*2];*/
#else
  float zb[BAS_Y][BAS_X];
  float hb[BAS_Y][BAS_X];
  float hb_max[BAS_Y][BAS_X];
  float hb_old[BAS_Y][BAS_X];
  float tide[BAS_Y][BAS_X];
  float tide_V1max[BAS_Y][BAS_X]; /* Used only for Tide Version 1,2 */
/* Instantaneous Track information...*/
  float storm_lat, storm_lon;
  float wspeed, wdirect, delp, size2;
  int   mask[BAS_Y][BAS_X];    /* mask for tide only runs--- 1: calculate tide 0: don't calculate tide*/
/*  char rexBuff[BAS_X*BAS_Y*2];*/
#endif
} slosh_type;

int InitWater_TideModeOverride (float *ht1, int f_tide, float *ht2);

void RunLoopStep (char * bsnAbrev, slosh_type * st, int imxb, int jmxb,
                  int *itime, int *mhalt,
                  short csflag, short f_smooth, 
                  short f_wantRex, double *modelClock, double rextime,
                  int f_first, TideGridType *tgrid, int f_tide, int nest, 
                  int rexOption, int bounOption, double Tf, int f_stat, 
                  double tidetime, int tideCoeff);

int CleanUp (slosh_type *st, int imxb, int jmxb, int f_saveEnv, int f_tide,
             char *envName, char envComment[161], float ht1, float ht2,
             TideGridType *tgrid);

int RunInit (char * bsnAbrev, slosh_type * st, char trkName[MY_MAX_PATH], char dtaName[MY_MAX_PATH],
             char envName[MY_MAX_PATH], char ft40Name[MY_MAX_PATH], int *mhalt,
             double *modelClock, int f_tide, int nest, int tideThresh, int tideCoeff, char *ft03Name, char *tideName,
             TideGridType *tgrid, char *adjDatumName, int spinUp, int f_saveSpinUp,
             rexType *rex, char f_wantRex, int rexSaveMin);

int ReadTrkFile (char trkName[MY_MAX_PATH], char rexComment[201],
                 char envComment[161], float *ht1, float *ht2);

int PerformRun (char *bsnAbrev, char dtaName[MY_MAX_PATH], char trkName[MY_MAX_PATH],
                char envName[MY_MAX_PATH], char *rexName, char *tideDir, int imxb, int jmxb, int bsnStatus,
                int rexSaveMin, sChar verbose, int rexOption, int bounOption, double Tf, 
                int f_tide, int nest, int flood, int tideThresh, int tideCoeff, int f_stat, int spinUp, int f_saveSpinUp, double asOf);
/* Huiqing Liu /MDL Nov. 2013 added nesting options "nest" */
/* Huiqing Liu /MDL Jan. 2014 added wet&dry options "flood" */
/* Huiqing Liu /MDL Jan. 2014 added output current to rexfile options "rexOption" */
/* Huiqing Liu /MDL Jan. 2016 added "timetime" time interval to add and subtract tide*/

#endif
