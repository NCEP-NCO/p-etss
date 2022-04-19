#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "usrparse.h"
#include "myutil.h"
#include "clock.h"

#ifdef MEMWATCH
#include "memwatch.h"
#endif

/*****************************************************************************
*****************************************************************************/
int ParseTide (const char *next, int *f_tide, int *tideThresh, int *tideCoeff)
{
   const char *split="";
   char arr[3]="";
   *tideThresh = 0;
   *tideCoeff = 0;
   if (strcmp (next, "0") == 0) {
      *f_tide = 0;
   } else if (strcmp (next, "T1") == 0) {
      *f_tide = -1;
   } else if (strcmp (next, "V1") == 0) {
      *f_tide = 1;
   } else if (strcmp (next, "V2") == 0) {
      *f_tide = 2;
   } else if (strcmp (next, "V3") == 0) {
      *f_tide = 3;
   } else if (strcmp (next, "V4") == 0) {
      *f_tide = 4;
   } else if (strcmp (next, "VDEF") == 0) {
      *f_tide = 99;
   } else if (strncmp (next, "V2.1", 4) == 0) {
      if (strlen (next) != 4) {
         *tideThresh = atoi (next + 5);
      }
      *f_tide = 21;
   } else if (strncmp (next, "V2.2", 4) == 0) {
      if (strlen (next) != 4) {
         *tideThresh = atoi (next + 5);
      }
      *f_tide = 22;
   } else if (strncmp (next, "V2.4", 4) == 0) {
      if (strlen (next) != 4) {
         *tideThresh = atoi (next + 5);
      }
      *f_tide = 24;
/* added by Huiqing Liu /MDL Nov. 2013 for V3.1 tide version */
   } else if (strncmp (next, "V3.1", 4) == 0) {
      if (strlen (next) != 4) {
         *tideThresh = atoi (next + 5);
      }
      *f_tide = 31;
/* added by Huiqing Liu /MDL Nov. 2016 for V4.X tide version */
   } else if (strncmp (next, "V4.1", 4) == 0) {
      if (strlen (next) != 4) {
     //    *tideThresh = atoi (next + 5);
         split = next + 5;
         strncpy(arr, split, 2);
	 arr[2]='\0';
         *tideCoeff = atoi(arr);
      //   printf("tideCoeff=%d\n",*tideCoeff);
         *tideThresh = atoi (next + 8);
      //   printf("tideThresh=%d\n",*tideThresh);
      }
      
      *f_tide = 41;
   } else if (strncmp (next, "V4.2", 4) == 0) {
      if (strlen (next) != 4) {
      //   *tideThresh = atoi (next + 5);
         split = next + 5;
         strncpy(arr, split, 2);
	 arr[2]='\0';
         *tideCoeff = atoi(arr);
         *tideThresh = atoi (next + 8);
      }
      *f_tide = 42;
   } else if (strncmp (next, "V4.4", 4) == 0) {
      if (strlen (next) != 4) {
      //   *tideThresh = atoi (next + 5);
         split = next + 5;
         strncpy(arr, split, 2);
	 arr[2]='\0';
         *tideCoeff = atoi(arr);
         *tideThresh = atoi (next + 8);
      }
      *f_tide = 44;

   } else {
      fprintf (stderr, "invalid -f_tide option '%s'\n", next);
      fprintf (stderr, "Try 0, T1, V1, V2, V2.1.X, V2.2.X, V2.4.X, V3,V3.1, V4, V4.1.X, V4.2.X, V4.4.X, VDEF\n");
      return -1;
   }
   return 0;
}

/*****************************************************************************
*****************************************************************************/
void UserInit (userType *usr)
{
   usr->cmd = 0;
   usr->verbose = 1;
   usr->basin = NULL;
   usr->rootDir = NULL;
   usr->bsnDir = NULL;
   usr->bntDir = NULL;
   usr->rexDir = NULL;
   usr->envDir = NULL;
   usr->f_appendBsn = 1;
   usr->tideDir = NULL;
   usr->trkFile = NULL;
   usr->rexFile = NULL;
   usr->envFile = NULL;
   usr->lstFile = NULL;
   usr->doneFile = NULL;
   usr->lstType = 0;
   usr->rexSaveMin = 10;
   usr->rexOption = 0;     /* Huiqing Liu /MDL Jan. 2014*/
   usr->bounOption = 0;     /* Huiqing Liu /MDL Jan. 2014*/
   usr->Tf = 1.;     /* Huiqing Liu /MDL Jan. 2014*/
   usr->f_tide = 0;   /* Default to no tides. */
   usr->nest = 0;     /* Huiqing Liu /MDL  Dec. 2013*/
   usr->flood = 1;     /* Huiqing Liu /MDL Jan. 2014*/
   usr->tidedatabase = 2012; /* Default to ec2012 database. */
   usr->f_stat = 0;   /* Default to instantaneous saves. */
   usr->asOf = 0;     /* Default to no as of time. */
   usr->spinUp = 0;
   usr->f_saveSpinUp = 0;
}

/*****************************************************************************
*****************************************************************************/
void UserFree (userType *usr)
{
   if (usr->basin != NULL) {
      free (usr->basin);
   }
   if (usr->rootDir != NULL) {
      free (usr->rootDir);
   }
   if (usr->bsnDir != NULL) {
      free (usr->bsnDir);
   }
   if (usr->bntDir != NULL) {
      free (usr->bntDir);
   }
   if (usr->envDir != NULL) {
      free (usr->envDir);
   }
   if (usr->rexDir != NULL) {
      free (usr->rexDir);
   }
   if (usr->tideDir != NULL) {
      free (usr->tideDir);
   }
/* Huiqing.Liu /MDL June 2014 don't need trk information */
   if (usr->trkFile != NULL) {
      free (usr->trkFile);
   } 
   if (usr->rexFile != NULL) {
      free (usr->rexFile);
   }
   if (usr->envFile != NULL) {
      free (usr->envFile);
   }
   if (usr->lstFile != NULL) {
      free (usr->lstFile);
   }
   if (usr->doneFile != NULL) {
      free (usr->doneFile);
   }
   UserInit (usr);
}

/*****************************************************************************
*****************************************************************************/
static char *UsrOpt[] = { "-help", "-V", "-verbose", "-basin",
   "-rootDir", "-trk", "-rexDir", "-rex", "-envDir", "-env", "-f_appendBsn",
   "-lst", "-lstType", "-doneFile", "-rexSave", "-rexOption", "-bounOption", "-Tf", "-f_tide", "-nest", "-flood","-TideDatabase",
   "-f_stat", "-spinUp", "-f_saveSpinUp", "-asOf", NULL
}; /*Huiqing Liu /MDL */

void Usage (const char *argv0)
{
   static char *UsrDes[] = { "(1 arg) usage or help command",
      "(1 arg) version command",
      "verbose level (0 no print statements; [1] some; 2 lots",
      "abbreviation for basin",
      "directory containing subdirs of /dta (sloshbsn info)\n"
         "\t\t/bnt (basin grid def) and /tidefile (tide info)",
      "100 point trk filename",
      "Name of rex directory (assuming a number of runs (lstType 1)",
      "output rex filename\n"
         "\t\t(if using lst, non-null means add .rex to trk name)",
      "Name of env directory (assuming a number of runs (lstType 1)"
      "output envelope filename\n"
         "\t\t(if using lst, non-null means add .env to trk name)",
      "Flag to append Basin Abbrev to rexDir and envDir [1]",
      "input list file to run several storms",
      "version of list file:\n"
         "\t\t[0] = match GUI list, 1 P-Surge list",
      "File whose existance indicates list file is complete",
      "File to look for when lst is complete.",
      "how often (in minutes) to save to the rex file [10]\n"
         "\t\t!!! RECOMMEND MULTIPLE OF 10 !!!",
      "Rex option:\n"
         "\t\t[0] = output surge to rex file (default value);\n"
         "\t\t 1  = output current magnitude to rex file", /* Huiqing Liu /MDL Jan. 2014 */
      "Boundary option:\n"
         "\t\t[0] = Static Height Boundary (default value);\n"
         "\t\t 1  = Partial Clamped Radiation Boundary", /* Huiqing Liu /MDL Jan. 2014 */
      "Damping Time (Hr):\n"
         "\t\t Damping time (Hr) (default value is set 1. hour)", /* Huiqing Liu /MDL Jan. 2014 */
      "tide options:\n"
         "\t\t[0] = surge only, T1 = tideV1 only,\n"
         "\t\tV1 = tideV1+surge, V2 = tideV2+surge, V3 = tideV3+surge,\n"
         "\t\tV2.1.{ht} = tideV2+surge(for depths < -${ht} ft) and > -290 ft\n"
         "\t\t  For <= -290 feet use tide + staticHt,\n"
         "\t\tV2.2.{ht} = similar to V2.1 except exclude subgrid cells,\n"
         "\t\tV2.4.{ht} = similar to V2.2.{ht} except remove the -290 feet limit,\n"
         "\t\tV3 = tideV3+surge by adding tidal forcing at deep water boundary grids,\n"
         "\t\tV3.1 = similar to V3 except adding tidal forcing at all water boundary grids,\n"
         "\t\tV4.1.{ht} = tideV2+surge(for depths < -${ht} ft) and > -290 ft\n"
         "\t\t  For <= -290 feet use tide + staticHt,\n"
         "\t\tV4.2.{ht} = similar to V2.1 except exclude subgrid cells.\n"
         "\t\tV4.4.{ht} = similar to V2.2.{ht} except remove the -290 feet limit.",
      "nesting option:\n"
         "\t\t[0] = normal run (default value);\n"
         "\t\t 1  = outer domain outputing nesting boundary for future inner domain runs;\n"
         "\t\t 2  = inner domain reading nesting boundary output from outer domain runs;\n" /*Huiqing Liu /MDL */
         "\t\t21  = inner domain reading nesting boundary output from outer domain runs,but only in deep water grids;\n" /*Huiqing Liu /MDL */
         "\t\t 3  = generate boundary grids total number and I & J index file;\n" /*Huiqing Liu /MDL */
         "\t\t31  = generate boundary grids total number and I & J index file,but only in deep water areas", /*Huiqing Liu /MDL */
      "flooding option:\n"
         "\t\t 0  = without overland flooding (same as ETSS);\n"
         "\t\t[1] = normal run with overland flooding (default value)", /* Huiqing Liu /MDL Jan. 2014 */
      "tidal database to use:\n"
         "\t\t[2012] = use ec2012, 2001 = use ec2001",
      "statistical method:\n"
         "\t\t[0] = save instantaneous values to rexfile\n"
         "\t\t1 = save max values over rexSave time intervals to rex/env",
      "number of hours of tidal spin up [0]",
      "Do we want to save the spin up to the rexfile ([0]=no, 1=yes)",
      "Date/Time (UTC) of when the model is no longer a hindcast.",
      NULL
   };
   unsigned int i, j;
   char buffer[21];
   unsigned int blanks = 15;

   fprintf (stderr, "Usage: %s [OPTION]...\n", argv0);
   fprintf (stderr, "\nOptions:\n");
   for (i = 0; i < sizeof (UsrOpt) / sizeof (UsrOpt[0]) - 1; i++) {
      if (strlen (UsrOpt[i]) <= blanks) {
         for (j = 0; j < blanks; j++) {
            if (j < strlen (UsrOpt[i])) {
               buffer[j] = UsrOpt[i][j];
            } else {
               buffer[j] = ' ';
            }
         }
         buffer[blanks] = '\0';
         fprintf (stderr, "%s %s\n", buffer, UsrDes[i]);
      } else {
         fprintf (stderr, "%s %s\n", UsrOpt[i], UsrDes[i]);
      }
   }
   fprintf (stderr, "\nSimplest way to run requires: -basin, -rootDir, "
            "-rex, -env\n");
   fprintf (stderr, "Remember to 'cd' to your working directory.\n\n");
}

/*****************************************************************************
*****************************************************************************/
static int ParseUserChoice (userType *usr, char *cur, char *next)
{
   enum { HELP, VERSION, VERBOSE, BASIN, ROOTDIR, TRKFILE, REXDIR, REXFILE,
      ENVDIR, ENVFILE, F_APPENDBSN, LSTFILE, LSTTYPE, DONEFILE, REXSAVEMIN,
      REXOPTION, BOUNOPTION, TF, F_TIDE, NEST, FLOOD, TIDEDATABASE, F_STAT, SPINUP, F_SAVESPINUP, ASOF
   };
/* Huiqing Liu /MDL Jan. 2014 Adding wet&dry options "FLOOD" and nesting options "NEST" */
   int index;           /* "cur"'s index into Opt, which matches enum val. */

   /* Figure out which option. */
   if (GetIndexFromStr (cur, UsrOpt, &index) < 0) {
      printf ("Invalid option '%s'\n", cur);
      return -1;
   }
   /* Handle the 1 argument options first. */
   switch (index) {
      case VERSION:
         usr->cmd = 1;
         return 1;
      case HELP:
         usr->cmd = 2;
         return 1;
   }
   /* It is definitely a 2 argument option, so check if next is NULL. */
   if (next == NULL) {
      printf ("%s needs another argument\n", cur);
      return -1;
   }
   /* Handle the 2 argument options. */
   switch (index) {
      case BASIN:
         if (usr->basin != NULL) {
            free (usr->basin);
         }
         usr->basin = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->basin, next);
         return 2;
      case ROOTDIR:
         if (usr->rootDir != NULL) {
            free (usr->rootDir);
         }
         usr->rootDir = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->rootDir, next);
         return 2;
/* Huiqing.Liu /MDL June 2014 don't need trk info */
      case TRKFILE:
         if (usr->trkFile != NULL) {
            free (usr->trkFile);
         }
         usr->trkFile = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->trkFile, next);
         return 2;
      case REXDIR:
         if (usr->rexDir != NULL) {
            free (usr->rexDir);
         }
         usr->rexDir = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->rexDir, next);
         return 2;
      case REXFILE:
         if (usr->rexFile != NULL) {
            free (usr->rexFile);
         }
         usr->rexFile = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->rexFile, next);
         return 2;
      case ENVDIR:
         if (usr->envDir != NULL) {
            free (usr->envDir);
         }
         usr->envDir = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->envDir, next);
         return 2;
      case ENVFILE:
         if (usr->envFile != NULL) {
            free (usr->envFile);
         }
         usr->envFile = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->envFile, next);
         return 2;
      case LSTFILE:
         if (usr->lstFile != NULL) {
            free (usr->lstFile);
         }
         usr->lstFile = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->lstFile, next);
         return 2;
      case LSTTYPE:
         usr->lstType = atoi (next);
         return 2;
      case DONEFILE:
         if (usr->doneFile != NULL) {
            free (usr->doneFile);
         }
         usr->doneFile = (char *) malloc ((strlen (next) + 1) * sizeof (char));
         strcpy (usr->doneFile, next);
         return 2;
      case REXSAVEMIN:
         usr->rexSaveMin = atoi (next);
         return 2;
      case VERBOSE:
         usr->verbose = atoi (next);
         return 2;
/* ----------------------------------------------------*/
/* Added by Huiqing Liu /MDL (Jan. 2014) for rex option: 0) output surge; 1) output current magnitude */
      case REXOPTION:
         usr->rexOption = atoi (next);
         return 2;
/* ----------------------------------------------------*/
/* Added by Huiqing Liu /MDL (Jan. 2014) for boundary option: 0) static height; 1) partial clamped radiation */
      case BOUNOPTION:
         usr->bounOption = atoi (next);
         return 2;
/* ----------------------------------------------------*/
/* Added by Huiqing Liu /MDL (Jan. 2014) for Damping Time (Hr.) of boundary option partial clamped radiation */
      case TF:
         usr->Tf = atof (next);
         return 2;
/* ----------------------------------------------------*/
      case F_TIDE:
#ifdef _EXPR_
         if (ParseTide (next, &(usr->f_tide), &(usr->tideThresh), &(usr->tideCoeff)) != 0) {
            return -1;
         }
#else
         printf ("-f_tide is currently an experimental option.  As such it has "
                 "been disabled with this operational version of SLOSH.\n");
#endif
         return 2;
/* Added by Huiqing Liu /MDL (Dec. 2013) for nesting option: 0) no nesting; 1) output nesting boundary for outer domain; 2) read nesting boundary for inner domain */
      case NEST:
         usr->nest = atoi (next);
         return 2;
/* ----------------------------------------------------*/
/* Added by Huiqing Liu /MDL (Jan. 2014) for flooding option: 0) no wet&dry; 1) with wet&dry */
      case FLOOD:
         usr->flood = atoi (next);
         return 2;
/* ----------------------------------------------------*/
      case TIDEDATABASE:
         usr->tidedatabase = atoi (next);
         return 2;
      case F_STAT:
#ifdef _EXPR_
         usr->f_stat = atoi (next);
#else
         printf ("-f_stat is currently an experimental option.  As such it has "
                 "been disabled with this operational version of SLOSH.\n");
#endif
         return 2;
      case F_APPENDBSN:
         usr->f_appendBsn = atoi (next);
         return 2;
      case SPINUP:
         usr->spinUp = atoi (next) * 3600;
         return 2;
      case F_SAVESPINUP:
         usr->f_saveSpinUp = atoi (next);
         return 2;
      case ASOF:
         if (Clock_Scan (&(usr->asOf), next, 1) != 0) {
            return 2;
         }
         return 2;
      default:
         printf ("Invalid option '%s'\n", cur);
         return -1;
   }
}

/*****************************************************************************
*****************************************************************************/
int ParseCmdLine (userType *usr, int myArgc, char **myArgv)
{
   int ans;             /* The returned value from ParseUserChoice */

   while (myArgc > 0) {
      if (myArgc != 1) {
         ans = ParseUserChoice (usr, *myArgv, myArgv[1]);
      } else {
         ans = ParseUserChoice (usr, *myArgv, NULL);
         if (ans == 2) {
            printf ("Option '%s' requires a second part\n", *myArgv);
            return -1;
         }
      }
      if (ans == -1) {
         return -1;
      }
      myArgc -= ans;
      myArgv += ans;
   }

   /* Derived usr variables. */
   if (usr->rootDir != NULL) {
      if (usr->bsnDir != NULL) {
         free (usr->bsnDir);
      }
      usr->bsnDir = (char *) malloc ((strlen (usr->rootDir) + 4 + 1) * sizeof (char));
      sprintf (usr->bsnDir, "%s/dta", usr->rootDir);
      if (usr->bntDir != NULL) {
         free (usr->bntDir);
      }
      usr->bntDir = (char *) malloc ((strlen (usr->rootDir) + 4 + 1) * sizeof (char));
      sprintf (usr->bntDir, "%s/bnt", usr->rootDir);
      if (usr->tideDir != NULL) {
         free (usr->tideDir);
      }
      usr->tideDir = (char *) malloc ((strlen (usr->rootDir) + 9 + 7 + 1) * sizeof (char));
      sprintf (usr->tideDir, "%s/tidefile.ec%4d", usr->rootDir, usr->tidedatabase);
   }
   return 0;
}
