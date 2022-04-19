/*
 * ** DEBUFR.C
 * ** Original Author: J. Ator 2009-07-01
 * ** Heavily Eviscerated by: Ryan Schuster in 2014
 * ** Check the bufr tank file exists before open it: Huiqing Liu in Sep. 2019
 * ** Convert to use WCOSS2 bufr module : Huiqing Liu in Sep. 2021
 * ** Abstract:
 * **  This program simply opens an input BUFR file and hands it off to
 * **  fdebufr.f in lib/sorc/mybufr/. There it is parsed and sent to an
 * **  output directory. The input program is called ofile.
 * **
 * ** REMARKS:
 * **   SUBPROGRAMS CALLED:
 * **     LOCAL      - fdebufr  openbt
 * **     BUFRLIB    - ccbfl    cobfl    crbmg    datelen  dxdump
 * **                  idxmsg   ireadsb  iupbs01  iupbs3   mtinfo
 * **                  openbf   readerme ufdump   upds3    bvers
 * **
 * ** ATTRIBUTES:
 * **   LANGUAGE: C
 * **   MACHINE: Portable to all platforms
 * */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#ifdef F77_INTSIZE_8
    typedef long f77int;
#else
    typedef int f77int;
#endif

#define BVERS_
extern void bvers_
   (char bvstr[9], int lenb);
#define FDEBUFR_
extern void myfdebufr_( char *fn, int lenf );

extern void cobfl_( char *bfl, char *io );
extern void ccbfl_ ( void );
extern int isdigit (char vers );

int main( int argc, char *argv[ ] ) {
   int ch;
   int errflg;

   char io = 'r';
   char root[100] = "";
   char tbldir[120];
// char tbldir[120] = "/lfs/h1/ops/canned/packages/dell1/decoders/decod_shared/fix/";
   char tblfil[120];
   char wkstr[120];
   char bvstr[9] = "        ";

   unsigned short ii;

   f77int lentd;

/*
* 	**  Get the valid options from the command line:
* 	**	-v	prints version information and exits
* 									*/
   errflg = 0;
   wkstr[0] = '\0';  /* initialize to empty string */
   while ( ( ch = getopt ( argc, argv, "vo:r:" ) ) != EOF ) {
      switch ( ch ) {
         case 'v':
	     bvers_( bvstr, sizeof(bvstr) );
	    /* append a trailing NULL to bvstr for printf */
             for ( ii = 0; ii < sizeof(bvstr); ii++ ) {
                if ( ( bvstr[ii] != '.' ) && ( !isdigit(bvstr[ii]) ) ) {
                   bvstr[ii] = '\0';
                   break;
                }
             }
             printf( "This is debufr v2.1.0, built with BUFRLIB v%s\n",bvstr );
             return 0;
      }
   }

/*
 * 	**  There should be one remaining command line argument specifying the
 * 	**  input BUFR file.
*/
   if ( (optind+1) != argc ) {
      printf( "\nUsage:  %s [options] BUFRfile\n\n", argv[0] );
      printf( "  where possible options are:\n" );
      printf( "    -v\n" );
      printf( "  and BUFRfile is [path/]filename of BUFR input file\n\n" );
      return -1;
   }

/*
 *      **  When running from shell script, get input file name and
 *      **  use it as output filename, too
*/
   char *ofile;
   ofile = malloc(16*sizeof(char));
   ofile = strncpy(ofile, argv[optind]+0, 20-5);
   ofile[15] = '\0';
/*
    **  Check the input BUFR file exists.
*/
   if(!access(argv[optind], F_OK )){

/*
 * 	**  The file is there, then open the input BUFR file.
*/
      cobfl_( argv[optind], &io );

/*
 *      **  We will not be specifying an outside DX BUFR table
*/
      strcpy( tblfil, "NULLFILE" );

/*
 * 	**  Read and decode each message from the input BUFR file.
*/
      myfdebufr_( ofile, strlen(ofile));

/*
 * 	**  Close the input BUFR file. 		
*/
      ccbfl_( );
      free(ofile);
   } else {
/*
 *      **  The file is not there, then print warnning message.
*/
      printf("Warning: The BUFR Tank File %s\t not Found\n",argv[optind]);
   }


   return 0;
}
