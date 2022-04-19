#!/usr/bin/perl -w
#___________________________________________________________________________
# Name     : form_ntc.pl
# Purpose  : This perl script is started by dbnprd and forms the input
#          : text files into wmo bulletin format for sending to the
#          : world.
#
# History  : Oct 16 2000 Peter Henrichsen
#          :   This new version of formbul.pl will segment the input text into parts according
#          :     to the size of the text file.
#          :   $max_size = 14500; # this is the size in bytes that is used to determine if the
#          :     bulletin text is to be split into segments.
#          :   THE LOGIC FOLLOWS:
#          :   $size = -s $infile; # obtain the size of the input file in bytes.
#          :   $line_count = `wc -l <$infile`; # get the line count
#          :   $number_bytes = $size + ($line_count*3); # caculate the new byte count.
#          :   $num_parts = int($number_bytes/$max_size + .8); # caculate the number of parts
#          :     The $num_parts is the number of segments that will be made from the input
#          :     text file.
#          : Nov 28 2000 Peter Henrichsen - Modified to run on the ibm asp
#          : Oct 15 2005                  - Modified to sent products to TOC via NTC.
#          :                                Renamed form_ntc.pl
#          : June 3 2015 Kit Menlove      - Cleaned up and added option to bypass segmentation.
#
# Arguments  There are two required arguments: -d and -f
#          :   -d specifies the deck or bulletin name
#          :   -f specifies the input text file (fullpath)
#          : There are six optional arguments: -j, -m, -p, -s, -o, and -n
#          :   -j specifies the 'job name'
#          :   -m specifies the model that made the text file
#          :   -p specifies the current PCOM path
#          :   -s specifies SENDDBN which turns on or off sending of the completed bulletin
#          :   -o specifies the output file name
#          :   -n specifies that the file should not be segmented regardless of its size
#
# Required environment variables:
#          : USHshared - required to find make_ntc_bull.pl
#          : FIXshared - required to locate bulls directory

# Required Items: bulls directory found at ${FIXshared}/bulls
#___________________________________________________________________________

use File::Basename;
use Getopt::Std;
use Time::Local;
use File::Copy;
no warnings 'once';

$FIXshared=$ENV{'FIXshared'};
die "$0: environment variable FIXshared not set!\n" unless ($FIXshared);
$deckpath = "$FIXshared/bulls"; #define the control deck path
$seg_size = 14800;  # this is the segment size
$max_size = 14500;  # this is the size in bytes that is used to determine if 
                    # the bulletin text is to be split into segments.
$block_size = 1280; # this is the blocking size for the finished bulletin file.
$header = "'100000 PERL FORMBUL VERSION 20001206.50"; # this is the 40 byte header line that is
                                                      # at the begenning of each bulletin.
$valid_centers = '(kwbc|kwhc|kwbj|kwnm|kwnh|kspc|kwno|kkci|kmia|outt|cwao)'; # define valid centers

# get input file name
getopts('d:f:j:m:p:s:o:n');

die "$0: input deck missing!\n" unless ($opt_d);
die "$0: file name missing!\n" unless ($opt_f);

if ($opt_j) {
  $job = $opt_j;
} elsif (defined $ENV{'job'}) {
  $job = $ENV{'job'};
} else {
  die "$0: job not defined\nPlease specify using -j argument or job environment variable\n";
}

$jobid = $ENV{'jobid'}; # get jobid from enviornment
$jobid = $job . '.' . $$ unless ($jobid);

# check to see if $opt_m is present
if ($opt_m) {
  $model = $opt_m;
} else {
  $model = "man";
}

# check to see if $opt_p is present
if ($opt_p) {
  $pcom = $opt_p;
} elsif (defined $ENV{'PCOM'}) {
  $pcom = $ENV{'PCOM'};
} else {
  die "$0: PCOM not defined.\nPlease specify using -p argument or PCOM environment variable\n";
}

# check to see if $opt_s is present
if ($opt_s) {
  $senddbn = $opt_s;
} elsif (defined $ENV{'SENDDBN'}) {
  $senddbn = $ENV{'SENDDBN'};
} else {
  $senddbn = 'NO';
}

$USHshared=$ENV{'USHshared'};
if ($senddbn eq 'YES' && !$USHshared) {
  die "$0: environment variable USHshared not set!\n";
}

($sec, $min, $hours, $mday, $mon) = (gmtime) [0,1,2,3,4]; # Get UTC time for various functions
$datetime = sprintf("%02d/%02d %02d:%02d:%02dZ ",$mon+1,$mday,$hours,$min,$sec);
$DDHHMM = sprintf("%02d%02d%02d",$mday,$hours,$min);
$endline = "<<@";
$deck_name = lc $opt_d; # transulate input deck name to lower case
$indeck = $deckpath . "/" . $deck_name;

print "jobid: $jobid\n";
print "model: $model\n";
print "deckpath: $deckpath\n";
print "SENDDBN: $senddbn\n";
print "PCOM: $pcom\n";
print "DDHHMM: $DDHHMM (day/hour/min of bulletin)\n";
print "Control card: $indeck\n";

GETCONTROLS: unless(open(INDECK, "< $indeck")) {
               if(! ($indeck=make_control_file($opt_d,$deckpath))) {
                 $msg = "FATAL ERROR: can not get $opt_d from $deckpath!";
                 print STDERR "$msg\n";
                 exit 1;
               }
               goto GETCONTROLS; # go back and read the control file.
             }

$infile = $opt_f;

unless(open(INPUTEXT, "<$infile")) {
  print STDERR "FATAL ERROR: cannot open $infile!";
  exit 1;
}
print "Reading $infile\n";

@bul_names = ();
@file_names = ();
@headers_ray = ();

$numbuls = &get_controls(*file_names,*headers_ray,*bul_names); # get headers and filenames

print "Number of bulletins to make: $numbuls\n";
for ($i=0; $i < $numbuls; $i++) {
  print "File name for bul$i: $file_names[$i]\n";
  print "Header for bul$i: $headers_ray[$i]\n";
}

if($numbuls == 1) {  # ONLY ONE BULLETIN IN THE INPUT TEXT FILE.
  $big_header = $headers_ray[0];
  $size = -s $infile;
  print "\nThe size of $infile is $size bytes\n";

  if ($opt_n) {  # THE SCRIPT WAS CALLED WITH SEGMENTATION DISABLED
    $num_parts = 1;
  } elsif ($size > $max_size) {  # THIS BULLETIN MUST BE SPLIT IN TO SEGMENTS!
    $line_count = `wc -l <$infile`;  # get the line count
    chomp($line_count);

    $number_bytes = $size + ($line_count*3);  # caculate the new byte count.
    print "\ttotal number of bytes is size + line_count ($line_count) * 3 = $number_bytes\n";
    $num_parts = int($number_bytes/$max_size + .8);
    print "\twill be segmented into $num_parts parts\n";
  } else {  # THIS BULLETIN IS SMALL AND IS NOT SEGMENTED!
    $num_parts = 1;
    print "file is small and will not be segmented\n";
  }

  @new_bull = &make_seg($num_parts,$big_header); # call sub make_seg to segment text file

  open (OUTFILE,'>'.$file_names[0]); # open output file.
  # Check on the length of each bulletin part and the block to $block_size bytes
  $num_records = 0;
  for ($part_num=0; $part_num < $num_parts; $part_num++) {
    # block the part into $block_size bytes
    $new_bull[$part_num] = &block_seg($part_num,$new_bull[$part_num]); 

    # WRITE this bulletin or segment to the output file.
    #   NOTE if this bulletin is segmented all the segments are written to the same file
    print OUTFILE $new_bull[$part_num];
  }
  close(OUTFILE); # finished with file - close it before posting.

  $filename = basename($infile);
  print "Bulletin $bulname made from $filename!\n";

  if($num_parts > 1) {
    print "$bulname USES $num_records RECORDS AND HAS $num_parts PARTS!\n";
  }

  # Check to see if a new name is to be used for the bulletin.
  if($opt_o) {
    print "Moving " . $file_names[0] . " to $opt_o\n";
    move($file_names[0], $opt_o);
    $send_name = $opt_o;
  } else {
    $send_name = $file_names[0];
  }

  # Check to see if we are to send or post the completed bulletin.
  if($senddbn eq 'YES') {
    # Send bulletin to TOC via NTC.                        
    if (system("$USHshared/make_ntc_bull.pl WMONV NONE KWNO NONE $send_name $pcom/$send_name") != 0) {
      die "Error in make_ntc_bull.pl";
    }
  } else {
    print "Testing $send_name - file NOT ALERTED!\n";
  }
} else {  # MORE THAN ONE BULLETIN IN THE INPUT TEXT FILE.
  print "We have $numbuls bulletins to make from $filename\n";
  $bulnames = "";
  for ($bulnum=0; $bulnum < $numbuls; $bulnum++) { # START OF NUMBER OF BULLETIN LOOP.
    print "$file_names[$bulnum] is file name for bul$bulnum\n";
    print "$headers_ray[$bulnum] is header for bul$bulnum\n"; 
    $big_header = $headers_ray[$bulnum]; 
    $len_bighead = length $big_header; 
    $numpart = 1;

    # call sub make_seg to get text from input file into bulletin format.
    @new_bull = &make_seg($numpart,$big_header); 

    # DO SOME CHECKING TO SEE IF THIS BULLETIN HAS MORE DATA THAN THE HEADER!
    $len_newbul = length $new_bull[0];
    $limit = $len_bighead+6;
    print "The length of big_head is $len_bighead length new_bull[0] =$len_newbul\n";

    if($len_newbul > $limit) {
      # GOOD Bulletin open output file for writing.
      open (OUTFILE,">$file_names[$bulnum]"); # open out put file.
      $num_records = 0;

      # block the part into $block_size bytes
      $new_bull[0] = &block_seg($part_num,$new_bull[$part_num]); 

      # WRITE to the output file.
      print OUTFILE $new_bull[0];

      close(OUTFILE); # close this file before posting in.
      $bulnames = $bulnames . $bul_names[$bulnum] . " ";

      # Check to see if a new name is to be used for the bulletin.
      if($opt_o) {
        print "Moving " . $file_names[$bulnum] . " to $opt_o\n";
        move($file_names[$bulnum], $opt_o);
        $send_name = $opt_o;
      } else {
        $send_name = $file_names[$bulnum];
      }

      # Check to see if we are to send or post the completed bulletin.
      if($senddbn eq 'YES') {
        # POST bulletin to status file for OSO to get and send.
        if (system("$USHshared/make_ntc_bull.pl WMONV NONE KWNO NONE $send_name $pcom/$send_name") != 0) {
          die "Error in make_ntc_bull.pl";
        }
      } else {
        print "Testing $send_name - NOT ALERTED!\n";
      }
    } else {
      # BAD *** SKIP THE BULLETIN BECAUSE IT IS ONLY A HEADER!
      print "Skipping bulletin $bulnum because $len_newbul is < or = $limit\n";
    }
  }
  print "Bulletins $bulnames\n\n";
}
exit 0;
########################### END OF MAIN PROGRAM ############################


#___________________________________________________________________________
# Name     : make_control_file
# Author   : Peter Henrichsen
# Purpose  : This perl subroutine make a control file for the perl script formbul.pl.
#
# History  : Wed Aug  9 07:08:37 EDT 2000 Peter Henrichsen
#
# Location : This script is found on lnx185 as:
#          : /tmp_mnt/export/sgi73/peterhen/ibm/formbul/(in formbul.pl)
#
# arg1 = control flags that come from the $9 arg from dbnet.  This is the bulletin
#        name or can have the form of bulletinname_centername_pil(a max of 6 characters)
#        Some examples are:   "tcus54", "tcus54_kwbc" or "tcus54_kwbc_scppr2" .
# arg2 = the control directory where to put the control file named as arg1.

# usage example:
#        make_control_file(tcus54_kwbc_scppr2, /usr/DBNet/user/bulls)
#___________________________________________________________________________
 
sub make_control_file {
  my($control_flags,$control_directory) = @_; # name the input args.
  my $center = "KWBC";
  my $pil = "";
  my $bulname = "";
  my $outcontrol_file = "";
     $control_flags =~ tr/A-Z/a-z/; # transulate to lower case
     $outcontrol_file = $control_directory . "/" . $control_flags; # create path for writing
  
  print "OPEN the bulletin control file $outcontrol_file\n";
  unless(open(OUTDECK, ">$outcontrol_file")) {
    print "Trouble opening $outcontrol_file for writing\n";
    return ""; # error return
  }
  # die "cannot open $outcontrol_file for writing : $!";
  $ts = time; # get number of seconds for use by local time function.
  
  $date = scalar(localtime($ts));# Get local time for lineone
  
  $part1 = "NUMBER OF BULLETINS TO MAKE01xxx MEMBER=";
  $lineone = $part1 . $control_flags . " " . $date . " DKPH\n"; #make lineone 
  
  # now write first line into $outdeck 
  print OUTDECK $lineone; # write lineone
  print "Wrote lineone = $lineone ";
    
  @parts = split(/_/, $control_flags); #split coltrol_flags into parts 
  $bulname = $parts[0]; #get bulletin name.
  if( @parts == 3) {
    if($parts[1] =~ /$valid_centers/){
      $center = $parts[1]; #get valid center name.
    }
    $pil = $parts[2]; # get pil
  
  } elsif ( @parts == 2) {
    if($parts[1] =~ /$valid_centers/) {
      $center = $parts[1];#get valid center name.
    }
  }
  $bulname =~ tr/a-z/A-Z/; # transulate to upper case:
  $center =~ tr/a-z/A-Z/; # transulate to upper case.
  $pil =~ tr/a-z/A-Z/; # transulate to upper case.
  my $len = length $bulname; # get length of bulletin name.
  print "Bulletin name =>$bulname<= Center name=>$center<= Pil =>$pil<=\n";
  $linetwo = "INEW B=" . $len . "00000" . $bulname . " F=" . $center .",W=" .  $pil . "\n";
  
  print OUTDECK $linetwo; # write linetwo
  #print " I wrote linetwo of $linetwo ";
  close(OUTDECK);
  return $outcontrol_file;  # good retun $outcontrol_file name
}


#___________________________________________________________________________
# Name     :get_controls
# Author   : Peter Henrichsen
# Purpose  : This perl subroutine reads the control card file and forms the
#          : the header lines and the output file_names
#
# History  : Fri Sep 15 07:08:55 EDT 2000 Peter Henrichsen
#
# Location : This script is found on lnx185 as:
#          : /tmp_mnt/export/sgi73/peterhen/ibm/formbul/(in formbul.pl)
#___________________________________________________________________________

sub get_controls {
  ($headers_ray,$file_names,$bul_names) = @_; # name the input args.
  my $num_buls;
  my $VALID_CENTERS = uc $valid_centers; # convert to uppercase.
  $center = "KWBC";
  
  $_ = <INDECK>; # read a line in from the INDECK
  
  chomp($_); # remove trailing \n from line
  
  if (/make/i) { # search line for 'make'
    if(substr($_,27,1) == 0) { # check tens part
      $num_buls = substr($_,28,1); # get number of bulletins to make
    } else {
      $num_buls = substr($_,27,2); # get number of bulletins to make
    }
    $num_buls = int($num_buls); # get $numbul into integer
    #print "Number of bulletins to make = $num_buls\n";
  }
  
  if($num_buls >= 1){
    for ($i=0; $i < $num_buls; $i++) {
      $_ = <INDECK>; # read in one line
      if (/inew/i) { # search for inew
        @words = split; # same as @words = split(/\s+/, $_);
        $len = length $words[1]; # get number of bytes in $words[1]
        $bulname = substr($words[1],8,$len-8);
        $pil = ""; # blank $pil
        $center = ""; # blank $center
        @parts = split(/,/, $words[2]); # get equal flags.
        $flags ={};

        foreach $part (@parts) {
          ($key, $value) = split /=/, $part;
          $flags{$key} = $value; # load into hash %flags           
        }
        while ( ($key,$value) = each %flags) { # print key and value
          print "$key => $value\n";
        }
  
        if($flags{F} =~ /$VALID_CENTERS/) {
          $center = $flags{F};
        } else {          
          $center = "KWBC";
          print "Using default center $center\n";
        }
 
        print "Bulname=$bulname center=$center\n";         
  
        $lenpil = 0;
        if($flags{W}) {
          $pil = $flags{W};
          $lenpil = length $pil;

          $output = $pcom . "/" . $bulname ."." . $center . "." . $pil . "." . $job;
          $head = sprintf("%s%s %s %s%s%s",$header,$bulname,$center,$DDHHMM,$endline,$pil);       

          $ispil = 1; # set ispil flag to true.
        } else {
          $output = $pcom . "/" . $bulname ."." . $center . "." . $job;
          $head = sprintf("%s%s %s %s",$header,$bulname,$center,$DDHHMM);
          $ispil = 0; # set ispil flag to false.
        }
      }
      print "Length of pil line: $lenpil\n"; 
      $output = lc $output; # transulate output file name into lowercase.
      $bul_names[$i] = $bulname;
      $file_names[$i] = $output;
      $headers_ray[$i] = $head;
    }
  }
  return $num_buls;
}


#___________________________________________________________________________
# Name     : make_seg
# Author   : Peter Henrichsen
# Purpose  : This perl subroutine segments a text file into parts for formbul.pl.
#
# History  : Wed Oct  4 06:44:13 EDT 2000 Peter Henrichsen
#
# Location : This script is found on lnx185 as:
#          : /tmp_mnt/export/sgi73/peterhen/ibm/formbul/(in formbul.pl)
#___________________________________________________________________________

sub make_seg {
  my($numparts,$header) = @_; # name the input args.
  my $endbul  = "<<%";
  my @seg_names = ();
  
  my @parts = ( PAA, PAB, PAC, PAD, PAE, PAF, PAG, PAH, PAI, PAJ, PAK, PAL, PAM,
                PAN, PAO, PAP, PAQ, PAR, PAS, PAT, PAU, PAV, PAW, PAX, PAY, PAZ, 
                PBA, PBB, PBC, PBD, PBE, PBF, PBG, PBH, PBI, PBJ, PBK, PBL, PBM,
                PBN, PBO, PBP, PBQ, PBR, PBS, PBT, PBU, PBV, PBW, PBX, PBY, PBZ,
                PCA, PCB, PCC, PCD, PCE, PCF, PCG, PCH, PCI, PCJ, PCK, PCL, PCM,
                PCN, PCO, PCP, PCQ, PCR, PCS, PCT, PCU, PCV, PCW, PCX, PCY, PCZ,
                PDA, PDB, PDC, PDD, PDE, PDF, PDG, PDH, PDI, PDJ, PDK, PDL, PDM,
                PDN, PDO, PDP, PDQ, PDR, PDS, PDT, PDU, PDV, PDW, PDX, PDY, PDZ,
                PEA, PEB, PEC, PED, PEE, PEF, PEG, PEH, PEI, PEJ, PEK, PEL, PEM,
                PEN, PEO, PEP, PEQ, PER, PES, PET, PEU, PEV, PEW, PEX, PEY, PEZ  );
  
  my $key = 0;
  
  my @bull_array = ();
  if($numparts == 1) {
    $key = 0;
    $bull_array[$key]= sprintf("%s%s",$header,$endline);
    #print "    bull_array$key  $bull_array[$key]\n";
  } else { # have parts
    if($ispil) { # check to see if there is a pil if so I must fidel with the header.
      $lenhead = length $header;
      $new_head = substr($header,0,$lenhead-9); # get truncated header
      print "$lenhead is the lenght of header, newhead =>$new_head<=\n";
      $offset = ($lenhead - 6);
      $new_pil = substr($header,$offset,6);# get new pil
      print "$new_pil = the pil from the header, the old pil is =>$pil<=\n";
      $header = $new_head;
    }
    for ($key=0; $key < $numparts; $key++) {
      $part_head = $parts[$key];
      $seg_names[$key] = $part_head;
      if($ispil) {
        $bull_array[$key]= sprintf("%s %s%s%s%s",
        $header,$parts[$key],$endline,$pil,$endline);
      } else {
        $bull_array[$key]= sprintf("%s %s%s",$header,$parts[$key],$endline);
      }
      #print "    bull_array$key  $bull_array[$key]\n";
    }
    #$part_head =~ s/(.).(.)/\1Z\2/;
    $part_head =~ s/(.).(.)/$1Z$2/;
    if($ispil) {
      $bull_array[--$key]= sprintf("%s %s%s%s%s",$header,$part_head,$endline,$pil,$endline);
    } else {
      $bull_array[--$key]= sprintf("%s %s%s",$header,$part_head,$endline);
    }
    #print "Last member of the bull_array$key  $bull_array[$key]\n";
  }

  $j=0;
     
NEWPART: $bulen = length $bull_array[$j]; # get number of bytes in bull_array.
  print "\nbull_array$j\n\t$bull_array[$j]\n\tLength: $bulen\n";

  while (<INPUTEXT>) { # read a line in from the INPUTEXT into line
    chomp($_); # remove trailing \n from line
    #if (/$bulname/i ) { # search for $bulname in line
    if (/$bulname/i or ($ispil and /$pil/i)) { # search for $bulname or $pil in line
      print "\t* Found $bulname $pil in '$_' ... skip this line!\n";
    } else {
      if (/nnnn/i) { # search for terminating nnnn's
        print "Found end because, last line= $_\n";
        $bull_array[$j] = $bull_array[$j] . $endbul;
        $bulen = $bulen + 3;
        print "Part $j is the LAST segment and is $bulen bytes long\n";
        goto ENDBUL;
      } else {
        $_ =~ s/\s+$//; # remove trailing blanks from line
        $_ =~ s/%/P/g; # remove percert from line
        $_ =~ s/>//g; # remove > from line
        $_ =~ s/'//g; # remove ' from line
        $_ =~ s/,/\//g; # remove , from line
        $_ =~ tr/\032//d; #  delete  CTRL-Z or '032' from line
        $_ =~ tr/\000//d; #  delete binary zero or CTRL-@ from line
        $_ =~ tr/\015//d; #  delete CTRL-M or carraige return from line
        $_ =~ tr/a-z/A-Z/; #  convert line to upper case
        $outline = $_ . $endline;  # add end line
        $linelen = length $outline; # get number of bytes in line
        if(($linelen + $bulen) <= $seg_size || $numparts == 1) {
          $bulen = $bulen + $linelen; # add bytes to total.
          $bull_array[$j] = $bull_array[$j] . $outline;
          if($bulen == $seg_size) {
            $bull_array[$j] = $bull_array[$j] . $endbul;
            $bulen = $bulen +3;
            print "\tBytes: $bulen\n";
            $j  = $j + 1;
            $bulen = 0;
            goto NEWPART;
          } else {
            $endseg = "";
          }
        } else {
          $endseg = 1;
          $bull_array[$j] = $bull_array[$j] . $endbul;
          $bulen = $bulen + 3;
          print "\tBytes: $bulen\n";
          $j = $j + 1;
          print "Will load $linelen bytes in segment $j\n";
          $bull_array[$j] = $bull_array[$j] . $outline;
          #print "Seg $j now is $bull_array[$j]\n";
          goto NEWPART;
        }
      }
    }
  } # end of while loop for input text file
  $bull_array[$j] = $bull_array[$j] . $endbul;
  $bulen = $bulen + 3;
  print "Part $j is the LAST segment and is $bulen bytes long\n";

ENDBUL:
  if($numparts > 1) {
    print "Segmenting is ON... checking for uncompleted segments\n";
    if(($j+1) < $numparts) {
      $old_head  = $seg_names[$j];
      $new_head = $old_head;
      $new_head =~ s/(.).(.)/$1Z$2/;
      print "I have another part so I must change $old_head to $new_head in segment $j\n";
      $bull_array[$j] =~ s/$old_head/$new_head/;
      $header_last = substr($bull_array[$j],0,65);
      print "The header for sement $j is now $header_last\n";
      #$num_parts = $numparts - 1;
      #$num_parts = $j + 1;
      #print "I INSERTED $new_head into segment $j and CHANGED num_parts from $numparts to $j + 1\n";
    } else {
      print "All segments are complete because $j + 1 = $numparts\n";
    }
  } else {
    print "Segmenting is OFF, no checking required.\n";
  }
  return @bull_array;
}


#___________________________________________________________________________
# Name     : block_seg
# Author   : Peter Henrichsen
# Purpose  : This perl subroutine blocks a bulletin or bulletin segment into
#            ($block_size = 1280) bytes.
#
# History  : Wed Aug 23 13:57:50 EDT 2000 Peter Henrichsen
#
# Location : This script is found on lnx185 as:
#          : /tmp_mnt/export/sgi73/peterhen/ibm/formbul/(in formbul.pl)
#___________________________________________________________________________

sub block_seg {
  my($part_num,$bul_ray) = @_; # name the input args.
  my $delimiter = "\n";
  my $pad = " ";
  my $big_buf = "";
  my $start = 0;
  my $num_bytes = length $bul_ray;
  my $sub_parts = int($num_bytes/$block_size + .5);

  # check to see if ($block_size * $sub_parts) is less than $num_bytes
  if(($block_size * $sub_parts) < $num_bytes) {
    $sub_parts = $sub_parts + 1;
  }
  my $num_add = ($block_size * $sub_parts) - $num_bytes;
  print "Part $part_num initially has $num_bytes bytes; ";
  for ($m=0; $m < $num_add; $m++) { # pad to even mul of $block_size.
    $bul_ray = $bul_ray . $pad;
  }
  $num_bytes = length $bul_ray;
  print "increased to $num_bytes bytes after padding (sub_parts=$sub_parts,num_add=$num_add); ";
  # add the delimiter after every $block_size bytes.
  for ($j=0; $j < $sub_parts; $j++) {
    $buf = substr($bul_ray,$start,$block_size);
    $start = $block_size * ($j + 1);
    $big_buf = $big_buf . $buf . $delimiter;
  }
  $num_records = $num_records + $sub_parts;
  $bul_ray = $big_buf;
  $num_bytes = length $bul_ray;
  print "$num_bytes bytes after adding the delimeter\n";
  return $bul_ray;
}

