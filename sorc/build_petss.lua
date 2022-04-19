-------------------------------------------------------------------------------
-- P-ETSS v1.1.5                                        Last Change: 2021-09-09
--                                                         Huiqing.Liu@noaa.gov
--                                                             NWS/OSTI/MDL/DSD
-------------------------------------------------------------------------------

whatis([===[ P-ETSS is a model to compute probalisitic storm surge and tide 
levels from extra-tropical storms ]===])

-- Make sure envvar is loaded
-- prereq("envvar/"..os.getenv("envvar_ver"))


-- Load required software

load("PrgEnv-intel/"..os.getenv("PrgEnv_intel_ver"))
load("intel/"..os.getenv("intel_ver"))
load("craype/"..os.getenv("craype_ver"))
load("g2/"..os.getenv("g2_ver"))
load("bacio/"..os.getenv("bacio_ver"))
load("w3nco/"..os.getenv("w3nco_ver"))
load("jasper/"..os.getenv("jasper_ver"))
load("libpng/"..os.getenv("libpng_ver"))
load("zlib/"..os.getenv("zlib_ver"))
load("g2c/"..os.getenv("g2c_ver"))
load("bufr/"..os.getenv("bufr_ver"))

setenv("optFlag","-O3")
setenv("COMP","ftn")
setenv("COMPC","cc")
setenv("C_COMP","cc")
