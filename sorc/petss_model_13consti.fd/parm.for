C L_ limited to 27,000 due to dta issues and all capital letters.
C Possible to include lower case letters to get to 53,000
      PARAMETER (M_=999,N_=999,L_=37000,NCT_=3000,LC_=L_)
C Remember to change COMMON /BCPTS/ to M_*2 + N_*2
      PARAMETER (NBK_=11000,ND_=6000)
      PARAMETER (M2G_=30000)
      COMMON /WDMAX/  WDMAX(M_,N_)
      COMMON /LLXFLE/ YLT(M_,N_),YLG(M_,N_)
      COMMON /DUMB5/  ZB(M_,N_),ZBM(M_,N_),I_boun(M_,N_)
      COMMON /DUMB7/  UB(M_,N_),VB(M_,N_),HB(M_,N_)
      COMMON /SCRTCH/ HSUB(M_,N_)
      COMMON /DUMB10/ HMX(M_,N_)
      COMMON /ARHMX/  IHMX(M_,N_)
      INTEGER*2       IHMX
      COMMON /ELPDST/ ELPCT(M_),ELPDT(M_),ELPCL(M_),ELPDL(M_)
      COMMON /ELPDS2/ ELPCL2(M_),ELPDL2(M_),SINL2(N_),SINT2(N_)
      COMMON /PLRPST/ SINT(N_),COST(N_),SINL(N_),COSL(N_)
      COMMON /SETMIX/ IS(N_),MS(N_),MF(N_)
      COMMON /SETMX1/ IE(N_),ME(N_)
      COMMON /ITREE/  ITREE(M_,N_)
      COMMON /KTREE/  KTREE(L_)
      CHARACTER*1     ITREE,KTREE
      COMMON /KSPCON/ KSKP(M_,N_)
      CHARACTER*1     KSKP
      COMMON /DUMB18/ ISQR(L_),JSQR(L_),ISIDE(L_),HWEIR(L_),
     1                ZBMIN(L_)
      COMMON /ACT1D/  FLWSQR(L_),F1DACT(L_)
      COMMON /CS1DA/  COS1D(L_),SIN1D(L_)
      CHARACTER*1     F1DACT
      COMMON /DUMCUT/ CUTL(NCT_),CUTLI(NCT_),CUTLE(NCT_)
      COMMON /BANK2/  DELCUT(LC_),BANK(LC_,2)
C
      COMMON /SUBDCE/ ZSUBCE,JSUB,JSUB1,ZSUB(N_)
      COMMON /LAKE/   JLDTMG,LDTMG(N_)
      COMMON /LDRY/   NODRY,IDRY(ND_),JDRY(ND_)
