      IMPLICIT NONE
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      COMMON/DOUBLES/ KWEI,SMFRTN, FSMFBN1,FSMABN1,FSMABN2,FSMFXX,FSMFYY
     *,FSMFR1,FSMFR2,FSMFR11,FSMFR12, FSMFRR,FSMFXY,BINSIZE,RBin,RBin_X,
     *RBin_Y
      REAL*8 KWEI, SMFRTN(22),  FSMFBN1(22,200,200), FSMABN1(200), FSMAB
     *N2(200), FSMFXX(22,200), FSMFYY(22,200), FSMFR1(22,200), FSMFR2(22
     *,200), FSMFR11(22,200), FSMFR12(22,200), FSMFRR(22,200), FSMFXY(22
     *,200,200), BINSIZE(200), RBin,RBin_X,RBin_Y
      COMMON/REALS/ XMIN2,XMAX2, XPLOT,HXMIN, YPLOT,YPLOTT, ERRYPLOT,X2Y
     *2,ZTOTAL,RSSD02,RSSD12,SSDVPS, SFACTOR,RFACTOR,RFACTOR1,RFACTOR2,
     *X,Y,Z,RRFACTOR,SSFACTOR,RSSD0,RSSD1, WEIGHT,EI, PARAEMAX2,PARAEMAX
     *1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAEMNE, PARANINC1,PARANINC2,PARA
     *NINC, EREAD,ZLAST, MUIDX,XIN,YIN,ZIN,UIN,VIN,WIN, SMFX2,SMFY2,SMFM
     *XR, HFIELD, RSCATT, DELTAZ,RSSDST,SSSDST,ERADII,E_ph_sp_e, E_ph_sp
     *_g, E_ph_sp_p, E_MIN_ph_sp, E_MAX_ph_sp, RFIELD,RFIELDe,XFLDMN,XFL
     *DMX, YFLDMN, YFLDMX, SMFMNZ, SMFMXZ, SMFMNX, SMFMXX, SMFMNY, SMFMX
     *Y, SMFMNR,RTREAT,RTREATe,XTREATMN,XTREATMX, YTREATMN,YTREATMX, SMF
     *MNE,SMFMXE, ABNTOTAL,ABNTOTALH,FLUAREA,NINC, RMIN_MIN,RMIN_MAX,RMA
     *X_MIN,RMAX_MAX,DOMEGA, SUM_TMP,SUM_TMP2,Z_SCORE,mumin,mumax
      real*8 XMIN2,XMAX2,  XPLOT(200),HXMIN,  YPLOT(200),YPLOTT, ERRYPLO
     *T(200), X2Y2,  ZTOTAL(22),  RSSD02,RSSD12,  SSDVPS(22+3),  SFACTOR
     *,RFACTOR,RFACTOR1,RFACTOR2,X,Y,Z,RRFACTOR,SSFACTOR, RSSD0,RSSD1,
     *WEIGHT,EI, PARAEMAX2,PARAEMAX1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAE
     *MNE, PARANINC1,PARANINC2,PARANINC, EREAD,ZLAST,MUIDX, XIN,YIN,ZIN,
     *UIN, VIN,WIN,  SMFX2(22), SMFY2(22),  SMFMXR(22),  HFIELD(22),  RS
     *CATT,  DELTAZ,  RSSDST(22,2000), SSSDST(22,2000), ERADII(200),  E_
     *ph_sp_e,  E_ph_sp_g,  E_ph_sp_p,  E_MIN_ph_sp,  E_MAX_ph_sp,  RFIE
     *LD,  RFIELDe,   XFLDMN,  XFLDMX,  YFLDMN,  YFLDMX,  SMFMNZ(22),
     *SMFMXZ(22),  SMFMNX(22),  SMFMXX(22),  SMFMNY(22),  SMFMXY(22),  S
     *MFMNR(22),  RTREAT,  RTREATe,   XTREATMN,  XTREATMX,  YTREATMN,  Y
     *TREATMX,  SMFMNE,SMFMXE,  ABNTOTAL,ABNTOTALH,  FLUAREA,   NINC,  R
     *MIN_MIN, RMIN_MAX,  RMAX_MIN,  RMAX_MAX, DOMEGA,  SUM_TMP(200),  S
     *UM_TMP2(200),  Z_SCORE,  mumin,mumax
      COMMON/INTEGERS/ PARANOT,PARANOT1,PARANOT2,PARANOP,PARANOP1, PARAN
     *OP2,IPARANOT,IPARANOT1,IPARANOT2, LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST, I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBER
     *ST,NBIT1,NBIT2, NFIELDi,NFIELDo,NFIELDe, NWRONG,LATCHJ,CHOICE,CHOI
     *CE1, K1,K2,K3,K4,IA, IX,IY,IR,K,IQ,LATCHI,IE, NPASSI,LATCH,SMFPXY,
     *NFIELD,NSMFPS,I_IN_EX, ISMFPS,IQSMFP,MSTART,MSMFXY,NSMFEE,NPASS_ph
     *_sp, N_ph_sp_e, N_ph_sp_g, N_ph_sp_p, FLUTYPE,ANGTYPE,IZLAST1, IZL
     *AST2,IMUIDX1,IMUIDX2,IZSCORE1,IZSCORE2,JUSTONE, MAXSCATTER,NUMSCAT
     *TER,ESTYPE,i_iaea_in,i_iaea_out,i_log, i_unit_in,i_unit_out
      INTEGER*8 PARANOT,PARANOT1,PARANOT2, PARANOP,PARANOP1,  PARANOP2,I
     *PARANOT,IPARANOT1,IPARANOT2,  LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST(200)
      INTEGER I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBERST,NBIT1,NBI
     *T2,  NWRONG,LATCHJ(29), CHOICE,CHOICE1,  K1,K2,K3,K4,IA, IX,IY,IR,
     *K(22), IQ,LATCHI,IE, NPASSI,  LATCH(22),   SMFPXY(22),   NFIELD,
     * NFIELDi,   NFIELDo,   NFIELDe,  NSMFPS,  I_IN_EX,  ISMFPS(22),
     *IQSMFP(22),   MSTART,   MSMFXY,  NSMFEE,  NPASS_ph_sp,  N_ph_sp_e,
     *  N_ph_sp_g,  N_ph_sp_p,  FLUTYPE,  ANGTYPE,  IZLAST1,  IZLAST2,
     *IMUIDX1,   IMUIDX2,  IZSCORE1,  IZSCORE2,  JUSTONE(200),  MAXSCATT
     *ER,  NUMSCATTER,  ESTYPE ,  i_iaea_in,  i_iaea_out,   i_log,   i_u
     *nit_in,i_unit_out
      COMMON/LOGICALS/OLDFILE,EGSPERT
      LOGICAL OLDFILE,    EGSPERT
      COMMON/RWPHSP/ STRING_TEMP_ZLAST_OUT,  STRING_TEMP_OUT,  IHSTRY_PH
     *SP,   iaea_dummy_long,  NHSTRY_DOS,   ESHORT,  WEIGHTTMP,  WT_PHSP
     *_SHORT,  X_PHSP_SHORT,  Y_PHSP_SHORT,  Z_PHSP_SHORT,  U_PHSP_SHORT
     *,  V_PHSP_SHORT,  W_PHSP_SHORT,  ZLAST_PHSP_SHORT,  MUIDX_PHSP_SHO
     *RT,  EKMAX_PHSP_SHORT,  EKMINE_PHSP_SHORT,  NINC_PHSP_SHORT,  dosx
     *yz2beam_index,  iaea_extra_floats,  NUM_PHSP_TOT,  PHOT_PHSP_TOT,
     *iaea_result,   iaea_n_stat,  iaea_q_typ,  iaea_typ_q,  iaea_q_inde
     *x,  I_PHSP,  IERR_PHSP,  LATCHTMP,  WRITE_PHSP_COUNTER,  WRITE_PHS
     *P_SOFAR,  PHSP_RESTART,  PHSP_RECL,  PHSP_RECL_OLD,  iaea_extra_in
     *ts,  iaea_extra_int_types,iaea_extra_float_types,   iaea_i_zlast,
     * iaea_i_latch,  iaea_i_muidx,  iaea_n_extra_ints,  iaea_n_extra_fl
     *oats,  i_iaea_open_for_write,   iaea_i_zlast_out,  iaea_i_latch_ou
     *t,  iaea_i_muidx_out,  dosxyz2beam_izscore,  MODE_RW
      CHARACTER*32000 STRING_TEMP_ZLAST_OUT(1)
      CHARACTER*28000 STRING_TEMP_OUT(1)
      INTEGER*8 IHSTRY_PHSP(1),iaea_dummy_long,NHSTRY_DOS
      REAL*4 ESHORT,WEIGHTTMP,WT_PHSP_SHORT,X_PHSP_SHORT,Y_PHSP_SHORT, Z
     *_PHSP_SHORT,U_PHSP_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,ZLAST_PHSP_SHO
     *RT, MUIDX_PHSP_SHORT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP
     *_SHORT,dosxyz2beam_index, iaea_extra_floats(10)
      INTEGER NUM_PHSP_TOT,PHOT_PHSP_TOT, iaea_result,iaea_n_stat,iaea_q
     *_typ(3),iaea_typ_q(3),iaea_q_index,I_PHSP, IERR_PHSP,LATCHTMP, WRI
     *TE_PHSP_COUNTER(1),WRITE_PHSP_SOFAR(1), PHSP_RESTART,PHSP_RECL,PHS
     *P_RECL_OLD,iaea_extra_ints(10), iaea_extra_int_types(10),iaea_extr
     *a_float_types(10),iaea_i_zlast, iaea_i_latch,iaea_i_muidx,iaea_n_e
     *xtra_ints,iaea_n_extra_floats, i_iaea_open_for_write,iaea_i_zlast_
     *out,iaea_i_latch_out, iaea_i_muidx_out,dosxyz2beam_izscore
      CHARACTER*5 MODE_RW
      INTEGER LATCH_TEMP_OUT,NUM_PHSP_TOFLUSH,TEMP_PHSP_COUNTER,REM_PHSP
      REAL*4 REAL_TEMP_OUT(7)
      CHARACTER*32 REC_TEMP_OUT
      INTEGER lnblnk1
      LETTER1(1)='-1'
      LETTER1(2)=' 0'
      LETTER1(3)=' 1'
      LETTER1(4)='2'
      LETTER1(5)=' 3'
      LETTER2(0)=' 0'
      LETTER2(1)=' 1'
      LETTER2(2)=' 2'
      LETTER2(3)=' 3'
      LETTER2(4)=' 4'
      LETTER2(5)=' 5'
      LETTER2(6)=' 6'
      LETTER2(7)=' 7'
      LETTER2(8)=' 8'
      LETTER2(9)=' 9'
      LETTER2(10)='10'
      LETTER2(11)='11'
      LETTER2(12)='12'
      LETTER2(13)='13'
      LETTER2(14)='14'
      LETTER2(15)='15'
      LETTER2(16)='16'
      LETTER2(17)='17'
      LETTER2(18)='18'
      LETTER2(19)='19'
      LETTER2(20)='20'
      LETTER2(21)='21'
      LETTER2(22)='22'
      LETTER2(23)='23'
      LETTER2(24)='24'
      LETTER2(25)='25'
      LETTER2(26)='26'
      LETTER2(27)='27'
      LETTER2(28)='28'
      LETTER2(29)='29'
      LETTER2(30)='30'
      LETTER2(31)='31'
      LETTER2(32)='32'
      LETTER2(33)='33'
      LETTER2(34)='34'
      LETTER2(35)='35'
      LETTER2(36)='36'
      LETTER2(37)='37'
      LETTER2(38)='38'
      LETTER2(39)='39'
      LETTER2(40)='40'
      LETTER2(41)='41'
      LETTER2(42)='42'
      LETTER2(43)='43'
      LETTER2(44)='44'
      LETTER2(45)='45'
      LETTER2(46)='46'
      LETTER2(47)='47'
      LETTER2(48)='48'
      LETTER2(49)='49'
      LETTER2(50)='50'
      LETTER2(51)='51'
      LETTER2(52)='52'
      LETTER2(53)='53'
      LETTER2(54)='54'
      LETTER2(55)='55'
      LETTER2(56)='56'
      LETTER2(57)='57'
      LETTER2(58)='58'
      LETTER2(59)='59'
      LETTER2(60)='60'
      LETTER2(61)='61'
      LETTER2(62)='62'
      LETTER2(63)='63'
      LETTER2(64)='64'
      LETTER2(65)='65'
      LETTER2(66)='66'
      LETTER2(67)='67'
      LETTER2(68)='68'
      LETTER2(69)='69'
      LETTER2(70)='70'
      LETTER2(71)='71'
      LETTER2(72)='72'
      LETTER2(73)='73'
      LETTER2(74)='74'
      LETTER2(75)='75'
      LETTER2(76)='76'
      LETTER2(77)='77'
      LETTER2(78)='78'
      LETTER2(79)='79'
      LETTER2(80)='80'
      LETTER2(81)='81'
      LETTER2(82)='82'
      LETTER2(83)='83'
      LETTER2(84)='84'
      LETTER2(85)='85'
      LETTER2(86)='86'
      LETTER2(87)='87'
      LETTER2(88)='88'
      LETTER2(89)='89'
      LETTER2(90)='90'
      LETTER2(91)='91'
      LETTER2(92)='92'
      LETTER2(93)='93'
      LETTER2(94)='94'
      LETTER2(95)='95'
      LETTER2(96)='96'
      LETTER2(97)='97'
      LETTER2(98)='98'
      LETTER2(99)='99'
      LETTER2(100)=' 0'
      LETTER2(101)='  '
      DO 1011 I_PHSP=1,1
        WRITE_PHSP_COUNTER(I_PHSP)=0
        WRITE_PHSP_SOFAR(I_PHSP)=0
        IHSTRY_PHSP(I_PHSP)=0
1011  CONTINUE
1012  CONTINUE
      PHSP_RESTART=0
      PHSP_RECL_OLD=0
      iaea_q_typ(1)=2
      iaea_q_typ(2)=1
      iaea_q_typ(3)=3
      iaea_typ_q(1)=0
      iaea_typ_q(2)=-1
      iaea_typ_q(3)=1
      iaea_n_extra_ints=0
      iaea_n_extra_floats=0
      iaea_i_zlast=-99
      iaea_i_latch=-99
      iaea_i_muidx=-99
      i_iaea_open_for_write=0
      iaea_i_zlast_out=-99
      iaea_i_latch_out=-99
      iaea_i_muidx_out=-99
      EGSPERT = .false.
      WRITE(6,1020)
1020  FORMAT(// ' Running BEAMDP, Version v 1.9'/ ' --------------------
     *---------------------------------------'/ ' Type ? at any prompt f
     *or help                              ')
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(0)
        WRITE(6,1030)
1030    FORMAT(' Do you wish more detailed information about the file cr
     *eated '/ ' by the program?   (y/n[Default])=> ',$)
        READ(5,1040)C1
1040    FORMAT(A1)
      END IF
      IF ((.NOT.EGSPERT)) THEN
        IF (((C1.EQ.'y').OR.(C1.EQ.'Y').OR.( C1.EQ.'?'))) THEN
          CALL HELP_MESSAGE(1)
        ELSE
          EGSPERT = .true.
        END IF
      END IF
1050  CONTINUE
      WRITE(6,1060)
1060  FORMAT(/' Input a number for the operation required: ')
      WRITE(6,1070)
1070  FORMAT(' ********************************************'/)
      WRITE(6,1080)
1080  FORMAT(' (0) - Process data for beam characterization models')
      WRITE(6,1090)
1090  FORMAT(' (1) - Derive fluence vs position from ph-sp data')
      WRITE(6,1100)
1100  FORMAT(' (2) - Derive energy fluence vs position from ph-sp data')
      WRITE(6,1110)
1110  FORMAT(' (3) - Derive spectral distribution from ph-sp data')
      WRITE(6,1120)
1120  FORMAT(' (4) - Derive energy fluence distribution from ph-sp data'
     *)
      WRITE(6,1130)
1130  FORMAT(' (5) - Derive mean energy distribution from ph-sp data')
      WRITE(6,1140)
1140  FORMAT(' (6) - Derive angular distribution from ph-sp data')
      WRITE(6,1150)
1150  FORMAT(' (7) - Derive ZLAST distribution from ph-sp data')
      WRITE(6,1160)
1160  FORMAT(' (8) - Derive distribution of particle weights from ph-spd
     *ata')
      WRITE(6,1170)
1170  FORMAT(' (9) - Derive X-Y scatter plot of particles from ph-sp dat
     *a')
      WRITE(6,1180)
1180  FORMAT(' (10) - Combine two ph-sp files into one')
      WRITE(6,1190)
1190  FORMAT(' (11) - List parameters for a number of ph-sp particles ')
      WRITE(6,1200)
1200  FORMAT(' (12) - Output (x,y,z) coordinates to file (IAEA only)')
      WRITE(6,1210)
1210  FORMAT(' (13) - Quit')
      WRITE(6,1220)
1220  FORMAT(' ',$)
      READ(5,1230)C2,ITYPE
1230  FORMAT(A2,I8)
      IF ((ITYPE.EQ.0)) THEN
        ITYPE=1
      ELSE
        ITYPE=0
      END IF
      IF ((C2.EQ.'? ')) THEN
        C1='?'
        CALL HELP_MESSAGE(1)
        GOTO 1050
      END IF
      READ(C2,'(I8)')CHOICE1
      WRITE(6,1240)CHOICE1
1240  FORMAT('Selected option', I4)
      IF((CHOICE1 .EQ. 13))GOTO 1250
      IF ((CHOICE1.GT.13).OR.(CHOICE1.LT.0)) THEN
        WRITE(6,1260)
1260    FORMAT(//' SORRY, NO SUCH OPTIONS!'/)
        CALL HELP_MESSAGE(1)
        GOTO 1050
      END IF
      IF ((CHOICE1 .EQ. 0)) THEN
        CALL BEAMDP1
        GOTO 1250
      END IF
      MSTART=0
1270  CONTINUE
      IF ((CHOICE1.EQ.1).OR.(CHOICE1.EQ.2).OR.(CHOICE1.EQ.5)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(2)
        END IF
1280    WRITE(6,1290)
1290    FORMAT(/' FIELD TYPE (0-circular ring, 1-square ring, 2-rectangu
     *lar): ' /' ',$)
        READ(5,'(I15)',ERR=1300) MSMFXY
        GOTO 1310
1300    CALL HELP_MESSAGE(2)
        GOTO 1280
1310    CONTINUE
        IF (((MSMFXY.LT.0).OR.(MSMFXY.GT.2))) THEN
          WRITE(6,1320)
1320      FORMAT(/' SORRY, NO SUCH FIELD TYPE!!!'//)
          GO TO 1300
        END IF
        IF ((MSMFXY .EQ. 0)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(3)
          END IF
1330      CONTINUE
          WRITE(6,1340)
1340      FORMAT(/' Nbin,IQ(-1,0,1,2,3),RFIELD'/' ',$)
          READ(5,'(2I12,F15.0)',ERR=1350) NFIELD,IQSMFP(1),RFIELD
          GOTO 1360
1350      CALL HELP_MESSAGE(3)
          GOTO 1330
1360      IF ((IQSMFP(1).LT. -1).OR.(IQSMFP(1).GT.3).OR.(NFIELD.LT.0).OR
     *    .(NFIELD .GT. 200).OR.(RFIELD.LT.0.)) THEN
            WRITE(6,1370)
1370        FORMAT(//' SORRY, RFIELD SHOULD BE > 0.0'/ '        NFIELD S
     *HOULD BE BETWEEN 1 AND $NB')
            WRITE(6,1380)
1380        FORMAT(' AND    IQ SHOULD BE -1, 0, 1, 2 OR 3!'//)
            GO TO 1350
          END IF
          IF ((NFIELD.EQ.0)) THEN
            NFIELD=40
            WRITE(6,1390)
1390        FORMAT(//' Nbin DEFAULTS TO 40'/)
          END IF
          IF ((RFIELD.EQ.0.)) THEN
            RFIELD=15.
            WRITE(6,1400)
1400        FORMAT(//' RFIELD DEFAULTS TO 15.0'/)
          END IF
          IF ((IQSMFP(1).EQ.0)) THEN
            WRITE(6,1410)
1410        FORMAT(/' IQ = 0 (ONLY SCORE FOR PHOTONS)')
          END IF
        ELSE IF((MSMFXY .EQ. 1)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(4)
          END IF
1420      CONTINUE
          WRITE(6,1430)
1430      FORMAT(/' Nbin,IQ(-1,0,1,2,3),HALF-WIDTH'/' ',$)
          READ(5,'(2I12,F15.0)',ERR=1440) NFIELD,IQSMFP(1),RFIELD
          GOTO 1450
1440      CALL HELP_MESSAGE(4)
          GOTO 1420
1450      IF ((IQSMFP(1).LT.-1).OR.(IQSMFP(1).GT.3).OR.(NFIELD.LT.0).OR.
     *    (NFIELD.GT.200).OR.(RFIELD.LT.0.)) THEN
            WRITE(6,1460)
1460        FORMAT(//' SORRY, RFIELD SHOULD BE > 0.0'/ '        NFIELD S
     *HOULD BE BETWEEN 1 AND $NB')
            WRITE(6,1470)
1470        FORMAT(' AND    IQ SHOULD BE -1, 0, 1, 2 OR 3!'//)
            GO TO 1440
          END IF
          IF ((NFIELD.EQ.0.)) THEN
            NFIELD=40
            WRITE(6,1480)
1480        FORMAT(/' Nbin defaults to 40'/)
          END IF
          IF ((RFIELD.EQ.0.)) THEN
            RFIELD=15.
            WRITE(6,1490)
1490        FORMAT(/' HALF-WIDTH DEFAULTS TO 15.0'/)
          END IF
          IF ((IQSMFP(1).EQ.0)) THEN
            WRITE(6,1500)
1500        FORMAT(/' IQ = 0 (ONLY SCORE FOR PHOTONS)')
          END IF
        ELSE IF((MSMFXY .EQ. 2)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(5)
          END IF
1510      CONTINUE
          WRITE(6,1520)
1520      FORMAT(/' Nbin,Orient(0:along x,1: y),IQ(-1,0,1,2,3),Xmin,Xmax
     *,Ymin,Ymax'/ ' ',$)
          READ(5,1530,ERR=1540) NFIELD,ISMFPS(1),IQSMFP(1),SMFMNX(1),SMF
     *    MXX(1),SMFMNY(1),SMFMXY(1)
1530      FORMAT(3I12,4F15.0)
          WRITE(6,1550)NFIELD,ISMFPS(1),IQSMFP(1),SMFMNX(1),SMFMXX(1),SM
     *    FMNY(1),SMFMXY(1)
1550      FORMAT(I5, I3,I4,4F10.4)
          GOTO 1560
1540      CALL HELP_MESSAGE(5)
          GOTO 1510
1560      IF ((SMFMNX(1).EQ.0.).AND.(SMFMNY(1).EQ.0.).AND.(SMFMXX(1).EQ.
     *    0.).AND. (SMFMXY(1).EQ.0.)) THEN
            WRITE(6,1570)
1570        FORMAT(/' Xmin,Xmax,Ymin,Ymax default to -15,15,-15,15')
            SMFMNX(1)=-15.
            SMFMXX(1)=15.
            SMFMNY(1)=-15.
            SMFMXY(1)=15.
          END IF
          IF ((SMFMNX(1).GT.SMFMXX(1)).OR.(SMFMNY(1).GT.SMFMXY(1)).OR.(I
     *    QSMFP(1).LT.-1).OR. (IQSMFP(1).GT.3).OR.(NFIELD.LT.1).OR.(NFIE
     *    LD.GT.200).OR.(ISMFPS(1).LT.0).OR.(ISMFPS(1).GT.1)) THEN
            WRITE(6,1580)
1580        FORMAT(//' SORRY, INPUTS SHOULD BE Xmin < Xmax,'/ '        N
     *FIELD SHOULD BE BETWEEN 1 AND $NB'/ '                         Ymin
     * < Ymax,'/ '        Orientation    = 0  or   1')
            WRITE(6,1590)
1590        FORMAT(' AND    IQ SHOULD BE -1, 0, 1, 2 OR 3!'//)
            GO TO 1540
          END IF
        END IF
      ELSE IF((CHOICE1 .EQ. 3 .OR. CHOICE1 .EQ. 4)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(6)
        END IF
1600    CONTINUE
        WRITE(6,1610)
1610    FORMAT(/' IQ(-1,0,1,2,3),Xmin,Xmax,Ymin,Ymax(or Rmin,Rmax)'/' ',
     *$)
        READ(5,'(I12,4F15.0)',ERR=1620) IQSMFP(1),SMFMNX(1),SMFMXX(1),SM
     *  FMNY(1),SMFMXY(1)
        WRITE(6,1630)IQSMFP(1),SMFMNX(1),SMFMXX(1),SMFMNY(1),SMFMXY(1)
1630    FORMAT(I4,4F10.4)
        GOTO 1640
1620    CALL HELP_MESSAGE(6)
        GOTO 1600
1640    IF ((SMFMNX(1).GT.SMFMXX(1)).OR.(SMFMNY(1).GT.SMFMXY(1)).OR.(IQS
     *  MFP(1).LT.-1).OR. (IQSMFP(1).GT.3)) THEN
          WRITE(6,1650)
1650      FORMAT(//' SORRY, INPUTS SHOULD BE Xmin < Xmax,' / '          
     *     Ymin < Ymax,')
          WRITE(6,1660)
1660      FORMAT(' AND    IQ SHOULD BE -1, 0, 1, OR 2!'//)
          GO TO 1620
        END IF
        IF ((SMFMNX(1).EQ.0.).AND.(SMFMNY(1).EQ.0.).AND.(SMFMXX(1).EQ.0.
     *  ).AND.(SMFMXY(1).EQ.0.)) THEN
          WRITE(6,1670)
1670      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax default to -
     *15,15,-15,15')
          SMFMNX(1)=-15.
          SMFMXX(1)=15.
          SMFMNY(1)=-15.
          SMFMXY(1)=15.
          FLUAREA=(SMFMXX(1)-SMFMNX(1))*(SMFMXY(1)-SMFMNY(1))
          MSMFXY=1
        ELSE IF((SMFMNY(1).EQ.0.).AND.(SMFMXY(1).EQ.0.)) THEN
          IF ((SMFMNX(1).LT.0.)) THEN
            SMFMNX(1)=0.
            WRITE(6,1680)
1680        FORMAT(/' Rmin defaults to 0.')
          END IF
          IF ((SMFMXX(1).LE.0.)) THEN
            SMFMXX(1)=15.
            WRITE(6,1690)
1690        FORMAT(/' Rmax defaults to 15.')
          END IF
          WRITE(6,1700)SMFMNX(1),SMFMXX(1)
1700      FORMAT(/' Annular region: Rmin,Rmax=',2F10.3)
          FLUAREA=3.1415927*(SMFMXX(1)**2-SMFMNX(1)**2)
          MSMFXY=0
        ELSE
          WRITE(6,1710)SMFMNX(1),SMFMXX(1),SMFMNY(1),SMFMXY(1)
1710      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax =',4F10.3)
          FLUAREA=(SMFMXX(1)-SMFMNX(1))*(SMFMXY(1)-SMFMNY(1))
          MSMFXY=1
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(7)
        END IF
1720    WRITE(6,1730)
1730    FORMAT(/' Nbin, Emin, Emax (in MeV, kinetic only) for the distri
     *bution ='/' ',$)
        READ(5,'(I15,2F15.0)',ERR=1740) NSMFEE,SMFMNE,SMFMXE
        WRITE(6,1750)NSMFEE,SMFMNE,SMFMXE
1750    FORMAT(I6,2F12.4)
        GOTO 1760
1740    CALL HELP_MESSAGE(7)
        GOTO 1720
1760    IF (((SMFMNE.LT.0.0).OR.(SMFMNE.GT.SMFMXE).OR.(SMFMXE.LT.0.0).OR
     *  .(NSMFEE.GT.200).OR.(NSMFEE.LT.1))) THEN
          WRITE(6,1770)200
1770      FORMAT(/' SORRY, ENERGIES SHOULD BE Emax > Emin > 0.0 '/ 'AND 
     *BIN NUMBER MUST BE BETWEEN 1 AND ',I4,'!!!'//)
          GO TO 1740
        END IF
      ELSE IF((CHOICE1 .EQ. 6)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(8)
        END IF
1780    CONTINUE
        WRITE(6,1790)
1790    FORMAT(/' IQ(-1,0,1,2,3),Xmin,Xmax,Ymin,Ymax(or Rmin,Rmax)'/' ',
     *$)
        READ(5,'(I12,4F15.0)',ERR=1800) IQSMFP(1),SMFMNX(1),SMFMXX(1),SM
     *  FMNY(1),SMFMXY(1)
        WRITE(6,1810)IQSMFP(1),SMFMNX(1),SMFMXX(1),SMFMNY(1),SMFMXY(1)
1810    FORMAT(I4,4F10.4)
        GOTO 1820
1800    CALL HELP_MESSAGE(8)
        GOTO 1780
1820    IF ((SMFMNX(1).GT.SMFMXX(1)).OR.(SMFMNY(1).GT.SMFMXY(1)).OR.(IQS
     *  MFP(1).LT.-1).OR. (IQSMFP(1).GT.3)) THEN
          WRITE(6,1830)
1830      FORMAT(//' SORRY, INPUTS SHOULD BE Xmin < Xmax,' / '          
     *     Ymin < Ymax,')
          WRITE(6,1840)
1840      FORMAT(' AND    IQ SHOULD BE -1, 0, 1, OR 2!'//)
          GO TO 1800
        END IF
        IF ((SMFMNX(1).EQ.0.).AND.(SMFMNY(1).EQ.0.).AND.(SMFMXX(1).EQ.0.
     *  ).AND.(SMFMXY(1).EQ.0.)) THEN
          WRITE(6,1850)
1850      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax default to -
     *15,15,-15,15')
          SMFMNX(1)=-15.
          SMFMXX(1)=15.
          SMFMNY(1)=-15.
          SMFMXY(1)=15.
          MSMFXY=1
        END IF
        IF ((SMFMNY(1).EQ.0.).AND.(SMFMXY(1).EQ.0.)) THEN
          IF ((SMFMNX(1).LT.0.)) THEN
            SMFMNX(1)=0.
            WRITE(6,1860)
1860        FORMAT(/' Rmin defaultS to 0.')
          END IF
          IF ((SMFMXX(1).LE.0.)) THEN
            SMFMXX(1)=15.
            WRITE(6,1870)
1870        FORMAT(/' RmAX defaultS to 15.')
          END IF
          WRITE(6,1880)SMFMNX(1),SMFMXX(1)
1880      FORMAT(/' Annular region: Rmin,Rmax=',2F8.3)
          MSMFXY=0
        ELSE
          WRITE(6,1890)SMFMNX(1),SMFMXX(1),SMFMNY(1),SMFMXY(1)
1890      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax =',4F8.3)
          MSMFXY=1
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(9)
        END IF
1900    WRITE(6,1910)
1910    FORMAT(/' Nbin, Amin, Amax (in degree) for the distribution ='/'
     * ',$)
        READ(5,'(I15,2F15.0)',ERR=1920) NSMFEE,SMFMNE,SMFMXE
        GOTO 1930
1920    CALL HELP_MESSAGE(9)
        GOTO 1900
1930    IF (((SMFMNE.LT.0.0).OR.(SMFMNE.GT.SMFMXE).OR.(SMFMXE.GT.180.).O
     *  R.(NSMFEE.GT.200).OR. (NSMFEE.LT.0))) THEN
          WRITE(6,1940)
1940      FORMAT(/' SORRY, ANGLE SHOULD BE 180 > Amax > Amin > 0.0 '/ ' 
     *  AND BIN NUMBER MUST BE BETWEEN 1 AND ',I4,'!!!'//)
          GO TO 1920
        END IF
        IF (((SMFMNE.EQ.0.0).AND.(SMFMXE.EQ.0.)).OR.(NSMFEE.EQ.0)) THEN
          NSMFEE=200
          SMFMNE=0.
          SMFMXE=90.
          WRITE(6,1950)200
1950      FORMAT(/' Nbin, Amin, Amax default to ',I2,', 0., 90. ')
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(52)
        END IF
1960    WRITE(6,1970)
1970    FORMAT(/' Emin, Emax (in MeV, kinetic only) FOR THE ANGULAR DIST
     *RIBUTION ='/' ',$)
        READ(5,'(2F15.0)',ERR=1980) SMFMNZ(1),SMFMXZ(1)
        GOTO 1990
1980    CALL HELP_MESSAGE(52)
        GOTO 1960
1990    IF (((SMFMNZ(1).LT.0.0).OR.(SMFMNZ(1).GT.SMFMXZ(1)).OR.(SMFMXZ(1
     *  ).LT.0.0))) THEN
          WRITE(6,2000)
2000      FORMAT(/' SORRY, ENERGIES SHOULD BE Emax > Emin > 0.0 '//)
          GO TO 1980
        END IF
        IF ((SMFMNZ(1).EQ.0.0 .AND. SMFMXZ(1).EQ.0.0)) THEN
          SMFMNZ(1)=0.0
          SMFMXZ(1)=999.0
          WRITE(6,2010)
2010      FORMAT(/' Emin, Emax default to 0 MeV, 999 MeV. ')
        END IF
      ELSE IF((CHOICE1 .EQ. 7)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(10)
        END IF
2020    CONTINUE
        WRITE(6,2030)
2030    FORMAT(/' IQ(-1,0,1,2,3),Xmin,Xmax,Ymin,Ymax(or Rmin,Rmax)'/' ',
     *$)
        READ(5,'(I12,4F15.0)',ERR=2040) IQSMFP(1),SMFMNX(1),SMFMXX(1),SM
     *  FMNY(1),SMFMXY(1)
        GOTO 2050
2040    CALL HELP_MESSAGE(10)
        GOTO 2020
2050    IF ((SMFMNX(1).GT.SMFMXX(1)).OR.(SMFMNY(1).GT.SMFMXY(1)).OR.(IQS
     *  MFP(1).LT.-1).OR. (IQSMFP(1).GT.3)) THEN
          WRITE(6,2060)
2060      FORMAT(//' SORRY, INPUTS SHOULD BE Xmin < Xmax,' / '          
     *     Ymin < Ymax,')
          WRITE(6,2070)
2070      FORMAT(' AND    IQ SHOULD BE -1, 0, 1, OR 2!'//)
          GO TO 2040
        END IF
        IF ((SMFMNX(1).EQ.0.).AND.(SMFMNY(1).EQ.0.).AND.(SMFMXX(1).EQ.0.
     *  ).AND.(SMFMXY(1).EQ.0.)) THEN
          WRITE(6,2080)
2080      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax default to -
     *15,15,-15,15')
          SMFMNX(1)=-15.
          SMFMXX(1)=15.
          SMFMNY(1)=-15.
          SMFMXY(1)=15.
          MSMFXY=1
        END IF
        IF ((SMFMNY(1).EQ.0.).AND.(SMFMXY(1).EQ.0.)) THEN
          IF ((SMFMNX(1).LT.0.)) THEN
            SMFMNX(1)=0.
            WRITE(6,2090)
2090        FORMAT(/' Rmin defaultS to 0.')
          END IF
          IF ((SMFMXX(1).LE.0.)) THEN
            SMFMXX(1)=15.
            WRITE(6,2100)
2100        FORMAT(/' RmAX defaultS to 15.')
          END IF
          WRITE(6,2110)SMFMNX(1),SMFMXX(1)
2110      FORMAT(/' Annular region: Rmin,Rmax=',2F8.3)
          MSMFXY=0
        ELSE
          WRITE(6,2120)SMFMNX(1),SMFMXX(1),SMFMNY(1),SMFMXY(1)
2120      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax =',4F8.3)
          MSMFXY=1
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(11)
        END IF
2130    WRITE(6,2140)
2140    FORMAT(/' Nbin, Zmin, Zmax (in cm) for the distribution ='/' ',$
     *)
        READ(5,'(I15,2F15.0)',ERR=2150) NSMFEE,SMFMNE,SMFMXE
        GOTO 2160
2150    CALL HELP_MESSAGE(11)
        GOTO 2130
2160    IF (((SMFMNE.GT.SMFMXE).OR.(NSMFEE.GT.200).OR. (NSMFEE.LT.0))) T
     *  HEN
          WRITE(6,2170)
2170      FORMAT(/' SORRY, ZLAST SHOULD BE BETWEEN Zmin AND Zmax '/ ' AN
     *D BIN NUMBER MUST BE BETWEEN 1 AND ',I4,'!!!'//)
          GO TO 2150
        END IF
        IF (((SMFMNE.EQ.0.0).AND.(SMFMXE.EQ.0.)).OR.(NSMFEE.EQ.0)) THEN
          NSMFEE=200
          SMFMNE=0.
          SMFMXE=100.
          WRITE(6,2180)200
2180      FORMAT(/' Nbin, Zmin, Zmax default to ',I2,', 0., 100. ')
        END IF
      ELSE IF((CHOICE1 .EQ. 8)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(12)
        END IF
2190    CONTINUE
        WRITE(6,2200)
2200    FORMAT(/' IQ(-1,0,1,2,3),Xmin,Xmax,Ymin,Ymax(or Rmin,Rmax)'/' ',
     *$)
        READ(5,'(I12,4F15.0)',ERR=2210) IQSMFP(1),SMFMNX(1),SMFMXX(1),SM
     *  FMNY(1),SMFMXY(1)
        GOTO 2220
2210    CALL HELP_MESSAGE(12)
        GOTO 2190
2220    IF ((SMFMNX(1).GT.SMFMXX(1)).OR.(SMFMNY(1).GT.SMFMXY(1)).OR.(IQS
     *  MFP(1).LT.-1).OR. (IQSMFP(1).GT.3)) THEN
          WRITE(6,2230)
2230      FORMAT(//' SORRY, INPUTS SHOULD BE Xmin < Xmax,'/ '           
     *   Ymin < Ymax,')
          WRITE(6,2240)
2240      FORMAT(' AND    IQ SHOULD BE -1, 0, 1, OR 2!'//)
          GO TO 2210
        END IF
        IF ((SMFMNX(1).EQ.0.).AND.(SMFMNY(1).EQ.0.).AND.(SMFMXX(1).EQ.0.
     *  ).AND.(SMFMXY(1).EQ.0.)) THEN
          WRITE(6,2250)
2250      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax default to -
     *15,15,-15,15')
          SMFMNX(1)=-15.
          SMFMXX(1)=15.
          SMFMNY(1)=-15.
          SMFMXY(1)=15.
          MSMFXY=1
        ELSE IF((SMFMNY(1).EQ.0.).AND.(SMFMXY(1).EQ.0.)) THEN
          IF ((SMFMNX(1).LT.0.)) THEN
            SMFMNX(1)=0.
            WRITE(6,2260)
2260        FORMAT(/' Rmin defaults to 0.')
          END IF
          IF ((SMFMXX(1).LE.0.)) THEN
            SMFMXX(1)=15.
            WRITE(6,2270)
2270        FORMAT(/' Rmax defaults to 15.')
          END IF
          WRITE(6,2280)SMFMNX(1),SMFMXX(1)
2280      FORMAT(/' Annular region: Rmin,Rmax=',2F10.3)
          MSMFXY=0
        ELSE
          WRITE(6,2290)SMFMNX(1),SMFMXX(1),SMFMNY(1),SMFMXY(1)
2290      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax =',4F10.3)
          MSMFXY=1
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(13)
        END IF
2300    WRITE(6,2310)
2310    FORMAT(/' Nbin, MIN WEIGHT, MAX WEIGHT FOR THE DISTRIBUTION ='/'
     * ',$)
        READ(5,'(I15,2F15.0)',ERR=2320) NSMFEE,SMFMNE,SMFMXE
        GOTO 2330
2320    CALL HELP_MESSAGE(13)
        GOTO 2300
2330    IF (((SMFMNE.LE.0).OR.(SMFMNE.GT.SMFMXE).OR.(NSMFEE.GT.200).OR.(
     *  NSMFEE.LT.1))) THEN
          WRITE(6,2340)200
2340      FORMAT(/' SORRY, MIN WEIGHT MUST BE > 0 '/ ' WITH MAX WEIGHT >
     * MIN WEIGHT '/ ' AND BIN NUMBER MUST BE BETWEEN 1 AND ',I4,'!!!'//
     *)
          GO TO 2320
        END IF
      ELSE IF((CHOICE1 .EQ. 9)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(14)
        END IF
2350    CONTINUE
        WRITE(6,2360)
2360    FORMAT(/' IQ(-1,0,1,2,3),Xmin,Xmax,Ymin,Ymax(or Rmin,Rmax)'/' ',
     *$)
        READ(5,'(I12,4F15.0)',ERR=2370) IQSMFP(1),SMFMNX(1),SMFMXX(1),SM
     *  FMNY(1),SMFMXY(1)
        GOTO 2380
2370    CALL HELP_MESSAGE(14)
        GOTO 2350
2380    IF ((SMFMNX(1).GT.SMFMXX(1)).OR.(SMFMNY(1).GT.SMFMXY(1)).OR.(IQS
     *  MFP(1).LT.-1).OR. (IQSMFP(1).GT.3)) THEN
          WRITE(6,2390)
2390      FORMAT(//' SORRY, INPUTS SHOULD BE Xmin < Xmax,' / '          
     *     Ymin < Ymax,')
          WRITE(6,2400)
2400      FORMAT(' AND    IQ SHOULD BE -1, 0, 1, OR 2!'//)
          GO TO 2370
        END IF
        IF ((SMFMNX(1).EQ.0.).AND.(SMFMNY(1).EQ.0.).AND.(SMFMXX(1).EQ.0.
     *  ).AND.(SMFMXY(1).EQ.0.)) THEN
          WRITE(6,2410)
2410      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax default to -
     *15,15,-15,15')
          SMFMNX(1)=-15.
          SMFMXX(1)=15.
          SMFMNY(1)=-15.
          SMFMXY(1)=15.
          MSMFXY=1
        ELSE IF((SMFMNY(1).EQ.0.).AND.(SMFMXY(1).EQ.0.)) THEN
          IF ((SMFMNX(1).LT.0.)) THEN
            SMFMNX(1)=0.
            WRITE(6,2420)
2420        FORMAT(/' Rmin defaults to 0.')
          END IF
          IF ((SMFMXX(1).LE.0.)) THEN
            SMFMXX(1)=15.
            WRITE(6,2430)
2430        FORMAT(/' Rmax defaults to 15.')
          END IF
          WRITE(6,2440)SMFMNX(1),SMFMXX(1)
2440      FORMAT(/' Annular region: Rmin,Rmax=',2F10.3)
          MSMFXY=0
        ELSE
          WRITE(6,2450)SMFMNX(1),SMFMXX(1),SMFMNY(1),SMFMXY(1)
2450      FORMAT(/' Rectangular region: Xmin,Xmax,Ymin,Ymax =',4F10.3)
          MSMFXY=1
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(15)
        END IF
2460    WRITE(6,2470)
2470    FORMAT(/' Emin, Emax (in MeV, kinetic only) FOR THE SCATTER PLOT
     * ='/' ',$)
        READ(5,'(2F15.0)',ERR=2480) SMFMNE,SMFMXE
        GOTO 2490
2480    CALL HELP_MESSAGE(15)
        GOTO 2460
2490    IF (((SMFMNE.LT.0.0).OR.(SMFMNE.GT.SMFMXE).OR.(SMFMXE.LT.0.0)))
     *  THEN
          WRITE(6,2500)200
2500      FORMAT(/' SORRY, ENERGIES SHOULD BE Emax > Emin > 0.0 '//)
          GO TO 2480
        END IF
      END IF
      IF ((CHOICE1.GT.0).AND.(CHOICE1.LT.10)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(16)
          CALL HELP_MESSAGE(17)
          CALL HELP_MESSAGE(18)
        END IF
2510    CONTINUE
        WRITE(6,2520)
2520    FORMAT(/' Input three integer variables on one line: I_IN_EX, Nb
     *it1,Nbit2'/ ' I_IN_EX (0-incl/excl bits, 1-excl bits, 2-incl regio
     *ns, 3-excl regions),'/ ' Nbit1   (# bits or regions to incl (if I_
     *IN_EX=0 or 2) or excl ', '(I_IN_EX=1 or 3),'/ ' Nbit2   (# bits to
     * excl (I_IN_EX=0 only)--all on one line :'/ ' ',$)
        READ(5,'(3I8)',ERR=2530)I_IN_EX,NBIT1,NBIT2
        WRITE(6,2540)I_IN_EX,NBIT1,NBIT2
2540    FORMAT(3I6)
        GOTO 2550
2530    CALL HELP_MESSAGE(16)
        CALL HELP_MESSAGE(17)
        GOTO 2510
2550    IF ((I_IN_EX .LT. 0 .OR. I_IN_EX .GT. 3)) THEN
          WRITE(6,2560)
2560      FORMAT(//' I_IN_EX OUT OF RANGE.  TRY AGAIN.')
          CALL HELP_MESSAGE(16)
          GO TO 2510
        END IF
        IF ((I_IN_EX.NE.0 .AND. NBIT2 .NE. 0)) THEN
          WRITE(6,2570)
2570      FORMAT(//' ***WARNING: Nbit2 is not 0 and you are using'/ ' I_
     *IN_EX=1, 2 or 3.  Nbit2 set to 0 here.'//)
          NBIT2=0
        END IF
        IF ((NBIT1.LT.0)) THEN
          WRITE(6,2580)
2580      FORMAT(//' SORRY, YOU SHOULD INPUT Nbit1 >= 0')
          CALL HELP_MESSAGE(17)
          GO TO 2510
        END IF
        IF ((NBIT2.LT.0)) THEN
          WRITE(6,2590)
2590      FORMAT(//' SORRY, YOU SHOULD INPUT Nbit2 >= 0')
          CALL HELP_MESSAGE(17)
          GO TO 2510
        END IF
        IF ((I_IN_EX .LT. 2 .AND. (NBIT1+NBIT2.LT.0.OR.NBIT1+NBIT2.GT.29
     *  ))) THEN
          WRITE(6,2600)
2600      FORMAT(//' SORRY, YOU SHOULD INPUT 0 <= total # of bits <= 29'
     *)
          CALL HELP_MESSAGE(16)
          CALL HELP_MESSAGE(17)
          GO TO 2510
        ELSE IF((NBIT1.LT.0.OR.NBIT1.GT.24)) THEN
          WRITE(6,2610)
2610      FORMAT(//' SORRY, YOU SHOULD INPUT 0 <= total # of regions <=2
     *4')
          CALL HELP_MESSAGE(16)
          CALL HELP_MESSAGE(17)
          GO TO 2510
        END IF
        IF ((NBIT1 .NE. 0)) THEN
2620      CONTINUE
          IF ((I_IN_EX.EQ.0.OR.I_IN_EX.EQ.2)) THEN
            WRITE(6,2630)
2630        FORMAT(/' BIT(I)/IREGION_TO_BIT(I) (I=1,Nbit1) to include:'/
     *' ',$)
          END IF
          IF ((I_IN_EX.EQ.1.OR.I_IN_EX.EQ.3)) THEN
            WRITE(6,2640)
2640        FORMAT(/' BIT(I)/IREGION_TO_BIT(I) (I=1,Nbit1) to exclude:'/
     *' ',$)
          END IF
          READ(5,'(29I8)',ERR=2650)(LATCH(I),I=1,NBIT1)
          GOTO 2660
2650      CALL HELP_MESSAGE(18)
          GOTO 2620
2660      CONTINUE
            DO 2661 I=1,NBIT1
            IF ((I_IN_EX .LT. 2 .AND. (LATCH(I).LT.0.OR.LATCH(I).GT.28))
     *      ) THEN
              WRITE(6,2670)
2670          FORMAT(//' SORRY, YOU SHOULD INPUT 0 <= BIT(I) <= 28')
              CALL HELP_MESSAGE(18)
              GOTO 2620
            ELSE IF((I_IN_EX .GE. 2 .AND. (LATCH(I).LT.0.OR.LATCH(I).GT.
     *      23))) THEN
              WRITE(6,2680)
2680          FORMAT(//' SORRY, YOU SHOULD INPUT 0 <= IREGION_TO_BIT(I)<
     *= 23')
              CALL HELP_MESSAGE(18)
              GOTO 2620
            END IF
2661      CONTINUE
2662      CONTINUE
        END IF
        IF ((NBIT2 .NE. 0)) THEN
2690      WRITE(6,2700)
2700      FORMAT(/' BIT(I)/IREGION_TO_BIT(I) (I=Nbit1+1,Nbit1+Nbit2) toe
     *xclude:'/' ',$)
          READ(5,'(29I8)',ERR=2710)(LATCH(NBIT1+I),I=1,NBIT2)
          GOTO 2720
2710      CALL HELP_MESSAGE(18)
          GOTO 2690
2720      CONTINUE
            DO 2721 I=1,NBIT2
            IF ((LATCH(NBIT1+I).LT.0 .OR. LATCH(NBIT1+I).GT.28)) THEN
              WRITE(6,2730)
2730          FORMAT(//' SORRY, YOU SHOULD INPUT 0 <= BIT(I) <= 28')
              CALL HELP_MESSAGE(18)
              GOTO 2690
            END IF
2721      CONTINUE
2722      CONTINUE
        END IF
      END IF
      IF ((CHOICE1.EQ.11)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(19)
        END IF
2740    WRITE(6,2750)
2750    FORMAT(/' From particle no. (default=1), to particle no. (defaul
     *t=100),' ' IQ(-1,0,1,2,3) =?'/' ',$)
        READ(5,'(3I12)',ERR=2760)NUMBERST,NUMBERP,IQSMFP(1)
        GOTO 2770
2760    CALL HELP_MESSAGE(19)
        GOTO 2740
2770    IF ((NUMBERST.LT.1)) THEN
          NUMBERST=1
          WRITE(6,2780)
2780      FORMAT(/' First particle no. defaults to 1!')
        END IF
        IF ((NUMBERP.LT.1)) THEN
          NUMBERP=100
          WRITE(6,2790)
2790      FORMAT(/' Last particle no. defaults to 100!')
        END IF
        IF ((IQSMFP(1).LT.-1).OR.(IQSMFP(1).GT.3)) THEN
          IQSMFP(1)=2
          WRITE(6,2800)
2800      FORMAT(/' IQ defaults to 2 (all the particles)!')
        ELSE IF((IQSMFP(1).EQ.0)) THEN
          WRITE(6,2810)
2810      FORMAT(/' IQ = 0 (only for photons)')
        END IF
      END IF
      IF ((CHOICE1.EQ.12)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(84)
        END IF
2820    WRITE(6,2830)
2830    FORMAT(/' min. MU, max. MU, no. of particles to plot '/ ' (defau
     *lts are 0, 1, and all particles in the range): ',$)
        READ(5,*) mumin,mumax,NUMBERP
        IF((mumin.LT.0))mumin=0.0
        IF((mumax.LT.mumin.OR.mumax.EQ.0))mumax=1.0
        WRITE(6,2840)mumin,mumax
2840    FORMAT(/' Will output (X,Y,Z) over MU range ',F8.3,' to ',F8.3)
        IF ((NUMBERP.LE.0)) THEN
          WRITE(6,2850)
2850      FORMAT(' Will output all particles in the range.'/)
        ELSE
          WRITE(6,2860)NUMBERP
2860      FORMAT(' Will output ',I5,' particles. '/)
        END IF
      END IF
      IF ((MSTART.EQ.0 .AND. ((CHOICE1.GT.0 .AND. CHOICE1.LT.10).OR.CHOI
     *CE1.EQ.11.OR.CHOICE1.EQ.12))) THEN
        CALL READNAME
        CALL OPENFILE
      END IF
      IF ((MSTART.EQ.0 .AND. CHOICE1.EQ.11)) THEN
        IF ((MODE_RW.EQ.'MODE0')) THEN
          WRITE(6,2870)
2870      FORMAT(/' ENERGY  IQ     X', 5X, '  Y',4X,'  U',4X,'  V',4X,' 
     *W',4X,'WEIGHT',6X, ' LATCH (set=1, not set=0)'/)
        ELSE IF((MODE_RW.EQ.'MODE2')) THEN
          WRITE(6,2880)
2880      FORMAT(/' ENERGY  IQ     X', 5X, '  Y',4X,'  U',4X,'  V',4X,' 
     *W',4X,' ZLAST   WEIGHT',6X, ' LATCH (set=1, not set=0)'/)
        END IF
        CALL READ_DATA
      END IF
      IF ((MSTART.EQ.0 .AND. CHOICE1.EQ.12)) THEN
        IF ((IMUIDX1.EQ.0)) THEN
          WRITE(6,2890)
2890      FORMAT(' This file does not include MU for each particle.'/ 'W
     *ill not filter based on MU range.'/)
        END IF
        WRITE(6,2900)
2900    FORMAT(/' Name of file to save data for graph plotting:'/' ',$)
        READ(5,2910)SPCNAM
2910    FORMAT(A80)
        OPEN(UNIT=3,STATUS='UNKNOWN',FILE=SPCNAM)
        CALL READ_DATA
        CLOSE(3)
      END IF
      IF ((CHOICE1.GT.0 .AND. CHOICE1.LT.10)) THEN
        IF ((MSTART.EQ.0)) THEN
          WRITE(6,2920)
2920      FORMAT(/' Name of file to save data for graph plotting:'/' ',$
     *)
          READ(5,2930)SPCNAM
2930      FORMAT(A80)
          WRITE(6,2940)SPCNAM
2940      FORMAT(' File name input is:', A80)
          OPEN(UNIT=3,STATUS='UNKNOWN',FILE=SPCNAM)
          IF ((CHOICE1.LT.9)) THEN
            IF (.NOT.EGSPERT) THEN
              CALL HELP_MESSAGE(20)
            END IF
2950        WRITE(6,2960)
2960        FORMAT(/' INPUT GRAPH TYPE (0: normal, 1: histogram) ')
            WRITE(6,2970)
2970        FORMAT(/' ',$)
            READ(5,2980)C1
2980        FORMAT(A1)
            IF ((C1.EQ.'?')) THEN
              CALL HELP_MESSAGE(20)
              GOTO 2950
            END IF
            READ(C1,'(I8)')ITYPE
            IF ((ITYPE.NE.1)) THEN
              ITYPE=0
            END IF
            IF ((CHOICE1.GE.1 .AND. CHOICE1.LE.4)) THEN
              IF (.NOT.EGSPERT) THEN
                CALL HELP_MESSAGE(21)
                IF ((CHOICE1 .EQ. 2 .OR. CHOICE1 .EQ. 4)) THEN
                  WRITE(6,2990)
2990              FORMAT(/' The fluence being estimated is the energy fl
     *uence'/)
                END IF
              END IF
3000          WRITE(6,3010)
3010          FORMAT(/' Plot planar fluence (1) or estimate of real flue
     *nce (0--default)')
              WRITE(6,3020)
3020          FORMAT(/' ',$)
              READ(5,3030)C1
3030          FORMAT(A1)
              IF ((C1.EQ.'?')) THEN
                CALL HELP_MESSAGE(21)
                GOTO 3000
              END IF
              READ(C1,'(I8)')FLUTYPE
              IF ((FLUTYPE.NE.1)) THEN
                FLUTYPE=0
              END IF
            ELSE IF((CHOICE1.EQ.6)) THEN
              IF (.NOT.EGSPERT) THEN
                CALL HELP_MESSAGE(53)
              END IF
3040          WRITE(6,3050)
3050          FORMAT(/' Plot particles/solid angle (1) or particles/angu
     *lar bin (0--default)')
              WRITE(6,3060)
3060          FORMAT(/' ',$)
              READ(5,3070)C1
3070          FORMAT(A1)
              IF ((C1.EQ.'?')) THEN
                CALL HELP_MESSAGE(53)
                GOTO 3040
              END IF
              READ(C1,'(I8)')ANGTYPE
              IF ((ANGTYPE.NE.1)) THEN
                ANGTYPE=0
              END IF
            END IF
          END IF
        END IF
        IF ((CHOICE1.EQ.9)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(51)
          END IF
3080      WRITE(6,3090)
3090      FORMAT(/' Input the maximum number of particles to include ins
     *catter plot'/ ' (default=total number of particles in phase space 
     *file).')
          WRITE(6,3100)
3100      FORMAT(/' ',$)
          READ(5,'(I12)',ERR=3110)MAXSCATTER
          GOTO 3120
3110      CALL HELP_MESSAGE(51)
          GOTO 3080
3120      IF ((MAXSCATTER.LE.0 .OR. MAXSCATTER.GT.PARANOT)) THEN
            MAXSCATTER=PARANOT
            WRITE(6,3130)MAXSCATTER
3130        FORMAT(/' Number defaults to',I12/)
          END IF
        END IF
3140    CONTINUE
        SERIESTITLE(1:4)='IQ ='
        SERIESTITLE(5:6)=LETTER1(IQSMFP(1)+2)
        IF ((NBIT1.EQ.0 .AND. NBIT2 .EQ. 0)) THEN
          SERIESTITLE(7:60)=' NO LATCH CHECK'
        ELSE
          IF ((I_IN_EX.EQ.0)) THEN
            IF ((NBIT1 .GT. 0)) THEN
              SERIESTITLE(7:19)=' INCL BIT #: '
              DO 3151 I=1,NBIT1
                SERIESTITLE(17+3*I:60)=LETTER2(LATCH(I))
3151          CONTINUE
3152          CONTINUE
            ELSE
              SERIESTITLE(7:19)='             '
            END IF
            IF ((NBIT2.GT.0)) THEN
              SERIESTITLE(17+3*NBIT1+2:17+3*NBIT1+14)=' EXCL BIT #: '
              DO 3161 I=1,NBIT2
                SERIESTITLE(17+3*(NBIT1+I-1)+15:60)= LETTER2(LATCH(NBIT1
     *          +I))
3161          CONTINUE
3162          CONTINUE
            END IF
          ELSE IF((I_IN_EX.EQ.1)) THEN
            SERIESTITLE(7:19)=' EXCL BIT #: '
            DO 3171 I=1,NBIT1
              SERIESTITLE(17+3*I:60)=LETTER2(LATCH(I))
3171        CONTINUE
3172        CONTINUE
          END IF
          IF ((I_IN_EX.EQ.2)) THEN
            SERIESTITLE(7:26)=' INCL REGION BIT #: '
            DO 3181 I=1,NBIT1
              SERIESTITLE(24+3*I:60)=LETTER2(LATCH(I))
3181        CONTINUE
3182        CONTINUE
          ELSE IF((I_IN_EX.EQ.3)) THEN
            SERIESTITLE(7:26)=' EXCL REGION BIT #: '
            DO 3191 I=1,NBIT1
              SERIESTITLE(24+3*I:60)=LETTER2(LATCH(I))
3191        CONTINUE
3192        CONTINUE
          END IF
        END IF
        IF ((CHOICE1 .EQ. 9)) THEN
          WRITE(6,*)'Begin writing graph data into file...'
          IF ((MSTART.EQ.0)) THEN
            WRITE(3,3200)PSDNAM(1:lnblnk1(PSDNAM))
3200        FORMAT('@g0 type xy '/ ,'@    title "X-Y scatter plot"'/ ,'@
     *    subtitle "',A,'"'/ ,'@    legend on'/ ,'@    legend x1 0.6'/ ,
     *'@    legend y1 0.75'/ ,'@    view xmin 0.250000'/ ,'@    xaxis  l
     *abel "X (cm)"'/ ,'@    yaxis  label "Y (cm)"')
          END IF
          IIII=61
3211      CONTINUE
            IIII = IIII - 1
            IF(((SERIESTITLE(IIII:IIII) .NE. ' ')))GO TO3212
          GO TO 3211
3212      CONTINUE
          WRITE(3,3220)MSTART,MSTART,MSTART,MSTART+1, MSTART,SERIESTITLE
     *    (1:IIII)
3220      FORMAT('@    s',I1,' symbol 1'/ ,'@    s',I1,' linestyle 0'/ ,
     *'@    s',I1,' symbol color ',I2/ ,'@    legend string ',I2,' "',A,
     *'"')
        END IF
        CALL READ_DATA
        IF ((CHOICE1.LT.9)) THEN
          WRITE(6,*)'Begin writing graph data into file...'
        END IF
        YPLOTT=FLOAT(IPARANOT-1)
        IF ((CHOICE1.EQ.1)) THEN
          GRAPHTITLE='fluence vs position'
          IF ((FLUTYPE.EQ.1)) THEN
            YTITLE='planar fluence/incident particle /cm\\S-2\\N'
          ELSE
            YTITLE='fluence/incident particle /cm\\S-2\\N'
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            XTITLE='R /cm '
            DO 3231 II=1,NFIELD
              FSMFRR(1,II)=FSMFRR(1,II)+SUM_TMP(II)
              FSMFRR(2,II)=FSMFRR(2,II)+SUM_TMP(II)*SUM_TMP(II)
              FSMFRR(1,II)=FSMFRR(1,II)/NINC
              FSMFRR(2,II)=FSMFRR(2,II)/NINC
              FSMFRR(2,II)=(FSMFRR(2,II)-FSMFRR(1,II)*FSMFRR(1,II))/(NIN
     *        C-1)
              IF((FSMFRR(2,II).GT.0.))FSMFRR(2,II)=SQRT(FSMFRR(2,II))
              FSMFRR(2,II)=MIN(FSMFRR(2,II),0.9999D00*FSMFRR(1,II))
              XPLOT(II)=SQRT(FLOAT(II)/FLOAT(NFIELD))*RFIELD
              YPLOT(II)=FSMFRR(1,II)/(3.1415927*RFIELD**2/NFIELD)
              ERRYPLOT(II)=FSMFRR(2,II)/(3.1415927*RFIELD**2/NFIELD)
3231        CONTINUE
3232        CONTINUE
            HXMIN=0.
            IF ((ITYPE.EQ.0)) THEN
              DO 3241 II=NFIELD,1,-1
                IF ((II.EQ.1)) THEN
                  XPLOT(II)=(HXMIN+XPLOT(II))/2.
                ELSE
                  XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                END IF
3241          CONTINUE
3242          CONTINUE
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          ELSE IF((MSMFXY.EQ.1)) THEN
            XTITLE='X /cm '
            DO 3251 II=1,NFIELD
              FSMFRR(1,II)=FSMFRR(1,II)+SUM_TMP(II)
              FSMFRR(2,II)=FSMFRR(2,II)+SUM_TMP(II)*SUM_TMP(II)
              FSMFRR(1,II)=FSMFRR(1,II)/NINC
              FSMFRR(2,II)=FSMFRR(2,II)/NINC
              FSMFRR(2,II)=(FSMFRR(2,II)-FSMFRR(1,II)*FSMFRR(1,II))/(NIN
     *        C-1)
              IF((FSMFRR(2,II).GT.0.))FSMFRR(2,II)=SQRT(FSMFRR(2,II))
              FSMFRR(2,II)=MIN(FSMFRR(2,II),0.9999D00*FSMFRR(1,II))
              XPLOT(II)=RFIELD*SQRT(FLOAT(II)/FLOAT(NFIELD))
              YPLOT(II)=FSMFRR(1,II)/(4*RFIELD**2/NFIELD)
              ERRYPLOT(II)=FSMFRR(2,II)/(4*RFIELD**2/NFIELD)
3251        CONTINUE
3252        CONTINUE
            HXMIN=0.
            IF ((ITYPE.EQ.0)) THEN
              DO 3261 II=NFIELD,1,-1
                IF ((II.EQ.1)) THEN
                  XPLOT(II)=(HXMIN+XPLOT(II))/2.
                ELSE
                  XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                END IF
3261          CONTINUE
3262          CONTINUE
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          ELSE IF((MSMFXY.EQ.2)) THEN
            IF ((ISMFPS(1).EQ.0)) THEN
              XTITLE='X /cm'
              DO 3271 II=1,NFIELD
                FSMFXX(1,II)=FSMFXX(1,II)+SUM_TMP(II)
                FSMFXX(2,II)=FSMFXX(2,II)+SUM_TMP(II)*SUM_TMP(II)
                FSMFXX(1,II)=FSMFXX(1,II)/NINC
                FSMFXX(2,II)=FSMFXX(2,II)/NINC
                FSMFXX(2,II)=(FSMFXX(2,II)-FSMFXX(1,II)*FSMFXX(1,II))/(N
     *          INC-1)
                IF((FSMFXX(2,II).GT.0.))FSMFXX(2,II)=SQRT(FSMFXX(2,II))
                FSMFXX(2,II)=MIN(FSMFXX(2,II),0.9999D00*FSMFXX(1,II))
                XPLOT(II)=((SMFMXX(1)-SMFMNX(1))*FLOAT(II)/FLOAT(NFIELD)
     *          ) +SMFMNX(1)
                YPLOT(II)=FSMFXX(1,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMXY(1
     *          )-SMFMNY(1))/NFIELD)
                ERRYPLOT(II)=FSMFXX(2,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMX
     *          Y(1)-SMFMNY(1))/NFIELD)
3271          CONTINUE
3272          CONTINUE
              HXMIN=SMFMNX(1)
              IF ((ITYPE.EQ.0)) THEN
                DO 3281 II=NFIELD,1,-1
                  IF ((II.EQ.1)) THEN
                    XPLOT(II)=(HXMIN+XPLOT(II))/2.
                  ELSE
                    XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                  END IF
3281            CONTINUE
3282            CONTINUE
              END IF
              CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTI
     *        TLE,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
            ELSE
              XTITLE='Y /cm'
              DO 3291 II=1,NFIELD
                FSMFYY(1,II)=FSMFYY(1,II)+SUM_TMP(II)
                FSMFYY(2,II)=FSMFYY(2,II)+SUM_TMP(II)*SUM_TMP(II)
                FSMFYY(1,II)=FSMFYY(1,II)/NINC
                FSMFYY(2,II)=FSMFYY(2,II)/NINC
                FSMFYY(2,II)=(FSMFYY(2,II)-FSMFYY(1,II)*FSMFYY(1,II))/(N
     *          INC-1)
                IF((FSMFYY(2,II).GT.0.))FSMFYY(2,II)=SQRT(FSMFYY(2,II))
                FSMFYY(2,II)=MIN(FSMFYY(2,II),0.9999D00*FSMFYY(1,II))
                XPLOT(II)=((SMFMXY(1)-SMFMNY(1))*FLOAT(II)/FLOAT(NFIELD)
     *          ) +SMFMNY(1)
                YPLOT(II)=FSMFYY(1,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMXY(1
     *          )-SMFMNY(1))/NFIELD)
                ERRYPLOT(II)=FSMFYY(2,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMX
     *          Y(1)-SMFMNY(1))/NFIELD)
3291          CONTINUE
3292          CONTINUE
              HXMIN=SMFMNY(1)
              IF ((ITYPE.EQ.0)) THEN
                DO 3301 II=NFIELD,1,-1
                  IF ((II.EQ.1)) THEN
                    XPLOT(II)=(HXMIN+XPLOT(II))/2.
                  ELSE
                    XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                  END IF
3301            CONTINUE
3302            CONTINUE
              END IF
              CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTI
     *        TLE,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
            END IF
          END IF
        END IF
        IF ((CHOICE1.EQ.2)) THEN
          GRAPHTITLE='energy fluence vs position'
          IF ((FLUTYPE.EQ.1)) THEN
            YTITLE='planar energy fluence/incident particle /MeV cm\\S-2
     *\\N)'
          ELSE
            YTITLE='energy fluence/incident particle /MeV cm\\S-2\\N)'
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            XTITLE='R /cm'
            DO 3311 II=1,NFIELD
              FSMFRR(1,II)=FSMFRR(1,II)+SUM_TMP(II)
              FSMFRR(2,II)=FSMFRR(2,II)+SUM_TMP(II)*SUM_TMP(II)
              FSMFRR(1,II)=FSMFRR(1,II)/NINC
              FSMFRR(2,II)=FSMFRR(2,II)/NINC
              FSMFRR(2,II)=(FSMFRR(2,II)-FSMFRR(1,II)*FSMFRR(1,II))/(NIN
     *        C-1)
              IF((FSMFRR(2,II).GT.0.))FSMFRR(2,II)=SQRT(FSMFRR(2,II))
              FSMFRR(2,II)=MIN(FSMFRR(2,II),0.9999D00*FSMFRR(1,II))
              XPLOT(II)=SQRT(FLOAT(II)/FLOAT(NFIELD))*RFIELD
              YPLOT(II)=FSMFRR(1,II)/(3.1415927*RFIELD**2/NFIELD)
              ERRYPLOT(II)=FSMFRR(2,II)/(3.1415927*RFIELD**2/NFIELD)
3311        CONTINUE
3312        CONTINUE
            HXMIN=0.
            IF ((ITYPE.EQ.0)) THEN
              DO 3321 II=NFIELD,1,-1
                IF ((II.EQ.1)) THEN
                  XPLOT(II)=(HXMIN+XPLOT(II))/2.
                ELSE
                  XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                END IF
3321          CONTINUE
3322          CONTINUE
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          ELSE IF((MSMFXY.EQ.1)) THEN
            XTITLE='X /cm'
            DO 3331 II=1,NFIELD
              FSMFRR(1,II)=FSMFRR(1,II)+SUM_TMP(II)
              FSMFRR(2,II)=FSMFRR(2,II)+SUM_TMP(II)*SUM_TMP(II)
              FSMFRR(1,II)=FSMFRR(1,II)/NINC
              FSMFRR(2,II)=FSMFRR(2,II)/NINC
              FSMFRR(2,II)=(FSMFRR(2,II)-FSMFRR(1,II)*FSMFRR(1,II))/(NIN
     *        C-1)
              IF((FSMFRR(2,II).GT.0.))FSMFRR(2,II)=SQRT(FSMFRR(2,II))
              FSMFRR(2,II)=MIN(FSMFRR(2,II),0.9999D00*FSMFRR(1,II))
              XPLOT(II)=RFIELD*SQRT(FLOAT(II)/FLOAT(NFIELD))
              YPLOT(II)=FSMFRR(1,II)/(4*RFIELD**2/NFIELD)
              ERRYPLOT(II)=FSMFRR(2,II)/(4*RFIELD**2/NFIELD)
3331        CONTINUE
3332        CONTINUE
            HXMIN=0.
            IF ((ITYPE.EQ.0)) THEN
              DO 3341 II=NFIELD,1,-1
                IF ((II.EQ.1)) THEN
                  XPLOT(II)=(HXMIN+XPLOT(II))/2.
                ELSE
                  XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                END IF
3341          CONTINUE
3342          CONTINUE
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          ELSE IF((MSMFXY.EQ.2)) THEN
            IF ((ISMFPS(1).EQ.0)) THEN
              XTITLE='X /cm'
              DO 3351 II=1,NFIELD
                FSMFXX(1,II)=FSMFXX(1,II)+SUM_TMP(II)
                FSMFXX(2,II)=FSMFXX(2,II)+SUM_TMP(II)*SUM_TMP(II)
                FSMFXX(1,II)=FSMFXX(1,II)/NINC
                FSMFXX(2,II)=FSMFXX(2,II)/NINC
                FSMFXX(2,II)=(FSMFXX(2,II)-FSMFXX(1,II)*FSMFXX(1,II))/(N
     *          INC-1)
                IF((FSMFXX(2,II).GT.0.))FSMFXX(2,II)=SQRT(FSMFXX(2,II))
                FSMFXX(2,II)=MIN(FSMFXX(2,II),0.9999D00*FSMFXX(1,II))
                XPLOT(II)=((SMFMXX(1)-SMFMNX(1))*FLOAT(II)/FLOAT(NFIELD)
     *          ) +SMFMNX(1)
                YPLOT(II)=FSMFXX(1,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMXY(1
     *          )-SMFMNY(1))/NFIELD)
                ERRYPLOT(II)=FSMFXX(2,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMX
     *          Y(1)-SMFMNY(1))/NFIELD)
3351          CONTINUE
3352          CONTINUE
              HXMIN=SMFMNX(1)
              IF ((ITYPE.EQ.0)) THEN
                DO 3361 II=NFIELD,1,-1
                  IF ((II.EQ.1)) THEN
                    XPLOT(II)=(HXMIN+XPLOT(II))/2.
                  ELSE
                    XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                  END IF
3361            CONTINUE
3362            CONTINUE
              END IF
              CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTI
     *        TLE,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
            ELSE
              XTITLE='Y /cm'
              DO 3371 II=1,NFIELD
                FSMFYY(1,II)=FSMFYY(1,II)+SUM_TMP(II)
                FSMFYY(2,II)=FSMFYY(2,II)+SUM_TMP(II)*SUM_TMP(II)
                FSMFYY(1,II)=FSMFYY(1,II)/NINC
                FSMFYY(2,II)=FSMFYY(2,II)/NINC
                FSMFYY(2,II)=(FSMFYY(2,II)-FSMFYY(1,II)*FSMFYY(1,II))/(N
     *          INC-1)
                IF((FSMFYY(2,II).GT.0.))FSMFYY(2,II)=SQRT(FSMFYY(2,II))
                FSMFYY(2,II)=MIN(FSMFYY(2,II),0.9999D00*FSMFYY(1,II))
                XPLOT(II)=((SMFMXY(1)-SMFMNY(1))*FLOAT(II)/FLOAT(NFIELD)
     *          ) +SMFMNY(1)
                YPLOT(II)=FSMFYY(1,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMXY(1
     *          )-SMFMNY(1))/NFIELD)
                ERRYPLOT(II)=FSMFYY(2,II)/((SMFMXX(1)-SMFMNX(1))* (SMFMX
     *          Y(1)-SMFMNY(1))/NFIELD)
3371          CONTINUE
3372          CONTINUE
              HXMIN=SMFMNY(1)
              IF ((ITYPE.EQ.0)) THEN
                DO 3381 II=NFIELD,1,-1
                  IF ((II.EQ.1)) THEN
                    XPLOT(II)=(HXMIN+XPLOT(II))/2.
                  ELSE
                    XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                  END IF
3381            CONTINUE
3382            CONTINUE
              END IF
              CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTI
     *        TLE,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
            END IF
          END IF
        ELSE IF((CHOICE1.EQ.3)) THEN
          GRAPHTITLE='spectral distribution'
          IF ((FLUTYPE.EQ.1)) THEN
            YTITLE='planar fluence/MeV/incident particle /cm\\S-2\\N MeV
     *\\S-1\\N'
          ELSE
            YTITLE='fluence/MeV/incident particle /cm\\S-2\\N MeV\\S-1\\
     *N'
          END IF
          XTITLE='energy /MeV'
          DO 3391 II=1,NSMFEE
            FSMFBN1(1,1,II)=FSMFBN1(1,1,II)+SUM_TMP(II)
            FSMFBN1(2,1,II)=FSMFBN1(2,1,II)+SUM_TMP(II)*SUM_TMP(II)
            FSMFBN1(1,1,II)=FSMFBN1(1,1,II)/NINC
            FSMFBN1(2,1,II)=FSMFBN1(2,1,II)/NINC
            FSMFBN1(2,1,II)=(FSMFBN1(2,1,II)-FSMFBN1(1,1,II)*FSMFBN1(1,1
     *      ,II))/(NINC-1)
            IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(2,1,
     *      II))
            FSMFBN1(2,1,II)=MIN(FSMFBN1(2,1,II),0.9999D00*FSMFBN1(1,1,II
     *      ))
            XPLOT(II)=(SMFMXE-SMFMNE)*FLOAT(II)/FLOAT(NSMFEE)+SMFMNE
            YPLOT(II)=FSMFBN1(1,1,II)/(FLUAREA* (SMFMXE-SMFMNE)/FLOAT(NS
     *      MFEE))
            ERRYPLOT(II)=FSMFBN1(2,1,II)/(FLUAREA* (SMFMXE-SMFMNE)/FLOAT
     *      (NSMFEE))
3391      CONTINUE
3392      CONTINUE
          HXMIN=SMFMNE
          IF ((ITYPE.EQ.0)) THEN
            DO 3401 II=NSMFEE,1,-1
              IF ((II.EQ.1)) THEN
                XPLOT(II)=(HXMIN+XPLOT(II))/2.
              ELSE
                XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
              END IF
3401        CONTINUE
3402        CONTINUE
          END IF
          CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NSMFEE,MSTART, SERIESTITLE,
     *    XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
        ELSE IF((CHOICE1.EQ.4)) THEN
          GRAPHTITLE='energy fluence distribution'
          IF ((FLUTYPE.EQ.1)) THEN
            YTITLE='planar energy fluence/MeV/incident particle  /cm\\S-
     *2\\N'
          ELSE
            YTITLE='energy fluence/MeV/incident particle  /cm\\S-2\\N'
          END IF
          XTITLE='energy /MeV'
          DO 3411 II=1,NSMFEE
            FSMFBN1(1,1,II)=FSMFBN1(1,1,II)+SUM_TMP(II)
            FSMFBN1(2,1,II)=FSMFBN1(2,1,II)+SUM_TMP(II)*SUM_TMP(II)
            FSMFBN1(1,1,II)=FSMFBN1(1,1,II)/NINC
            FSMFBN1(2,1,II)=FSMFBN1(2,1,II)/NINC
            FSMFBN1(2,1,II)=(FSMFBN1(2,1,II)-FSMFBN1(1,1,II)*FSMFBN1(1,1
     *      ,II))/(NINC-1)
            IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(2,1,
     *      II))
            FSMFBN1(2,1,II)=MIN(FSMFBN1(2,1,II),0.9999D00*FSMFBN1(1,1,II
     *      ))
            XPLOT(II)=(SMFMXE-SMFMNE)*FLOAT(II)/FLOAT(NSMFEE)+SMFMNE
            YPLOT(II)=FSMFBN1(1,1,II)/(FLUAREA* (SMFMXE-SMFMNE)/FLOAT(NS
     *      MFEE))
            ERRYPLOT(II)=FSMFBN1(2,1,II)/(FLUAREA* (SMFMXE-SMFMNE)/FLOAT
     *      (NSMFEE))
3411      CONTINUE
3412      CONTINUE
          HXMIN=SMFMNE
          IF ((ITYPE.EQ.0)) THEN
            DO 3421 II=NSMFEE,1,-1
              IF ((II.EQ.1)) THEN
                XPLOT(II)=(HXMIN+XPLOT(II))/2.
              ELSE
                XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
              END IF
3421        CONTINUE
3422        CONTINUE
          END IF
          CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NSMFEE,MSTART, SERIESTITLE,
     *    XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
        ELSE IF((CHOICE1.EQ.5)) THEN
          GRAPHTITLE='mean energy distribution'
          YTITLE='mean energy /MeV'
          IF ((MSMFXY.EQ.0)) THEN
            XTITLE='R /cm'
            DO 3431 II=1,NFIELD
              FSMFBN1(1,1,II)=FSMFBN1(1,1,II)+SUM_TMP(II)
              FSMFBN1(2,1,II)=FSMFBN1(2,1,II)+SUM_TMP(II)*SUM_TMP(II)
              FSMFBN1(1,2,II)=FSMFBN1(1,2,II)+SUM_TMP2(II)
              FSMFBN1(2,2,II)=FSMFBN1(2,2,II)+SUM_TMP2(II)*SUM_TMP2(II)
              FSMFBN1(3,1,II)=FSMFBN1(3,1,II)+SUM_TMP(II)*SUM_TMP2(II)
              FSMFBN1(1,1,II)=FSMFBN1(1,1,II)/NINC
              FSMFBN1(2,1,II)=FSMFBN1(2,1,II)/NINC
              FSMFBN1(2,1,II)=(FSMFBN1(2,1,II)-FSMFBN1(1,1,II)*FSMFBN1(1
     *        ,1,II))/(NINC-1)
              IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(2,
     *        1,II))
              FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II)/FSMFBN1(1,1,
     *        II))
              FSMFBN1(1,2,II)=FSMFBN1(1,2,II)/NINC
              FSMFBN1(2,2,II)=FSMFBN1(2,2,II)/NINC
              FSMFBN1(2,2,II)=(FSMFBN1(2,2,II)-FSMFBN1(1,2,II)*FSMFBN1(1
     *        ,2,II))/(NINC-1)
              IF((FSMFBN1(2,2,II).GT.0.))FSMFBN1(2,2,II)=SQRT(FSMFBN1(2,
     *        2,II))
              FSMFBN1(2,2,II)=MIN(0.9999D00,FSMFBN1(2,2,II)/FSMFBN1(1,2,
     *        II))
              IF ((JUSTONE(II).LT.10)) THEN
                FSMFBN1(3,1,II)=0.
              ELSE
                FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/NINC-FSMFBN1(1,1,II)*FSM
     *          FBN1(1,2,II)
                FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/(FSMFBN1(1,1,II)*FSMFBN1
     *          (1,2,II)*(NINC-1))
              END IF
              FSMFBN1(2,1,II)=FSMFBN1(2,1,II)**2+FSMFBN1(2,2,II)**2-2*FS
     *        MFBN1(3,1,II)
              IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(2,
     *        1,II))
              FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II))
              XPLOT(II)=SQRT(FLOAT(II)/FLOAT(NFIELD))*RFIELD
              IF ((FSMFBN1(1,1,II).EQ.0.)) THEN
                YPLOT(II)=0
              ELSE
                YPLOT(II)=FSMFBN1(1,1,II)/FSMFBN1(1,2,II)
              END IF
              IF ((FSMFBN1(1,1,II).EQ.0.).OR.(FSMFBN1(1,2,II).EQ.0.)) TH
     *        EN
                ERRYPLOT(II)=0
              ELSE
                ERRYPLOT(II)=FSMFBN1(2,1,II)*YPLOT(II)
              END IF
3431        CONTINUE
3432        CONTINUE
            HXMIN=0.
            IF ((ITYPE.EQ.0)) THEN
              DO 3441 II=NFIELD,1,-1
                IF ((II.EQ.1)) THEN
                  XPLOT(II)=(HXMIN+XPLOT(II))/2.
                ELSE
                  XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                END IF
3441          CONTINUE
3442          CONTINUE
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          ELSE IF((MSMFXY.EQ.1)) THEN
            XTITLE='X /cm'
            DO 3451 II=1,NFIELD
              FSMFBN1(1,1,II)=FSMFBN1(1,1,II)+SUM_TMP(II)
              FSMFBN1(2,1,II)=FSMFBN1(2,1,II)+SUM_TMP(II)*SUM_TMP(II)
              FSMFBN1(1,2,II)=FSMFBN1(1,2,II)+SUM_TMP2(II)
              FSMFBN1(2,2,II)=FSMFBN1(2,2,II)+SUM_TMP2(II)*SUM_TMP2(II)
              FSMFBN1(3,1,II)=FSMFBN1(3,1,II)+SUM_TMP(II)*SUM_TMP2(II)
              FSMFBN1(1,1,II)=FSMFBN1(1,1,II)/NINC
              FSMFBN1(2,1,II)=FSMFBN1(2,1,II)/NINC
              FSMFBN1(2,1,II)=(FSMFBN1(2,1,II)-FSMFBN1(1,1,II)*FSMFBN1(1
     *        ,1,II))/(NINC-1)
              IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(2,
     *        1,II))
              FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II)/FSMFBN1(1,1,
     *        II))
              FSMFBN1(1,2,II)=FSMFBN1(1,2,II)/NINC
              FSMFBN1(2,2,II)=FSMFBN1(2,2,II)/NINC
              FSMFBN1(2,2,II)=(FSMFBN1(2,2,II)-FSMFBN1(1,2,II)*FSMFBN1(1
     *        ,2,II))/(NINC-1)
              IF((FSMFBN1(2,2,II).GT.0.))FSMFBN1(2,2,II)=SQRT(FSMFBN1(2,
     *        2,II))
              FSMFBN1(2,2,II)=MIN(0.9999D00,FSMFBN1(2,2,II)/FSMFBN1(1,2,
     *        II))
              IF ((JUSTONE(II).LT.10)) THEN
                FSMFBN1(3,1,II)=0.
              ELSE
                FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/NINC-FSMFBN1(1,1,II)*FSM
     *          FBN1(1,2,II)
                FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/(FSMFBN1(1,1,II)*FSMFBN1
     *          (1,2,II)*(NINC-1))
              END IF
              FSMFBN1(2,1,II)=FSMFBN1(2,1,II)**2+FSMFBN1(2,2,II)**2-2*FS
     *        MFBN1(3,1,II)
              IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(2,
     *        1,II))
              FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II))
              XPLOT(II)=RFIELD*SQRT(FLOAT(II)/FLOAT(NFIELD))
              IF ((FSMFBN1(1,1,II).EQ.0.)) THEN
                YPLOT(II)=0
              ELSE
                YPLOT(II)=FSMFBN1(1,1,II)/FSMFBN1(1,2,II)
              END IF
              IF ((FSMFBN1(1,1,II).EQ.0.).OR.(FSMFBN1(1,2,II).EQ.0.)) TH
     *        EN
                ERRYPLOT(II)=0
              ELSE
                ERRYPLOT(II)=FSMFBN1(2,1,II)*YPLOT(II)
              END IF
3451        CONTINUE
3452        CONTINUE
            HXMIN=0.
            IF ((ITYPE.EQ.0)) THEN
              DO 3461 II=NFIELD,1,-1
                IF ((II.EQ.1)) THEN
                  XPLOT(II)=(HXMIN+XPLOT(II))/2.
                ELSE
                  XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                END IF
3461          CONTINUE
3462          CONTINUE
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          ELSE IF((MSMFXY.EQ.2)) THEN
            IF ((ISMFPS(1).EQ.0)) THEN
              XTITLE='X /cm'
              DO 3471 II=1,NFIELD
                FSMFBN1(1,1,II)=FSMFBN1(1,1,II)+SUM_TMP(II)
                FSMFBN1(2,1,II)=FSMFBN1(2,1,II)+SUM_TMP(II)*SUM_TMP(II)
                FSMFBN1(1,2,II)=FSMFBN1(1,2,II)+SUM_TMP2(II)
                FSMFBN1(2,2,II)=FSMFBN1(2,2,II)+SUM_TMP2(II)*SUM_TMP2(II
     *          )
                FSMFBN1(3,1,II)=FSMFBN1(3,1,II)+SUM_TMP(II)*SUM_TMP2(II)
                FSMFBN1(1,1,II)=FSMFBN1(1,1,II)/NINC
                FSMFBN1(2,1,II)=FSMFBN1(2,1,II)/NINC
                FSMFBN1(2,1,II)=(FSMFBN1(2,1,II)-FSMFBN1(1,1,II)*FSMFBN1
     *          (1,1,II))/(NINC-1)
                IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(
     *          2,1,II))
                FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II)/FSMFBN1(1,
     *          1,II))
                FSMFBN1(1,2,II)=FSMFBN1(1,2,II)/NINC
                FSMFBN1(2,2,II)=FSMFBN1(2,2,II)/NINC
                FSMFBN1(2,2,II)=(FSMFBN1(2,2,II)-FSMFBN1(1,2,II)*FSMFBN1
     *          (1,2,II))/(NINC-1)
                IF((FSMFBN1(2,2,II).GT.0.))FSMFBN1(2,2,II)=SQRT(FSMFBN1(
     *          2,2,II))
                FSMFBN1(2,2,II)=MIN(0.9999D00,FSMFBN1(2,2,II)/FSMFBN1(1,
     *          2,II))
                IF ((JUSTONE(II).LT.10)) THEN
                  FSMFBN1(3,1,II)=0.
                ELSE
                  FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/NINC-FSMFBN1(1,1,II)*F
     *            SMFBN1(1,2,II)
                  FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/(FSMFBN1(1,1,II)*FSMFB
     *            N1(1,2,II)*(NINC-1))
                END IF
                FSMFBN1(2,1,II)=FSMFBN1(2,1,II)**2+FSMFBN1(2,2,II)**2-2*
     *          FSMFBN1(3,1,II)
                IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(
     *          2,1,II))
                FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II))
                XPLOT(II)=((SMFMXX(1)-SMFMNX(1))*FLOAT(II)/FLOAT(NFIELD)
     *          ) +SMFMNX(1)
                IF ((FSMFBN1(1,1,II).EQ.0.)) THEN
                  YPLOT(II)=0.
                ELSE
                  YPLOT(II)=FSMFBN1(1,1,II)/FSMFBN1(1,2,II)
                END IF
                IF ((FSMFBN1(1,1,II).EQ.0.).OR.(FSMFBN1(1,2,II).EQ.0.))
     *          THEN
                  ERRYPLOT(II)=0
                ELSE
                  ERRYPLOT(II)=FSMFBN1(2,1,II)*YPLOT(II)
                END IF
3471          CONTINUE
3472          CONTINUE
              HXMIN=SMFMNX(1)
              IF ((ITYPE.EQ.0)) THEN
                DO 3481 II=NFIELD,1,-1
                  IF ((II.EQ.1)) THEN
                    XPLOT(II)=(HXMIN+XPLOT(II))/2.
                  ELSE
                    XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                  END IF
3481            CONTINUE
3482            CONTINUE
              END IF
              CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTI
     *        TLE,XTITLE, YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
            ELSE
              XTITLE='Y /cm'
              DO 3491 II=1,NFIELD
                FSMFBN1(1,1,II)=FSMFBN1(1,1,II)+SUM_TMP(II)
                FSMFBN1(2,1,II)=FSMFBN1(2,1,II)+SUM_TMP(II)*SUM_TMP(II)
                FSMFBN1(1,2,II)=FSMFBN1(1,2,II)+SUM_TMP2(II)
                FSMFBN1(2,2,II)=FSMFBN1(2,2,II)+SUM_TMP2(II)*SUM_TMP2(II
     *          )
                FSMFBN1(3,1,II)=FSMFBN1(3,1,II)+SUM_TMP(II)*SUM_TMP2(II)
                FSMFBN1(1,1,II)=FSMFBN1(1,1,II)/NINC
                FSMFBN1(2,1,II)=FSMFBN1(2,1,II)/NINC
                FSMFBN1(2,1,II)=(FSMFBN1(2,1,II)-FSMFBN1(1,1,II)*FSMFBN1
     *          (1,1,II))/(NINC-1)
                IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(
     *          2,1,II))
                FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II)/FSMFBN1(1,
     *          1,II))
                FSMFBN1(1,2,II)=FSMFBN1(1,2,II)/NINC
                FSMFBN1(2,2,II)=FSMFBN1(2,2,II)/NINC
                FSMFBN1(2,2,II)=(FSMFBN1(2,2,II)-FSMFBN1(1,2,II)*FSMFBN1
     *          (1,2,II))/(NINC-1)
                IF((FSMFBN1(2,2,II).GT.0.))FSMFBN1(2,2,II)=SQRT(FSMFBN1(
     *          2,2,II))
                FSMFBN1(2,2,II)=MIN(0.9999D00,FSMFBN1(2,2,II)/FSMFBN1(1,
     *          2,II))
                IF ((JUSTONE(II).LT.10)) THEN
                  FSMFBN1(3,1,II)=0.
                ELSE
                  FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/NINC-FSMFBN1(1,1,II)*F
     *            SMFBN1(1,2,II)
                  FSMFBN1(3,1,II)=FSMFBN1(3,1,II)/(FSMFBN1(1,1,II)*FSMFB
     *            N1(1,2,II)*(NINC-1))
                END IF
                FSMFBN1(2,1,II)=FSMFBN1(2,1,II)**2+FSMFBN1(2,2,II)**2-2*
     *          FSMFBN1(3,1,II)
                IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(
     *          2,1,II))
                FSMFBN1(2,1,II)=MIN(0.9999D00,FSMFBN1(2,1,II))
                XPLOT(II)=((SMFMXY(1)-SMFMNY(1))*FLOAT(II)/FLOAT(NFIELD)
     *          ) +SMFMNY(1)
                IF ((FSMFBN1(1,1,II).EQ.0.)) THEN
                  YPLOT(II)=0.
                ELSE
                  YPLOT(II)=FSMFBN1(1,1,II)/FSMFBN1(1,2,II)
                END IF
                IF ((FSMFBN1(1,1,II).EQ.0.).OR.(FSMFBN1(1,2,II).EQ.0.))
     *          THEN
                  ERRYPLOT(II)=0
                ELSE
                  ERRYPLOT(II)=FSMFBN1(2,1,II)*YPLOT(II)
                END IF
3491          CONTINUE
3492          CONTINUE
              HXMIN=SMFMNY(1)
              IF ((ITYPE.EQ.0)) THEN
                DO 3501 II=NFIELD,1,-1
                  IF ((II.EQ.1)) THEN
                    XPLOT(II)=(HXMIN+XPLOT(II))/2.
                  ELSE
                    XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
                  END IF
3501            CONTINUE
3502            CONTINUE
              END IF
              CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTI
     *        TLE,XTITLE, YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
            END IF
          END IF
        ELSE IF((CHOICE1.EQ.6)) THEN
          GRAPHTITLE='angular distribution'
          IF ((ANGTYPE.EQ.1)) THEN
            YTITLE='particles/unit solid angle/incident particle'
          ELSE
            YTITLE='particles/angular bin/incident particle'
          END IF
          XTITLE='angle /degree'
          DO 3511 II=1,NSMFEE
            FSMABN1(II)=FSMABN1(II)+SUM_TMP(II)
            FSMABN2(II)=FSMABN2(II)+SUM_TMP(II)*SUM_TMP(II)
            FSMABN1(II)=FSMABN1(II)/NINC
            FSMABN2(II)=FSMABN2(II)/NINC
            FSMABN2(II)=(FSMABN2(II)-FSMABN1(II)*FSMABN1(II))/(NINC-1)
            IF((FSMABN2(II).GT.0.))FSMABN2(II)=SQRT(FSMABN2(II))
            FSMABN2(II)=MIN(FSMABN2(II),0.9999D00*FSMABN1(II))
            XPLOT(II)=(SMFMXE-SMFMNE)*FLOAT(II)/FLOAT(NSMFEE)+SMFMNE
            IF ((ANGTYPE.EQ.1)) THEN
              IF ((II.EQ.1)) THEN
                DOMEGA=2*3.1415927*SIN((XPLOT(II)+SMFMNE)/2/57.29578)* (
     *          XPLOT(II)-SMFMNE)/57.29578
              ELSE
                DOMEGA=2*3.1415927*SIN((XPLOT(II)+XPLOT(II-1))/2/57.2957
     *          8)* (XPLOT(II)-XPLOT(II-1))/57.29578
              END IF
            ELSE
              DOMEGA=1.
            END IF
            YPLOT(II)=FSMABN1(II)/DOMEGA
            ERRYPLOT(II)=FSMABN2(II)/DOMEGA
3511      CONTINUE
3512      CONTINUE
          HXMIN=SMFMNE
          IF ((ITYPE.EQ.0)) THEN
            DO 3521 II=NSMFEE,1,-1
              IF ((II.EQ.1)) THEN
                XPLOT(II)=(HXMIN+XPLOT(II))/2.
              ELSE
                XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
              END IF
3521        CONTINUE
3522        CONTINUE
          END IF
          CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NSMFEE,MSTART, SERIESTITLE,
     *    XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
        ELSE IF((CHOICE1.EQ.7)) THEN
          GRAPHTITLE='distribution of ZLAST'
          YTITLE='particles/bin(equal)/incident particle'
          XTITLE='Z /cm '
          DO 3531 II=1,NSMFEE
            FSMABN1(II)=FSMABN1(II)+SUM_TMP(II)
            FSMABN2(II)=FSMABN2(II)+SUM_TMP(II)*SUM_TMP(II)
            FSMABN1(II)=FSMABN1(II)/NINC
            FSMABN2(II)=FSMABN2(II)/NINC
            FSMABN2(II)=(FSMABN2(II)-FSMABN1(II)*FSMABN1(II))/(NINC-1)
            IF((FSMABN2(II).GT.0.))FSMABN2(II)=SQRT(FSMABN2(II))
            FSMABN2(II)=MIN(FSMABN2(II),0.9999D00*FSMABN1(II))
            XPLOT(II)=(SMFMXE-SMFMNE)*FLOAT(II)/FLOAT(NSMFEE)+SMFMNE
            YPLOT(II)=FSMABN1(II)
            ERRYPLOT(II)=FSMABN2(II)
3531      CONTINUE
3532      CONTINUE
          HXMIN=SMFMNE
          IF ((ITYPE.EQ.0)) THEN
            DO 3541 II=NSMFEE,1,-1
              IF ((II.EQ.1)) THEN
                XPLOT(II)=(HXMIN+XPLOT(II))/2.
              ELSE
                XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
              END IF
3541        CONTINUE
3542        CONTINUE
          END IF
          CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NSMFEE,MSTART, SERIESTITLE,
     *    XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
        ELSE IF((CHOICE1.EQ.8)) THEN
          GRAPHTITLE='weight distribution'
          YTITLE='particles/bin(equal width)/incident particle'
          XTITLE='weight'
          DO 3551 II=1,NSMFEE
            FSMFBN1(1,1,II)=FSMFBN1(1,1,II)+SUM_TMP(II)
            FSMFBN1(2,1,II)=FSMFBN1(2,1,II)+SUM_TMP(II)*SUM_TMP(II)
            FSMFBN1(1,1,II)=FSMFBN1(1,1,II)/NINC
            FSMFBN1(2,1,II)=FSMFBN1(2,1,II)/NINC
            FSMFBN1(2,1,II)=(FSMFBN1(2,1,II)-FSMFBN1(1,1,II)*FSMFBN1(1,1
     *      ,II))/(NINC-1)
            IF((FSMFBN1(2,1,II).GT.0.))FSMFBN1(2,1,II)=SQRT(FSMFBN1(2,1,
     *      II))
            FSMFBN1(2,1,II)=MIN(FSMFBN1(2,1,II),0.9999D00*FSMFBN1(1,1,II
     *      ))
            XPLOT(II)=EXP((LOG(SMFMXE)-LOG(SMFMNE))*FLOAT(II)/FLOAT(NSMF
     *      EE)+ LOG(SMFMNE))
            YPLOT(II)=FSMFBN1(1,1,II)
            ERRYPLOT(II)=FSMFBN1(2,1,II)
3551      CONTINUE
3552      CONTINUE
          HXMIN=SMFMNE
          IF ((ITYPE.EQ.0)) THEN
            DO 3561 II=NSMFEE,1,-1
              IF ((II.EQ.1)) THEN
                XPLOT(II)=(HXMIN+XPLOT(II))/2.
              ELSE
                XPLOT(II)=(XPLOT(II-1)+XPLOT(II))/2.
              END IF
3561        CONTINUE
3562        CONTINUE
          END IF
          CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NSMFEE,MSTART, SERIESTITLE,
     *    XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,2)
        ELSE IF((CHOICE1.EQ.9)) THEN
          WRITE(3,'(''&'')')
        END IF
        IF ((CHOICE1.EQ.1)) THEN
          WRITE(6,3570)
3570      FORMAT(/' Create another set of fluence vs position data using
     * the same'/ ' ph-sp file?')
        END IF
        IF ((CHOICE1.EQ.2)) THEN
          WRITE(6,3580)
3580      FORMAT(/' Create another set of energy fluence vs position dat
     *a using the'/ ' same ph-sp file?')
        ELSE IF((CHOICE1.EQ.3)) THEN
          WRITE(6,3590)
3590      FORMAT(/' Create another set of spectral data using the same p
     *h-sp file?')
        END IF
        IF ((CHOICE1.EQ.4)) THEN
          WRITE(6,3600)
3600      FORMAT(/' Create another set of energy fluence data using thes
     *ame ph-sp file?')
        ELSE IF((CHOICE1.EQ.5)) THEN
          WRITE(6,3610)
3610      FORMAT(/' Create another set of mean energy data using the sam
     *e ph-sp file?')
        ELSE IF((CHOICE1.EQ.6)) THEN
          WRITE(6,3620)
3620      FORMAT(/' Create another set of angular data using the same ph
     *-sp file?')
        ELSE IF((CHOICE1.EQ.7)) THEN
          WRITE(6,3630)
3630      FORMAT(/' Create another set of zlast data using the same ph-s
     *p file?')
        ELSE IF((CHOICE1.EQ.8)) THEN
          WRITE(6,3640)
3640      FORMAT(/' Create another set of weight data using the same ph-
     *sp file?')
        ELSE IF((CHOICE1.EQ.9)) THEN
          WRITE(6,3650)
3650      FORMAT(/' Create another set of x-y scatter data using the sam
     *e ph-sp file?')
        END IF
        WRITE(6,3660)
3660    FORMAT(' (1: yes, 0: no - quit)'/' ',$)
        READ(5,3670)II
3670    FORMAT(I8)
        WRITE(6,3680)II
3680    FORMAT(I8)
        IF ((II .EQ. 1)) THEN
          MSTART=MSTART+1
          WRITE(6,3690)
3690      FORMAT(' Using same ph-sp file')
          GOTO 1270
        ELSE
          WRITE(6,3700)
3700      FORMAT(/' Plot the data just created using xmgrace before quit
     *ing?')
          WRITE(6,3710)
3710      FORMAT(' (1: yes, 0: no - quit)'/' ',$)
          READ(5,3720)II
3720      FORMAT(I8)
          IF ((II .EQ. 1)) THEN
            WRITE(6,3730)
3730        FORMAT('Plot the data')
            CALL XVGR_SCRIPT
          END IF
          GOTO 1250
        END IF
      END IF
      IF((CHOICE1 .EQ. 10))CALL ADD_FILES
1250  CONTINUE
      WRITE(6,3740)
3740  FORMAT(/' BYE!'/// ' If running gui, this window will close before
     * next run!'/)
      STOP
      END
      SUBROUTINE XVGR_SCRIPT
      IMPLICIT NONE
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      OPEN(4,FILE='xmgrace_script',FORM='FORMATTED',STATUS='UNKNOWN')
      SCRIPT(3)(1:22)='xmgrace -autoscale xy '
      SCRIPT(3)(23:100)= SPCNAM
      SCRIPT(1)='#!/bin/sh'
      SCRIPT(2)='#xmgrace_script'
      SCRIPT(4)='exit'
      WRITE (4,*)SCRIPT(1)(1:60)
      WRITE (4,*)SCRIPT(2)(1:60)
      WRITE (4,*)SCRIPT(3)(1:)
      WRITE (4,*)SCRIPT(4)(1:60)
      CLOSE(4)
      CALL SYSTEM('chmod +x xmgrace_script')
      CALL SYSTEM('xmgrace_script &')
      RETURN
      END
      SUBROUTINE READ_DATA
      IMPLICIT NONE
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      COMMON/DOUBLES/ KWEI,SMFRTN, FSMFBN1,FSMABN1,FSMABN2,FSMFXX,FSMFYY
     *,FSMFR1,FSMFR2,FSMFR11,FSMFR12, FSMFRR,FSMFXY,BINSIZE,RBin,RBin_X,
     *RBin_Y
      REAL*8 KWEI, SMFRTN(22),  FSMFBN1(22,200,200), FSMABN1(200), FSMAB
     *N2(200), FSMFXX(22,200), FSMFYY(22,200), FSMFR1(22,200), FSMFR2(22
     *,200), FSMFR11(22,200), FSMFR12(22,200), FSMFRR(22,200), FSMFXY(22
     *,200,200), BINSIZE(200), RBin,RBin_X,RBin_Y
      COMMON/REALS/ XMIN2,XMAX2, XPLOT,HXMIN, YPLOT,YPLOTT, ERRYPLOT,X2Y
     *2,ZTOTAL,RSSD02,RSSD12,SSDVPS, SFACTOR,RFACTOR,RFACTOR1,RFACTOR2,
     *X,Y,Z,RRFACTOR,SSFACTOR,RSSD0,RSSD1, WEIGHT,EI, PARAEMAX2,PARAEMAX
     *1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAEMNE, PARANINC1,PARANINC2,PARA
     *NINC, EREAD,ZLAST, MUIDX,XIN,YIN,ZIN,UIN,VIN,WIN, SMFX2,SMFY2,SMFM
     *XR, HFIELD, RSCATT, DELTAZ,RSSDST,SSSDST,ERADII,E_ph_sp_e, E_ph_sp
     *_g, E_ph_sp_p, E_MIN_ph_sp, E_MAX_ph_sp, RFIELD,RFIELDe,XFLDMN,XFL
     *DMX, YFLDMN, YFLDMX, SMFMNZ, SMFMXZ, SMFMNX, SMFMXX, SMFMNY, SMFMX
     *Y, SMFMNR,RTREAT,RTREATe,XTREATMN,XTREATMX, YTREATMN,YTREATMX, SMF
     *MNE,SMFMXE, ABNTOTAL,ABNTOTALH,FLUAREA,NINC, RMIN_MIN,RMIN_MAX,RMA
     *X_MIN,RMAX_MAX,DOMEGA, SUM_TMP,SUM_TMP2,Z_SCORE,mumin,mumax
      real*8 XMIN2,XMAX2,  XPLOT(200),HXMIN,  YPLOT(200),YPLOTT, ERRYPLO
     *T(200), X2Y2,  ZTOTAL(22),  RSSD02,RSSD12,  SSDVPS(22+3),  SFACTOR
     *,RFACTOR,RFACTOR1,RFACTOR2,X,Y,Z,RRFACTOR,SSFACTOR, RSSD0,RSSD1,
     *WEIGHT,EI, PARAEMAX2,PARAEMAX1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAE
     *MNE, PARANINC1,PARANINC2,PARANINC, EREAD,ZLAST,MUIDX, XIN,YIN,ZIN,
     *UIN, VIN,WIN,  SMFX2(22), SMFY2(22),  SMFMXR(22),  HFIELD(22),  RS
     *CATT,  DELTAZ,  RSSDST(22,2000), SSSDST(22,2000), ERADII(200),  E_
     *ph_sp_e,  E_ph_sp_g,  E_ph_sp_p,  E_MIN_ph_sp,  E_MAX_ph_sp,  RFIE
     *LD,  RFIELDe,   XFLDMN,  XFLDMX,  YFLDMN,  YFLDMX,  SMFMNZ(22),
     *SMFMXZ(22),  SMFMNX(22),  SMFMXX(22),  SMFMNY(22),  SMFMXY(22),  S
     *MFMNR(22),  RTREAT,  RTREATe,   XTREATMN,  XTREATMX,  YTREATMN,  Y
     *TREATMX,  SMFMNE,SMFMXE,  ABNTOTAL,ABNTOTALH,  FLUAREA,   NINC,  R
     *MIN_MIN, RMIN_MAX,  RMAX_MIN,  RMAX_MAX, DOMEGA,  SUM_TMP(200),  S
     *UM_TMP2(200),  Z_SCORE,  mumin,mumax
      COMMON/INTEGERS/ PARANOT,PARANOT1,PARANOT2,PARANOP,PARANOP1, PARAN
     *OP2,IPARANOT,IPARANOT1,IPARANOT2, LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST, I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBER
     *ST,NBIT1,NBIT2, NFIELDi,NFIELDo,NFIELDe, NWRONG,LATCHJ,CHOICE,CHOI
     *CE1, K1,K2,K3,K4,IA, IX,IY,IR,K,IQ,LATCHI,IE, NPASSI,LATCH,SMFPXY,
     *NFIELD,NSMFPS,I_IN_EX, ISMFPS,IQSMFP,MSTART,MSMFXY,NSMFEE,NPASS_ph
     *_sp, N_ph_sp_e, N_ph_sp_g, N_ph_sp_p, FLUTYPE,ANGTYPE,IZLAST1, IZL
     *AST2,IMUIDX1,IMUIDX2,IZSCORE1,IZSCORE2,JUSTONE, MAXSCATTER,NUMSCAT
     *TER,ESTYPE,i_iaea_in,i_iaea_out,i_log, i_unit_in,i_unit_out
      INTEGER*8 PARANOT,PARANOT1,PARANOT2, PARANOP,PARANOP1,  PARANOP2,I
     *PARANOT,IPARANOT1,IPARANOT2,  LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST(200)
      INTEGER I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBERST,NBIT1,NBI
     *T2,  NWRONG,LATCHJ(29), CHOICE,CHOICE1,  K1,K2,K3,K4,IA, IX,IY,IR,
     *K(22), IQ,LATCHI,IE, NPASSI,  LATCH(22),   SMFPXY(22),   NFIELD,
     * NFIELDi,   NFIELDo,   NFIELDe,  NSMFPS,  I_IN_EX,  ISMFPS(22),
     *IQSMFP(22),   MSTART,   MSMFXY,  NSMFEE,  NPASS_ph_sp,  N_ph_sp_e,
     *  N_ph_sp_g,  N_ph_sp_p,  FLUTYPE,  ANGTYPE,  IZLAST1,  IZLAST2,
     *IMUIDX1,   IMUIDX2,  IZSCORE1,  IZSCORE2,  JUSTONE(200),  MAXSCATT
     *ER,  NUMSCATTER,  ESTYPE ,  i_iaea_in,  i_iaea_out,   i_log,   i_u
     *nit_in,i_unit_out
      COMMON/LOGICALS/OLDFILE,EGSPERT
      LOGICAL OLDFILE,    EGSPERT
      COMMON/RWPHSP/ STRING_TEMP_ZLAST_OUT,  STRING_TEMP_OUT,  IHSTRY_PH
     *SP,   iaea_dummy_long,  NHSTRY_DOS,   ESHORT,  WEIGHTTMP,  WT_PHSP
     *_SHORT,  X_PHSP_SHORT,  Y_PHSP_SHORT,  Z_PHSP_SHORT,  U_PHSP_SHORT
     *,  V_PHSP_SHORT,  W_PHSP_SHORT,  ZLAST_PHSP_SHORT,  MUIDX_PHSP_SHO
     *RT,  EKMAX_PHSP_SHORT,  EKMINE_PHSP_SHORT,  NINC_PHSP_SHORT,  dosx
     *yz2beam_index,  iaea_extra_floats,  NUM_PHSP_TOT,  PHOT_PHSP_TOT,
     *iaea_result,   iaea_n_stat,  iaea_q_typ,  iaea_typ_q,  iaea_q_inde
     *x,  I_PHSP,  IERR_PHSP,  LATCHTMP,  WRITE_PHSP_COUNTER,  WRITE_PHS
     *P_SOFAR,  PHSP_RESTART,  PHSP_RECL,  PHSP_RECL_OLD,  iaea_extra_in
     *ts,  iaea_extra_int_types,iaea_extra_float_types,   iaea_i_zlast,
     * iaea_i_latch,  iaea_i_muidx,  iaea_n_extra_ints,  iaea_n_extra_fl
     *oats,  i_iaea_open_for_write,   iaea_i_zlast_out,  iaea_i_latch_ou
     *t,  iaea_i_muidx_out,  dosxyz2beam_izscore,  MODE_RW
      CHARACTER*32000 STRING_TEMP_ZLAST_OUT(1)
      CHARACTER*28000 STRING_TEMP_OUT(1)
      INTEGER*8 IHSTRY_PHSP(1),iaea_dummy_long,NHSTRY_DOS
      REAL*4 ESHORT,WEIGHTTMP,WT_PHSP_SHORT,X_PHSP_SHORT,Y_PHSP_SHORT, Z
     *_PHSP_SHORT,U_PHSP_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,ZLAST_PHSP_SHO
     *RT, MUIDX_PHSP_SHORT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP
     *_SHORT,dosxyz2beam_index, iaea_extra_floats(10)
      INTEGER NUM_PHSP_TOT,PHOT_PHSP_TOT, iaea_result,iaea_n_stat,iaea_q
     *_typ(3),iaea_typ_q(3),iaea_q_index,I_PHSP, IERR_PHSP,LATCHTMP, WRI
     *TE_PHSP_COUNTER(1),WRITE_PHSP_SOFAR(1), PHSP_RESTART,PHSP_RECL,PHS
     *P_RECL_OLD,iaea_extra_ints(10), iaea_extra_int_types(10),iaea_extr
     *a_float_types(10),iaea_i_zlast, iaea_i_latch,iaea_i_muidx,iaea_n_e
     *xtra_ints,iaea_n_extra_floats, i_iaea_open_for_write,iaea_i_zlast_
     *out,iaea_i_latch_out, iaea_i_muidx_out,dosxyz2beam_izscore
      CHARACTER*5 MODE_RW
      INTEGER LATCH_TEMP_OUT,NUM_PHSP_TOFLUSH,TEMP_PHSP_COUNTER,REM_PHSP
      REAL*4 REAL_TEMP_OUT(7)
      CHARACTER*32 REC_TEMP_OUT
      integer lnblnk1
      i_log=6
      DO 5011 III=1,200
        XPLOT(III)=0.0
        YPLOT(III)=0.0
        ERRYPLOT(III)=0.0
        JUSTONE(III)=0
        NHSTRY_LAST(III)=0.
        SUM_TMP(III)=0.
        SUM_TMP2(III)=0.
5011  CONTINUE
5012  CONTINUE
      DO 5021 III=1,200
        FSMABN1(III)=0.0
        FSMABN2(III)=0.0
5021  CONTINUE
5022  CONTINUE
      DO 5031 III=1,22
        DO 5041 IIII=1,200
          FSMFRR(III,IIII)=0.0
          FSMFXX(III,IIII)=0.0
          FSMFYY(III,IIII)=0.0
          DO 5051 IIIII=1,200
            FSMFXY(III,IIII,IIIII)=0.0
5051      CONTINUE
5052      CONTINUE
5041    CONTINUE
5042    CONTINUE
        DO 5061 IIII=1,200
          DO 5071 IIIII=1,200
            FSMFBN1(III,IIIII,IIII)=0.0
5071      CONTINUE
5072      CONTINUE
5061    CONTINUE
5062    CONTINUE
5031  CONTINUE
5032  CONTINUE
      IF (((SMFMXE-SMFMNE) .NE. 0.)) THEN
        SFACTOR=FLOAT(NSMFEE)/(SMFMXE-SMFMNE)
        SSFACTOR=FLOAT(NSMFEE)/(SMFMXE-SMFMNE)
      ELSE
        SFACTOR=FLOAT(NSMFEE)
        SSFACTOR=FLOAT(NSMFEE)
      END IF
      IF ((RFIELD .NE. 0.)) THEN
        RFACTOR=FLOAT(NFIELD)/(RFIELD**2)
      ELSE
        RFACTOR=FLOAT(NFIELD)
      END IF
      IF (((SMFMXY(1)-SMFMNY(1)) .NE. 0.)) THEN
        Y=FLOAT(NFIELD)/(SMFMXY(1)-SMFMNY(1))
      END IF
      IF (((SMFMXX(1)-SMFMNX(1)) .NE. 0.)) THEN
        X=FLOAT(NFIELD)/(SMFMXX(1)-SMFMNX(1))
      ELSE
        X=FLOAT(NFIELD)
      END IF
      XMIN2=SMFMNX(1)*SMFMNX(1)
      XMAX2=SMFMXX(1)*SMFMXX(1)
5080  CONTINUE
      IPARANOT=1
      NUMSCATTER=0
      IIII=0
      NHSTRY=0
5091  CONTINUE
5100    CONTINUE
        IPARANOT=IPARANOT+1
        IF ((IPARANOT .EQ. PARANOT+2)) THEN
          GOTO 5110
        END IF
        IF ((i_iaea_in.EQ.1)) THEN
          NPASSI=0
          call iaea_get_particle(i_unit_in,iaea_n_stat,iaea_q_index,ESHO
     *    RT,WT_PHSP_SHORT, X_PHSP_SHORT,Y_PHSP_SHORT,Z_PHSP_SHORT,U_PHS
     *    P_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,iaea_extra_floats,iaea_extr
     *    a_ints)
          IF ((iaea_n_stat.EQ.-1)) THEN
            WRITE(i_log,*)' Error getting particle data from IAEA phsp f
     *ile.'
            call exit(1)
          ELSE IF((iaea_n_stat.EQ.-2)) THEN
            WRITE(i_log,*)' WARNING: Reached end of IAEA phsp file.  Fil
     *e restarted.'
          ELSE IF((iaea_n_stat.GE.0)) THEN
            NHSTRY=NHSTRY+iaea_n_stat
            IF ((iaea_i_latch.EQ.-99)) THEN
              LATCHI=0
            ELSE
              LATCHI=iaea_extra_ints(iaea_i_latch)
            END IF
            IQ=iaea_typ_q(iaea_q_index)
            IF (( (IQ.EQ.1) .OR. (IQ.EQ.-1) )) THEN
              ESHORT = ESHORT + 0.5109989461
            END IF
            EI=ESHORT
            IF ((iaea_i_zlast.EQ.-99)) THEN
              ZLAST_PHSP_SHORT=0
            ELSE
              ZLAST_PHSP_SHORT= iaea_extra_floats(iaea_i_zlast)
            END IF
            IF ((iaea_i_muidx.EQ.-99)) THEN
              MUIDX_PHSP_SHORT=0
            ELSE
              MUIDX_PHSP_SHORT= iaea_extra_floats(iaea_i_muidx)
            END IF
            WEIGHT=WT_PHSP_SHORT
            XIN=X_PHSP_SHORT
            YIN=Y_PHSP_SHORT
            ZIN=Z_PHSP_SHORT
            UIN=U_PHSP_SHORT
            VIN=V_PHSP_SHORT
            WIN=W_PHSP_SHORT
            ZLAST=ZLAST_PHSP_SHORT
            MUIDX=MUIDX_PHSP_SHORT
          END IF
        ELSE
          IZLAST1=0
          IF((MODE_RW.EQ.'MODE2'))IZLAST1=1
          IF ((IZLAST1.NE.0)) THEN
            READ(2,REC=IPARANOT,IOSTAT=IERR_PHSP) LATCHI,ESHORT,X_PHSP_S
     *      HORT,Y_PHSP_SHORT, U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT,Z
     *      LAST_PHSP_SHORT
          ELSE
            READ(2,REC=IPARANOT,IOSTAT=IERR_PHSP) LATCHI,ESHORT,X_PHSP_S
     *      HORT,Y_PHSP_SHORT, U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT
          END IF
          IF ((IPARANOT.EQ.76695869)) THEN
            write(*,*)' x,y,u,v,wt,eshort,latch', X_PHSP_SHORT,Y_PHSP_SH
     *      ORT,U_PHSP_SHORT,V_PHSP_SHORT, WT_PHSP_SHORT,ESHORT,LATCHI
          END IF
          IF ((ESHORT.LT.0.0)) THEN
            NHSTRY=NHSTRY+1
            ESHORT=ABS(ESHORT)
          END IF
          EI=ESHORT
          WEIGHT=WT_PHSP_SHORT
          XIN=X_PHSP_SHORT
          YIN=Y_PHSP_SHORT
          UIN=U_PHSP_SHORT
          VIN=V_PHSP_SHORT
          IF((IZLAST1.NE.0))ZLAST=ZLAST_PHSP_SHORT
          IF ((IERR_PHSP.EQ.0)) THEN
            EI=ESHORT
            IF ((BTEST(LATCHI, 31))) THEN
              NPASSI = 1
              LATCHI = IBCLR(LATCHI,31)
            ELSE
              NPASSI = 0
            END IF
            IF ((BTEST(LATCHI, 30))) THEN
              IQ = -1
              LATCHI = IBCLR(LATCHI,30)
              LATCHI = IBCLR(LATCHI,29)
            ELSE
              IF ((BTEST(LATCHI, 29))) THEN
                IQ = 1
                LATCHI = IBCLR(LATCHI,29)
              ELSE
                IQ = 0
              END IF
            END IF
            WIN = min( 1., UIN**2 + VIN**2)
            WIN = sqrt(1. - WIN)
            WIN = SIGN(WIN,WEIGHT)
            WEIGHT=ABS(WEIGHT)
          END IF
        END IF
        IF ((NPASSI .NE. 0)) THEN
          NPASS_ph_sp = NPASS_ph_sp + 1
        END IF
        IF ((IQ .NE. 0)) THEN
          EI=EI-0.5109989461
        END IF
        IF ((IQSMFP(1) .NE. 2)) THEN
          IF ((IQSMFP(1) .EQ.3)) THEN
            IF ((IQ .EQ.0)) THEN
              GOTO 5100
            END IF
          ELSE IF((IQSMFP(1) .NE. IQ)) THEN
            GOTO 5100
          END IF
        END IF
        IF ((CHOICE1.EQ.11)) THEN
          DO 5121 I=1,29
            IF ((BTEST(LATCHI, I-1))) THEN
              LATCHJ(30-I)=1
            ELSE
              LATCHJ(30-I)=0
            END IF
5121      CONTINUE
5122      CONTINUE
          IIII=IIII+1
          IF ((IIII.GE.NUMBERST)) THEN
            IF ((MODE_RW.EQ.'MODE0')) THEN
              WRITE(6,5130) EI,IQ,XIN,YIN,UIN,VIN,WIN,WEIGHT,(LATCHJ(I),
     *        I=1,29)
5130          FORMAT(F7.3,I4,2F8.3,3F7.3,1PE10.3, 2X,5I1,1X,24I1)
            ELSE IF((MODE_RW.EQ.'MODE2')) THEN
              WRITE(6,5140) EI,IQ,XIN,YIN,UIN,VIN,WIN,ZLAST,WEIGHT,(LATC
     *        HJ(I),I=1,29)
5140          FORMAT(F7.3,I4,2F8.3,3F7.3,F8.3,1PE10.3, 2X,5I1,1X,24I1)
            END IF
          END IF
          IF((NUMBERP .EQ. IIII))RETURN
          IF ((IIII.EQ.1)) THEN
            IF ((MODE_RW.EQ.'MODE0')) THEN
              WRITE(6,5150)
            ELSE IF((MODE_RW.EQ.'MODE2')) THEN
              WRITE(6,5160)
            END IF
          END IF
        ELSE IF((CHOICE1.EQ.12)) THEN
          IF ((IMUIDX1.EQ.0 .OR. (MUIDX.GE.mumin .AND. MUIDX.LE.mumax)))
     *     THEN
            IIII=IIII+1
            WRITE(3,'(F8.3,'','',F8.3,'','',F8.3)')XIN,YIN,ZIN
          END IF
          IF((NUMBERP .EQ. IIII))RETURN
        END IF
5150    FORMAT(/' ENERGY  IQ     X', 5X, '  Y',4X,'  U',4X,'  V',4X,'  W
     *',4X,'WEIGHT',6X, ' LATCH (set=1, not set=0)'/)
5160    FORMAT(/' ENERGY  IQ     X', 5X, '  Y',4X,'  U',4X,'  V',4X,'  W
     *',4X,' ZLAST   WEIGHT',6X, ' LATCH (set=1, not set=0)'/)
        IF ((I_IN_EX .EQ. 0)) THEN
          DO 5171 I=1,NBIT1
            III=LATCH(I)
            IF((BTEST(LATCHI, III)))GO TO5172
            IF((I.EQ.NBIT1))GOTO 5100
5171      CONTINUE
5172      CONTINUE
          DO 5181 I=1,NBIT2
            III=LATCH(I+NBIT1)
            IF((BTEST(LATCHI, III)))GOTO 5100
5181      CONTINUE
5182      CONTINUE
          GOTO 5190
        ELSE IF((I_IN_EX .EQ. 1)) THEN
          DO 5201 I=1,NBIT1
            III=LATCH(I)
            IF((BTEST(LATCHI, III)))GOTO 5100
5201      CONTINUE
5202      CONTINUE
        ELSE IF((I_IN_EX .EQ. 2)) THEN
          DO 5211 I=1,NBIT1
            III=LATCH(I)
            IF((IBITS(LATCHI,24,5).EQ.III))GOTO 5190
5211      CONTINUE
5212      CONTINUE
          GOTO 5100
        ELSE IF((I_IN_EX .EQ. 3)) THEN
          DO 5221 I=1,NBIT1
            III=LATCH(I)
            IF((IBITS(LATCHI,24,5).EQ.III))GOTO 5100
5221      CONTINUE
5222      CONTINUE
        END IF
5190    CONTINUE
        X2Y2=XIN*XIN+YIN*YIN
        IF ((CHOICE1 .EQ. 1)) THEN
          IF ((FLUTYPE.NE.1)) THEN
            WEIGHT=WEIGHT/MAX(0.08716,ABS(WIN))
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            IR=INT(X2Y2*RFACTOR)+1
            IF ((IR .GT. NFIELD)) THEN
              GOTO 5100
            END IF
            IF ((NHSTRY_LAST(IR).EQ.NHSTRY)) THEN
              SUM_TMP(IR)=SUM_TMP(IR)+WEIGHT
            ELSE
              FSMFRR(1,IR)=FSMFRR(1,IR)+SUM_TMP(IR)
              FSMFRR(2,IR)=FSMFRR(2,IR)+SUM_TMP(IR)*SUM_TMP(IR)
              SUM_TMP(IR)=WEIGHT
              NHSTRY_LAST(IR)=NHSTRY
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IX=INT(XIN*XIN*RFACTOR)+1
            IY=INT(YIN*YIN*RFACTOR)+1
            IF (((IX .GT. NFIELD) .OR. (IY .GT. NFIELD))) THEN
              GOTO 5100
            END IF
            IF ((IX .LE. IY)) THEN
              IF ((NHSTRY_LAST(IY).EQ.NHSTRY)) THEN
                SUM_TMP(IY)=SUM_TMP(IY)+WEIGHT
              ELSE
                FSMFRR(1,IY)=FSMFRR(1,IY)+SUM_TMP(IY)
                FSMFRR(2,IY)=FSMFRR(2,IY)+SUM_TMP(IY)*SUM_TMP(IY)
                SUM_TMP(IY)=WEIGHT
                NHSTRY_LAST(IY)=NHSTRY
              END IF
            ELSE
              IF ((NHSTRY_LAST(IX).EQ.NHSTRY)) THEN
                SUM_TMP(IX)=SUM_TMP(IX)+WEIGHT
              ELSE
                FSMFRR(1,IX)=FSMFRR(1,IX)+SUM_TMP(IX)
                FSMFRR(2,IX)=FSMFRR(2,IX)+SUM_TMP(IX)*SUM_TMP(IX)
                SUM_TMP(IX)=WEIGHT
                NHSTRY_LAST(IX)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.2)) THEN
            IF ((ISMFPS(1).EQ.0)) THEN
              IX=INT((XIN-SMFMNX(1))*X)+1
              IF ((IX.GT.NFIELD).OR.(IX.LT.1).OR.((XIN-SMFMNX(1)).LT.0.
     *        .OR. YIN.GE.SMFMXY(1) .OR. YIN.LE.SMFMNY(1))) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IX).EQ.NHSTRY)) THEN
                SUM_TMP(IX)=SUM_TMP(IX)+WEIGHT
              ELSE
                FSMFXX(1,IX)=FSMFXX(1,IX)+SUM_TMP(IX)
                FSMFXX(2,IX)=FSMFXX(2,IX)+SUM_TMP(IX)*SUM_TMP(IX)
                SUM_TMP(IX)=WEIGHT
                NHSTRY_LAST(IX)=NHSTRY
              END IF
            ELSE
              IY=INT((YIN-SMFMNY(1))*Y)+1
              IF ((IY.GT.NFIELD).OR.(IY.LT.1).OR.((YIN-SMFMNY(1)).LT.0.
     *        .OR. XIN.GE.SMFMXX(1) .OR. XIN.LE.SMFMNX(1))) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IY).EQ.NHSTRY)) THEN
                SUM_TMP(IY)=SUM_TMP(IY)+WEIGHT
              ELSE
                FSMFYY(1,IY)=FSMFYY(1,IY)+SUM_TMP(IY)
                FSMFYY(2,IY)=FSMFYY(2,IY)+SUM_TMP(IY)*SUM_TMP(IY)
                SUM_TMP(IY)=WEIGHT
                NHSTRY_LAST(IY)=NHSTRY
              END IF
            END IF
          END IF
        END IF
        IF ((CHOICE1 .EQ. 2)) THEN
          IF ((FLUTYPE.NE.1)) THEN
            WEIGHT=WEIGHT/MAX(0.08716,ABS(WIN))
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            IR=INT(X2Y2*RFACTOR)+1
            IF ((IR .GT. NFIELD)) THEN
              GOTO 5100
            END IF
            IF ((NHSTRY_LAST(IR).EQ.NHSTRY)) THEN
              SUM_TMP(IR)=SUM_TMP(IR)+WEIGHT*EI
            ELSE
              FSMFRR(1,IR)=FSMFRR(1,IR)+SUM_TMP(IR)
              FSMFRR(2,IR)=FSMFRR(2,IR)+SUM_TMP(IR)*SUM_TMP(IR)
              SUM_TMP(IR)=WEIGHT*EI
              NHSTRY_LAST(IR)=NHSTRY
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IX=INT(XIN*XIN*RFACTOR)+1
            IY=INT(YIN*YIN*RFACTOR)+1
            IF (((IX .GT. NFIELD) .OR. (IY .GT. NFIELD))) THEN
              GOTO 5100
            END IF
            IF ((IX .LE. IY)) THEN
              IF ((NHSTRY_LAST(IY).EQ.NHSTRY)) THEN
                SUM_TMP(IY)=SUM_TMP(IY)+WEIGHT*EI
              ELSE
                FSMFRR(1,IY)=FSMFRR(1,IY)+SUM_TMP(IY)
                FSMFRR(2,IY)=FSMFRR(2,IY)+SUM_TMP(IY)*SUM_TMP(IY)
                SUM_TMP(IY)=WEIGHT*EI
                NHSTRY_LAST(IY)=NHSTRY
              END IF
            ELSE
              IF ((NHSTRY_LAST(IX).EQ.NHSTRY)) THEN
                SUM_TMP(IX)=SUM_TMP(IX)+WEIGHT*EI
              ELSE
                FSMFRR(1,IX)=FSMFRR(1,IX)+SUM_TMP(IX)
                FSMFRR(2,IX)=FSMFRR(2,IX)+SUM_TMP(IX)*SUM_TMP(IX)
                SUM_TMP(IX)=WEIGHT*EI
                NHSTRY_LAST(IX)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.2)) THEN
            IF ((ISMFPS(1).EQ.0)) THEN
              IX=INT((XIN-SMFMNX(1))*X)+1
              IF ((IX.GT.NFIELD).OR.(IX.LT.1).OR.((XIN-SMFMNX(1)).LT.0.
     *        .OR. YIN.GE.SMFMXY(1) .OR. YIN.LE.SMFMNY(1))) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IX).EQ.NHSTRY)) THEN
                SUM_TMP(IX)=SUM_TMP(IX)+WEIGHT*EI
              ELSE
                FSMFXX(1,IX)=FSMFXX(1,IX)+SUM_TMP(IX)
                FSMFXX(2,IX)=FSMFXX(2,IX)+SUM_TMP(IX)*SUM_TMP(IX)
                SUM_TMP(IX)=WEIGHT*EI
                NHSTRY_LAST(IX)=NHSTRY
              END IF
            ELSE
              IY=INT((YIN-SMFMNY(1))*Y)+1
              IF ((IY.GT.NFIELD).OR.(IY.LT.1).OR.((YIN-SMFMNY(1)).LT.0.
     *        .OR. XIN.GE.SMFMXX(1) .OR. XIN.LE.SMFMNX(1))) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IY).EQ.NHSTRY)) THEN
                SUM_TMP(IY)=SUM_TMP(IY)+WEIGHT*EI
              ELSE
                FSMFYY(1,IY)=FSMFYY(1,IY)+SUM_TMP(IY)
                FSMFYY(2,IY)=FSMFYY(2,IY)+SUM_TMP(IY)*SUM_TMP(IY)
                SUM_TMP(IY)=WEIGHT*EI
                NHSTRY_LAST(IY)=NHSTRY
              END IF
            END IF
          END IF
        ELSE IF((CHOICE1 .EQ. 3)) THEN
          IF ((FLUTYPE.NE.1)) THEN
            WEIGHT=WEIGHT/MAX(0.08716,ABS(WIN))
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            IF ((X2Y2.GE.XMIN2).AND.(X2Y2.LT.XMAX2)) THEN
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IE=INT((EI-SMFMNE)*SFACTOR)+1
              IF ((IE .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IE).EQ.NHSTRY)) THEN
                SUM_TMP(IE)=SUM_TMP(IE)+WEIGHT
              ELSE
                FSMFBN1(1,1,IE)=FSMFBN1(1,1,IE)+SUM_TMP(IE)
                FSMFBN1(2,1,IE)=FSMFBN1(2,1,IE)+SUM_TMP(IE)*SUM_TMP(IE)
                SUM_TMP(IE)=WEIGHT
                NHSTRY_LAST(IE)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IF ((YIN.GT.SMFMNY(1)).AND.(YIN.LT.SMFMXY(1)).AND.(XIN.GT.SM
     *      FMNX(1)) .AND.(XIN.LT.SMFMXX(1))) THEN
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IE=INT((EI-SMFMNE)*SFACTOR)+1
              IF ((IE .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IE).EQ.NHSTRY)) THEN
                SUM_TMP(IE)=SUM_TMP(IE)+WEIGHT
              ELSE
                FSMFBN1(1,1,IE)=FSMFBN1(1,1,IE)+SUM_TMP(IE)
                FSMFBN1(2,1,IE)=FSMFBN1(2,1,IE)+SUM_TMP(IE)*SUM_TMP(IE)
                SUM_TMP(IE)=WEIGHT
                NHSTRY_LAST(IE)=NHSTRY
              END IF
            END IF
          END IF
        ELSE IF((CHOICE1 .EQ. 4)) THEN
          IF ((FLUTYPE.NE.1)) THEN
            WEIGHT=WEIGHT/MAX(0.08716,ABS(WIN))
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            IF ((X2Y2.GE.XMIN2).AND.(X2Y2.LT.XMAX2)) THEN
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IE=INT((EI-SMFMNE)*SFACTOR)+1
              IF ((IE .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IE).EQ.NHSTRY)) THEN
                SUM_TMP(IE)=SUM_TMP(IE)+WEIGHT*EI
              ELSE
                FSMFBN1(1,1,IE)=FSMFBN1(1,1,IE)+SUM_TMP(IE)
                FSMFBN1(2,1,IE)=FSMFBN1(2,1,IE)+SUM_TMP(IE)*SUM_TMP(IE)
                SUM_TMP(IE)=WEIGHT*EI
                NHSTRY_LAST(IE)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IF ((YIN.GT.SMFMNY(1)).AND.(YIN.LT.SMFMXY(1)).AND.(XIN.GT.SM
     *      FMNX(1)) .AND.(XIN.LT.SMFMXX(1))) THEN
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IE=INT((EI-SMFMNE)*SFACTOR)+1
              IF ((IE .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IE).EQ.NHSTRY)) THEN
                SUM_TMP(IE)=SUM_TMP(IE)+WEIGHT*EI
              ELSE
                FSMFBN1(1,1,IE)=FSMFBN1(1,1,IE)+SUM_TMP(IE)
                FSMFBN1(2,1,IE)=FSMFBN1(2,1,IE)+SUM_TMP(IE)*SUM_TMP(IE)
                SUM_TMP(IE)=WEIGHT*EI
                NHSTRY_LAST(IE)=NHSTRY
              END IF
            END IF
          END IF
        ELSE IF((CHOICE1 .EQ. 5)) THEN
          WEIGHT=WEIGHT/MAX(0.08716,ABS(WIN))
          IF ((MSMFXY.EQ.0)) THEN
            IR=INT(X2Y2*RFACTOR)+1
            IF ((IR .GT. NFIELD)) THEN
              GOTO 5100
            END IF
            JUSTONE(IR)=JUSTONE(IR)+1
            IF ((NHSTRY_LAST(IR).EQ.NHSTRY)) THEN
              SUM_TMP(IR)=SUM_TMP(IR)+WEIGHT*EI
              SUM_TMP2(IR)=SUM_TMP2(IR)+WEIGHT
            ELSE
              FSMFBN1(1,1,IR)=FSMFBN1(1,1,IR)+SUM_TMP(IR)
              FSMFBN1(2,1,IR)=FSMFBN1(2,1,IR)+SUM_TMP(IR)*SUM_TMP(IR)
              FSMFBN1(1,2,IR)=FSMFBN1(1,2,IR)+SUM_TMP2(IR)
              FSMFBN1(2,2,IR)=FSMFBN1(2,2,IR)+SUM_TMP2(IR)*SUM_TMP2(IR)
              FSMFBN1(3,1,IR)=FSMFBN1(3,1,IR)+SUM_TMP(IR)*SUM_TMP2(IR)
              SUM_TMP(IR)=WEIGHT*EI
              SUM_TMP2(IR)=WEIGHT
              NHSTRY_LAST(IR)=NHSTRY
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IX=INT(XIN*XIN*RFACTOR)+1
            IY=INT(YIN*YIN*RFACTOR)+1
            IF (((IX .GT. NFIELD) .OR. (IY .GT. NFIELD))) THEN
              GOTO 5100
            END IF
            IF ((IX .LE. IY)) THEN
              JUSTONE(IY)=JUSTONE(IY)+1
              IF ((NHSTRY_LAST(IY).EQ.NHSTRY)) THEN
                SUM_TMP(IY)=SUM_TMP(IY)+WEIGHT*EI
                SUM_TMP2(IY)=SUM_TMP2(IY)+WEIGHT
              ELSE
                FSMFBN1(1,1,IY)=FSMFBN1(1,1,IY)+SUM_TMP(IY)
                FSMFBN1(2,1,IY)=FSMFBN1(2,1,IY)+SUM_TMP(IY)*SUM_TMP(IY)
                FSMFBN1(1,2,IY)=FSMFBN1(1,2,IY)+SUM_TMP2(IY)
                FSMFBN1(2,2,IY)=FSMFBN1(2,2,IY)+SUM_TMP2(IY)*SUM_TMP2(IY
     *          )
                FSMFBN1(3,1,IY)=FSMFBN1(3,1,IY)+SUM_TMP(IY)*SUM_TMP2(IY)
                SUM_TMP(IY)=WEIGHT*EI
                SUM_TMP2(IY)=WEIGHT
                NHSTRY_LAST(IY)=NHSTRY
              END IF
            ELSE
              JUSTONE(IX)=JUSTONE(IX)+1
              IF ((NHSTRY_LAST(IX).EQ.NHSTRY)) THEN
                SUM_TMP(IX)=SUM_TMP(IX)+WEIGHT*EI
                SUM_TMP2(IX)=SUM_TMP2(IX)+WEIGHT
              ELSE
                FSMFBN1(1,1,IX)=FSMFBN1(1,1,IX)+SUM_TMP(IX)
                FSMFBN1(2,1,IX)=FSMFBN1(2,1,IX)+SUM_TMP(IX)*SUM_TMP(IX)
                FSMFBN1(1,2,IX)=FSMFBN1(1,2,IX)+SUM_TMP2(IX)
                FSMFBN1(2,2,IX)=FSMFBN1(2,2,IX)+SUM_TMP2(IX)*SUM_TMP2(IX
     *          )
                FSMFBN1(3,1,IX)=FSMFBN1(3,1,IX)+SUM_TMP(IX)*SUM_TMP2(IX)
                SUM_TMP(IX)=WEIGHT*EI
                SUM_TMP2(IX)=WEIGHT
                NHSTRY_LAST(IX)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.2)) THEN
            IF ((ISMFPS(1).EQ.0)) THEN
              IX=INT((XIN-SMFMNX(1))*X)+1
              IF ((IX.GT.NFIELD).OR.(IX.LT.1).OR.((XIN-SMFMNX(1)).LT.0.
     *        .OR. YIN.GE.SMFMXY(1) .OR. YIN.LE.SMFMNY(1))) THEN
                GOTO 5100
              END IF
              JUSTONE(IX)=JUSTONE(IX)+1
              IF ((NHSTRY_LAST(IX).EQ.NHSTRY)) THEN
                SUM_TMP(IX)=SUM_TMP(IX)+WEIGHT*EI
                SUM_TMP2(IX)=SUM_TMP2(IX)+WEIGHT
              ELSE
                FSMFBN1(1,1,IX)=FSMFBN1(1,1,IX)+SUM_TMP(IX)
                FSMFBN1(2,1,IX)=FSMFBN1(2,1,IX)+SUM_TMP(IX)*SUM_TMP(IX)
                FSMFBN1(1,2,IX)=FSMFBN1(1,2,IX)+SUM_TMP2(IX)
                FSMFBN1(2,2,IX)=FSMFBN1(2,2,IX)+SUM_TMP2(IX)*SUM_TMP2(IX
     *          )
                FSMFBN1(3,1,IX)=FSMFBN1(3,1,IX)+SUM_TMP(IX)*SUM_TMP2(IX)
                SUM_TMP(IX)=WEIGHT*EI
                SUM_TMP2(IX)=WEIGHT
                NHSTRY_LAST(IX)=NHSTRY
              END IF
            ELSE
              IY=INT((YIN-SMFMNY(1))*Y)+1
              IF ((IY.GT.NFIELD).OR.(IY.LT.1).OR.((YIN-SMFMNY(1)).LT.0.
     *        .OR. XIN.GE.SMFMXX(1) .OR. XIN.LE.SMFMNX(1))) THEN
                GOTO 5100
              END IF
              JUSTONE(IY)=JUSTONE(IY)+1
              IF ((NHSTRY_LAST(IY).EQ.NHSTRY)) THEN
                SUM_TMP(IY)=SUM_TMP(IY)+WEIGHT*EI
                SUM_TMP2(IY)=SUM_TMP2(IY)+WEIGHT
              ELSE
                FSMFBN1(1,1,IY)=FSMFBN1(1,1,IY)+SUM_TMP(IY)
                FSMFBN1(2,1,IY)=FSMFBN1(2,1,IY)+SUM_TMP(IY)*SUM_TMP(IY)
                FSMFBN1(1,2,IY)=FSMFBN1(1,2,IY)+SUM_TMP2(IY)
                FSMFBN1(2,2,IY)=FSMFBN1(2,2,IY)+SUM_TMP2(IY)*SUM_TMP2(IY
     *          )
                FSMFBN1(3,1,IY)=FSMFBN1(3,1,IY)+SUM_TMP(IY)*SUM_TMP2(IY)
                SUM_TMP(IY)=WEIGHT*EI
                SUM_TMP2(IY)=WEIGHT
                NHSTRY_LAST(IY)=NHSTRY
              END IF
            END IF
          END IF
        ELSE IF((CHOICE1 .EQ. 6)) THEN
          IF ((EI.LT.SMFMNZ(1))) THEN
            GOTO 5100
          END IF
          IF ((EI.GT.SMFMXZ(1))) THEN
            GOTO 5100
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            IF ((X2Y2.GE.XMIN2).AND.(X2Y2.LT.XMAX2)) THEN
              EI=ACOS(WIN)*57.29578
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IA=INT((EI-SMFMNE)*SSFACTOR)+1
              IF ((IA.GT.NSMFEE).OR.(IA.LT.1)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IA).EQ.NHSTRY)) THEN
                SUM_TMP(IA)=SUM_TMP(IA)+WEIGHT
              ELSE
                FSMABN1(IA)=FSMABN1(IA)+SUM_TMP(IA)
                FSMABN2(IA)=FSMABN2(IA)+SUM_TMP(IA)*SUM_TMP(IA)
                SUM_TMP(IA)=WEIGHT
                NHSTRY_LAST(IA)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IF ((YIN.GT.SMFMNY(1)).AND.(YIN.LT.SMFMXY(1)).AND.(XIN.GT.SM
     *      FMNX(1)) .AND.(XIN.LT.SMFMXX(1))) THEN
              EI=ACOS(WIN)*57.29578
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IA=INT((EI-SMFMNE)*SSFACTOR)+1
              IF ((IA .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IA).EQ.NHSTRY)) THEN
                SUM_TMP(IA)=SUM_TMP(IA)+WEIGHT
              ELSE
                FSMABN1(IA)=FSMABN1(IA)+SUM_TMP(IA)
                FSMABN2(IA)=FSMABN2(IA)+SUM_TMP(IA)*SUM_TMP(IA)
                SUM_TMP(IA)=WEIGHT
                NHSTRY_LAST(IA)=NHSTRY
              END IF
            END IF
          END IF
        ELSE IF((CHOICE1 .EQ. 7)) THEN
          IF ((MSMFXY.EQ.0)) THEN
            IF ((X2Y2.GE.XMIN2).AND.(X2Y2.LT.XMAX2)) THEN
              IF ((ZLAST.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IA=INT((ZLAST-SMFMNE)*SSFACTOR)+1
              IF ((IA .GT. NSMFEE).OR.(IA .LT. 1)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IA).EQ.NHSTRY)) THEN
                SUM_TMP(IA)=SUM_TMP(IA)+WEIGHT
              ELSE
                FSMABN1(IA)=FSMABN1(IA)+SUM_TMP(IA)
                FSMABN2(IA)=FSMABN2(IA)+SUM_TMP(IA)*SUM_TMP(IA)
                SUM_TMP(IA)=WEIGHT
                NHSTRY_LAST(IA)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IF ((YIN.GT.SMFMNY(1)).AND.(YIN.LT.SMFMXY(1)).AND.(XIN.GT.SM
     *      FMNX(1)) .AND.(XIN.LT.SMFMXX(1))) THEN
              IF ((ZLAST.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IA=INT((ZLAST-SMFMNE)*SSFACTOR)+1
              IF ((IA .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IA).EQ.NHSTRY)) THEN
                SUM_TMP(IA)=SUM_TMP(IA)+WEIGHT
              ELSE
                FSMABN1(IA)=FSMABN1(IA)+SUM_TMP(IA)
                FSMABN2(IA)=FSMABN2(IA)+SUM_TMP(IA)*SUM_TMP(IA)
                SUM_TMP(IA)=WEIGHT
                NHSTRY_LAST(IA)=NHSTRY
              END IF
            END IF
          END IF
        ELSE IF((CHOICE1 .EQ. 8)) THEN
          IF ((SMFMXE.NE.SMFMNE)) THEN
            SFACTOR=FLOAT(NSMFEE)/(LOG(SMFMXE)-LOG(SMFMNE))
          END IF
          IF ((MSMFXY.EQ.0)) THEN
            IF ((X2Y2.GE.XMIN2).AND.(X2Y2.LT.XMAX2)) THEN
              IF ((WEIGHT.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IE=INT((LOG(WEIGHT)-LOG(SMFMNE))*SFACTOR)+1
              IF ((IE .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IE).EQ.NHSTRY)) THEN
                SUM_TMP(IE)=SUM_TMP(IE)+1
              ELSE
                FSMFBN1(1,1,IE)=FSMFBN1(1,1,IE)+SUM_TMP(IE)
                FSMFBN1(2,1,IE)=FSMFBN1(2,1,IE)+SUM_TMP(IE)*SUM_TMP(IE)
                SUM_TMP(IE)=1
                NHSTRY_LAST(IE)=NHSTRY
              END IF
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IF ((YIN.GT.SMFMNY(1)).AND.(YIN.LT.SMFMXY(1)).AND.(XIN.GT.SM
     *      FMNX(1)) .AND.(XIN.LT.SMFMXX(1))) THEN
              IF ((WEIGHT.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IE=INT((LOG(WEIGHT)-LOG(SMFMNE))*SFACTOR)+1
              IF ((IE .GT. NSMFEE)) THEN
                GOTO 5100
              END IF
              IF ((NHSTRY_LAST(IE).EQ.NHSTRY)) THEN
                SUM_TMP(IE)=SUM_TMP(IE)+1
              ELSE
                FSMFBN1(1,1,IE)=FSMFBN1(1,1,IE)+SUM_TMP(IE)
                FSMFBN1(2,1,IE)=FSMFBN1(2,1,IE)+SUM_TMP(IE)*SUM_TMP(IE)
                SUM_TMP(IE)=1
                NHSTRY_LAST(IE)=NHSTRY
              END IF
            END IF
          END IF
        ELSE IF((CHOICE1 .EQ. 9)) THEN
          IF ((MSMFXY.EQ.0)) THEN
            IF ((X2Y2.GE.XMIN2).AND.(X2Y2.LT.XMAX2)) THEN
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IF ((EI .GT. SMFMXE)) THEN
                GOTO 5100
              END IF
              WRITE(3,'(1PE15.4,1PE15.4)')XIN,YIN
              NUMSCATTER=NUMSCATTER+1
            END IF
          ELSE IF((MSMFXY.EQ.1)) THEN
            IF ((YIN.GT.SMFMNY(1)).AND.(YIN.LT.SMFMXY(1)).AND.(XIN.GT.SM
     *      FMNX(1)) .AND.(XIN.LT.SMFMXX(1))) THEN
              IF ((EI.LT.SMFMNE)) THEN
                GOTO 5100
              END IF
              IF ((EI .GT. SMFMXE)) THEN
                GOTO 5100
              END IF
              WRITE(3,'(1PE15.4,1PE15.4)')XIN,YIN
              NUMSCATTER=NUMSCATTER+1
            END IF
          END IF
          IF((NUMSCATTER.EQ.MAXSCATTER))GOTO 5110
        END IF
      GO TO 5091
5092  CONTINUE
5110  CONTINUE
      RETURN
      END
      SUBROUTINE ADD_FILES
      IMPLICIT NONE
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      COMMON/DOUBLES/ KWEI,SMFRTN, FSMFBN1,FSMABN1,FSMABN2,FSMFXX,FSMFYY
     *,FSMFR1,FSMFR2,FSMFR11,FSMFR12, FSMFRR,FSMFXY,BINSIZE,RBin,RBin_X,
     *RBin_Y
      REAL*8 KWEI, SMFRTN(22),  FSMFBN1(22,200,200), FSMABN1(200), FSMAB
     *N2(200), FSMFXX(22,200), FSMFYY(22,200), FSMFR1(22,200), FSMFR2(22
     *,200), FSMFR11(22,200), FSMFR12(22,200), FSMFRR(22,200), FSMFXY(22
     *,200,200), BINSIZE(200), RBin,RBin_X,RBin_Y
      COMMON/REALS/ XMIN2,XMAX2, XPLOT,HXMIN, YPLOT,YPLOTT, ERRYPLOT,X2Y
     *2,ZTOTAL,RSSD02,RSSD12,SSDVPS, SFACTOR,RFACTOR,RFACTOR1,RFACTOR2,
     *X,Y,Z,RRFACTOR,SSFACTOR,RSSD0,RSSD1, WEIGHT,EI, PARAEMAX2,PARAEMAX
     *1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAEMNE, PARANINC1,PARANINC2,PARA
     *NINC, EREAD,ZLAST, MUIDX,XIN,YIN,ZIN,UIN,VIN,WIN, SMFX2,SMFY2,SMFM
     *XR, HFIELD, RSCATT, DELTAZ,RSSDST,SSSDST,ERADII,E_ph_sp_e, E_ph_sp
     *_g, E_ph_sp_p, E_MIN_ph_sp, E_MAX_ph_sp, RFIELD,RFIELDe,XFLDMN,XFL
     *DMX, YFLDMN, YFLDMX, SMFMNZ, SMFMXZ, SMFMNX, SMFMXX, SMFMNY, SMFMX
     *Y, SMFMNR,RTREAT,RTREATe,XTREATMN,XTREATMX, YTREATMN,YTREATMX, SMF
     *MNE,SMFMXE, ABNTOTAL,ABNTOTALH,FLUAREA,NINC, RMIN_MIN,RMIN_MAX,RMA
     *X_MIN,RMAX_MAX,DOMEGA, SUM_TMP,SUM_TMP2,Z_SCORE,mumin,mumax
      real*8 XMIN2,XMAX2,  XPLOT(200),HXMIN,  YPLOT(200),YPLOTT, ERRYPLO
     *T(200), X2Y2,  ZTOTAL(22),  RSSD02,RSSD12,  SSDVPS(22+3),  SFACTOR
     *,RFACTOR,RFACTOR1,RFACTOR2,X,Y,Z,RRFACTOR,SSFACTOR, RSSD0,RSSD1,
     *WEIGHT,EI, PARAEMAX2,PARAEMAX1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAE
     *MNE, PARANINC1,PARANINC2,PARANINC, EREAD,ZLAST,MUIDX, XIN,YIN,ZIN,
     *UIN, VIN,WIN,  SMFX2(22), SMFY2(22),  SMFMXR(22),  HFIELD(22),  RS
     *CATT,  DELTAZ,  RSSDST(22,2000), SSSDST(22,2000), ERADII(200),  E_
     *ph_sp_e,  E_ph_sp_g,  E_ph_sp_p,  E_MIN_ph_sp,  E_MAX_ph_sp,  RFIE
     *LD,  RFIELDe,   XFLDMN,  XFLDMX,  YFLDMN,  YFLDMX,  SMFMNZ(22),
     *SMFMXZ(22),  SMFMNX(22),  SMFMXX(22),  SMFMNY(22),  SMFMXY(22),  S
     *MFMNR(22),  RTREAT,  RTREATe,   XTREATMN,  XTREATMX,  YTREATMN,  Y
     *TREATMX,  SMFMNE,SMFMXE,  ABNTOTAL,ABNTOTALH,  FLUAREA,   NINC,  R
     *MIN_MIN, RMIN_MAX,  RMAX_MIN,  RMAX_MAX, DOMEGA,  SUM_TMP(200),  S
     *UM_TMP2(200),  Z_SCORE,  mumin,mumax
      COMMON/INTEGERS/ PARANOT,PARANOT1,PARANOT2,PARANOP,PARANOP1, PARAN
     *OP2,IPARANOT,IPARANOT1,IPARANOT2, LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST, I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBER
     *ST,NBIT1,NBIT2, NFIELDi,NFIELDo,NFIELDe, NWRONG,LATCHJ,CHOICE,CHOI
     *CE1, K1,K2,K3,K4,IA, IX,IY,IR,K,IQ,LATCHI,IE, NPASSI,LATCH,SMFPXY,
     *NFIELD,NSMFPS,I_IN_EX, ISMFPS,IQSMFP,MSTART,MSMFXY,NSMFEE,NPASS_ph
     *_sp, N_ph_sp_e, N_ph_sp_g, N_ph_sp_p, FLUTYPE,ANGTYPE,IZLAST1, IZL
     *AST2,IMUIDX1,IMUIDX2,IZSCORE1,IZSCORE2,JUSTONE, MAXSCATTER,NUMSCAT
     *TER,ESTYPE,i_iaea_in,i_iaea_out,i_log, i_unit_in,i_unit_out
      INTEGER*8 PARANOT,PARANOT1,PARANOT2, PARANOP,PARANOP1,  PARANOP2,I
     *PARANOT,IPARANOT1,IPARANOT2,  LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST(200)
      INTEGER I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBERST,NBIT1,NBI
     *T2,  NWRONG,LATCHJ(29), CHOICE,CHOICE1,  K1,K2,K3,K4,IA, IX,IY,IR,
     *K(22), IQ,LATCHI,IE, NPASSI,  LATCH(22),   SMFPXY(22),   NFIELD,
     * NFIELDi,   NFIELDo,   NFIELDe,  NSMFPS,  I_IN_EX,  ISMFPS(22),
     *IQSMFP(22),   MSTART,   MSMFXY,  NSMFEE,  NPASS_ph_sp,  N_ph_sp_e,
     *  N_ph_sp_g,  N_ph_sp_p,  FLUTYPE,  ANGTYPE,  IZLAST1,  IZLAST2,
     *IMUIDX1,   IMUIDX2,  IZSCORE1,  IZSCORE2,  JUSTONE(200),  MAXSCATT
     *ER,  NUMSCATTER,  ESTYPE ,  i_iaea_in,  i_iaea_out,   i_log,   i_u
     *nit_in,i_unit_out
      COMMON/LOGICALS/OLDFILE,EGSPERT
      LOGICAL OLDFILE,    EGSPERT
      COMMON/RWPHSP/ STRING_TEMP_ZLAST_OUT,  STRING_TEMP_OUT,  IHSTRY_PH
     *SP,   iaea_dummy_long,  NHSTRY_DOS,   ESHORT,  WEIGHTTMP,  WT_PHSP
     *_SHORT,  X_PHSP_SHORT,  Y_PHSP_SHORT,  Z_PHSP_SHORT,  U_PHSP_SHORT
     *,  V_PHSP_SHORT,  W_PHSP_SHORT,  ZLAST_PHSP_SHORT,  MUIDX_PHSP_SHO
     *RT,  EKMAX_PHSP_SHORT,  EKMINE_PHSP_SHORT,  NINC_PHSP_SHORT,  dosx
     *yz2beam_index,  iaea_extra_floats,  NUM_PHSP_TOT,  PHOT_PHSP_TOT,
     *iaea_result,   iaea_n_stat,  iaea_q_typ,  iaea_typ_q,  iaea_q_inde
     *x,  I_PHSP,  IERR_PHSP,  LATCHTMP,  WRITE_PHSP_COUNTER,  WRITE_PHS
     *P_SOFAR,  PHSP_RESTART,  PHSP_RECL,  PHSP_RECL_OLD,  iaea_extra_in
     *ts,  iaea_extra_int_types,iaea_extra_float_types,   iaea_i_zlast,
     * iaea_i_latch,  iaea_i_muidx,  iaea_n_extra_ints,  iaea_n_extra_fl
     *oats,  i_iaea_open_for_write,   iaea_i_zlast_out,  iaea_i_latch_ou
     *t,  iaea_i_muidx_out,  dosxyz2beam_izscore,  MODE_RW
      CHARACTER*32000 STRING_TEMP_ZLAST_OUT(1)
      CHARACTER*28000 STRING_TEMP_OUT(1)
      INTEGER*8 IHSTRY_PHSP(1),iaea_dummy_long,NHSTRY_DOS
      REAL*4 ESHORT,WEIGHTTMP,WT_PHSP_SHORT,X_PHSP_SHORT,Y_PHSP_SHORT, Z
     *_PHSP_SHORT,U_PHSP_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,ZLAST_PHSP_SHO
     *RT, MUIDX_PHSP_SHORT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP
     *_SHORT,dosxyz2beam_index, iaea_extra_floats(10)
      INTEGER NUM_PHSP_TOT,PHOT_PHSP_TOT, iaea_result,iaea_n_stat,iaea_q
     *_typ(3),iaea_typ_q(3),iaea_q_index,I_PHSP, IERR_PHSP,LATCHTMP, WRI
     *TE_PHSP_COUNTER(1),WRITE_PHSP_SOFAR(1), PHSP_RESTART,PHSP_RECL,PHS
     *P_RECL_OLD,iaea_extra_ints(10), iaea_extra_int_types(10),iaea_extr
     *a_float_types(10),iaea_i_zlast, iaea_i_latch,iaea_i_muidx,iaea_n_e
     *xtra_ints,iaea_n_extra_floats, i_iaea_open_for_write,iaea_i_zlast_
     *out,iaea_i_latch_out, iaea_i_muidx_out,dosxyz2beam_izscore
      CHARACTER*5 MODE_RW
      INTEGER LATCH_TEMP_OUT,NUM_PHSP_TOFLUSH,TEMP_PHSP_COUNTER,REM_PHSP
      REAL*4 REAL_TEMP_OUT(7)
      CHARACTER*32 REC_TEMP_OUT
      integer*4 len, lnblnk1, itmp
      EQUIVALENCE(REC_TEMP_OUT(1:4),LATCH_TEMP_OUT)
      EQUIVALENCE(REC_TEMP_OUT(5:8),REAL_TEMP_OUT(1))
      EQUIVALENCE(REC_TEMP_OUT(9:12),REAL_TEMP_OUT(2))
      EQUIVALENCE(REC_TEMP_OUT(13:16),REAL_TEMP_OUT(3))
      EQUIVALENCE(REC_TEMP_OUT(17:20),REAL_TEMP_OUT(4))
      EQUIVALENCE(REC_TEMP_OUT(21:24),REAL_TEMP_OUT(5))
      EQUIVALENCE(REC_TEMP_OUT(25:28),REAL_TEMP_OUT(6))
      EQUIVALENCE(REC_TEMP_OUT(29:32),REAL_TEMP_OUT(7))
      i_log=6
3010  CONTINUE
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(22)
      END IF
3020  WRITE(6,3030)
3030  FORMAT(/' Name of the old ph-sp file 1: '/)
      READ(5,3040)OLDNAM
3040  FORMAT(A80)
      IF ((OLDNAM.EQ.' ').OR.(OLDNAM.EQ.'?')) THEN
        GOTO 3050
      END IF
      GOTO 3060
3050  CALL HELP_MESSAGE(22)
      GOTO 3020
3060  i_iaea_in=0
      len=lnblnk1(OLDNAM)
      IF ((OLDNAM(len-8:len).EQ.'.IAEAphsp')) THEN
        i_iaea_in=1
        OLDNAM=OLDNAM(:len-9)
        WRITE(6,3070)
3070    FORMAT(/' Data is in IAEA format.'/)
      END IF
      PARANOT1=0
      PARANOP1=0
      PARAEMAX1=0.
      PARAEMNE1=0.
      PARANINC1=0.
      MODE_RW1=' '
      IZLAST1=0
      IMUIDX1=0
      IZSCORE1=0
      IF ((i_iaea_in.EQ.1)) THEN
        i_unit_in=2
        call iaea_new_source(i_unit_in,OLDNAM,1,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error opening IAEA phase space source.'
        END IF
        call iaea_get_max_particles(i_unit_in,-1,iaea_dummy_long)
        PARANOT1=iaea_dummy_long
        call iaea_get_max_particles(i_unit_in,1,iaea_dummy_long)
        PARANOP1=iaea_dummy_long
        call iaea_get_total_original_particles(i_unit_in,iaea_dummy_long
     *  )
        LPARANINC1=iaea_dummy_long
        call iaea_get_maximum_energy(i_unit_in,EKMAX_PHSP_SHORT)
        PARAEMAX1=EKMAX_PHSP_SHORT
        call iaea_get_constant_variable(i_unit_in,2,Z_PHSP_SHORT,iaea_re
     *  sult)
        IF ((iaea_result.EQ.-3)) THEN
          write(i_log,*) ' Z positions of each particle will be read fro
     *m phase space data.'
          Z_SCORE=999.
          IZSCORE1=1
        ELSE IF((iaea_result.LT.0)) THEN
          write(i_log,*)' Error reading Z position where IAEA phsp was s
     *cored.'
          IZSCORE1=0
        ELSE
          Z_SCORE=Z_PHSP_SHORT
          IZSCORE1=0
        END IF
        call iaea_get_extra_numbers(i_unit_in,iaea_n_extra_floats,iaea_n
     *  _extra_ints)
        IF ((iaea_n_extra_floats .EQ. -1 .OR. iaea_n_extra_ints .EQ. -1)
     *  ) THEN
          write(i_log,*)' Error reading number of extra variables stored
     * in IAEA phsp'
        END IF
        call iaea_get_type_extra_variables(i_unit_in,iaea_result,iaea_ex
     *  tra_int_types, iaea_extra_float_types)
        IF ((iaea_result.EQ.-1)) THEN
          write(i_log,*)' Error getting types of extra variables stored
     *in IAEA phsp'
        END IF
        DO 3081 I_PHSP=1,iaea_n_extra_ints
          IF ((iaea_extra_int_types(I_PHSP).EQ.2)) THEN
            iaea_i_latch=I_PHSP
            GO TO3082
          END IF
3081    CONTINUE
3082    CONTINUE
        IF ((iaea_i_latch.EQ.-99)) THEN
          write(i_log,*)' Warning: IAEA format phsp file does not store
     *LATCH'
        END IF
        DO 3091 I_PHSP=1,iaea_n_extra_floats
          IF ((iaea_extra_float_types(I_PHSP).EQ.3)) THEN
            iaea_i_zlast=I_PHSP
            GO TO3092
          END IF
3091    CONTINUE
3092    CONTINUE
        IF ((iaea_i_zlast.EQ.-99)) THEN
          IZLAST1=0
        ELSE
          IZLAST1=1
        END IF
        iaea_i_muidx=MAX(1,iaea_i_zlast+1)
        IF ((iaea_i_muidx.GT.iaea_n_extra_floats .OR. iaea_extra_float_t
     *  ypes(iaea_i_muidx).NE.0)) THEN
          iaea_i_muidx=-99
          IMUIDX1=0
        ELSE
          IMUIDX1=1
        END IF
        WRITE(6,3100)PARANOT1,PARANOP1,PARAEMAX1,LPARANINC1
3100    FORMAT(/'            Total number of particles in file:',I13/ ' 
     *                    Total number of photons:',I13/ 'The rest are e
     *lectrons/positrons.'/ ' '/ '      Maximum kinetic energy of the pa
     *rticles:',F13.3,' MeV'/ ' # of incident particles from original so
     *urce:',I13)
        IF ((IZSCORE1.EQ.0)) THEN
          WRITE(6,3110)Z_SCORE
3110      FORMAT('                     Z where phsp file scored:',F13.3,
     *' cm')
        END IF
        IF ((IZLAST1.NE.0)) THEN
          WRITE(6,3120)
3120      FORMAT(' ZLAST scored in this file.')
        END IF
        IF ((IMUIDX1.NE.0)) THEN
          WRITE(6,3130)
3130      FORMAT(' Fractional MU index scored in this file.')
        END IF
      ELSE
        itmp=-1
        IF ((itmp.GT.0)) THEN
          PHSP_RECL=4*8
          OPEN(UNIT=2,FILE=OLDNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3140)
3140        FORMAT(/' ***error opening file as MODE2 ****' /' *** THE FI
     *LE FORMAT MAY BE WRONG (I.E., NOT A MODE2 FILE) ***'/ /' *** WE NO
     *W TRY TO OPEN IT AS A MODE3 FILE ***'//)
            OPEN(UNIT=2,STATUS='OLD',FILE=OLDNAM, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3150)
3150          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
              STOP
            END IF
            READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW1,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT1=NUM_PHSP_TOT
            PARANOP1=PHOT_PHSP_TOT
            PARAEMAX1=EKMAX_PHSP_SHORT
            PARAEMNE1=EKMINE_PHSP_SHORT
            PARANINC1=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3160)
3160          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW1 .NE. 'MODE3')) THEN
              WRITE(6,3170)
3170          FORMAT(//' That file does not start with MODE3,', ' as all
     * old compressed files (with ZLAST) must'/ '  Check it out and trya
     *gain!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,3180)
3180          FORMAT(//' This is a MODE3 file! '/ ' Please convert it in
     *to a MODE2 file using [readphsp] ', ' and try again!'///)
              IERR_PHSP=1
              STOP
            END IF
          END IF
          READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW1,NUM_PHSP_TOT,PHOT_PHSP_
     *    TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          PARANOT1=NUM_PHSP_TOT
          PARANOP1=PHOT_PHSP_TOT
          PARAEMAX1=EKMAX_PHSP_SHORT
          PARAEMNE1=EKMINE_PHSP_SHORT
          PARANINC1=NINC_PHSP_SHORT
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3190)
3190        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW1 .NE. 'MODE2')) THEN
            WRITE(6,3200)
3200        FORMAT(//' That file does not start with MODE2,', ' as stand
     *ard compressed files with ZLAST must'/ '  Check it out and try aga
     *in!'///)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.EQ.0)) THEN
          PHSP_RECL=4*7
          OPEN(UNIT=2,FILE=OLDNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3210)
3210        FORMAT(/' ***ERROR opening file as MODE0****' /' *** THE FIL
     *E FORMAT MAY BE WRONG (I.E., NOT A MODE0 FILE) ***'/ /' *** WE NOW
     * TRY TO OPEN IT AS A MODE1 FILE ***'//)
            OPEN(UNIT=2,STATUS='OLD',FILE=OLDNAM, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3220)
3220          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
              STOP
            END IF
            READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW1,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT1=NUM_PHSP_TOT
            PARANOP1=PHOT_PHSP_TOT
            PARAEMAX1=EKMAX_PHSP_SHORT
            PARAEMNE1=EKMINE_PHSP_SHORT
            PARANINC1=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3230)
3230          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW1 .NE. 'MODE1')) THEN
              WRITE(6,3240)
3240          FORMAT(//' That file does not start with MODE1,', ' as all
     * old compressed files must'/ '  Check it out and try again!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,3250)
3250          FORMAT(//' This is a MODE1 file! '/ ' Please convert it in
     *to a MODE0 file using [readphsp] ', 'and try again!'///)
              IERR_PHSP=1
              STOP
            END IF
          END IF
          READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW1,NUM_PHSP_TOT,PHOT_PHSP_
     *    TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          PARANOT1=NUM_PHSP_TOT
          PARANOP1=PHOT_PHSP_TOT
          PARAEMAX1=EKMAX_PHSP_SHORT
          PARAEMNE1=EKMINE_PHSP_SHORT
          PARANINC1=NINC_PHSP_SHORT
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3260)
3260        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW1 .NE. 'MODE0')) THEN
            WRITE(6,3270)
3270        FORMAT(/' Does not start with MODE0 as files without ZLAST m
     *ust'/ '  Try again!'//)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.LT.0)) THEN
          WRITE(6,3280)
3280      FORMAT(/' First, try to open it as a MODE0 file')
          PHSP_RECL=4*7
          OPEN(UNIT=2,FILE=OLDNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3290)
3290        FORMAT(/' Now try to open it as a MODE2 file')
            itmp=1
            PHSP_RECL=4*8
            OPEN(UNIT=2,FILE=OLDNAM,FORM='UNFORMATTED',ACCESS='DIRECT',
     *      RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3300)
3300          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
            ELSE
              READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW1,NUM_PHSP_TOT,PHOT_P
     *        HSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHOR
     *        T
              PARANOT1=NUM_PHSP_TOT
              PARANOP1=PHOT_PHSP_TOT
              PARAEMAX1=EKMAX_PHSP_SHORT
              PARAEMNE1=EKMINE_PHSP_SHORT
              PARANINC1=NINC_PHSP_SHORT
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,3310)
3310            FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE**
     ** '//)
                STOP
              END IF
              IF ((MODE_RW1.NE.'MODE2')) THEN
                WRITE(6,3320)
3320            FORMAT(//' That file does not start with MODE2,', ' as s
     *tandard compressed files with ZLAST must'/ '  Check it out and try
     * again!'///)
                CLOSE(2)
                STOP
              END IF
            END IF
          ELSE
            READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW1,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT1=NUM_PHSP_TOT
            PARANOP1=PHOT_PHSP_TOT
            PARAEMAX1=EKMAX_PHSP_SHORT
            PARAEMNE1=EKMINE_PHSP_SHORT
            PARANINC1=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3330)
3330          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            itmp=0
            IF ((MODE_RW1.NE.'MODE0')) THEN
              WRITE(6,3340)
3340          FORMAT(/' The file does not start with MODE0 as it suppose
     *d to')
              CLOSE(2)
              WRITE(6,3350)
3350          FORMAT(/' Now try to open it as a MODE2 file')
              itmp=1
              PHSP_RECL=4*8
              OPEN(UNIT=2,FILE=OLDNAM,FORM='UNFORMATTED',ACCESS='DIRECT'
     *        , RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,3360)
3360            FORMAT(//' *** PHASE SPACE FILE CANNOT BE OPENED. ***')
              ELSE
                READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW1,NUM_PHSP_TOT,PHOT
     *          _PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_
     *          SHORT
                PARANOT1=NUM_PHSP_TOT
                PARANOP1=PHOT_PHSP_TOT
                PARAEMAX1=EKMAX_PHSP_SHORT
                PARAEMNE1=EKMINE_PHSP_SHORT
                PARANINC1=NINC_PHSP_SHORT
                IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,3370)
3370              FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE
     **** '//)
                  STOP
                END IF
                IF ((MODE_RW1.NE.'MODE2')) THEN
                  WRITE(6,3380)
3380              FORMAT(//' That file does not start with MODE2,', ' as
     * standard compressed files with ZLAST must'/ '  Check it out and t
     *ry again!'///)
                  CLOSE(2)
                  STOP
                END IF
              END IF
            END IF
          END IF
        END IF
        IF((IERR_PHSP.NE.0))GOTO 3390
        WRITE(6,3400)PARANOT1,PARANOP1,PARAEMAX1,PARAEMNE1
3400    FORMAT(/'            Total number of particles in file:',I13/ ' 
     *                    Total number of photons:',I13/ 'The rest are e
     *lectrons/positrons.'/ ' '/ '      Maximum kinetic energy of the pa
     *rticles:',F13.3,' MeV'/ '      Minimum kinetic energy of the elect
     *rons:',F13.3,' MeV')
        PARANOT=PARANOT1
        IF ((PARANINC1 .LT. 2.)) THEN
          WRITE(6,3410)PARANINC1
3410      FORMAT('        Minimum kinetic energy of the photons:',F13.3,
     *' MeV')
        ELSE
          WRITE(6,3420)PARANINC1
3420      FORMAT(' # of incident particles from original source:',F13.1)
        END IF
      END IF
3430  CONTINUE
      WRITE(6,3440)
3440  FORMAT(/' Name of the old ph-sp file 2: '/' ',$)
      READ(5,3450)SPCNAM
3450  FORMAT(A80)
      IF ((SPCNAM .EQ. ' ')) THEN
        GOTO 3430
      END IF
      i_iaea_out=0
      len=lnblnk1(SPCNAM)
      IF ((SPCNAM(len-8:len).EQ.'.IAEAphsp')) THEN
        i_iaea_out=1
        SPCNAM=SPCNAM(:len-9)
        WRITE(6,3460)
3460    FORMAT(/' Data is in IAEA format.'/)
      END IF
      IF ((i_iaea_in.NE.i_iaea_out)) THEN
        WRITE(6,3470)
3470    FORMAT(//' *****ERROR*****'/ ' Both phase space files must havet
     *he same format'/ ' (IAEA or standard BEAMnrc).  Try again.'//)
        STOP
      END IF
      PARANOT2=0
      PARANOP2=0
      LPARANINC2=0
      PARAEMAX2=0.
      PARAEMNE2=0.
      PARANINC2=0.
      MODE_RW2=' '
      IZLAST2=0
      IMUIDX2=0
      IZSCORE2=0
      IF ((i_iaea_out.EQ.1)) THEN
        i_unit_out=3
        call iaea_new_source(i_unit_out,SPCNAM,1+2,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error opening IAEA phase space file for write.
     *'
          call exit(1)
        END IF
        i_iaea_open_for_write=1
        IF ((1.EQ.0)) THEN
          IF((IZSCORE2.NE.1))call iaea_set_constant_variable(i_unit_out,
     *    2,Z_SCORE)
          IF ((IZLAST2.NE.0)) THEN
            IF ((IMUIDX2.NE.0)) THEN
              call iaea_set_extra_numbers(i_unit_out,2,1)
              call iaea_set_type_extrafloat_variable(i_unit_out,1,0)
              iaea_i_muidx_out=2
            ELSE
              call iaea_set_extra_numbers(i_unit_out,1,1)
            END IF
            call iaea_set_type_extrafloat_variable(i_unit_out,0,3)
            iaea_i_zlast_out=1
          ELSE IF((IMUIDX2.NE.0)) THEN
            call iaea_set_extra_numbers(i_unit_out,1,1)
            call iaea_set_type_extrafloat_variable(i_unit_out,0,0)
            iaea_i_muidx_out=1
          ELSE
            call iaea_set_extra_numbers(i_unit_out,0,1)
          END IF
          call iaea_set_type_extralong_variable(i_unit_out,0,2)
          iaea_i_latch_out=1
        END IF
        call iaea_get_max_particles(i_unit_out,-1,iaea_dummy_long)
        PARANOT2=iaea_dummy_long
        call iaea_get_max_particles(i_unit_out,1,iaea_dummy_long)
        PARANOP2=iaea_dummy_long
        call iaea_get_total_original_particles(i_unit_out,iaea_dummy_lon
     *  g)
        LPARANINC2=iaea_dummy_long
        call iaea_get_maximum_energy(i_unit_out,EKMAX_PHSP_SHORT)
        PARAEMAX2=EKMAX_PHSP_SHORT
        call iaea_get_constant_variable(i_unit_out,2,Z_PHSP_SHORT,iaea_r
     *  esult)
        IF ((iaea_result.EQ.-3)) THEN
          write(i_log,*) ' Z positions of each particle will be read fro
     *m phase space data.'
          Z_SCORE=999.
          IZSCORE2=1
        ELSE IF((iaea_result.LT.0)) THEN
          write(i_log,*)' Error reading Z position where IAEA phsp was s
     *cored.'
          IZSCORE2=0
        ELSE
          Z_SCORE=Z_PHSP_SHORT
          IZSCORE2=0
        END IF
        call iaea_get_extra_numbers(i_unit_out,iaea_n_extra_floats,iaea_
     *  n_extra_ints)
        IF ((iaea_n_extra_floats .EQ. -1 .OR. iaea_n_extra_ints .EQ. -1)
     *  ) THEN
          write(i_log,*)' Error reading number of extra variables stored
     * in IAEA phsp'
        END IF
        call iaea_get_type_extra_variables(i_unit_out,iaea_result,iaea_e
     *  xtra_int_types, iaea_extra_float_types)
        IF ((iaea_result.EQ.-1)) THEN
          write(i_log,*)' Error getting types of extra variables stored
     *in IAEA phsp'
        END IF
        DO 3481 I_PHSP=1,iaea_n_extra_ints
          IF ((iaea_extra_int_types(I_PHSP).EQ.2)) THEN
            iaea_i_latch=I_PHSP
            GO TO3482
          END IF
3481    CONTINUE
3482    CONTINUE
        IF ((iaea_i_latch.EQ.-99)) THEN
          write(i_log,*)' Warning: IAEA format phsp file does not store
     *LATCH'
        END IF
        DO 3491 I_PHSP=1,iaea_n_extra_floats
          IF ((iaea_extra_float_types(I_PHSP).EQ.3)) THEN
            iaea_i_zlast=I_PHSP
            GO TO3492
          END IF
3491    CONTINUE
3492    CONTINUE
        IF ((iaea_i_zlast.EQ.-99)) THEN
          IZLAST2=0
        ELSE
          IZLAST2=1
        END IF
        iaea_i_muidx=MAX(1,iaea_i_zlast+1)
        IF ((iaea_i_muidx.GT.iaea_n_extra_floats .OR. iaea_extra_float_t
     *  ypes(iaea_i_muidx).NE.0)) THEN
          iaea_i_muidx=-99
          IMUIDX2=0
        ELSE
          IMUIDX2=1
        END IF
        WRITE(6,3500)PARANOT2,PARANOP2,PARAEMAX2,LPARANINC2
3500    FORMAT(/'            Total number of particles in file:',I13/ ' 
     *                    Total number of photons:',I13/ 'The rest are e
     *lectrons/positrons.'/ ' '/ '      Maximum kinetic energy of the pa
     *rticles:',F13.3,' MeV'/ ' # of incident particles from original so
     *urce:',I13)
        IF ((IZSCORE2.EQ.1)) THEN
          WRITE(6,3510)Z_SCORE
3510      FORMAT('                     Z where phsp file scored:',F13.3,
     *' cm')
        END IF
        IF ((IZLAST2.NE.0)) THEN
          WRITE(6,3520)
3520      FORMAT(' ZLAST scored in phase space data.')
        END IF
        IF ((IMUIDX2.NE.0)) THEN
          WRITE(6,3530)
3530      FORMAT(' Fractional MU index scored in phase space data.')
        END IF
      ELSE
        itmp=-1
        IF ((itmp.GT.0)) THEN
          PHSP_RECL=4*8
          OPEN(UNIT=3,FILE=SPCNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3540)
3540        FORMAT(/' ***error opening file as MODE2 ****' /' *** THE FI
     *LE FORMAT MAY BE WRONG (I.E., NOT A MODE2 FILE) ***'/ /' *** WE NO
     *W TRY TO OPEN IT AS A MODE3 FILE ***'//)
            OPEN(UNIT=3,STATUS='OLD',FILE=SPCNAM, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3550)
3550          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
              STOP
            END IF
            READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT2=NUM_PHSP_TOT
            PARANOP2=PHOT_PHSP_TOT
            PARAEMAX2=EKMAX_PHSP_SHORT
            PARAEMNE2=EKMINE_PHSP_SHORT
            PARANINC2=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3560)
3560          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW2 .NE. 'MODE3')) THEN
              WRITE(6,3570)
3570          FORMAT(//' That file does not start with MODE3,', ' as all
     * old compressed files (with ZLAST) must'/ '  Check it out and trya
     *gain!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,3580)
3580          FORMAT(//' This is a MODE3 file! '/ ' Please convert it in
     *to a MODE2 file using [readphsp] ', ' and try again!'///)
              IERR_PHSP=1
              STOP
            END IF
          END IF
          READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_PHSP_
     *    TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          PARANOT2=NUM_PHSP_TOT
          PARANOP2=PHOT_PHSP_TOT
          PARAEMAX2=EKMAX_PHSP_SHORT
          PARAEMNE2=EKMINE_PHSP_SHORT
          PARANINC2=NINC_PHSP_SHORT
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3590)
3590        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW2 .NE. 'MODE2')) THEN
            WRITE(6,3600)
3600        FORMAT(//' That file does not start with MODE2,', ' as stand
     *ard compressed files with ZLAST must'/ '  Check it out and try aga
     *in!'///)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.EQ.0)) THEN
          PHSP_RECL=4*7
          OPEN(UNIT=3,FILE=SPCNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3610)
3610        FORMAT(/' ***ERROR opening file as MODE0****' /' *** THE FIL
     *E FORMAT MAY BE WRONG (I.E., NOT A MODE0 FILE) ***'/ /' *** WE NOW
     * TRY TO OPEN IT AS A MODE1 FILE ***'//)
            OPEN(UNIT=3,STATUS='OLD',FILE=SPCNAM, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3620)
3620          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
              STOP
            END IF
            READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT2=NUM_PHSP_TOT
            PARANOP2=PHOT_PHSP_TOT
            PARAEMAX2=EKMAX_PHSP_SHORT
            PARAEMNE2=EKMINE_PHSP_SHORT
            PARANINC2=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3630)
3630          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW2 .NE. 'MODE1')) THEN
              WRITE(6,3640)
3640          FORMAT(//' That file does not start with MODE1,', ' as all
     * old compressed files must'/ '  Check it out and try again!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,3650)
3650          FORMAT(//' This is a MODE1 file! '/ ' Please convert it in
     *to a MODE0 file using [readphsp] ', 'and try again!'///)
              IERR_PHSP=1
              STOP
            END IF
          END IF
          READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_PHSP_
     *    TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          PARANOT2=NUM_PHSP_TOT
          PARANOP2=PHOT_PHSP_TOT
          PARAEMAX2=EKMAX_PHSP_SHORT
          PARAEMNE2=EKMINE_PHSP_SHORT
          PARANINC2=NINC_PHSP_SHORT
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3660)
3660        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW2 .NE. 'MODE0')) THEN
            WRITE(6,3670)
3670        FORMAT(/' Does not start with MODE0 as files without ZLAST m
     *ust'/ '  Try again!'//)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.LT.0)) THEN
          WRITE(6,3680)
3680      FORMAT(/' First, try to open it as a MODE0 file')
          PHSP_RECL=4*7
          OPEN(UNIT=3,FILE=SPCNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,3690)
3690        FORMAT(/' Now try to open it as a MODE2 file')
            itmp=1
            PHSP_RECL=4*8
            OPEN(UNIT=3,FILE=SPCNAM,FORM='UNFORMATTED',ACCESS='DIRECT',
     *      RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3700)
3700          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
            ELSE
              READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_P
     *        HSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHOR
     *        T
              PARANOT2=NUM_PHSP_TOT
              PARANOP2=PHOT_PHSP_TOT
              PARAEMAX2=EKMAX_PHSP_SHORT
              PARAEMNE2=EKMINE_PHSP_SHORT
              PARANINC2=NINC_PHSP_SHORT
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,3710)
3710            FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE**
     ** '//)
                STOP
              END IF
              IF ((MODE_RW2.NE.'MODE2')) THEN
                WRITE(6,3720)
3720            FORMAT(//' That file does not start with MODE2,', ' as s
     *tandard compressed files with ZLAST must'/ '  Check it out and try
     * again!'///)
                CLOSE(3)
                STOP
              END IF
            END IF
          ELSE
            READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT2=NUM_PHSP_TOT
            PARANOP2=PHOT_PHSP_TOT
            PARAEMAX2=EKMAX_PHSP_SHORT
            PARAEMNE2=EKMINE_PHSP_SHORT
            PARANINC2=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,3730)
3730          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            itmp=0
            IF ((MODE_RW2.NE.'MODE0')) THEN
              WRITE(6,3740)
3740          FORMAT(/' The file does not start with MODE0 as it suppose
     *d to')
              CLOSE(3)
              WRITE(6,3750)
3750          FORMAT(/' Now try to open it as a MODE2 file')
              itmp=1
              PHSP_RECL=4*8
              OPEN(UNIT=3,FILE=SPCNAM,FORM='UNFORMATTED',ACCESS='DIRECT'
     *        , RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,3760)
3760            FORMAT(//' *** PHASE SPACE FILE CANNOT BE OPENED. ***')
              ELSE
                READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT
     *          _PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_
     *          SHORT
                PARANOT2=NUM_PHSP_TOT
                PARANOP2=PHOT_PHSP_TOT
                PARAEMAX2=EKMAX_PHSP_SHORT
                PARAEMNE2=EKMINE_PHSP_SHORT
                PARANINC2=NINC_PHSP_SHORT
                IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,3770)
3770              FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE
     **** '//)
                  STOP
                END IF
                IF ((MODE_RW2.NE.'MODE2')) THEN
                  WRITE(6,3780)
3780              FORMAT(//' That file does not start with MODE2,', ' as
     * standard compressed files with ZLAST must'/ '  Check it out and t
     *ry again!'///)
                  CLOSE(3)
                  STOP
                END IF
              END IF
            END IF
          END IF
        END IF
        IF((IERR_PHSP.NE.0))GOTO 3790
        WRITE(6,3800)PARANOT2,PARANOP2,PARAEMAX2,PARAEMNE2
3800    FORMAT(/'            Total number of particles in file:',I13/ ' 
     *                     Total number of photons:',I13/ ' The rest are
     * electrons/positrons.'/ ' '/ '      Maximum kinetic energy of the 
     *particles:',F13.3,' MeV'/ '      Minimum kinetic energy of the ele
     *ctrons:',F13.3,' MeV')
        IF ((PARANINC2 .LT. 2.)) THEN
          WRITE(6,3810)PARANINC2
3810      FORMAT('        Minimum kinetic energy of the photons:',F13.3,
     *' MeV')
        ELSE
          WRITE(6,3820)PARANINC2
3820      FORMAT(' # of incident particles from original source:',F13.1)
        END IF
      END IF
      IF ((i_iaea_in.NE.i_iaea_out)) THEN
        WRITE(6,3830)
3830    FORMAT(//' *****ERROR*****'/ ' Both phase space files must havet
     *he same format'/ ' (IAEA or standard BEAMnrc).  Try again.'//)
        IF ((i_iaea_in.EQ.1)) THEN
          call iaea_destroy_source(i_unit_in,iaea_result)
          IF ((iaea_result.LT.0)) THEN
            WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_i
     *      n
          END IF
        ELSE
          CLOSE(2)
        END IF
        IF ((i_iaea_out.EQ.1)) THEN
          call iaea_destroy_source(i_unit_out,iaea_result)
          IF ((iaea_result.LT.0)) THEN
            WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_o
     *      ut
          END IF
        ELSE
          CLOSE(3)
        END IF
        RETURN
      ELSE IF(((i_iaea_in.EQ.0 .AND. MODE_RW2 .NE. MODE_RW1) .OR. (i_iae
     *a_in.EQ.1 .AND. IZLAST1 .NE. IZLAST2))) THEN
        WRITE(6,3840)
3840    FORMAT(/' SORRY, CANNOT COMBINE FILES OF DIFFERENT MODES!!! '/)
        WRITE(6,3850)
3850    FORMAT(/' PLEASE CONVERT FILES INTO EITHER MODE0 OR 2, THEN TRYA
     *GAIN.'/)
        IF ((i_iaea_in.EQ.1)) THEN
          call iaea_destroy_source(i_unit_in,iaea_result)
          IF ((iaea_result.LT.0)) THEN
            WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_i
     *      n
          END IF
          call iaea_destroy_source(i_unit_out,iaea_result)
          IF ((iaea_result.LT.0)) THEN
            WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_o
     *      ut
          END IF
        ELSE
          CLOSE(2)
          CLOSE(3)
        END IF
        RETURN
      ELSE IF((i_iaea_in.EQ.1 .AND. IZSCORE1 .NE. IZSCORE2)) THEN
        WRITE(6,3860)
3860    FORMAT(/' Error: One IAEA phase space file scores particle Z, wh
     *ile the'/ ' other does not.  Cannot combine.'/)
        call iaea_destroy_source(i_unit_in,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_in
        END IF
        call iaea_destroy_source(i_unit_out,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_out
        END IF
        RETURN
      ELSE IF((i_iaea_in.EQ.1 .AND. IMUIDX1 .NE. IMUIDX2)) THEN
        WRITE(6,3870)
3870    FORMAT(/' Warning: One IAEA phase space file scores fractional M
     *U index,'/ ' while the other does not.  Combined file will lose th
     *is '/ ' information.'/)
      END IF
      WRITE(6,3880)
3880  FORMAT(/' The contents of file 1 will be appended to file 2.'/)
      WRITE(6,3890)
3890  FORMAT(/' BEGIN READING/WRITING PH-SP DATA .....'/)
      IPARANOT2=PARANOT2+1
      IF((i_iaea_in.EQ.0 .AND. MODE_RW1.EQ.'MODE2'))IZLAST1=1
      NHSTRY=0
      IF ((i_iaea_in.EQ.1)) THEN
        DO 3901 IPARANOT1=1,PARANOT1
          NPASSI=0
          call iaea_get_particle(i_unit_in,iaea_n_stat,iaea_q_index,ESHO
     *    RT,WT_PHSP_SHORT, X_PHSP_SHORT,Y_PHSP_SHORT,Z_PHSP_SHORT,U_PHS
     *    P_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,iaea_extra_floats,iaea_extr
     *    a_ints)
          IF ((iaea_n_stat.EQ.-1)) THEN
            WRITE(i_log,*)' Error getting particle data from IAEA phsp f
     *ile.'
            call exit(1)
          ELSE IF((iaea_n_stat.EQ.-2)) THEN
            WRITE(i_log,*)' WARNING: Reached end of IAEA phsp file.  Fil
     *e restarted.'
          ELSE IF((iaea_n_stat.GE.0)) THEN
            NHSTRY=NHSTRY+iaea_n_stat
            IF ((iaea_i_latch.EQ.-99)) THEN
              LATCHI=0
            ELSE
              LATCHI=iaea_extra_ints(iaea_i_latch)
            END IF
            IQ=iaea_typ_q(iaea_q_index)
            IF (( (IQ.EQ.1) .OR. (IQ.EQ.-1) )) THEN
              ESHORT = ESHORT + 0.5109989461
            END IF
            EREAD=ESHORT
            IF ((iaea_i_zlast.EQ.-99)) THEN
              ZLAST_PHSP_SHORT=0
            ELSE
              ZLAST_PHSP_SHORT= iaea_extra_floats(iaea_i_zlast)
            END IF
            IF ((iaea_i_muidx.EQ.-99)) THEN
              MUIDX_PHSP_SHORT=0
            ELSE
              MUIDX_PHSP_SHORT= iaea_extra_floats(iaea_i_muidx)
            END IF
            WEIGHT=WT_PHSP_SHORT
            XIN=X_PHSP_SHORT
            YIN=Y_PHSP_SHORT
            ZIN=Z_PHSP_SHORT
            UIN=U_PHSP_SHORT
            VIN=V_PHSP_SHORT
            WIN=W_PHSP_SHORT
            ZLAST=ZLAST_PHSP_SHORT
            MUIDX=MUIDX_PHSP_SHORT
          END IF
          IF ((NPASSI.EQ.0)) THEN
            ESHORT=EREAD
            iaea_n_stat=NHSTRY-IHSTRY_PHSP(1)
            IHSTRY_PHSP(1)=NHSTRY
            IF (( (IQ.EQ.1) .OR. (IQ.EQ.-1) )) THEN
              ESHORT = ESHORT - 0.5109989461
            END IF
            WT_PHSP_SHORT=WEIGHT
            X_PHSP_SHORT=XIN
            Y_PHSP_SHORT=YIN
            Z_PHSP_SHORT=ZIN
            U_PHSP_SHORT=UIN
            V_PHSP_SHORT=VIN
            W_PHSP_SHORT=WIN
            ZLAST_PHSP_SHORT=ZLAST
            MUIDX_PHSP_SHORT=MUIDX
            IF ((i_iaea_open_for_write.EQ.1)) THEN
              iaea_extra_ints(iaea_i_latch_out)=LATCHI
              IF((iaea_i_zlast_out.GT.0))iaea_extra_floats(iaea_i_zlast_
     *        out)=ZLAST_PHSP_SHORT
              IF((iaea_i_muidx_out.GT.0))iaea_extra_floats(iaea_i_muidx_
     *        out)=MUIDX_PHSP_SHORT
            ELSE
              iaea_extra_ints(iaea_i_latch)=LATCHI
              IF((iaea_i_zlast.GT.0))iaea_extra_floats(iaea_i_zlast)=ZLA
     *        ST_PHSP_SHORT
              IF((iaea_i_muidx.GT.0))iaea_extra_floats(iaea_i_muidx)=MUI
     *        DX_PHSP_SHORT
            END IF
            call iaea_write_particle(i_unit_out,iaea_n_stat,iaea_q_typ(I
     *      Q+2),ESHORT, WT_PHSP_SHORT,X_PHSP_SHORT,Y_PHSP_SHORT,Z_PHSP_
     *      SHORT,U_PHSP_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,iaea_extra_flo
     *      ats,iaea_extra_ints)
          END IF
3901    CONTINUE
3902    CONTINUE
        iaea_dummy_long=LPARANINC1+LPARANINC2
        call iaea_set_total_original_particles(i_unit_out,iaea_dummy_lon
     *  g)
        call iaea_destroy_source(i_unit_in,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_in
        END IF
        call iaea_destroy_source(i_unit_out,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit_out
        END IF
      ELSE
        PARANOP1=0
        DO 3911 IPARANOT1=2,PARANOT1+1
          IF ((IZLAST1.NE.0)) THEN
            READ(2,REC=IPARANOT1,IOSTAT=IERR_PHSP) LATCHI,ESHORT,X_PHSP_
     *      SHORT,Y_PHSP_SHORT, U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT,
     *      ZLAST_PHSP_SHORT
          ELSE
            READ(2,REC=IPARANOT1,IOSTAT=IERR_PHSP) LATCHI,ESHORT,X_PHSP_
     *      SHORT,Y_PHSP_SHORT, U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT
          END IF
          IF ((IPARANOT1.EQ.76695869)) THEN
            write(*,*)' x,y,u,v,wt,eshort,latch', X_PHSP_SHORT,Y_PHSP_SH
     *      ORT,U_PHSP_SHORT,V_PHSP_SHORT, WT_PHSP_SHORT,ESHORT,LATCHI
          END IF
          IF ((ESHORT.LT.0.0)) THEN
            NHSTRY=NHSTRY+1
            ESHORT=ABS(ESHORT)
          END IF
          EREAD=ESHORT
          WEIGHT=WT_PHSP_SHORT
          XIN=X_PHSP_SHORT
          YIN=Y_PHSP_SHORT
          UIN=U_PHSP_SHORT
          VIN=V_PHSP_SHORT
          IF((IZLAST1.NE.0))ZLAST=ZLAST_PHSP_SHORT
          IPARANOT2=IPARANOT2+1
          PARANOP1=PARANOP1+1-IBITS(LATCHI,29,1)-IBITS(LATCHI,30,1)
          IF ((IPARANOT2.GT.2 .AND. WRITE_PHSP_COUNTER(1).EQ.0 .AND. WRI
     *    TE_PHSP_SOFAR(1).EQ.0)) THEN
            IF (((IPARANOT2-1)-1000*((IPARANOT2-1)/1000).NE.0)) THEN
              CLOSE(3)
              IF ((IZLAST1.NE.0)) THEN
                PHSP_RECL=4*8
                OPEN(UNIT=3,FILE=SPCNAM,ACCESS='DIRECT',RECL=PHSP_RECL,
     *          FORM='UNFORMATTED',STATUS='UNKNOWN')
                DO 3921 I_PHSP=1,(IPARANOT2-1)-1000*((IPARANOT2-1)/1000)
                  READ(3,REC=1000*INT(dble(IPARANOT2-1)/1000)+I_PHSP) ST
     *            RING_TEMP_ZLAST_OUT(1)(32*(I_PHSP-1)+1:32*I_PHSP)
3921            CONTINUE
3922            CONTINUE
                CLOSE(3)
                PHSP_RECL=4*8000
                OPEN(UNIT=3,FILE=SPCNAM,ACCESS='DIRECT',RECL=PHSP_RECL,
     *          FORM='UNFORMATTED',STATUS='UNKNOWN')
              ELSE
                PHSP_RECL=4*7
                OPEN(UNIT=3,FILE=SPCNAM,ACCESS='DIRECT',RECL=PHSP_RECL,
     *          FORM='UNFORMATTED',STATUS='UNKNOWN')
                DO 3931 I_PHSP=1,(IPARANOT2-1)-1000*((IPARANOT2-1)/1000)
                  READ(3,REC=1000*INT(dble(IPARANOT2-1)/1000)+I_PHSP) ST
     *            RING_TEMP_OUT(1)(28*(I_PHSP-1)+1:28*I_PHSP)
3931            CONTINUE
3932            CONTINUE
                CLOSE(3)
                PHSP_RECL=4*7000
                OPEN(UNIT=3,FILE=SPCNAM,ACCESS='DIRECT',RECL=PHSP_RECL,
     *          FORM='UNFORMATTED',STATUS='UNKNOWN')
              END IF
            END IF
            WRITE_PHSP_SOFAR(1)=1000*INT(dble(IPARANOT2-1)/1000)
            IF ((WRITE_PHSP_SOFAR(1).GT.0)) THEN
              PHSP_RESTART=1
              WRITE_PHSP_COUNTER(1)=(IPARANOT2-1)-1000*((IPARANOT2-1)/10
     *        00)
            ELSE
              WRITE_PHSP_COUNTER(1)=(IPARANOT2-2)-1000*((IPARANOT2-2)/10
     *        00)
            END IF
          END IF
          IF ((NHSTRY.NE.IHSTRY_PHSP(1))) THEN
            EREAD=-EREAD
            IHSTRY_PHSP(1)=NHSTRY
          END IF
          LATCH_TEMP_OUT=LATCHI
          REAL_TEMP_OUT(1)=EREAD
          REAL_TEMP_OUT(2)=XIN
          REAL_TEMP_OUT(3)=YIN
          REAL_TEMP_OUT(4)=UIN
          REAL_TEMP_OUT(5)=VIN
          REAL_TEMP_OUT(6)=WEIGHT
          IF((IZLAST1.NE.0))REAL_TEMP_OUT(7)=ZLAST
          IF ((IPARANOT2.LE.1000 .AND. PHSP_RESTART.EQ.0)) THEN
            IF ((IZLAST1.NE.0)) THEN
              STRING_TEMP_ZLAST_OUT(1)(32*WRITE_PHSP_COUNTER(1)+33: 32*W
     *        RITE_PHSP_COUNTER(1)+64)=REC_TEMP_OUT(1:32)
            ELSE
              STRING_TEMP_OUT(1)(28*WRITE_PHSP_COUNTER(1)+29: 28*WRITE_P
     *        HSP_COUNTER(1)+56)=REC_TEMP_OUT(1:28)
            END IF
          ELSE
            IF ((IZLAST1.NE.0)) THEN
              STRING_TEMP_ZLAST_OUT(1)(32*WRITE_PHSP_COUNTER(1)+1: 32*WR
     *        ITE_PHSP_COUNTER(1)+32)=REC_TEMP_OUT(1:32)
            ELSE
              STRING_TEMP_OUT(1)(28*WRITE_PHSP_COUNTER(1)+1: 28*WRITE_PH
     *        SP_COUNTER(1)+28)=REC_TEMP_OUT(1:28)
            END IF
          END IF
          WRITE_PHSP_COUNTER(1)=WRITE_PHSP_COUNTER(1)+1
          IF ((IPARANOT2.EQ.1000.OR.WRITE_PHSP_COUNTER(1).EQ.1000)) THEN
            IF ((IZLAST1.NE.0 .AND. PHSP_RECL.EQ.4*8)) THEN
              CLOSE(3)
              PHSP_RECL_OLD=PHSP_RECL
              PHSP_RECL=4*8000
              OPEN(UNIT=3,FILE=SPCNAM,ACCESS='DIRECT',RECL=PHSP_RECL, FO
     *        RM='UNFORMATTED',STATUS='UNKNOWN')
            ELSE IF((IZLAST1.EQ.0 .AND. PHSP_RECL.EQ.4*7)) THEN
              PHSP_RECL_OLD=PHSP_RECL
              CLOSE(3)
              PHSP_RECL=4*7000
              OPEN(UNIT=3,FILE=SPCNAM,ACCESS='DIRECT',RECL=PHSP_RECL, FO
     *        RM='UNFORMATTED',STATUS='UNKNOWN')
            END IF
            IF ((IZLAST1.NE.0)) THEN
              WRITE(3,REC=INT(dble(IPARANOT2)/1000)) STRING_TEMP_ZLAST_O
     *        UT(1)
            ELSE
              WRITE(3,REC=INT(dble(IPARANOT2)/1000)) STRING_TEMP_OUT(1)
            END IF
            IF ((PHSP_RECL_OLD.NE.0)) THEN
              PHSP_RECL=PHSP_RECL_OLD
              CLOSE(3)
              OPEN(UNIT=3,FILE=SPCNAM,ACCESS='DIRECT',RECL=PHSP_RECL, FO
     *        RM='UNFORMATTED',STATUS='UNKNOWN')
            END IF
            WRITE_PHSP_COUNTER(1)=0
            WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+1000
          END IF
3911    CONTINUE
3912    CONTINUE
3940    CONTINUE
        TEMP_PHSP_COUNTER=0
        NUM_PHSP_TOFLUSH=WRITE_PHSP_COUNTER(1)
        IF ((NUM_PHSP_TOFLUSH.GT.0)) THEN
          IF ((WRITE_PHSP_SOFAR(1).EQ.0)) THEN
            WRITE_PHSP_COUNTER(1)=WRITE_PHSP_COUNTER(1)+1
            NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH+1
          END IF
3951      CONTINUE
            TEMP_PHSP_COUNTER=NUM_PHSP_TOFLUSH+1
3961        CONTINUE
              TEMP_PHSP_COUNTER=TEMP_PHSP_COUNTER-1
              REM_PHSP=MOD(WRITE_PHSP_SOFAR(1),TEMP_PHSP_COUNTER)
              IF(((REM_PHSP.EQ.0)))GO TO3962
            GO TO 3961
3962        CONTINUE
            CLOSE(3)
            IF ((IZLAST1.NE.0)) THEN
              PHSP_RECL=4*8*TEMP_PHSP_COUNTER
              OPEN(UNIT=3,FILE=SPCNAM,RECL=PHSP_RECL, ACCESS='DIRECT',FO
     *        RM='UNFORMATTED',STATUS='UNKNOWN')
              WRITE(3,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNTER+1) STRIN
     *        G_TEMP_ZLAST_OUT(1) (32*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TO
     *        FLUSH)+1: 32*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TOFLUSH+TEMP_
     *        PHSP_COUNTER))
            ELSE
              PHSP_RECL=4*7*TEMP_PHSP_COUNTER
              OPEN(UNIT=3,FILE=SPCNAM,RECL=PHSP_RECL, ACCESS='DIRECT',FO
     *        RM='UNFORMATTED',STATUS='UNKNOWN')
              WRITE(3,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNTER+1) STRIN
     *        G_TEMP_OUT(1) (28*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TOFLUSH)
     *        +1: 28*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TOFLUSH+TEMP_PHSP_C
     *        OUNTER))
            END IF
            WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+TEMP_PHSP_COUNTER
            NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH-TEMP_PHSP_COUNTER
            IF(((NUM_PHSP_TOFLUSH.EQ.0)))GO TO3952
          GO TO 3951
3952      CONTINUE
          WRITE_PHSP_COUNTER(1)=0
          WRITE_PHSP_SOFAR(1)=0
        END IF
        PARANOT2 = PARANOT2 + PARANOT1
        PARANOP2 = PARANOP2 + PARANOP1
        IF ((PARANOT1.GT.0 .AND. PARAEMAX2 .LT. PARAEMAX1)) THEN
          PARAEMAX2=PARAEMAX1
        END IF
        IF ((PARANOT1.GT.0 .AND. PARAEMNE2 .GT. PARAEMNE1)) THEN
          PARAEMNE2=PARAEMNE1
        END IF
        NINC = (PARANOT1/PARANOT)*PARANINC1 + PARANINC2
        IF (((PARANINC1 .LT. 2.).OR.(PARANINC2 .LT. 2.))) THEN
          WRITE(6,3970)
3970      FORMAT(/' FILE 1 AND/OR 2 IS AN OLDER FILE WHICH STORED',/ ' M
     *IN. PHOTON ENERGY INSTEAD OF # OF INCIDENT PARTICLES',/ ' NUMBER O
     *F INCIDENT PARTICLES SET TO 1 IN COMBINED FILE')
          NINC = 1
        END IF
        CLOSE(3)
        IF ((IZLAST1.NE.0)) THEN
          PHSP_RECL=4*8
        ELSE
          PHSP_RECL=4*7
        END IF
        OPEN(UNIT=3,FILE=SPCNAM,RECL=PHSP_RECL,ACCESS='DIRECT', FORM='UN
     *FORMATTED',STATUS='UNKNOWN')
        IF ((PARANOT2.GT.2147483647)) THEN
          write(*,*)' Warning while writing phase space file:'
          write(*,*)' No. of particles > 2^31-1.'
          write(*,*)' Total no. of particles (and photons) written'
          write(*,*)' to header may be nonsense.'
        END IF
        NUM_PHSP_TOT=PARANOT2
        PHOT_PHSP_TOT=PARANOP2
        EKMAX_PHSP_SHORT=PARAEMAX2
        EKMINE_PHSP_SHORT=PARAEMNE2
        NINC_PHSP_SHORT=NINC
        WRITE(3,REC=1)MODE_RW2,NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_PHSP_SHO
     *  RT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
        CLOSE(3)
        IF ((IZLAST1.NE.0)) THEN
          PHSP_RECL=4*8000
        ELSE
          PHSP_RECL=4*7000
        END IF
        OPEN(UNIT=3,FILE=SPCNAM,RECL=PHSP_RECL,ACCESS='DIRECT', FORM='UN
     *FORMATTED',STATUS='UNKNOWN')
        CLOSE(UNIT=2)
        CLOSE(UNIT=3)
      END IF
      WRITE(6,3980)
3980  FORMAT(/' Finished reading/writing ph-sp data .....'/)
      RETURN
3790  CONTINUE
      WRITE(6,3990)
3990  FORMAT(//' CANNOT FIND/OPEN THE PH-SP FILE!!!'///)
      CALL HELP_MESSAGE(22)
      GOTO 3430
3390  WRITE(6,4000)
4000  FORMAT(//' CANNOT FIND/OPEN THE PH-SP FILE!!!'///)
      GOTO 3050
      END
      SUBROUTINE BEAMDP1
      IMPLICIT NONE
      REAL TENERGY,TPARTICLE,TEEE
      REAL*8 TPPP
      LOGICAL RMINUNDEFINED
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      COMMON/DOUBLES/ KWEI,SMFRTN, FSMFBN1,FSMABN1,FSMABN2,FSMFXX,FSMFYY
     *,FSMFR1,FSMFR2,FSMFR11,FSMFR12, FSMFRR,FSMFXY,BINSIZE,RBin,RBin_X,
     *RBin_Y
      REAL*8 KWEI, SMFRTN(22),  FSMFBN1(22,200,200), FSMABN1(200), FSMAB
     *N2(200), FSMFXX(22,200), FSMFYY(22,200), FSMFR1(22,200), FSMFR2(22
     *,200), FSMFR11(22,200), FSMFR12(22,200), FSMFRR(22,200), FSMFXY(22
     *,200,200), BINSIZE(200), RBin,RBin_X,RBin_Y
      COMMON/REALS/ XMIN2,XMAX2, XPLOT,HXMIN, YPLOT,YPLOTT, ERRYPLOT,X2Y
     *2,ZTOTAL,RSSD02,RSSD12,SSDVPS, SFACTOR,RFACTOR,RFACTOR1,RFACTOR2,
     *X,Y,Z,RRFACTOR,SSFACTOR,RSSD0,RSSD1, WEIGHT,EI, PARAEMAX2,PARAEMAX
     *1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAEMNE, PARANINC1,PARANINC2,PARA
     *NINC, EREAD,ZLAST, MUIDX,XIN,YIN,ZIN,UIN,VIN,WIN, SMFX2,SMFY2,SMFM
     *XR, HFIELD, RSCATT, DELTAZ,RSSDST,SSSDST,ERADII,E_ph_sp_e, E_ph_sp
     *_g, E_ph_sp_p, E_MIN_ph_sp, E_MAX_ph_sp, RFIELD,RFIELDe,XFLDMN,XFL
     *DMX, YFLDMN, YFLDMX, SMFMNZ, SMFMXZ, SMFMNX, SMFMXX, SMFMNY, SMFMX
     *Y, SMFMNR,RTREAT,RTREATe,XTREATMN,XTREATMX, YTREATMN,YTREATMX, SMF
     *MNE,SMFMXE, ABNTOTAL,ABNTOTALH,FLUAREA,NINC, RMIN_MIN,RMIN_MAX,RMA
     *X_MIN,RMAX_MAX,DOMEGA, SUM_TMP,SUM_TMP2,Z_SCORE,mumin,mumax
      real*8 XMIN2,XMAX2,  XPLOT(200),HXMIN,  YPLOT(200),YPLOTT, ERRYPLO
     *T(200), X2Y2,  ZTOTAL(22),  RSSD02,RSSD12,  SSDVPS(22+3),  SFACTOR
     *,RFACTOR,RFACTOR1,RFACTOR2,X,Y,Z,RRFACTOR,SSFACTOR, RSSD0,RSSD1,
     *WEIGHT,EI, PARAEMAX2,PARAEMAX1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAE
     *MNE, PARANINC1,PARANINC2,PARANINC, EREAD,ZLAST,MUIDX, XIN,YIN,ZIN,
     *UIN, VIN,WIN,  SMFX2(22), SMFY2(22),  SMFMXR(22),  HFIELD(22),  RS
     *CATT,  DELTAZ,  RSSDST(22,2000), SSSDST(22,2000), ERADII(200),  E_
     *ph_sp_e,  E_ph_sp_g,  E_ph_sp_p,  E_MIN_ph_sp,  E_MAX_ph_sp,  RFIE
     *LD,  RFIELDe,   XFLDMN,  XFLDMX,  YFLDMN,  YFLDMX,  SMFMNZ(22),
     *SMFMXZ(22),  SMFMNX(22),  SMFMXX(22),  SMFMNY(22),  SMFMXY(22),  S
     *MFMNR(22),  RTREAT,  RTREATe,   XTREATMN,  XTREATMX,  YTREATMN,  Y
     *TREATMX,  SMFMNE,SMFMXE,  ABNTOTAL,ABNTOTALH,  FLUAREA,   NINC,  R
     *MIN_MIN, RMIN_MAX,  RMAX_MIN,  RMAX_MAX, DOMEGA,  SUM_TMP(200),  S
     *UM_TMP2(200),  Z_SCORE,  mumin,mumax
      COMMON/INTEGERS/ PARANOT,PARANOT1,PARANOT2,PARANOP,PARANOP1, PARAN
     *OP2,IPARANOT,IPARANOT1,IPARANOT2, LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST, I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBER
     *ST,NBIT1,NBIT2, NFIELDi,NFIELDo,NFIELDe, NWRONG,LATCHJ,CHOICE,CHOI
     *CE1, K1,K2,K3,K4,IA, IX,IY,IR,K,IQ,LATCHI,IE, NPASSI,LATCH,SMFPXY,
     *NFIELD,NSMFPS,I_IN_EX, ISMFPS,IQSMFP,MSTART,MSMFXY,NSMFEE,NPASS_ph
     *_sp, N_ph_sp_e, N_ph_sp_g, N_ph_sp_p, FLUTYPE,ANGTYPE,IZLAST1, IZL
     *AST2,IMUIDX1,IMUIDX2,IZSCORE1,IZSCORE2,JUSTONE, MAXSCATTER,NUMSCAT
     *TER,ESTYPE,i_iaea_in,i_iaea_out,i_log, i_unit_in,i_unit_out
      INTEGER*8 PARANOT,PARANOT1,PARANOT2, PARANOP,PARANOP1,  PARANOP2,I
     *PARANOT,IPARANOT1,IPARANOT2,  LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST(200)
      INTEGER I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBERST,NBIT1,NBI
     *T2,  NWRONG,LATCHJ(29), CHOICE,CHOICE1,  K1,K2,K3,K4,IA, IX,IY,IR,
     *K(22), IQ,LATCHI,IE, NPASSI,  LATCH(22),   SMFPXY(22),   NFIELD,
     * NFIELDi,   NFIELDo,   NFIELDe,  NSMFPS,  I_IN_EX,  ISMFPS(22),
     *IQSMFP(22),   MSTART,   MSMFXY,  NSMFEE,  NPASS_ph_sp,  N_ph_sp_e,
     *  N_ph_sp_g,  N_ph_sp_p,  FLUTYPE,  ANGTYPE,  IZLAST1,  IZLAST2,
     *IMUIDX1,   IMUIDX2,  IZSCORE1,  IZSCORE2,  JUSTONE(200),  MAXSCATT
     *ER,  NUMSCATTER,  ESTYPE ,  i_iaea_in,  i_iaea_out,   i_log,   i_u
     *nit_in,i_unit_out
      COMMON/LOGICALS/OLDFILE,EGSPERT
      LOGICAL OLDFILE,    EGSPERT
      LETTER1(1)='-1'
      LETTER1(2)=' 0'
      LETTER1(3)=' 1'
      LETTER1(4)='2'
      LETTER1(5)=' 3'
      LETTER2(0)=' 0'
      LETTER2(1)=' 1'
      LETTER2(2)=' 2'
      LETTER2(3)=' 3'
      LETTER2(4)=' 4'
      LETTER2(5)=' 5'
      LETTER2(6)=' 6'
      LETTER2(7)=' 7'
      LETTER2(8)=' 8'
      LETTER2(9)=' 9'
      LETTER2(10)='10'
      LETTER2(11)='11'
      LETTER2(12)='12'
      LETTER2(13)='13'
      LETTER2(14)='14'
      LETTER2(15)='15'
      LETTER2(16)='16'
      LETTER2(17)='17'
      LETTER2(18)='18'
      LETTER2(19)='19'
      LETTER2(20)='20'
      LETTER2(21)='21'
      LETTER2(22)='22'
      LETTER2(23)='23'
      LETTER2(24)='24'
      LETTER2(25)='25'
      LETTER2(26)='26'
      LETTER2(27)='27'
      LETTER2(28)='28'
      LETTER2(29)='29'
      LETTER2(30)='30'
      LETTER2(31)='31'
      LETTER2(32)='32'
      LETTER2(33)='33'
      LETTER2(34)='34'
      LETTER2(35)='35'
      LETTER2(36)='36'
      LETTER2(37)='37'
      LETTER2(38)='38'
      LETTER2(39)='39'
      LETTER2(40)='40'
      LETTER2(41)='41'
      LETTER2(42)='42'
      LETTER2(43)='43'
      LETTER2(44)='44'
      LETTER2(45)='45'
      LETTER2(46)='46'
      LETTER2(47)='47'
      LETTER2(48)='48'
      LETTER2(49)='49'
      LETTER2(50)='50'
      LETTER2(51)='51'
      LETTER2(52)='52'
      LETTER2(53)='53'
      LETTER2(54)='54'
      LETTER2(55)='55'
      LETTER2(56)='56'
      LETTER2(57)='57'
      LETTER2(58)='58'
      LETTER2(59)='59'
      LETTER2(60)='60'
      LETTER2(61)='61'
      LETTER2(62)='62'
      LETTER2(63)='63'
      LETTER2(64)='64'
      LETTER2(65)='65'
      LETTER2(66)='66'
      LETTER2(67)='67'
      LETTER2(68)='68'
      LETTER2(69)='69'
      LETTER2(70)='70'
      LETTER2(71)='71'
      LETTER2(72)='72'
      LETTER2(73)='73'
      LETTER2(74)='74'
      LETTER2(75)='75'
      LETTER2(76)='76'
      LETTER2(77)='77'
      LETTER2(78)='78'
      LETTER2(79)='79'
      LETTER2(80)='80'
      LETTER2(81)='81'
      LETTER2(82)='82'
      LETTER2(83)='83'
      LETTER2(84)='84'
      LETTER2(85)='85'
      LETTER2(86)='86'
      LETTER2(87)='87'
      LETTER2(88)='88'
      LETTER2(89)='89'
      LETTER2(90)='90'
      LETTER2(91)='91'
      LETTER2(92)='92'
      LETTER2(93)='93'
      LETTER2(94)='94'
      LETTER2(95)='95'
      LETTER2(96)='96'
      LETTER2(97)='97'
      LETTER2(98)='98'
      LETTER2(99)='99'
      LETTER2(100)=' 0'
      LETTER2(101)='  '
20010 CONTINUE
      IF ((CHOICE1 .EQ. 0)) THEN
        WRITE(6,20020)
20020   FORMAT(/' INPUT A NUMBER TO CHOOSE AN OPTION: '/)
        WRITE(6,20030)
20030   FORMAT(' (0) - Input new sub-source specifiers & analyze ph-sp d
     *ata')
        WRITE(6,20040)
20040   FORMAT(' (1) - Change sub-source specifiers in an existing sourc
     *e model')
        WRITE(6,20050)
20050   FORMAT(' (2) - Analyze ph-sp data using sub-source specifiers fr
     *om an'/ '       existing source model')
        WRITE(6,20060)
20060   FORMAT(' (3) - Plot characteristics of source model')
        WRITE(6,20070)
20070   FORMAT(' (4) - QUIT')
        WRITE(6,20080)
20080   FORMAT(' ',$)
        READ(5,20090)CHOICE
20090   FORMAT(I8)
        IF((CHOICE .EQ. 4))RETURN
        IF ((CHOICE.GT.4).OR.(CHOICE.LT.0)) THEN
          WRITE(6,20100)
20100     FORMAT(//' SORRY, NO SUCH OPTIONS!'/)
          RETURN
        END IF
      END IF
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(59)
      END IF
      RSSD1=0.0
      K1=0
      K2=0
      K3=0
      K4=0
      KWEI=0.
      DELTAZ=25.
      SSMFPS=' '
      OLDFILE= .true.
      IF ((CHOICE.EQ.0)) THEN
        OLDFILE=.false.
        IF ((.NOT.EGSPERT)) THEN
          CALL HELP_MESSAGE(60)
        END IF
20110   CONTINUE
        WRITE(6,20120)
20120   FORMAT(/ ' Would you like to use sub-source specifiers from an e
     *xisting source model'/ ' as a reference? (y/n[Default])=> ',$)
        READ(5,20130)C1
20130   FORMAT(a1)
        IF ((C1.EQ.'?')) THEN
          CALL HELP_MESSAGE(30)
          GOTO 20110
        END IF
        IF((C1.EQ.'y' .OR. C1.EQ.'Y'))OLDFILE=.true.
      END IF
      IF ((OLDFILE)) THEN
        IF (.NOT.EGSPERT) THEN
          IF ((CHOICE.EQ.0)) THEN
            CALL HELP_MESSAGE(55)
          ELSE IF((CHOICE.EQ.1)) THEN
            CALL HELP_MESSAGE(56)
          ELSE IF((CHOICE.EQ.2)) THEN
            CALL HELP_MESSAGE(57)
          ELSE IF((CHOICE.EQ.3)) THEN
            CALL HELP_MESSAGE(58)
          END IF
        END IF
20140   IF ((CHOICE.EQ.0)) THEN
          WRITE(6,20150)
20150     FORMAT(/' Source model containing sub-source specifiers to use
     * as reference=> '/ ' ',$)
        ELSE IF((CHOICE.EQ.1)) THEN
          WRITE(6,20160)
20160     FORMAT(/' Source model containing sub-source specifiers to bem
     *odified=> '/ ' ',$)
        ELSE IF((CHOICE.EQ.2)) THEN
          WRITE(6,20170)
20170     FORMAT(/' Source model containing sub-source specifiers for an
     *alyzing phsp data=>'/ ' ',$)
        ELSE IF((CHOICE.EQ.3)) THEN
          WRITE(6,20180)
20180     FORMAT(/' Source model to plot=>'/' ',$)
        END IF
        READ(5,20190)OLDNAM
20190   FORMAT(A80)
        IF ((OLDNAM.EQ.'?')) THEN
          CALL HELP_MESSAGE(49)
          GOTO 20140
        END IF
        CLOSE(UNIT=3,STATUS='KEEP')
        OPEN(UNIT=3,STATUS='OLD',FILE=OLDNAM,ERR=20200)
        READ(3,'(A80)',ERR=20200)SSMFPS
        READ(3,*,ERR=20200)NSMFPS
        DO 20211 I=1,NSMFPS
          READ(3,*,ERR=20200)ISMFPS(I),IQSMFP(I),LATCH(I)
          IF ((CHOICE.EQ.2 .AND. LATCH(I).GT.22)) THEN
            WRITE(6,20220)I,ISMFPS(I),IQSMFP(I),LATCH(I),22
20220       FORMAT(//' ***WARNING IN SUB-SOURCE ',I3,' (type=',I3,',char
     *ge=',I3,'):'/ ' LATCH bit associated with source = ',I3/ ' LATCH b
     *it should be <=',I3/ ' Bit will be selected anyway.'//)
          END IF
          IF ((ISMFPS(I) .EQ. 1)) THEN
            READ(3,*,ERR=20200)SMFMNZ(I),SMFMNX(I),SMFMXX(I), SMFMNY(I),
     *      SMFMXY(I),SMFX2(I),SMFY2(I)
          ELSE IF((ISMFPS(I) .EQ. 11)) THEN
            READ(3,*,ERR=20200)SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMFMXX(I),
     *      SMFMNY(I),SMFMXY(I),SMFX2(I),SMFY2(I)
          ELSE IF((ISMFPS(I) .EQ. 2)) THEN
            READ(3,*,ERR=20200)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),
     *      SMFMXY(I),SMFX2(I),SMFY2(I),SMFPXY(I)
          ELSE IF((ISMFPS(I) .EQ. 3)) THEN
            READ(3,*,ERR=20200)SMFMNZ(I),SMFMNR(I),SMFMXR(I),RSSD0
          ELSE IF((ISMFPS(I) .EQ. 4)) THEN
            READ(3,*,ERR=20200)SMFMNZ(I),SMFMNX(I),SMFMXX(I), SMFMNY(I),
     *      SMFMXY(I)
          ELSE
            READ(3,*,ERR=20200)SMFMNZ(I),SMFMNR(I)
          END IF
20211   CONTINUE
20212   CONTINUE
        DO 20231 I=1,1
          READ(3,*,ERR=20200)NSMFEE,SMFMNE,SMFMXE
          READ(3,*,ERR=20200)ESTYPE
          IF ((ESTYPE .EQ. 0)) THEN
            READ(3,*,ERR=20200)RTREATe,RFIELDe
            NFIELDe=2
            ERADII(1) = RTREATe
            ERADII(2) = RFIELDe
          ELSE
            READ(3,*,ERR=20200)NFIELDe,RFIELDe
            READ(3,*,ERR=20200)(ERADII(II),II=1,NFIELDe)
          END IF
          READ(3,*,ERR=20200)MSMFXY
          IF ((MSMFXY .EQ. 0)) THEN
            READ(3,*,ERR=20200)NFIELDi,RTREAT,NFIELDo,RFIELD
          ELSE IF((MSMFXY .EQ. 1)) THEN
            READ(3,*,ERR=20200)NFIELDi,RTREAT,NFIELDo,RFIELD
          ELSE IF((MSMFXY .EQ. 2)) THEN
            READ(3,*,ERR=20200)NFIELD,XFLDMN,XFLDMX,YFLDMN,YFLDMX
            READ(3,*,ERR=20200)XTREATMN,XTREATMX,YTREATMN,YTREATMX
          END IF
          READ(3,'(A100)',ERR=20200)PSDNAM
20231   CONTINUE
20232   CONTINUE
        GOTO 20240
20200   CALL HELP_MESSAGE(49)
        GOTO 20140
20240   CONTINUE
      END IF
      IF ((CHOICE.EQ.1).OR.(CHOICE.EQ.3)) THEN
        DO 20251 I=1,NSMFPS
          READ(3,*,ERR=20260)II
          IF ((I .NE. II)) THEN
            GOTO 20270
          END IF
          READ(3,*,ERR=20260)ISMFPS(I),IQSMFP(I),LATCH(I)
          IF ((ISMFPS(I) .EQ. 1)) THEN
            READ(3,*,ERR=20260)SMFMNZ(I),SMFMNX(I),SMFMXX(I), SMFMNY(I),
     *      SMFMXY(I),SMFX2(I),SMFY2(I)
          ELSE IF((ISMFPS(I) .EQ. 11)) THEN
            READ(3,*,ERR=20260)SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMFMXX(I),
     *      SMFMNY(I),SMFMXY(I),SMFX2(I),SMFY2(I)
          ELSE IF((ISMFPS(I) .EQ. 2)) THEN
            READ(3,*,ERR=20260)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),
     *      SMFMXY(I),SMFX2(I),SMFY2(I),SMFPXY(I)
          ELSE IF((ISMFPS(I) .EQ. 3)) THEN
            READ(3,*,ERR=20260)SMFMNZ(I),SMFMNR(I),SMFMXR(I),RSSD0
          ELSE IF((ISMFPS(I) .EQ. 4)) THEN
            READ(3,*,ERR=20260)SMFMNZ(I),SMFMNX(I),SMFMXX(I), SMFMNY(I),
     *      SMFMXY(I)
          ELSE
            READ(3,*,ERR=20260)SMFMNZ(I),SMFMNR(I)
          END IF
          READ(3,*,ERR=20260)SMFRTN(I)
          READ(3,*,ERR=20260)NSMFEE,SMFMNE,SMFMXE
          DO 20281 III=1,NFIELDe
            READ(3,*,ERR=20260) (FSMFBN1(I,III,II),II=1,NSMFEE)
20281     CONTINUE
20282     CONTINUE
          READ(3,*,ERR=20260)MSMFXY
          IF ((MSMFXY .EQ. 0)) THEN
            READ(3,*,ERR=20260)NFIELDi,RTREAT,NFIELDo,RFIELD
            NFIELD = NFIELDi + NFIELDo
            READ(3,*,ERR=20260)(FSMFRR(I,II),II=1,NFIELD)
          ELSE IF((MSMFXY .EQ. 1)) THEN
            READ(3,*,ERR=20260)NFIELDi,RTREAT,NFIELDo,RFIELD
            NFIELD = NFIELDi + NFIELDo
            READ(3,*,ERR=20260)(FSMFRR(I,II),II=1,NFIELD)
            READ(3,*)(FSMFR11(I,II),II=1,NFIELD)
            READ(3,*)(FSMFR12(I,II),II=1,NFIELD)
          ELSE IF((MSMFXY .EQ. 2)) THEN
            READ(3,*,ERR=20260)NFIELD,XFLDMN,XFLDMX
            READ(3,*,ERR=20260)NFIELD,YFLDMN,YFLDMX
            READ(3,*,ERR=20260)((FSMFXY(I,II,III),II=1,NFIELD),III=1,NFI
     *      ELD)
          END IF
20251   CONTINUE
20252   CONTINUE
        READ(3,*,ERR=20260) (FSMABN1(II),II=1,200)
        ABNTOTAL=0.
        DO 20291 II=1,200
          ABNTOTAL=ABNTOTAL+FSMABN1(II)
20291   CONTINUE
20292   CONTINUE
        ABNTOTALH=0.
        DO 20301 II=1,200
          ABNTOTALH=ABNTOTALH+FSMABN1(II)
          IF (((ABNTOTAL .NE. 0.) .AND. (ABNTOTALH/ABNTOTAL .GE. 0.5)))
     *    THEN
            RSCATT=100.*TAN(FLOAT(II)*0.1745/FLOAT(200))
            IF (((RSCATT .LE. 0.) .AND. (RSCATT .GT. 50.))) THEN
              RSCATT=5.
            END IF
            GOTO 20310
          END IF
20301   CONTINUE
20302   CONTINUE
20310   CONTINUE
        GOTO 20320
20270   WRITE(6,20330)
20330   FORMAT(//' SUB-SOURCES ARE NOT IN THE RIGHT ORDER, WHY?'//)
        GOTO 20320
20260   WRITE(6,20340)
20340   FORMAT(//' ENERGY OR PLANAR FLUENCE MAY NOT BE CORRECT!'//)
20320   CLOSE(UNIT=3,STATUS='KEEP')
        WRITE(6,20350)
20350   FORMAT(//' WARNING: data contained in the file may not be righti
     *f it was'/ '          created before the execution of the programm
     *e BEAMDP'/ '          was completed!!!'/)
        IF((CHOICE.EQ.3))GOTO 20360
      END IF
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(54)
      END IF
20370 WRITE(6,20380)
20380 FORMAT(/' File name for the new/modified source model:'/' ',$)
      READ(5,20390)SPCNAM
20390 FORMAT(A80)
      IF ((SPCNAM.EQ.'?')) THEN
        CALL HELP_MESSAGE(31)
        GO TO 20370
      END IF
      IF ((SPCNAM.EQ.' ')) THEN
        IF ((OLDNAM.EQ.' ')) THEN
          OLDNAM='beamdp.tmp'
          WRITE(6,20400)
20400     FORMAT(/' FILE NAME DEFAULT TO [beamdp.tmp]'/)
        END IF
        SPCNAM=OLDNAM
      END IF
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(32)
      END IF
20410 WRITE(6,20420)
20420 FORMAT(/' Detailed information about the source: ')
      IF ((SSMFPS .NE. ' ')) THEN
        WRITE(6,20430)SSMFPS
20430   FORMAT(/' THE OLD INFORMATION WAS:'/' ', A80/ ' (Return to keept
     *he information, otherwise type in the new one):'/)
      END IF
      READ(5,20440)DSMFPS
20440 FORMAT(A80)
      IF ((DSMFPS.EQ.'?')) THEN
        CALL HELP_MESSAGE(32)
        GO TO 20410
      END IF
      IF ((DSMFPS .NE. ' ')) THEN
        SSMFPS=DSMFPS
      ELSE IF((SSMFPS .EQ. ' ')) THEN
        WRITE(6,20450)
20450   FORMAT(/' INFORMATION ABOUT THE SIMULATED BEAM (to be filled lat
     *er)'/)
        SSMFPS='to-be-filled-later'
      END IF
      IF ((CHOICE .EQ. 2)) THEN
        RSSD1=RSSD0
        GOTO 20460
      END IF
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(33)
      END IF
20470 WRITE(6,20480)
20480 FORMAT(/' Number of sub-sources=> ',$)
      IF (OLDFILE) THEN
        WRITE(6,20490)NSMFPS
20490   FORMAT(/' The old value was',I4,'. You cannot change this value.
     *'/)
      ELSE
        READ(5,20500)C2
20500   FORMAT(A2)
        IF ((C2.EQ.'? ')) THEN
          CALL HELP_MESSAGE(33)
          GO TO 20470
        END IF
        READ(C2,'(I8)')NSMFPS
        IF ((NSMFPS .LT. 1).OR.(NSMFPS .GT. 22)) THEN
          CALL HELP_MESSAGE(33)
          GO TO 20470
        END IF
      END IF
      DO 20511 I=1,NSMFPS
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(34)
        END IF
20520   WRITE(6,20530)I
20530   FORMAT(/' INPUT PARAMETERS FOR SUB-SOURCE',I4,':'// ' SOURCE TYP
     *E(1&11-appl,2-coll,3-ring,4-rect.plane,5-circ.plane),'/ ' CHARGE O
     *F PARTICLES (0-photons,-1-electrons,1-positrons),'/ ' LATCH NUMBER
     * FOR THE SUB-SOURCE DURING BEAM SIMULATION'/' ',$)
        IF (OLDFILE) THEN
          WRITE(6,20540)ISMFPS(I),IQSMFP(I),LATCH(I)
20540     FORMAT(/' The old values were',3I4,'.'/' Do you want to change
     * them?'/' ',$)
          READ(5,20550)C1
20550     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 20560
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,20570)I
20570       FORMAT(//' INPUT PARAMETERS FOR SUB-SOURCE:',I4/// ' SOURCET
     *YPE (1&11-appl,2-coll,3-ring,4-rect.plane,5-circ.plane),'/ ' CHARG
     *E OF PARTICLES (0-photons,-1-electrons,1-positrons), AND'/ ' LATCH
     * NUMBER FOR THE SUB-SOURCE DURING BEAM SIMULATION'/)
          ELSE
            GOTO 20580
          END IF
        END IF
        READ(5,'(3I15)',ERR=20560) ISMFPS(I),IQSMFP(I),LATCH(I)
        GOTO 20580
20560   CALL HELP_MESSAGE(34)
        GOTO 20520
20580   CONTINUE
        IF (((ISMFPS(I).LT.1).OR.(ISMFPS(I).GT.11).OR.(IQSMFP(I).LT.-1)
     *  .OR.(IQSMFP(I).GT.1))) THEN
          WRITE(6,20590)
20590     FORMAT(/' SORRY, OUT OF RANGE!!!'//)
          GO TO 20560
        END IF
        IF ((LATCH(I) .GT. 22)) THEN
          WRITE(6,20600)22
20600     FORMAT(///' SORRY, LATCH BIT MUST BE <=',I3,'!!!'//)
          GOTO 20560
        END IF
        IF ((ISMFPS(I).EQ.1)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(35)
          END IF
20610     WRITE(6,20620)
20620     FORMAT(/' Zmin, X-, X+, Y-, Y+, |x|max, |y|max (in cm) OF THES
     *OURCE:'/ ' ',$)
          IF (OLDFILE) THEN
            WRITE(6,20630)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY
     *      (I), SMFX2(I),SMFY2(I)
20630       FORMAT(/' The old values were',7F10.3,'.'/ ' Do you want toc
     *hange them?'/' ',$)
            READ(5,20640)C1
20640       FORMAT(A1)
            IF ((C1.EQ.'?')) THEN
              GO TO 20650
            END IF
            IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
              WRITE(6,20660)
20660         FORMAT(/' Zmin, X-, X+, Y-, Y+, |x|max, |y|max (in cm) OFT
     *HE SOURCE:' /' ',$)
            ELSE
              GOTO 20670
            END IF
          END IF
          READ(5,'(7F15.0)',ERR=20650) SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMF
     *    MNY(I),SMFMXY(I),SMFX2(I),SMFY2(I)
          GOTO 20670
20650     CALL HELP_MESSAGE(35)
          GO TO 20610
20670     CONTINUE
          IF ((SMFMNZ(I).LT.0.).OR.(SMFMNX(I).GE. SMFMXX(I)).OR. (SMFMNY
     *    (I).GE. SMFMXY(I))) THEN
            WRITE(6,20680)
20680       FORMAT(//' SORRY, Zmin MUST BE >= 0.0 cm')
            WRITE(6,20690)
20690       FORMAT('        X+ MUST BE > X-')
            WRITE(6,20700)
20700       FORMAT('        Y+ MUST BE > Y-'//)
            GO TO 20610
          END IF
          HFIELD(I)=(SMFMXX(I)-SMFMNX(I)+SMFMXY(I)-SMFMNY(I))*1.1/4.
        ELSE IF((ISMFPS(I).EQ.11)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(36)
          END IF
20710     WRITE(6,20720)
20720     FORMAT(/' Zmin,Zmax,X-,X+,Y-,Y+,|x|max,|y|max (in cm) OF THE S
     *OURCE:'/)
          IF (OLDFILE) THEN
            WRITE(6,20730)SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMFMXX(I),SMFMNY
     *      (I),SMFMXY(I), SMFX2(I),SMFY2(I)
20730       FORMAT(/' The old values were',8F10.3,'.'/ ' Do you want toc
     *hange them?'/' ',$)
            READ(5,20740)C1
20740       FORMAT(A1)
            IF ((C1.EQ.'?')) THEN
              GO TO 20750
            END IF
            IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
              WRITE(6,20760)
20760         FORMAT(/' Zmin,Zmax,X-,X+,Y-,Y+,|x|max,|y|max (in cm) OF T
     *HE SOURCE:'/)
            ELSE
              GOTO 20770
            END IF
          END IF
          READ(5,'(7F15.0)',ERR=20750) SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMF
     *    MXX(I),SMFMNY(I),SMFMXY(I), SMFX2(I),SMFY2(I)
          GOTO 20770
20750     CALL HELP_MESSAGE(36)
          GOTO 20710
20770     CONTINUE
          IF ((SMFMNZ(I).LT.0.).OR.(SMFMNZ(I).GT.SMFMXZ(I)).OR.(SMFMNX(I
     *    ).GE. SMFMXX(I)).OR. (SMFMNY(I).GE. SMFMXY(I))) THEN
            WRITE(6,20780)
20780       FORMAT(//' SORRY, Zmax MUST BE >= Zmin >=0.0 cm')
            WRITE(6,20790)
20790       FORMAT('        X+ MUST BE > X-')
            WRITE(6,20800)
20800       FORMAT('        Y+ MUST BE > Y-'//)
            GO TO 20710
          END IF
          HFIELD(I)=(SMFMXX(I)-SMFMNX(I)+SMFMXY(I)-SMFMNY(I))*1.1/4.
        ELSE IF((ISMFPS(I).EQ.2)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(37)
          END IF
20810     WRITE(6,20820)
20820     FORMAT(/' Zmin,X-,X+,Y-,Y+,|x|max,|y|max(in cm),Orientation OF
     * THE SOURCE'/ ' (0-collimator bars along x-axis,1-along y-axis):'/
     *' ',$)
          IF (OLDFILE) THEN
            WRITE(6,20830)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY
     *      (I), SMFX2(I),SMFY2(I),SMFPXY(I)
20830       FORMAT(/' The old values were',7F10.3,I4,'.'/ ' Do you wantt
     *o change them?'/' ',$)
            READ(5,20840)C1
20840       FORMAT(A1)
            IF ((C1.EQ.'?')) THEN
              GO TO 20850
            END IF
            IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
              WRITE(6,20860)
20860         FORMAT(/' Zmin,X-,X+,Y-,Y+,|x|max,|y|max(in cm),Orientatio
     *n OF THE SOURCE'/ '  (0-collimator bars along x-axis,1-along y-axi
     *s):'/' ',$)
            ELSE
              GOTO 20870
            END IF
          END IF
          READ(5,'(7F15.0,I4)',ERR=20850) SMFMNZ(I),SMFMNX(I),SMFMXX(I),
     *    SMFMNY(I),SMFMXY(I), SMFX2(I),SMFY2(I),SMFPXY(I)
          GOTO 20870
20850     CALL HELP_MESSAGE(37)
          GOTO 20810
20870     CONTINUE
          IF (((SMFMNZ(I).LT.0.).OR.(SMFMNX(I).GE. SMFMXX(I)).OR. (SMFMN
     *    Y(I).GE. SMFMXY(I)).OR.(SMFPXY(I).LT.0.0).OR.(SMFPXY(I).GT.1.)
     *    )) THEN
            WRITE(6,20880)
20880       FORMAT(//' SORRY, SSD  MUST BE >= 0.0')
            WRITE(6,20890)
20890       FORMAT('        X+ MUST BE > X-')
            WRITE(6,20900)
20900       FORMAT('        Y+ MUST BE > Y-')
            WRITE(6,20910)
20910       FORMAT('        Orientation must be = 0 or 1'//)
            GO TO 20810
          END IF
          HFIELD(I)=(SMFMXX(I)-SMFMNX(I)+SMFMXY(I)-SMFMNY(I))/4.
        ELSE IF((ISMFPS(I) .EQ. 3)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(38)
          END IF
20920     WRITE(6,20930)
20930     FORMAT(/' SSD, Rmin and Rmax OF THE SOURCE(=0 for point source
     *):' /' ',$)
          IF (OLDFILE) THEN
            WRITE(6,20940)SMFMNZ(I),SMFMNR(I),SMFMXR(I)
20940       FORMAT(/' The old values were',3F10.3,'.'/ ' Do you want toc
     *hange them?'/' ',$)
            READ(5,20950)C1
20950       FORMAT(A1)
            IF ((C1.EQ.'?')) THEN
              GO TO 20960
            END IF
            IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
              WRITE(6,20970)
20970         FORMAT(/' SSD, Rmin and Rmax OF THE SOURCE(=0 for point so
     *urce):' /' ',$)
            ELSE
              GOTO 20980
            END IF
          END IF
          READ(5,'(3F15.0)',ERR=20960)SMFMNZ(I),SMFMNR(I),SMFMXR(I)
          GOTO 20980
20960     CALL HELP_MESSAGE(38)
          GOTO 20920
20980     CONTINUE
          IF ((SMFMNZ(I).LT.0.).OR.(SMFMNR(I).LT.0.0).OR.(SMFMNR(I) .GT.
     *     SMFMXR(I))) THEN
            WRITE(6,20990)
20990       FORMAT(//' SORRY, SSD  MUST BE >= 0.0')
            WRITE(6,21000)
21000       FORMAT('        Radius MUST BE >= 0.0'//)
            WRITE(6,21010)
21010       FORMAT('        Rmax MUST BE >= Rmin'//)
            GO TO 20920
          END IF
          IF ((SMFMNR(I) .LT. 1.0).AND.(SMFMNZ(I) .GT. 50.).AND.(RSSD0 .
     *    NE. 1000.)) THEN
            IF (.NOT.EGSPERT) THEN
              CALL HELP_MESSAGE(39)
            END IF
21020       WRITE(6,21030)
21030       FORMAT(/' Radius(cm) of a ring region on the surface for SSD
     * analysis:'/ ' ',$)
            IF (OLDFILE) THEN
              WRITE(6,21040)RSSD0
21040         FORMAT(/' The old value was',F10.3,'.'/ ' Do you want to c
     *hange it?'/' ',$)
              READ(5,21050)C1
21050         FORMAT(A1)
              IF ((C1.EQ.'?')) THEN
                GO TO 21060
              END IF
              IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
                WRITE(6,21070)
21070           FORMAT(/' Radius(cm) of a ring region on the surface for
     * SSD analysis:' /' ',$)
              ELSE
                GOTO 21080
              END IF
            END IF
            READ(5,'(F15.0)',ERR=21060)RSSD0
            GOTO 21080
21060       CALL HELP_MESSAGE(39)
            GOTO 21020
21080       IF ((RSSD0.LT.0.)) THEN
              WRITE(6,21090)
21090         FORMAT(//' SORRY, Radius  MUST BE >= 0.0'/)
              GO TO 21060
            END IF
            IF ((RSSD0 .EQ. RSSD1)) THEN
              WRITE(6,21100)
21100         FORMAT(///' SORRY, YOU CAN ONLY HAVE ONE POINT SOURCE!!!'/
     */ ' THINK ABOUT IT AND TRY AGAIN.'//)
              STOP
            END IF
            RSSD1=RSSD0
            RSSD0=1000.
          END IF
          HFIELD(I)=SMFMNR(I)
        ELSE IF((ISMFPS(I) .EQ. 4)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(40)
          END IF
21110     WRITE(6,21120)
21120     FORMAT(/' SSD, X-, X+, Y- and Y+ (in cm) OF THE SOURCE:',$)
          IF (OLDFILE) THEN
            WRITE(6,21130)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY
     *      (I)
21130       FORMAT(/' The old values were',5F10.3,'.'/ ' Do you want toc
     *hange them?'/' ',$)
            READ(5,21140)C1
21140       FORMAT(A1)
            IF ((C1.EQ.'?')) THEN
              GO TO 21150
            END IF
            IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
              WRITE(6,21160)
21160         FORMAT(/' SSD, X-, X+, Y- and Y+ (in cm) OF THE SOURCE:',$
     *)
            ELSE
              GOTO 21170
            END IF
          END IF
          READ(5,'(5F15.0)',ERR=21150)SMFMNZ(I),SMFMNX(I),SMFMXX(I), SMF
     *    MNY(I),SMFMXY(I)
          GOTO 21170
21150     CALL HELP_MESSAGE(40)
          GOTO 21110
21170     CONTINUE
          IF ((SMFMNZ(I).LT.0.).OR.(SMFMXX(I).LT.SMFMNX(I)).OR.(SMFMNY(I
     *    ) .GT. SMFMXY(I))) THEN
            WRITE(6,21180)
21180       FORMAT(//' SORRY, SSD  MUST BE >= 0.0')
            WRITE(6,21190)
21190       FORMAT('        X+ MUST BE > X-'//)
            WRITE(6,21200)
21200       FORMAT('        Y+ MUST BE > Y-'//)
            GO TO 21150
          END IF
        ELSE IF((ISMFPS(I) .EQ. 5)) THEN
          IF (.NOT.EGSPERT) THEN
            CALL HELP_MESSAGE(41)
          END IF
21210     WRITE(6,21220)
21220     FORMAT(/' SSD and Radius (in cm) OF THE SOURCE:'/ ' ',$)
          IF (OLDFILE) THEN
            WRITE(6,21230)SMFMNZ(I),SMFMNR(I)
21230       FORMAT(/' The old values were',2F10.3,'.'/ ' Do you want toc
     *hange them?'/' ',$)
            READ(5,21240)C1
21240       FORMAT(A1)
            IF ((C1.EQ.'?')) THEN
              GO TO 21250
            END IF
            IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
              WRITE(6,21260)
21260         FORMAT(/' SSD and Radius (in cm) OF THE SOURCE:'/ ' ',$)
            ELSE
              GOTO 21270
            END IF
          END IF
          READ(5,'(2F15.0)',ERR=21250)SMFMNZ(I),SMFMNR(I)
          GOTO 21270
21250     CALL HELP_MESSAGE(41)
          GOTO 21210
21270     CONTINUE
          IF ((SMFMNZ(I).LT.0.).OR.(SMFMNR(I).LT.0.0)) THEN
            WRITE(6,21280)
21280       FORMAT(//' SORRY, SSD  MUST BE >= 0.0')
            WRITE(6,21290)
21290       FORMAT('        Radius MUST BE >= 0.0'//)
            GO TO 21250
          END IF
        END IF
20511 CONTINUE
20512 CONTINUE
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(42)
      END IF
21300 WRITE(6,21310)
21310 FORMAT(/' Nbin, Emin, Emax (in MeV, kinetic only) for the spectrum
     *:'/ ' ',$)
      IF (OLDFILE) THEN
        WRITE(6,21320)NSMFEE,SMFMNE,SMFMXE
21320   FORMAT(/' The old values were',I4,2F10.3,'.'/ ' Do you want to c
     *hange them?'/' ',$)
        READ(5,21330)C1
21330   FORMAT(A1)
        IF ((C1.EQ.'?')) THEN
          GO TO 21340
        END IF
        IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
          WRITE(6,21350)
21350     FORMAT(/' Nbin, Emin, Emax (in MeV, kinetic only) for the spec
     *trum:'/' ',$)
        ELSE
          GOTO 21360
        END IF
      END IF
      READ(5,'(I15,2F15.0)',ERR=21340) NSMFEE,SMFMNE,SMFMXE
      WRITE(6,21370)NSMFEE,SMFMNE,SMFMXE
21370 FORMAT(I6,2F12.4)
      GOTO 21360
21340 CALL HELP_MESSAGE(42)
      GOTO 21300
21360 CONTINUE
      IF (((SMFMNE.LT.0.0).OR.(SMFMNE.GT.SMFMXE).OR.(SMFMXE.LT.0.0).OR.(
     *NSMFEE.GT.200).OR.(NSMFEE.LT.1))) THEN
        WRITE(6,21380)200
21380   FORMAT(/' Sorry, energies should be Emax > Emin > 0.0 '/ '    an
     *d bin number must be between 1 and ',I4,'!!!'/)
        GO TO 21340
      END IF
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(80)
      END IF
21390 WRITE(6,21400)
21400 FORMAT(/' ENERGY SPECTRUM TYPE ', '(0-inside/outside treatment fie
     *ld, 1-different radii):'/' ',$)
      IF (OLDFILE) THEN
        WRITE(6,21410)ESTYPE
21410   FORMAT(/' The old value was',I4,'.'/' Do you want to change it?'
     */' ',$)
        READ(5,21420)C1
21420   FORMAT(A1)
        IF ((C1.EQ.'?')) THEN
          GO TO 21430
        END IF
        IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
          WRITE(6,21440)
21440     FORMAT(/' ENERGY SPECTRUM TYPE (0-inside/outside, 1-differentr
     *adii):'/' ',$)
        ELSE
          GOTO 21450
        END IF
      END IF
      READ(5,'(I15)',ERR=21430) ESTYPE
      GOTO 21450
21430 CALL HELP_MESSAGE(80)
      GOTO 21390
21450 CONTINUE
      IF (((ESTYPE.LT.0).OR.(ESTYPE.GT.1))) THEN
        WRITE(6,21460)
21460   FORMAT(/' SORRY, NO SUCH ENERGY SPECTRUM TYPE!!!'//)
        GO TO 21430
      END IF
      IF ((ESTYPE .EQ. 0)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(81)
        END IF
21470   WRITE(6,21480)
21480   FORMAT(/' R_treatment,R_scoring:'/' ',$)
        IF (OLDFILE) THEN
          WRITE(6,21490)RTREATe,RFIELDe
21490     FORMAT(/' The old values were',F10.3,F10.3,'.'/ 'Do you want t
     *o change them?'/' ',$)
          READ(5,21500)C1
21500     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 21510
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,21520)
21520       FORMAT(/' R_treatment,R_scoring:'/' ',$)
          ELSE
            GOTO 21530
          END IF
        END IF
        READ(5,'(F15.0,F15.0)',ERR=21510) RTREATe,RFIELDe
        GOTO 21530
21510   CALL HELP_MESSAGE(81)
        GOTO 21470
21530   CONTINUE
        IF ((RTREATe.LT.0.0001).OR.(RFIELDe.LT.RTREATe)) THEN
          WRITE(6,21540)
21540     FORMAT(//' SORRY, RADIUS  MUST BE >= 0.0001' / ' AND DEFINED F
     *IELD MUST BE INSIDE SCORING FIELD')
          GO TO 21510
        END IF
        NFIELDe = 2
        ERADII(1) = RTREATe
        ERADII(2) = RFIELDe
      ELSE IF((ESTYPE .EQ. 1)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(82)
        END IF
21550   WRITE(6,21560)
21560   FORMAT(/' NbinEn,R_scoring:'/' ',$)
        IF (OLDFILE) THEN
          WRITE(6,21570)NFIELDe,RFIELDe
21570     FORMAT(/' The old values were',I4,F10.3,'.'/ 'Do you want to c
     *hange them?'/' ',$)
          READ(5,21580)C1
21580     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 21590
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,21600)
21600       FORMAT(/' NbinEn,R_scoring:'/' ',$)
          ELSE
            GOTO 21610
          END IF
        END IF
        READ(5,'(I12,F15.0)',ERR=21590) NFIELDe,RFIELDe
        GOTO 21610
21590   CALL HELP_MESSAGE(82)
        GOTO 21550
21610   CONTINUE
        IF ((RFIELDe.LT.0.0001).OR.(NFIELDe.LT.1).OR.(NFIELDe.GT.200)) T
     *  HEN
          WRITE(6,21620)
21620     FORMAT(//' SORRY, SCORING FIELD RADIUS MUST BE >= 0.0001,')
          WRITE(6,21630)200
21630     FORMAT(' AND NUMBER OF BINS MUST BE BETWEEN 1 AND ',I5,'!'//)
          GO TO 21590
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(83)
        END IF
21640   WRITE(6,*) ('R[',I,'],',I=1,NFIELDe)
        IF (OLDFILE) THEN
          WRITE(6,*)(ERADII(I),I=1,NFIELDe)
          WRITE(6,21650)(ERADII(I),I=1,NFIELDe)
21650     FORMAT(/' The old values were',A,'.'/'Do you want to change it
     *?'/' ',$)
          READ(5,21660)C1
21660     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 21670
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,*) ('R[',I,'],',I=1,NFIELDe)
          ELSE
            GOTO 21680
          END IF
        END IF
        READ(5,*,ERR=21670)(ERADII(I),I=1,NFIELDe)
        GOTO 21680
21670   CALL HELP_MESSAGE(83)
        GOTO 21640
21680   CONTINUE
        DO 21691 I=1,NFIELDe
          IF ((ERADII(I).LT.0.0001).OR.(ERADII(I).GT.RFIELDe)) THEN
            WRITE(6,21700)
21700       FORMAT(//' SORRY, RADIUS  MUST BE >= 0.0001' / ' AND MUST BE
     * INSIDE SCORING FIELD')
            GO TO 21670
          END IF
21691   CONTINUE
21692   CONTINUE
        IF ((ERADII(NFIELDe) .NE. RFIELDe)) THEN
          WRITE(6,21710)
21710     FORMAT(//' SORRY, LAST RADIUS MUST BE EQUAL TO R_scoring,')
          GO TO 21670
        END IF
      END IF
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(43)
      END IF
21720 WRITE(6,21730)
21730 FORMAT(/' FIELD TYPE (0-circular ring,1-square ring,2-rectangular)
     *:'/' ',$)
      IF (OLDFILE) THEN
        WRITE(6,21740)MSMFXY
21740   FORMAT(/' The old value was',I4,'.'/' Do you want to change it?'
     */' ',$)
        READ(5,21750)C1
21750   FORMAT(A1)
        IF ((C1.EQ.'?')) THEN
          GO TO 21760
        END IF
        IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
          WRITE(6,21770)
21770     FORMAT(/' INPUT FIELD TYPE (0-circular, 1-square, 2-rectangula
     *r):'/' ',$)
        ELSE
          GOTO 21780
        END IF
      END IF
      READ(5,'(I15)',ERR=21760) MSMFXY
      GOTO 21780
21760 CALL HELP_MESSAGE(43)
      GOTO 21720
21780 CONTINUE
      IF (((MSMFXY.LT.0).OR.(MSMFXY.GT.2))) THEN
        WRITE(6,21790)
21790   FORMAT(/' SORRY, NO SUCH FIELD TYPE!!!'//)
        GO TO 21760
      END IF
      IF ((MSMFXY .EQ. 0)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(44)
        END IF
21800   WRITE(6,21810)
21810   FORMAT(/' NbinFlIn, R_treatment, NbinFlOut, R_scoring:'/' ',$)
        IF (OLDFILE) THEN
          WRITE(6,21820)NFIELDi,RTREAT,NFIELDo,RFIELD
21820     FORMAT(/' The old values were',I4,F10.3,I4,F10.3,'.'/ 'Do youw
     *ant to change them?'/' ',$)
          READ(5,21830)C1
21830     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 21840
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,21850)
21850       FORMAT(/' NbinFlIn, R_treatment, NbinFlOut, R_scoring:'/' ',
     *$)
          ELSE
            GOTO 21860
          END IF
        END IF
        READ(5,'(I12,F15.0,I12,F15.0)',ERR=21840) NFIELDi,RTREAT,NFIELDo
     *  ,RFIELD
        GOTO 21860
21840   CALL HELP_MESSAGE(44)
        GOTO 21800
21860   CONTINUE
        NFIELD = NFIELDi + NFIELDo
        IF ((RTREAT.LT.0.0001).OR.(RFIELD.LT.RTREAT)) THEN
          WRITE(6,21870)
21870     FORMAT(//' SORRY, RADIUS  MUST BE >= 0.0001' / ' AND DEFINED F
     *IELD MUST BE INSIDE SCORING FIELD')
          GO TO 21840
        END IF
        IF ((NFIELDi.LT.1).OR.(NFIELDi.GT.200).OR.(NFIELDo.LT.1).OR.(NFI
     *  ELDo.GT.200)) THEN
          WRITE(6,21880)200
21880     FORMAT(//' SORRY, NUMBER OF BINS MUST BE BETWEEN 0 AND ',I5,'!
     *'//)
          GO TO 21840
        END IF
        IF (((NFIELD.LT.1).OR.(NFIELD).GT.200)) THEN
          WRITE(6,21890)200
21890     FORMAT(//' SORRY, TOTAL NUMBER OF BINS MUST BE BETWEEN 1 AND '
     *,I5,'!'//)
          GO TO 21840
        END IF
      ELSE IF((MSMFXY .EQ. 1)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(47)
        END IF
21900   WRITE(6,21910)
21910   FORMAT(/' NbinFlIn,1/2 TREAT FIELD WIDTH,NbinFlOut,1/2 SCORING F
     *IELD WIDTH:'/ ' ',$)
        IF (OLDFILE) THEN
          WRITE(6,21920)NFIELDi,RTREAT,NFIELDo,RFIELD
21920     FORMAT(/' The old values were',I4,F10.3,I4,F10.3,'.'/ 'Do youw
     *ant to change them?'/' ',$)
          READ(5,21930)C1
21930     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 21940
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,21950)
21950       FORMAT(/' NbinFlIn,1/2 TREAT FIELD WIDTH,NbinFlOut,1/2 SCORI
     *NG FIELD WIDTH:'/ ' ',$)
          ELSE
            GOTO 21960
          END IF
        END IF
        READ(5,'(I12,F15.0,I12,F15.0)',ERR=21940) NFIELDi,RTREAT,NFIELDo
     *  ,RFIELD
        GOTO 21960
21940   CALL HELP_MESSAGE(47)
        GOTO 21900
21960   CONTINUE
        NFIELD = NFIELDi + NFIELDo
        IF ((RTREAT.LT.0.0001).OR.(RFIELD.LT.RTREAT)) THEN
          WRITE(6,21970)
21970     FORMAT(//' SORRY, 1/2 WIDTH  MUST BE >= 0.0001,' / ' AND DEFIN
     *ED FIELD MUST BE INSIDE SCORING FIELD,')
          GO TO 21940
        END IF
        IF ((NFIELDi.LT.1).OR.(NFIELDi.GT.200).OR.(NFIELDo.LT.1).OR.(NFI
     *  ELDo.GT.200)) THEN
          WRITE(6,21980)200
21980     FORMAT(//' SORRY, NUMBER OF BINS MUST BE BETWEEN 0 AND ',I5,'!
     *'//)
          GO TO 21940
        END IF
        IF ((NFIELD.LT.1).OR.(NFIELD.GT.200)) THEN
          WRITE(6,21990)
21990     FORMAT(//' SORRY, TOTAL NUMBER OF BINS MUST BE BETWEEN 1 AND '
     *,I5,'!'//)
          GO TO 21940
        END IF
      ELSE IF((MSMFXY .EQ. 2)) THEN
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(45)
        END IF
22000   WRITE(6,22010)
22010   FORMAT(/' INPUT NFbin,XFmin,XFmax,YFmin,YFmax FOR THE SCORING FI
     *ELD:'/' ',$)
        IF (OLDFILE) THEN
          WRITE(6,22020)NFIELD,XFLDMN,XFLDMX,YFLDMN,YFLDMX
22020     FORMAT(/' The old values were',I4,4F10.3,'.'/ ' Do you want to
     * change them? '/' ',$)
          READ(5,22030)C1
22030     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 22040
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,22050)
22050       FORMAT(/' INPUT NFbin, XFmin, XFmax, YFmin, YFmax:'/' ',$)
          ELSE
            GOTO 22060
          END IF
        END IF
        READ(5,'(I12,4F15.0)',ERR=22040)NFIELD,XFLDMN,XFLDMX,YFLDMN,YFLD
     *  MX
        GOTO 22060
22040   CALL HELP_MESSAGE(45)
        GOTO 22000
22060   CONTINUE
        IF ((XFLDMN.GE.XFLDMX).OR.(YFLDMN.GE.YFLDMX).OR. (NFIELD.LT.1).O
     *  R.(NFIELD.GT.200)) THEN
          WRITE(6,22070)
22070     FORMAT(//' SORRY, XFmax MUST BE > XFmin')
          WRITE(6,22080)
22080     FORMAT('        YFmax MUST BE > YFmin'//)
          WRITE(6,22090)200
22090     FORMAT('        NFbin MUST BE BETWEEN 1 AND ',I5,'!!'//)
          GO TO 22040
        END IF
        IF (.NOT.EGSPERT) THEN
          CALL HELP_MESSAGE(46)
        END IF
22100   WRITE(6,22110)
22110   FORMAT(/' INPUT Xmin,Xmax,Ymin,Ymax FOR TREATMENT FIELD: ',$)
        IF (OLDFILE) THEN
          WRITE(6,22120)XTREATMN,XTREATMX,YTREATMN,YTREATMX
22120     FORMAT(/' The old values were',4F10.3,'.'/ ' Do you want to ch
     *ange them?'/' ',$)
          READ(5,22130)C1
22130     FORMAT(A1)
          IF ((C1.EQ.'?')) THEN
            GO TO 22140
          END IF
          IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
            WRITE(6,22150)
22150       FORMAT(/' INPUT Xmin,Xmax,Ymin,Ymax FOR TREATMENT FIELD: ',$
     *)
          ELSE
            GOTO 22160
          END IF
        END IF
        READ(5,'(4F15.0)',ERR=22140)XTREATMN,XTREATMX,YTREATMN,YTREATMX
        GOTO 22160
22140   CALL HELP_MESSAGE(46)
        GOTO 22100
22160   CONTINUE
        IF ((XTREATMX.LT.XTREATMN).OR.(YTREATMX.LT.YTREATMN)) THEN
          WRITE(6,22170)
22170     FORMAT(//' SORRY, XTFmin > XTFmax OR YTFmin > YTFmax NOT ALLOW
     *ED!')
          GO TO 22140
        END IF
      END IF
20460 CONTINUE
      WRITE(6,22180)
22180 FORMAT(/' The order of the sub-sources in terms of their distances
     * to the'/ ' scoring plane and their LATCH numbers:       '// '   o
     *rder     sub-source #       SSD           LATCH #      CHARGE'/)
      DO 22191 I=1,NSMFPS
        K(I)=1
        Z=SMFMNZ(1)
        IF ((I .GT. 1)) THEN
22200     CONTINUE
          DO 22211 II=1,I-1
            IF ((K(I) .EQ. K(II))) THEN
              K(I)=K(I)+1
              Z=SMFMNZ(K(I))
              GOTO 22200
            END IF
22211     CONTINUE
22212     CONTINUE
        END IF
22220   CONTINUE
        IF ((I .NE. NSMFPS)) THEN
          DO 22231 III=1,NSMFPS
            IF ((SMFMNZ(III) .LT. Z)) THEN
              DO 22241 II=1,I
                IF ((III .EQ. K(II))) THEN
                  GOTO 22250
                END IF
22241         CONTINUE
22242         CONTINUE
              K(I)=III
              Z=SMFMNZ(III)
22250         CONTINUE
            END IF
22231     CONTINUE
22232     CONTINUE
        END IF
        WRITE(6,22260)I,K(I),SMFMNZ(K(I)),LATCH(K(I)),IQSMFP(K(I))
22260   FORMAT(I6, I14, F16.3,2I14)
22191 CONTINUE
22192 CONTINUE
3020  CONTINUE
      CALL READNAME
      OPEN(UNIT=1,STATUS='UNKNOWN',FILE=SPCNAM)
      WRITE(1,'(A80)')SSMFPS
      WRITE(1,*)NSMFPS
      DO 22271 I=1,NSMFPS
        WRITE(1,*)ISMFPS(I),IQSMFP(I),LATCH(I)
        IF ((ISMFPS(I) .EQ. 1)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 11)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SM
     *    FMXY(I), SMFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 2)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I),SMFPXY(I)
        ELSE IF((ISMFPS(I) .EQ. 3)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNR(I),SMFMXR(I),RSSD1
        ELSE IF((ISMFPS(I) .EQ. 4)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I)
        ELSE IF((ISMFPS(I) .EQ. 5)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNR(I)
        END IF
22271 CONTINUE
22272 CONTINUE
      WRITE(1,*)NSMFEE,SMFMNE,SMFMXE
      WRITE(1,*)ESTYPE
      IF ((ESTYPE .EQ. 0)) THEN
        WRITE(1,*)RTREATe,RFIELDe
      ELSE IF((ESTYPE .EQ. 1)) THEN
        WRITE(1,*)NFIELDe,RFIELDe
        WRITE(1,*)(ERADII(I),I=1,NFIELDe)
      END IF
      WRITE(1,*)MSMFXY
      I=1
      IF ((MSMFXY .EQ. 0)) THEN
        WRITE(1,*)NFIELDi,RTREAT,NFIELDo,RFIELD
      ELSE IF((MSMFXY .EQ. 1)) THEN
        WRITE(1,*)NFIELDi,RTREAT,NFIELDo,RFIELD
      ELSE IF((MSMFXY .EQ. 2)) THEN
        WRITE(1,*)NFIELD,XFLDMN,XFLDMX,YFLDMN,YFLDMX
        WRITE(1,*)XTREATMN,XTREATMX,YTREATMN,YTREATMX
      END IF
      WRITE(1,'(A100)')PSDNAM
      DO 22281 I=1,NSMFPS
        WRITE(1,*)I
        WRITE(1,*)ISMFPS(I),IQSMFP(I),LATCH(I)
        IF ((ISMFPS(I) .EQ. 1)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 11)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SM
     *    FMXY(I), SMFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 2)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I),SMFPXY(I)
        ELSE IF((ISMFPS(I) .EQ. 3)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNR(I),SMFMXR(I),RSSD1
        ELSE IF((ISMFPS(I) .EQ. 4)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I)
        ELSE IF((ISMFPS(I) .EQ. 5)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNR(I)
        END IF
        WRITE(1,*)SMFRTN(I),SSDVPS(I)
        WRITE(1,*)NSMFEE,SMFMNE,SMFMXE
        DO 22291 III=1,NFIELDe
          WRITE(1,*) (FSMFBN1(I,III,II),II=1,NSMFEE)
22291   CONTINUE
22292   CONTINUE
        WRITE(1,*)MSMFXY
        IF ((MSMFXY .EQ. 0)) THEN
          WRITE(1,*) NFIELDi,RTREAT,NFIELDo,RFIELD
          WRITE(1,*)(FSMFRR(I,II),II=1,NFIELD)
        END IF
        IF ((MSMFXY .EQ. 1)) THEN
          WRITE(1,*)NFIELDi,RTREAT,NFIELDo,RFIELD
          WRITE(1,*)(FSMFRR(I,II),II=1,NFIELD)
          WRITE(1,*)(FSMFR11(I,II),II=1,NFIELD)
          WRITE(1,*)(FSMFR12(I,II),II=1,NFIELD)
        ELSE IF((MSMFXY .EQ. 2)) THEN
          WRITE(1,*)NFIELD,XFLDMN,XFLDMX
          WRITE(1,*)NFIELD,YFLDMN,YFLDMX
          WRITE(1,*)((FSMFXY(I,II,III),II=1,NFIELD),III=1,NFIELD)
        END IF
22281 CONTINUE
22282 CONTINUE
      WRITE(1,*) (FSMABN1(II),II=1,200)
      CLOSE(UNIT=1,STATUS='KEEP')
      IF ((CHOICE .EQ. 1)) THEN
        GOTO 22300
      END IF
22310 CONTINUE
      CALL OPENFILE
      CALL READ_PHSP
      CLOSE (2)
      WRITE(6,22320)
22320 FORMAT(/' BEGIN SUMMARISING THE PH-SP DATA.....')
      IF ((N_ph_sp_e .NE. 0)) THEN
        E_ph_sp_e = E_ph_sp_e/N_ph_sp_e
      END IF
      IF ((N_ph_sp_g .NE. 0)) THEN
        E_ph_sp_g = E_ph_sp_g/N_ph_sp_g
      END IF
      IF ((N_ph_sp_p .NE. 0)) THEN
        E_ph_sp_p = E_ph_sp_p/N_ph_sp_p
      END IF
      WRITE(6,'(/T10,''INFORMATION ABOUT THE FULL PHASE SPACE DATA ''//
     *                         '' Read total''
     *                                 ,I10,'' particles and ignored'',I
     *6,'' multiple passers'')') N_ph_sp_e+N_ph_sp_g+N_ph_sp_p,NPASS_ph_
     *sp
      WRITE(6,'(/'' There were'',I10,'' electrons with average energy'',
     *                   T55, F9.4,'' MeV'')') N_ph_sp_e,E_ph_sp_e
      WRITE(6,'(/'' There were'',I10,'' photons with average energy'',
     *                   T55, F9.4,'' MeV'')') N_ph_sp_g,E_ph_sp_g
      WRITE(6,'(/'' There were'',I10,'' positrons with average energy'',
     *                   T55, F9.4,'' MeV'')') N_ph_sp_p,E_ph_sp_p
      WRITE(6,'(/'' Minimum / maximum particle energies were'',
     *               F9.3,'' /'',F9.3,'' MeV'')') E_MIN_ph_sp,E_MAX_ph_s
     *p
      WRITE(6,22330)K4,(K1+K2+K3),K1,K2,K3,NWRONG
22330 FORMAT(/'           ',I10,' PARTICLES SCORED FOR ENERGY/FLUENCE DI
     *STRIBUTIONS' /'           ',I10,' PARTICLES IGNORED BECAUSE LATCH#
     * ARE NOT SET' /'           ',I10,' OF THEM ARE ELECTRONS' /'      
     *',I10,' OF THEM ARE PHOTONS, AND' /'           ',I10,' OF THEM ARE
     * POSITRONS' /'  ALSO     ',I10,' PARTICLES NOT SCORED DUE TO OUT O
     *F THE FIELD')
      DO 22341 I=1,NSMFPS
        IF ((ISMFPS(I) .EQ. 3).AND.(SMFMNR(I) .EQ. 0.0)) THEN
          III=0
          DO 22351 IIII=1,2000
            IF ((RSSDST(I,IIII) .NE. 0.0).AND.(III .EQ. 0)) THEN
              III=1
            END IF
22351     CONTINUE
22352     CONTINUE
          IF ((III .EQ. 0)) THEN
            WRITE(6,22360)I
22360       FORMAT(/' No particles from source ',I4,' falling into the r
     *ing.'/ ' Was the radius of the ring outside of the field?'/ '   '/
     *' The SSD for the point source has been taken to be 100 cm.'/ ' Ho
     *wever, you can try again with a different ring radius.')
            IF ((IQSMFP(I) .EQ. -1)) THEN
              SSDVPS(22+1)=100.
            ELSE IF((IQSMFP(I) .EQ. 0)) THEN
              SSDVPS(22+2)=100.
            ELSE IF((IQSMFP(I) .EQ. 1)) THEN
              SSDVPS(22+3)=100.
            END IF
            GOTO 22370
          END IF
          DO 22381 II=1,3
            DO 22391 IIII=3,1998
              SSSDST(I,IIII)=RSSDST(I,IIII)*0.3+ RSSDST(I,IIII-1)*0.25+R
     *        SSDST(I,IIII+1)*0.25+ RSSDST(I,IIII-2)*0.1+RSSDST(I,IIII+2
     *        )*0.1
22391       CONTINUE
22392       CONTINUE
            DO 22401 IIII=3,1998
              RSSDST(I,IIII)=SSSDST(I,IIII)
22401       CONTINUE
22402       CONTINUE
            J=1
            DO 22411 IIII=2,2000
              IF ((II .EQ. 1)) THEN
                RSSDST(I,IIII)=10.*RSSDST(I,IIII)/FLOAT(IIII)
              END IF
              IF ((RSSDST(I,IIII) .GE. RSSDST(I,J))) THEN
                J=IIII
              END IF
22411       CONTINUE
22412       CONTINUE
22381     CONTINUE
22382     CONTINUE
          DO 22421 IIII=J-10,J+10
            DO 22431 III=1,10
              RSSDST(I,IIII*10+III)=RSSDST(I,IIII+1)
22431       CONTINUE
22432       CONTINUE
22421     CONTINUE
22422     CONTINUE
          J=(J-8)*10
          DO 22441 JJ=1,3
            IF((JJ .NE. 1))J=J-80
            DO 22451 IIII=J,J+160
              SSSDST(I,IIII)=RSSDST(I,IIII)*0.05+ RSSDST(I,IIII-1)*0.04+
     *        RSSDST(I,IIII+1)*0.04+ RSSDST(I,IIII-2)*0.035+RSSDST(I,III
     *        I+2)*0.035+ RSSDST(I,IIII-3)*0.03+RSSDST(I,IIII+3)*0.03+ R
     *        SSDST(I,IIII-4)*0.025+RSSDST(I,IIII+4)*0.025+ RSSDST(I,III
     *        I-5)*0.025+RSSDST(I,IIII+5)*0.025+ RSSDST(I,IIII-6)*0.025+
     *        RSSDST(I,IIII+6)*0.025+ RSSDST(I,IIII-7)*0.025+RSSDST(I,II
     *        II+7)*0.025+ RSSDST(I,IIII-8)*0.025+RSSDST(I,IIII+8)*0.025
     *        + RSSDST(I,IIII-9)*0.025+RSSDST(I,IIII+9)*0.025+ RSSDST(I,
     *        IIII-10)*0.02+RSSDST(I,IIII+10)*0.02+ RSSDST(I,IIII-11)*0.
     *        02+RSSDST(I,IIII+11)*0.02+ RSSDST(I,IIII-12)*0.02+RSSDST(I
     *        ,IIII+12)*0.02+ RSSDST(I,IIII-13)*0.02+RSSDST(I,IIII+13)*0
     *        .02+ RSSDST(I,IIII-14)*0.02+RSSDST(I,IIII+14)*0.02+ RSSDST
     *        (I,IIII-15)*0.02+RSSDST(I,IIII+15)*0.02+ RSSDST(I,IIII-16)
     *        *0.02+RSSDST(I,IIII+16)*0.02+ RSSDST(I,IIII-17)*0.02+RSSDS
     *        T(I,IIII+17)*0.02+ RSSDST(I,IIII-18)*0.02+RSSDST(I,IIII+18
     *        )*0.02+ RSSDST(I,IIII-19)*0.02+RSSDST(I,IIII+19)*0.02+ RSS
     *        DST(I,IIII-20)*0.02+RSSDST(I,IIII+20)*0.02
22451       CONTINUE
22452       CONTINUE
            DO 22461 IIII=J,J+160
              RSSDST(I,IIII)=SSSDST(I,IIII)
              IF((RSSDST(I,IIII) .GE. RSSDST(I,J)))J=IIII
22461       CONTINUE
22462       CONTINUE
22441     CONTINUE
22442     CONTINUE
          ZTOTAL(I)=DELTAZ*(RSSD1+RSSD0)/2./(FLOAT(J)/100.-(RSSD1+RSSD0)
     *    /2.)
          IF ((ZTOTAL(I) .GT. 1.0E+30)) THEN
            WRITE(6,22470)
22470       FORMAT(///' The particles are incident along Z-axis. Is it a
     * parallel'/ ' beam or the radius of the ring close to zero?'/ ''/ 
     *' The SSD for the point source has been taken to be 1.0E+30 cm.'/ 
     *' However, you can try again with a different ring radius.'//)
            IF ((IQSMFP(I) .EQ. -1)) THEN
              SSDVPS(22+1)=1.0E+30
            ELSE IF((IQSMFP(I) .EQ. 0)) THEN
              SSDVPS(22+2)=1.0E+30
            ELSE IF((IQSMFP(I) .EQ. 1)) THEN
              SSDVPS(22+3)=1.0E+30
            END IF
          ELSE
            WRITE(6,22480)I,ZTOTAL(I)
22480       FORMAT(/' The virtual SSD for source',I4,' is',F7.1,' cm '/'
     * according to the full ph-sp data.'/)
            IF ((IQSMFP(I) .EQ. -1)) THEN
              SSDVPS(22+1)=ZTOTAL(I)
            ELSE IF((IQSMFP(I) .EQ. 0)) THEN
              SSDVPS(22+2)=ZTOTAL(I)
            ELSE IF((IQSMFP(I) .EQ. 1)) THEN
              SSDVPS(22+3)=ZTOTAL(I)
            END IF
          END IF
22370     CONTINUE
        END IF
22341 CONTINUE
22342 CONTINUE
      IF ((MSMFXY .EQ. 1)) THEN
        DO 22491 I=1,NSMFPS
          FSMFR11(I,1)=1.
          FSMFR12(I,1)=0.
          DO 22501 II=2,NFIELD
            IF ((II.LE.NFIELDi)) THEN
              RBin=RTREAT**2*(FLOAT(II)/FLOAT(NFIELDi))
            ELSE
              RBin=(RFIELD**2-RTREAT**2)*(FLOAT(II-NFIELDi)/FLOAT(NFIELD
     *        o))+ RTREAT**2
            END IF
            IF ((FSMFR1(I,II).LT.FSMFR2(I,II)).AND.(FSMFR2(I,II).GT.0.))
     *       THEN
              IF ((FSMFR1(I,II)+FSMFR2(I,II) .LE. 10.)) THEN
                FSMFR11(I,II)=1.
                FSMFR12(I,II)=0.
              ELSE
                FSMFR11(I,II)=(7.*FSMFR1(I,II)-FSMFR2(I,II))/ (11.*FSMFR
     *          2(I,II)-5.*FSMFR1(I,II))
                FSMFR12(I,II)=12.*(FSMFR1(I,II)-FSMFR2(I,II))/ (11.*FSMF
     *          R2(I,II)-5.*FSMFR1(I,II))/RBin
              END IF
            ELSE IF((FSMFR2(I,II).LE.FSMFR1(I,II)).AND.(FSMFR1(I,II).GT.
     *      0.)) THEN
              IF ((FSMFR1(I,II)+FSMFR2(I,II) .LE. 10.)) THEN
                FSMFR11(I,II)=1.
                FSMFR12(I,II)=0.
              ELSE
                FSMFR11(I,II)=1.
                FSMFR12(I,II)=12.*(FSMFR1(I,II)-FSMFR2(I,II))/ (7.*FSMFR
     *          1(I,II)-FSMFR2(I,II))/RBin
              END IF
            END IF
22501     CONTINUE
22502     CONTINUE
22491   CONTINUE
22492   CONTINUE
      END IF
      IF ((SSDVPS(22+1).EQ.0.)) THEN
        WRITE(6,22510)
22510   FORMAT(//' ***WARNING:'/ ' You have not defined a virtual points
     *ource for e-'/ ' However, the SSD of this source may be used in so
     *me calculations.'/ ' A value of 100cm will be assumed.'//)
        SSDVPS(22+1)=100.
      END IF
      IF ((SSDVPS(22+2).EQ.0.)) THEN
        WRITE(6,22520)
22520   FORMAT(//' ***WARNING:'/ ' You have not defined a virtual points
     *ource for photons'/ ' However, the SSD of this source may be used 
     *in some calculations.'/ ' A value of 100cm will be assumed.'//)
        SSDVPS(22+2)=100.
      END IF
      DO 22531 I=1,NSMFPS
        IF ((IQSMFP(I) .EQ. -1)) THEN
          SSDVPS(I)=SSDVPS(22+1)
        ELSE IF((IQSMFP(I) .EQ. 0)) THEN
          SSDVPS(I)=SSDVPS(22+2)
        ELSE IF((IQSMFP(I) .EQ. 1)) THEN
          IF ((SSDVPS(22+3) .EQ. 0.)) THEN
            SSDVPS(I)=SSDVPS(22+1)
          ELSE
            SSDVPS(I)=SSDVPS(22+3)
          END IF
        END IF
22531 CONTINUE
22532 CONTINUE
      WRITE(6,22540)
22540 FORMAT(/' BEGIN OUTPUTING DATA.....')
      OPEN(UNIT=1,STATUS='UNKNOWN',FILE=SPCNAM)
      WRITE(1,'(A80)')SSMFPS
      WRITE(1,*)NSMFPS
      DO 22551 I=1,NSMFPS
        WRITE(1,*)ISMFPS(I),IQSMFP(I),LATCH(I)
        IF ((ISMFPS(I) .EQ. 1)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 11)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SM
     *    FMXY(I), SMFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 2)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I),SMFPXY(I)
        ELSE IF((ISMFPS(I) .EQ. 3)) THEN
          IF ((SMFMNZ(I) .NE. SSDVPS(I)).AND.(SMFMNR(I) .EQ. 0.0)) THEN
            WRITE(6,22560)I,SMFMNZ(I),SSDVPS(I)
22560       FORMAT(/' SSD FOR SUB-SOURCE',I4,' HAS BEEN RE-SET FROM',F7.
     *1, ' cm TO',F7.1,' cm'/' ACCORDING TO THE PHASE-SPACE DATA.')
            SMFMNZ(I)=SSDVPS(I)
          END IF
          WRITE(1,*)SMFMNZ(I),SMFMNR(I),SMFMXR(I),RSSD1
        ELSE IF((ISMFPS(I) .EQ. 4)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I)
        ELSE
          WRITE(1,*)SMFMNZ(I),SMFMNR(I)
        END IF
22551 CONTINUE
22552 CONTINUE
      WRITE(1,*)NSMFEE,SMFMNE,SMFMXE
      WRITE(1,*)ESTYPE
      IF ((ESTYPE .EQ. 0)) THEN
        WRITE(1,*)RTREATe,RFIELDe
      ELSE IF((ESTYPE .EQ. 1)) THEN
        WRITE(1,*)NFIELDe,RFIELDe
        WRITE(1,*)(ERADII(I),I=1,NFIELDe)
      END IF
      WRITE(1,*)MSMFXY
      I=1
      IF ((MSMFXY .EQ. 0)) THEN
        WRITE(1,*)NFIELDi,RTREAT,NFIELDo,RFIELD
      ELSE IF((MSMFXY .EQ. 1)) THEN
        WRITE(1,*)NFIELDi,RTREAT,NFIELDo,RFIELD
      ELSE IF((MSMFXY .EQ. 2)) THEN
        WRITE(1,*)NFIELD,XFLDMN,XFLDMX,YFLDMN,YFLDMX
        WRITE(1,*)XTREATMN,XTREATMX,YTREATMN,YTREATMX
      END IF
      WRITE(1,'(A100)')PSDNAM
      DO 22571 I=1,NSMFPS
        WRITE(1,*)I
        WRITE(1,*)ISMFPS(I),IQSMFP(I),LATCH(I)
        IF ((ISMFPS(I) .EQ. 1)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 11)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMXZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SM
     *    FMXY(I), SMFX2(I),SMFY2(I)
        ELSE IF((ISMFPS(I) .EQ. 2)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I), S
     *    MFX2(I),SMFY2(I),SMFPXY(I)
        ELSE IF((ISMFPS(I) .EQ. 3)) THEN
          IF ((SMFMNZ(I) .NE. SSDVPS(I)).AND.(SMFMNR(I) .EQ. 0.0)) THEN
            WRITE(6,22580)I,SMFMNZ(I),SSDVPS(I)
22580       FORMAT(//' SSD FOR SUB-SOURCE',I4,' HAS BEEN RE-SET FROM',F7
     *.1, ' cm TO',F7.1,' cm ACCORDING TO THE PH-SP DATA.'//)
            SMFMNZ(I)=SSDVPS(I)
          END IF
          WRITE(1,*)SMFMNZ(I),SMFMNR(I),SMFMXR(I),RSSD1
        ELSE IF((ISMFPS(I) .EQ. 4)) THEN
          WRITE(1,*)SMFMNZ(I),SMFMNX(I),SMFMXX(I),SMFMNY(I),SMFMXY(I)
        ELSE
          WRITE(1,*)SMFMNZ(I),SMFMNR(I)
        END IF
        SMFRTN(I)=SMFRTN(I)/KWEI
        WRITE(1,*)SMFRTN(I),SSDVPS(I)
        WRITE(1,*)NSMFEE,SMFMNE,SMFMXE
        DO 22591 III=1,NFIELDe
          WRITE(1,*) (FSMFBN1(I,III,II),II=1,NSMFEE)
22591   CONTINUE
22592   CONTINUE
        WRITE(1,*)MSMFXY
        IF ((MSMFXY .EQ. 0)) THEN
          WRITE(1,*)NFIELDi,RTREAT,NFIELDo,RFIELD
          WRITE(1,*)(FSMFRR(I,II),II=1,NFIELD)
        ELSE IF((MSMFXY .EQ. 1)) THEN
          WRITE(1,*)NFIELDi,RTREAT,NFIELDo,RFIELD
          WRITE(1,*)(FSMFRR(I,II),II=1,NFIELD)
          WRITE(1,*)(FSMFR11(I,II),II=1,NFIELD)
          WRITE(1,*)(FSMFR12(I,II),II=1,NFIELD)
        ELSE IF((MSMFXY .EQ. 2)) THEN
          WRITE(1,*)NFIELD,XFLDMN,XFLDMX
          WRITE(1,*)NFIELD,YFLDMN,YFLDMX
          WRITE(1,*)((FSMFXY(I,II,III),II=1,NFIELD),III=1,NFIELD)
        END IF
22571 CONTINUE
22572 CONTINUE
      III=1
      DO 22601 II=2,200
        IF ((FSMABN1(II) .GT. FSMABN1(III))) THEN
          III=II
        END IF
22601 CONTINUE
22602 CONTINUE
      DO 22611 II=1,200
        IF ((FSMABN1(III).GT.0)) THEN
          IF((II .NE. III))FSMABN1(II)=FSMABN1(II)/FSMABN1(III)
        END IF
        IF((II .EQ. 200))FSMABN1(III)=1.
22611 CONTINUE
22612 CONTINUE
      WRITE(1,*)(FSMABN1(II),II=1,200)
      CLOSE(UNIT=1,STATUS='KEEP')
20360 CONTINUE
      IF ((CHOICE .GT. 2)) THEN
        DO 22621 I=1,NFIELD
          IF ((I .LE. NFIELDi)) THEN
            BINSIZE(I)=RTREAT**2/(FLOAT(NFIELDi))
          ELSE
            BINSIZE(I)=(RFIELD**2-RTREAT**2)/(FLOAT(NFIELDo))
          END IF
22621   CONTINUE
22622   CONTINUE
        DO 22631 I=1,NSMFPS
          III=1
          IIIII=1
          DO 22641 II=1,NFIELD
            IF ((MSMFXY .EQ. 0).OR.(MSMFXY .EQ. 1)) THEN
              IF ((FSMFRR(I,II)/BINSIZE(II) .GT. FSMFRR(I,III)/BINSIZE(I
     *        II))) THEN
                III=II
              END IF
            ELSE IF((MSMFXY .EQ. 2)) THEN
              DO 22651 IIII=1,NFIELD
                IF ((FSMFXY(I,II,IIII) .GT. FSMFXY(I,III,IIIII))) THEN
                  III=II
                  IIIII=IIII
                END IF
22651         CONTINUE
22652         CONTINUE
            END IF
22641     CONTINUE
22642     CONTINUE
          DO 22661 II=1,NFIELD
            IF ((MSMFXY .EQ. 0).OR.(MSMFXY .EQ. 1)) THEN
              IF ((FSMFRR(I,III) .NE. 0.)) THEN
                IF((II .NE. III))FSMFRR(I,II)=FSMFRR(I,II)/FSMFRR(I,III)
                IF((II .EQ. NFIELD))FSMFRR(I,III)=1.
              END IF
            ELSE IF((MSMFXY .EQ. 2)) THEN
              DO 22671 IIII=1,NFIELD
                IF ((FSMFXY(I,III,IIIII) .NE. 0.)) THEN
                  IF (((II .NE. III) .OR. (IIII .NE. IIIII))) THEN
                    FSMFXY(I,II,IIII)=FSMFXY(I,II,IIII)/ FSMFXY(I,III,II
     *              III)
                    IF(((II .EQ. NFIELD) .AND. (IIII .EQ. NFIELD)))FSMFX
     *              Y(I,III,IIIII)=1.
                  END IF
                END IF
22671         CONTINUE
22672         CONTINUE
            END IF
22661     CONTINUE
22662     CONTINUE
          DO 22681 IIII=1,NFIELDe
            III=1
            DO 22691 II=2,NSMFEE
              IF ((FSMFBN1(I,IIII,II) .GT. FSMFBN1(I,IIII,III))) THEN
                III=II
              END IF
22691       CONTINUE
22692       CONTINUE
            DO 22701 II=1,NSMFEE
              IF((II .NE. III))FSMFBN1(I,IIII,II)=FSMFBN1(I,IIII,II)/FSM
     *        FBN1(I,IIII,III)
              IF((II .EQ. NSMFEE))FSMFBN1(I,IIII,III)=1.
22701       CONTINUE
22702       CONTINUE
22681     CONTINUE
22682     CONTINUE
22631   CONTINUE
22632   CONTINUE
        WRITE(6,22710)
22710   FORMAT(//'Name of file to save data for graph plotting:'/' ',$)
        READ(5,22720)SPCNAM
22720   FORMAT(A80)
        WRITE(6,22730)SPCNAM
22730   FORMAT(' File name input is:', A80)
        OPEN(UNIT=3,STATUS='UNKNOWN',FILE=SPCNAM)
        MSTART=-1
22740   CONTINUE
        TEEE=0.
        TPPP=0.
        DO 22751 I=1,NSMFPS
          TENERGY=0.0
          TPARTICLE=0
22760     WRITE(6,22770)I,IQSMFP(I)
22770     FORMAT(/' PLOT ENERGY/PLANAR FLUENCE FOR SOURCE',I3,' (CHARGE=
     *',I3,')?'/)
          WRITE(6,22780)
22780     FORMAT(' (0) - No')
          IF ((ESTYPE .EQ. 0)) THEN
            WRITE(6,22790)
22790       FORMAT(' (1) - E inside treatment field')
            WRITE(6,22800)
22800       FORMAT(' (2) - E outside treatment field')
          ELSE
            WRITE(6,22810)
22810       FORMAT(' (3) - E annulus')
          END IF
          WRITE(6,22820)
22820     FORMAT(' (4) - Fluence')
          READ(5,22830)CHOICE
22830     FORMAT(I8)
          IF (((CHOICE.EQ.1 .AND. ESTYPE.EQ.1) .OR. (CHOICE.EQ.2 .AND. E
     *    STYPE.EQ.1))) THEN
            WRITE(6,22840)
22840       FORMAT(//' Not a valid energy option with this field type.'/
     *' Try again.'//)
            GOTO 22760
          ELSE IF((CHOICE.EQ.3 .AND. ESTYPE.EQ.0)) THEN
            WRITE(6,22850)
22850       FORMAT(//' You must have different energy spectra to'/ ' plo
     *t energy in an arbitrary radial bin.'/ ' Try again.'//)
            GOTO 22760
          ELSE IF((CHOICE .NE. 0)) THEN
            MSTART=MSTART+1
          END IF
          IF ((CHOICE .EQ. 1).OR.(CHOICE .EQ. 2).OR.(CHOICE .EQ. 3)) THE
     *    N
            IF ((CHOICE .EQ. 1)) THEN
              II=1
            ELSE IF((CHOICE .EQ. 2)) THEN
              II=2
            ELSE IF((CHOICE .EQ. 3)) THEN
22861         CONTINUE
                WRITE(6,22870)NFIELDe
22870           FORMAT(' There are ',I3,' different annuluses')
                WRITE(6,22880)
22880           FORMAT(' Input the number of the annulus: ',$)
                READ(5,22890)II
22890           FORMAT(I3)
                IF(((II.GT.0).AND.(II.LE.NFIELDe)))GO TO22862
              GO TO 22861
22862         CONTINUE
            END IF
            GRAPHTITLE='spectral distribution'
            YTITLE='relative particles/bin'
            XTITLE='energy /MeV'
            DO 22901 III=1,NSMFEE
              XPLOT(III)=SMFMXE*FLOAT(III)/FLOAT(NSMFEE)
              YPLOT(III)=0
              IF((FSMFBN1(I,II,III).GT.0))YPLOT(III)=FSMFBN1(I,II,III)
              ERRYPLOT(III)=0.
              TENERGY=TENERGY+YPLOT(III)*XPLOT(III)
              TPARTICLE=TPARTICLE+YPLOT(III)
22901       CONTINUE
22902       CONTINUE
            IF ((TPARTICLE.GT.0)) THEN
              WRITE(6,22910)TENERGY/TPARTICLE
22910         FORMAT(/' MEAN E FOR THIS SOURCE INSIDE TREATMENT FIELD=',
     *F10.3,'MeV')
              TEEE=TEEE+TENERGY/TPARTICLE*SMFRTN(I)
              TPPP=TPPP+SMFRTN(I)
            ELSE
              WRITE(6,22920)
22920         FORMAT(/' NO PARTICLES FROM THIS SOURCE INSIDE TREATMENT F
     *IELD')
            END IF
            IF ((TPPP.GT.0)) THEN
              WRITE(6,22930)TEEE/TPPP
22930         FORMAT(/' MEAN E FOR ALL THE PARTICLES (SO FAR)=',F10.3,'M
     *eV')
            END IF
            HXMIN=SMFMNE
            ITYPE=1
            SERIESTITLE(1:7)='SOURCE'
            SERIESTITLE(8:9)=LETTER2(I)
            SERIESTITLE(10:15)=' IQ ='
            SERIESTITLE(16:17)=LETTER1(IQSMFP(I)+2)
            IF ((CHOICE .EQ. 1)) THEN
              SERIESTITLE(18:60)=' IN TREATMENT FIELD'
            ELSE IF((CHOICE .EQ. 2)) THEN
              SERIESTITLE(18:60)=' OUTSIDE TREATMENT FIELD'
            ELSE IF((CHOICE .EQ. 3)) THEN
              IF ((II.EQ.1)) THEN
                SERIESTITLE(18:60)=' IN R = 0.00 - ' //LETTER2(INT(ERADI
     *          I(II)))//'.' //LETTER2(INT(100*(ERADII(II)-INT(ERADII(II
     *          )))))
              ELSE
                SERIESTITLE(18:60)=' IN R = ' //LETTER2(INT(ERADII(II-1)
     *          ))//'.' //LETTER2(INT(100*(ERADII(II-1)-INT(ERADII(II-1)
     *          ))))//' - ' //LETTER2(INT(ERADII(II)))//'.' //LETTER2(IN
     *          T(100*(ERADII(II)-INT(ERADII(II)))))
              END IF
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NSMFEE,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          ELSE IF((CHOICE .EQ. 4)) THEN
            GRAPHTITLE='planar fluence distribution'
            YTITLE='relative particles/bin'
            ITYPE=1
            SERIESTITLE(1:20)='PLANAR-F FOR SOURCE'
            SERIESTITLE(21:23)=LETTER2(I)
            SERIESTITLE(24:28)=' IQ ='
            SERIESTITLE(29:60)=LETTER1(IQSMFP(I)+2)
            IF ((MSMFXY .EQ. 0)) THEN
              XTITLE='radius /cm'
            ELSE IF((MSMFXY .EQ. 1)) THEN
              XTITLE='half-side /cm'
            ELSE IF((MSMFXY .EQ. 2)) THEN
              WRITE(6,22940)NFIELD
22940         FORMAT(' ORIENTATION (0: ALONG X, 1: Y), BIN NUMBER (1 - '
     *,I3,'):' /' ',$)
              READ(5,22950)II,III
22950         FORMAT(2I8)
              IF ((II .EQ. 0)) THEN
                XTITLE='X /cm '
                HXMIN=XFLDMN
                SERIESTITLE(32:40)=' Y-BIN #:'
                SERIESTITLE(41:60)=LETTER2(III)
              ELSE IF((II .EQ. 1)) THEN
                XTITLE='Y /cm '
                HXMIN=YFLDMN
                SERIESTITLE(32:40)=' X-BIN #:'
                SERIESTITLE(41:60)=LETTER2(III)
              END IF
            END IF
            IF ((MSMFXY .EQ. 0).OR.(MSMFXY .EQ. 1)) THEN
              DO 22961 II=1,NFIELDi
                XPLOT(II)=RTREAT*SQRT((FLOAT(II))/(FLOAT(NFIELDi)))
                YPLOT(II)=FSMFRR(I,II)
                ERRYPLOT(II)=0.
                HXMIN=0
22961         CONTINUE
22962         CONTINUE
              DO 22971 II=NFIELDi+1,NFIELD
                XPLOT(II)=SQRT(((RFIELD**2-RTREAT**2)* ((FLOAT(II-NFIELD
     *          i))/(FLOAT(NFIELDo))))+RTREAT**2)
                YPLOT(II)=FSMFRR(I,II)*(((RTREAT**2/FLOAT(nFIELDi)))/ ((
     *          RFIELD**2-RTREAT**2)/FLOAT(NFIELDo)))
                ERRYPLOT(II)=0.
                HXMIN=0
22971         CONTINUE
22972         CONTINUE
            ELSE IF((MSMFXY .EQ. 2)) THEN
              IF ((II .EQ. 0)) THEN
                DO 22981 II=1,NFIELD
                  XPLOT(II)=(XFLDMN+(XFLDMX-XFLDMN)*FLOAT(II)/ FLOAT(NFI
     *            ELD))
                  YPLOT(II)=FSMFXY(I,II,III)
                  ERRYPLOT(II)=0.
                  WRITE(6,*)XFLDMN,XFLDMX,XPLOT(II),YPLOT(II)
22981           CONTINUE
22982           CONTINUE
              ELSE IF((II .EQ. 1)) THEN
                DO 22991 II=1,NFIELD
                  XPLOT(II)=(YFLDMN+(YFLDMX-YFLDMN)*FLOAT(II)/ FLOAT(NFI
     *            ELD))
                  YPLOT(II)=FSMFXY(I,III,II)
                  ERRYPLOT(II)=0.
                  WRITE(6,*)YFLDMN,YFLDMX,XPLOT(II),YPLOT(II)
22991           CONTINUE
22992           CONTINUE
              END IF
            END IF
            CALL XVGRPLOT(XPLOT,YPLOT,ERRYPLOT,NFIELD,MSTART, SERIESTITL
     *      E,XTITLE,YTITLE,GRAPHTITLE,PSDNAM,3,ITYPE,HXMIN,0)
          END IF
22751   CONTINUE
22752   CONTINUE
        WRITE(6,23000)
23000   FORMAT(/' PLOT ANOTHER ENERGY/PLANAR FLUENCE INTO THE SAME FILE?
     *'/ ' (Note: energy and fluence should be in separate file!)'/)
        WRITE(6,23010)
23010   FORMAT(' INPUT (1) TO CONTINUE OR (0) TO QUIT:'/' ',$)
        READ(5,23020)CHOICE
23020   FORMAT(I8)
        IF ((CHOICE .EQ. 1)) THEN
          GOTO 22740
        END IF
        CLOSE(UNIT=3,STATUS='KEEP')
        CALL XVGR_SCRIPT
      END IF
22300 CONTINUE
      WRITE(6,23030)
23030 FORMAT(/' WOULD YOU LIKE TO RUN THIS PROGRAMME AGAIN?')
      WRITE(6,23040)
23040 FORMAT(/' INPUT (1) TO CONTINUE OR (0) TO QUIT:'/' ',$)
      READ(5,23050)CHOICE
23050 FORMAT(I8)
      IF ((CHOICE .EQ. 1)) THEN
        EGSPERT = .true.
        GOTO 20010
      END IF
      RETURN
      END
      SUBROUTINE READ_PHSP
      IMPLICIT NONE
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      COMMON/DOUBLES/ KWEI,SMFRTN, FSMFBN1,FSMABN1,FSMABN2,FSMFXX,FSMFYY
     *,FSMFR1,FSMFR2,FSMFR11,FSMFR12, FSMFRR,FSMFXY,BINSIZE,RBin,RBin_X,
     *RBin_Y
      REAL*8 KWEI, SMFRTN(22),  FSMFBN1(22,200,200), FSMABN1(200), FSMAB
     *N2(200), FSMFXX(22,200), FSMFYY(22,200), FSMFR1(22,200), FSMFR2(22
     *,200), FSMFR11(22,200), FSMFR12(22,200), FSMFRR(22,200), FSMFXY(22
     *,200,200), BINSIZE(200), RBin,RBin_X,RBin_Y
      COMMON/REALS/ XMIN2,XMAX2, XPLOT,HXMIN, YPLOT,YPLOTT, ERRYPLOT,X2Y
     *2,ZTOTAL,RSSD02,RSSD12,SSDVPS, SFACTOR,RFACTOR,RFACTOR1,RFACTOR2,
     *X,Y,Z,RRFACTOR,SSFACTOR,RSSD0,RSSD1, WEIGHT,EI, PARAEMAX2,PARAEMAX
     *1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAEMNE, PARANINC1,PARANINC2,PARA
     *NINC, EREAD,ZLAST, MUIDX,XIN,YIN,ZIN,UIN,VIN,WIN, SMFX2,SMFY2,SMFM
     *XR, HFIELD, RSCATT, DELTAZ,RSSDST,SSSDST,ERADII,E_ph_sp_e, E_ph_sp
     *_g, E_ph_sp_p, E_MIN_ph_sp, E_MAX_ph_sp, RFIELD,RFIELDe,XFLDMN,XFL
     *DMX, YFLDMN, YFLDMX, SMFMNZ, SMFMXZ, SMFMNX, SMFMXX, SMFMNY, SMFMX
     *Y, SMFMNR,RTREAT,RTREATe,XTREATMN,XTREATMX, YTREATMN,YTREATMX, SMF
     *MNE,SMFMXE, ABNTOTAL,ABNTOTALH,FLUAREA,NINC, RMIN_MIN,RMIN_MAX,RMA
     *X_MIN,RMAX_MAX,DOMEGA, SUM_TMP,SUM_TMP2,Z_SCORE,mumin,mumax
      real*8 XMIN2,XMAX2,  XPLOT(200),HXMIN,  YPLOT(200),YPLOTT, ERRYPLO
     *T(200), X2Y2,  ZTOTAL(22),  RSSD02,RSSD12,  SSDVPS(22+3),  SFACTOR
     *,RFACTOR,RFACTOR1,RFACTOR2,X,Y,Z,RRFACTOR,SSFACTOR, RSSD0,RSSD1,
     *WEIGHT,EI, PARAEMAX2,PARAEMAX1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAE
     *MNE, PARANINC1,PARANINC2,PARANINC, EREAD,ZLAST,MUIDX, XIN,YIN,ZIN,
     *UIN, VIN,WIN,  SMFX2(22), SMFY2(22),  SMFMXR(22),  HFIELD(22),  RS
     *CATT,  DELTAZ,  RSSDST(22,2000), SSSDST(22,2000), ERADII(200),  E_
     *ph_sp_e,  E_ph_sp_g,  E_ph_sp_p,  E_MIN_ph_sp,  E_MAX_ph_sp,  RFIE
     *LD,  RFIELDe,   XFLDMN,  XFLDMX,  YFLDMN,  YFLDMX,  SMFMNZ(22),
     *SMFMXZ(22),  SMFMNX(22),  SMFMXX(22),  SMFMNY(22),  SMFMXY(22),  S
     *MFMNR(22),  RTREAT,  RTREATe,   XTREATMN,  XTREATMX,  YTREATMN,  Y
     *TREATMX,  SMFMNE,SMFMXE,  ABNTOTAL,ABNTOTALH,  FLUAREA,   NINC,  R
     *MIN_MIN, RMIN_MAX,  RMAX_MIN,  RMAX_MAX, DOMEGA,  SUM_TMP(200),  S
     *UM_TMP2(200),  Z_SCORE,  mumin,mumax
      COMMON/INTEGERS/ PARANOT,PARANOT1,PARANOT2,PARANOP,PARANOP1, PARAN
     *OP2,IPARANOT,IPARANOT1,IPARANOT2, LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST, I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBER
     *ST,NBIT1,NBIT2, NFIELDi,NFIELDo,NFIELDe, NWRONG,LATCHJ,CHOICE,CHOI
     *CE1, K1,K2,K3,K4,IA, IX,IY,IR,K,IQ,LATCHI,IE, NPASSI,LATCH,SMFPXY,
     *NFIELD,NSMFPS,I_IN_EX, ISMFPS,IQSMFP,MSTART,MSMFXY,NSMFEE,NPASS_ph
     *_sp, N_ph_sp_e, N_ph_sp_g, N_ph_sp_p, FLUTYPE,ANGTYPE,IZLAST1, IZL
     *AST2,IMUIDX1,IMUIDX2,IZSCORE1,IZSCORE2,JUSTONE, MAXSCATTER,NUMSCAT
     *TER,ESTYPE,i_iaea_in,i_iaea_out,i_log, i_unit_in,i_unit_out
      INTEGER*8 PARANOT,PARANOT1,PARANOT2, PARANOP,PARANOP1,  PARANOP2,I
     *PARANOT,IPARANOT1,IPARANOT2,  LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST(200)
      INTEGER I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBERST,NBIT1,NBI
     *T2,  NWRONG,LATCHJ(29), CHOICE,CHOICE1,  K1,K2,K3,K4,IA, IX,IY,IR,
     *K(22), IQ,LATCHI,IE, NPASSI,  LATCH(22),   SMFPXY(22),   NFIELD,
     * NFIELDi,   NFIELDo,   NFIELDe,  NSMFPS,  I_IN_EX,  ISMFPS(22),
     *IQSMFP(22),   MSTART,   MSMFXY,  NSMFEE,  NPASS_ph_sp,  N_ph_sp_e,
     *  N_ph_sp_g,  N_ph_sp_p,  FLUTYPE,  ANGTYPE,  IZLAST1,  IZLAST2,
     *IMUIDX1,   IMUIDX2,  IZSCORE1,  IZSCORE2,  JUSTONE(200),  MAXSCATT
     *ER,  NUMSCATTER,  ESTYPE ,  i_iaea_in,  i_iaea_out,   i_log,   i_u
     *nit_in,i_unit_out
      COMMON/LOGICALS/OLDFILE,EGSPERT
      LOGICAL OLDFILE,    EGSPERT
      COMMON/RWPHSP/ STRING_TEMP_ZLAST_OUT,  STRING_TEMP_OUT,  IHSTRY_PH
     *SP,   iaea_dummy_long,  NHSTRY_DOS,   ESHORT,  WEIGHTTMP,  WT_PHSP
     *_SHORT,  X_PHSP_SHORT,  Y_PHSP_SHORT,  Z_PHSP_SHORT,  U_PHSP_SHORT
     *,  V_PHSP_SHORT,  W_PHSP_SHORT,  ZLAST_PHSP_SHORT,  MUIDX_PHSP_SHO
     *RT,  EKMAX_PHSP_SHORT,  EKMINE_PHSP_SHORT,  NINC_PHSP_SHORT,  dosx
     *yz2beam_index,  iaea_extra_floats,  NUM_PHSP_TOT,  PHOT_PHSP_TOT,
     *iaea_result,   iaea_n_stat,  iaea_q_typ,  iaea_typ_q,  iaea_q_inde
     *x,  I_PHSP,  IERR_PHSP,  LATCHTMP,  WRITE_PHSP_COUNTER,  WRITE_PHS
     *P_SOFAR,  PHSP_RESTART,  PHSP_RECL,  PHSP_RECL_OLD,  iaea_extra_in
     *ts,  iaea_extra_int_types,iaea_extra_float_types,   iaea_i_zlast,
     * iaea_i_latch,  iaea_i_muidx,  iaea_n_extra_ints,  iaea_n_extra_fl
     *oats,  i_iaea_open_for_write,   iaea_i_zlast_out,  iaea_i_latch_ou
     *t,  iaea_i_muidx_out,  dosxyz2beam_izscore,  MODE_RW
      CHARACTER*32000 STRING_TEMP_ZLAST_OUT(1)
      CHARACTER*28000 STRING_TEMP_OUT(1)
      INTEGER*8 IHSTRY_PHSP(1),iaea_dummy_long,NHSTRY_DOS
      REAL*4 ESHORT,WEIGHTTMP,WT_PHSP_SHORT,X_PHSP_SHORT,Y_PHSP_SHORT, Z
     *_PHSP_SHORT,U_PHSP_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,ZLAST_PHSP_SHO
     *RT, MUIDX_PHSP_SHORT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP
     *_SHORT,dosxyz2beam_index, iaea_extra_floats(10)
      INTEGER NUM_PHSP_TOT,PHOT_PHSP_TOT, iaea_result,iaea_n_stat,iaea_q
     *_typ(3),iaea_typ_q(3),iaea_q_index,I_PHSP, IERR_PHSP,LATCHTMP, WRI
     *TE_PHSP_COUNTER(1),WRITE_PHSP_SOFAR(1), PHSP_RESTART,PHSP_RECL,PHS
     *P_RECL_OLD,iaea_extra_ints(10), iaea_extra_int_types(10),iaea_extr
     *a_float_types(10),iaea_i_zlast, iaea_i_latch,iaea_i_muidx,iaea_n_e
     *xtra_ints,iaea_n_extra_floats, i_iaea_open_for_write,iaea_i_zlast_
     *out,iaea_i_latch_out, iaea_i_muidx_out,dosxyz2beam_izscore
      CHARACTER*5 MODE_RW
      INTEGER LATCH_TEMP_OUT,NUM_PHSP_TOFLUSH,TEMP_PHSP_COUNTER,REM_PHSP
      REAL*4 REAL_TEMP_OUT(7)
      CHARACTER*32 REC_TEMP_OUT
      i_log=6
      E_MIN_ph_sp = 1000.0
      E_MAX_ph_sp = 0.0
      RSSD0=RSSD1-RSSD1/40.
      RSSD02=RSSD0*RSSD0
      RSSD12=RSSD1*RSSD1
      DO 7011 III=1,2000
        DO 7021 IIII=1,22
          RSSDST(IIII,III)=0.0
7021    CONTINUE
7022    CONTINUE
7011  CONTINUE
7012  CONTINUE
      DO 7031 III=1,200
        FSMABN1(III)=0.0
7031  CONTINUE
7032  CONTINUE
      DO 7041 III=1,NSMFPS
        SMFRTN(III)=0.0
        DO 7051 IIII=1,NFIELD
          FSMFRR(III,IIII)=0.0
          FSMFXX(III,IIII)=0.0
          FSMFYY(III,IIII)=0.0
          DO 7061 IIIII=1,NFIELD
            FSMFXY(III,IIII,IIIII)=0.0
7061      CONTINUE
7062      CONTINUE
7051    CONTINUE
7052    CONTINUE
        DO 7071 IIII=1,NSMFEE
          DO 7081 IIIII=1,NFIELD
            FSMFBN1(III,IIIII,IIII)=0.0
7081      CONTINUE
7082      CONTINUE
7071    CONTINUE
7072    CONTINUE
7041  CONTINUE
7042  CONTINUE
      N_ph_sp_e=0
      N_ph_sp_g=0
      N_ph_sp_p=0
      NPASS_ph_sp=0
      E_ph_sp_e=0.0
      E_ph_sp_g=0.0
      E_ph_sp_p=0.0
      E_MAX_ph_sp=0.0
      E_MIN_ph_sp = 1000.0
      SFACTOR=FLOAT(NSMFEE)/SMFMXE
      SSFACTOR=FLOAT(200)/0.1745
      IF ((RFIELD .EQ. 0.0).AND.(MSMFXY .NE. 2)) THEN
        RFIELD=0.0001
        WRITE(6,7090)
7090    FORMAT(/' RADIUS RE-SET TO 0.0001 CM')
      END IF
      IF ((RFIELD .EQ. 0.)) THEN
        RFACTOR=FLOAT(NFIELD)
      ELSE
        RFACTOR=FLOAT(NFIELD)/(RFIELD**2)
      END IF
      RFACTOR1 = FLOAT(NFIELDi)/(RTREAT**2)
      RFACTOR2 = FLOAT(NFIELDo)/(RFIELD**2-RTREAT**2)
      IF (((YFLDMX-YFLDMN) .EQ. 0.)) THEN
        Y=FLOAT(NFIELD)
      ELSE
        Y=FLOAT(NFIELD)/(YFLDMX-YFLDMN)
      END IF
      IF (((XFLDMX-XFLDMN) .EQ. 0.)) THEN
        X=FLOAT(NFIELD)
      ELSE
        X=FLOAT(NFIELD)/(XFLDMX-XFLDMN)
      END IF
7100  CONTINUE
      WRITE(6,7110)
7110  FORMAT(/' BEGIN READING PH-SP DATA .....')
      IPARANOT=1
      NHSTRY=0
7121  CONTINUE
7130    CONTINUE
        IPARANOT=IPARANOT+1
        IF ((IPARANOT .EQ. PARANOT+2)) THEN
          GOTO 7140
        END IF
        IF ((i_iaea_in.EQ.1)) THEN
          NPASSI=0
          call iaea_get_particle(i_unit_in,iaea_n_stat,iaea_q_index,ESHO
     *    RT,WT_PHSP_SHORT, X_PHSP_SHORT,Y_PHSP_SHORT,Z_PHSP_SHORT,U_PHS
     *    P_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,iaea_extra_floats,iaea_extr
     *    a_ints)
          IF ((iaea_n_stat.EQ.-1)) THEN
            WRITE(i_log,*)' Error getting particle data from IAEA phsp f
     *ile.'
            call exit(1)
          ELSE IF((iaea_n_stat.EQ.-2)) THEN
            WRITE(i_log,*)' WARNING: Reached end of IAEA phsp file.  Fil
     *e restarted.'
          ELSE IF((iaea_n_stat.GE.0)) THEN
            NHSTRY=NHSTRY+iaea_n_stat
            IF ((iaea_i_latch.EQ.-99)) THEN
              LATCHI=0
            ELSE
              LATCHI=iaea_extra_ints(iaea_i_latch)
            END IF
            IQ=iaea_typ_q(iaea_q_index)
            IF (( (IQ.EQ.1) .OR. (IQ.EQ.-1) )) THEN
              ESHORT = ESHORT + 0.5109989461
            END IF
            EI=ESHORT
            IF ((iaea_i_zlast.EQ.-99)) THEN
              ZLAST_PHSP_SHORT=0
            ELSE
              ZLAST_PHSP_SHORT= iaea_extra_floats(iaea_i_zlast)
            END IF
            IF ((iaea_i_muidx.EQ.-99)) THEN
              MUIDX_PHSP_SHORT=0
            ELSE
              MUIDX_PHSP_SHORT= iaea_extra_floats(iaea_i_muidx)
            END IF
            WEIGHT=WT_PHSP_SHORT
            XIN=X_PHSP_SHORT
            YIN=Y_PHSP_SHORT
            ZIN=Z_PHSP_SHORT
            UIN=U_PHSP_SHORT
            VIN=V_PHSP_SHORT
            WIN=W_PHSP_SHORT
            ZLAST=ZLAST_PHSP_SHORT
            MUIDX=MUIDX_PHSP_SHORT
          END IF
        ELSE
          IZLAST1=0
          IF((MODE_RW.EQ.'MODE2'))IZLAST1=1
          IF ((IZLAST1.NE.0)) THEN
            READ(2,REC=IPARANOT,IOSTAT=IERR_PHSP) LATCHI,ESHORT,X_PHSP_S
     *      HORT,Y_PHSP_SHORT, U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT,Z
     *      LAST_PHSP_SHORT
          ELSE
            READ(2,REC=IPARANOT,IOSTAT=IERR_PHSP) LATCHI,ESHORT,X_PHSP_S
     *      HORT,Y_PHSP_SHORT, U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT
          END IF
          IF ((IPARANOT.EQ.76695869)) THEN
            write(*,*)' x,y,u,v,wt,eshort,latch', X_PHSP_SHORT,Y_PHSP_SH
     *      ORT,U_PHSP_SHORT,V_PHSP_SHORT, WT_PHSP_SHORT,ESHORT,LATCHI
          END IF
          IF ((ESHORT.LT.0.0)) THEN
            NHSTRY=NHSTRY+1
            ESHORT=ABS(ESHORT)
          END IF
          EI=ESHORT
          WEIGHT=WT_PHSP_SHORT
          XIN=X_PHSP_SHORT
          YIN=Y_PHSP_SHORT
          UIN=U_PHSP_SHORT
          VIN=V_PHSP_SHORT
          IF((IZLAST1.NE.0))ZLAST=ZLAST_PHSP_SHORT
          IF ((IERR_PHSP.EQ.0)) THEN
            EI=ESHORT
            IF ((BTEST(LATCHI, 31))) THEN
              NPASSI = 1
              LATCHI = IBCLR(LATCHI,31)
            ELSE
              NPASSI = 0
            END IF
            IF ((BTEST(LATCHI, 30))) THEN
              IQ = -1
              LATCHI = IBCLR(LATCHI,30)
              LATCHI = IBCLR(LATCHI,29)
            ELSE
              IF ((BTEST(LATCHI, 29))) THEN
                IQ = 1
                LATCHI = IBCLR(LATCHI,29)
              ELSE
                IQ = 0
              END IF
            END IF
            WIN = min( 1., UIN**2 + VIN**2)
            WIN = sqrt(1. - WIN)
            WIN = SIGN(WIN,WEIGHT)
            WEIGHT=ABS(WEIGHT)
          END IF
        END IF
        IF ((NPASSI .NE. 0)) THEN
          NPASS_ph_sp = NPASS_ph_sp + 1
          GOTO 7130
        END IF
        IF ((IQ .EQ. -1)) THEN
          EI=EI-0.5109989461
          N_ph_sp_e = N_ph_sp_e + 1
          E_ph_sp_e = E_ph_sp_e+EI
        ELSE IF((IQ .EQ. 0)) THEN
          N_ph_sp_g = N_ph_sp_g + 1
          E_ph_sp_g = E_ph_sp_g+EI
        ELSE IF((IQ .EQ. +1)) THEN
          EI=EI-0.5109989461
          N_ph_sp_p = N_ph_sp_p + 1
          E_ph_sp_p = E_ph_sp_p+EI
        ELSE
          WRITE(6,7150)IQ
7150      FORMAT(/' Charge of the particle = ',I4,' ???'// ' Never seent
     *his kind of particle!!!'// ' Something must have gone WRONG!!!'/)
          STOP
        END IF
        E_MIN_ph_sp = MIN(E_MIN_ph_sp,EI)
        E_MAX_ph_sp = MAX(E_MAX_ph_sp,EI)
        IF ((SMFMNE .GT. E_MIN_ph_sp)) THEN
          WRITE(6,7160)IPARANOT-1,IQ,EI,SMFMNE
7160      FORMAT(/' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'/
     *' '/ '     For particle number', I13, '  of charge',I3/ ' The (min
     *.) energy read from the PH-SP file= ',F12.5// '     is smaller tha
     *n your input lower limit of', F10.3 / '              stopping now 
     *                   '/ ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     *!!!!!!!!'/)
          STOP
        END IF
        IF ((SMFMXE .LT. E_MAX_ph_sp)) THEN
          WRITE(6,7170)IPARANOT-1,IQ,EI,SMFMXE
7170      FORMAT(/' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'/
     *' '/ '     For particle number', I13, '  of charge',I3/ ' The (max
     *.) energy read from the PH-SP file= ',F12.5// '     is larger than
     * your input upper limit of', F10.3 / '              stopping now  
     *                  '/ ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     *!!!!!!!'/)
          STOP
        ELSE IF((EI .LT. 0.0)) THEN
          WRITE(6,7180)
7180      FORMAT(/' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'/
     *// ' '/ '     THE ENERGY READ FROM THE PH-SP DATA FILE '// '     I
     *S SMALLER THAN ZERO, ANYTHING WRONG?  '/// '              stopping
     * now                    '/ ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     *!!!!!!!!!!!!!'/)
          STOP
        END IF
        I=0
        II=0
7191    CONTINUE
          I=I+1
          IF((I .GT. NSMFPS))GOTO 7200
          III=LATCH(K(I))
          IF ((III.LE.0 .AND. IBITS(LATCHI,1,22).EQ.0 .AND. IQSMFP(K(I))
     *     .EQ. IQ)) THEN
            II=K(I)
            GOTO 7210
          ELSE IF((BTEST(LATCHI, III)).AND.(IQSMFP(K(I)) .EQ. IQ)) THEN
            II=K(I)
            GOTO 7210
          END IF
        GO TO 7191
7192    CONTINUE
7200    CONTINUE
        IF ((II .EQ. 0)) THEN
          IF ((IQ .EQ. -1)) THEN
            K1=K1+1
          ELSE IF((IQ .EQ. 0)) THEN
            K2=K2+1
          ELSE
            K3=K3+1
          END IF
        END IF
        GOTO 7130
7210    CONTINUE
        X2Y2=XIN*XIN+YIN*YIN
        IF ((ISMFPS(II) .EQ. 3).AND.(SMFMNR(II) .EQ. 0.0)) THEN
          IF ((X2Y2.LT.RSSD12).AND.(X2Y2.GT.RSSD02).AND.(WIN .NE. 0.0))
     *    THEN
            IF ((WIN .NE. 0.)) THEN
              RRFACTOR=DELTAZ/WIN
            ELSE
              RRFACTOR=DELTAZ
            END IF
            IIII=INT(10.*SQRT((XIN+RRFACTOR*UIN)*(XIN+RRFACTOR*UIN)+ (YI
     *      N+RRFACTOR*VIN)*(YIN+RRFACTOR*VIN)))+1
            IF ((IIII .LT. 2000)) THEN
              RSSDST(II,IIII)=RSSDST(II,IIII)+WEIGHT
            END IF
          END IF
          IF ((IQ .NE. 0)) THEN
            IF ((X2Y2 .LE. 0.25)) THEN
              IA=INT(ACOS(WIN)*SSFACTOR)+1
              IF (((IA .LE. 200) .AND. (EI .GE. SMFMXE/2.))) THEN
                FSMABN1(IA)=FSMABN1(IA)+WEIGHT
              END IF
            END IF
          END IF
        END IF
        IF ((MSMFXY .EQ. 0)) THEN
          IF ((X2Y2.LE.(RTREAT*RTREAT))) THEN
            IR=INT(X2Y2*RFACTOR1)+1
          ELSE IF((X2Y2.LE.(RFIELD*RFIELD))) THEN
            IR=INT((X2Y2-RTREAT**2)*RFACTOR2)+NFIELDi+1
          ELSE
            IR = NFIELD + 1
          END IF
          IF ((IR .GT. NFIELD)) THEN
            NWRONG=NWRONG+1
            GOTO 7130
          END IF
          FSMFRR(II,IR)=FSMFRR(II,IR)+WEIGHT
        ELSE IF((MSMFXY .EQ. 1)) THEN
          IF ((ABS(XIN) .LE. RTREAT)) THEN
            IX=INT(XIN*XIN*RFACTOR1)+1
            RBin_X=0.5*SQRT(RTREAT**2*(FLOAT(IX)/FLOAT(NFIELDi)))
          ELSE IF((ABS(XIN) .LE. RFIELD)) THEN
            IX=INT((XIN*XIN-RTREAT**2)*RFACTOR2)+NFIELDi+1
            RBin_X=0.5*SQRT((RFIELD**2-RTREAT**2)*(FLOAT(IX-NFIELDi)/ FL
     *      OAT(NFIELDo))+RTREAT**2)
          ELSE
            IX = NFIELD + 1
          END IF
          IF ((ABS(YIN) .LE. RTREAT)) THEN
            IY=INT(YIN*YIN*RFACTOR1)+1
            RBin_Y=0.5*RTREAT*SQRT(FLOAT(IY)/FLOAT(NFIELDi))
          ELSE IF((ABS(YIN) .LE. RFIELD)) THEN
            IY=INT((YIN*YIN-RTREAT**2)*RFACTOR2)+NFIELDi+1
            RBin_Y=0.5*SQRT((RFIELD**2-RTREAT**2)*(FLOAT(IY-NFIELDi)/ FL
     *      OAT(NFIELDo))+RTREAT**2)
          ELSE
            IY = NFIELD + 1
          END IF
          IF (((IX .GT. NFIELD) .OR. (IY .GT. NFIELD))) THEN
            NWRONG=NWRONG+1
            GOTO 7130
          END IF
          IF ((IX .LE. IY)) THEN
            FSMFRR(II,IY)=FSMFRR(II,IY)+WEIGHT
            IF ((ABS(XIN) .LE. RBin_Y)) THEN
              FSMFR1(II,IY)=FSMFR1(II,IY)+WEIGHT
            ELSE
              FSMFR2(II,IY)=FSMFR2(II,IY)+WEIGHT
            END IF
            IF((IX .EQ. IY))FSMFR2(II,IY)=FSMFR2(II,IY)+WEIGHT
          ELSE
            FSMFRR(II,IX)=FSMFRR(II,IX)+WEIGHT
            IF ((ABS(YIN) .LE. RBin_X)) THEN
              FSMFR1(II,IX)=FSMFR1(II,IX)+WEIGHT
            ELSE
              FSMFR2(II,IX)=FSMFR2(II,IX)+WEIGHT
            END IF
          END IF
        ELSE IF((MSMFXY .EQ. 2)) THEN
          IF (((XIN-XFLDMN) .GE. 0.0)) THEN
            IX=INT((XIN-XFLDMN)*X)+1
          ELSE
            IX=0
          END IF
          IF (((YIN-YFLDMN) .GE. 0.0)) THEN
            IY=INT((YIN-YFLDMN)*Y)+1
          ELSE
            IY=0
          END IF
          IF (((IX .GT. NFIELD) .OR. (IY .GT. NFIELD))) THEN
            NWRONG=NWRONG+1
            GOTO 7130
          ELSE IF(((IX .LT. 1) .OR. (IY .LT. 1))) THEN
            NWRONG=NWRONG+1
            GOTO 7130
          END IF
          FSMFXY(II,IX,IY)=FSMFXY(II,IX,IY)+WEIGHT
        END IF
        SMFRTN(II)=SMFRTN(II)+WEIGHT
        IE=INT(EI*SFACTOR)+1
        IF((IE .GT. NSMFEE))IE=NSMFEE
        IF ((MSMFXY .EQ. 0)) THEN
          DO 7221 III=1,NFIELDe
            IF (( SQRT(XIN*XIN+YIN*YIN) .LE. ERADII(III) )) THEN
              FSMFBN1(II,III,IE) = FSMFBN1(II,III,IE)+WEIGHT
              GO TO7222
            END IF
7221      CONTINUE
7222      CONTINUE
        ELSE IF((MSMFXY .EQ. 1)) THEN
          DO 7231 III=1,NFIELDe
            IF (((ABS(XIN) .LE. ERADII(III)) .AND. (ABS(YIN) .LE. ERADII
     *      (III)))) THEN
              FSMFBN1(II,III,IE) = FSMFBN1(II,III,IE)+WEIGHT
              GO TO7232
            END IF
7231      CONTINUE
7232      CONTINUE
        ELSE IF((MSMFXY .EQ. 2)) THEN
          IF (((XIN .LE. XTREATMX) .AND. (YIN .LE. YTREATMX) .AND. (XIN
     *    .GT. XTREATMN) .AND. (YIN .GT. YTREATMN))) THEN
            FSMFBN1(II,1,IE)=FSMFBN1(II,1,IE)+WEIGHT
          ELSE
            FSMFBN1(II,2,IE)=FSMFBN1(II,2,IE)+WEIGHT
          END IF
        END IF
        K4=K4+1
        KWEI=KWEI+WEIGHT
      GO TO 7121
7122  CONTINUE
7140  CONTINUE
      RETURN
      END
      SUBROUTINE READNAME
      IMPLICIT NONE
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      COMMON/DOUBLES/ KWEI,SMFRTN, FSMFBN1,FSMABN1,FSMABN2,FSMFXX,FSMFYY
     *,FSMFR1,FSMFR2,FSMFR11,FSMFR12, FSMFRR,FSMFXY,BINSIZE,RBin,RBin_X,
     *RBin_Y
      REAL*8 KWEI, SMFRTN(22),  FSMFBN1(22,200,200), FSMABN1(200), FSMAB
     *N2(200), FSMFXX(22,200), FSMFYY(22,200), FSMFR1(22,200), FSMFR2(22
     *,200), FSMFR11(22,200), FSMFR12(22,200), FSMFRR(22,200), FSMFXY(22
     *,200,200), BINSIZE(200), RBin,RBin_X,RBin_Y
      COMMON/REALS/ XMIN2,XMAX2, XPLOT,HXMIN, YPLOT,YPLOTT, ERRYPLOT,X2Y
     *2,ZTOTAL,RSSD02,RSSD12,SSDVPS, SFACTOR,RFACTOR,RFACTOR1,RFACTOR2,
     *X,Y,Z,RRFACTOR,SSFACTOR,RSSD0,RSSD1, WEIGHT,EI, PARAEMAX2,PARAEMAX
     *1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAEMNE, PARANINC1,PARANINC2,PARA
     *NINC, EREAD,ZLAST, MUIDX,XIN,YIN,ZIN,UIN,VIN,WIN, SMFX2,SMFY2,SMFM
     *XR, HFIELD, RSCATT, DELTAZ,RSSDST,SSSDST,ERADII,E_ph_sp_e, E_ph_sp
     *_g, E_ph_sp_p, E_MIN_ph_sp, E_MAX_ph_sp, RFIELD,RFIELDe,XFLDMN,XFL
     *DMX, YFLDMN, YFLDMX, SMFMNZ, SMFMXZ, SMFMNX, SMFMXX, SMFMNY, SMFMX
     *Y, SMFMNR,RTREAT,RTREATe,XTREATMN,XTREATMX, YTREATMN,YTREATMX, SMF
     *MNE,SMFMXE, ABNTOTAL,ABNTOTALH,FLUAREA,NINC, RMIN_MIN,RMIN_MAX,RMA
     *X_MIN,RMAX_MAX,DOMEGA, SUM_TMP,SUM_TMP2,Z_SCORE,mumin,mumax
      real*8 XMIN2,XMAX2,  XPLOT(200),HXMIN,  YPLOT(200),YPLOTT, ERRYPLO
     *T(200), X2Y2,  ZTOTAL(22),  RSSD02,RSSD12,  SSDVPS(22+3),  SFACTOR
     *,RFACTOR,RFACTOR1,RFACTOR2,X,Y,Z,RRFACTOR,SSFACTOR, RSSD0,RSSD1,
     *WEIGHT,EI, PARAEMAX2,PARAEMAX1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAE
     *MNE, PARANINC1,PARANINC2,PARANINC, EREAD,ZLAST,MUIDX, XIN,YIN,ZIN,
     *UIN, VIN,WIN,  SMFX2(22), SMFY2(22),  SMFMXR(22),  HFIELD(22),  RS
     *CATT,  DELTAZ,  RSSDST(22,2000), SSSDST(22,2000), ERADII(200),  E_
     *ph_sp_e,  E_ph_sp_g,  E_ph_sp_p,  E_MIN_ph_sp,  E_MAX_ph_sp,  RFIE
     *LD,  RFIELDe,   XFLDMN,  XFLDMX,  YFLDMN,  YFLDMX,  SMFMNZ(22),
     *SMFMXZ(22),  SMFMNX(22),  SMFMXX(22),  SMFMNY(22),  SMFMXY(22),  S
     *MFMNR(22),  RTREAT,  RTREATe,   XTREATMN,  XTREATMX,  YTREATMN,  Y
     *TREATMX,  SMFMNE,SMFMXE,  ABNTOTAL,ABNTOTALH,  FLUAREA,   NINC,  R
     *MIN_MIN, RMIN_MAX,  RMAX_MIN,  RMAX_MAX, DOMEGA,  SUM_TMP(200),  S
     *UM_TMP2(200),  Z_SCORE,  mumin,mumax
      COMMON/INTEGERS/ PARANOT,PARANOT1,PARANOT2,PARANOP,PARANOP1, PARAN
     *OP2,IPARANOT,IPARANOT1,IPARANOT2, LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST, I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBER
     *ST,NBIT1,NBIT2, NFIELDi,NFIELDo,NFIELDe, NWRONG,LATCHJ,CHOICE,CHOI
     *CE1, K1,K2,K3,K4,IA, IX,IY,IR,K,IQ,LATCHI,IE, NPASSI,LATCH,SMFPXY,
     *NFIELD,NSMFPS,I_IN_EX, ISMFPS,IQSMFP,MSTART,MSMFXY,NSMFEE,NPASS_ph
     *_sp, N_ph_sp_e, N_ph_sp_g, N_ph_sp_p, FLUTYPE,ANGTYPE,IZLAST1, IZL
     *AST2,IMUIDX1,IMUIDX2,IZSCORE1,IZSCORE2,JUSTONE, MAXSCATTER,NUMSCAT
     *TER,ESTYPE,i_iaea_in,i_iaea_out,i_log, i_unit_in,i_unit_out
      INTEGER*8 PARANOT,PARANOT1,PARANOT2, PARANOP,PARANOP1,  PARANOP2,I
     *PARANOT,IPARANOT1,IPARANOT2,  LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST(200)
      INTEGER I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBERST,NBIT1,NBI
     *T2,  NWRONG,LATCHJ(29), CHOICE,CHOICE1,  K1,K2,K3,K4,IA, IX,IY,IR,
     *K(22), IQ,LATCHI,IE, NPASSI,  LATCH(22),   SMFPXY(22),   NFIELD,
     * NFIELDi,   NFIELDo,   NFIELDe,  NSMFPS,  I_IN_EX,  ISMFPS(22),
     *IQSMFP(22),   MSTART,   MSMFXY,  NSMFEE,  NPASS_ph_sp,  N_ph_sp_e,
     *  N_ph_sp_g,  N_ph_sp_p,  FLUTYPE,  ANGTYPE,  IZLAST1,  IZLAST2,
     *IMUIDX1,   IMUIDX2,  IZSCORE1,  IZSCORE2,  JUSTONE(200),  MAXSCATT
     *ER,  NUMSCATTER,  ESTYPE ,  i_iaea_in,  i_iaea_out,   i_log,   i_u
     *nit_in,i_unit_out
      COMMON/LOGICALS/OLDFILE,EGSPERT
      LOGICAL OLDFILE,    EGSPERT
      INTEGER lnblnk1,len
      IF (.NOT.EGSPERT) THEN
        CALL HELP_MESSAGE(61)
      END IF
10010 WRITE(6,10020)
10020 FORMAT(/' Name of file containing phase space data (with ext., < A
     *100):'/' ',$)
      IF (OLDFILE) THEN
        WRITE(6,10030)PSDNAM
10030   FORMAT(/' The old file was      ',A100/ ' Do you want to changei
     *t? ',$)
        READ(5,10040)C1
10040   FORMAT(A1)
        IF((C1.EQ.'?'))GO TO 10050
        IF ((C1.EQ.'y').OR.(C1.EQ.'Y')) THEN
          WRITE(6,10060)
10060     FORMAT(/' Name of file containing phase space data (with ext.,
     * < A100):' /' ',$)
        ELSE
          GOTO 10070
        END IF
      END IF
      READ(5,'(A100)',ERR=10050)PSDNAM
      WRITE(6,10080)PSDNAM
10080 FORMAT(' File name input is:', A100)
      i_iaea_in=0
      len=lnblnk1(PSDNAM)
      IF ((PSDNAM(len-8:len).EQ.'.IAEAphsp')) THEN
        i_iaea_in=1
        PSDNAM=PSDNAM(:len-9)//char(0)
        WRITE(6,10090)
10090   FORMAT(/' Data is in IAEA format.'/)
      END IF
      GOTO 10070
10050 CALL HELP_MESSAGE(50)
      GOTO 10010
10070 CONTINUE
      RETURN
      END
      SUBROUTINE OPENFILE
      IMPLICIT NONE
      COMMON/CHARACTERS/ MODE_RW1,MODE_RW2,C1,C2,AC,PSDNAM, SPCNAM,OLDNA
     *M, SSMFPS, DSMFPS,GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE,SCRIPT, LET
     *TER1,LETTER2
      CHARACTER*5 MODE_RW1,MODE_RW2
      CHARACTER*1 C1
      CHARACTER*2 C2
      CHARACTER*2 AC(3)
      CHARACTER*100 PSDNAM
      CHARACTER*80 SPCNAM,  OLDNAM,  SSMFPS,   DSMFPS
      CHARACTER*60 GRAPHTITLE
      CHARACTER*60 XTITLE,YTITLE,SERIESTITLE
      CHARACTER*102 SCRIPT(4)
      CHARACTER*2 LETTER1(5),LETTER2(0:101)
      COMMON/DOUBLES/ KWEI,SMFRTN, FSMFBN1,FSMABN1,FSMABN2,FSMFXX,FSMFYY
     *,FSMFR1,FSMFR2,FSMFR11,FSMFR12, FSMFRR,FSMFXY,BINSIZE,RBin,RBin_X,
     *RBin_Y
      REAL*8 KWEI, SMFRTN(22),  FSMFBN1(22,200,200), FSMABN1(200), FSMAB
     *N2(200), FSMFXX(22,200), FSMFYY(22,200), FSMFR1(22,200), FSMFR2(22
     *,200), FSMFR11(22,200), FSMFR12(22,200), FSMFRR(22,200), FSMFXY(22
     *,200,200), BINSIZE(200), RBin,RBin_X,RBin_Y
      COMMON/REALS/ XMIN2,XMAX2, XPLOT,HXMIN, YPLOT,YPLOTT, ERRYPLOT,X2Y
     *2,ZTOTAL,RSSD02,RSSD12,SSDVPS, SFACTOR,RFACTOR,RFACTOR1,RFACTOR2,
     *X,Y,Z,RRFACTOR,SSFACTOR,RSSD0,RSSD1, WEIGHT,EI, PARAEMAX2,PARAEMAX
     *1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAEMNE, PARANINC1,PARANINC2,PARA
     *NINC, EREAD,ZLAST, MUIDX,XIN,YIN,ZIN,UIN,VIN,WIN, SMFX2,SMFY2,SMFM
     *XR, HFIELD, RSCATT, DELTAZ,RSSDST,SSSDST,ERADII,E_ph_sp_e, E_ph_sp
     *_g, E_ph_sp_p, E_MIN_ph_sp, E_MAX_ph_sp, RFIELD,RFIELDe,XFLDMN,XFL
     *DMX, YFLDMN, YFLDMX, SMFMNZ, SMFMXZ, SMFMNX, SMFMXX, SMFMNY, SMFMX
     *Y, SMFMNR,RTREAT,RTREATe,XTREATMN,XTREATMX, YTREATMN,YTREATMX, SMF
     *MNE,SMFMXE, ABNTOTAL,ABNTOTALH,FLUAREA,NINC, RMIN_MIN,RMIN_MAX,RMA
     *X_MIN,RMAX_MAX,DOMEGA, SUM_TMP,SUM_TMP2,Z_SCORE,mumin,mumax
      real*8 XMIN2,XMAX2,  XPLOT(200),HXMIN,  YPLOT(200),YPLOTT, ERRYPLO
     *T(200), X2Y2,  ZTOTAL(22),  RSSD02,RSSD12,  SSDVPS(22+3),  SFACTOR
     *,RFACTOR,RFACTOR1,RFACTOR2,X,Y,Z,RRFACTOR,SSFACTOR, RSSD0,RSSD1,
     *WEIGHT,EI, PARAEMAX2,PARAEMAX1,PARAEMAX, PARAEMNE1,PARAEMNE2,PARAE
     *MNE, PARANINC1,PARANINC2,PARANINC, EREAD,ZLAST,MUIDX, XIN,YIN,ZIN,
     *UIN, VIN,WIN,  SMFX2(22), SMFY2(22),  SMFMXR(22),  HFIELD(22),  RS
     *CATT,  DELTAZ,  RSSDST(22,2000), SSSDST(22,2000), ERADII(200),  E_
     *ph_sp_e,  E_ph_sp_g,  E_ph_sp_p,  E_MIN_ph_sp,  E_MAX_ph_sp,  RFIE
     *LD,  RFIELDe,   XFLDMN,  XFLDMX,  YFLDMN,  YFLDMX,  SMFMNZ(22),
     *SMFMXZ(22),  SMFMNX(22),  SMFMXX(22),  SMFMNY(22),  SMFMXY(22),  S
     *MFMNR(22),  RTREAT,  RTREATe,   XTREATMN,  XTREATMX,  YTREATMN,  Y
     *TREATMX,  SMFMNE,SMFMXE,  ABNTOTAL,ABNTOTALH,  FLUAREA,   NINC,  R
     *MIN_MIN, RMIN_MAX,  RMAX_MIN,  RMAX_MAX, DOMEGA,  SUM_TMP(200),  S
     *UM_TMP2(200),  Z_SCORE,  mumin,mumax
      COMMON/INTEGERS/ PARANOT,PARANOT1,PARANOT2,PARANOP,PARANOP1, PARAN
     *OP2,IPARANOT,IPARANOT1,IPARANOT2, LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST, I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBER
     *ST,NBIT1,NBIT2, NFIELDi,NFIELDo,NFIELDe, NWRONG,LATCHJ,CHOICE,CHOI
     *CE1, K1,K2,K3,K4,IA, IX,IY,IR,K,IQ,LATCHI,IE, NPASSI,LATCH,SMFPXY,
     *NFIELD,NSMFPS,I_IN_EX, ISMFPS,IQSMFP,MSTART,MSMFXY,NSMFEE,NPASS_ph
     *_sp, N_ph_sp_e, N_ph_sp_g, N_ph_sp_p, FLUTYPE,ANGTYPE,IZLAST1, IZL
     *AST2,IMUIDX1,IMUIDX2,IZSCORE1,IZSCORE2,JUSTONE, MAXSCATTER,NUMSCAT
     *TER,ESTYPE,i_iaea_in,i_iaea_out,i_log, i_unit_in,i_unit_out
      INTEGER*8 PARANOT,PARANOT1,PARANOT2, PARANOP,PARANOP1,  PARANOP2,I
     *PARANOT,IPARANOT1,IPARANOT2,  LPARANINC,LPARANINC1,LPARANINC2,
     *NHSTRY,NHSTRY_LAST(200)
      INTEGER I,II,III,IIII,IIIII,ITYPE, J,JJ,NUMBERP,NUMBERST,NBIT1,NBI
     *T2,  NWRONG,LATCHJ(29), CHOICE,CHOICE1,  K1,K2,K3,K4,IA, IX,IY,IR,
     *K(22), IQ,LATCHI,IE, NPASSI,  LATCH(22),   SMFPXY(22),   NFIELD,
     * NFIELDi,   NFIELDo,   NFIELDe,  NSMFPS,  I_IN_EX,  ISMFPS(22),
     *IQSMFP(22),   MSTART,   MSMFXY,  NSMFEE,  NPASS_ph_sp,  N_ph_sp_e,
     *  N_ph_sp_g,  N_ph_sp_p,  FLUTYPE,  ANGTYPE,  IZLAST1,  IZLAST2,
     *IMUIDX1,   IMUIDX2,  IZSCORE1,  IZSCORE2,  JUSTONE(200),  MAXSCATT
     *ER,  NUMSCATTER,  ESTYPE ,  i_iaea_in,  i_iaea_out,   i_log,   i_u
     *nit_in,i_unit_out
      COMMON/LOGICALS/OLDFILE,EGSPERT
      LOGICAL OLDFILE,    EGSPERT
      COMMON/RWPHSP/ STRING_TEMP_ZLAST_OUT,  STRING_TEMP_OUT,  IHSTRY_PH
     *SP,   iaea_dummy_long,  NHSTRY_DOS,   ESHORT,  WEIGHTTMP,  WT_PHSP
     *_SHORT,  X_PHSP_SHORT,  Y_PHSP_SHORT,  Z_PHSP_SHORT,  U_PHSP_SHORT
     *,  V_PHSP_SHORT,  W_PHSP_SHORT,  ZLAST_PHSP_SHORT,  MUIDX_PHSP_SHO
     *RT,  EKMAX_PHSP_SHORT,  EKMINE_PHSP_SHORT,  NINC_PHSP_SHORT,  dosx
     *yz2beam_index,  iaea_extra_floats,  NUM_PHSP_TOT,  PHOT_PHSP_TOT,
     *iaea_result,   iaea_n_stat,  iaea_q_typ,  iaea_typ_q,  iaea_q_inde
     *x,  I_PHSP,  IERR_PHSP,  LATCHTMP,  WRITE_PHSP_COUNTER,  WRITE_PHS
     *P_SOFAR,  PHSP_RESTART,  PHSP_RECL,  PHSP_RECL_OLD,  iaea_extra_in
     *ts,  iaea_extra_int_types,iaea_extra_float_types,   iaea_i_zlast,
     * iaea_i_latch,  iaea_i_muidx,  iaea_n_extra_ints,  iaea_n_extra_fl
     *oats,  i_iaea_open_for_write,   iaea_i_zlast_out,  iaea_i_latch_ou
     *t,  iaea_i_muidx_out,  dosxyz2beam_izscore,  MODE_RW
      CHARACTER*32000 STRING_TEMP_ZLAST_OUT(1)
      CHARACTER*28000 STRING_TEMP_OUT(1)
      INTEGER*8 IHSTRY_PHSP(1),iaea_dummy_long,NHSTRY_DOS
      REAL*4 ESHORT,WEIGHTTMP,WT_PHSP_SHORT,X_PHSP_SHORT,Y_PHSP_SHORT, Z
     *_PHSP_SHORT,U_PHSP_SHORT, V_PHSP_SHORT,W_PHSP_SHORT,ZLAST_PHSP_SHO
     *RT, MUIDX_PHSP_SHORT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP
     *_SHORT,dosxyz2beam_index, iaea_extra_floats(10)
      INTEGER NUM_PHSP_TOT,PHOT_PHSP_TOT, iaea_result,iaea_n_stat,iaea_q
     *_typ(3),iaea_typ_q(3),iaea_q_index,I_PHSP, IERR_PHSP,LATCHTMP, WRI
     *TE_PHSP_COUNTER(1),WRITE_PHSP_SOFAR(1), PHSP_RESTART,PHSP_RECL,PHS
     *P_RECL_OLD,iaea_extra_ints(10), iaea_extra_int_types(10),iaea_extr
     *a_float_types(10),iaea_i_zlast, iaea_i_latch,iaea_i_muidx,iaea_n_e
     *xtra_ints,iaea_n_extra_floats, i_iaea_open_for_write,iaea_i_zlast_
     *out,iaea_i_latch_out, iaea_i_muidx_out,dosxyz2beam_izscore
      CHARACTER*5 MODE_RW
      INTEGER LATCH_TEMP_OUT,NUM_PHSP_TOFLUSH,TEMP_PHSP_COUNTER,REM_PHSP
      REAL*4 REAL_TEMP_OUT(7)
      CHARACTER*32 REC_TEMP_OUT
      integer itmp
      i_log=6
1010  CONTINUE
      IF ((i_iaea_in.EQ.1)) THEN
        i_unit_in=2
        call iaea_new_source(i_unit_in,PSDNAM,1,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error opening IAEA phase space source.'
        END IF
        call iaea_get_max_particles(i_unit_in,-1,iaea_dummy_long)
        PARANOT=iaea_dummy_long
        call iaea_get_max_particles(i_unit_in,1,iaea_dummy_long)
        PARANOP=iaea_dummy_long
        call iaea_get_total_original_particles(i_unit_in,iaea_dummy_long
     *  )
        LPARANINC=iaea_dummy_long
        call iaea_get_maximum_energy(i_unit_in,EKMAX_PHSP_SHORT)
        PARAEMAX=EKMAX_PHSP_SHORT
        call iaea_get_constant_variable(i_unit_in,2,Z_PHSP_SHORT,iaea_re
     *  sult)
        IF ((iaea_result.EQ.-3)) THEN
          write(i_log,*) ' Z positions of each particle will be read fro
     *m phase space data.'
          Z_SCORE=999.
          IZSCORE1=1
        ELSE IF((iaea_result.LT.0)) THEN
          write(i_log,*)' Error reading Z position where IAEA phsp was s
     *cored.'
          IZSCORE1=0
        ELSE
          Z_SCORE=Z_PHSP_SHORT
          IZSCORE1=0
        END IF
        call iaea_get_extra_numbers(i_unit_in,iaea_n_extra_floats,iaea_n
     *  _extra_ints)
        IF ((iaea_n_extra_floats .EQ. -1 .OR. iaea_n_extra_ints .EQ. -1)
     *  ) THEN
          write(i_log,*)' Error reading number of extra variables stored
     * in IAEA phsp'
        END IF
        call iaea_get_type_extra_variables(i_unit_in,iaea_result,iaea_ex
     *  tra_int_types, iaea_extra_float_types)
        IF ((iaea_result.EQ.-1)) THEN
          write(i_log,*)' Error getting types of extra variables stored
     *in IAEA phsp'
        END IF
        DO 1021 I_PHSP=1,iaea_n_extra_ints
          IF ((iaea_extra_int_types(I_PHSP).EQ.2)) THEN
            iaea_i_latch=I_PHSP
            GO TO1022
          END IF
1021    CONTINUE
1022    CONTINUE
        IF ((iaea_i_latch.EQ.-99)) THEN
          write(i_log,*)' Warning: IAEA format phsp file does not store
     *LATCH'
        END IF
        DO 1031 I_PHSP=1,iaea_n_extra_floats
          IF ((iaea_extra_float_types(I_PHSP).EQ.3)) THEN
            iaea_i_zlast=I_PHSP
            GO TO1032
          END IF
1031    CONTINUE
1032    CONTINUE
        IF ((iaea_i_zlast.EQ.-99)) THEN
          IZLAST1=0
        ELSE
          IZLAST1=1
        END IF
        iaea_i_muidx=MAX(1,iaea_i_zlast+1)
        IF ((iaea_i_muidx.GT.iaea_n_extra_floats .OR. iaea_extra_float_t
     *  ypes(iaea_i_muidx).NE.0)) THEN
          iaea_i_muidx=-99
          IMUIDX1=0
        ELSE
          IMUIDX1=1
        END IF
        WRITE(6,1040)PARANOT,PARANOP,PARAEMAX,LPARANINC
1040    FORMAT(/'            Total number of particles in file:',I13/ ' 
     *                    Total number of photons:',I13/ 'The rest are e
     *lectrons/positrons.'/ ' '/ '      Maximum kinetic energy of the pa
     *rticles:',F13.3,' MeV'/ ' # of incident particles from original so
     *urce:',I13)
        IF ((IZLAST1.EQ.1)) THEN
          MODE_RW='MODE2'
        ELSE
          MODE_RW='MODE0'
        END IF
        IF ((IZSCORE1.EQ.0)) THEN
          WRITE(6,1050)Z_SCORE
1050      FORMAT('                       Z at which phsp scored:',F13.3,
     *' cm')
        END IF
        IF ((IMUIDX1.NE.0)) THEN
          WRITE(6,1060)
1060      FORMAT(' Fractional MU index also scored in phase space data.'
     */)
        END IF
        NINC=LPARANINC
      ELSE
        itmp=-1
        IF ((itmp.GT.0)) THEN
          PHSP_RECL=4*8
          OPEN(UNIT=2,FILE=PSDNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1070)
1070        FORMAT(/' ***error opening file as MODE2 ****' /' *** THE FI
     *LE FORMAT MAY BE WRONG (I.E., NOT A MODE2 FILE) ***'/ /' *** WE NO
     *W TRY TO OPEN IT AS A MODE3 FILE ***'//)
            OPEN(UNIT=2,STATUS='OLD',FILE=PSDNAM, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1080)
1080          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
              STOP
            END IF
            READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW,NUM_PHSP_TOT,PHOT_PHSP
     *      _TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT=NUM_PHSP_TOT
            PARANOP=PHOT_PHSP_TOT
            PARAEMAX=EKMAX_PHSP_SHORT
            PARAEMNE=EKMINE_PHSP_SHORT
            PARANINC=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1090)
1090          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW .NE. 'MODE3')) THEN
              WRITE(6,1100)
1100          FORMAT(//' That file does not start with MODE3,', ' as all
     * old compressed files (with ZLAST) must'/ '  Check it out and trya
     *gain!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,1110)
1110          FORMAT(//' This is a MODE3 file! '/ ' Please convert it in
     *to a MODE2 file using [readphsp] ', ' and try again!'///)
              IERR_PHSP=1
              STOP
            END IF
          END IF
          READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW,NUM_PHSP_TOT,PHOT_PHSP_T
     *    OT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          PARANOT=NUM_PHSP_TOT
          PARANOP=PHOT_PHSP_TOT
          PARAEMAX=EKMAX_PHSP_SHORT
          PARAEMNE=EKMINE_PHSP_SHORT
          PARANINC=NINC_PHSP_SHORT
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1120)
1120        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW .NE. 'MODE2')) THEN
            WRITE(6,1130)
1130        FORMAT(//' That file does not start with MODE2,', ' as stand
     *ard compressed files with ZLAST must'/ '  Check it out and try aga
     *in!'///)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.EQ.0)) THEN
          PHSP_RECL=4*7
          OPEN(UNIT=2,FILE=PSDNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1140)
1140        FORMAT(/' ***ERROR opening file as MODE0****' /' *** THE FIL
     *E FORMAT MAY BE WRONG (I.E., NOT A MODE0 FILE) ***'/ /' *** WE NOW
     * TRY TO OPEN IT AS A MODE1 FILE ***'//)
            OPEN(UNIT=2,STATUS='OLD',FILE=PSDNAM, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1150)
1150          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
              STOP
            END IF
            READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW,NUM_PHSP_TOT,PHOT_PHSP
     *      _TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT=NUM_PHSP_TOT
            PARANOP=PHOT_PHSP_TOT
            PARAEMAX=EKMAX_PHSP_SHORT
            PARAEMNE=EKMINE_PHSP_SHORT
            PARANINC=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1160)
1160          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW .NE. 'MODE1')) THEN
              WRITE(6,1170)
1170          FORMAT(//' That file does not start with MODE1,', ' as all
     * old compressed files must'/ '  Check it out and try again!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,1180)
1180          FORMAT(//' This is a MODE1 file! '/ ' Please convert it in
     *to a MODE0 file using [readphsp] ', 'and try again!'///)
              IERR_PHSP=1
              STOP
            END IF
          END IF
          READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW,NUM_PHSP_TOT,PHOT_PHSP_T
     *    OT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          PARANOT=NUM_PHSP_TOT
          PARANOP=PHOT_PHSP_TOT
          PARAEMAX=EKMAX_PHSP_SHORT
          PARAEMNE=EKMINE_PHSP_SHORT
          PARANINC=NINC_PHSP_SHORT
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1190)
1190        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW .NE. 'MODE0')) THEN
            WRITE(6,1200)
1200        FORMAT(/' Does not start with MODE0 as files without ZLAST m
     *ust'/ '  Try again!'//)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.LT.0)) THEN
          WRITE(6,1210)
1210      FORMAT(/' First, try to open it as a MODE0 file')
          PHSP_RECL=4*7
          OPEN(UNIT=2,FILE=PSDNAM,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1220)
1220        FORMAT(/' Now try to open it as a MODE2 file')
            itmp=1
            PHSP_RECL=4*8
            OPEN(UNIT=2,FILE=PSDNAM,FORM='UNFORMATTED',ACCESS='DIRECT',
     *      RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1230)
1230          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
            ELSE
              READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW,NUM_PHSP_TOT,PHOT_PH
     *        SP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
              PARANOT=NUM_PHSP_TOT
              PARANOP=PHOT_PHSP_TOT
              PARAEMAX=EKMAX_PHSP_SHORT
              PARAEMNE=EKMINE_PHSP_SHORT
              PARANINC=NINC_PHSP_SHORT
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1240)
1240            FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE**
     ** '//)
                STOP
              END IF
              IF ((MODE_RW.NE.'MODE2')) THEN
                WRITE(6,1250)
1250            FORMAT(//' That file does not start with MODE2,', ' as s
     *tandard compressed files with ZLAST must'/ '  Check it out and try
     * again!'///)
                CLOSE(2)
                STOP
              END IF
            END IF
          ELSE
            READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW,NUM_PHSP_TOT,PHOT_PHSP
     *      _TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT=NUM_PHSP_TOT
            PARANOP=PHOT_PHSP_TOT
            PARAEMAX=EKMAX_PHSP_SHORT
            PARAEMNE=EKMINE_PHSP_SHORT
            PARANINC=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1260)
1260          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            itmp=0
            IF ((MODE_RW.NE.'MODE0')) THEN
              WRITE(6,1270)
1270          FORMAT(/' The file does not start with MODE0 as it suppose
     *d to')
              CLOSE(2)
              WRITE(6,1280)
1280          FORMAT(/' Now try to open it as a MODE2 file')
              itmp=1
              PHSP_RECL=4*8
              OPEN(UNIT=2,FILE=PSDNAM,FORM='UNFORMATTED',ACCESS='DIRECT'
     *        , RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1290)
1290            FORMAT(//' *** PHASE SPACE FILE CANNOT BE OPENED. ***')
              ELSE
                READ(2,REC=1,IOSTAT=IERR_PHSP)MODE_RW,NUM_PHSP_TOT,PHOT_
     *          PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_S
     *          HORT
                PARANOT=NUM_PHSP_TOT
                PARANOP=PHOT_PHSP_TOT
                PARAEMAX=EKMAX_PHSP_SHORT
                PARAEMNE=EKMINE_PHSP_SHORT
                PARANINC=NINC_PHSP_SHORT
                IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,1300)
1300              FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE
     **** '//)
                  STOP
                END IF
                IF ((MODE_RW.NE.'MODE2')) THEN
                  WRITE(6,1310)
1310              FORMAT(//' That file does not start with MODE2,', ' as
     * standard compressed files with ZLAST must'/ '  Check it out and t
     *ry again!'///)
                  CLOSE(2)
                  STOP
                END IF
              END IF
            END IF
          END IF
        END IF
        IF((IERR_PHSP.NE.0))GOTO 1320
        WRITE(6,1330)PARANOT,PARANOP,PARAEMAX,PARAEMNE
1330    FORMAT(/'            Total number of particles in file:',I13/ ' 
     *                     Total number of photons:',I13/ ' The rest are
     * electrons/positrons.'/ ' '/ '      Maximum kinetic energy of the 
     *particles:',F13.3,' MeV'/ '      Minimum kinetic energy of the ele
     *ctrons:',F13.3,' MeV')
        IF ((PARANINC .LT. 2.)) THEN
          WRITE(6,1340)PARANINC
1340      FORMAT('        Minimum kinetic energy of the photons:',F13.3,
     *' MeV')
          NINC=1
        ELSE
          WRITE(6,1350)PARANINC
1350      FORMAT(' # of incident particles from original source:',F13.1)
          NINC=PARANINC
        END IF
      END IF
      RETURN
1320  WRITE(6,1360)
1360  FORMAT(//' CANNOT FIND/OPEN THE PH-SP FILE!!!'///)
      IF (EGSPERT) THEN
        CALL HELP_MESSAGE(61)
      END IF
      CALL READNAME
      GOTO 1010
      END
      SUBROUTINE HELP_MESSAGE(NMESSAGE)
      INTEGER NMESSAGE
      IF ((NMESSAGE.EQ.0)) THEN
        WRITE(6,7010)
7010    FORMAT(/ ' BEAMDP (BEAM Data Processor) creates a source paramet
     *er file'/ ' for beam characterization models with information obta
     *ined'/ ' from the user and derived from a full phase-space data fi
     *le'/ ' created by BEAM'/ ' '/ ' This programme can be used to deri
     *ve planar fluence, spectrum,'/ ' mean energy and angle distributio
     *n, etc., from a phase-space'/ ' file created by BEAM.'/ ' '/ ' Ify
     *ou are not familiar with this programme, you can get an'/ ' explan
     *ation before any input request. Otherwise, the prompts'/ ' will be
     * terse.'// ' However, you can get help by typing a ? to any prompt
     *.'/)
      ELSE IF((NMESSAGE.EQ.1)) THEN
        WRITE(6,7020)
7020    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' You have the following operation options:'/ ' -------
     *------------------------------------------------------'/ '        
     *                                              '/ ' (0) - data proc
     *essing for beam characterization models'/ '       (input 0 for mor
     *e detailed information)'/ ' (1) - deriving fluence vs position fro
     *m ph_sp data'/ '       (number of particles vs position for requir
     *ed charge, region'/ '        and LATCH)'/ ' (2) - deriving energy 
     *fluence vs position from ph_sp data'/ '       (kinetic energy vs p
     *osition for required charge, region and'/ '        LATCH)'/ ' (3) 
     *- deriving energy spectrum from ph_sp data'/ '       (particle spe
     *ctrum for required charge, region and LATCH)'/ ' (4) - deriving en
     *ergy fluence distribution from ph_sp data '/ '       (energy fluen
     *ce distribution for required charge, region and'/ '   LATCH)'/ ' (
     *5) - deriving mean energy information from ph_sp data'/ '       (m
     *ean energy for required charge, region and LATCH)'/' (6) - derivin
     *g angular distribution from ph_sp data')
        WRITE(6,7030)
7030    FORMAT('       (angular distribution for required charge, region
     * and LATCH)'/ ' (7) - deriving zlast distribution from ph_sp data'
     */ '       (angular distribution for required charge, region andLAT
     *CH)'/ ' (8) - deriving information about particle weights fromph_s
     *p data '/ '       (distribution of weights for specified charge,re
     *gion and LATCH)'/ ' (9) - deriving X-Y positions of particles'/ ' 
     *      (X-Y scatter plot of particles for specified charge,region a
     *nd'/ '        LATCH)'/ ' (10) - combining two ph-sp files into one
     *'/ '       (write the contents of file1 into file2)'/ ' (11) - lis
     *t parameters for a number of ph-sp particles'/ '       (list IQ,X,
     *Y,U,V,W,E,WEIGHT,LATCH on the screen)'/ ' (12) - quit this program
     *me'/ ' -----------------------------------------------------------
     *--')
      ELSE IF((NMESSAGE.EQ.2)) THEN
        WRITE(6,7040)
7040    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' CHOOSE FROM THE FOLLOWING: '/ ' ---------------------
     *----------------------------------------'/ ' '/ '    0 for a circu
     *lar field with circular ring bins'/ '    1 for a square field with
     * square ring bins'/ '    2 for a rectangular field with rectangula
     *r voxels'/ ' '/ ' ------------------------------------------------
     *-------------'/)
      ELSE IF((NMESSAGE.EQ.3)) THEN
        WRITE(6,7050)200
7050    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS REQUIRED FOR FLUENCE/ENERGY FLUENCE VS POS
     *ITION:'/ ' -------------------------------------------------------
     *------ '/ ' '/ ' The following parameters are needed for fluence v
     *s position:'/ ' '/ '  1) number of radial bins into which field is
     * divided (<',I4,')'/ '  2) charge of the particles (0:phot,1:posi,
     *-1:elec,2:all,3:e+e-)'/ '  3) outer radius of the circular field'/
     *' '/ ' -----------------------------------------------------------
     *--'/)
      ELSE IF((NMESSAGE.EQ.4)) THEN
        WRITE(6,7060)200
7060    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS REQUIRED FOR FLUENCE/ENERGY FLUENCE VS POS
     *ITION:'/ ' -------------------------------------------------------
     *------'/ ' '/ ' The following parameters are needed for fluence vs
     * position: '/ ' '/ '  1) number of square bins into which field is
     * divided (<',I4,')'/ '  2) charge of the particles (0:photon,1:e+,
     *-1:e-,2:all,3:e+e-)'/ '  3) half-side of the square field'/ ' '/ '
     * -------------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.5)) THEN
        WRITE(6,7070)200
7070    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS REQUIRED FOR FLUENCE/ENERGY FLUENCE VS POS
     *ITION:'/ ' -------------------------------------------------------
     *------'/ ' '/ ' Input the following parameters:'/ ' '/ '  1) numbe
     *r of bins into which field is divided (<',I4,')'/ '  2) orientatio
     *n (0-along x-axis; 1-along y-axis)'/ '  3) charge (0:photon,1:e+,-
     *1:e-,2:all,3:e+e-)'/ '  4) Xmin for a rectangular field'/ '  5) Xm
     *ax for a rectangular field'/ '  6) Ymin for a rectangular field'/'
     *  7) Ymax for a rectangular field'/ ' '/ ' -----------------------
     *--------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.6)) THEN
        WRITE(6,7080)
7080    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR THE SPECTRAL OR ENERGY FLUENCE DISTRIB
     *UTION:'/ ' -------------------------------------------------------
     *------'/ ' '/ ' The following parameters are needed for the energy
     * spectrum '/ ' or energy fluence distribution: '/ ' '/ '  1) charg
     *e of the particles (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/ '  2)X
     *min for a rectangular region anywhere on the scoring plane'/ '  3)
     * Xmax for the rectangular region'/ '  4) Ymin for the rectangular 
     *region'/ '  5) Ymax for the rectangular region'/ '     (Xmin,Xmax,
     *Ymin,Ymax default to -15,15,-15,15)'/ ' Or'/ '  1) charge of thepa
     *rticles (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/ '  2) Rmin foran 
     *annular region centred at the z-axis'/ '  3) Rmax for the annular 
     *region'/ '     (Rmin,Rmax default to 0,15)'/ ' '/ '     Note: if y
     *ou hit return here you get a rectangular field'/ '       (i.e.,  X
     *min,Xmax,Ymin,Ymax =-15,15,-15,15)'/ ' ---------------------------
     *----------------------------------'/)
      ELSE IF((NMESSAGE.EQ.7)) THEN
        WRITE(6,7090)200
7090    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' NUMBER OF BINS AND MIN. ENERGY, MAX. ENERGY FOR TH
     *E DISTRIBUTION:'/ ' ----------------------------------------------
     *------------------ '/ ' '/ ' The maximum number of bins allowed is
     * ',I4/ ' and the energy range given below should be consistent wit
     *h the'/ ' phase-space data. For example, Emin should be =< (ECUT-0
     *.511)'/ ' used in the simulation with BEAM, and Emax should be gre
     *ater '/ ' than the maximum kinetic energy of the particles in thep
     *h-sp data.'/ ' '/ ' ----------------------------------------------
     *------------------'/)
      ELSE IF((NMESSAGE.EQ.8)) THEN
        WRITE(6,7100)
7100    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR THE REQUIRED ANGULAR DISTRIBUTION:'/ '
     * -------------------------------------------------------------'/ '
     * '/ ' The following parameters are needed:'/ ' '/ '  1) charge oft
     *he particles (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/ '  2) Xminfo
     *r a rectangular region anywhere on the scoring plane'/ '  3) Xmax 
     *for the rectangular region'/ '  4) Ymin for the rectangular region
     *'/ '  5) Ymax for the rectangular region'/ '     (Xmin,Xmax,Ymin,Y
     *max default to -15,15,-15,15)'/ ' Or '/ '  1) charge of the partic
     *les (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/ '  2) Rmin for an ann
     *ular region centred at the z-axis'/ '  3) Rmax for the annular reg
     *ion'/ '     (Rmin,Rmax default to 0,15)'/ ' '/ '     Note: if you 
     *hit return here you get a rectangular field'/ '       (i.e.,  Xmin
     *,Xmax,Ymin,Ymax =-15,15,-15,15)'/ ' ------------------------------
     *-------------------------------'/)
      ELSE IF((NMESSAGE.EQ.9)) THEN
        WRITE(6,7110)200
7110    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' NUMBER OF BINS, MIN. ANGLE, MAX. ANGLE: '/ ' --------
     *-----------------------------------------------------'/ ' '/ ' The
     * maximum number of bins allowed is ',I4/ ' and the angle should be
     * between 0 - 90 degrees'/ ' '/ ' ---------------------------------
     *----------------------------'/)
      ELSE IF((NMESSAGE.EQ.10)) THEN
        WRITE(6,7120)
7120    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR THE REQUIRED ZLAST DISTRIBUTION:'/ ' -
     *------------------------------------------------------------'/ ' '
     */ ' The following parameters are needed:'/ ' '/ '  1) charge ofthe
     * particles (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/ '  2) Xminfor 
     *a rectangular region anywhere on the scoring plane'/ '  3) Xmax fo
     *r the rectangular region'/ '  4) Ymin for the rectangular region'/
     * '  5) Ymax for the rectangular region'/ '     (Xmin,Xmax,Ymin,Yma
     *x default to -15,15,-15,15)'/ ' Or'/ '  1) charge of the particles
     * (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/ '  2) Rmin for an annula
     *r region centred at the z-axis'/ '  3) Rmax for the annular region
     *'/ '     (Rmin,Rmax default to 0,15)'/ ' '/ '     Note: if you hit
     * return here you get a rectangular field'/ '       (i.e.,  Xmin,Xm
     *ax,Ymin,Ymax =-15,15,-15,15)'/ ' ---------------------------------
     *----------------------------'/)
      ELSE IF((NMESSAGE.EQ.11)) THEN
        WRITE(6,7130)200
7130    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' NUMBER OF BINS, Zmin, Zmax:'/ ' ---------------------
     *----------------------------------------'/ ' '/ ' The maximum numb
     *er of bins allowed is ',I4/ ' and the range of ZLAST '/ ' '/ ' ---
     *----------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.12)) THEN
        WRITE(6,7140)
7140    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR THE REQUIRED WEIGHT DISTRIBUTION:'/ '-
     *------------------------------------------------------------'/ ''/
     * ' The following parameters are needed for the weight distribution
     *:'/ ' '/ '  1) charge of the particles (0:phot,1:posit,-1:elect,2:
     *all,3:e-e+)'/ '  2) Xmin for a rectangular region anywhere on the 
     *scoring plane'/ '  3) Xmax for the rectangular region'/ '  4) Ymin
     * for the rectangular region'/ '  5) Ymax for the rectangular regio
     *n'/ '     (Xmin,Xmax,Ymin,Ymax default to -15,15,-15,15)'/ ' Or'/ 
     *'  1) charge of the particles (0:phot,1:posit,-1:elect,2:all,3:e-e
     *+)'/ '  2) Rmin for an annular region centred at the z-axis'/'  3)
     * Rmax for the annular region'/ '     (Rmin,Rmax default to0,15)'/ 
     *' '/ '     Note: if you hit return here you get a rectangular fiel
     *d'/ '       (i.e.,  Xmin,Xmax,Ymin,Ymax =-15,15,-15,15)'/' -------
     *------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.13)) THEN
        WRITE(6,7150)200
7150    FORMAT(/ ' -----------------------------------------------------
     *------------'/ ' NUMBER OF BINS AND MIN. WEIGHT, MAX. WEIGHT FOR T
     *HE DISTRIBUTION:'/ ' ---------------------------------------------
     *--------------------'/ ' '/ ' The maximum number of bins allowed i
     *s ',I4/ ' and the weight range given below should be consistent wi
     *th the'/ ' phase-space data and also consistent with fact that the
     * weights'/ ' are plotted on a logarithmic scale. For example, them
     *in. weight'/ ' should be slightly greater than 0 and the max. weig
     *ht should be'/ ' slightly greater than 1.'/ ' '/ ' ---------------
     *--------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.14)) THEN
        WRITE(6,7160)
7160    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR THE X-Y SCATTER PLOT:'/ ' ------------
     *-------------------------------------------------'/ ' '/ ' The fol
     *lowing parameters are needed for the X-Y scatter plot'/ ' '/ '  1)
     * charge of the particles (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/'
     *  2) Xmin for a rectangular region anywhere on the scoring plane'/
     * '  3) Xmax for the rectangular region'/ '  4) Ymin for the rectan
     *gular region'/ '  5) Ymax for the rectangular region'/ '     (Xmin
     *,Xmax,Ymin,Ymax default to -15,15,-15,15)'/ ' Or'/ '  1) charge of
     * the particles (0:phot,1:posit,-1:elect,2:all,3:e-e+)'/ '  2) Rmin
     * for an annular region centred at the z-axis'/ '  3) Rmax for the 
     *annular region'/ '     (Rmin,Rmax default to 0,15)'/ ' '/ ' Note: 
     *if you hit return here you get a rectangular field'/ '  (i.e.,  Xm
     *in,Xmax,Ymin,Ymax =-15,15,-15,15)'/ ' ----------------------------
     *---------------------------------'/)
      ELSE IF((NMESSAGE.EQ.15)) THEN
        WRITE(6,7170)
7170    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' MIN. ENERGY, MAX. ENERGY FOR THE PARTICLES IN THE SCA
     *TTER PLOT:'/ ' ------------------------------------------------  '
     */ ' '/ ' The energy range given below should be consistent with th
     *e'/' phase-space data. For example, Emin should be =< (ECUT-0.511)
     *'/ ' used in the simulation with BEAM, and Emax should be greater'
     */ ' than the maximum kinetic energy of the particles in the ph-sp 
     *data.'/ ' '/ ' ---------------------------------------------------
     *----------'/)
      ELSE IF((NMESSAGE.EQ.16)) THEN
        WRITE(6,7180)
7180    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' LATCH bit filters:'/ ' ------------------------------
     *-------------------------------'/ ' '/ ' Input the following 3 var
     *ibles in the same line:'/ ' '/ '  1) I_IN_EX: flag for LATCH check
     *ing'/ '    = 0 INclusive/EXclusive for bits: if any of the 1st set
     * of'/ '        Nbit1 bits are set and none of the 2nd set of Nbit2
     * bits'/ '        are set, the particle is scored'/ '    = 1 EXclus
     *ive for bits: if any of the set of Nbit1 bits'/ '        are set,t
     *he particle is not scored'/ '    = 2 INclusive for regions: score 
     *secondary particles that'/ '        originated in the regions with
     * IREGION_TO_BIT'/ '        equal to any of the set of Nbit1 bits'/
     * '    = 3 EXclusive for regions: do not score secondary particles'
     */ '        that originated in regions with IREGION_TO_BIT equal'/ 
     *'        to any of the set of Nbit1 bits'/)
      ELSE IF((NMESSAGE.EQ.17)) THEN
        WRITE(6,7190)
7190    FORMAT('  2) Nbit1: the number of bits or regions of origin to'/
     *'            include (I_IN_EX=0,2) or exclude (I_IN_EX=1,3)'/ '  3
     *) Nbit2: the number of bits to exclude.  Only has meaning'/ '     
     *     for I_IN_EX=0, otherwise, it is automatically'/ '     set to 
     *0.'/ ' '/ '    Restrictions: for I_IN_EX=0:    0<=Nbit1+Nbit2<=29'
     */ '                  for I_IN_EX=1:    0<=Nbit1<=29'/'            
     *      for I_IN_EX=2,3:  0<=Nbit1<=24'//)
      ELSE IF((NMESSAGE.EQ.18)) THEN
        WRITE(6,7200)
7200    FORMAT(' Input the following in the next line:'/ '  BIT(I) or IR
     *EGION_TO_BIT(I) (I = 1, Nbit1):'/ '          bits/IREGION_TO_BITs 
     *to'/ '            include (I_IN_EX=0,2) or'/ '               exclu
     *de (I_IN_EX=1,3)'/ ' '/ ' Input the next lineonly if I_IN_EX=0 and
     * Nbit2>0:'/ '  BIT(I) or IREGION_TO_BIT(I) (I = Nbit1+1, Nbit1+Nbi
     *t2):'/ '    bits to exclude when'/ '   I_IN_EX=0'/ ' '/ '     (BIT
     *(I) should be between 0 and 28 and IREGION_TO_BIT(I)'/ '      shou
     *ld) be between 0 and 23)'/ ' '/ '  (Note: if no LATCH checking is 
     *required input 0,0,0 below)'/ ' '/ ' -----------------------------
     *--------------------------------'/)
      ELSE IF((NMESSAGE.EQ.19)) THEN
        WRITE(6,7210)
7210    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' LIST PARAMETERS FOR A NUMBER OF PARTICLES:'/ ' ------
     *-------------------------------------------------------'/ ' '/ ' I
     *nput the following:'/ ' '/ '  1) Number: number of particles to be
     * listed (default to 100)'/ ' '/ '  2) IQ    : charge (0:phot,1:pos
     *it,-1:elect,2:all,3:e-e+)'/ ' '/ ' -------------------------------
     *------------------------------'/)
      ELSE IF((NMESSAGE.EQ.20)) THEN
        WRITE(6,7220)
7220    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' YOU CAN CHOOSE THE FOLLOWING GRAPH TYPES:        '/ '
     * -------------------------------------------------------------'/ '
     * '/ ' (0) - NORMAL POINT GRAPH'/ ' (1) - HISTOGRAM'/ ' '/ ' ------
     *-------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.21)) THEN
        WRITE(6,7230)
7230    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' Plot of  real fluence or planar fluence:'/ ' --------
     *-----------------------------------------------------'/ ' '/ ' (0)
     * - estimated real fluence: E(particle weight/cos(Z-angle))'/ '    
     *                       ------------------------------- '/ '       
     *                               total area'/ '  '/ ' (1) - planar f
     *luence: E(particle weight)'/ '                       -------------
     *------'/ '                           total area'/ ' '/ ' ---------
     *----------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.22)) THEN
        WRITE(6,7240)
7240    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' FILE CONTAINING FULL PHASE-SPACE DATA:'/ ' ----------
     *---------------------------------------------------'/ ' '/ ' The f
     *ull phase-space data created by the NRCC EGSnrc usercode'/ ' BEAMn
     *rc is stored in a compressed form. This programme uncompresses'/ '
     * the data first and then analyses the data.'/ ' '/ ' The name of t
     *he file containing full phase-space data should be'/ ' supplied he
     *re (with extension). '/ ' '/ ' A reminder - default BEAM ph-sp fil
     *e name: [filename].egsphsp1'/ ' ----------------------------------
     *---------------------------'/)
      ELSE IF((NMESSAGE.EQ.30)) THEN
        WRITE(6,7250)
7250    FORMAT(/ ' -----------------------------------------------------
     *-------------------'/ ' USING SUB-SOURCE SPECIFIERS FROM AN EXISTI
     *NG SOURCE MODEL FOR REFERENCE:'/ ' -------------------------------
     *-----------------------------------------'/ ' '/ ' In order to hel
     *p input the sub-source specifiers required to create a'/ ' new sou
     *rce model, the sub-source specifiers from an existing source'/ ' m
     *odel can be used as a reference. However, if you do this, then the
     *'/ ' old and new source models will consist of the same number ofs
     *ub-sources.'/ '   '/ ' In fact, it is sometimes easier to modifyth
     *e sub-source specifiers'/ ' directly by editing the source model t
     *han doing it interactively'/ ' by running this program, and youare
     * not confined by the'/ ' condition mentioned above.'/ ' ----------
     *-------------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.31)) THEN
        WRITE(6,7260)
7260    FORMAT(/ ' -----------------------------------------------------
     *----------'/ ' FILE NAME INPUT:'/ ' ------------------------------
     *---------------------------------'/ ' '/ ' The name of the file co
     *ntaining the new source model.'/ ' This could have been created by
     * inputting sub-source specifiers'/ ' from scratch and analyzing ph
     *ase space data, modifying the'/ ' sub-source specifiers of an exis
     *ting source model, or analyzing'/ ' phase space data using sub-sou
     *rce specifiers from an existing'/ ' source model.'/ ' ------------
     *---------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.32)) THEN
        WRITE(6,7270)
7270    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' DETAILED INFORMATION ABOUT THE BEAM:'/ ' ------------
     *-------------------------------------------------'/ '             
     *                                    '/ ' The information about the
     * machine, beam energy, field size, '/ ' etc. will serve as a remin
     *der for later use.'// ' Now input this information in one line (< 
     *70 characters).'/ ' ----------------------------------------------
     *---------------'/)
      ELSE IF((NMESSAGE.EQ.33)) THEN
        WRITE(6,7280)22
7280    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' NUMBER OF SUB-SOURCES FOR THE SOURCE MODEL:'/ ' -----
     *--------------------------------------------------------'/ ' '/ 'T
     *he maximum number of sub-sources is ',I4,'.'/ ' This number is det
     *ermined by the bit-setting number in BEAM.'/ ' In general, one typ
     *e of particles from a CM should be considered'/ ' to be from a sub
     *-source. However, scattering foils, mirror and'/ ' monitoring cham
     *ber can be worked out as a virtual point source.'/ ' If the relati
     *ve source intensity of a sub-source is < 0.1% this'/ ' sub-source 
     *will automatically be ignored in later use.'/ ' ------------------
     *-------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.34)) THEN
        WRITE(6,7290)I
7290    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' SOURCE TYPE FOR SUB-SOURCE',I4,', CHARGE, LATCH #:'/'
     * -------------------------------------------------------------'/ '
     *'/ ' The source model consists of a number of sub-sources; each ha
     *s'/ ' its own energy spectrum and field distribution at the phanto
     *m'/ ' surface. '/ ' '/ ' The component modules in a linear acceler
     *ator are classified'/ ' into the following types:'/ ' '/ '       S
     *ource type 1  for aperture applicators'/ '                   11 fo
     *r tubular applicators'/ '                   2  for collimators'/ '
     *                   3  for rings, cones, and point sources'/ '     
     *         4  for rectangular plane sources'/ '   5  for circular pl
     *ane sources'/)
        WRITE(6,7300)22,22,22
7300    FORMAT(/ ' The charge of the particles from the source:'/ ' '/ '
     *           charge  0  for photons'/ '                  -1  for ele
     *ctrons and'/ '                   1  for positrons.'/ ' '/ ' The LA
     *TCH bit number must be <=',I3/ ' If you input a LATCH bit in the r
     *ange 1 - ',I3,' you are'/ ' selecting particles that have been/int
     *eracted in region'/ ' associated with the LATCH bit you have input
     *.  If you input'/ ' LATCH <= 0, then you are selecting particles t
     *hat have none'/ ' of bits 1 - ',I3,' set.  This latter option is g
     *enerally only'/ ' meaningful for virtual point sources.'/ ' '/ ' -
     *------------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.35)) THEN
        WRITE(6,7310)
7310    FORMAT(/ ' -----------------------------------------------------
     *---------'/ ' SOURCE PARAMETERS FOR THIS SUB-SOURCE:'/ ' ---------
     *-----------------------------------------------------'/ ' '/ ' The
     * following parameters are required for this sub-source:'// ' 1) di
     *stance (in cm) from the sub-source to the scoring plane'/ ' 2) min
     *imum x coordinate (cm) for the opening of the applicator'/ ' 3) ma
     *ximum x coordinate (cm) for the opening of the applicator'/ ' 4) m
     *inimum y coordinate (cm) for the opening of the applicator'/ ' 5)m
     *aximum y coordinate (cm) for the opening of the applicator'/ ' 6) 
     *maximum |x| coordinate (cm) for the applicator'/ ' 7) maximum |y| 
     *coordinate (cm) for the applicator'/ ' ---------------------------
     *----------------------------------'/)
      ELSE IF((NMESSAGE.EQ.36)) THEN
        WRITE(6,7320)
7320    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' SOURCE PARAMETERS FOR THIS SUB-SOURCE:'/ ' ----------
     *---------------------------------------------------'/ ' '/ ' The f
     *ollowing parameters are required for this sub-source: '// ' 1) dis
     *tance (in cm) from source bottom to scoring plane'/ ' 2) distance(
     *in cm) from source top to scoring plane'/ ' 3) minimum x coordinat
     *e (cm) for the opening of the applicator'/ ' 4) maximum x coordina
     *te (cm) for the opening of the applicator'/ ' 5) minimum y coordin
     *ate (cm) for the opening of the applicator'/ ' 6) maximum y coordi
     *nate (cm) for the opening of the applicator'/ ' 7) maximum |x| coo
     *rdinate (cm) for the applicator'/ ' 8) maximum |y| coordinate (cm)
     * for the applicator'/ ' ------------------------------------------
     *-------------------'/)
      ELSE IF((NMESSAGE.EQ.37)) THEN
        WRITE(6,7330)
7330    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' SOURCE PARAMETERS FOR THIS SUB-SOURCE:'/ ' ----------
     *---------------------------------------------------'/ ' '/ ' The f
     *ollowing parameters are required for this sub-source:'/ ' '/ ' 1.d
     *istance (in cm) from the sub-source to the scoring plane'/ ' 2. mi
     *nimum x coordinate (cm) for the opening of the collimator'/ ' 3.ma
     *ximum x coordinate (cm) for the opening of the collimator'/ ' 4. m
     *inimum y coordinate (cm) for the opening of the collimator'/ ' 5. 
     *maximum x coordinate (cm) for the opening of the collimator'/ '6. 
     *maximum |x| coordinate (cm) for the collimator'/ ' 7. maximum |y| 
     *coordinate (cm) for the collimator'/ ' 8. orientation of the colli
     *mator:'/ '    (0-collimator bars along x-axis, 1-along y-axis)'/ '
     * -------------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.38)) THEN
        WRITE(6,7340)
7340    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' SOURCE PARAMETERS FOR THIS SUB-SOURCE:'/ ' ----------
     *---------------------------------------------------'/ ' '/ ' The f
     *ollowing parameters are required for this sub-source:'/ ' '/ '   1
     *) distance from the sub-source to the scoring plane'/ '   2) Rmin:
     * radius of the opening of the ring or cone'/ '   3) Rmax: outer ra
     *dius of the ring or cone'/ ' '/ ' Note:'/ ' '/ '   If it is a poin
     *t source the radius must be 0.0 cm, and'/ '   the SSD will be re-e
     *valuated based on the ph-sp data.'/ ' ----------------------------
     *---------------------------------'/)
      ELSE IF((NMESSAGE.EQ.39)) THEN
        WRITE(6,7350)
7350    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' RADIUS OF A RING OPENING TO ANALYSE THE VIRTUAL SSD:'
     */ ' -------------------------------------------------------------'
     */ ''/ ' In order to derive the SSD for a (virtual) point source, t
     *he'/ ' particles falling into this ring (R1) will be transported'/
     *' in vacuum. The radius of the projected image of this ring (R2)'/
     * ' at a diatance, d, from the scoring plane will be calculated.'/ 
     *' The virtual SSD is then calculated by'/ ' '/ '     SSD = d R1/(R
     *2 - R1)'/ ' '/ ' see "Beam Characterization: a multiple-source mod
     *el"'/ '     by C-M Ma and D.W.O. Rogers, NRC Report PIRS-0509(C)'/
     * ' '/ ' The radius (in cm) of the ring region on the scoring plane
     *'/ ' should be supplied here (R > 0.0 cm and within the field).'/ 
     *' -------------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.40)) THEN
        WRITE(6,7360)
7360    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' SOURCE PARAMETERS FOR THIS SUB-SOURCE:'/ ' ----------
     *---------------------------------------------------'/ ' '/ ' The f
     *ollowing parameters are required for this sub-source:'/ ' '/ ' 1)d
     *istance from the sub-source to the scoring plane'/ ' 2) minimum x 
     *coordinate (cm) for the plane source'/ ' 3) maximum x coordinate(c
     *m) for the plane source'/ ' 4) minimum y coordinate (cm) for the p
     *lane source'/ ' 5) maximum y coordinate (cm) for the plane source'
     */ ' '/ ' ---------------------------------------------------------
     *----'/)
      ELSE IF((NMESSAGE.EQ.41)) THEN
        WRITE(6,7370)
7370    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' SOURCE PARAMETERS FOR THIS SUB-SOURCE:        '/ ' --
     *-----------------------------------------------------------'/ ' '/
     * ' The following parameters are required for this sub-source:'/ ' 
     *'/ '   1) distance from the sub-source to the scoring plane'/ '   
     *2) radius of the plane source '/ ' '/ ' --------------------------
     *-----------------------------------'/)
      ELSE IF((NMESSAGE.EQ.42)) THEN
        WRITE(6,7380)200
7380    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' NUMBER OF BINS AND MIN. ENERGY, MAX. ENERGY FOR THE S
     *PECTRUM:'/ ' -----------------------------------------------------
     *--------'/ ' '/ ' The maximum number of bins allowed is ',I4/ ' an
     *d the energy range given below should be consistent with the'/ ' p
     *hase-space data. For example, Emin should be =< (ECUT-0.511)'/ ' u
     *sed in the simulation with BEAM, and Emax should be greater '/ ' t
     *han the maximum kinetic energy of the particles in the ph-sp data.
     *'/ ' '/ ' Note: the min., max. energy and bin number will be the s
     *ame '/ ' for all the sub-sources.'/ ' '/ ' -----------------------
     *--------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.43)) THEN
        WRITE(6,7390)
7390    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' SCORING/TREATMENT FIELD TYPE:'/ ' -------------------
     *------------------------------------------'/ ' '/ ' There are four
     * different field types:'/ ' '/ '    field type = 0 for circular fi
     *eld with circular ring bins'/ '               = 1 for square field
     * with square ring bins'/ '               = 2 for rectangular field
     * with rectangular bins'/ ' '/ ' Note: field type will be the samef
     *or all the sub-sources.'/ ' --------------------------------------
     *-----------------------'/)
      ELSE IF((NMESSAGE.EQ.44)) THEN
        WRITE(6,7400)200,200,200
7400    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR THE CIRCULAR FIELD PLANAR FLUENCE DIST
     *RIBUTION:'/ ' ----------------------------------------------------
     *---------'/ ' '/ ' The following parameters are required for thiss
     *ource to'/ ' describe the circular field planar fluence distributi
     *on:'/ ' '/ '  1) number of bins for the planar fluence distributio
     *n'/ '     inside the treatment field (<',I4,')'/ '  2) radius ofth
     *e treatment field'/ '     Note: use slightly bigger value'/ '  3) 
     *number of bins for the planar fluence distribution'/ '     outside
     * the treatment field (<',I4,')'/ '  4) radius of the scoring field
     * (=1 + a margin)'/ ' '/ ' Note: These will be the same for all the
     * sub-sources.'/ '       Total number of bins must be smaller than 
     *(<',I4,').'/ ' ---------------------------------------------------
     *----------'/)
      ELSE IF((NMESSAGE.EQ.45)) THEN
        WRITE(6,7410)
7410    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR A RECTANGULAR SCORING FIELD:'/ ' -----
     *--------------------------------------------------------'/ ' '/ 'T
     *he following parameters are required for the planar fluence'/ ' di
     *stribution in the scoring field (usually > treatment field):'/ ''/
     * '  1) number of bins for the planar fluence (same for x & y)'/'  
     *2) minimum x coordinate for the scoring field'/ '  3) maximum x co
     *ordinate for the scoring field'/ '  4) minimum y coordinate for th
     *e scoring field'/ '  5) maximum y coordinate for the scoringfield'
     */ ' -------------------------------------------------------------'
     */)
      ELSE IF((NMESSAGE.EQ.46)) THEN
        WRITE(6,7420)
7420    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS DESCRIBING A RECTANGULAR TREATMENT FIELD:'
     */ ' -------------------------------------------------------------'
     */ ' '/ ' The following parameters are required to define the field
     *'/ ' for treatment or measurement. Energy spectrum will be scored'
     */ ' separately for particles falling inside and outside this '/' t
     *reatment field:   '/ ' '/ '  1) minimum x coordinate for thescorin
     *g field'/ '  2) maximum x coordinate for the scoring field'/'  3) 
     *minimum y coordinate for the scoring field'/ '  4) maximum y coord
     *inate for the scoring field'/ ' '/ ' NOTE: The treatmentfield shou
     *ld be inside the scoring field.'/ ' ------------------------------
     *-------------------------------'/)
      ELSE IF((NMESSAGE.EQ.47)) THEN
        WRITE(6,7430)200,200,200
7430    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' PARAMETERS FOR THE SQUARE FIELD PLANAR FLUENCE DISTRI
     *BUTION:'/ ' ------------------------------------------------------
     *-------'/ ' '/ ' The following parameters are required for this so
     *urce to'/ ' describe the square field planar fluence distribution:
     *'/ ' '/ '  1) number of bins for the planar fluence distribution'/
     *'     inside the treatment field (<',I4,')'/ '  2) half-width of t
     *he treatment field'/ '     Note: use slightly bigger value'/ ' 3) 
     *number of bins for the planar fluence distribution'/ '     outside
     * the treatment field (<',I4,')'/ '  4) half-width of the scoring f
     *ield = treat. field + a margin'/ ' '/ ' Note: These will be the sa
     *me for all the sub-sources.'/ '       Total number of bins must be
     * smaller than (<',I4,').'/ ' -------------------------------------
     *------------------------'/)
      ELSE IF((NMESSAGE.EQ.49)) THEN
        WRITE(6,7440)
7440    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' WARNING ON SOURCE MODEL FILE NAME INPUT:'/ ' --------
     *-----------------------------------------------------'/ ' '/ ' Are
     * you sure that there is a file in the specified directory'/ ' or i
     *t contains the right data? Check it out and then decide'/ ' whethe
     *r to try again or switch to another file!!!'/ ' ------------------
     *-------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.50)) THEN
        WRITE(6,7450)
7450    FORMAT(/ ' -----------------------------------------------------
     *----------'/ ' FILE CONTAINING FULL PHASE-SPACE DATA:'/ ' --------
     *-------------------------------------------------------'/ ' '/ ' T
     *he full phase-space data created by the NRCC EGSnrc usercode'/ ' B
     *EAMnrc is stored in a compressed form. This programme uncompresses
     *'/ ' the data first and then analyses the data.'/ ' '/ ' The nameo
     *f the file containing full phase-space data should be'/ ' supplied
     * here (with extension). '/ ' '/ ' A reminder - default BEAM ph-sp 
     *file name: [filename].egsphsp1'/ ' -------------------------------
     *--------------------------------'/)
      ELSE IF((NMESSAGE.EQ.51)) THEN
        WRITE(6,7460)
7460    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' Input the maximum number of particles to output to
     * the scatter'/ ' plot.  The number actually output may be less tha
     *n this because'/ ' it is also limited by the charge and/or LATCH b
     *its selected above.'/ ' If input is <=0 or > no. of particles in p
     *hase space file'/ ' then this defaults to the total no. of particl
     *es in the file.'/ ' ----------------------------------------------
     *------------------'/)
      ELSE IF((NMESSAGE.EQ.52)) THEN
        WRITE(6,7470)
7470    FORMAT(/ ' -----------------------------------------------------
     *--------------'/ ' MIN. ENERGY, MAX. ENERGY FOR PARTICLES IN THE A
     *NGULAR DISTRIBUTION:'/ ' -----------------------------------------
     *--------------------------'/ ' '/ ' If you are interested in the a
     *ngular distribution of all    '/ ' particles, then the energy rang
     *e given below should be consistent'/ ' with the phase-space data.F
     *or example, Emin should be =< (ECUT-0.511)'/ ' used in the simulat
     *ion with BEAM, and Emax should be greater '/ ' than the maximum ki
     *netic energy of the particles in the ph-sp data.'/ ' '/ ' --------
     *-----------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.53)) THEN
        WRITE(6,7480)
7480    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' Type of angular distribution:'/ ' -------------------
     *------------------------------------------'/ ' '/ ' (0) - plot par
     *ticles/angular bin'/ ' '/ ' (1) - plot particles/solid angle'/ ' '
     */ ' -------------------------------------------------------------'
     */)
      ELSE IF((NMESSAGE.EQ.54)) THEN
        WRITE(6,7490)
7490    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' FILE NAME INPUT:'/ ' --------------------------------
     *-----------------------------'/ ' '/ ' File name of the new/modifi
     *ed source model.'/ ' ---------------------------------------------
     *----------------'/)
      ELSE IF((NMESSAGE.EQ.55)) THEN
        WRITE(6,7500)
7500    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' FILE NAME INPUT:'/ ' --------------------------------
     *-----------------------------'/ '                             '/ '
     * The name of the existing sourcemodel file to be used as a'/ ' ref
     *erence for sub-source specifierinputs.'/ ' '/ ' The file name shou
     *ld be supplied here (with extension < A80)'/ ' and make sure that 
     *it exists and in the right directory!'/ ' '/ ' The most important 
     *thing is to make sure that thefile'/ ' contains the right data. Ch
     *eck its contents if you are asked'/ ' to supply with the file name
     * again.'/ ' ------------------------------------------------------
     *-------'/)
      ELSE IF((NMESSAGE.EQ.56)) THEN
        WRITE(6,7510)
7510    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' FILE NAME INPUT:'/ ' --------------------------------
     *-----------------------------'/ ' '/ ' The name of the existing so
     *urce model containing sub-source  '/ ' specifiers to be modified.'
     */ ' '/ ' The file name should be supplied here (with extension < A
     *80)'/ ' and make sure that it exists and in the right directory!'/
     * ' '/ ' The most important thing is to make sure that the file '/ 
     *' contains the right data. Check its contents if you are asked'/ '
     * to supply with the file name again.'/ ' -------------------------
     *------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.57)) THEN
        WRITE(6,7520)
7520    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' FILE NAME INPUT:'/ ' --------------------------------
     *-----------------------------'/ ' '/ ' The name of the existing so
     *urce model file containing'/ ' sub-source specifiers to be used fo
     *r analyzing the phase space'/ ' data.'/ ' '/ ' The file name shoul
     *d be supplied here (with extension < A80)'/ ' and make sure that i
     *t exists and in the right directory!'/ ' '/ ' The most important t
     *hing is to make sure that the file'/ ' contains the right data. Ch
     *eck its contents if you are asked'/ ' to supply with the file name
     * again.'/ ' ------------------------------------------------------
     *-------'/)
      ELSE IF((NMESSAGE.EQ.58)) THEN
        WRITE(6,7530)
7530    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' FILE NAME INPUT:'/ ' --------------------------------
     *-----------------------------'/ ' '/ ' The name of the source mode
     *l file whose characteristics are '/ ' to be plotted.'/ ' '/ ' Thef
     *ile name should be supplied here (with extension < A80)'/ ' and ma
     *ke sure that it exists and in the right directory!'/ ' '/ ' The mo
     *st important thing is to make sure that the file '/ ' contains the
     * right data. Check its contents if you are asked'/ ' to supply wit
     *h the file name again.'/ ' ---------------------------------------
     *----------------------'/)
      ELSE IF((NMESSAGE.EQ.59)) THEN
        WRITE(6,7540)
7540    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' DATA FILE FOR A SOURCE MODEL BASED ON FULL PHASE_S
     *PACE DATA'/ ' ****************************************************
     *************'/ ' '/ ' The program creates a source model file fora
     * Monte'/ ' Carlo code. The source model is based on the full phase
     *-space'/ ' data created by NRCC EGSnrc usercode BEAMnrc for a part
     *icular beam'/ ' from a clinical linear accelerator.')
        WRITE(6,7550)
7550    FORMAT(/ ' The source model consists of a number of sub-sources;
     * each has'/ ' its own energy spectrum and field distribution at th
     *e phantom'/ ' surface. The component modules in a linear accelerat
     *or are'/ ' considered as following types of sub-sources:'// '     
     *  source type 1 & 11 for applicators'/ '   2 for collimators'/ '  
     *                      3 for rings, cones, and point sources'/ '   
     *                     4 for rectangular plane sources'/ '          
     *              5 for circular plane sources'/ ' ')
        WRITE(6,7560)
7560    FORMAT(' For more information about the simplified source models
     *, see'// ' report "Beam Characterization: A Multiple-Source Model"
     *'/ ' by C-M Ma and D W O Rogers, IRS, NRC, Ottawa, Canada K1A 0R6'
     */)
      ELSE IF((NMESSAGE.EQ.60)) THEN
        WRITE(6,7570)
7570    FORMAT(/ ' -----------------------------------------------------
     *-------------------'/ ' USING SUB-SOURCE SPECIFIERS FROM AN EXISTI
     *NG SOURCE MODEL FOR REFERENCE:'/ ' -------------------------------
     *-----------------------------------------'/ ' '/ ' In order to hel
     *p input the sub-source specifiers required to create a'/ ' new sou
     *rce model, the sub-source specifiers from an existing source'/ ' m
     *odel can be used as a reference. However, if you do this, then the
     *'/ ' old and new source models will consist of the same number ofs
     *ub-sources.'/ ' '/ ' In fact, it is sometimes easier to modify the
     * sub-source specifiers'/ ' directly by editing the source model th
     *an doing it interactively'/ ' by running this program, and you are
     * not confined by the'/ ' condition mentioned above.'/ ' ----------
     *--------------------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.61)) THEN
        WRITE(6,7580)
7580    FORMAT(/ ' -----------------------------------------------------
     *-------------'/ ' FILE CONTAINING FULL PHASE-SPACE DATA:'/ ' -----
     *-------------------------------------------------------------'/ ''
     */ ' The full phase-space data created by the NRCC EGSnrc usercode'
     */ ' BEAMnrc is stored in a compressed form. This programme uncompr
     *esses'/ ' the data first and then analyses the data.'/ ' '/ ' The 
     *name of the file containing full phase-space data should be'/ ' su
     *pplied here (with extension).'/ ' '/ ' A reminder - default BEAMph
     *-sp file name: [filename].egsphsp1'/ ' ---------------------------
     *---------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.80)) THEN
        WRITE(6,7590)
7590    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' ENERGY SPECTRUM TYPE:'/ ' ------------------------
     *----------------------------------------'/ ' '/ ' There are two di
     *fferent energy spectrum types:'/ ' '/ ' 1) Inside and outside thet
     *reatment field'/ ' 2) Different radii (independent of treatment-sc
     *oring fields)'/ ' '/ ' -------------------------------------------
     *---------------------'/)
      ELSE IF((NMESSAGE.EQ.81)) THEN
        WRITE(6,7600)
7600    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' ENERGY SPECTRUM TYPE FOR INSIDE/OUTSIDE FIELD:'/ '
     * ----------------------------------------------------------------'
     */ ' The following parameters are required for this energy spectrum
     *'/ ' type:'/ ' '/ '  1) radius of the treatment field'/ '  2) radi
     *us of the scoring field'/ ' '/ ' ---------------------------------
     *-------------------------------'/)
      ELSE IF((NMESSAGE.EQ.82)) THEN
        WRITE(6,7610)200
7610    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' ENERGY SPECTRUM TYPE WITH DIFFERENT RADII:'/ ' ---
     *-------------------------------------------------------------'/ 'T
     *he following parameters are required for this energy spectrum'/ ' 
     *type:'/ ' '/ '  1) number of energy bins with different radii (<',
     *I4,')'/ '  2) radius of the scoring field'/ ' '/ ' ---------------
     *-------------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.83)) THEN
        WRITE(6,7620)
7620    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' ENERGY SPECTRUM RADII:'/ ' -----------------------
     *-----------------------------------------'/ ' The following parame
     *ters are required for this energy spectrum'/ ' radii:'/ ' '/ '  Nb
     *inEn number of different radii'/ ' '/ ' Note: the last radius must
     * be equal to radius of the scoring field'/ ' ---------------------
     *-------------------------------------------'/)
      ELSE IF((NMESSAGE.EQ.84)) THEN
        WRITE(6,7630)
7630    FORMAT(/ ' -----------------------------------------------------
     *--------'/ ' Parameters for writing particle (X,Y,Z) to a file (IA
     *EA phsp only):'/ ' -----------------------------------------------
     *--------------'/ ' '/ ' The following parameters are input:'/ ' '/
     *'  1) Min. fractional MU index (default 0)'/ '  2) Max. fractional
     * MU index (default 1)'/ '  3) No. of particles to plot (default al
     *l in range)'/ ' '/ ' If MU index is not scored, then it will not b
     *e used as a filter.'/ ' ------------------------------------------
     *-------------------'/)
      ELSE
        WRITE(6,7640)
7640    FORMAT(/ ' -----------------------------------------------------
     *-----------'/ ' ERROR: THIS MESSAGE TYPE DOES NOT EXIST'/ ' ------
     *----------------------------------------------------------'/)
      END IF
      RETURN
      END
      SUBROUTINE XVGRPLOT (X, Y, ERRY, NPTS, CURVENUM, SERIESTITLE, XTIT
     *LE, YTITLE, GRAPHTITLE, SUBTITLE, UNITNUM, TYPE, HISTXMIN, AXISTYP
     *E)
      IMPLICIT NONE
      integer*4 MAX, IDEBUG
      PARAMETER (MAX = 400)
      integer*4 NPTS,NPTS1,CURVENUM, COUNT,UNITNUM,TYPE,AXISTYPE
      real*8 X(NPTS),Y(NPTS),ERRY(NPTS),YMIN,HISTXMIN,ERRYOLD,SMALLESTX,
     * SMALLESTY,FUDGE
      integer*4 TITLELENGTH,SUBLENGTH,XAXISLENGTH,YAXISLENGTH,SERIESLENG
     *TH
      integer*4 LOGX, LOGY, LOGDY,ZEROYCOUNT
      CHARACTER*(*) SUBTITLE
      CHARACTER*(*) GRAPHTITLE,XTITLE,YTITLE,SERIESTITLE
      CHARACTER*10 INDEX
      CHARACTER*3 INDEXNUM
      logical TESTFILE, ALLPOS
      real*8 one
      parameter (one = 1)
      FUDGE = 1.e-10
      IDEBUG = 0
      IF ((IDEBUG .EQ. 1)) THEN
        write(6,'(//'' Entering xvgrplot ''/)')
        write(6,'('' Curve'',I3,'' to go to unit'',I3)')CURVENUM,UNITNUM
        write(6,'(''SERIESTITLE: '',a60)') SERIESTITLE
        write(6,'(''XTITLE:      '',a60)') XTITLE
        write(6,'(''YTITLE       '',a60)') YTITLE
        write(6,'(''GRAPHTILE:   '',a60)') graphtitle
        write(6,'(''SUBTITLE:    '',a80)') subtitle
      END IF
      IF (( NPTS .gt. MAX)) THEN
        WRITE(6,7650)NPTS, MAX
7650    FORMAT(//' **************************'/ ' Number of points asked
     * for =', I5, ' is greater than max allowed of', I4/ ' Setting NPTS
     * to MAX, you could adjust MAX in xvgrplot.mortran'/ ' ************
     ***************'//)
        NPTS1 = MAX
      ELSE
        NPTS1 = NPTS
      END IF
      INQUIRE(UNIT = UNITNUM,OPENED=TESTFILE)
      IF ((.NOT.TESTFILE)) THEN
        WRITE(6,7660) UNITNUM
7660    FORMAT (//'  ---------Error in Subroutine XVGRPLOT---------' ,/'
     *   Unit specified (',I2,') is not open.' ,/'   Unit must be opened
     * before using subroutine.' ,/'   Data not written to file.' ,/'  -
     *---------------------------------------------'//)
        RETURN
      END IF
      IF ((GRAPHTITLE .EQ. ' ')) THEN
        GRAPHTITLE = 'Untitled Graph - No title specified in subroutine'
      END IF
      IF ((XTITLE .EQ. ' ')) THEN
        XTITLE = 'X-axis not titled in subroutine'
      END IF
      IF ((YTITLE .EQ. ' ')) THEN
        YTITLE = 'Y-axis not titled in subroutine'
      END IF
      IF ((SERIESTITLE .EQ. ' ')) THEN
        SERIESTITLE = 'series # '
        INDEX = '0123456789'
        INDEXNUM = INDEX(CURVENUM+1:CURVENUM+1)
        SERIESTITLE(9:9) = INDEXNUM
      END IF
      TITLELENGTH = 61
      SUBLENGTH = 61
      XAXISLENGTH = 61
      YAXISLENGTH = 61
      SERIESLENGTH = 61
7671  CONTINUE
        TITLELENGTH = TITLELENGTH - 1
        IF(((GRAPHTITLE(TITLELENGTH:TITLELENGTH) .NE. ' ')))GO TO7672
      GO TO 7671
7672  CONTINUE
7681  CONTINUE
        SUBLENGTH = SUBLENGTH - 1
        IF(((SUBTITLE(SUBLENGTH:SUBLENGTH) .NE. ' ')))GO TO7682
      GO TO 7681
7682  CONTINUE
7691  CONTINUE
        XAXISLENGTH = XAXISLENGTH - 1
        IF(((XTITLE(XAXISLENGTH:XAXISLENGTH) .NE. ' ')))GO TO7692
      GO TO 7691
7692  CONTINUE
7701  CONTINUE
        YAXISLENGTH = YAXISLENGTH - 1
        IF(((YTITLE(YAXISLENGTH:YAXISLENGTH) .NE. ' ')))GO TO7702
      GO TO 7701
7702  CONTINUE
7711  CONTINUE
        SERIESLENGTH = SERIESLENGTH - 1
        IF(((SERIESTITLE(SERIESLENGTH:SERIESLENGTH) .NE. ' ')))GO TO7712
      GO TO 7711
7712  CONTINUE
      IF (( IDEBUG .EQ. 1)) THEN
        WRITE(6,7720)SERIESLENGTH,YAXISLENGTH,XAXISLENGTH,SUBLENGTH,TITL
     *  ELENGTH
7720    FORMAT(' SERIESLENGTH,YAXISLENGTH,XAXISLENGTH,SUBLENGTH,TITLELEN
     *GTH'/ 5I10)
      END IF
      LOGX = 0
      LOGY = 0
      LOGDY = 0
      ZEROYCOUNT=0
      ALLPOS=.TRUE.
      IF (( X(1).EQ.0.0 )) THEN
        SMALLESTX = 0.1
      ELSE
        SMALLESTX=X(1)
      END IF
      IF (( Y(1).EQ.0.0 )) THEN
        SMALLESTY = 0.1
      ELSE
        SMALLESTY=Y(1)
      END IF
      DO 7731 COUNT=1,NPTS1
        IF (((X(COUNT) .LT. SMALLESTX) .AND. (X(COUNT).NE.0.))) THEN
          SMALLESTX=X(COUNT)
        END IF
        IF (((Y(COUNT) .LT. SMALLESTY) .AND. (Y(COUNT).NE.0.))) THEN
          SMALLESTY=Y(COUNT)
        END IF
        IF (((X(COUNT) .LT. 0.).OR.(Y(COUNT) .LT. 0.))) THEN
          ALLPOS=.FALSE.
        END IF
7731  CONTINUE
7732  CONTINUE
      IF ((ALLPOS)) THEN
        DO 7741 COUNT=1,NPTS1
          IF ((X(COUNT).EQ.0.)) THEN
            X(COUNT)=SMALLESTX*FUDGE
          END IF
          IF ((Y(COUNT).EQ.0.)) THEN
            Y(COUNT)=SMALLESTY*FUDGE
          END IF
7741    CONTINUE
7742    CONTINUE
      END IF
      IF ((AXISTYPE .GT. 0)) THEN
        DO 7751 COUNT=1,NPTS1
          IF ((X(COUNT) .LE. 0.)) THEN
            LOGX = 1
          END IF
          IF ((Y(COUNT) .LE. 0.)) THEN
            LOGY = 1
          END IF
          IF (((Y(COUNT)-ERRY(COUNT)) .LE. 0.)) THEN
            LOGDY = 1
          END IF
7751    CONTINUE
7752    CONTINUE
      ELSE
        DO 7761 COUNT=1,NPTS1
          IF ((Y(COUNT)-ERRY(COUNT) .LT. Y(COUNT)-0.9999*Y(COUNT) .AND.
     *    Y(COUNT).GT.0.)) THEN
            ZEROYCOUNT=ZEROYCOUNT+1
            IF ((ZEROYCOUNT.EQ.1)) THEN
              WRITE(6,7770)
7770          FORMAT (/' ------------WARNING from Subroutine XVGRPLOT---
     *--------' ,/'  Some errors give 0 value and so are adjusted so   '
     * ,/'  that you can switch to a log Y scale while in xmgr.   ',/' -
     *------------------------------------------------------'/)
            END IF
            ERRYOLD=ERRY(COUNT)
            ERRY(COUNT) = 0.9999*Y(COUNT)
          END IF
7761    CONTINUE
7762    CONTINUE
      END IF
      IF ((CURVENUM .EQ. 0)) THEN
        IF ((AXISTYPE .EQ. 0)) THEN
          WRITE(UNITNUM,7780) 'xy'
        ELSE IF((AXISTYPE .EQ. 1)) THEN
          WRITE(UNITNUM,7780) 'logy'
          WRITE(UNITNUM,7790)
        ELSE IF((AXISTYPE .EQ. 2)) THEN
          WRITE(UNITNUM,7780) 'logx'
          WRITE(UNITNUM,7790)
        ELSE IF((AXISTYPE .EQ. 3)) THEN
          WRITE(UNITNUM,7780) 'logxy'
          WRITE(UNITNUM,7790)
          WRITE(UNITNUM,7800)
        ELSE
          WRITE(6,7810) AXISTYPE
7810      FORMAT (//'  ------------Error in Subroutine XVGRPLOT---------
     *--' ,/'   AXISTYPE specified (',I2,') is not a valid option.' ,/' 
     *----------------------------------------------'//)
          RETURN
        END IF
7780    FORMAT ('@g0 type ',A,' ')
7790    FORMAT ('@    xaxis  ticklabel format exponential')
7800    FORMAT ('@    yaxis  ticklabel format exponential')
        WRITE(UNITNUM,7820) GRAPHTITLE(1:TITLELENGTH) ,SUBTITLE(1:SUBLEN
     *  GTH) ,XTITLE(1:XAXISLENGTH) ,YTITLE(1:YAXISLENGTH)
7820    FORMAT ('@    title "',A,'"'/ ,'@    subtitle "',A,'"'/ ,'@    l
     *egend on'/ ,'@    legend box linestyle 0'/ ,'@    legend x1 0.6'/,
     *'@    legend y1 0.75'/ ,'@    view xmin 0.250000'/ ,'@    xaxis  l
     *abel "',A,'"'/ ,'@    timestamp on'/ ,'@    yaxis  label "',A,'"')
      END IF
      IF ((AXISTYPE .EQ. 1 .AND. LOGY .EQ. 1)) THEN
        WRITE(UNITNUM,7780) 'xy'
        WRITE(6,7830)
7830    FORMAT (/' ----------WARNING from Subroutine XVGRPLOT---------',
     */'  Log scale requested for Y axis when one or more   ' ,/'  Ydata
     * points are 0 or negative.                  ' ,//'  Y axis scale c
     *hanged to linear.                   ' ,/' ------------------------
     *---------------------------'/)
      END IF
      IF ((AXISTYPE .EQ. 2 .AND. LOGX .EQ. 1)) THEN
        WRITE(UNITNUM,7780) 'xy'
        WRITE(6,7840)
7840    FORMAT (/' ----------WARNING from Subroutine XVGRPLOT---------',
     */'  Log scale requested for X axis when one or more   ' ,/'  Xdata
     * points are 0 or negative.                  ' ,//'  X axis scale c
     *hanged to linear.                   ' ,/' ------------------------
     *---------------------------'/)
      END IF
      IF ((AXISTYPE .EQ. 3 .AND. (LOGX .EQ. 1 .OR. LOGY .EQ. 1))) THEN
        IF ((LOGX .EQ. 1 .AND. LOGY .EQ. 1)) THEN
          WRITE(UNITNUM,7780) 'xy'
          WRITE(6,7850)
7850      FORMAT (/' ----------WARNING from Subroutine XVGRPLOT---------
     *' ,/'  Log scale requested for X axis and Y axis when    ' ,/'  on
     *e or more X and Y data points are 0 or negative.' ,//'  X and Y ax
     *es scales changed to linear.            ' ,/' --------------------
     *-------------------------------'/)
        ELSE IF((LOGX .EQ. 1)) THEN
          WRITE(UNITNUM,7780) 'logy'
          WRITE(6,7840)
        ELSE
          WRITE(UNITNUM,7780) 'logx'
          WRITE(6,7830)
        END IF
      END IF
      IF ((LOGDY .EQ. 1 .AND. LOGY .NE. 1 .AND. (AXISTYPE .EQ. 3 .OR. AX
     *ISTYPE .EQ. 1))) THEN
        WRITE(6,3200)
3200    FORMAT (/' ------------WARNING from Subroutine XVGRPLOT---------
     *--' ,/'  Log scale requested for Y axis, and Y value less      ' ,
     */'  error gives 0 or negative value.                      ' ,//'  
     *Error adjusted to aviod negavite values on log scale. ' ,/' ------
     *-------------------------------------------------'/)
        DO 7861 COUNT=1,NPTS1
          IF ((Y(COUNT)-ERRY(COUNT) .LT. Y(COUNT)-0.9999*Y(COUNT))) THEN
            ERRYOLD = ERRY(COUNT)
            ERRY(COUNT) = 0.9999*Y(COUNT)
            WRITE(6,7870) COUNT,ERRYOLD,ERRY(COUNT)
7870        FORMAT (/'  Error adjusted on point #',I2,' from',1PE11.4, '
     * to' /'        ',1PE11.4,'.')
          END IF
7861    CONTINUE
7862    CONTINUE
        WRITE(6,7880)
7880    FORMAT (/' -----------------------------------------------------
     *--'/)
      END IF
      IF ((CURVENUM .LT. 10 )) THEN
        WRITE(UNITNUM,'(''@    s'',I1,'' on'')') CURVENUM
      ELSE
        WRITE(UNITNUM,'(''@    s'',I2,'' on'')') CURVENUM
      END IF
      WRITE(UNITNUM,7890) CURVENUM,SERIESTITLE(1:SERIESLENGTH)
7890  FORMAT ('@    legend string ',I2,' "',A,'"')
      IF ((TYPE .EQ. 0)) THEN
        DO 7901 COUNT=1,NPTS1
          IF ((ERRY(COUNT) .NE. 0)) THEN
            GOTO 7910
          END IF
7901    CONTINUE
7902    CONTINUE
        WRITE(UNITNUM,7920)
7920    FORMAT ('@TYPE xy')
        IF ((CURVENUM .LT. 10)) THEN
          WRITE(UNITNUM,7930) CURVENUM
          IF ((CURVENUM .EQ. 9)) THEN
            WRITE(UNITNUM,7940) CURVENUM, CURVENUM+1
          ELSE
            WRITE(UNITNUM,7950) CURVENUM, CURVENUM+1
          END IF
        ELSE
          WRITE(UNITNUM,7960) CURVENUM
          WRITE(UNITNUM,7970) CURVENUM, CURVENUM+1
        END IF
7930    FORMAT ('@    s',I1,' errorbar length 0.000000')
7960    FORMAT ('@    s',I2,' errorbar length 0.000000')
7940    FORMAT ('@    s',I1,' symbol color ',I2)
7950    FORMAT ('@    s',I1,' symbol color ',I1)
7970    FORMAT ('@    s',I2,' symbol color ',I2)
        DO 7981 COUNT=1,NPTS1
          WRITE(UNITNUM,7990) X(COUNT),Y(COUNT)
7981    CONTINUE
7982    CONTINUE
7990    FORMAT (1PE15.4,1PE15.4)
        GOTO 8000
7910    CONTINUE
        WRITE(UNITNUM,8010)
8010    FORMAT ('@TYPE xydy')
        IF ((CURVENUM .LT. 10)) THEN
          WRITE(UNITNUM,7930) CURVENUM
          IF ((CURVENUM .EQ. 9)) THEN
            WRITE(UNITNUM,7940) CURVENUM, CURVENUM+1
          ELSE
            WRITE(UNITNUM,7950) CURVENUM, CURVENUM+1
          END IF
        ELSE
          WRITE(UNITNUM,7960) CURVENUM
          WRITE(UNITNUM,7970) CURVENUM, CURVENUM+1
        END IF
        DO 8021 COUNT=1,NPTS1
          WRITE(UNITNUM,8030) X(COUNT),Y(COUNT),ERRY(COUNT)
8021    CONTINUE
8022    CONTINUE
8030    FORMAT (1PE15.4,1PE15.4,1PE15.4)
8000    CONTINUE
      ELSE
        YMIN = ABS(1.E5 * Y(1))
        DO 8041 COUNT=1,NPTS1
          IF ((ABS(Y(COUNT)) .LT. YMIN)) THEN
            YMIN = ABS(Y(COUNT))
          END IF
8041    CONTINUE
8042    CONTINUE
        YMIN = SIGN(one,Y(1)) * 1.E-5 * YMIN
        Y(NPTS1+1) = YMIN
        IF (((AXISTYPE .EQ. 2 .OR. AXISTYPE .EQ. 3) .AND. HISTXMIN .EQ.
     *  0)) THEN
          IF ((X(1) .EQ. (X(2)-X(1)))) THEN
            HISTXMIN = X(1)-0.5*(X(2)-X(1))
          ELSE
            HISTXMIN = X(1)-(X(2)-X(1))
          END IF
          WRITE(6,8050) HISTXMIN
8050      FORMAT (/' ---------WARNING from Subroutine XVGRPLOT--------',
     */'  Minimum bin for X specified as 0 with log scale  ' ,/'  on X a
     *xis.  Minimum X bin set to ',1PE10.3,'.' ,/' ---------------------
     *----------------------------'/)
        END IF
        DO 8061 COUNT=1,NPTS1
          IF ((ERRY(COUNT) .NE. 0.)) THEN
            GOTO 8070
          END IF
8061    CONTINUE
8062    CONTINUE
        IF ((CURVENUM .LT. 10)) THEN
          WRITE(UNITNUM,7930) CURVENUM
          IF ((CURVENUM .EQ. 9)) THEN
            WRITE(UNITNUM,7940) CURVENUM, CURVENUM+1
          ELSE
            WRITE(UNITNUM,7950) CURVENUM, CURVENUM+1
          END IF
        ELSE
          WRITE(UNITNUM,7960) CURVENUM
          WRITE(UNITNUM,7970) CURVENUM, CURVENUM+1
        END IF
        WRITE(UNITNUM,7920)
        WRITE(UNITNUM,7990) HISTXMIN,YMIN
        WRITE(UNITNUM,7990) HISTXMIN,Y(1)
        DO 8081 COUNT=1,NPTS1
          WRITE(UNITNUM,7990) X(COUNT),Y(COUNT)
          WRITE(UNITNUM,7990) X(COUNT),Y(COUNT+1)
8081    CONTINUE
8082    CONTINUE
        GOTO 8090
8070    CONTINUE
        ERRY(NPTS1+1) = 0.0
        IF ((CURVENUM .LT. 10)) THEN
          WRITE(UNITNUM,7930) CURVENUM
          IF ((CURVENUM .EQ. 9)) THEN
            WRITE(UNITNUM,7940) CURVENUM, CURVENUM+1
          ELSE
            WRITE(UNITNUM,7950) CURVENUM, CURVENUM+1
          END IF
        ELSE
          WRITE(UNITNUM,7960) CURVENUM
          WRITE(UNITNUM,7970) CURVENUM, CURVENUM+1
        END IF
        WRITE(UNITNUM,8010)
        IF ((HISTXMIN .EQ. 0.0)) THEN
          HISTXMIN = SMALLESTX*FUDGE
        END IF
        WRITE(UNITNUM,8030) HISTXMIN, YMIN, 0.
        WRITE(UNITNUM,8030) HISTXMIN, Y(1), 0.
        WRITE(UNITNUM,8030) (X(1)+HISTXMIN)/2., Y(1), ERRY(1)
        DO 8101 COUNT=1,NPTS1
          WRITE (UNITNUM,8030) X(COUNT),Y(COUNT),0.
          WRITE (UNITNUM,8030) X(COUNT),Y(COUNT+1),0.
          IF ((COUNT .LT. NPTS1)) THEN
            WRITE (UNITNUM,8030) (X(COUNT)+X(COUNT+1))/2.,Y(COUNT+1),ERR
     *      Y(COUNT+1)
          END IF
8101    CONTINUE
8102    CONTINUE
8090    CONTINUE
      END IF
      WRITE(UNITNUM,'(''&'')')
      RETURN
      END
      integer*4 function lnblnk1(string)
      character*(*) string
      DO 8111 i=len(string),1,-1
        j = ichar(string(i:i))
        IF (( j .EQ. 0 )) THEN
          lnblnk1 = i-1
          return
        END IF
        IF (( j .NE. 9 .AND. j .NE. 10 .AND. j .NE. 11 .AND. j .NE. 12 .
     *  AND. j .NE. 13 .AND. j .NE. 32 )) THEN
          lnblnk1 = i
          return
        END IF
8111  CONTINUE
8112  CONTINUE
      lnblnk1 = 0
      return
      end
