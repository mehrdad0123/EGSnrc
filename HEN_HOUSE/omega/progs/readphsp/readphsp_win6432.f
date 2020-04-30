      INTEGER I, IIN, IOUT, IQ, LATCH, CHARGE,  INMODE, OUTMODE, IWRITEM
     *AX,NPHOTON, in_particle_so_far,  out_part_so_far,  out_phot_so_far
     *,  in_particle_tot,   IWRITESTART, LRECORD, NHSTRY
      CHARACTER*70 NAMEIN, NAMEOUT
      CHARACTER*5 MODE
      CHARACTER*1 RECORDL
      REAL*4 AV_E(3), U, V, W, X, Y, WT, EMAXK,EMINE,NINC, tot_wt_read(3
     *),  tot_num_read(3),  ZLAST
      INTEGER NESBIN, NXBIN, NYBIN, NUBIN, NVBIN, BIN_NUM
      REAL*4 ESMIN, XMIN, YMIN, UMIN, VMIN, ESMAX, XMAX, YMAX, UMAX, VMA
     *X, ESBIN, XBIN, YBIN, UBIN, VBIN
      COMMON/PAWC/H(3000000)
      COMMON/QUEST/IQUEST(100)
      REAL XTUPLE(10),H
      INTEGER NTUPLE,IQUEST,ISTAT,ICYCLE,NPRIME,LRECL
      INTEGER*4 SWAPI,SWAPII
      INTEGER SWAPDUMMY1,SWAPDUMMY2,SWAPYES
      REAL*4 SWAPA
      CHARACTER*1 SWAPGOAHEAD,SWAPAWAY
      EQUIVALENCE(SWAPA,SWAPI)
      COMMON /phasesp/ e,x,y,zlast,wt,iq,latch1,latch2,w,u,v
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
      integer*8 IHSTRY_PHSP(1),iaea_dummy_long,NHSTRY_DOS
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
      EQUIVALENCE(REC_TEMP_OUT(1:4),LATCH_TEMP_OUT)
      EQUIVALENCE(REC_TEMP_OUT(5:8),REAL_TEMP_OUT(1))
      EQUIVALENCE(REC_TEMP_OUT(9:12),REAL_TEMP_OUT(2))
      EQUIVALENCE(REC_TEMP_OUT(13:16),REAL_TEMP_OUT(3))
      EQUIVALENCE(REC_TEMP_OUT(17:20),REAL_TEMP_OUT(4))
      EQUIVALENCE(REC_TEMP_OUT(21:24),REAL_TEMP_OUT(5))
      EQUIVALENCE(REC_TEMP_OUT(25:28),REAL_TEMP_OUT(6))
      EQUIVALENCE(REC_TEMP_OUT(29:32),REAL_TEMP_OUT(7))
      DATA tot_wt_read /3*0.0/, tot_num_read / 3*0.0/, AV_E /3*0.0/
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
      NPHOTON=0
      EMAXK=0.0
      NINC=0.0
      EMINE=0.0
      SWAPYES=0
      SWAPAWAY='n'
      CALL GETARG(1, NAMEIN)
      CALL GETARG(2, NAMEOUT)
      CALL GETARG(3, RECORDL)
      READ(RECORDL,'(I1)')LRECORD
1021  CONTINUE
          WRITE(6,1030)NAMEIN
1030      FORMAT(' WELCOME TO USE NRCC PROGRAM "READ PH_ase SPace"!'/ /'
     * MODE OF INPUT PHASE-SPACE FILE called: ',A70 /'         ACCESS=DI
     *RECT, THE DEFAULT      ---> 0' /'         ACCESS=SEQUENTIAL       
     * ---> 1' /'         ACCESS=DIRECT, WITH ZLAST       ---> 2'/'     
     *    ACCESS=SEQUENTIAL, WITH ZLAST   ---> 3' /'  Option:  ',$)
          READ(5,1040)INMODE
1040      FORMAT(I1)
          IF(((INMODE .GE. 0 .AND. INMODE .LE.3)))GO TO1022
      GO TO 1021
1022  CONTINUE
      IIN=1
      IF ((INMODE .EQ. 1)) THEN
          OPEN (IIN,FILE=NAMEIN,STATUS='OLD',FORM='UNFORMATTED')
          READ(IIN) MODE
          IF ((MODE .NE. 'MODE1')) THEN
              WRITE(6,1050)
1050          FORMAT(//' *** SORRY, THIS IS NOT A MODE1 FILE,', ' INPUTA
     * MODE1 FILE AND TRY AGAIN!'/)
              STOP
          END IF
      ELSE IF((INMODE .EQ. 3)) THEN
          OPEN (IIN,FILE=NAMEIN,STATUS='OLD',FORM='UNFORMATTED')
          READ(IIN) MODE
          IF ((MODE .NE. 'MODE3')) THEN
              WRITE(6,1060)
1060          FORMAT(//' *** SORRY, THIS IS NOT A MODE3 FILE,', ' INPUTA
     * MODE3 FILE AND TRY AGAIN!'/)
              STOP
          END IF
      ELSE
          IF ((INMODE.GT.0)) THEN
              PHSP_RECL=4*8
              OPEN(UNIT=IIN,FILE=NAMEIN,FORM='UNFORMATTED',ACCESS='DIREC
     *T', RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,1070)
1070              FORMAT(/' ***error opening file as MODE2 ****' /' ***T
     *HE FILE FORMAT MAY BE WRONG (I.E., NOT A MODE2 FILE) ***'/ /' *** 
     *WE NOW TRY TO OPEN IT AS A MODE3 FILE ***'//)
                  OPEN(UNIT=IIN,STATUS='OLD',FILE=NAMEIN, FORM='UNFORMAT
     *TED', IOSTAT=IERR_PHSP)
                  IF ((IERR_PHSP.NE.0)) THEN
                      WRITE(6,1080)
1080                  FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. *
     ***')
                      STOP
                  END IF
                  READ(IIN,REC=1,IOSTAT=IERR_PHSP)MODE,NUM_PHSP_TOT,PHOT
     *            _PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHS
     *            P_SHORT
                  in_particle_tot=NUM_PHSP_TOT
                  NPHOTON=PHOT_PHSP_TOT
                  EMAXK=EKMAX_PHSP_SHORT
                  EMINE=EKMINE_PHSP_SHORT
                  NINC=NINC_PHSP_SHORT
                  IF ((IERR_PHSP.NE.0)) THEN
                      WRITE(6,1090)
1090                  FORMAT(//' ***ERROR READING HEADER OF PHASE SPACEF
     *ILE*** '//)
                      STOP
                  END IF
                  IF ((MODE .NE. 'MODE3')) THEN
                      WRITE(6,1100)
1100                  FORMAT(//' That file does not start with MODE3,','
     * as all old compressed files (with ZLAST) must'/'  Check it out an
     *d try again!'///)
                      IERR_PHSP=1
                      STOP
                  ELSE
                      WRITE(6,1110)
1110                  FORMAT(//' This is a MODE3 file! '/ ' Please conve
     *rt it into a MODE2 file using [readphsp] ', ' and try again!'///)
                      IERR_PHSP=1
                      STOP
                  END IF
              END IF
              READ(IIN,REC=1,IOSTAT=IERR_PHSP)MODE,NUM_PHSP_TOT,PHOT_PHS
     *        P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
              in_particle_tot=NUM_PHSP_TOT
              NPHOTON=PHOT_PHSP_TOT
              EMAXK=EKMAX_PHSP_SHORT
              EMINE=EKMINE_PHSP_SHORT
              NINC=NINC_PHSP_SHORT
              IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,1120)
1120              FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE
     **** '//)
                  STOP
              END IF
              IF ((MODE .NE. 'MODE2')) THEN
                  WRITE(6,1130)
1130              FORMAT(//' That file does not start with MODE2,', ' as
     * standard compressed files with ZLAST must'/ '  Check it out and t
     *ry again!'///)
                  IERR_PHSP=1
                  STOP
              END IF
          ELSE IF((INMODE.EQ.0)) THEN
              PHSP_RECL=4*7
              OPEN(UNIT=IIN,FILE=NAMEIN,FORM='UNFORMATTED',ACCESS='DIREC
     *T', RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,1140)
1140              FORMAT(/' ***ERROR opening file as MODE0****' /' *** T
     *HE FILE FORMAT MAY BE WRONG (I.E., NOT A MODE0 FILE) ***'/ /' ***W
     *E NOW TRY TO OPEN IT AS A MODE1 FILE ***'//)
                  OPEN(UNIT=IIN,STATUS='OLD',FILE=NAMEIN, FORM='UNFORMAT
     *TED', IOSTAT=IERR_PHSP)
                  IF ((IERR_PHSP.NE.0)) THEN
                      WRITE(6,1150)
1150                  FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. *
     ***')
                      STOP
                  END IF
                  READ(IIN,REC=1,IOSTAT=IERR_PHSP)MODE,NUM_PHSP_TOT,PHOT
     *            _PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHS
     *            P_SHORT
                  in_particle_tot=NUM_PHSP_TOT
                  NPHOTON=PHOT_PHSP_TOT
                  EMAXK=EKMAX_PHSP_SHORT
                  EMINE=EKMINE_PHSP_SHORT
                  NINC=NINC_PHSP_SHORT
                  IF ((IERR_PHSP.NE.0)) THEN
                      WRITE(6,1160)
1160                  FORMAT(//' ***ERROR READING HEADER OF PHASE SPACEF
     *ILE*** '//)
                      STOP
                  END IF
                  IF ((MODE .NE. 'MODE1')) THEN
                      WRITE(6,1170)
1170                  FORMAT(//' That file does not start with MODE1,','
     * as all old compressed files must'/ '  Check it out and try again!
     *'///)
                      IERR_PHSP=1
                      STOP
                  ELSE
                      WRITE(6,1180)
1180                  FORMAT(//' This is a MODE1 file! '/ ' Please conve
     *rt it into a MODE0 file using [readphsp] ', 'and try again!'///)
                      IERR_PHSP=1
                      STOP
                  END IF
              END IF
              READ(IIN,REC=1,IOSTAT=IERR_PHSP)MODE,NUM_PHSP_TOT,PHOT_PHS
     *        P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
              in_particle_tot=NUM_PHSP_TOT
              NPHOTON=PHOT_PHSP_TOT
              EMAXK=EKMAX_PHSP_SHORT
              EMINE=EKMINE_PHSP_SHORT
              NINC=NINC_PHSP_SHORT
              IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,1190)
1190              FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE
     **** '//)
                  STOP
              END IF
              IF ((MODE .NE. 'MODE0')) THEN
                  WRITE(6,1200)
1200              FORMAT(/' Does not start with MODE0 as files without Z
     *LAST must'/ '  Try again!'//)
                  IERR_PHSP=1
                  STOP
              END IF
          ELSE IF((INMODE.LT.0)) THEN
              WRITE(6,1210)
1210          FORMAT(/' First, try to open it as a MODE0 file')
              PHSP_RECL=4*7
              OPEN(UNIT=IIN,FILE=NAMEIN,FORM='UNFORMATTED',ACCESS='DIREC
     *T', RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,1220)
1220              FORMAT(/' Now try to open it as a MODE2 file')
                  INMODE=1
                  PHSP_RECL=4*8
                  OPEN(UNIT=IIN,FILE=NAMEIN,FORM='UNFORMATTED',ACCESS='D
     *IRECT', RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
                  IF ((IERR_PHSP.NE.0)) THEN
                      WRITE(6,1230)
1230                  FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. *
     ***')
                  ELSE
                      READ(IIN,REC=1,IOSTAT=IERR_PHSP)MODE,NUM_PHSP_TOT,
     *                PHOT_PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,
     *                NINC_PHSP_SHORT
                      in_particle_tot=NUM_PHSP_TOT
                      NPHOTON=PHOT_PHSP_TOT
                      EMAXK=EKMAX_PHSP_SHORT
                      EMINE=EKMINE_PHSP_SHORT
                      NINC=NINC_PHSP_SHORT
                      IF ((IERR_PHSP.NE.0)) THEN
                          WRITE(6,1240)
1240                      FORMAT(//' ***ERROR READING HEADER OF PHASE SP
     *ACE FILE*** '//)
                          STOP
                      END IF
                      IF ((MODE.NE.'MODE2')) THEN
                          WRITE(6,1250)
1250                      FORMAT(//' That file does not start with MODE2
     *,', ' as standard compressed files with ZLAST must'/ '  Check it o
     *ut and try again!'///)
                          CLOSE(IIN)
                          STOP
                      END IF
                  END IF
              ELSE
                  READ(IIN,REC=1,IOSTAT=IERR_PHSP)MODE,NUM_PHSP_TOT,PHOT
     *            _PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHS
     *            P_SHORT
                  in_particle_tot=NUM_PHSP_TOT
                  NPHOTON=PHOT_PHSP_TOT
                  EMAXK=EKMAX_PHSP_SHORT
                  EMINE=EKMINE_PHSP_SHORT
                  NINC=NINC_PHSP_SHORT
                  IF ((IERR_PHSP.NE.0)) THEN
                      WRITE(6,1260)
1260                  FORMAT(//' ***ERROR READING HEADER OF PHASE SPACEF
     *ILE*** '//)
                      STOP
                  END IF
                  INMODE=0
                  IF ((MODE.NE.'MODE0')) THEN
                      WRITE(6,1270)
1270                  FORMAT(/' The file does not start with MODE0 as it
     * supposed to')
                      CLOSE(IIN)
                      WRITE(6,1280)
1280                  FORMAT(/' Now try to open it as a MODE2 file')
                      INMODE=1
                      PHSP_RECL=4*8
                      OPEN(UNIT=IIN,FILE=NAMEIN,FORM='UNFORMATTED',ACCES
     *                S='DIRECT', RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IER
     *                R_PHSP)
                      IF ((IERR_PHSP.NE.0)) THEN
                          WRITE(6,1290)
1290                      FORMAT(//' *** PHASE SPACE FILE CANNOT BE OPEN
     *ED. ***')
                      ELSE
                          READ(IIN,REC=1,IOSTAT=IERR_PHSP)MODE,NUM_PHSP_
     *                    TOT,PHOT_PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHS
     *                    P_SHORT,NINC_PHSP_SHORT
                          in_particle_tot=NUM_PHSP_TOT
                          NPHOTON=PHOT_PHSP_TOT
                          EMAXK=EKMAX_PHSP_SHORT
                          EMINE=EKMINE_PHSP_SHORT
                          NINC=NINC_PHSP_SHORT
                          IF ((IERR_PHSP.NE.0)) THEN
                              WRITE(6,1300)
1300                          FORMAT(//' ***ERROR READING HEADER OF PHAS
     *E SPACE FILE*** '//)
                              STOP
                          END IF
                          IF ((MODE.NE.'MODE2')) THEN
                              WRITE(6,1310)
1310                          FORMAT(//' That file does not start with M
     *ODE2,', ' as standard compressed files with ZLAST must'/ '  Checki
     *t out and try again!'///)
                              CLOSE(IIN)
                              STOP
                          END IF
                      END IF
                  END IF
              END IF
          END IF
          WRITE(6,1320)in_particle_tot,NPHOTON,EMAXK,EMINE,NINC
1320      FORMAT(//' *** THIS FILE CONTAINS          ',I13,' PARTICLES'/
     *'                                 ',I13,' OF THEM ARE PHOTONS'/ ' 
     *    MAXIMUM KINETIC ENERGY      ',F13.3,' MeV'/ '     MINIMUM ENER
     *GY FOR ELECTRONS',F13.3,' MeV'/ '    ORIGINAL # INCIDENT PARTICLES
     *',F13.1,/)
          IF ((in_particle_tot.LT.0.OR.NPHOTON.GT.in_particle_tot.OR. NP
     *    HOTON.LT.0.OR.EMAXK.LT.0.OR.EMINE.GT.EMAXK.OR.EMINE.LT.0.OR. N
     *    INC.LT.0)) THEN
              WRITE(6,1330)
1330          FORMAT(/1x,78('=')/,' IF THE ABOVE INFO IS NONSENSE, CONSI
     *DER BYTE SWAPPING', ' THIS FILE.'/1x,78('=')/)
          END IF
      END IF
1341  CONTINUE
          WRITE(6,1350)NAMEOUT
1350      FORMAT(' MODE OF OUTPUT FILE called: ',A70 /'      ACCESS=DIRE
     *CT (DEFAULT)        ---> 0' /'      ACCESS=SEQUENTIAL ---> 1' /'  
     *    ACCESS=DIRECT, WITH ZLAST      ---> 2' /'      ACCESS=SEQUENTI
     *AL, WITH ZLAST  ---> 3' /'      PAW FILE          ---> 4' /'      
     *SWAP BYTES--SAME MODE AS INPUT ---> 5' /' Option: ',$)
          READ(5,1360)OUTMODE
1360      FORMAT(I1)
          IF(((OUTMODE .GE. 0 .AND. OUTMODE .LE. 5)))GO TO1342
      GO TO 1341
1342  CONTINUE
      IOUT=2
      IF ((OUTMODE .EQ. 1)) THEN
          OPEN (IOUT,FILE=NAMEOUT,STATUS='NEW',FORM='UNFORMATTED')
          WRITE(IOUT)'MODE1'
          IF ((INMODE.EQ.2.OR.INMODE.EQ.3)) THEN
              WRITE(6,1370)
1370          FORMAT(//' *** ZLAST read in but not written out'/)
          END IF
      ELSE IF((OUTMODE .EQ. 0.OR.OUTMODE .EQ. 2)) THEN
          IF ((OUTMODE.NE.0)) THEN
              MODE='MODE2'
              PHSP_RECL=4*8000
          ELSE
              MODE='MODE0'
              PHSP_RECL=4*7000
          END IF
          OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RECL=PHSP_RECL, FO
     *    RM='UNFORMATTED',STATUS='UNKNOWN')
          CLOSE(IOUT)
          IF ((OUTMODE.NE.0)) THEN
              PHSP_RECL=4*8
          ELSE
              PHSP_RECL=4*7
          END IF
          OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT', FO
     *    RM='UNFORMATTED',STATUS='UNKNOWN')
          IF ((0.GT.2147483647)) THEN
              write(*,*)' Warning while writing phase space file:'
              write(*,*)' No. of particles > 2^31-1.'
              write(*,*)' Total no. of particles (and photons) written'
              write(*,*)' to header may be nonsense.'
          END IF
          NUM_PHSP_TOT=0
          PHOT_PHSP_TOT=0
          EKMAX_PHSP_SHORT=0
          EKMINE_PHSP_SHORT=0
          NINC_PHSP_SHORT=0
          WRITE(IOUT,REC=1)MODE,NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_PHSP_SH
     *    ORT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          CLOSE(IOUT)
          IF ((OUTMODE.NE.0)) THEN
              PHSP_RECL=4*8000
          ELSE
              PHSP_RECL=4*7000
          END IF
          OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT', FO
     *    RM='UNFORMATTED',STATUS='UNKNOWN')
          IF (((INMODE.EQ.2.OR.INMODE.EQ.3) .AND. OUTMODE.EQ.0)) THEN
              WRITE(6,1380)
1380          FORMAT(//' *** ZLAST read in but not written out'/)
          ELSE IF(((INMODE.EQ.0.OR.INMODE.EQ.1) .AND. OUTMODE.EQ.2)) THE
     *    N
              WRITE(6,1390)
1390          FORMAT(//' *** OUTPUT of ZLAST requested but not read in.S
     *et 0')
          END IF
      ELSE IF((OUTMODE .EQ. 3)) THEN
          OPEN (IOUT,FILE=NAMEOUT,STATUS='NEW',FORM='UNFORMATTED')
          WRITE(IOUT)'MODE3'
          IF ((INMODE.EQ.0.OR.INMODE.EQ.1)) THEN
              WRITE(6,1400)
1400          FORMAT(//' *** OUTPUT of ZLAST requested but not read in.S
     *et 0')
          END IF
      ELSE IF((OUTMODE .EQ. 4)) THEN
          CALL HLIMIT(3000000)
          NTUPLE=10
          NTUPLE_CWN=10
          LRECL=2048
          WRITE(6,1410)LRECL
1410      FORMAT(//' USING RECORD LENGTH IN WORDS = ',I8// ' IN PAW USE:
     * HI/FILE 2 PAWFILE 2048 ')
          IF (((LRECL.LT.1024).OR.(LRECL.GT.16384))) THEN
              LRECL=1024
              WRITE(6,1420)
1420          FORMAT(/' RECORD LENGTH DEFAULTED TO 1024!'//)
          END IF
          NPRIME=32000
          CALL HBSET('BSIZE',16384,IERR)
          CALL HROPEN(IOUT,'phasesp',NAMEOUT,'N',LRECL,ISTAT)
          CALL HBNT(NTUPLE_CWN,'BEAM',' ')
          CALL HBNAME(NTUPLE_CWN,'phasesp',e,'e,x,y,zlast,wt,iq:i*4:4,
     *                  latch1:u*4:16,latch2:u*4:16,w,u,v')
      ELSE IF((OUTMODE .EQ. 5)) THEN
          IF ((INMODE .NE. 0 .AND. INMODE .NE.2)) THEN
              WRITE(6,1430)
1430          FORMAT(/' BYTE SWAPPING CAN ONLY BE DONE ON MODE0 AND MODE
     *2 FILES.'/)
              STOP
          ELSE IF((NAMEOUT .EQ. NAMEIN)) THEN
              IOUT=IIN
          ELSE
              IF ((INMODE.NE.0)) THEN
                  MODE='MODE2'
                  PHSP_RECL=4*8000
              ELSE
                  MODE='MODE0'
                  PHSP_RECL=4*7000
              END IF
              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RECL=PHSP_RECL
     *        , FORM='UNFORMATTED',STATUS='UNKNOWN')
              CLOSE(IOUT)
              IF ((INMODE.NE.0)) THEN
                  PHSP_RECL=4*8
              ELSE
                  PHSP_RECL=4*7
              END IF
              OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT'
     *        , FORM='UNFORMATTED',STATUS='UNKNOWN')
              IF ((0.GT.2147483647)) THEN
                  write(*,*)' Warning while writing phase space file:'
                  write(*,*)' No. of particles > 2^31-1.'
                  write(*,*)' Total no. of particles (and photons) writt
     *en'
                  write(*,*)' to header may be nonsense.'
              END IF
              NUM_PHSP_TOT=0
              PHOT_PHSP_TOT=0
              EKMAX_PHSP_SHORT=0
              EKMINE_PHSP_SHORT=0
              NINC_PHSP_SHORT=0
              WRITE(IOUT,REC=1)MODE,NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_PHS
     *        P_SHORT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
              CLOSE(IOUT)
              IF ((INMODE.NE.0)) THEN
                  PHSP_RECL=4*8000
              ELSE
                  PHSP_RECL=4*7000
              END IF
              OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT'
     *        , FORM='UNFORMATTED',STATUS='UNKNOWN')
          END IF
          SWAPI=in_particle_tot
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          in_particle_tot=SWAPI
          SWAPI=NPHOTON
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          NPHOTON=SWAPI
          SWAPA=EMAXK
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          EMAXK=SWAPA
          SWAPA=EMINE
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          EMINE=SWAPA
          SWAPA=NINC
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          NINC=SWAPA
          WRITE(6,1440)in_particle_tot,NPHOTON,EMAXK,EMINE,NINC
1440      FORMAT(//' INFO IN 1ST RECORD OF INPUT FILE AFTER BYTE SWAPPIN
     *G:'/ ' *** THIS FILE CONTAINS',I13,' PARTICLES'/ '                
     * ',I13,' OF THEM ARE PHOTONS'/ '     MAXIMUM KINETIC ENERGY      '
     *,F13.3,' MeV'/ '     MINIMUM ENERGY FOR ELECTRONS',F13.3,' MeV'/ '
     *    ORIGINAL # INCIDENT PARTICLES',F13.1,/)
          WRITE(6,1450)
1450      FORMAT(/' You can swap bytes to make a file for the machine yo
     *u are on'/ ' because the file was created elsewhere'/ ' or you can
     * be converting a file to use on another machine'/ ' Are you swappi
     *ng bytes to be compatible with a machine'/ ' other than the one yo
     *u are running on now? [y/n] :',$)
          READ(5,1460)SWAPAWAY
1460      FORMAT(A1)
          IF ((SWAPAWAY.EQ.'y')) THEN
              SWAPI=in_particle_tot
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              in_particle_tot=SWAPI
              SWAPI=NPHOTON
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              NPHOTON=SWAPI
              SWAPA=NINC
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              NINC=SWAPA
          ELSE
              SWAPAWAY='n'
          END IF
      END IF
      WRITE(6,1470)
1470  FORMAT(/' HOW MANY PARTICLE RECORDS TO WRITE (default to 100000000
     *): ',$)
      READ(5,1480)IWRITEMAX
1480  FORMAT(I10)
      WRITE(6,1490)
1490  FORMAT(/' PARTICLE NUMBER TO START AT (default to 1): ',$)
      READ(5,1500)IWRITESTART
1500  FORMAT(I10)
      IF ((IWRITEMAX .LE. 0)) THEN
          IF ((INMODE.EQ.5)) THEN
              IWRITEMAX = 1000000
          ELSE
              IWRITEMAX = 100000000
          END IF
      END IF
      IF((IWRITESTART .LE.0))IWRITESTART=1
      IF((INMODE.EQ.5))IBWRITEMAX=IWRITEMAX+1
      WRITE(6,1510)(IWRITEMAX)
1510  FORMAT(/' WILL OUTPUT UP TO ', I10,'  PARTICLE RECORDS')
      WRITE(6,1520)IWRITESTART
1520  FORMAT(/' STARTING FROM PARTICLE ',I10)
      WRITE(6,1530)
1530  FORMAT(/' PARTICLE TYPE FOR OUTPUT: (-1=e-,0=photon,+1=e+,2=all,3=
     *e-&e+) ',$)
      READ(5,1540)CHARGE
1540  FORMAT(I2)
      in_particle_so_far=1
      out_part_so_far = 1
      out_phot_so_far=0
      NHSTRY=0
1551  CONTINUE
          IF ((INMODE .EQ. 1)) THEN
              READ (IIN,ERR=1560,END=1560) LATCH,ESHORT,X,Y,U,V,WT
          ELSE IF((INMODE .EQ. 3)) THEN
              READ (IIN,ERR=1560,END=1560) LATCH,ESHORT,X,Y,U,V,WT,ZLAST
          ELSE
              IF ((OUTMODE.EQ.5)) THEN
                  IF ((INMODE.NE.0)) THEN
                      READ(IIN,REC=in_particle_so_far+IWRITESTART,IOSTAT
     *                =IERR_PHSP) LATCH,ESHORT,X,Y,U,V,WT,ZLAST
                  ELSE
                      READ(IIN,REC=in_particle_so_far+IWRITESTART,IOSTAT
     *                =IERR_PHSP) LATCH,ESHORT,X,Y,U,V,WT
                  END IF
              ELSE
                  IF ((INMODE.NE.0)) THEN
                      READ(IIN,REC=in_particle_so_far+IWRITESTART,IOSTAT
     *                =IERR_PHSP) LATCH,ESHORT,X_PHSP_SHORT,Y_PHSP_SHORT
     *                , U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT,ZLAST_PH
     *                SP_SHORT
                  ELSE
                      READ(IIN,REC=in_particle_so_far+IWRITESTART,IOSTAT
     *                =IERR_PHSP) LATCH,ESHORT,X_PHSP_SHORT,Y_PHSP_SHORT
     *                , U_PHSP_SHORT,V_PHSP_SHORT,WT_PHSP_SHORT
                  END IF
                  IF ((in_particle_so_far+IWRITESTART.EQ.76695869)) THEN
                      write(*,*)' x,y,u,v,wt,eshort,latch', X_PHSP_SHORT
     *                ,Y_PHSP_SHORT,U_PHSP_SHORT,V_PHSP_SHORT, WT_PHSP_S
     *                HORT,ESHORT,LATCH
                  END IF
                  IF ((ESHORT.LT.0.0)) THEN
                      NHSTRY=NHSTRY+1
                      ESHORT=ABS(ESHORT)
                  END IF
                  ESHORT=ESHORT
                  WT=WT_PHSP_SHORT
                  X=X_PHSP_SHORT
                  Y=Y_PHSP_SHORT
                  U=U_PHSP_SHORT
                  V=V_PHSP_SHORT
                  IF((INMODE.NE.0))ZLAST=ZLAST_PHSP_SHORT
              END IF
              IF((IERR_PHSP.NE.0))GOTO 1560
              IF ((OUTMODE .EQ. 5)) THEN
                  SWAPI=LATCH
                  SWAPII=0
                  SWAPDUMMY1=SWAPI
                  SWAPDUMMY2=SWAPII
                  CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                  CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                  CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                  CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                  SWAPI=SWAPDUMMY2
                  LATCH=SWAPI
                  SWAPA=ESHORT
                  SWAPII=0
                  SWAPDUMMY1=SWAPI
                  SWAPDUMMY2=SWAPII
                  CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                  CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                  CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                  CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                  SWAPI=SWAPDUMMY2
                  ESHORT=SWAPA
                  IF ((ESHORT.LT.0.0)) THEN
                      NHSTRY=NHSTRY+1
                      ESHORT=ABS(ESHORT)
                  END IF
                  SWAPA=X
                  SWAPII=0
                  SWAPDUMMY1=SWAPI
                  SWAPDUMMY2=SWAPII
                  CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                  CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                  CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                  CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                  SWAPI=SWAPDUMMY2
                  X=SWAPA
                  SWAPA=Y
                  SWAPII=0
                  SWAPDUMMY1=SWAPI
                  SWAPDUMMY2=SWAPII
                  CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                  CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                  CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                  CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                  SWAPI=SWAPDUMMY2
                  Y=SWAPA
                  SWAPA=U
                  SWAPII=0
                  SWAPDUMMY1=SWAPI
                  SWAPDUMMY2=SWAPII
                  CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                  CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                  CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                  CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                  SWAPI=SWAPDUMMY2
                  U=SWAPA
                  SWAPA=V
                  SWAPII=0
                  SWAPDUMMY1=SWAPI
                  SWAPDUMMY2=SWAPII
                  CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                  CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                  CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                  CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                  SWAPI=SWAPDUMMY2
                  V=SWAPA
                  SWAPA=WT
                  SWAPII=0
                  SWAPDUMMY1=SWAPI
                  SWAPDUMMY2=SWAPII
                  CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                  CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                  CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                  CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                  SWAPI=SWAPDUMMY2
                  WT=SWAPA
                  IF ((INMODE .EQ. 2)) THEN
                      SWAPA=ZLAST
                      SWAPII=0
                      SWAPDUMMY1=SWAPI
                      SWAPDUMMY2=SWAPII
                      CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
                      CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
                      CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
                      CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
                      SWAPI=SWAPDUMMY2
                      ZLAST=SWAPA
                  END IF
              END IF
          END IF
          IF ((SWAPAWAY.EQ.'y')) THEN
              SWAPI=LATCH
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              LATCH=SWAPI
              SWAPA=WT
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              WT=SWAPA
              SWAPA=ESHORT
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              ESHORT=SWAPA
          END IF
          IF ((INMODE .NE. 5)) THEN
              IF ((BTEST(LATCH, 30) )) THEN
                  IQ=-1
              ELSE IF((BTEST(LATCH, 29) )) THEN
                  IQ=1
              ELSE
                  IQ=0
              END IF
          END IF
          tot_wt_read(IQ+2) = tot_wt_read(IQ+2)+ABS(WT)
          tot_num_read(IQ+2) = tot_num_read(IQ+2)+1.0
          AV_E(IQ+2) = AV_E(IQ+2)+ESHORT*ABS(WT)
          IF ((SWAPAWAY.EQ.'y')) THEN
              SWAPI=LATCH
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              LATCH=SWAPI
              SWAPA=WT
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              WT=SWAPA
              SWAPA=ESHORT
              SWAPII=0
              SWAPDUMMY1=SWAPI
              SWAPDUMMY2=SWAPII
              CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
              CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
              CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
              CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
              SWAPI=SWAPDUMMY2
              ESHORT=SWAPA
          END IF
          IF ((IQ .EQ. CHARGE .OR. CHARGE .EQ. 2 .OR. (CHARGE .EQ. 3 .AN
     *    D.ABS(IQ).EQ.1) )) THEN
              IF ((OUTMODE .EQ. 0 .OR. OUTMODE.EQ.2)) THEN
                  IF ((out_part_so_far+1.GT.2 .AND. WRITE_PHSP_COUNTER(1
     *            ).EQ.0 .AND. WRITE_PHSP_SOFAR(1).EQ.0)) THEN
                      IF (((out_part_so_far+1-1)-1000*((out_part_so_far+
     *                1-1)/1000).NE.0)) THEN
                          CLOSE(IOUT)
                          IF ((OUTMODE.NE.0)) THEN
                              PHSP_RECL=4*8
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                              DO 1571 I_PHSP=1,(out_part_so_far+1-1)-100
     *                        0*((out_part_so_far+1-1)/1000)
                                  READ(IOUT,REC=1000*INT(dble(out_part_s
     *                            o_far+1-1)/1000)+I_PHSP) STRING_TEMP_Z
     *                            LAST_OUT(1)(32*(I_PHSP-1)+1:32*I_PHSP)
1571                          CONTINUE
1572                          CONTINUE
                              CLOSE(IOUT)
                              PHSP_RECL=4*8000
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                          ELSE
                              PHSP_RECL=4*7
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                              DO 1581 I_PHSP=1,(out_part_so_far+1-1)-100
     *                        0*((out_part_so_far+1-1)/1000)
                                  READ(IOUT,REC=1000*INT(dble(out_part_s
     *                            o_far+1-1)/1000)+I_PHSP) STRING_TEMP_O
     *                            UT(1)(28*(I_PHSP-1)+1:28*I_PHSP)
1581                          CONTINUE
1582                          CONTINUE
                              CLOSE(IOUT)
                              PHSP_RECL=4*7000
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                          END IF
                      END IF
                      WRITE_PHSP_SOFAR(1)=1000*INT(dble(out_part_so_far+
     *                1-1)/1000)
                      IF ((WRITE_PHSP_SOFAR(1).GT.0)) THEN
                          PHSP_RESTART=1
                          WRITE_PHSP_COUNTER(1)=(out_part_so_far+1-1)-10
     *                    00*((out_part_so_far+1-1)/1000)
                      ELSE
                          WRITE_PHSP_COUNTER(1)=(out_part_so_far+1-2)-10
     *                    00*((out_part_so_far+1-2)/1000)
                      END IF
                  END IF
                  IF ((NHSTRY.NE.IHSTRY_PHSP(1))) THEN
                      ESHORT=-ESHORT
                      IHSTRY_PHSP(1)=NHSTRY
                  END IF
                  LATCH_TEMP_OUT=LATCH
                  REAL_TEMP_OUT(1)=ESHORT
                  REAL_TEMP_OUT(2)=X
                  REAL_TEMP_OUT(3)=Y
                  REAL_TEMP_OUT(4)=U
                  REAL_TEMP_OUT(5)=V
                  REAL_TEMP_OUT(6)=WT
                  IF((OUTMODE.NE.0))REAL_TEMP_OUT(7)=ZLAST
                  IF ((out_part_so_far+1.LE.1000 .AND. PHSP_RESTART.EQ.0
     *            )) THEN
                      IF ((OUTMODE.NE.0)) THEN
                          STRING_TEMP_ZLAST_OUT(1)(32*WRITE_PHSP_COUNTER
     *                    (1)+33: 32*WRITE_PHSP_COUNTER(1)+64)=REC_TEMP_
     *                    OUT(1:32)
                      ELSE
                          STRING_TEMP_OUT(1)(28*WRITE_PHSP_COUNTER(1)+29
     *                    : 28*WRITE_PHSP_COUNTER(1)+56)=REC_TEMP_OUT(1:
     *                    28)
                      END IF
                  ELSE
                      IF ((OUTMODE.NE.0)) THEN
                          STRING_TEMP_ZLAST_OUT(1)(32*WRITE_PHSP_COUNTER
     *                    (1)+1: 32*WRITE_PHSP_COUNTER(1)+32)=REC_TEMP_O
     *                    UT(1:32)
                      ELSE
                          STRING_TEMP_OUT(1)(28*WRITE_PHSP_COUNTER(1)+1:
     *                     28*WRITE_PHSP_COUNTER(1)+28)=REC_TEMP_OUT(1:2
     *                    8)
                      END IF
                  END IF
                  WRITE_PHSP_COUNTER(1)=WRITE_PHSP_COUNTER(1)+1
                  IF ((out_part_so_far+1.EQ.1000.OR.WRITE_PHSP_COUNTER(1
     *            ).EQ.1000)) THEN
                      IF ((OUTMODE.NE.0 .AND. PHSP_RECL.EQ.4*8)) THEN
                          CLOSE(IOUT)
                          PHSP_RECL_OLD=PHSP_RECL
                          PHSP_RECL=4*8000
                          OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RE
     *                    CL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNO
     *WN')
                      ELSE IF((OUTMODE.EQ.0 .AND. PHSP_RECL.EQ.4*7)) THE
     *                N
                          PHSP_RECL_OLD=PHSP_RECL
                          CLOSE(IOUT)
                          PHSP_RECL=4*7000
                          OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RE
     *                    CL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNO
     *WN')
                      END IF
                      IF ((OUTMODE.NE.0)) THEN
                          WRITE(IOUT,REC=INT(dble(out_part_so_far+1)/100
     *                    0)) STRING_TEMP_ZLAST_OUT(1)
                      ELSE
                          WRITE(IOUT,REC=INT(dble(out_part_so_far+1)/100
     *                    0)) STRING_TEMP_OUT(1)
                      END IF
                      IF ((PHSP_RECL_OLD.NE.0)) THEN
                          PHSP_RECL=PHSP_RECL_OLD
                          CLOSE(IOUT)
                          OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RE
     *                    CL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNO
     *WN')
                      END IF
                      WRITE_PHSP_COUNTER(1)=0
                      WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+1000
                  END IF
              ELSE IF((OUTMODE .EQ. 1)) THEN
                  WRITE (IOUT) LATCH,ESHORT,X,Y,U,V,WT
              ELSE IF((OUTMODE .EQ. 3)) THEN
                  WRITE (IOUT) LATCH,ESHORT,X,Y,U,V,WT,ZLAST
              ELSE IF((OUTMODE .EQ. 5)) THEN
                  IF ((out_part_so_far+1.GT.2 .AND. WRITE_PHSP_COUNTER(1
     *            ).EQ.0 .AND. WRITE_PHSP_SOFAR(1).EQ.0)) THEN
                      IF (((out_part_so_far+1-1)-1000*((out_part_so_far+
     *                1-1)/1000).NE.0)) THEN
                          CLOSE(IOUT)
                          IF ((INMODE.NE.0)) THEN
                              PHSP_RECL=4*8
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                              DO 1591 I_PHSP=1,(out_part_so_far+1-1)-100
     *                        0*((out_part_so_far+1-1)/1000)
                                  READ(IOUT,REC=1000*INT(dble(out_part_s
     *                            o_far+1-1)/1000)+I_PHSP) STRING_TEMP_Z
     *                            LAST_OUT(1)(32*(I_PHSP-1)+1:32*I_PHSP)
1591                          CONTINUE
1592                          CONTINUE
                              CLOSE(IOUT)
                              PHSP_RECL=4*8000
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                          ELSE
                              PHSP_RECL=4*7
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                              DO 1601 I_PHSP=1,(out_part_so_far+1-1)-100
     *                        0*((out_part_so_far+1-1)/1000)
                                  READ(IOUT,REC=1000*INT(dble(out_part_s
     *                            o_far+1-1)/1000)+I_PHSP) STRING_TEMP_O
     *                            UT(1)(28*(I_PHSP-1)+1:28*I_PHSP)
1601                          CONTINUE
1602                          CONTINUE
                              CLOSE(IOUT)
                              PHSP_RECL=4*7000
                              OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT
     *',RECL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNOWN')
                          END IF
                      END IF
                      WRITE_PHSP_SOFAR(1)=1000*INT(dble(out_part_so_far+
     *                1-1)/1000)
                      IF ((WRITE_PHSP_SOFAR(1).GT.0)) THEN
                          PHSP_RESTART=1
                          WRITE_PHSP_COUNTER(1)=(out_part_so_far+1-1)-10
     *                    00*((out_part_so_far+1-1)/1000)
                      ELSE
                          WRITE_PHSP_COUNTER(1)=(out_part_so_far+1-2)-10
     *                    00*((out_part_so_far+1-2)/1000)
                      END IF
                  END IF
                  IF ((NHSTRY.NE.IHSTRY_PHSP(1))) THEN
                      ESHORT=-ESHORT
                      IHSTRY_PHSP(1)=NHSTRY
                  END IF
                  LATCH_TEMP_OUT=LATCH
                  REAL_TEMP_OUT(1)=ESHORT
                  REAL_TEMP_OUT(2)=X
                  REAL_TEMP_OUT(3)=Y
                  REAL_TEMP_OUT(4)=U
                  REAL_TEMP_OUT(5)=V
                  REAL_TEMP_OUT(6)=WT
                  IF((INMODE.NE.0))REAL_TEMP_OUT(7)=ZLAST
                  IF ((out_part_so_far+1.LE.1000 .AND. PHSP_RESTART.EQ.0
     *            )) THEN
                      IF ((INMODE.NE.0)) THEN
                          STRING_TEMP_ZLAST_OUT(1)(32*WRITE_PHSP_COUNTER
     *                    (1)+33: 32*WRITE_PHSP_COUNTER(1)+64)=REC_TEMP_
     *                    OUT(1:32)
                      ELSE
                          STRING_TEMP_OUT(1)(28*WRITE_PHSP_COUNTER(1)+29
     *                    : 28*WRITE_PHSP_COUNTER(1)+56)=REC_TEMP_OUT(1:
     *                    28)
                      END IF
                  ELSE
                      IF ((INMODE.NE.0)) THEN
                          STRING_TEMP_ZLAST_OUT(1)(32*WRITE_PHSP_COUNTER
     *                    (1)+1: 32*WRITE_PHSP_COUNTER(1)+32)=REC_TEMP_O
     *                    UT(1:32)
                      ELSE
                          STRING_TEMP_OUT(1)(28*WRITE_PHSP_COUNTER(1)+1:
     *                     28*WRITE_PHSP_COUNTER(1)+28)=REC_TEMP_OUT(1:2
     *                    8)
                      END IF
                  END IF
                  WRITE_PHSP_COUNTER(1)=WRITE_PHSP_COUNTER(1)+1
                  IF ((out_part_so_far+1.EQ.1000.OR.WRITE_PHSP_COUNTER(1
     *            ).EQ.1000)) THEN
                      IF ((INMODE.NE.0 .AND. PHSP_RECL.EQ.4*8)) THEN
                          CLOSE(IOUT)
                          PHSP_RECL_OLD=PHSP_RECL
                          PHSP_RECL=4*8000
                          OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RE
     *                    CL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNO
     *WN')
                      ELSE IF((INMODE.EQ.0 .AND. PHSP_RECL.EQ.4*7)) THEN
                          PHSP_RECL_OLD=PHSP_RECL
                          CLOSE(IOUT)
                          PHSP_RECL=4*7000
                          OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RE
     *                    CL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNO
     *WN')
                      END IF
                      IF ((INMODE.NE.0)) THEN
                          WRITE(IOUT,REC=INT(dble(out_part_so_far+1)/100
     *                    0)) STRING_TEMP_ZLAST_OUT(1)
                      ELSE
                          WRITE(IOUT,REC=INT(dble(out_part_so_far+1)/100
     *                    0)) STRING_TEMP_OUT(1)
                      END IF
                      IF ((PHSP_RECL_OLD.NE.0)) THEN
                          PHSP_RECL=PHSP_RECL_OLD
                          CLOSE(IOUT)
                          OPEN(UNIT=IOUT,FILE=NAMEOUT,ACCESS='DIRECT',RE
     *                    CL=PHSP_RECL, FORM='UNFORMATTED',STATUS='UNKNO
     *WN')
                      END IF
                      WRITE_PHSP_COUNTER(1)=0
                      WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+1000
                  END IF
              ELSE
                  IF (( abs(IQ) .EQ. 1)) THEN
                      ESHORT=ESHORT-0.5109989461
                  END IF
                  LATCH = IBCLR(LATCH,29)
                  LATCH = IBCLR(LATCH,30)
                  LATCH = IBCLR(LATCH,31)
                  W = MIN( 1., U**2 + V**2)
                  W = SQRT(1 - W)
                  W = SIGN(W,WT)
                  WT = ABS(WT)
                  E = ESHORT
                  LATCH1 = iand(LATCH,65535)
                  LATCH2 = LATCH/65536
                  CALL HFNT(NTUPLE_CWN)
              END IF
              IF((IQ.EQ.0))out_phot_so_far=out_phot_so_far+1
              IF ((out_part_so_far .GE. IWRITEMAX)) THEN
                  GOTO 1610
              END IF
              out_part_so_far = out_part_so_far + 1
          END IF
          IF ((in_particle_so_far .EQ. 1)) THEN
              WRITE(6,1620)
1620          FORMAT(/' HAVE STARTED PROCESSING. ')
          ELSE IF((mod(in_particle_so_far,100000) .EQ. 0)) THEN
              WRITE(6,1630)in_particle_so_far,out_part_so_far-1
1630          FORMAT(/' HAVE READ ',I10,' PARTICLES AND WRITTEN', I10,'P
     *ARTICLES. ')
          END IF
          IF ((INMODE.EQ.0.OR.INMODE.EQ.2)) THEN
              IF ((in_particle_so_far .GE. in_particle_tot)) THEN
                  out_part_so_far = out_part_so_far - 1
                  GOTO 1610
              END IF
          END IF
          in_particle_so_far=in_particle_so_far+1
      GO TO 1551
1552  CONTINUE
1560  WRITE(6,1640)
1640  FORMAT(/'  Read to end of input file'//)
      in_particle_so_far = in_particle_so_far - 1
      out_part_so_far = out_part_so_far -1
1610  CONTINUE
      IF ((OUTMODE.EQ.5)) THEN
          TEMP_PHSP_COUNTER=0
          NUM_PHSP_TOFLUSH=WRITE_PHSP_COUNTER(1)
          IF ((NUM_PHSP_TOFLUSH.GT.0)) THEN
              IF ((WRITE_PHSP_SOFAR(1).EQ.0)) THEN
                  WRITE_PHSP_COUNTER(1)=WRITE_PHSP_COUNTER(1)+1
                  NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH+1
              END IF
1651          CONTINUE
                  TEMP_PHSP_COUNTER=NUM_PHSP_TOFLUSH+1
1661              CONTINUE
                      TEMP_PHSP_COUNTER=TEMP_PHSP_COUNTER-1
                      REM_PHSP=MOD(WRITE_PHSP_SOFAR(1),TEMP_PHSP_COUNTER
     *                )
                      IF(((REM_PHSP.EQ.0)))GO TO1662
                  GO TO 1661
1662              CONTINUE
                  CLOSE(IOUT)
                  IF ((INMODE.NE.0)) THEN
                      PHSP_RECL=4*8*TEMP_PHSP_COUNTER
                      OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL, ACCESS
     *                ='DIRECT',FORM='UNFORMATTED',STATUS='UNKNOWN')
                      WRITE(IOUT,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNT
     *                ER+1) STRING_TEMP_ZLAST_OUT(1) (32*(WRITE_PHSP_COU
     *                NTER(1)-NUM_PHSP_TOFLUSH)+1: 32*(WRITE_PHSP_COUNTE
     *                R(1)-NUM_PHSP_TOFLUSH+TEMP_PHSP_COUNTER))
                  ELSE
                      PHSP_RECL=4*7*TEMP_PHSP_COUNTER
                      OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL, ACCESS
     *                ='DIRECT',FORM='UNFORMATTED',STATUS='UNKNOWN')
                      WRITE(IOUT,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNT
     *                ER+1) STRING_TEMP_OUT(1) (28*(WRITE_PHSP_COUNTER(1
     *                )-NUM_PHSP_TOFLUSH)+1: 28*(WRITE_PHSP_COUNTER(1)-N
     *                UM_PHSP_TOFLUSH+TEMP_PHSP_COUNTER))
                  END IF
                  WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+TEMP_PHSP_COUN
     *            TER
                  NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH-TEMP_PHSP_COUNTER
                  IF(((NUM_PHSP_TOFLUSH.EQ.0)))GO TO1652
              GO TO 1651
1652          CONTINUE
              WRITE_PHSP_COUNTER(1)=0
              WRITE_PHSP_SOFAR(1)=0
          END IF
      ELSE
          TEMP_PHSP_COUNTER=0
          NUM_PHSP_TOFLUSH=WRITE_PHSP_COUNTER(1)
          IF ((NUM_PHSP_TOFLUSH.GT.0)) THEN
              IF ((WRITE_PHSP_SOFAR(1).EQ.0)) THEN
                  WRITE_PHSP_COUNTER(1)=WRITE_PHSP_COUNTER(1)+1
                  NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH+1
              END IF
1671          CONTINUE
                  TEMP_PHSP_COUNTER=NUM_PHSP_TOFLUSH+1
1681              CONTINUE
                      TEMP_PHSP_COUNTER=TEMP_PHSP_COUNTER-1
                      REM_PHSP=MOD(WRITE_PHSP_SOFAR(1),TEMP_PHSP_COUNTER
     *                )
                      IF(((REM_PHSP.EQ.0)))GO TO1682
                  GO TO 1681
1682              CONTINUE
                  CLOSE(IOUT)
                  IF ((OUTMODE.NE.0)) THEN
                      PHSP_RECL=4*8*TEMP_PHSP_COUNTER
                      OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL, ACCESS
     *                ='DIRECT',FORM='UNFORMATTED',STATUS='UNKNOWN')
                      WRITE(IOUT,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNT
     *                ER+1) STRING_TEMP_ZLAST_OUT(1) (32*(WRITE_PHSP_COU
     *                NTER(1)-NUM_PHSP_TOFLUSH)+1: 32*(WRITE_PHSP_COUNTE
     *                R(1)-NUM_PHSP_TOFLUSH+TEMP_PHSP_COUNTER))
                  ELSE
                      PHSP_RECL=4*7*TEMP_PHSP_COUNTER
                      OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL, ACCESS
     *                ='DIRECT',FORM='UNFORMATTED',STATUS='UNKNOWN')
                      WRITE(IOUT,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNT
     *                ER+1) STRING_TEMP_OUT(1) (28*(WRITE_PHSP_COUNTER(1
     *                )-NUM_PHSP_TOFLUSH)+1: 28*(WRITE_PHSP_COUNTER(1)-N
     *                UM_PHSP_TOFLUSH+TEMP_PHSP_COUNTER))
                  END IF
                  WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+TEMP_PHSP_COUN
     *            TER
                  NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH-TEMP_PHSP_COUNTER
                  IF(((NUM_PHSP_TOFLUSH.EQ.0)))GO TO1672
              GO TO 1671
1672          CONTINUE
              WRITE_PHSP_COUNTER(1)=0
              WRITE_PHSP_SOFAR(1)=0
          END IF
      END IF
      WRITE(6,1690)in_particle_so_far,out_part_so_far
1690  FORMAT(/' HAVE FINISHED READING',I10,' AND WRITING ',I  10,' PARTI
     *CLES.'/)
      NPHOTON=out_phot_so_far
      NINC=NINC*in_particle_so_far/in_particle_tot
      IF ((SWAPAWAY.EQ.'y')) THEN
          SWAPI=out_part_so_far
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          out_part_so_far=SWAPI
          SWAPI=NPHOTON
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          NPHOTON=SWAPI
          SWAPA=NINC
          SWAPII=0
          SWAPDUMMY1=SWAPI
          SWAPDUMMY2=SWAPII
          CALL MVBITS(SWAPDUMMY1, 24, 8, SWAPDUMMY2, 0)
          CALL MVBITS(SWAPDUMMY1, 16, 8, SWAPDUMMY2, 8)
          CALL MVBITS(SWAPDUMMY1, 8, 8, SWAPDUMMY2, 16)
          CALL MVBITS(SWAPDUMMY1, 0,8,SWAPDUMMY2, 24)
          SWAPI=SWAPDUMMY2
          NINC=SWAPA
      END IF
      IF ((OUTMODE.EQ.0)) THEN
          CLOSE(IOUT)
          IF ((OUTMODE.NE.0)) THEN
              PHSP_RECL=4*8
          ELSE
              PHSP_RECL=4*7
          END IF
          OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT', FO
     *    RM='UNFORMATTED',STATUS='UNKNOWN')
          IF ((out_part_so_far.GT.2147483647)) THEN
              write(*,*)' Warning while writing phase space file:'
              write(*,*)' No. of particles > 2^31-1.'
              write(*,*)' Total no. of particles (and photons) written'
              write(*,*)' to header may be nonsense.'
          END IF
          NUM_PHSP_TOT=out_part_so_far
          PHOT_PHSP_TOT=NPHOTON
          EKMAX_PHSP_SHORT=EMAXK
          EKMINE_PHSP_SHORT=EMINE
          NINC_PHSP_SHORT=NINC
          WRITE(IOUT,REC=1)'MODE0',NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_PHSP
     *    _SHORT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          CLOSE(IOUT)
          IF ((OUTMODE.NE.0)) THEN
              PHSP_RECL=4*8000
          ELSE
              PHSP_RECL=4*7000
          END IF
          OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT', FO
     *    RM='UNFORMATTED',STATUS='UNKNOWN')
      ELSE IF((OUTMODE.EQ.2)) THEN
          CLOSE(IOUT)
          IF ((OUTMODE.NE.0)) THEN
              PHSP_RECL=4*8
          ELSE
              PHSP_RECL=4*7
          END IF
          OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT', FO
     *    RM='UNFORMATTED',STATUS='UNKNOWN')
          IF ((out_part_so_far.GT.2147483647)) THEN
              write(*,*)' Warning while writing phase space file:'
              write(*,*)' No. of particles > 2^31-1.'
              write(*,*)' Total no. of particles (and photons) written'
              write(*,*)' to header may be nonsense.'
          END IF
          NUM_PHSP_TOT=out_part_so_far
          PHOT_PHSP_TOT=NPHOTON
          EKMAX_PHSP_SHORT=EMAXK
          EKMINE_PHSP_SHORT=EMINE
          NINC_PHSP_SHORT=NINC
          WRITE(IOUT,REC=1)'MODE2',NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_PHSP
     *    _SHORT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          CLOSE(IOUT)
          IF ((OUTMODE.NE.0)) THEN
              PHSP_RECL=4*8000
          ELSE
              PHSP_RECL=4*7000
          END IF
          OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT', FO
     *    RM='UNFORMATTED',STATUS='UNKNOWN')
      ELSE IF((OUTMODE .EQ. 5)) THEN
          IF ((INMODE.EQ.0)) THEN
              CLOSE(IOUT)
              IF ((INMODE.NE.0)) THEN
                  PHSP_RECL=4*8
              ELSE
                  PHSP_RECL=4*7
              END IF
              OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT'
     *        , FORM='UNFORMATTED',STATUS='UNKNOWN')
              IF ((out_part_so_far.GT.2147483647)) THEN
                  write(*,*)' Warning while writing phase space file:'
                  write(*,*)' No. of particles > 2^31-1.'
                  write(*,*)' Total no. of particles (and photons) writt
     *en'
                  write(*,*)' to header may be nonsense.'
              END IF
              NUM_PHSP_TOT=out_part_so_far
              PHOT_PHSP_TOT=NPHOTON
              EKMAX_PHSP_SHORT=EMAXK
              EKMINE_PHSP_SHORT=EMINE
              NINC_PHSP_SHORT=NINC
              WRITE(IOUT,REC=1)'MODE0',NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_
     *        PHSP_SHORT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
              CLOSE(IOUT)
              IF ((INMODE.NE.0)) THEN
                  PHSP_RECL=4*8000
              ELSE
                  PHSP_RECL=4*7000
              END IF
              OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT'
     *        , FORM='UNFORMATTED',STATUS='UNKNOWN')
          ELSE IF((INMODE.EQ.2)) THEN
              CLOSE(IOUT)
              IF ((INMODE.NE.0)) THEN
                  PHSP_RECL=4*8
              ELSE
                  PHSP_RECL=4*7
              END IF
              OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT'
     *        , FORM='UNFORMATTED',STATUS='UNKNOWN')
              IF ((out_part_so_far.GT.2147483647)) THEN
                  write(*,*)' Warning while writing phase space file:'
                  write(*,*)' No. of particles > 2^31-1.'
                  write(*,*)' Total no. of particles (and photons) writt
     *en'
                  write(*,*)' to header may be nonsense.'
              END IF
              NUM_PHSP_TOT=out_part_so_far
              PHOT_PHSP_TOT=NPHOTON
              EKMAX_PHSP_SHORT=EMAXK
              EKMINE_PHSP_SHORT=EMINE
              NINC_PHSP_SHORT=NINC
              WRITE(IOUT,REC=1)'MODE2',NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_
     *        PHSP_SHORT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
              CLOSE(IOUT)
              IF ((INMODE.NE.0)) THEN
                  PHSP_RECL=4*8000
              ELSE
                  PHSP_RECL=4*7000
              END IF
              OPEN(UNIT=IOUT,FILE=NAMEOUT,RECL=PHSP_RECL,ACCESS='DIRECT'
     *        , FORM='UNFORMATTED',STATUS='UNKNOWN')
          END IF
      END IF
      DO 1701 I=1,3
          IF ((tot_wt_read(I).EQ.0.0)) THEN
              AV_E(I)=0.0
          ELSE
              AV_E(I)=AV_E(I)/tot_wt_read(I)
          END IF
1701  CONTINUE
1702  CONTINUE
      WRITE(6,1710)(tot_num_read(I),tot_wt_read(I), AV_E(I),I=1,3),NHSTR
     *Y
1710  FORMAT(// '              *** INFORMATION ABOUT THE PARTICLES READI
     *N ***'// '         ELECTRONS                 PHOTONS     POSITRONS
     *'/ 'number   weight  ave en     number weight  ave en        numbe
     *r weight  ave en'/ '                  (MeV)           (MeV)       
     *               (MeV)'/ 3(0PF9.0,F9.0,0PF8.3)// I10,' primary histo
     *ries represented in particles read in.'// ' HAPPY COMPUTING, BYE!'
     *//)
      CLOSE(IIN)
      IF ((OUTMODE .EQ. 4)) THEN
          CALL HROUT(NTUPLE_CWN,ICYCLE,' ')
          CALL HREND('phasesp')
      ELSE
          CLOSE(IOUT)
      END IF
      STOP
1720  WRITE(6,1730)NAMEOUT
1730  FORMAT(' Tried to open following file as new and it was there:'/1x
     *,A70/ '  Either remove it or re-run with different output file nam
     *e'/ '  Output file name(with extension) is 3rd parameter (for byte
     * swapping'/ '  name can be same as input file name (but overwrites
     * the file!)'//)
      STOP
      END
      integer*4 function lnblnk1(string)
      character*(*) string
      DO 1741 i=len(string),1,-1
          j = ichar(string(i:i))
          IF (( j .EQ. 0 )) THEN
              lnblnk1 = i-1
              return
          END IF
          IF (( j .NE. 9 .AND. j .NE. 10 .AND. j .NE. 11 .AND. j .NE. 12
     *     .AND. j .NE. 13 .AND. j .NE. 32 )) THEN
              lnblnk1 = i
              return
          END IF
1741  CONTINUE
1742  CONTINUE
      lnblnk1 = 0
      return
      end
