      CHARACTER*80 infile,outfile,arg,istart,iscore,iend,inum,tempinfile
     *,resp, i_iaea
      INTEGER i,j,k,iipar,iargc,numarg,lnblnk1,iistart,iiscore,iiend,ndi
     *gits, sumerr,ifirst,i_iaea_in
      logical ex,append
      numarg=iargc()
      IF ((numarg.LT.3)) THEN
        WRITE(6,1010)
1010    FORMAT(/' Usage: '// ' addphsp infile(no ext.) outfile(no ext.)i
     *par [istart] [iscore] [i_iaea]'// ' infile  = BEAM input file'/ ' 
     *outfile = output file for summed phsp data.  Extension .egsphsp(is
     *core)'/ '           gets added automatically.'/ ' ipar    = no. of
     * parallel runs to be added'/ ' istart  = starting index of paralle
     *l runs to be added (defaults to 1)'/ ' iscore  = scoring plane num
     *ber (defaults to 1)--input 0 for no scoring plane'/' i_iaea  = set
     * to 1 if files are in IAEA format (defaults to 0)'// ' Note: phsp 
     *files to be added have naming scheme:'/ ' infile_w(istart).egsphsp
     *(iscore),...,', 'infile_w(istart+ipar-1).egsphsp(iscore)'/ ' or:'/
     * ' infile_w(istart).iscore.IAEAphsp,...,', 'infile_w(istart+ipar-1
     *).iscore.IAEAphsp'/)
        stop
      END IF
      call getarg(1,arg)
      infile=arg(:lnblnk1(arg))
      call getarg(2,arg)
      outfile=arg(:lnblnk1(arg))
      call getarg(3,arg)
      iipar=0
      DO 1021 i=1,lnblnk1(arg)
        iipar=iipar+(ichar(arg(i:i))-48)*10**(lnblnk1(arg)-i)
1021  CONTINUE
1022  CONTINUE
      IF ((numarg.GT.3)) THEN
        call getarg(4,arg)
        istart=arg(:lnblnk1(arg))
        IF ((numarg.GT.4)) THEN
          call getarg(5,arg)
          iscore=arg(:lnblnk1(arg))
          IF ((numarg.GT.5)) THEN
            call getarg(6,arg)
            i_iaea=arg(:lnblnk1(arg))
          ELSE
            i_iaea='0'
          END IF
        ELSE
          iscore='1'
          i_iaea='0'
        END IF
      ELSE
        istart='1'
        iscore='1'
        i_iaea='0'
      END IF
      iistart=0
      iiscore=0
      i_iaea_in=0
      DO 1031 i=1,lnblnk1(istart)
        iistart=iistart+(ichar(istart(i:i))-48)*10**(lnblnk1(istart)-i)
1031  CONTINUE
1032  CONTINUE
      DO 1041 i=1,lnblnk1(iscore)
        iiscore=iiscore+(ichar(iscore(i:i))-48)*10**(lnblnk1(iscore)-i)
1041  CONTINUE
1042  CONTINUE
      IF((i_iaea.EQ.'1'))i_iaea_in=1
      iiend=iistart+iipar-1
      DO 1051 i=1,80
        IF ((mod(iiend,10**i).EQ.iiend)) THEN
          ndigits=i
          GO TO1052
        END IF
1051  CONTINUE
1052  CONTINUE
      DO 1061 i=1,ndigits
        IF ((i.EQ.1)) THEN
          iend=char(iiend/(10**(ndigits-i))+48)
        ELSE
          iend=iend(:lnblnk1(iend))//char(iiend/(10**(ndigits-i))+48)
        END IF
        iiend=mod(iiend,10**(ndigits-i))
1061  CONTINUE
1062  CONTINUE
      IF ((i_iaea_in.EQ.1)) THEN
        IF((iscore(:lnblnk1(iscore)).NE.'0'))outfile=outfile(:lnblnk1(ou
     *  tfile))//'.'//iscore(:lnblnk1(iscore))
        IF ((iscore(:lnblnk1(iscore)).NE.'0')) THEN
          WRITE(6,1070)infile(:lnblnk1(infile))//'_w'//istart(:lnblnk1(i
     *    start))//'.'//iscore(:lnblnk1(iscore))// '.IAEAphsp', infile(:
     *    lnblnk1(infile))//'_w'//iend(:lnblnk1(iend))// '.'//iscore(:ln
     *    blnk1(iscore))//'.IAEAphsp', outfile(:lnblnk1(outfile))//'.IAE
     *Aphsp'
1070      FORMAT(/' Will sum from phsp file ',A/' to ',A/ ' And output r
     *esult to ',A/)
        ELSE
          WRITE(6,1080)infile(:lnblnk1(infile))//'_w'//istart(:lnblnk1(i
     *    start))// '.IAEAphsp', infile(:lnblnk1(infile))//'_w'//iend(:l
     *    nblnk1(iend))// '.'//iscore(:lnblnk1(iscore))//'.IAEAphsp', ou
     *    tfile(:lnblnk1(outfile))//'.IAEAphsp'
1080      FORMAT(/' Will sum from phsp file ',A/' to ',A/ ' And output r
     *esult to ',A/)
        END IF
      ELSE
        outfile=outfile(:lnblnk1(outfile))//'.egsphsp'//iscore(:lnblnk1(
     *  iscore))
        WRITE(6,1090)infile(:lnblnk1(infile))//'_w'//istart(:lnblnk1(ist
     *  art))//'.egsphsp'//iscore(:lnblnk1(iscore)), infile(:lnblnk1(inf
     *  ile))//'_w'//iend(:lnblnk1(iend))//'.egsphsp'//iscore(:lnblnk1(i
     *  score)), outfile(:lnblnk1(outfile))
1090    FORMAT(/' Will sum from phsp file ',A/' to ',A/ ' And output res
     *ult to ',A/)
      END IF
      append=.false.
      IF ((i_iaea_in.EQ.1)) THEN
        inquire(file=outfile(:lnblnk1(outfile))//'.IAEAphsp',exist=ex)
      ELSE
        inquire(file=outfile,exist=ex)
      END IF
      IF ((ex)) THEN
        WRITE(6,1100)outfile(:lnblnk1(outfile))
1100    FORMAT(/' Output file ',A,' exists.'/ ' Add to it [add], overwri
     *te it [ow], or quit [q--default]:',$)
        READ(5,'(A)')resp
        IF ((resp(:lnblnk1(resp)).EQ.'add')) THEN
          WRITE(6,1110)
1110      FORMAT(/' Will append phsp data to existing phsp file.'/)
          append=.true.
        ELSE IF((resp(:lnblnk1(resp)).EQ.'ow')) THEN
          WRITE(6,1120)
1120      FORMAT(/' Will overwrite existing file.'/)
        ELSE
          WRITE(6,1130)
1130      FORMAT(/' Quitting.'/)
          stop
        END IF
      END IF
      DO 1141 i=iistart,iistart+iipar-1
        k=i
        DO 1151 j=1,80
          IF ((mod(k,10**j).EQ.k)) THEN
            ndigits=j
            GO TO1152
          END IF
1151    CONTINUE
1152    CONTINUE
        DO 1161 j=1,ndigits
          IF ((j.EQ.1)) THEN
            inum=char(k/(10**(ndigits-j))+48)
          ELSE
            inum=inum(:lnblnk1(inum))//char(k/(10**(ndigits-j))+48)
          END IF
          k=mod(k,10**(ndigits-j))
1161    CONTINUE
1162    CONTINUE
        IF ((i_iaea_in.EQ.1)) THEN
          tempinfile=infile(:lnblnk1(infile))//'_w'//inum(:lnblnk1(inum)
     *    )
          IF((iscore(:lnblnk1(iscore)).NE.'0'))tempinfile=tempinfile(:ln
     *    blnk1(tempinfile))// '.'//iscore(:lnblnk1(iscore))
        ELSE
          tempinfile=infile(:lnblnk1(infile))//'_w'//inum(:lnblnk1(inum)
     *    )//'.egsphsp'// iscore(:lnblnk1(iscore))
        END IF
        IF (((i.EQ.iistart .OR. sumerr.EQ.3) .AND. .NOT.append)) THEN
          call add_files(tempinfile(:lnblnk1(tempinfile)),outfile(:lnbln
     *    k1(outfile)),1,sumerr,i_iaea_in)
        ELSE IF((sumerr.EQ.2)) THEN
          GO TO1142
        ELSE
          call add_files(tempinfile(:lnblnk1(tempinfile)),outfile(:lnbln
     *    k1(outfile)),0,sumerr,i_iaea_in)
        END IF
1141  CONTINUE
1142  CONTINUE
      WRITE(6,1170)
1170  FORMAT(/' Done.'/)
      stop
      end
      SUBROUTINE ADD_FILES(infile,outfile,ifirst,sumerr,i_iaea_in)
      IMPLICIT NONE
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
      character*80 infile,outfile
      integer ifirst,sumerr,IZLAST1,IMUPHSP1,IMUPHSP2, LATCHI,lnblnk1,i_
     *iaea_in,i_log, i_unit_in,i_unit_out,IZLAST2,NPASSI,IQ,IZSCORE1,IZS
     *CORE2
      integer*8 PARANOT,PARANOT1,PARANOP1,PARANOT2,PARANOP2, IPARANOT1,I
     *PARANOT2,LPARANINC1,LPARANINC2,LNINC,NHSTRY
      real PARAEMAX1,PARAEMNE1,PARANINC1,PARAEMAX2,PARAEMNE2,PARANINC2,N
     *INC, ZLAST,EREAD,WEIGHT,XIN,YIN,ZIN,UIN,VIN,WIN,Z_SCORE,MU_IDX
      character*5 MODE_RW1,MODE_RW2
      integer itmp
      EQUIVALENCE(REC_TEMP_OUT(1:4),LATCH_TEMP_OUT)
      EQUIVALENCE(REC_TEMP_OUT(5:8),REAL_TEMP_OUT(1))
      EQUIVALENCE(REC_TEMP_OUT(9:12),REAL_TEMP_OUT(2))
      EQUIVALENCE(REC_TEMP_OUT(13:16),REAL_TEMP_OUT(3))
      EQUIVALENCE(REC_TEMP_OUT(17:20),REAL_TEMP_OUT(4))
      EQUIVALENCE(REC_TEMP_OUT(21:24),REAL_TEMP_OUT(5))
      EQUIVALENCE(REC_TEMP_OUT(25:28),REAL_TEMP_OUT(6))
      EQUIVALENCE(REC_TEMP_OUT(29:32),REAL_TEMP_OUT(7))
      DO 1181 I_PHSP=1,1
        WRITE_PHSP_COUNTER(I_PHSP)=0
        WRITE_PHSP_SOFAR(I_PHSP)=0
        IHSTRY_PHSP(I_PHSP)=0
1181  CONTINUE
1182  CONTINUE
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
      sumerr=0
      i_log=6
      IF ((i_iaea_in.EQ.1)) THEN
        WRITE(6,1190)INFILE(:lnblnk1(INFILE))//'.IAEAphsp',OUTFILE(:lnbl
     *  nk1(OUTFILE))//'.IAEAphsp'
1190    FORMAT(/' Adding ',A,' to ',A,': '/)
      ELSE
        WRITE(6,1200)INFILE(:lnblnk1(INFILE)),OUTFILE(:lnblnk1(OUTFILE))
1200    FORMAT(/' Adding ',A,' to ',A,': '/)
      END IF
      PARANOT1=0
      PARANOP1=0
      PARAEMAX1=0.
      PARAEMNE1=0.
      PARANINC1=0.
      MODE_RW1=' '
      IF ((i_iaea_in.EQ.1)) THEN
        WRITE(6,1210)INFILE(:lnblnk1(INFILE))//'.IAEAphsp'
1210    FORMAT(/' Header information for ',A,':'/)
        i_unit_in=2
        IERR_PHSP=0
        call iaea_new_source(i_unit_in,INFILE,1,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error opening IAEA phase space source.'
        END IF
        IF ((iaea_result.LT.0)) THEN
          IF ((ifirst.EQ.1)) THEN
            sumerr=3
          ELSE
            sumerr=1
          END IF
          GOTO 1220
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
        DO 1231 I_PHSP=1,iaea_n_extra_ints
          IF ((iaea_extra_int_types(I_PHSP).EQ.2)) THEN
            iaea_i_latch=I_PHSP
            GO TO1232
          END IF
1231    CONTINUE
1232    CONTINUE
        IF ((iaea_i_latch.EQ.-99)) THEN
          write(i_log,*)' Warning: IAEA format phsp file does not store
     *LATCH'
        END IF
        DO 1241 I_PHSP=1,iaea_n_extra_floats
          IF ((iaea_extra_float_types(I_PHSP).EQ.3)) THEN
            iaea_i_zlast=I_PHSP
            GO TO1242
          END IF
1241    CONTINUE
1242    CONTINUE
        IF ((iaea_i_zlast.EQ.-99)) THEN
          IZLAST1=0
        ELSE
          IZLAST1=1
        END IF
        iaea_i_muidx=MAX(1,iaea_i_zlast+1)
        IF ((iaea_i_muidx.GT.iaea_n_extra_floats .OR. iaea_extra_float_t
     *  ypes(iaea_i_muidx).NE.0)) THEN
          iaea_i_muidx=-99
          IMUPHSP1=0
        ELSE
          IMUPHSP1=1
        END IF
        WRITE(6,1250)PARANOT1,PARANOP1,PARAEMAX1,LPARANINC1
1250    FORMAT(/'            TOTAL NUMBER OF PARTICLES IN FILE:',I13/ ' 
     *                    TOTAL NUMBER OF PHOTONS:',I13/ 'THE REST ARE E
     *LECTRONS/POSITRONS.'/ ' '/ '      MAXIMUM KINETIC ENERGY OF THE PA
     *RTICLES:',F13.3,' MeV'/ ' # OF INCIDENT PARTICLES FROM ORIGINAL SO
     *URCE:',I13/)
        IF ((IZSCORE1.EQ.0)) THEN
          WRITE(6,1260)Z_SCORE
1260      FORMAT('                       Z AT WHICH PHSP SCORED:',F13.3,
     *' cm'/)
        END IF
        PARANOT=PARANOT1
      ELSE
        WRITE(6,1270)INFILE(:lnblnk1(INFILE))
1270    FORMAT(/' Header information for ',A,':'/)
        itmp=-1
        IF ((itmp.GT.0)) THEN
          PHSP_RECL=4*8
          OPEN(UNIT=2,FILE=INFILE,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1280)
1280        FORMAT(/' ***error opening file as MODE2 ****' /' *** THE FI
     *LE FORMAT MAY BE WRONG (I.E., NOT A MODE2 FILE) ***'/ /' *** WE NO
     *W TRY TO OPEN IT AS A MODE3 FILE ***'//)
            OPEN(UNIT=2,STATUS='OLD',FILE=INFILE, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1290)
1290          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
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
              WRITE(6,1300)
1300          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW1 .NE. 'MODE3')) THEN
              WRITE(6,1310)
1310          FORMAT(//' That file does not start with MODE3,', ' as all
     * old compressed files (with ZLAST) must'/ '  Check it out and trya
     *gain!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,1320)
1320          FORMAT(//' This is a MODE3 file! '/ ' Please convert it in
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
            WRITE(6,1330)
1330        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW1 .NE. 'MODE2')) THEN
            WRITE(6,1340)
1340        FORMAT(//' That file does not start with MODE2,', ' as stand
     *ard compressed files with ZLAST must'/ '  Check it out and try aga
     *in!'///)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.EQ.0)) THEN
          PHSP_RECL=4*7
          OPEN(UNIT=2,FILE=INFILE,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1350)
1350        FORMAT(/' ***ERROR opening file as MODE0****' /' *** THE FIL
     *E FORMAT MAY BE WRONG (I.E., NOT A MODE0 FILE) ***'/ /' *** WE NOW
     * TRY TO OPEN IT AS A MODE1 FILE ***'//)
            OPEN(UNIT=2,STATUS='OLD',FILE=INFILE, FORM='UNFORMATTED', IO
     *      STAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1360)
1360          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
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
              WRITE(6,1370)
1370          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW1 .NE. 'MODE1')) THEN
              WRITE(6,1380)
1380          FORMAT(//' That file does not start with MODE1,', ' as all
     * old compressed files must'/ '  Check it out and try again!'///)
              IERR_PHSP=1
              STOP
            ELSE
              WRITE(6,1390)
1390          FORMAT(//' This is a MODE1 file! '/ ' Please convert it in
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
            WRITE(6,1400)
1400        FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE*** '/
     */)
            STOP
          END IF
          IF ((MODE_RW1 .NE. 'MODE0')) THEN
            WRITE(6,1410)
1410        FORMAT(/' Does not start with MODE0 as files without ZLAST m
     *ust'/ '  Try again!'//)
            IERR_PHSP=1
            STOP
          END IF
        ELSE IF((itmp.LT.0)) THEN
          WRITE(6,1420)
1420      FORMAT(/' First, try to open it as a MODE0 file')
          PHSP_RECL=4*7
          OPEN(UNIT=2,FILE=INFILE,FORM='UNFORMATTED',ACCESS='DIRECT', RE
     *    CL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
          IF ((IERR_PHSP.NE.0)) THEN
            WRITE(6,1430)
1430        FORMAT(/' Now try to open it as a MODE2 file')
            itmp=1
            PHSP_RECL=4*8
            OPEN(UNIT=2,FILE=INFILE,FORM='UNFORMATTED',ACCESS='DIRECT',
     *      RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1440)
1440          FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
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
                WRITE(6,1450)
1450            FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE**
     ** '//)
                STOP
              END IF
              IF ((MODE_RW1.NE.'MODE2')) THEN
                WRITE(6,1460)
1460            FORMAT(//' That file does not start with MODE2,', ' as s
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
              WRITE(6,1470)
1470          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            itmp=0
            IF ((MODE_RW1.NE.'MODE0')) THEN
              WRITE(6,1480)
1480          FORMAT(/' The file does not start with MODE0 as it suppose
     *d to')
              CLOSE(2)
              WRITE(6,1490)
1490          FORMAT(/' Now try to open it as a MODE2 file')
              itmp=1
              PHSP_RECL=4*8
              OPEN(UNIT=2,FILE=INFILE,FORM='UNFORMATTED',ACCESS='DIRECT'
     *        , RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1500)
1500            FORMAT(//' *** PHASE SPACE FILE CANNOT BE OPENED. ***')
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
                  WRITE(6,1510)
1510              FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE
     **** '//)
                  STOP
                END IF
                IF ((MODE_RW1.NE.'MODE2')) THEN
                  WRITE(6,1520)
1520              FORMAT(//' That file does not start with MODE2,', ' as
     * standard compressed files with ZLAST must'/ '  Check it out and t
     *ry again!'///)
                  CLOSE(2)
                  STOP
                END IF
              END IF
            END IF
          END IF
        END IF
        IF ((IERR_PHSP.NE.0)) THEN
          IF ((ifirst.EQ.1)) THEN
            sumerr=3
          ELSE
            sumerr=1
          END IF
          GOTO 1220
        END IF
        IZLAST1=0
        IF((MODE_RW1.EQ.'MODE2'))IZLAST1=1
        PARANOT=PARANOT1
        WRITE(6,1530)PARANOT1,PARANOP1,PARAEMAX1,PARAEMNE1,PARANINC1
1530    FORMAT(/'            TOTAL NUMBER OF PARTICLES IN FILE:',I13/ ' 
     *                    TOTAL NUMBER OF PHOTONS:',I13/ 'THE REST ARE E
     *LECTRONS/POSITRONS.'/ ' '/ '      MAXIMUM KINETIC ENERGY OF THE PA
     *RTICLES:',F13.3,' MeV'/ '      MINIMUM KINETIC ENERGY OF THE ELECT
     *RONS:',F13.3,' MeV'/ ' # OF INCIDENT PARTICLES FROMORIGINAL SOURCE
     *:',F13.1)
      END IF
      IF ((i_iaea_in.EQ.1)) THEN
        WRITE(6,1540)OUTFILE(:lnblnk1(OUTFILE))//'.IAEAphsp'
1540    FORMAT(//' Header information for ',A,':'/)
      ELSE
        WRITE(6,1550)OUTFILE(:lnblnk1(OUTFILE))
1550    FORMAT(//' Header information for ',A,':'/)
      END IF
      PARANOT2=0
      PARANOP2=0
      LPARANINC2=0
      PARAEMAX2=0.
      PARAEMNE2=0.
      PARANINC2=0.
      MODE_RW2=' '
      IF ((ifirst.EQ.0)) THEN
        IF ((i_iaea_in.EQ.1)) THEN
          i_unit_out=3
          call iaea_new_source(i_unit_out,OUTFILE,1+2,iaea_result)
          IF ((iaea_result.LT.0)) THEN
            WRITE(i_log,*)' Error opening IAEA phase space file for writ
     *e.'
            call exit(1)
          END IF
          i_iaea_open_for_write=1
          IF ((1.EQ.0)) THEN
            IF((IZSCORE2.NE.1))call iaea_set_constant_variable(i_unit_ou
     *      t,2,Z_SCORE)
            IF ((IZLAST2.NE.0)) THEN
              IF ((IMUPHSP2.NE.0)) THEN
                call iaea_set_extra_numbers(i_unit_out,2,1)
                call iaea_set_type_extrafloat_variable(i_unit_out,1,0)
                iaea_i_muidx_out=2
              ELSE
                call iaea_set_extra_numbers(i_unit_out,1,1)
              END IF
              call iaea_set_type_extrafloat_variable(i_unit_out,0,3)
              iaea_i_zlast_out=1
            ELSE IF((IMUPHSP2.NE.0)) THEN
              call iaea_set_extra_numbers(i_unit_out,1,1)
              call iaea_set_type_extrafloat_variable(i_unit_out,0,0)
              iaea_i_muidx_out=1
            ELSE
              call iaea_set_extra_numbers(i_unit_out,0,1)
            END IF
            call iaea_set_type_extralong_variable(i_unit_out,0,2)
            iaea_i_latch_out=1
          END IF
          IF ((iaea_result.LT.0)) THEN
            sumerr=2
            GOTO 1560
          END IF
          call iaea_get_max_particles(i_unit_out,-1,iaea_dummy_long)
          PARANOT2=iaea_dummy_long
          call iaea_get_max_particles(i_unit_out,1,iaea_dummy_long)
          PARANOP2=iaea_dummy_long
          call iaea_get_total_original_particles(i_unit_out,iaea_dummy_l
     *    ong)
          LPARANINC2=iaea_dummy_long
          call iaea_get_maximum_energy(i_unit_out,EKMAX_PHSP_SHORT)
          PARAEMAX2=EKMAX_PHSP_SHORT
          call iaea_get_constant_variable(i_unit_out,2,Z_PHSP_SHORT,iaea
     *    _result)
          IF ((iaea_result.EQ.-3)) THEN
            write(i_log,*) ' Z positions of each particle will be read f
     *rom phase space data.'
            Z_SCORE=999.
            IZSCORE2=1
          ELSE IF((iaea_result.LT.0)) THEN
            write(i_log,*)' Error reading Z position where IAEA phsp was
     * scored.'
            IZSCORE2=0
          ELSE
            Z_SCORE=Z_PHSP_SHORT
            IZSCORE2=0
          END IF
          call iaea_get_extra_numbers(i_unit_out,iaea_n_extra_floats,iae
     *    a_n_extra_ints)
          IF ((iaea_n_extra_floats .EQ. -1 .OR. iaea_n_extra_ints .EQ. -
     *    1)) THEN
            write(i_log,*)' Error reading number of extra variables stor
     *ed in IAEA phsp'
          END IF
          call iaea_get_type_extra_variables(i_unit_out,iaea_result,iaea
     *    _extra_int_types, iaea_extra_float_types)
          IF ((iaea_result.EQ.-1)) THEN
            write(i_log,*)' Error getting types of extra variables store
     *d in IAEA phsp'
          END IF
          DO 1571 I_PHSP=1,iaea_n_extra_ints
            IF ((iaea_extra_int_types(I_PHSP).EQ.2)) THEN
              iaea_i_latch=I_PHSP
              GO TO1572
            END IF
1571      CONTINUE
1572      CONTINUE
          IF ((iaea_i_latch.EQ.-99)) THEN
            write(i_log,*)' Warning: IAEA format phsp file does not stor
     *e LATCH'
          END IF
          DO 1581 I_PHSP=1,iaea_n_extra_floats
            IF ((iaea_extra_float_types(I_PHSP).EQ.3)) THEN
              iaea_i_zlast=I_PHSP
              GO TO1582
            END IF
1581      CONTINUE
1582      CONTINUE
          IF ((iaea_i_zlast.EQ.-99)) THEN
            IZLAST2=0
          ELSE
            IZLAST2=1
          END IF
          iaea_i_muidx=MAX(1,iaea_i_zlast+1)
          IF ((iaea_i_muidx.GT.iaea_n_extra_floats .OR. iaea_extra_float
     *    _types(iaea_i_muidx).NE.0)) THEN
            iaea_i_muidx=-99
            IMUPHSP2=0
          ELSE
            IMUPHSP2=1
          END IF
          WRITE(6,1590)PARANOT2,PARANOP2,PARAEMAX2,LPARANINC2
1590      FORMAT(/'            TOTAL NUMBER OF PARTICLES IN FILE:',I13/'
     *                      TOTAL NUMBER OF PHOTONS:',I13/ '      THE RE
     *ST ARE ELECTRONS/POSITRONS.'/ ' '/ '      MAXIMUM KINETIC ENERGY O
     *F THE PARTICLES:',F13.3,' MeV'/ ' # OF INCIDENT PARTICLES FROM ORI
     *GINAL SOURCE:',I13/)
          IF ((IZSCORE2.EQ.1)) THEN
            WRITE(6,1600)Z_SCORE
1600        FORMAT('                       Z AT WHICH PHSP SCORED:',F13.
     *3,' cm'/)
          END IF
          IF ((IZLAST1 .NE. IZLAST2)) THEN
            WRITE(6,1610)
1610        FORMAT(/' FILE TO BE ADDED HAS A DIFFERENT MODE THAN SUMMEDF
     *ILE!!! '/)
            WRITE(6,1620)
1620        FORMAT(/' WILL MOVE ON TO THE NEXT FILE TO BE ADDED.'/)
            call iaea_destroy_source(i_unit_in,iaea_result)
            IF ((iaea_result.LT.0)) THEN
              WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit
     *        _in
            END IF
            call iaea_destroy_source(i_unit_out,iaea_result)
            IF ((iaea_result.LT.0)) THEN
              WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit
     *        _out
            END IF
            sumerr=1
            RETURN
          END IF
          IF ((IZSCORE1 .NE. IZSCORE2)) THEN
            WRITE(6,1630)
1630        FORMAT(/' Error: File mismatch in scoring of particle Z posi
     *tion.'/ ' Will move on to the next file to be added.'/)
            call iaea_destroy_source(i_unit_in,iaea_result)
            IF ((iaea_result.LT.0)) THEN
              WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit
     *        _in
            END IF
            call iaea_destroy_source(i_unit_out,iaea_result)
            IF ((iaea_result.LT.0)) THEN
              WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit
     *        _out
            END IF
            sumerr=1
            RETURN
          END IF
          IF ((IMUPHSP1.NE.IMUPHSP2)) THEN
            WRITE(6,1640)
1640        FORMAT(/' Error: File mismatch in scoring of fractional MU i
     *ndex.'/ ' Will move on to the next file to be added.'/)
            call iaea_destroy_source(i_unit_in,iaea_result)
            IF ((iaea_result.LT.0)) THEN
              WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit
     *        _in
            END IF
            call iaea_destroy_source(i_unit_out,iaea_result)
            IF ((iaea_result.LT.0)) THEN
              WRITE(i_log,*)' Error closing IAEA phase space ID ',i_unit
     *        _out
            END IF
            sumerr=1
            RETURN
          END IF
          NHSTRY=LPARANINC2
        ELSE
          itmp=-1
          IF ((itmp.GT.0)) THEN
            PHSP_RECL=4*8
            OPEN(UNIT=3,FILE=OUTFILE,FORM='UNFORMATTED',ACCESS='DIRECT',
     *       RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1650)
1650          FORMAT(/' ***error opening file as MODE2 ****' /' *** THEF
     *ILE FORMAT MAY BE WRONG (I.E., NOT A MODE2 FILE) ***'/ /' *** WENO
     *W TRY TO OPEN IT AS A MODE3 FILE ***'//)
              OPEN(UNIT=3,STATUS='OLD',FILE=OUTFILE, FORM='UNFORMATTED',
     *         IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1660)
1660            FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
                STOP
              END IF
              READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_P
     *        HSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHOR
     *        T
              PARANOT2=NUM_PHSP_TOT
              PARANOP2=PHOT_PHSP_TOT
              PARAEMAX2=EKMAX_PHSP_SHORT
              PARAEMNE2=EKMINE_PHSP_SHORT
              PARANINC2=NINC_PHSP_SHORT
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1670)
1670            FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE**
     ** '//)
                STOP
              END IF
              IF ((MODE_RW2 .NE. 'MODE3')) THEN
                WRITE(6,1680)
1680            FORMAT(//' That file does not start with MODE3,', ' as a
     *ll old compressed files (with ZLAST) must'/ '  Check it out and tr
     *y again!'///)
                IERR_PHSP=1
                STOP
              ELSE
                WRITE(6,1690)
1690            FORMAT(//' This is a MODE3 file! '/ ' Please convert iti
     *nto a MODE2 file using [readphsp] ', ' and try again!'///)
                IERR_PHSP=1
                STOP
              END IF
            END IF
            READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT2=NUM_PHSP_TOT
            PARANOP2=PHOT_PHSP_TOT
            PARAEMAX2=EKMAX_PHSP_SHORT
            PARAEMNE2=EKMINE_PHSP_SHORT
            PARANINC2=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1700)
1700          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW2 .NE. 'MODE2')) THEN
              WRITE(6,1710)
1710          FORMAT(//' That file does not start with MODE2,', ' as sta
     *ndard compressed files with ZLAST must'/ '  Check it out and try a
     *gain!'///)
              IERR_PHSP=1
              STOP
            END IF
          ELSE IF((itmp.EQ.0)) THEN
            PHSP_RECL=4*7
            OPEN(UNIT=3,FILE=OUTFILE,FORM='UNFORMATTED',ACCESS='DIRECT',
     *       RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1720)
1720          FORMAT(/' ***ERROR opening file as MODE0****' /' *** THE F
     *ILE FORMAT MAY BE WRONG (I.E., NOT A MODE0 FILE) ***'/ /' *** WE N
     *OW TRY TO OPEN IT AS A MODE1 FILE ***'//)
              OPEN(UNIT=3,STATUS='OLD',FILE=OUTFILE, FORM='UNFORMATTED',
     *         IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1730)
1730            FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
                STOP
              END IF
              READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_P
     *        HSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHOR
     *        T
              PARANOT2=NUM_PHSP_TOT
              PARANOP2=PHOT_PHSP_TOT
              PARAEMAX2=EKMAX_PHSP_SHORT
              PARAEMNE2=EKMINE_PHSP_SHORT
              PARANINC2=NINC_PHSP_SHORT
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1740)
1740            FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE**
     ** '//)
                STOP
              END IF
              IF ((MODE_RW2 .NE. 'MODE1')) THEN
                WRITE(6,1750)
1750            FORMAT(//' That file does not start with MODE1,', ' as a
     *ll old compressed files must'/ '  Check it out and try again!'///)
                IERR_PHSP=1
                STOP
              ELSE
                WRITE(6,1760)
1760            FORMAT(//' This is a MODE1 file! '/ ' Please convert iti
     *nto a MODE0 file using [readphsp] ', 'and try again!'///)
                IERR_PHSP=1
                STOP
              END IF
            END IF
            READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PHOT_PHS
     *      P_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
            PARANOT2=NUM_PHSP_TOT
            PARANOP2=PHOT_PHSP_TOT
            PARAEMAX2=EKMAX_PHSP_SHORT
            PARAEMNE2=EKMINE_PHSP_SHORT
            PARANINC2=NINC_PHSP_SHORT
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1770)
1770          FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE***'
     *//)
              STOP
            END IF
            IF ((MODE_RW2 .NE. 'MODE0')) THEN
              WRITE(6,1780)
1780          FORMAT(/' Does not start with MODE0 as files without ZLAST
     * must'/ '  Try again!'//)
              IERR_PHSP=1
              STOP
            END IF
          ELSE IF((itmp.LT.0)) THEN
            WRITE(6,1790)
1790        FORMAT(/' First, try to open it as a MODE0 file')
            PHSP_RECL=4*7
            OPEN(UNIT=3,FILE=OUTFILE,FORM='UNFORMATTED',ACCESS='DIRECT',
     *       RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
            IF ((IERR_PHSP.NE.0)) THEN
              WRITE(6,1800)
1800          FORMAT(/' Now try to open it as a MODE2 file')
              itmp=1
              PHSP_RECL=4*8
              OPEN(UNIT=3,FILE=OUTFILE,FORM='UNFORMATTED',ACCESS='DIRECT
     *', RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
              IF ((IERR_PHSP.NE.0)) THEN
                WRITE(6,1810)
1810            FORMAT(//' *** PHASE SPACE FILE CANNOT BE FOUND. ***')
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
                  WRITE(6,1820)
1820              FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE
     **** '//)
                  STOP
                END IF
                IF ((MODE_RW2.NE.'MODE2')) THEN
                  WRITE(6,1830)
1830              FORMAT(//' That file does not start with MODE2,', ' as
     * standard compressed files with ZLAST must'/ '  Check it out and t
     *ry again!'///)
                  CLOSE(3)
                  STOP
                END IF
              END IF
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
                WRITE(6,1840)
1840            FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FILE**
     ** '//)
                STOP
              END IF
              itmp=0
              IF ((MODE_RW2.NE.'MODE0')) THEN
                WRITE(6,1850)
1850            FORMAT(/' The file does not start with MODE0 as it suppo
     *sed to')
                CLOSE(3)
                WRITE(6,1860)
1860            FORMAT(/' Now try to open it as a MODE2 file')
                itmp=1
                PHSP_RECL=4*8
                OPEN(UNIT=3,FILE=OUTFILE,FORM='UNFORMATTED',ACCESS='DIRE
     *CT', RECL=PHSP_RECL,STATUS='OLD',IOSTAT=IERR_PHSP)
                IF ((IERR_PHSP.NE.0)) THEN
                  WRITE(6,1870)
1870              FORMAT(//' *** PHASE SPACE FILE CANNOT BE OPENED. ***'
     *)
                ELSE
                  READ(3,REC=1,IOSTAT=IERR_PHSP)MODE_RW2,NUM_PHSP_TOT,PH
     *            OT_PHSP_TOT, EKMAX_PHSP_SHORT,EKMINE_PHSP_SHORT,NINC_P
     *            HSP_SHORT
                  PARANOT2=NUM_PHSP_TOT
                  PARANOP2=PHOT_PHSP_TOT
                  PARAEMAX2=EKMAX_PHSP_SHORT
                  PARAEMNE2=EKMINE_PHSP_SHORT
                  PARANINC2=NINC_PHSP_SHORT
                  IF ((IERR_PHSP.NE.0)) THEN
                    WRITE(6,1880)
1880                FORMAT(//' ***ERROR READING HEADER OF PHASE SPACE FI
     *LE*** '//)
                    STOP
                  END IF
                  IF ((MODE_RW2.NE.'MODE2')) THEN
                    WRITE(6,1890)
1890                FORMAT(//' That file does not start with MODE2,', 'a
     *s standard compressed files with ZLAST must'/ '  Check it out and 
     *try again!'///)
                    CLOSE(3)
                    STOP
                  END IF
                END IF
              END IF
            END IF
          END IF
          IF ((IERR_PHSP.NE.0)) THEN
            sumerr=2
            GOTO 1560
          END IF
          WRITE(6,1900)PARANOT2,PARANOP2,PARAEMAX2,PARAEMNE2,PARANINC2
1900      FORMAT(/'            TOTAL NUMBER OF PARTICLES IN FILE:',I13/'
     *                      TOTAL NUMBER OF PHOTONS:',I13/ '      THE RE
     *ST ARE ELECTRONS/POSITRONS.'/ ' '/ '      MAXIMUM KINETIC ENERGY O
     *F THE PARTICLES:',F13.3,' MeV'/ '      MINIMUM KINETIC ENERGY OF T
     *HE ELECTRONS:',F13.3,' MeV'/ ' # OF INCIDENT PARTICLES FROM ORIGIN
     *AL SOURCE:',F13.1)
          IF ((MODE_RW2 .NE. MODE_RW1)) THEN
            WRITE(6,1910)
1910        FORMAT(/' FILE TO BE ADDED HAS A DIFFERENT MODE THAN SUMMEDF
     *ILE!!! '/)
            WRITE(6,1920)
1920        FORMAT(/' WILL MOVE ON TO THE NEXT FILE TO BE ADDED.'/)
            CLOSE(UNIT=2)
            CLOSE(UNIT=3)
            sumerr=1
            RETURN
          END IF
          NHSTRY=PARANINC2
        END IF
      ELSE
        NHSTRY=0
        WRITE(6,1930)
1930    FORMAT(/' First time writing to this file.'/ ' No header data to
     * display.'/)
        IF ((i_iaea_in.EQ.1)) THEN
          i_unit_out=3
          call iaea_new_source(i_unit_out,OUTFILE,0+2,iaea_result)
          IF ((iaea_result.LT.0)) THEN
            WRITE(i_log,*)' Error opening IAEA phase space file for writ
     *e.'
            call exit(1)
          END IF
          i_iaea_open_for_write=1
          IF ((0.EQ.0)) THEN
            IF((IZSCORE1.NE.1))call iaea_set_constant_variable(i_unit_ou
     *      t,2,Z_SCORE)
            IF ((IZLAST1.NE.0)) THEN
              IF ((IMUPHSP1.NE.0)) THEN
                call iaea_set_extra_numbers(i_unit_out,2,1)
                call iaea_set_type_extrafloat_variable(i_unit_out,1,0)
                iaea_i_muidx_out=2
              ELSE
                call iaea_set_extra_numbers(i_unit_out,1,1)
              END IF
              call iaea_set_type_extrafloat_variable(i_unit_out,0,3)
              iaea_i_zlast_out=1
            ELSE IF((IMUPHSP1.NE.0)) THEN
              call iaea_set_extra_numbers(i_unit_out,1,1)
              call iaea_set_type_extrafloat_variable(i_unit_out,0,0)
              iaea_i_muidx_out=1
            ELSE
              call iaea_set_extra_numbers(i_unit_out,0,1)
            END IF
            call iaea_set_type_extralong_variable(i_unit_out,0,2)
            iaea_i_latch_out=1
          END IF
        ELSE
          IF ((IZLAST1.NE.0)) THEN
            MODE_RW2='MODE2'
            PHSP_RECL=4*8000
          ELSE
            MODE_RW2='MODE0'
            PHSP_RECL=4*7000
          END IF
          OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL, FORM=
     *    'UNFORMATTED',STATUS='UNKNOWN')
          CLOSE(3)
          IF ((IZLAST1.NE.0)) THEN
            PHSP_RECL=4*8
          ELSE
            PHSP_RECL=4*7
          END IF
          OPEN(UNIT=3,FILE=OUTFILE,RECL=PHSP_RECL,ACCESS='DIRECT', FORM=
     *    'UNFORMATTED',STATUS='UNKNOWN')
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
          WRITE(3,REC=1)MODE_RW2,NUM_PHSP_TOT,PHOT_PHSP_TOT,EKMAX_PHSP_S
     *    HORT, EKMINE_PHSP_SHORT,NINC_PHSP_SHORT
          CLOSE(3)
          IF ((IZLAST1.NE.0)) THEN
            PHSP_RECL=4*8000
          ELSE
            PHSP_RECL=4*7000
          END IF
          OPEN(UNIT=3,FILE=OUTFILE,RECL=PHSP_RECL,ACCESS='DIRECT', FORM=
     *    'UNFORMATTED',STATUS='UNKNOWN')
        END IF
      END IF
      WRITE(6,1940)
1940  FORMAT(/' BEGIN READING/WRITING PH-SP DATA .....'/)
      IPARANOT2=PARANOT2+1
      IF ((i_iaea_in.EQ.1)) THEN
        DO 1951 IPARANOT1=1,PARANOT1
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
            MU_IDX=MUIDX_PHSP_SHORT
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
            MUIDX_PHSP_SHORT=MU_IDX
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
1951    CONTINUE
1952    CONTINUE
        LNINC = LPARANINC1 + LPARANINC2
        iaea_dummy_long=LNINC
        call iaea_set_total_original_particles(i_unit_out,iaea_dummy_lon
     *  g)
        call iaea_update_header(i_unit_out,iaea_result)
        IF ((iaea_result.LT.0)) THEN
          WRITE(i_log,*)' Error writing IAEA phase space header for ID '
     *    ,i_unit_out
          call exit(1)
        END IF
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
        DO 1961 IPARANOT1=2,PARANOT1+1
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
                OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL,
     *           FORM='UNFORMATTED',STATUS='UNKNOWN')
                DO 1971 I_PHSP=1,(IPARANOT2-1)-1000*((IPARANOT2-1)/1000)
                  READ(3,REC=1000*INT(dble(IPARANOT2-1)/1000)+I_PHSP) ST
     *            RING_TEMP_ZLAST_OUT(1)(32*(I_PHSP-1)+1:32*I_PHSP)
1971            CONTINUE
1972            CONTINUE
                CLOSE(3)
                PHSP_RECL=4*8000
                OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL,
     *           FORM='UNFORMATTED',STATUS='UNKNOWN')
              ELSE
                PHSP_RECL=4*7
                OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL,
     *           FORM='UNFORMATTED',STATUS='UNKNOWN')
                DO 1981 I_PHSP=1,(IPARANOT2-1)-1000*((IPARANOT2-1)/1000)
                  READ(3,REC=1000*INT(dble(IPARANOT2-1)/1000)+I_PHSP) ST
     *            RING_TEMP_OUT(1)(28*(I_PHSP-1)+1:28*I_PHSP)
1981            CONTINUE
1982            CONTINUE
                CLOSE(3)
                PHSP_RECL=4*7000
                OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL,
     *           FORM='UNFORMATTED',STATUS='UNKNOWN')
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
              OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL, F
     *        ORM='UNFORMATTED',STATUS='UNKNOWN')
            ELSE IF((IZLAST1.EQ.0 .AND. PHSP_RECL.EQ.4*7)) THEN
              PHSP_RECL_OLD=PHSP_RECL
              CLOSE(3)
              PHSP_RECL=4*7000
              OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL, F
     *        ORM='UNFORMATTED',STATUS='UNKNOWN')
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
              OPEN(UNIT=3,FILE=OUTFILE,ACCESS='DIRECT',RECL=PHSP_RECL, F
     *        ORM='UNFORMATTED',STATUS='UNKNOWN')
            END IF
            WRITE_PHSP_COUNTER(1)=0
            WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+1000
          END IF
1961    CONTINUE
1962    CONTINUE
1990    CONTINUE
        TEMP_PHSP_COUNTER=0
        NUM_PHSP_TOFLUSH=WRITE_PHSP_COUNTER(1)
        IF ((NUM_PHSP_TOFLUSH.GT.0)) THEN
          IF ((WRITE_PHSP_SOFAR(1).EQ.0)) THEN
            WRITE_PHSP_COUNTER(1)=WRITE_PHSP_COUNTER(1)+1
            NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH+1
          END IF
2001      CONTINUE
            TEMP_PHSP_COUNTER=NUM_PHSP_TOFLUSH+1
2011        CONTINUE
              TEMP_PHSP_COUNTER=TEMP_PHSP_COUNTER-1
              REM_PHSP=MOD(WRITE_PHSP_SOFAR(1),TEMP_PHSP_COUNTER)
              IF(((REM_PHSP.EQ.0)))GO TO2012
            GO TO 2011
2012        CONTINUE
            CLOSE(3)
            IF ((IZLAST1.NE.0)) THEN
              PHSP_RECL=4*8*TEMP_PHSP_COUNTER
              OPEN(UNIT=3,FILE=OUTFILE,RECL=PHSP_RECL, ACCESS='DIRECT',F
     *        ORM='UNFORMATTED',STATUS='UNKNOWN')
              WRITE(3,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNTER+1) STRIN
     *        G_TEMP_ZLAST_OUT(1) (32*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TO
     *        FLUSH)+1: 32*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TOFLUSH+TEMP_
     *        PHSP_COUNTER))
            ELSE
              PHSP_RECL=4*7*TEMP_PHSP_COUNTER
              OPEN(UNIT=3,FILE=OUTFILE,RECL=PHSP_RECL, ACCESS='DIRECT',F
     *        ORM='UNFORMATTED',STATUS='UNKNOWN')
              WRITE(3,REC=WRITE_PHSP_SOFAR(1)/TEMP_PHSP_COUNTER+1) STRIN
     *        G_TEMP_OUT(1) (28*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TOFLUSH)
     *        +1: 28*(WRITE_PHSP_COUNTER(1)-NUM_PHSP_TOFLUSH+TEMP_PHSP_C
     *        OUNTER))
            END IF
            WRITE_PHSP_SOFAR(1)=WRITE_PHSP_SOFAR(1)+TEMP_PHSP_COUNTER
            NUM_PHSP_TOFLUSH=NUM_PHSP_TOFLUSH-TEMP_PHSP_COUNTER
            IF(((NUM_PHSP_TOFLUSH.EQ.0)))GO TO2002
          GO TO 2001
2002      CONTINUE
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
        CLOSE(3)
        IF ((IZLAST1.NE.0)) THEN
          PHSP_RECL=4*8
        ELSE
          PHSP_RECL=4*7
        END IF
        OPEN(UNIT=3,FILE=OUTFILE,RECL=PHSP_RECL,ACCESS='DIRECT', FORM='U
     *NFORMATTED',STATUS='UNKNOWN')
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
        OPEN(UNIT=3,FILE=OUTFILE,RECL=PHSP_RECL,ACCESS='DIRECT', FORM='U
     *NFORMATTED',STATUS='UNKNOWN')
        CLOSE(UNIT=2)
        CLOSE(UNIT=3)
      END IF
      WRITE(6,2020)
2020  FORMAT(/' Finished reading/writing ph-sp data .....'/)
      RETURN
1220  WRITE(6,2030)
2030  FORMAT(//' CANNOT FIND/OPEN THE PH-SP FILE TO BE ADDED!!!'/ ' WILL
     * MOVE ON TO THE NEXT ONE IN THE SERIES.'///)
      RETURN
1560  WRITE(6,2040)
2040  FORMAT(//' CANNOT FIND/OPEN THE FILE CONTAINING SUMMED PHSP DATA!!
     *!'/ ' PROGRAM WILL TERMINATE.'///)
      RETURN
      END
      integer*4 function lnblnk1(string)
      character*(*) string
      DO 2051 i=len(string),1,-1
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
2051  CONTINUE
2052  CONTINUE
      lnblnk1 = 0
      return
      end
