      IMPLICIT NONE
      Character*60 ctformat
      Character*40 machine
      Character*256 CTFileName
      Integer nmed
      Character*4 media(24,20)
      Real rhor( 2097154)
      Integer med( 2097154)
      Real xbound(128+1)
      Real ybound(128+1)
      Real zbound(128+1)
      Integer CTArraySize(3)
      Real CTVoxelSize(3)
      Real CTOffset(3)
      Integer*2 CTHounsData(512,512,270)
      Integer*2 CTResizeData(128,128,128)
      Integer CTErrorCode
      Integer imax,jmax,kmax
      Integer lnblnk1,l
      integer iargc,narg
      character input_file*256
      logical unknown_format,file_exists
#define HAVE_C_COMPILER
#if defined HAVE_C_COMPILER
#define WITH_DICOM
#endif
      narg = iargc()
      IF (( narg .GT. 0 )) THEN
          call getarg(1,input_file)
          inquire(file=input_file,exist=file_exists)
          IF (( .NOT.file_exists )) THEN
              write(6,'(/a,a,a/)') 'File ',input_file(:lnblnk1(input_fil
     *        e)), ' does not exist'
              call exit(1)
          END IF
          open(5,file=input_file)
          open(1,file=input_file(:lnblnk1(input_file))//'.ctlst')
      ELSE
          write(6,'(/a/)') 'No input file -> all input to come from the
     *terminal'
          open(1,file='interactiv.ctlst')
      END IF
      write(6,1010)
      write(1,1010)
1010  format('==========================================================
     *===')
      write(6,1020)
      write(1,1020)
1020  format(' Running ctcreate')
      write(6,1030)
      write(1,1030)
1030  format('==========================================================
     *===')
      write(6,1040)
      write(1,1040)
1040  format(//' *******************************************************
     ******'/ ' '/ '           CT formats currently supported:'/ '     -
     *----------------------------- '/ ' '/ '           1. Pinnacle     
     *               '/ '           2. CADPLAN ')
#ifdef WITH_DICOM
      write(6,1050)
      write(1,1050)
1050  format( '           3. DICOM                       ')
#endif
      write(6,1060)
      write(1,1060)
1060  format(//' *******************************************************
     ******')
      write(6,1070)
      write(1,1070)
1070  format(//' Input the format of your CT data'/ ' : ',$)
      read(5,'(A60)') ctformat
      write(6,1080)ctformat(:lnblnk1(ctformat))
      write(1,1080)ctformat(:lnblnk1(ctformat))
1080  format(A60)
      unknown_format = .true.
      IF ((ctformat.EQ.'pinnacle'.OR.ctformat.EQ.'PINNACLE'.OR.ctformat.
     *EQ.'Pinnacle')) THEN
          ctformat='Pinnacle'
          unknown_format = .false.
      ELSE IF((ctformat.EQ.'cadplan'.OR.ctformat.EQ.'Cadplan'.OR.ctforma
     *t.EQ.'CadPlan'.OR. ctformat.EQ.'CADPLAN')) THEN
          ctformat='CADPLAN'
          unknown_format = .false.
      ELSE IF((ctformat.EQ.'aapm'.OR. ctformat.EQ.'AAPM')) THEN
          write(6,1090)
          write(1,1090)
1090      format(//' Convert CT data from AAPM format to Pinnacle format
     * using'/ ' $OMEGA_HOME/dosxyznrc/CT/aapm2pinnacle.'//)
          call exit(1)
      END IF
#ifdef WITH_DICOM
      IF ((ctformat.EQ.'dicom'.OR.ctformat.EQ.'Dicom'.OR.ctformat.EQ.'DI
     *COM'.OR.ctformat.EQ.'3')) THEN
          ctformat='DICOM'
          unknown_format = .false.
      END IF
#else
      IF ((ctformat.EQ.'dicom'.OR.ctformat.EQ.'Dicom'.OR.ctformat.EQ.'DI
     *COM'.OR.ctformat.EQ.'3')) THEN
          WRITE(6,1100)
1100      FORMAT(//' The DICOM routines are written in C. You need a wor
     *king'/ ' C compiler to use them'//)
          call exit(1)
      END IF
#endif
      IF (( unknown_format )) THEN
          WRITE(6,1110)ctformat
1110      FORMAT(//' CT data format ',a,' not currently handled.'//)
          call exit(1)
      END IF
      IF ((ctformat.EQ.'Pinnacle')) THEN
          write(6,1120)
          write(1,1120)
1120      format(//' Input the full name of the header file for the CT d
     *ata'/ ' : ',$)
      ELSE IF((ctformat.EQ.'CADPLAN')) THEN
          write(6,1130)
          write(1,1130)
1130      format(//' Input the full name of the file of CT data file nam
     *es'/ ' : ',$)
      ELSE IF((ctformat.EQ.'DICOM')) THEN
          write(6,1140)
          write(1,1140)
1140      format(//' Input the full name of the file of DICOM file names
     *'/ ' : ',$)
      END IF
      read(5,'(A256)') CTFilename
      l = lnblnk1(CTFilename)
      write(6,1150)CTFilename(:l)
      write(1,1150)CTFilename(:l)
1150  format(A)
      IF ((ctformat.EQ.'Pinnacle')) THEN
          Call ReadCT_Pinnacle(CTFileName, CTArraySize, CTHounsData, CTO
     *    ffset, CTVoxelSize,CTErrorCode)
      ELSE IF((ctformat.EQ.'CADPLAN')) THEN
          Call ReadCT_CADPLAN(CTFileName, CTArraySize, CTHounsData, CTOf
     *    fset, CTVoxelSize,CTErrorCode)
      END IF
#ifdef WITH_DICOM
      IF ((ctformat.EQ.'DICOM')) THEN
          CTFilename(l+1:l+1) = char(0)
          Call ReadCT_DICOM(CTFileName, CTArraySize, CTHounsData, CTOffs
     *    et, CTVoxelSize,CTErrorCode)
      END IF
#endif
      IF ((CTArraySize(1).GT.512 .OR. CTArraySize(2).GT.512 .OR. CTArray
     *Size(3).GT.270)) THEN
          WRITE(6,1160)CTArraySize(1),CTArraySize(2),CTArraySize(3), 512
     *    ,512,270
1160      FORMAT(//' ***ERROR:'/ ' CT data array is ',I4,'x',I4,'x',I4/'
     * Max. ctcreate can deal with is ',I4,'x',I4,'x',I4/ ' Go into ctcr
     *eate and change $CTIMAX,$CTJMAX,$CTKMAX to deal'/ ' with thesize o
     *f the CT data array, recompile and try again.'//)
          STOP
      END IF
      Call SubsizeCT(CTArraySize,CTHounsData,CTVoxelSize,CTOffset)
      imax=128
      jmax=128
      kmax=128
      Call ResampleCT(CTArraySize(1),CTArraySize(2),CTArraySize(3), CTVo
     *xelSize(1),CTVoxelSize(2),CTVoxelSize(3), CTHounsData, imax,jmax,k
     *max, xbound,ybound,zbound, CTResizeData, CTOffset)
      Call CTToMedium(imax,jmax,kmax,  CTResizeData,  nmed,  med,  rhor,
     *  media)
      Call write_phantom(CTFileName,nmed,media, imax,jmax,kmax,xbound,yb
     *ound,zbound, rhor,med,ctformat)
      Close(1)
      End
      Subroutine ReadCT_Pinnacle(fname,asize,ctdata,offset,vsize,error)
      IMPLICIT NONE
      Character*256 fname
      Character*256 vname
      Character*256 ctname
      Character*40 machine
      Integer asize(3)
      Real vsize(3)
      Real offset(3)
      Integer i,j,k
      Integer error
      Integer*2 ctdata(512,512,270)
      Integer*2 dummy
      Integer byteorder
      Integer maxctnum
      Integer maxcount
      Integer outofrange
      Real ReadReal,ReadInt
      Integer lnblnk1
      write(6,1170)
      write(1,1170)
1170  format('==========================================================
     *===')
      write(6,1180)
      write(1,1180)
1180  format(' CT Phantom has been chosen and reading  ')
      write(6,1190)
      write(1,1190)
1190  format(' headerfile information.                ')
      vname='x_dim'
      asize(1)=Int(ReadInt(fname,vname))
      vname='y_dim'
      asize(2)=Int(ReadInt(fname,vname))
      vname='z_dim'
      asize(3)=Int(ReadInt(fname,vname))
      vname='x_pixdim'
      vsize(1)=ReadReal(fname,vname)
      vname='y_pixdim'
      vsize(2)=ReadReal(fname,vname)
      vname='z_pixdim'
      vsize(3)=ReadReal(fname,vname)
      vname='x_start'
      offset(1)=ReadReal(fname,vname)
      vname='y_start'
      offset(2)=ReadReal(fname,vname)
      vname='z_start'
      offset(3)=ReadReal(fname,vname)
      write(6,1200)fname(:lnblnk1(fname))
      write(1,1200)fname(:lnblnk1(fname))
1200  format(' Header File Name ->   ',A)
      write(6,1210)asize(1)
      write(1,1210)asize(1)
1210  format(' X Pixel Number   ->   ',I4)
      write(6,1220)asize(2)
      write(1,1220)asize(2)
1220  format(' Y Pixel Number   ->   ',I4)
      write(6,1230)asize(3)
      write(1,1230)asize(3)
1230  format(' Z Pixel Number   ->   ',I4)
      write(6,1240)vsize(1)
      write(1,1240)vsize(1)
1240  format(' X Pixel Size     ->   ',F8.3,' (cm)')
      write(6,1250)vsize(2)
      write(1,1250)vsize(2)
1250  format(' Y Pixel Size     ->   ',F8.3,' (cm)')
      write(6,1260)vsize(3)
      write(1,1260)vsize(3)
1260  format(' Z Pixel Size     ->   ',F8.3,' (cm)')
      write(6,1270)offset(1)
      write(1,1270)offset(1)
1270  format(' X Pixel Offset   ->   ',F8.3,' (cm)')
      write(6,1280)offset(2)
      write(1,1280)offset(2)
1280  format(' Y Pixel Offset   ->   ',F8.3,' (cm)')
      write(6,1290)offset(3)
      write(1,1290)offset(3)
1290  format(' Z Pixel Offset   ->   ',F8.3,' (cm)')
      maxctnum=2000
      IF ((asize(1).GT.512 .OR. asize(2).GT.512 .OR. asize(3).GT.270 ))
     *THEN
          write(6,1300)
          write(1,1300)
1300      format(' ERROR -> The CT volume is larger than  ')
          write(6,1310)
          write(1,1310)
1310      format('the compiled code can handle. Please   ')
          write(6,1320)
          write(1,1320)
1320      format('adjust the array sizes and try again.')
          write(6,1330)
          write(1,1330)
1330      format('  Thanks! ')
          call exit(1)
      END IF
      write(6,1340)offset(1),offset(1)+asize(1)*vsize(1), offset(2),offs
     *et(2)+asize(2)*vsize(2), offset(3),offset(3)+asize(3)*vsize(3)
      write(1,1340)offset(1),offset(1)+asize(1)*vsize(1), offset(2),offs
     *et(2)+asize(2)*vsize(2), offset(3),offset(3)+asize(3)*vsize(3)
1340  format(/' CT data goes from  x = ',F12.5,' - ',F12.5,' cm'/ '     
     *          y = ',F12.5,' - ',F12.5,' cm'/ ' z = ',F12.5,' - ',F12.5
     *,' cm'/)
      write(6,1350)
      write(1,1350)
1350  format('--------------------------------------------')
      ctname=fname(:index(fname,'header')-1) // 'img'
      write(6,1360)
      write(1,1360)
1360  format(' Reading in the CT data from the image file.')
      write(6,1370)ctname(:lnblnk1(ctname))
      write(1,1370)ctname(:lnblnk1(ctname))
1370  format(A)
      call read_ct_data(ctdata,asize(1),asize(2),asize(3),ctname)
      write(6,1380)
      write(1,1380)
1380  format(' Done reading in the image file data.')
      vname='byte_order'
      byteorder=Int(ReadInt(fname,vname))
      write(6,1390)
      write(1,1390)
1390  format('--------------------------------------------')
      write(6,1400)
      write(1,1400)
1400  format(' Determining if data needs to be byte       ')
      write(6,1410)
      write(1,1410)
1410  format(' swapped.')
      IF ((vname.eq.'Error')) THEN
          write(6,1420)
          write(1,1420)
1420      format(' No byte order given in header file.        ')
          write(6,1430)
          write(1,1430)
1430      format(' Will compare Hounsfield data against the')
          write(6,1440)
          write(1,1440)
1440      format(' Hounsfield maximum value - 2000. ')
          outofrange=0
          DO 1451 i=1,asize(1)
              DO 1461 j=1,asize(2)
                  DO 1471 k=1,asize(3)
                      IF ((maxctnum.lt.ctdata(i,j,k))) THEN
                          outofrange=1
                          write(6,1480)
                          write(1,1480)
1480                      format(' Data values excede the maximum indica
     *ted in')
                          write(6,1490)
                          write(1,1490)
1490                      format(' header file.')
                          goto 1007
                      END IF
1471              CONTINUE
1472              CONTINUE
1461          CONTINUE
1462          CONTINUE
1451      CONTINUE
1452      CONTINUE
1007      Continue
      ELSE
          IF (( ( byteorder .EQ. 0 .AND. '1234' .EQ. '1234' ) .OR. ( byt
     *    eorder .EQ. 1 .AND. '1234' .EQ. '4321' ) )) THEN
              outofrange=0
              write(6,1500)byteorder,'1234'
              write(1,1500)byteorder,'1234'
1500          format(' Byte order of file (',i1, ') is same as byte orde
     *r of this machine ',a4)
          ELSE
              write(6,1510)byteorder,'1234'
              write(1,1510)byteorder,'1234'
1510          format(' Byte order of file (',i1, ') is different from by
     *te order of this machine ',a4)
              write(6,1520)
              write(1,1520)
1520          format('---> will swap bytes')
              outofrange=1
          END IF
      END IF
      IF ((outofrange.ne.0)) THEN
          write(6,1530)
          write(1,1530)
1530      format('============================================')
          write(6,1540)
          write(1,1540)
1540      format(' Swapping the bytes in the CT data set.')
          DO 1551 i=1,asize(1)
              DO 1561 j=1,asize(2)
                  DO 1571 k=1,asize(3)
                      dummy=ctdata(i,j,k)
                      Call swapbytes2(dummy)
                      ctdata(i,j,k)=dummy
1571              CONTINUE
1572              CONTINUE
1561          CONTINUE
1562          CONTINUE
1551      CONTINUE
1552      CONTINUE
      ELSE
          write(6,1580)
          write(1,1580)
1580      format(' No Byte swap necessary, continuing.        ')
      END IF
      maxcount=0
      DO 1591 i=1,asize(1)
          DO 1601 j=1,asize(2)
              DO 1611 k=1,asize(3)
                  IF ((ctdata(i,j,k).GT.maxctnum)) THEN
                      maxcount=maxcount+1
                  END IF
1611          CONTINUE
1612          CONTINUE
1601      CONTINUE
1602      CONTINUE
1591  CONTINUE
1592  CONTINUE
      IF ((maxcount.GT.0)) THEN
          write(6,1620)maxcount,maxctnum
          write(1,1620)maxcount,maxctnum
1620      format(' Warning: ',I10,' pixels had Pinnacle CT no. > the max
     *imum value of ',I10/ ' supported by Pinnacle format (or indicatedi
     *n header).'/)
      END IF
      write(6,1630)
      write(1,1630)
1630  format('============================================')
      END
      Function ReadReal(filename,vname)
      IMPLICIT NONE
      Character*256 filename
      Character*256 vname
      Character*256 dummy
      Real readreal
      Integer istat
      Integer iindex
      Integer ivname
      Integer idummy
      Integer lnblnk1
      Open(45,file=filename,status='old',access='sequential')
1000  Read(45,IOSTAT=istat,ERR=1001,FMT='(A256)') dummy
      IF ((dummy(:1).eq.'!')) THEN
          goto 1000
      END IF
      dummy=dummy(:lnblnk1(dummy)-1)
      ivname=lnblnk1(vname)
      idummy=lnblnk1(dummy)
      iindex=Index(dummy(:idummy),vname(:ivname))
      IF ((iindex.EQ.0)) THEN
          goto 1000
      END IF
      CLOSE(45,status='keep')
      iindex=iindex+ivname+1
      dummy=dummy(iindex+1:)
      IF ((Index(dummy,'.EQ.').NE.0)) THEN
          iindex=Index(dummy,'=')+1
          dummy=dummy(iindex+1:)
      END IF
      IF ((Index(dummy,':').NE.0)) THEN
          iindex=Index(dummy,':')+1
          dummy=dummy(iindex+1:)
      END IF
      Read(dummy,fmt='(f10.3)') readreal
      vname='Okay'
      Return
1001  CONTINUE
      write(6,1640)
      write(1,1640)
1640  format(' An error occurred while searching for :')
      write(6,1650)vname
      write(1,1650)vname
1650  format(' > ',A)
      write(6,1660)
      write(1,1660)
1660  format(' in:')
      write(6,1670)filename
      write(1,1670)filename
1670  format(' > ',A,'.')
      write(6,1680)
      write(1,1680)
1680  format(' Probably End Of File.')
      IF ((istat.gt.0)) THEN
          goto 1000
      END IF
      ReadReal=0.00
      CLOSE(45,status='keep')
      vname='Error'
      return
      End
      Function ReadInt(filename,vname)
      IMPLICIT NONE
      Character*256 filename
      Character*256 vname
      Character*256 dummy
      Real readint
      Integer istat
      Integer iindex
      Integer ivname
      Integer idummy
      Integer lnblnk1
      Open(45,file=filename,status='old',access='sequential')
1000  CONTINUE
      Read(45,IOSTAT=istat,ERR=1001,FMT='(A256)') dummy
      IF ((dummy(:1).eq.'!')) THEN
          goto 1000
      END IF
      dummy=dummy(:lnblnk1(dummy)-1)
      ivname=lnblnk1(vname)
      idummy=lnblnk1(dummy)
      iindex=Index(dummy(:idummy),vname(:ivname))
      IF ((iindex.EQ.0)) THEN
          goto 1000
      END IF
      CLOSE(45,status='keep')
      iindex=iindex+ivname+1
      dummy=dummy(iindex+1:)
      IF ((Index(dummy,'.EQ.').NE.0)) THEN
          iindex=Index(dummy,'=')+1
          dummy=dummy(iindex+1:)
      END IF
      IF ((Index(dummy,':').NE.0)) THEN
          iindex=Index(dummy,':')+1
          dummy=dummy(iindex+1:)
      END IF
      Read(dummy,fmt='(i10)') idummy
      ReadInt=idummy
      vname='Okay'
      Return
1001  Continue
      write(6,1690)
      write(1,1690)
1690  format(' An error occurred while searching for ')
      dummy=vname(:lnblnk1(vname))
      write(6,1700)dummy
      write(1,1700)dummy
1700  format(' > ',A)
      write(6,1710)
      write(1,1710)
1710  format(' in:')
      dummy=filename(:lnblnk1(filename))
      write(6,1720)dummy
      write(1,1720)dummy
1720  format(' > ',A,'.')
      write(6,1730)
      write(1,1730)
1730  format(' Probably End Of File.')
      IF ((istat.gt.0)) THEN
          goto 1000
      END IF
      ReadInt=0.00
      Close(45,status='keep')
      vname='Error'
      return
      End
      Subroutine ReadChar(filename,vname,result)
      IMPLICIT NONE
      Character*256 filename
      Character*256 vname
      Character*256 dummy
      Character*256 result
      Integer istat
      Integer iindex
      Integer ivname
      Integer idummy
      Integer lnblnk1
      Open(45,file=filename,status='old',access='sequential')
1000  CONTINUE
      Read(45,IOSTAT=istat,Err=1001,FMT='(A256)') dummy
      IF ((dummy(:1).eq.'!')) THEN
          goto 1000
      END IF
      ivname=lnblnk1(vname)
      idummy=lnblnk1(dummy)
      iindex=Index(dummy(:idummy),vname(:ivname))
      IF ((iindex.EQ.0)) THEN
          goto 1000
      END IF
      Close(45,status='keep')
      iindex=iindex+ivname+1
      IF ((Index(dummy(iindex:),'.EQ.').NE.0)) THEN
          iindex=iindex+Index(dummy(iindex:),'=')+1
      END IF
      Read(dummy(iindex:),fmt='(A256)') result
      vname='Okay'
      RETURN
1001  CONTINUE
      write(6,1740)
      write(1,1740)
1740  format(' An error occurred while searching for:')
      write(6,1750)vname
      write(1,1750)vname
1750  format(' > ',A)
      write(6,1760)
      write(1,1760)
1760  format(' in:' )
      write(6,1770)filename
      write(1,1770)filename
1770  format(' > ',A,'.')
      write(6,1780)
      write(1,1780)
1780  format(' Probaly End Of File.')
      IF ((istat.gt.0)) THEN
          goto 1000
      END IF
      result='0'
      Close(45,status='keep')
      vname='Error'
      return
      End
      Subroutine swapbytes2(i)
      IMPLICIT NONE
      Integer*2 i,ii
      Integer dummy1,dummy2
      ii=0
      dummy1=i
      dummy2=ii
      call mvbits(dummy1, 0, 8, dummy2, 8)
      call mvbits(dummy1, 8, 8, dummy2, 0)
      i=dummy2
      End
      subroutine read_ct_data(ctdata,imax,jmax,kmax,ctname)
      IMPLICIT NONE
      Integer*2 ctdata(512,512,270)
      Integer imax
      Integer jmax
      Integer kmax
      Integer i,j,k,irl
      Character*256 ctname
      integer*2 i2_array(imax)
      integer rec_length
      rec_length = 4*imax/2
      open(45,file=ctname,Status='old',Form='unformatted', Access='direc
     *t',recl=rec_length)
      DO 1791 j=1,jmax
          DO 1801 k=1,kmax
              irl = j + (k-1)*jmax
              read(45,rec=irl) i2_array
              DO 1811 i=1,imax
                  ctdata(i,j,k) = i2_array(i)
1811          CONTINUE
1812          CONTINUE
1801      CONTINUE
1802      CONTINUE
1791  CONTINUE
1792  CONTINUE
      close(45)
      End
      Subroutine ReadCT_CADPLAN(fname,asize,ctdata,offset,vsize,error)
      IMPLICIT NONE
      Character*256 fname
      Character*80 Filename1
      Integer asize(3)
      Real vsize(3)
      Real oldvsize3
      Real offset(3)
      Real zpos_prev
      Integer error
      Integer ios
      Integer*2 ctdata(512,512,270)
      INTEGER Date
      INTEGER*2 Year,Month,Day
      REAL*4 REX,REY,REZ
      CHARACTER SEQNO*12,SCDATE*12,CTTYP*40,HSPNAM*40,PATID*16,PATNAM*40
      CHARACTER CMNTS1*40,CMNTS2*40,CMNTS3*40,CMNTS4*40
      BYTE Wert(512)
      INTEGER*2 Wert1(512),Hex1,Hex2
      Integer*2 Zahl1,Zahl2,Zahl3,Zahl4
      INTEGER*4 DEZ(512)
      INTEGER IT, Bytes, Snummer, J, I, K
      asize(3)=0
      OPEN(45,file=fname,status='old',access='sequential')
1821  CONTINUE
          IT=0
          J=0
          I=0
          K=0
          Bytes=140
          Snummer=1
          READ(45,END=1830,FMT='(A80)') Filename1
          write(6,1840)Filename1
          write(1,1840)Filename1
1840      format(/' Working on file '/ ' : ',A80)
          OPEN (45+1,FILE=Filename1,STATUS='OLD',ACCESS='DIRECT', FORM='
     *UNFORMATTED',RECL=4*128)
          READ(45+1,REC=Snummer) (Wert(IT),IT=1,Bytes),SEQNO, SCDATE,CTT
     *    YP, HSPNAM,PATID,PATNAM,CMNTS1,CMNTS2,CMNTS3,CMNTS4,REX,REY, W
     *    ert(141),Wert(142),Wert(143),Wert(144),Wert(145),Wert(146), We
     *    rt(147), Wert(148),REZ
          asize(3)=asize(3)+1
          IF ((asize(3).EQ.1)) THEN
              write(6,1850)
              write(1,1850)
1850          format(//5X, 'Output of basic header information!'/)
              DO 1861 IT=1,Bytes+8
                  IF ((Wert(IT).LT.0)) THEN
                      Wert1(IT)=Wert(IT)
                      IF ((MOD(IT,2).EQ.0)) THEN
                          Wert1(IT)=Wert1(IT)+256
                      END IF
                  ELSE
                      Wert1(IT)=Wert(IT)
                  END IF
1861          CONTINUE
1862          CONTINUE
              DO 1871 IT=1,Bytes+8
                  K=(IT+1)/2
                  IF ((MOD(IT,2).NE.0)) THEN
                      Zahl1=Wert1(IT)
                  ELSE
                      Zahl2=Wert1(IT)
                  END IF
                  IF ((MOD(IT,2).EQ.0)) THEN
                      DEZ(K)=Zahl1*256+Zahl2
                  END IF
                  IF ((Wert(IT).NE.0)) THEN
                      J=J+1
                  END IF
1871          CONTINUE
1872          CONTINUE
              IF ((J.EQ.0)) THEN
                  write(6,1880)Filename1
                  write(1,1880)Filename1
1880              format(//' Found only zeroes in header of file '/A80//
     *)
              END IF
              Date=DEZ(5)
              Year=Date/512+72
              Month=(Date/512.-(Year-72))*512./32.
              Day=Date-(Year-72)*512-Month*32
              write(6,1890)DEZ(2),DEZ(3),DEZ(4),Month,Day,Year, DEZ(6),D
     *        EZ(7),DEZ(8),DEZ(9),DEZ(10),DEZ(11),SCDATE, HSPNAM,PATID,P
     *        ATNAM,CMNTS1,CMNTS2,CMNTS3,CMNTS4,REX, REY,DEZ(70),DEZ(71)
     *        ,DEZ(72),DEZ(73),REZ
              write(1,1890)DEZ(2),DEZ(3),DEZ(4),Month,Day,Year, DEZ(6),D
     *        EZ(7),DEZ(8),DEZ(9),DEZ(10),DEZ(11),SCDATE, HSPNAM,PATID,P
     *        ATNAM,CMNTS1,CMNTS2,CMNTS3,CMNTS4,REX, REY,DEZ(70),DEZ(71)
     *        ,DEZ(72),DEZ(73),REZ
1890          format(//'Number of slices:',T50,I4, / 'Total number of bl
     *ocks:',T50,I4, / 'First data-block:',T50,I4, / 'File creation date
     *:',T46,I2,'-',I2,'-',I2, / 'Type of diagnostic device' / '(1000-19
     *99 = X-ray CT)',T50,I4, / 'Relative position of slice [mm]:',T50,I
     *6, / 'Size of pixel-matrix in X:',T50,I4, / 'Size of pixel-matrix 
     *in Y:',T50,I4, / 'Pixelsize (x) [um]:',T50,I5, / 'Pixelsize (y) [u
     *m]:',T50,I5, ///'Date of the scan (usergiven string):',T50,A12, / 
     *'Name of the hospital:',T50,A40, / 'Patient-ID:',T50,A16, / 'Patie
     *ntname:',T50,A40, / 'Free text:',T50,A40,/ 'Free text:',T50,A40, /
     * 'Free text:',T50,A40, / 'Free text:',T50,A40, / 'x-coordinate for
     * sagittal slice (cm):',T60,F7.3,/ 'y-coordinate for coronal slice 
     *(cm):',T60,F7.3, / 'x-coordinate for sagittal slice (pixel):',T60,
     *I4, / 'y-coordinate forcoronal slice (pixel):',T60,I4, / '1, if RE
     *Z has valid data:',T60,I4, / 'Free Int:',T60,I4, / 'z-coordinate (
     *cm) of the central pixel row in frontal', / 'and sagital slice (RE
     *Z):',T60,F7.3,/)
              zpos_prev=DEZ(7)/10.
              asize(1)=DEZ(8)
              asize(2)=DEZ(9)
              vsize(1)=DEZ(10)/10000.
              vsize(2)=DEZ(11)/10000.
              offset(1)=-asize(1)*vsize(1)/2
              offset(2)=-asize(2)*vsize(2)/2
              offset(3)=DEZ(7)/10.
          ELSE
              DO 1901 IT=13,14
                  IF ((Wert(IT).LT.0)) THEN
                      Wert1(IT)=Wert(IT)
                      IF ((MOD(IT,2).EQ.0)) THEN
                          Wert1(IT)=Wert1(IT)+256
                      END IF
                  ELSE
                      Wert1(IT)=Wert(IT)
                  END IF
1901          CONTINUE
1902          CONTINUE
              DO 1911 IT=13,14
                  K=(IT+1)/2
                  IF ((MOD(IT,2).NE.0)) THEN
                      Zahl1=Wert1(IT)
                  ELSE
                      Zahl2=Wert1(IT)
                  END IF
                  IF ((MOD(IT,2).EQ.0)) THEN
                      DEZ(K)=Zahl1*256+Zahl2
                  END IF
1911          CONTINUE
1912          CONTINUE
              write(6,1920)DEZ(7)
              write(1,1920)DEZ(7)
1920          format(/' Relative position of slice [mm]:',T50,I12)
              oldvsize3=vsize(3)
              vsize(3)=DEZ(7)/10.-zpos_prev
              zpos_prev=DEZ(7)/10.
              WRITE(*,*)
              IF ((asize(3).GT.2 .AND. vsize(3).NE.oldvsize3)) THEN
                  write(6,1930)
                  write(1,1930)
1930              format(//' ***WARNING '/ ' The slices in this CT dataa
     *re not evenly spaced.'/ ' Output phantom will have errors.'//)
              END IF
          END IF
          IT=0
          J=0
          I=0
          K=0
          Bytes=512
          Snummer=14
          DO 1941 Snummer=14,269
              READ(45+1,REC=Snummer,ERR=1950, IOSTAT=ios) (Wert(IT),IT=1
     *        ,Bytes)
1950          IF ((ios .GT. 0)) THEN
                  write(6,1960)
                  write(1,1960)
1960              format(//' ***WARNING '/ ' An error occurred during re
     *ading of CT data.'/ ' If this happens repeatedly, check the CT fil
     *es.'//)
              END IF
              DO 1971 IT=1,Bytes
                  IF ((Wert(IT).LT.0)) THEN
                      Wert1(IT)=Wert(IT)
                      Wert1(IT)=Wert1(IT)+256
                  ELSE
                      Wert1(IT)=Wert(IT)
                  END IF
1971          CONTINUE
1972          CONTINUE
              DO 1981 IT=1,Bytes
                  K=(IT+1)/2
                  Hex1=Wert1(IT)/16
                  Hex2=MOD(Wert1(IT),16)
                  IF ((MOD(IT,2).NE.0)) THEN
                      Zahl1=Hex1
                      Zahl2=Hex2
                  ELSE
                      Zahl3=Hex1
                      Zahl4=Hex2
                  END IF
                  IF ((MOD(IT,2).EQ.0)) THEN
                      ctdata(K,Snummer-13,asize(3))= Zahl1*4096+Zahl2*25
     *                6+Zahl3*16+Zahl4
                      IF ((ctdata(K,Snummer-13,asize(3)).NE.0)) THEN
                          J=J+1
                      END IF
                  END IF
1981          CONTINUE
1982          CONTINUE
1941      CONTINUE
1942      CONTINUE
          IF ((J.EQ.0)) THEN
              write(6,1990)
              write(1,1990)
1990          format(///1X, 'Found only zeros in CT data!!!'///)
          END IF
          CLOSE(45+1)
      GO TO 1821
1822  CONTINUE
1830  write(6,2000)fname
      write(1,2000)fname
2000  format(/' Finished reading all files from '/ ' : ',A80)
      CLOSE(45)
      write(6,2010)offset(1),offset(1)+asize(1)*vsize(1), offset(2),offs
     *et(2)+asize(2)*vsize(2), offset(3),offset(3)+asize(3)*vsize(3), as
     *ize(1),asize(2),asize(3),vsize(1), vsize(2),vsize(3)
      write(1,2010)offset(1),offset(1)+asize(1)*vsize(1), offset(2),offs
     *et(2)+asize(2)*vsize(2), offset(3),offset(3)+asize(3)*vsize(3), as
     *ize(1),asize(2),asize(3),vsize(1), vsize(2),vsize(3)
2010  format(//' Summary of CADPLAN CT data as read in:'// '  X range :'
     *,F12.5,' - ',F12.5,' cm'/ '  Y range : ',F12.5,' - ',F12.5,' cm'/'
     *  Z range : ',F12.5,' - ',F12.5,' cm'/ ' '/ '  X dimension = ',I12
     */ '  Y dimension = ',I12/ '  Z dimension = ',I12/ ' '/ '  X voxel 
     *size = ',F12.5,' cm'/ '  Y voxel size = ',F12.5,' cm'/ '  Z voxel 
     *size = ',F12.5,' cm')
      RETURN
      END
      subroutine SubsizeCT(asize,ctdata,vsize,offset)
      IMPLICIT NONE
      Integer asize(3)
      Real vsize(3)
      Real offset(3)
      Integer*2 ctdata(512,512,270)
      Integer i,j,k,ii,jj,kk
      Integer imax,jmax,kmax
      Real xctsubmin,  xctsubmax, yctsubmin, yctsubmax, zctsubmin, zctsu
     *bmax
      Integer ictsubmin, ictsubmax, jctsubmin, jctsubmax, kctsubmin, kct
     *submax
      Real xbounds(512+1), ybounds(512+1), zbounds(270+1)
      write(6,2020)
      write(1,2020)
2020  format('--------------------------------------------')
      write(6,2030)
      write(1,2030)
2030  format('CT Volume subset selection.')
      write(6,2040)
      write(1,2040)
2040  format('Please enter the positions of limiting ')
      write(6,2050)
      write(1,2050)
2050  format('planes (cm):')
      write(6,2060)
      write(1,2060)
2060  format('  xctsubmin,xctsubmax,yctsubmin,yctsubmax,zctsubmin,zctsub
     *max')
      read(5,*) xctsubmin,xctsubmax, yctsubmin,yctsubmax, zctsubmin,zcts
     *ubmax
      write(6,2070)xctsubmin,xctsubmax,yctsubmin,yctsubmax,zctsubmin,zct
     *submax
      write(1,2070)xctsubmin,xctsubmax,yctsubmin,yctsubmax,zctsubmin,zct
     *submax
2070  format(' : ',6F10.4)
      imax=asize(1)
      jmax=asize(2)
      kmax=asize(3)
      xbounds(1)=offset(1)
      DO 2081 i=2,imax+1
          xbounds(i)=xbounds(i-1)+vsize(1)
2081  CONTINUE
2082  CONTINUE
      ybounds(1)=offset(2)
      DO 2091 i=2,jmax+1
          ybounds(i)=ybounds(i-1)+vsize(2)
2091  CONTINUE
2092  CONTINUE
      zbounds(1)=offset(3)
      DO 2101 i=2,kmax+1
          zbounds(i)=zbounds(i-1)+vsize(3)
2101  CONTINUE
2102  CONTINUE
      IF ((xctsubmin.EQ.0.0 .AND. xctsubmax.EQ.0.0 .AND. yctsubmin.EQ.0.
     *0 .AND. yctsubmax .EQ. 0.0 .AND. zctsubmin.EQ.0.0 .AND. zctsubmax.
     *EQ.0.0)) THEN
          write(6,2110)
          write(1,2110)
2110      format(' No subset requested, will use entire CT volume.')
          write(6,2120)
          write(1,2120)
2120      format('--------------------------------------------')
          xctsubmin=xbounds(1)
          xctsubmax=xbounds(imax+1)
          yctsubmin=ybounds(1)
          yctsubmax=ybounds(jmax+1)
          zctsubmin=zbounds(1)
          zctsubmax=zbounds(kmax+1)
      END IF
      IF (((xctsubmin.LT.xbounds(1) .AND. xctsubmax.LT.xbounds(1)) .OR.
     *(xctsubmax.GT.xbounds(imax+1) .AND. xctsubmin.GT.xbounds(imax+1)))
     *) THEN
          write(6,2130)
          write(1,2130)
2130      format(' ***WARNING: X range does not intersect the original')
          write(6,2140)
          write(1,2140)
2140      format(' CT data.  Will use entire X range of original data.')
          write(6,2150)
          write(1,2150)
2150      format('----------------------------------------------')
          xctsubmin=xbounds(1)
          xctsubmax=xbounds(imax+1)
      END IF
      IF (((yctsubmin.LT.ybounds(1) .AND. yctsubmax.LT.ybounds(1)) .OR.
     *(yctsubmax.GT.ybounds(jmax+1) .AND. yctsubmin.GT.ybounds(jmax+1)))
     *) THEN
          write(6,2160)
          write(1,2160)
2160      format(' ***WARNING: Y range does not intersect the original')
          write(6,2170)
          write(1,2170)
2170      format(' CT data.  Will use entire Y range of original data.')
          write(6,2180)
          write(1,2180)
2180      format('----------------------------------------------')
          yctsubmin=ybounds(1)
          yctsubmax=ybounds(jmax+1)
      END IF
      IF (((zctsubmin.LT.zbounds(1) .AND. zctsubmax.LT.zbounds(1)) .OR.
     *(zctsubmax.GT.zbounds(kmax+1) .AND. zctsubmin.GT.zbounds(kmax+1)))
     *) THEN
          write(6,2190)
          write(1,2190)
2190      format(' ***WARNING: Z range does not intersect the original')
          write(6,2200)
          write(1,2200)
2200      format(' CT data.  Will use entire Z range of original data.')
          write(6,2210)
          write(1,2210)
2210      format('----------------------------------------------')
          zctsubmin=zbounds(1)
          zctsubmax=zbounds(kmax+1)
      END IF
      IF ((xctsubmax .LE. xctsubmin)) THEN
          write(6,2220)
          write(1,2220)
2220      format('***WARNING: X upper limit is <=  X lower limit')
          write(6,2230)
          write(1,2230)
2230      format(' will use entire X range.                     ')
          write(6,2240)
          write(1,2240)
2240      format('----------------------------------------------')
          xctsubmin=xbounds(1)
          xctsubmax=xbounds(imax+1)
      END IF
      IF ((yctsubmax .LE. yctsubmin)) THEN
          write(6,2250)
          write(1,2250)
2250      format('***WARNING: Y upper limit is <=  Y lower limit')
          write(6,2260)
          write(1,2260)
2260      format(' will use entire Y range.                     ')
          write(6,2270)
          write(1,2270)
2270      format('----------------------------------------------')
          yctsubmin=ybounds(1)
          yctsubmax=ybounds(jmax+1)
      END IF
      IF ((zctsubmax .LE. zctsubmin)) THEN
          write(6,2280)
          write(1,2280)
2280      format('***WARNING: Z upper limit is <=  Z lower limit')
          write(6,2290)
          write(1,2290)
2290      format(' will use entire Z range.                     ')
          write(6,2300)
          write(1,2300)
2300      format('----------------------------------------------')
          zctsubmin=zbounds(1)
          zctsubmax=zbounds(kmax+1)
      END IF
      IF ((xctsubmin.LT.xbounds(1))) THEN
          write(6,2310)
          write(1,2310)
2310      format('X lower limit out of bounds, will be set to ')
          write(6,2320)
          write(1,2320)
2320      format(' lower bound.                               ')
          write(6,2330)
          write(1,2330)
2330      format('--------------------------------------------')
          xctsubmin=xbounds(1)
      END IF
      IF ((xctsubmax.GT.xbounds(imax+1))) THEN
          write(6,2340)
          write(1,2340)
2340      format('X upper limit out of bounds, will be set to ')
          write(6,2350)
          write(1,2350)
2350      format(' upper bound.                                ')
          write(6,2360)
          write(1,2360)
2360      format('--------------------------------------------')
          xctsubmax=xbounds(imax+1)
      END IF
      IF ((yctsubmin.LT.ybounds(1))) THEN
          write(6,2370)
          write(1,2370)
2370      format('Y lower limit out of bounds, will be set to ')
          write(6,2380)
          write(1,2380)
2380      format(' lower bound.                               ')
          write(6,2390)
          write(1,2390)
2390      format('--------------------------------------------')
          yctsubmin=ybounds(1)
      END IF
      IF ((yctsubmax.GT.ybounds(jmax+1))) THEN
          write(6,2400)
          write(1,2400)
2400      format('Y upper limit out of bounds, will be set to ')
          write(6,2410)
          write(1,2410)
2410      format(' upper bound.                               ')
          write(6,2420)
          write(1,2420)
2420      format('--------------------------------------------')
          yctsubmax=ybounds(jmax+1)
      END IF
      IF ((zctsubmin.LT.zbounds(1))) THEN
          write(6,2430)
          write(1,2430)
2430      format('Z lower limit out of bounds, will be set to ')
          write(6,2440)
          write(1,2440)
2440      format(' lower bound.                               ')
          write(6,2450)
          write(1,2450)
2450      format('--------------------------------------------')
          zctsubmin=zbounds(1)
      END IF
      IF ((zctsubmax.GT.zbounds(kmax+1))) THEN
          write(6,2460)
          write(1,2460)
2460      format('Z upper limit out of bounds, will be set to ')
          write(6,2470)
          write(1,2470)
2470      format(' upper bound.                               ')
          write(6,2480)
          write(1,2480)
2480      format('--------------------------------------------')
          zctsubmax=zbounds(kmax+1)
      END IF
      write(6,2490)
      write(1,2490)
2490  format('The voxel index limits are as follows:')
      DO 2501 i=1,imax
          IF ((xbounds(i).LE.xctsubmin .AND. xbounds(i+1).GT.xctsubmin))
     *     THEN
              ictsubmin=i
          END IF
          IF ((xbounds(i).LE.xctsubmax .AND. xbounds(i+1).GE.xctsubmax))
     *     THEN
              ictsubmax=i
          END IF
2501  CONTINUE
2502  CONTINUE
      write(6,2510)ictsubmin,ictsubmax
      write(1,2510)ictsubmin,ictsubmax
2510  format('I Limits -> i=',I6,' to i=',I6)
      DO 2521 j=1,jmax
          IF ((ybounds(j).LE.yctsubmin .AND. ybounds(j+1).GT.yctsubmin))
     *     THEN
              jctsubmin=j
          END IF
          IF ((ybounds(j).LE.yctsubmax .AND. ybounds(j+1).GE.yctsubmax))
     *     THEN
              jctsubmax=j
          END IF
2521  CONTINUE
2522  CONTINUE
      write(6,2530)jctsubmin,jctsubmax
      write(1,2530)jctsubmin,jctsubmax
2530  format('J Limits -> j=',I6,' to j=',I6)
      DO 2541 k=1,kmax
          IF ((zbounds(k).LE.zctsubmin .AND. zbounds(k+1).GT.zctsubmin))
     *     THEN
              kctsubmin=k
          END IF
          IF ((zbounds(k).LE.zctsubmax .AND. zbounds(k+1).GE.zctsubmax))
     *     THEN
              kctsubmax=k
          END IF
2541  CONTINUE
2542  CONTINUE
      IF ((zbounds(kmax).EQ.zctsubmax )) THEN
          kctsubmax=kmax
      END IF
      write(6,2550)kctsubmin,kctsubmax
      write(1,2550)kctsubmin,kctsubmax
2550  format('K Limits -> k=',I6,' to k=',I6)
      xctsubmin=xbounds(ictsubmin)
      xctsubmax=xbounds(ictsubmax+1)
      yctsubmin=ybounds(jctsubmin)
      yctsubmax=ybounds(jctsubmax+1)
      zctsubmin=zbounds(kctsubmin)
      zctsubmax=zbounds(kctsubmax+1)
      write(6,2560)xctsubmin,xctsubmax,yctsubmin,yctsubmax,zctsubmin,zct
     *submax
      write(1,2560)xctsubmin,xctsubmax,yctsubmin,yctsubmax,zctsubmin,zct
     *submax
2560  format(' xctsubmin,xctsubmax,yctsubmin,yctsubmax,zctsubmin,zctsubm
     *ax (cm)'/ ' after adjustment to fit integer no. of voxels '/ ' : '
     *,6F10.4)
      ii=1
      jj=1
      kk=1
      DO 2571 k=kctsubmin,kctsubmax
          DO 2581 j=jctsubmin,jctsubmax
              DO 2591 i=ictsubmin,ictsubmax
                  ctdata(ii,jj,kk)=ctdata(i,j,k)
                  ii=ii+1
2591          CONTINUE
2592          CONTINUE
              ii=1
              jj=jj+1
2581      CONTINUE
2582      CONTINUE
          jj=1
          kk=kk+1
2571  CONTINUE
2572  CONTINUE
      offset(1)=offset(1)+vsize(1)*(ictsubmin-1)
      offset(2)=offset(2)+vsize(2)*(jctsubmin-1)
      offset(3)=offset(3)+vsize(3)*(kctsubmin-1)
      asize(1)=ictsubmax-ictsubmin+1
      asize(2)=jctsubmax-jctsubmin+1
      asize(3)=kctsubmax-kctsubmin+1
      write(6,2600)
      write(1,2600)
2600  format('============================================')
      END
      subroutine ResampleCT(ct_imax,ct_jmax,ct_kmax, ct_xthickness,ct_yt
     *hickness,ct_zthickness, ct_data, xyz_imax,xyz_jmax,xyz_kmax, xyz_x
     *bounds,xyz_ybounds,xyz_zbounds, xyz_ct, CTOffset)
      IMPLICIT NONE
      INTEGER ct_imax,  ct_jmax,  ct_kmax,  xyz_imax,  xyz_jmax,  xyz_km
     *ax,  i_lower_xyz,  i_upper_xyz,  j_lower_xyz,  j_upper_xyz,  k_low
     *er_xyz,  k_upper_xyz,  param,  i_ct,j_ct,k_ct,i_xyz,j_xyz,k_xyz,i,
     *j,k
      INTEGER*2 ct_data(512,512,270),  xyz_ct(128,128,128)
      REAL xyz_xbounds(128+1),  xyz_ybounds(128+1),  xyz_zbounds(128+1),
     *  ct_xthickness,  ct_ythickness,  ct_zthickness,      xyz_xthickne
     *ss,  xyz_ythickness,  xyz_zthickness,  weight_xyz(512+512+270+2),
     *   CTOffset(3),  realxyz_ct(128,128,128)
      CHARACTER*1 iorjork
      write(6,2610)
      write(1,2610)
2610  format(/' Resample CT data for dosxyznrc '/ ' --------------------
     *------- ')
2620  write(6,2630)ct_imax*ct_xthickness/128,ct_jmax*ct_ythickness/128,
     *ct_kmax*ct_zthickness/128
      write(1,2630)ct_imax*ct_xthickness/128,ct_jmax*ct_ythickness/128,
     *ct_kmax*ct_zthickness/128
2630  format(/' Input the x,y,z dimensions (cm) of the dosxyznrc voxelso
     *n one line'/ ' (min= ',F12.5,' x',F12.5,' x',F12.5,' cm)'/ ' :',$)
      read(5,*) xyz_xthickness,xyz_ythickness,xyz_zthickness
      write(6,2640)xyz_xthickness,xyz_ythickness,xyz_zthickness
      write(1,2640)xyz_xthickness,xyz_ythickness,xyz_zthickness
2640  format(3F12.5)
      IF ((xyz_xthickness .LE.0..OR.xyz_ythickness .LE.0..OR.xyz_zthickn
     *ess .LE.0.)) THEN
          write(6,2650)
          write(1,2650)
2650      format(' Dimensions must all be positive.  Try again.')
          STOP
      ELSE IF((xyz_xthickness .GT. ct_imax*ct_xthickness.OR. xyz_ythickn
     *ess .GT. ct_jmax*ct_ythickness.OR. xyz_zthickness .GT. ct_kmax*ct_
     *zthickness)) THEN
          write(6,2660)
          write(1,2660)
2660      format(' Dimension in a direction cannot be greater than total
     * size of'/ ' CT data in that direction.  Try again.')
          STOP
      ELSE IF((xyz_xthickness .LT. ct_imax*ct_xthickness/128.OR. xyz_yth
     *ickness .LT. ct_jmax*ct_ythickness/128.OR. xyz_zthickness .LT. ct_
     *kmax*ct_zthickness/128)) THEN
          write(6,2670)
          write(1,2670)
2670      format(' Dimensions in at least one direction < min allowed. E
     *ither increase'/ ' dimension(s) or go into dosxyznrc_user_macros.m
     *ortran and increase IMAX,'/ ' JMAX and/or KMAX')
          STOP
      END IF
      xyz_imax=INT(ct_imax*ct_xthickness/xyz_xthickness)
      xyz_jmax=INT(ct_jmax*ct_ythickness/xyz_ythickness)
      xyz_kmax=INT(ct_kmax*ct_zthickness/xyz_zthickness)
      xyz_xthickness=FLOAT(ct_imax)*ct_xthickness/xyz_imax
      xyz_ythickness=FLOAT(ct_jmax)*ct_ythickness/xyz_jmax
      xyz_zthickness=FLOAT(ct_kmax)*ct_zthickness/xyz_kmax
      write(6,2680)xyz_xthickness
      write(1,2680)xyz_xthickness
2680  format('New X voxel thickness -> ',F10.2)
      write(6,2690)xyz_ythickness
      write(1,2690)xyz_ythickness
2690  format('New Y voxel thickness -> ',F10.2)
      write(6,2700)xyz_zthickness
      write(1,2700)xyz_zthickness
2700  format('New Z voxel thickness -> ',F10.2)
      write(6,2710)xyz_imax
      write(1,2710)xyz_imax
2710  format('New number X voxels   -> ',I10)
      write(6,2720)xyz_jmax
      write(1,2720)xyz_jmax
2720  format('New number Y voxels   -> ',I10)
      write(6,2730)xyz_kmax
      write(1,2730)xyz_kmax
2730  format('New number Z voxels   -> ',I10)
      write(6,2740)xyz_xthickness,xyz_ythickness,xyz_zthickness
      write(1,2740)xyz_xthickness,xyz_ythickness,xyz_zthickness
2740  format(' Final x,y,z dimensions of dosxyznrc voxels in cm (adjuste
     *d so that an', ' integer'/ ' number fit exactly on the CT data):',
     *3F12.5)
      xyz_xbounds(1)=CTOffset(1)
      xyz_ybounds(1)=CTOffset(2)
      xyz_zbounds(1)=CTOffset(3)
      DO 2751 i_xyz=1,xyz_imax
          xyz_xbounds(i_xyz+1)=xyz_xbounds(i_xyz)+xyz_xthickness
2751  CONTINUE
2752  CONTINUE
      DO 2761 i_xyz=1,xyz_jmax
          xyz_ybounds(i_xyz+1)=xyz_ybounds(i_xyz)+xyz_ythickness
2761  CONTINUE
2762  CONTINUE
      DO 2771 i_xyz=1,xyz_kmax
          xyz_zbounds(i_xyz+1)=xyz_zbounds(i_xyz)+xyz_zthickness
2771  CONTINUE
2772  CONTINUE
      DO 2781 i_xyz=1,xyz_imax+xyz_jmax+xyz_kmax
          weight_xyz(i_xyz)=0.0
2781  CONTINUE
2782  CONTINUE
      DO 2791 i_xyz=1,xyz_imax
          DO 2801 j_xyz=1,xyz_jmax
              DO 2811 k_xyz=1,xyz_kmax
                  realxyz_ct(i_xyz,j_xyz,k_xyz)=0.0
2811          CONTINUE
2812          CONTINUE
2801      CONTINUE
2802      CONTINUE
2791  CONTINUE
2792  CONTINUE
      write(6,2820)
      write(1,2820)
2820  format('Calculating bounds and new CT values')
      DO 2831 i_ct=1,ct_imax
          DO 2841 i_xyz=1,xyz_imax
              IF (((xyz_xbounds(i_xyz) .LE. xyz_xbounds(1)+ct_xthickness
     *        *(i_ct-1)) .AND. (xyz_xbounds(i_xyz+1) .GE. xyz_xbounds(1)
     *        +ct_xthickness*(i_ct-1)))) THEN
                  i_lower_xyz=i_xyz
              END IF
              IF (((xyz_xbounds(i_xyz) .LE. xyz_xbounds(1)+ct_xthickness
     *        *FLOAT(i_ct)) .AND. (xyz_xbounds(i_xyz+1) .GE. xyz_xbounds
     *        (1)+ct_xthickness*FLOAT(i_ct)) )) THEN
                  i_upper_xyz=i_xyz
                  GO TO2842
              END IF
2841      CONTINUE
2842      CONTINUE
          iorjork='i'
          IF ((iorjork.EQ.'i')) THEN
              param=0
          ELSE IF((iorjork.EQ.'j')) THEN
              param=xyz_imax
          ELSE IF((iorjork.EQ.'k')) THEN
              param=xyz_imax+xyz_jmax
          END IF
          IF ((i_lower_xyz.EQ.i_upper_xyz)) THEN
              weight_xyz(param+i_lower_xyz)=ct_xthickness/ (xyz_xbounds(
     *        i_lower_xyz+1)- xyz_xbounds(i_lower_xyz))
          ELSE
              DO 2851 i_xyz=i_lower_xyz,i_upper_xyz
                  IF (((xyz_xbounds(i_xyz).GE. (xyz_xbounds(1)+ct_xthick
     *            ness*(i_ct-1))).AND. (xyz_xbounds(i_xyz+1).LE. (xyz_xb
     *            ounds(1)+ct_xthickness*(i_ct))))) THEN
                      weight_xyz(param+i_xyz)=1.00
                  ELSE IF(((xyz_xbounds(i_xyz).LE. (xyz_xbounds(1)+ct_xt
     *            hickness*(i_ct-1))).AND. (xyz_xbounds(i_xyz+1).LE. (xy
     *            z_xbounds(1)+ct_xthickness*(i_ct))))) THEN
                      weight_xyz(param+i_xyz)= (xyz_xbounds(i_xyz+1)-(xy
     *                z_xbounds(1)+ ct_xthickness*(i_ct-1)))/ (xyz_xboun
     *                ds(i_xyz+1)-xyz_xbounds(i_xyz))
                  ELSE
                      weight_xyz(param+i_xyz)= ((xyz_xbounds(1)+ct_xthic
     *                kness*(i_ct))- xyz_xbounds(i_xyz))/ (xyz_xbounds(i
     *                _xyz+1)-xyz_xbounds(i_xyz))
                  END IF
2851          CONTINUE
2852          CONTINUE
          END IF
          DO 2861 j_ct=1,ct_jmax
              DO 2871 i_xyz=1,xyz_jmax
                  IF (((xyz_ybounds(i_xyz) .LE. xyz_ybounds(1)+ct_ythick
     *            ness*(j_ct-1)) .AND. (xyz_ybounds(i_xyz+1) .GE. xyz_yb
     *            ounds(1)+ct_ythickness*(j_ct-1)))) THEN
                      j_lower_xyz=i_xyz
                  END IF
                  IF (((xyz_ybounds(i_xyz) .LE. xyz_ybounds(1)+ct_ythick
     *            ness*FLOAT(j_ct)) .AND. (xyz_ybounds(i_xyz+1) .GE. xyz
     *            _ybounds(1)+ct_ythickness*FLOAT(j_ct)) )) THEN
                      j_upper_xyz=i_xyz
                      GO TO2872
                  END IF
2871          CONTINUE
2872          CONTINUE
              iorjork='j'
              IF ((iorjork.EQ.'i')) THEN
                  param=0
              ELSE IF((iorjork.EQ.'j')) THEN
                  param=xyz_imax
              ELSE IF((iorjork.EQ.'k')) THEN
                  param=xyz_imax+xyz_jmax
              END IF
              IF ((j_lower_xyz.EQ.j_upper_xyz)) THEN
                  weight_xyz(param+j_lower_xyz)=ct_ythickness/ (xyz_ybou
     *            nds(j_lower_xyz+1)- xyz_ybounds(j_lower_xyz))
              ELSE
                  DO 2881 i_xyz=j_lower_xyz,j_upper_xyz
                      IF (((xyz_ybounds(i_xyz).GE. (xyz_ybounds(1)+ct_yt
     *                hickness*(j_ct-1))).AND. (xyz_ybounds(i_xyz+1).LE.
     *                 (xyz_ybounds(1)+ct_ythickness*(j_ct))))) THEN
                          weight_xyz(param+i_xyz)=1.00
                      ELSE IF(((xyz_ybounds(i_xyz).LE. (xyz_ybounds(1)+c
     *                t_ythickness*(j_ct-1))).AND. (xyz_ybounds(i_xyz+1)
     *                .LE. (xyz_ybounds(1)+ct_ythickness*(j_ct))))) THEN
                          weight_xyz(param+i_xyz)= (xyz_ybounds(i_xyz+1)
     *                    -(xyz_ybounds(1)+ ct_ythickness*(j_ct-1)))/ (x
     *                    yz_ybounds(i_xyz+1)-xyz_ybounds(i_xyz))
                      ELSE
                          weight_xyz(param+i_xyz)= ((xyz_ybounds(1)+ct_y
     *                    thickness*(j_ct))- xyz_ybounds(i_xyz))/ (xyz_y
     *                    bounds(i_xyz+1)-xyz_ybounds(i_xyz))
                      END IF
2881              CONTINUE
2882              CONTINUE
              END IF
              DO 2891 k_ct=1,ct_kmax
                  DO 2901 i_xyz=1,xyz_kmax
                      IF (((xyz_zbounds(i_xyz) .LE. xyz_zbounds(1)+ct_zt
     *                hickness*(k_ct-1)) .AND. (xyz_zbounds(i_xyz+1) .GE
     *                . xyz_zbounds(1)+ct_zthickness*(k_ct-1)))) THEN
                          k_lower_xyz=i_xyz
                      END IF
                      IF (((xyz_zbounds(i_xyz) .LE. xyz_zbounds(1)+ct_zt
     *                hickness*FLOAT(k_ct)) .AND. (xyz_zbounds(i_xyz+1)
     *                .GE. xyz_zbounds(1)+ct_zthickness*FLOAT(k_ct)) ))
     *                THEN
                          k_upper_xyz=i_xyz
                          GO TO2902
                      END IF
2901              CONTINUE
2902              CONTINUE
                  iorjork='k'
                  IF ((iorjork.EQ.'i')) THEN
                      param=0
                  ELSE IF((iorjork.EQ.'j')) THEN
                      param=xyz_imax
                  ELSE IF((iorjork.EQ.'k')) THEN
                      param=xyz_imax+xyz_jmax
                  END IF
                  IF ((k_lower_xyz.EQ.k_upper_xyz)) THEN
                      weight_xyz(param+k_lower_xyz)=ct_zthickness/ (xyz_
     *                zbounds(k_lower_xyz+1)- xyz_zbounds(k_lower_xyz))
                  ELSE
                      DO 2911 i_xyz=k_lower_xyz,k_upper_xyz
                          IF (((xyz_zbounds(i_xyz).GE. (xyz_zbounds(1)+c
     *                    t_zthickness*(k_ct-1))).AND. (xyz_zbounds(i_xy
     *                    z+1).LE. (xyz_zbounds(1)+ct_zthickness*(k_ct))
     *                    ))) THEN
                              weight_xyz(param+i_xyz)=1.00
                          ELSE IF(((xyz_zbounds(i_xyz).LE. (xyz_zbounds(
     *                    1)+ct_zthickness*(k_ct-1))).AND. (xyz_zbounds(
     *                    i_xyz+1).LE. (xyz_zbounds(1)+ct_zthickness*(k_
     *                    ct))))) THEN
                              weight_xyz(param+i_xyz)= (xyz_zbounds(i_xy
     *                        z+1)-(xyz_zbounds(1)+ ct_zthickness*(k_ct-
     *                        1)))/ (xyz_zbounds(i_xyz+1)-xyz_zbounds(i_
     *                        xyz))
                          ELSE
                              weight_xyz(param+i_xyz)= ((xyz_zbounds(1)+
     *                        ct_zthickness*(k_ct))- xyz_zbounds(i_xyz))
     *                        / (xyz_zbounds(i_xyz+1)-xyz_zbounds(i_xyz)
     *                        )
                          END IF
2911                  CONTINUE
2912                  CONTINUE
                  END IF
                  DO 2921 i_xyz=i_lower_xyz,i_upper_xyz
                      DO 2931 j_xyz=j_lower_xyz,j_upper_xyz
                          DO 2941 k_xyz=k_lower_xyz,k_upper_xyz
                              realxyz_ct(i_xyz,j_xyz,k_xyz)= realxyz_ct(
     *                        i_xyz,j_xyz,k_xyz)+ FLOAT(ct_data(i_ct,j_c
     *                        t,k_ct))* weight_xyz(i_xyz)* weight_xyz(xy
     *                        z_imax+j_xyz)* weight_xyz(xyz_imax+xyz_jma
     *                        x+k_xyz)
2941                      CONTINUE
2942                      CONTINUE
2931                  CONTINUE
2932                  CONTINUE
2921              CONTINUE
2922              CONTINUE
2891          CONTINUE
2892          CONTINUE
2861      CONTINUE
2862      CONTINUE
2831  CONTINUE
2832  CONTINUE
      DO 2951 i=1,xyz_imax
          DO 2961 j=1,xyz_jmax
              DO 2971 k=1,xyz_kmax
                  xyz_ct(i,j,k)=NINT(realxyz_ct(i,j,k))
2971          CONTINUE
2972          CONTINUE
2961      CONTINUE
2962      CONTINUE
2951  CONTINUE
2952  CONTINUE
      RETURN
      END
      subroutine CTToMedium(new_x_dim,new_y_dim,new_z_dim, New_CT_Data,
     *num_material, material_region, density_region, material_name)
      INTEGER*2 New_CT_Data(128,128,128)
      INTEGER new_x_dim,  new_y_dim,  new_z_dim,  material_region( 20971
     *54),  num_material,   material_ct_upper_bound(20), material_ct_low
     *er_bound,  i_material,I,J,K,  ct_low,ct_high
      REAL density_region( 2097154),  material_density_lower_bound(20),
     * material_density_upper_bound(20)
      CHARACTER*4 material_name(24,20)
      DO 2981 I=1,new_x_dim
          DO 2991 J=1,new_y_dim
              DO 3001 K=1,new_z_dim
                  material_region((1 + I + (J-1)*new_x_dim + (K-1)*new_x
     *            _dim*new_y_dim))=0.
                  density_region((1 + I + (J-1)*new_x_dim + (K-1)*new_x_
     *            dim*new_y_dim))=0
3001          CONTINUE
3002          CONTINUE
2991      CONTINUE
2992      CONTINUE
2981  CONTINUE
2982  CONTINUE
      write(6,3010)
      write(1,3010)
3010  format(/' The CT-Density Ramp'/ ' -------------------'/)
3020  write(6,3030)20
      write(1,3030)20
3030  format(' Number of media (max ',I4,'), min. CT number of ramp'/ '(
     *0,0 if you want to use the hard-wired ramp function): ',$)
      READ(5,*) num_material,material_ct_lower_bound
      write(6,3040)num_material,material_ct_lower_bound
      write(1,3040)num_material,material_ct_lower_bound
3040  format(2I10)
      IF ((num_material.LT.0 .OR. num_material .GT. 20)) THEN
          write(6,3050)
          write(1,3050)
3050      format(' Number of materials out of range. Try again. ')
          STOP
      ELSE IF((num_material.EQ.0)) THEN
          write(6,3060)
          write(1,3060)
3060      format(/' Using the following default CT ramp.'/ ' Note: Thisi
     *s optimized for display and example '/ ' calculations.  It is reco
     *mmended that you enter '/ ' the CT conversion ramp for your own im
     *ager.'/)
          DO 3071 i=1,4
              DO 3081 j=1,24
                  material_name(j,i)=' '
3081          CONTINUE
3082          CONTINUE
3071      CONTINUE
3072      CONTINUE
          num_material=4
          material_ct_lower_bound=-1024
          material_name(1,1)='A'
          material_name(2,1)='I'
          material_name(3,1)='R'
          material_name(4,1)='7'
          material_name(5,1)='0'
          material_name(6,1)='0'
          material_name(7,1)='I'
          material_name(8,1)='C'
          material_name(9,1)='R'
          material_name(10,1)='U'
          material_ct_upper_bound(1)= -974
          material_density_lower_bound(1)=0.001
          material_density_upper_bound(1)=0.044
          material_name(1,2)='L'
          material_name(2,2)='U'
          material_name(3,2)='N'
          material_name(4,2)='G'
          material_name(5,2)='7'
          material_name(6,2)='0'
          material_name(7,2)='0'
          material_name(8,2)='I'
          material_name(9,2)='C'
          material_name(10,2)='R'
          material_name(11,2)='U'
          material_ct_upper_bound(2)= -724
          material_density_lower_bound(2)=0.044
          material_density_upper_bound(2)=0.302
          material_name(1,3)='I'
          material_name(2,3)='C'
          material_name(3,3)='R'
          material_name(4,3)='U'
          material_name(5,3)='T'
          material_name(6,3)='I'
          material_name(7,3)='S'
          material_name(8,3)='S'
          material_name(9,3)='U'
          material_name(10,3)='E'
          material_name(11,3)='7'
          material_name(12,3)='0'
          material_name(13,3)='0'
          material_name(14,3)='I'
          material_name(15,3)='C'
          material_name(16,3)='R'
          material_name(17,3)='U'
          material_ct_upper_bound(3)= 101
          material_density_lower_bound(3)=0.302
          material_density_upper_bound(3)=1.101
          material_name(1,4)='I'
          material_name(2,4)='C'
          material_name(3,4)='R'
          material_name(4,4)='P'
          material_name(5,4)='B'
          material_name(6,4)='O'
          material_name(7,4)='N'
          material_name(8,4)='E'
          material_name(9,4)='7'
          material_name(10,4)='0'
          material_name(11,4)='0'
          material_name(12,4)='I'
          material_name(13,4)='C'
          material_name(14,4)='R'
          material_name(15,4)='U'
          material_ct_upper_bound(4)= 1976
          material_density_lower_bound(4)=1.101
          material_density_upper_bound(4)=2.088
          write(6,3090)material_ct_lower_bound
          write(1,3090)material_ct_lower_bound
3090      format(' CT no. lower bound of ramp = ',I5)
          DO 3101 i_material=1,num_material
              write(6,3110)i_material,(material_name(j,i_material),j=1,2
     *        4)
              write(1,3110)i_material,(material_name(j,i_material),j=1,2
     *        4)
3110          format(/' Medium ',I4,' : ',24a1)
              write(6,3120)material_ct_upper_bound(i_material), material
     *        _density_lower_bound(i_material), material_density_upper_b
     *        ound(i_material)
              write(1,3120)material_ct_upper_bound(i_material), material
     *        _density_lower_bound(i_material), material_density_upper_b
     *        ound(i_material)
3120          format(' CT no. upper bound, density lower bound (g/cm^3),
     *'/ ' density upper bound (g/cm^3)--all on one line'/ ' : ',I5,3F12
     *.5)
3101      CONTINUE
3102      CONTINUE
      ELSE
          DO 3131 i_material=1,num_material
              write(6,3140)i_material
              write(1,3140)i_material
3140          format(/' Medium ',I4,' : ',$)
              READ(5,3150)(material_name(j,i_material),j=1,24)
3150          FORMAT(24a1)
              write(6,3160)(material_name(j,i_material),j=1,24)
              write(1,3160)(material_name(j,i_material),j=1,24)
3160          format(24a1)
              write(6,3170)
              write(1,3170)
3170          format(' CT no. upper bound, density lower bound (g/cm^3),
     *'/ ' density upper bound (g/cm^3)--all on one line'/ ' : ',$)
              READ(5,*) material_ct_upper_bound(i_material), material_de
     *        nsity_lower_bound(i_material), material_density_upper_boun
     *        d(i_material)
              write(6,3180)material_ct_upper_bound(i_material), material
     *        _density_lower_bound(i_material), material_density_upper_b
     *        ound(i_material)
              write(1,3180)material_ct_upper_bound(i_material), material
     *        _density_lower_bound(i_material), material_density_upper_b
     *        ound(i_material)
3180          format(I5,2F12.5)
              IF ((material_ct_upper_bound(i_material).LT. material_ct_l
     *        ower_bound .OR. material_density_lower_bound(i_material).L
     *        T.0 .OR. material_density_upper_bound(i_material) .LT. mat
     *        erial_density_lower_bound(i_material))) THEN
                  write(6,3190)
                  write(1,3190)
3190              format(' CT no. or density out of range, or density up
     *per bound <'/ ' density lower bound.  Try again.')
                  STOP
              END IF
3131      CONTINUE
3132      CONTINUE
      END IF
      ct_low=0
      ct_high=0
      DO 3201 K=1,new_z_dim
          DO 3211 J=1,new_y_dim
              DO 3221 I=1,new_x_dim
                  IF ((New_CT_Data(I,J,K).LT.material_ct_lower_bound)) T
     *            HEN
                      New_CT_Data(I,J,K)=0
                      material_region((1 + I + (J-1)*new_x_dim + (K-1)*n
     *                ew_x_dim*new_y_dim))=0
                      density_region((1 + I + (J-1)*new_x_dim + (K-1)*ne
     *                w_x_dim*new_y_dim))=0
                      ct_low=ct_low+1
                  ELSE IF((New_CT_Data(I,J,K) .GT. material_ct_upper_bou
     *            nd(num_material))) THEN
                      New_CT_Data(I,J,K)=material_ct_upper_bound(num_mat
     *                erial)
                      material_region((1 + I + (J-1)*new_x_dim + (K-1)*n
     *                ew_x_dim*new_y_dim))=num_material
                      density_region((1 + I + (J-1)*new_x_dim + (K-1)*ne
     *                w_x_dim*new_y_dim))= material_density_upper_bound(
     *                num_material)
                      ct_high=ct_high+1
                  ELSE
                      DO 3231 i_material=1,num_material
                          IF ((New_CT_Data(I,J,K).LE. material_ct_upper_
     *                    bound(i_material))) THEN
                              material_region((1 + I + (J-1)*new_x_dim +
     *                         (K-1)*new_x_dim*new_y_dim))=i_material
                              IF ((i_material.EQ.1)) THEN
                                  density_region((1 + I + (J-1)*new_x_di
     *                            m + (K-1)*new_x_dim*new_y_dim))= mater
     *                            ial_density_lower_bound(i_material)+ (
     *                            New_CT_Data(I,J,K)-material_ct_lower_b
     *                            ound)* (material_density_upper_bound(i
     *                            _material)- material_density_lower_bou
     *                            nd(i_material))/ (material_ct_upper_bo
     *                            und(i_material)- material_ct_lower_bou
     *                            nd)
                                  material_region((1 + I + (J-1)*new_x_d
     *                            im + (K-1)*new_x_dim*new_y_dim))=1
                              ELSE
                                  density_region((1 + I + (J-1)*new_x_di
     *                            m + (K-1)*new_x_dim*new_y_dim))= mater
     *                            ial_density_lower_bound(i_material)+ (
     *                            New_CT_Data(I,J,K)-material_ct_upper_b
     *                            ound(i_material-1))* (material_density
     *                            _upper_bound(i_material)- material_den
     *                            sity_lower_bound(i_material))/ (materi
     *                            al_ct_upper_bound(i_material)- materia
     *                            l_ct_upper_bound(i_material-1))
                                  material_region((1 + I + (J-1)*new_x_d
     *                            im + (K-1)*new_x_dim*new_y_dim))=i_mat
     *                            erial
                              END IF
                              GO TO3232
                          END IF
3231                  CONTINUE
3232                  CONTINUE
                  END IF
3221          CONTINUE
3222          CONTINUE
3211      CONTINUE
3212      CONTINUE
3201  CONTINUE
3202  CONTINUE
      IF ((ct_low.GT.0)) THEN
          write(6,3240)ct_low,material_ct_lower_bound
          write(1,3240)ct_low,material_ct_lower_bound
3240      format(/' Warning: CT number in ',I10,' voxels is < min. CT nu
     *mber of '/ ' ramp (',I10,').  Medium in these voxels is set to 0 (
     *vacuum).'/)
      END IF
      IF ((ct_high.GT.0)) THEN
          write(6,3250)ct_high,material_ct_upper_bound(num_material),num
     *    _material
          write(1,3250)ct_high,material_ct_upper_bound(num_material),num
     *    _material
3250      format(/' Warning: CT number in ',I10,' voxels is > max. CT nu
     *mber of '/ ' ramp (',I10,').  Medium in these voxels is set to med
     *ium no. ',I4,/)
      END IF
      RETURN
      END
      subroutine write_material(fname,iimax,jjmax,kkmax,xbnd,ybnd,zbnd,
     *rho,med,CT,CTformat)
      IMPLICIT NONE
      Character*256 fname
      Character*80 ddataname
      Character*60 CTformat
      INTEGER iimax,  jjmax,  kkmax,  lnblnk1, rindex, ii,jj,kk
      REAL xbnd(128+1),  ybnd(128+1),  zbnd(128+1),  rho( 2097154)
      Integer med( 2097154)
      Integer*2 CT(128,128,128)
      IF ((CTformat.EQ.'CADPLAN')) THEN
          ddataname=fname(rindex(fname,'/')+1:lnblnk1(fname)) // '.CTfor
     *PAW'
      ELSE IF((CTformat.EQ.'Pinnacle')) THEN
          ddataname=fname(rindex(fname,'/')+1:index(fname,'header')-1) /
     *    / 'CTforPAW'
      ELSE IF((CTformat.EQ.'DICOM')) THEN
          ddataname=fname(rindex(fname,'/')+1:lnblnk1(fname)-1) // '.CTf
     *orPAW'
      END IF
      write(6,3260)ddataname(:lnblnk1(ddataname))
      write(1,3260)ddataname(:lnblnk1(ddataname))
3260  format(/' Writing CT phantom data into ', A,' for display.'/)
      Open (15,file=ddataname,Status='new',ERR=3270)
      write(15,*)iimax,jjmax,kkmax
      write(15,*)(xbnd(ii),ii=1,iimax+1)
      write(15,*)(ybnd(jj),jj=1,jjmax+1)
      write(15,*)(zbnd(kk),kk=1,kkmax+1)
      write(15,*)(((rho((ii+(jj-1)*iimax+(kk-1)*iimax*jjmax)),ii=1,iimax
     *),jj=1,jjmax),kk=1,kkmax)
      write(15,*)(((CT(ii,jj,kk),ii=1,iimax),jj=1,jjmax),kk=1,kkmax)
      close(15)
      RETURN
3270  write(6,3280)ddataname(:lnblnk1(ddataname))
      write(1,3280)ddataname(:lnblnk1(ddataname))
3280  format(//' ***ERROR: '/ ' Cannot write to ',A,'.  File already exi
     *sts.'//)
      end
      subroutine write_phantom(fname,nmed,media, iimax,jjmax,kkmax,xbnd,
     *ybnd,zbnd, rho,med,CTformat)
      IMPLICIT NONE
      CHARACTER*256 fname
      CHARACTER*80 phantname
      Character*4 media(24,20)
      Character*60 CTformat
      INTEGER nmed,  iimax,  jjmax,  kkmax,  med( 2097154),  lnblnk1, ri
     *ndex, ii,jj,kk
      REAL xbnd(128+1),  ybnd(128+1),  zbnd(128+1),  rho( 2097154),  est
     *epe(20)
      IF ((CTformat.EQ.'CADPLAN')) THEN
          phantname=fname(rindex(fname,'/')+1:lnblnk1(fname)) // '.egsph
     *ant'
      ELSE IF((CTformat.EQ.'Pinnacle')) THEN
          phantname=fname(rindex(fname,'/')+1:index(fname,'header')-1) /
     *    / 'egsphant'
      ELSE IF((CTformat.EQ.'DICOM')) THEN
          phantname=fname(rindex(fname,'/')+1:lnblnk1(fname)-1) // '.egs
     *phant'
      END IF
      write(6,3290)phantname(:lnblnk1(phantname))
      write(1,3290)phantname(:lnblnk1(phantname))
3290  format(/' Writing CT phantom data into ', A,' to be read by dosxyz
     *nrc.'/)
      DO 3301 ii=1,nmed
          estepe(ii)=1.0
3301  CONTINUE
3302  CONTINUE
      Open (15,file=phantname,Status='new',ERR=3310)
      WRITE(15,'(i2)') nmed
      DO 3321 ii=1,nmed
          Write(15,'(24a1)') (media(jj,ii),jj=1,24)
3321  CONTINUE
3322  CONTINUE
      WRITE(15,*) (estepe(ii),ii=1,nmed)
      WRITE(15,'(3i5)') iimax,jjmax,kkmax
      WRITE(15,*) (xbnd(ii),ii=1,iimax+1)
      WRITE(15,*) (ybnd(jj),jj=1,jjmax+1)
      WRITE(15,*) (zbnd(kk),kk=1,kkmax+1)
      DO 3331 kk=1,kkmax
          DO 3341 jj=1,jjmax
              WRITE(15,1399) (med((1 + ii + (jj-1)*iimax + (kk-1)*iimax*
     *        jjmax)),ii=1,iimax)
3341      CONTINUE
3342      CONTINUE
          WRITE(15,*)
3331  CONTINUE
3332  CONTINUE
1399  FORMAT(128i1)
      DO 3351 kk=1,kkmax
          DO 3361 jj=1,jjmax
              WRITE(15,*) (rho((1 + ii + (jj-1)*iimax + (kk-1)*iimax*jjm
     *        ax)),ii=1,iimax)
3361      CONTINUE
3362      CONTINUE
          WRITE(15,*)
3351  CONTINUE
3352  CONTINUE
      Close(15)
      RETURN
3310  write(6,3370)phantname(:lnblnk1(phantname))
      write(1,3370)phantname(:lnblnk1(phantname))
3370  format(//' ***ERROR: '/ ' Cannot write to ',A,'.  File already exi
     *sts.'//)
      end
      FUNCTION rindex(c,a)
      CHARACTER c*(*)
      CHARACTER a*1
      INTEGER j,rindex
      DO 3381 j=LEN(c),1,-1
          IF ((c(j:j) .EQ. a)) THEN
              rindex=j
              RETURN
          END IF
3381  CONTINUE
3382  CONTINUE
      rindex=0
      RETURN
      end
      integer function lnblnk1(string)
      character*(*) string
      DO 3391 i=len(string),1,-1
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
3391  CONTINUE
3392  CONTINUE
      lnblnk1 = 0
      return
      end
