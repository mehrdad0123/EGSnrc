      integer*4 function lnblnk1(string)
      character*(*) string
      DO 1011 i=len(string),1,-1
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
1011  CONTINUE
1012  CONTINUE
      lnblnk1 = 0
      return
      end
      program beam_build
      character*120 spec_module
      call getarg(1,spec_module)
      IF ((lnblnk1(spec_module).EQ.0)) THEN
        write(6,'(/a)')'**** Error: '
        write(6,'(a)')'You have to provide the name of a spec_module (no
     * extension)'
        write(6,'(a)')'on the command line.'
        write(6,'(/a)')'**** Quitting now.'
        call exit(1)
      END IF
      call build_accel(spec_module(:lnblnk1(spec_module)))
      end
      subroutine build_accel(spec_module)
      character*120 spec_module,omega_home,cm_home,spec_home,accel_home,
     * accel_name,file_name,egs_home,beam_home,spec_file
      character*10 cmtype(25),cmid(25)
      character*30 the_file
      character*20 vname
      character*300 text
      character*1 blank
      integer*4 iindex,length,num_cms,num_ids,i,istat,egs_system,line,j
      integer*4 lnblnk1
      logical fileexist,egs_isdir,first_time
      character file_sep
      data blank/' '/
      file_sep = char(92)
      call getenv('EGS_HOME',egs_home)
      IF (( lnblnk1(egs_home) .LT. 1 )) THEN
        write(6,*) '*** Error: Environment variable EGS_HOME is not set'
        write(6,*) '           quitting now'
        call exit(1)
      END IF
      omega_home='F:/Git/EGSnrc-master/HEN_HOUSE/' // 'omega'
      spec_file='F:/Git/EGSnrc-master/HEN_HOUSE/' // 'specs' // char(92)
     * // 'beamnrc.spec'
      beam_home=omega_home(:lnblnk1(omega_home)) // char(92) // 'beamnrc
     *'
      cm_home=beam_home(:lnblnk1(beam_home)) // char(92) // 'CMs' // cha
     *r(92)
      spec_home=egs_home(:lnblnk1(egs_home)) // 'beamnrc' // char(92) //
     * 'spec_modules' // char(92)
      accel_home=egs_home(:lnblnk1(egs_home)) // 'BEAM_' // spec_module(
     *:lnblnk1(spec_module)) // char(92)
      accel_name=accel_home(:lnblnk1(accel_home))//'BEAM_'//spec_module(
     *:lnblnk1(spec_module))
      file_name=spec_home(:lnblnk1(spec_home))//spec_module(:lnblnk1(spe
     *c_module))//'.module'
      open(8,file=file_name(:lnblnk1(file_name)),status='old',err=1020)
      write(6,*)' Now reading CMs and identifiers from ',file_name(:lnbl
     *nk1(file_name))
      read(8,end=1030,err=1040,fmt='(A)') text
      text=text(:lnblnk1(text))
      vname='CM names'
      iindex=index(text,vname(:lnblnk1(vname)))
      iindex=iindex+lnblnk1(vname)+1
      text=text(iindex:)
      iindex=index(text,':')
      text=text(iindex+1:)
      write(*,*)' text: ',text
      num_cms=0
1051  CONTINUE
        IF((lnblnk1(text).EQ.0))GO TO1052
        length = lnblnk1(text)
1061    IF(index(text,blank).NE.1)GO TO 1062
          IF (( length .GE. 2 )) THEN
            text=text(2:)
          ELSE
            GO TO1062
          END IF
          length = length - 1
        GO TO 1061
1062    CONTINUE
        num_cms=num_cms+1
        cmtype(num_cms)=text(:index(text,blank)-1)
        text=text(index(text,blank):)
      GO TO 1051
1052  CONTINUE
      read(8,end=1030,err=1040,fmt='(A)') text
      text=text(:lnblnk1(text))
      vname='Identifiers'
      iindex=index(text,vname(:lnblnk1(vname)))
      iindex=iindex+lnblnk1(vname)+1
      text=text(iindex:)
      iindex=index(text,':')
      text=text(iindex+1:)
      write(*,*)' text: ',text
      num_ids=0
1071  CONTINUE
        IF((lnblnk1(text).EQ.0))GO TO1072
        length = lnblnk1(text)
1081    IF(index(text,blank).NE.1)GO TO 1082
          IF (( length .GE. 2 )) THEN
            text=text(2:)
          ELSE
            GO TO1082
          END IF
          length = length - 1
        GO TO 1081
1082    CONTINUE
        num_ids=num_ids+1
        cmid(num_ids)=text(:index(text,blank)-1)
        text=text(index(text,blank):)
      GO TO 1071
1072  CONTINUE
      IF ((num_ids .NE. num_cms)) THEN
        write(6,'(/a)')'**** Error: '
        write(6,*)'Number of CM types and identifiers do not match'
        write(6,*)'in ',spec_home(:lnblnk1(spec_home))//spec_module(:lnb
     *  lnk1(spec_module))//'.module'
        write(6,*)'Check the file and try again.'
        write(6,'(/a)')'**** Quitting now.'
        GOTO 1090
      END IF
      write(6,'(/a)')'Will use the following CM types and identifiers:'
      write(6,'(a,$)')' CM Names: '
      do i=1,num_cms
        write(6,'(a,'' '',$)') cmtype(i)(:lnblnk1(cmtype(i)))
      end do
      write(6,'(/a,$)')' Identifiers: '
      do i=1,num_cms
        write(6,'(a,'' '',$)') cmid(i)(:lnblnk1(cmid(i)))
      end do
      IF ((.NOT.egs_isdir(accel_home(:lnblnk1(accel_home))))) THEN
        write(6,'(/a)')'Directory ',accel_home(:lnblnk1(accel_home))
        write(6,'(a)')'does not exist.  Creating it.'
        istat=egs_system('mkdir '//accel_home(:lnblnk1(accel_home)))
        IF ((istat.NE.0)) THEN
          write(6,'(/a)')'**** Error creating directory ',accel_home(:ln
     *    blnk1(accel_home))
          write(6,'(a)')'**** Quitting now.'
          call exit(1)
        END IF
      END IF
      write(6,'(//a)')'(Re)creating/checking files:'
      write(6,'(2x,a)')accel_name(:lnblnk1(accel_name))//'_cm.mortran an
     *d'
      write(6,'(2x,a)')accel_name(:lnblnk1(accel_name))//'_macros.mortra
     *n'
      write(6,'(2x,a)')accel_home(:lnblnk1(accel_home))//'Makefile'
      write(6,'(2x,a)')accel_home(:lnblnk1(accel_home))//'modules.make'
      write(6,'(2x,a)')accel_home(:lnblnk1(accel_home))//'sources.make'
      write(6,'(2x,a)')accel_name(:lnblnk1(accel_name))//'.io'
      inquire(file=accel_name(:lnblnk1(accel_name))//'_cm.mortran',exist
     *=fileexist)
      IF ((fileexist)) THEN
        write(6,'(/a)')accel_name(:lnblnk1(accel_name))//'_cm.mortran al
     *ready exists.'
        write(6,'(a)')' Will overwrite.'
      END IF
      inquire(file=accel_name(:lnblnk1(accel_name))//'_macros.mortran',e
     *xist=fileexist)
      IF ((fileexist)) THEN
        write(6,'(a)')accel_name(:lnblnk1(accel_name))//'_macros.mortran
     * already exists.'
        write(6,'(a)')' Will overwrite.'
      END IF
      inquire(file=accel_home(:lnblnk1(accel_home))//'modules.make',exis
     *t=fileexist)
      IF ((fileexist)) THEN
        write(6,'(a)')accel_home(:lnblnk1(accel_home))//'modules.make al
     *ready exists.'
        write(6,'(a)')' Will overwrite.'
      END IF
      file_name=accel_name(:lnblnk1(accel_name))//'_cm.mortran'
      open(9,file=file_name(:lnblnk1(file_name)),status='unknown',err=10
     *20)
      file_name=accel_name(:lnblnk1(accel_name))//'_macros.mortran'
      open(10,file=file_name(:lnblnk1(file_name)),status='unknown',err=1
     *020)
      file_name=accel_home(:lnblnk1(accel_home))//'modules.make'
      open(14,file=file_name(:lnblnk1(file_name)),status='unknown',err=1
     *020)
      file_name=accel_home(:lnblnk1(accel_home))//'sources.make'
      inquire(file=file_name(:lnblnk1(file_name)),exist=fileexist)
      IF ((fileexist)) THEN
        write(6,'(/a)') 'File sources.make already exists. Leaving it un
     *touched'
      ELSE
        write(6,'(/a)') 'Creating sources.make '
        open(15,file=file_name(:lnblnk1(file_name)),status='unknown',err
     *  =1020)
        write(15,'(a)') '# Generated by beam_build '
        write(15,'(a)') '#'
        write(15,'(a)') '# You may change the list of sources used to bu
     *ild this accelerator'
        write(15,'(a)') '# by adjusting the list of source files below'
        write(15,'(a)') '#'
        open(27,file=spec_file,status='old',err=1120)
        iindex = 1
1131    CONTINUE
          read(27,'(a256)',err=1140,end=1140) text
          i = index(text,'SOURCES =')
          IF (( i .EQ. 1 )) THEN
1151        CONTINUE
              l = lnblnk1(text)
              write(15,'(a)') text(:l)
              IF((text(l:l) .NE. char(92)))goto 1160
              read(27,'(a256)',err=1140,end=1140) text
            GO TO 1151
1152        CONTINUE
          END IF
        GO TO 1131
1132    CONTINUE
1160    CONTINUE
        rewind(27)
        write(15,*)
        iindex = 2
1171    CONTINUE
          read(27,'(a256)',err=1140,end=1140) text
          i = index(text,'LIB_SOURCES =')
          IF (( i .EQ. 1 )) THEN
1181        CONTINUE
              l = lnblnk1(text)
              write(15,'(a)') text(:l)
              IF((text(l:l) .NE. char(92)))goto 1190
              read(27,'(a256)',err=1140,end=1140) text
            GO TO 1181
1182        CONTINUE
          END IF
        GO TO 1171
1172    CONTINUE
1190    CONTINUE
        close(15)
        close(27)
      END IF
      file_name=accel_home(:lnblnk1(accel_home))//'Makefile'
      inquire(file=file_name(:lnblnk1(file_name)),exist=fileexist)
      IF ((fileexist)) THEN
        write(6,'(a)') 'A Makefile already exists. Leaving it untouched'
      ELSE
        write(6,'(a)') 'Creating Makefile'
        open(15,file=file_name(:lnblnk1(file_name)),status='unknown',err
     *  =1020)
        write(15,'(a)') '# Automatically created by beam_build.exe'
        write(15,'(a)') '#'
        write(15,'(a)') 'include $(EGS_CONFIG)'
        write(15,'(a)') 'include $(SPEC_DIR)beamnrc.spec'
        write(15,'(/a,a)') 'accelerator = ',spec_module(:lnblnk1(spec_mo
     *  dule))
        write(15,'(/a)') 'include $(BEAM_MAKEFILE)'
        close(15)
      END IF
      file_name=accel_name(:lnblnk1(accel_name))//'.io'
      inquire(file=file_name(:lnblnk1(file_name)),exist=fileexist)
      IF ((fileexist)) THEN
        write(6,'(a)') 'A .io file already exists. Leaving it untouched'
      ELSE
        write(6,'(a)') 'Creating .io file'
        open(15,file=file_name(:lnblnk1(file_name)),status='unknown',err
     *  =1020)
        file_name=beam_home(:lnblnk1(beam_home)) // char(92) // 'default
     *_beam.io'
        open(16,file=file_name(:lnblnk1(file_name)),status='old',err=102
     *  0)
1201    CONTINUE
          read(16,end=1210,err=1040,fmt='(A)') text
          write(15,'(a)') text(:lnblnk1(text))
        GO TO 1201
1202    CONTINUE
1210    close(16)
        close(15)
      END IF
      write(14,'(a,a)') '#  modules.make file for BEAM_',spec_module(:ln
     *blnk1(spec_module))
      write(14,'(a)') '#'
      write(14,'(a)') '# Automatically generated by beam_build.exe'
      write(14,'(a)') '# Do not edit this file, all changes will be lost
     *!'
      write(14,'(a)') '#'
      write(14,*)
      write(14,'(a,a)') 'CM_MACRO_FILES = ',file_sep
      DO 1221 i=1,num_cms
        write(14,'(a,a,$)') '      $(CM_HOME)',cmtype(i)(:lnblnk1(cmtype
     *  (i)))//'_macros.mortran'
        IF((i .LT. num_cms))write(14,'(a)') file_sep
1221  CONTINUE
1222  CONTINUE
      write(14,*)
      write(14,*)
      write(14,'(a,a)') 'CM_MORTRAN_FILES = ',file_sep
      DO 1231 i=1,num_cms
        write(14,'(a,a,$)') '      $(CM_HOME)',cmtype(i)(:lnblnk1(cmtype
     *  (i)))//'_cm.mortran'
        IF((i .LT. num_cms))write(14,'(a)') file_sep
1231  CONTINUE
1232  CONTINUE
      write(14,*)
      file_name=beam_home(:lnblnk1(beam_home))//char(92)//'beamnrc_cm_ma
     *cros.hdr'
      open(11,file=file_name(:lnblnk1(file_name)),status='old',err=1020)
1241  CONTINUE
        read(11,end=1250,err=1040,fmt='(A)') text
        write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      GO TO 1241
1242  CONTINUE
1250  text='REPLACE{$CM_LIST} WITH {CMLIST('
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      DO 1271 i=1,num_cms
        text='      '//cmid(i)(:lnblnk1(cmid(i)))
        IF((i.LT.num_cms))text=text(:lnblnk1(text))//','
        write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
1271  CONTINUE
1272  CONTINUE
      text='   )}'
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      text='REPLACE{$CM_TYPE} WITH {CMTYPE('
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      DO 1281 i=1,num_cms
        text='      '//cmtype(i)(:lnblnk1(cmtype(i)))
        IF((i.LT.num_cms))text=text(:lnblnk1(text))//','
        write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
1281  CONTINUE
1282  CONTINUE
      text='   )}'
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      text='REPLACE{$SYNC_CM_LIST} WITH {SYNCCMLIST('
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      DO 1291 i=1,num_cms
        IF ((cmtype(i).EQ.'SYNCJAWS' .OR. cmtype(i).EQ.'SYNCVMLC' .OR. c
     *  mtype(i).EQ.'SYNCMLCE' .OR. cmtype(i).EQ.'SYNCHDMLC')) THEN
          text='      '//cmid(i)(:lnblnk1(cmid(i)))//','
          write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
        END IF
1291  CONTINUE
1292  CONTINUE
      text='      END_SYNC_CMS'
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      text='   )}'
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      text='REPLACE{$DYN_CM_LIST} WITH {DYNCMLIST('
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      DO 1301 i=1,num_cms
        IF ((cmtype(i).EQ.'DYNJAWS' .OR. cmtype(i).EQ.'DYNVMLC')) THEN
          text='      '//cmid(i)(:lnblnk1(cmid(i)))//','
          write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
        END IF
1301  CONTINUE
1302  CONTINUE
      text='      END_DYN_CMS'
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      text='   )}'
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      text='REPLACE{$CM_TYPE} WITH {CMTYPE('
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      DO 1311 i=1,num_cms
        text='      '//cmtype(i)(:lnblnk1(cmtype(i)))
        IF((i.LT.num_cms))text=text(:lnblnk1(text))//','
        write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
1311  CONTINUE
1312  CONTINUE
      text='   )}'
      write(10,err=1260,fmt='(A)') text(:lnblnk1(text))
      DO 1321 i=1,num_cms
        DO 1331 j=1,2
          IF ((j.EQ.1)) THEN
            the_file=cmtype(i)(:lnblnk1(cmtype(i)))//'_cm.mortran'
          ELSE
            the_file=cmtype(i)(:lnblnk1(cmtype(i)))//'_macros.mortran'
          END IF
          file_name=cm_home(:lnblnk1(cm_home))//the_file(:lnblnk1(the_fi
     *    le))
          open(11,file=file_name(:lnblnk1(file_name)),status='old',err=1
     *    020)
          line=0
1341      CONTINUE
            read(11,end=1350,err=1040,fmt='(A)') text
            line=line+1
1361        CONTINUE
              iindex=index(text,'$'//cmtype(i)(:lnblnk1(cmtype(i))))
              IF((iindex.EQ.0))GO TO1362
              text=text(:iindex-1)//cmid(i)(:lnblnk1(cmid(i)))// text(ii
     *        ndex+lnblnk1(cmtype(i))+1:)
            GO TO 1361
1362        CONTINUE
            IF ((lnblnk1(text).GT.80)) THEN
              write(6,'(/a)')'**** Warning: '
              write(6,*)'In file ',file_name(:lnblnk1(file_name))
              write(6,*)'Line ',line,' is >80 characters long after repl
     *acing $CM'
              write(6,'(a)')'with $CMID.  This may cause problems during
     * compile.'
            END IF
            write(8+j,err=1260,fmt='(A)') text(:lnblnk1(text))
          GO TO 1341
1342      CONTINUE
1350      close(11)
1331    CONTINUE
1332    CONTINUE
1321  CONTINUE
1322  CONTINUE
      text='%N "Turn off MORTRAN listing'
      write(9,err=1260,fmt='(A)') text(:lnblnk1(text))
      close(9)
      close(10)
      call exit(0)
1120  CONTINUE
      write(6,'(/a)') '**** Error: beamnrc.spec does not exist'
      GOTO 1090
1140  CONTINUE
      write(6,'(/a)') '**** Error:'
      IF (( iindex .EQ. 1 )) THEN
        write(6,*) ' Failed to find SOURCES definition in beamnrc.spec'
      ELSE
        write(6,*) ' Failed to find LIB_SOURCES definition in beamnrc.sp
     *ec'
      END IF
      GOTO 1090
1030  write(6,'(/a)')'**** Error: '
      write(6,*)'Something is wrong with the format'
      write(6,*)'of ',spec_home(:lnblnk1(spec_home))//spec_module(:lnbln
     *k1(spec_module))//'.module'
      write(6,'(/a)')'**** Quitting now.'
      GOTO 1090
1040  write(6,'(/a)')'**** Error: '
      write(6,*)'Error reading text ',text
      write(6,*)'in ',file_name(:lnblnk1(file_name))
      write(6,'(/a)')'**** Quitting now.'
      GOTO 1090
1020  write(6,'(/a)')'**** Error: '
      write(6,*)'Could not open ',file_name(:lnblnk1(file_name))
      write(6,'(/a)')'**** Quitting now.'
      GOTO 1090
1260  write(6,'(/a)')'**** Error: '
      write(6,*)'Error writing text ',text(:lnblnk1(text))
      write(6,*)'in file ',file_name(:lnblnk1(file_name))
      write(6,'(/a)')'**** Quitting now.'
      GOTO 1090
1090  call exit(1)
      return
      end
C***************************************************************************
C
C   This file was automatically generated by:
C   EGSnrc-configure-windows.exe version 1.0
C   It contains various subroutines and functions for date, time,
C   CPU time, host name, etc.
C
C   Attention: all changes will be lost the next time you run
C   EGSnrc-configure-windows.exe.
C
C***************************************************************************

C##############################################################################
C
C  EGSnrc egs_system subroutine v1
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C*****************************************************************************
C egs_system(command)  runs a system command and returns the status
C                      command must be null-terminated
C*****************************************************************************
      integer function egs_system(command)
      character*(*) command
      integer system, istat
      istat = system(command)
      egs_system = istat
      return
      end

C##############################################################################
C
C  EGSnrc egs_isdir subroutine v2
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C*****************************************************************************
C
C  egs_isdir(file_name)  Returns .true., if the string file_name points to
C                        an existing directory.
C                        This version uses the inquire intrinsic, which
C                        only tells us whether file_name points to an existing
C                        object (file, directory, link, etc.), not specifically
C                        an existing directory. But that's all I need.
C                        The reason for having different versions of this
C                        function is that on some systems (e.g. OSF1) the
C                        inquire intrinsic only returns .true. for existing
C                        files, not for existing directories.
C
C*****************************************************************************

      logical function egs_isdir(file_name)
      implicit none
      character*(*) file_name
      logical ex
      inquire(file=file_name,exist=ex)
      egs_isdir = ex
      return
      end

C##############################################################################
C
C  EGSnrc date subroutines v1
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C***************************************************************************
C
C   egs_fdate(out):  print a 24 char date and time string in the form
C                         'Tue Mar 18 08:16:42 2003'
C                    to the unit specified by out without end of line
C                    i.e. the sequence
C                    write(6,'(a,$)') 'Today is '
C                    call egs_fdate(6)
C                    write(6,'(a)') '. Have a nice date'
C                    should result in something like
C                    Today is Tue Mar 18 08:16:42 2003. Have a nice date
C                    printed to unit 6.
C
C***************************************************************************

      subroutine egs_fdate(ounit)
      integer ounit
      character*24 string
      call fdate(string)
      write(ounit,'(a,$)') string
      end

C***************************************************************************
C
C   egs_get_fdate(string) assignes a 24 char date and time string to string
C                         string must be at least 24 chars long, otherwise
C                         this subroutine has no effect.
C
C***************************************************************************

      subroutine egs_get_fdate(string)
      character*(*) string
      if( len(string).ge.24 ) call fdate(string)
      return
      end

C##############################################################################
C
C  EGSnrc egs_date_and_time subroutine v1
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


      subroutine egs_date_and_time(vnow)
      integer vnow(8)
      character dat*8,tim*10,zon*5
      call date_and_time(dat,tim,zon,vnow)
      return
      end

C##############################################################################
C
C  EGSnrc egs_date subroutine v1
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C*************************************************************************
C
C egs_date(ounit): print a 11 char string in the form
C                     '18-Mar-2003'
C                  to the unit specified by ounit
C                  No end of line character is inserted
C
C*************************************************************************

      subroutine egs_date(ounit)
      integer ounit
      character string*24, dat*11
      call fdate(string)
      dat(1:2) = string(9:10)
      dat(3:3) = '-'
      dat(4:6) = string(5:7)
      dat(7:7) = '-'
      dat(8:11) = string(21:24)
      write(ounit,'(a,$)') dat
      return
      end

C##############################################################################
C
C  EGSnrc egs_time subroutine v1
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C $Id: egs_time_v1.f,v 1.1 2003/07/11 19:17:08 iwan Exp $
C*************************************************************************
C
C egs_time(ounit): print a 8 char string in the form hh:mm:ss
C                  to the unit specified by ounit
C                  No end of line character is inserted
C
C*************************************************************************

      subroutine egs_time(ounit)
      integer ounit
      character string*24
      call fdate(string)
      write(ounit,'(a,$)') string(12:19)
      return
      end

C##############################################################################
C
C  EGSnrc seconds timing subroutines v1
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C*****************************************************************************
C
C real function egs_secnds(t0): returns seconds passed since midnight minus t0
C
C*****************************************************************************

      real function egs_secnds(t0)
      real t0,t1
      character dat*8,tim*10,zon*5
      integer values(8)
      call date_and_time(dat,tim,zon,values)
      t1 = 3600.*values(5) + 60.*values(6) + values(7) + 0.001*values(8)
      egs_secnds = t1 - t0
      return
      end

C*****************************************************************************
C
C real function egs_tot_time()
C
C   On first call returns seconds passed since 1/1/1970
C   On subsequent calls returns
C     - seconds since last call, if flag = 0
C     - seconds since first call, else
C
C*****************************************************************************

      real function egs_tot_time(flag)
      integer flag
      character dat*8,tim*10,zon*5
      integer vnow(8), vlast(8),i
      real t,egs_time_diff,t0
      data vlast/1970,1,1,5*0/,t0/-1/
      save vlast,t0
      call date_and_time(dat,tim,zon,vnow)
      t = egs_time_diff(vlast,vnow)
      do i=1,8
        vlast(i)=vnow(i)
      end do
      if( t0.lt.0 ) then
        t0 = 0
        egs_tot_time = t
      else
        t0 = t0 + t
        if(flag.eq.0) then
          egs_tot_time = t
        else
          egs_tot_time = t0
        end if
      end if
      return
      end

C##############################################################################
C
C  EGSnrc date and time subroutines
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C****************************************************************************
C
C Returns the time difference between vstart and vend
C vstart and vend are integer arrays of dimension 8 with elements
C corresponding to the specification of the data_and_time routine, i.e.
C   array(1) = year
C   array(2) = month of the year   (1...12)
C   array(3) = day of the month    (1...31)
C   array(4) = difference in minutes from UTC
C   array(5) = hour of the day     (1...23)
C   array(6) = minute of the hour  (1...59)
C   array(7) = seconds of the minute (1...59)
C   array(8) = miliseconds of the second (1...999)
C
C Note: this implementation ignores the time difference from UTC field
C
C*****************************************************************************
      real function egs_time_diff(vstart,vend)
      integer    vstart(8),vend(8)
      real       egs_time_diff_o
      if( vend(1).lt.vstart(1).or.
     &  (vend(1).eq.vstart(1).and.vend(2).lt.vstart(2)) ) then
        egs_time_diff = -egs_time_diff_o(vend,vstart)
      else
        egs_time_diff = egs_time_diff_o(vstart,vend)
      end if
      return
      end

C******************************************************************************
C
C day difference between the dates specified by the integer arrays vstart and
C vend. The arrays are v(1)=year, v(2)=month, v(3)=day
C
C******************************************************************************
      integer function egs_day_diff(vstart,vend)
      integer vstart(3),vend(3),egs_day_diff_o
      if( vend(1).lt.vstart(1).or.
     &  (vend(1).eq.vstart(1).and.vend(2).lt.vstart(2)) ) then
        egs_day_diff = -egs_day_diff_o(vend,vstart)
      else
        egs_day_diff = egs_day_diff_o(vstart,vend)
      end if
      return
      end

C******************************************************************************
C
C Returns a 3-letter abreviation of the day of the week in the string day,
C given a day specified by the integer array values
C   values(1)=year, values(2)=month, values(3)=day
C
C******************************************************************************
      subroutine egs_weekday(values,day)
      character*(*) day
      integer       values(3)
      integer       days,vtmp(3),egs_day_diff,aux
      character*3   wdays(7)
      data wdays/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
      vtmp(1) = 1970
      vtmp(2) = 1
      vtmp(3) = 1
      days = egs_day_diff(vtmp,values)
      aux = mod(days,7)
      days = 4 + aux
      if( days.gt.7 ) days = days - 7
      day(:len(day)) = ' '
      aux = min(len(day),3)
      day(:aux) = wdays(days)(:aux)
      return
      end

C*****************************************************************************
C
C Same as egs_day_diff above, but assumes that vend specifies a later date
C than vstart.
C
C*****************************************************************************
      integer function egs_day_diff_o(vstart,vend)
      integer vstart(3),vend(3)
      integer    days
      logical    next_month
      integer    tm,m,ty,y
      integer    mdays(12)
      data       mdays/31,28,31,30,31,30,31,31,30,31,30,31/
      days = 0
      ty = vstart(1)
      y  = vend(1)
      tm = vstart(2)
      m  = vend(2)
      next_month = .true.
      do while(next_month)
        if( tm.eq.m.and.ty.eq.y ) then
          next_month = .false.
        else
          days = days + mdays(tm)
          if( tm.eq.2.and.mod(ty,4).eq.0 ) days = days + 1
          tm = tm + 1
          if( tm.gt.12 ) then
            ty = ty + 1
            tm = 1
          end if
        end if
      end do
      days = days + vend(3) - vstart(3)
      egs_day_diff_o = days
      return
      end

C******************************************************************************
C
C Same as egs_time_diff above, but assumes that vend specifies a later date
C than vstart.
C
C******************************************************************************
      real function egs_time_diff_o(vstart,vend)
      integer    vstart(8),vend(8)
      integer    days,hours,minutes,secs,msecs
      integer    egs_day_diff_o
      days = egs_day_diff_o(vstart,vend)
      hours = vend(5) - vstart(5)
      minutes = vend(6) - vstart(6)
      secs = vend(7) - vstart(7)
      msecs = vend(8) - vstart(8)
      egs_time_diff_o = 3600.*(24.*days+hours)+60.*minutes+secs+
     &                  0.001*msecs
      return
      end

C******************************************************************************
C
C Returns in month a 3-letter abreviation of the month specified by mo, if
C mo is between 1 and 12, or an empty string otherwise.
C
C******************************************************************************
      subroutine egs_month(mo,month)
      integer mo
      character*(*) month
      integer iaux
      character*3   months(12)
      data months/'Jan','Feb','Mar','Apr','May','Jun', 'Jul','Aug','Sep'
     *,'Oct','Nov','Dec'/
      iaux = min(len(month),3)
      month(:len(month)) = ' '
      if( mo.ge.1.and.mo.le.12 ) month(:iaux) = months(mo)(:iaux)
      return
      end

C******************************************************************************
C
C Converts a 3-letter abreviation of a month to its corresponding integer
C value, if the string month is a valid month, or -1 otherwise.
C
C******************************************************************************
      integer function egs_conver_month(month)
      character*3 month
      character*3 months(12)
      integer i
      data months/'Jan','Feb','Mar','Apr','May','Jun', 'Jul','Aug','Sep'
     *,'Oct','Nov','Dec'/
      do i=1,12
        if( month.eq.months(i) ) then
          egs_conver_month = i
          return
        end if
      end do
      egs_conver_month = -1
      return
      end


C##############################################################################
C
C  EGSnrc egs_etime subroutine
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C*****************************************************************************
C
C real function egs_etime(): returns CPU time consumed since the start of
C                            the program
C
C*****************************************************************************

      real function egs_etime()
      real tarray(2),etime
      egs_etime = etime(tarray)
      return
      end

C##############################################################################
C
C  EGSnrc canonical system name subroutines
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C******************************************************************************
C
C Print the canonical system name as determined by the config.guess script
C or the Windows installation program to the unit specified by ounit.
C
C*****************************************************************************

      subroutine egs_print_canonical_system(ounit)
      integer ounit
      write(6,'(a,$)') 'win3264'
      return
      end

C******************************************************************************
C
C Assign the canonical system name as determined by the config.guess script
C or the Windows installation program to the string pointed to by res
C
C******************************************************************************

      subroutine egs_get_canonical_system(res)
      character*(*) res
      integer l1,l2
      l1 = lnblnk1('win3264')
      l2 = len(res)
      res(:l2) = ' '
      if( l2.ge.l1 ) then
        res(:l1) = 'win3264'
      else
        res(:l2) = 'win3264'
      end if
      return
      end


C##############################################################################
C
C  EGSnrc configuration name subroutines
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C******************************************************************************
C
C Print the configuration name as specified suring the configuration
C process to the unit specified by ounit.
C
C*****************************************************************************

      subroutine egs_print_configuration_name(ounit)
      integer ounit
      write(6,'(a,$)') 'win6432'
      return
      end

C******************************************************************************
C
C Assign the configuration name as specified suring the configuration
C process to the string pointed to by res
C
C******************************************************************************

      subroutine egs_get_configuration_name(res)
      character*(*) res
      integer l1,l2
      l1 = lnblnk1('win6432')
      l2 = len(res)
      res(:l2) = ' '
      if( l2.ge.l1 ) then
        res(:l1) = 'win6432'
      else
        res(:l2) = 'win6432'
      end if
      return
      end


C##############################################################################
C
C  EGSnrc hostname subroutines v1
C  Copyright (C) 2015 National Research Council Canada
C
C  This file is part of EGSnrc.
C
C  EGSnrc is free software: you can redistribute it and/or modify it under
C  the terms of the GNU Affero General Public License as published by the
C  Free Software Foundation, either version 3 of the License, or (at your
C  option) any later version.
C
C  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
C  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
C  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
C  more details.
C
C  You should have received a copy of the GNU Affero General Public License
C  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
C
C##############################################################################
C
C  Author:          Iwan Kawrakow, 2003
C
C  Contributors:
C
C##############################################################################


C*****************************************************************************
C
C Print the host name to the unit specified by ounit without inserting
C a new line character.
C
C*****************************************************************************

      subroutine egs_print_hostnm(ounit)
      integer ounit
      character*256 string
      integer res,hostnm,lnblnk1
      res = hostnm(string)
      if( res.ne.0 ) then
        write(6,'(a,a)') 'hostnm returned with a non-zero status '
        stop
      end if
      write(ounit,'(a,$)') string(:lnblnk1(string))
      return
      end

C*****************************************************************************
C
C Assign the host name to the string pointed to be hname.
C
C*****************************************************************************

      subroutine egs_get_hostnm(hname)
      character*(*) hname
      character*256 string
      integer res,hostnm,lnblnk1,l1,l2,l
      res = hostnm(string)
      if( res.ne.0 ) then
        write(6,'(a,a)') 'hostnm returned with a non-zero status '
        stop
      end if
      l1 = lnblnk1(string)
      l2 = len(hname)
      hname(:l2) = ' '
      l = min(l1,l2)
      hname(:l) = string(:l)
      return
      end

