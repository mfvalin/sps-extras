program re_tag_scale
  use ISO_C_BINDING
  implicit none
  integer :: nargs
  character(len=1024) :: old_file, new_file, the_new_date
  integer :: i, status
  integer :: fstdin, fstdout
  integer, external :: fnom, fstouv, fstinf, fstsui, fstluk
  real,    dimension(:,:), allocatable :: array
  integer :: ni, nj, nk, nrec
  integer :: nbits,datyp,ip1,ip2,ip3,dateo,deet,npas
  integer :: datev, new_datev
  integer :: ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,extra1,extra2,extra3
  character(len=1) :: grtyp
  character(len=4) :: nomvar
  character(len=2) :: typvar
  character(len=12) :: etiket
  character(len=64) :: force_z, command
  integer :: newig1, newig2
  integer :: renamed, rescaled, suppressed, zeroed
  logical :: fix_records
  character(len=4), dimension(8) :: fix_lvl_vars
  integer :: fix_lvl_ip1 = -1
  integer :: current_arg

  fix_records = .false.
  fix_lvl_vars = ['T5  ','T9  ','TAMN','TAMX','TADM','QADM','UADM','VADM']
  nargs = command_argument_count()
  if(nargs < 3) call print_usage
  call GET_COMMAND_ARGUMENT(1,old_file)
  call GET_COMMAND_ARGUMENT(2,new_file)
  call GET_COMMAND_ARGUMENT(3,the_new_date)
  newig1 = -1
  newig2 = -1
  renamed = 0
  rescaled = 0
  suppressed = 0
  zeroed = 0
  current_arg = 4
  do while(nargs >=current_arg)
    call GET_COMMAND_ARGUMENT(current_arg,command)
    current_arg = current_arg + 1
    if(trim(command) == '--FIX' .or. trim(command) == '--fix') then
      fix_records = .true.
      print *,'INFO: fix_records mode active'
      cycle
    endif
    if(trim(command) == '--IP1' .or. trim(command) == '--ip1') then
      if(nargs < current_arg) goto 777
      call GET_COMMAND_ARGUMENT(current_arg,command)
      current_arg = current_arg + 1
      read(command,*,err=777)fix_lvl_ip1
      print 100,'INFO: setting ip1 to 0 if ip1 == ',trim(command)
      print 100,'      for variables :',fix_lvl_vars
100 format(A,20A5)
      cycle
    endif
    read(command,*,err=777)newig1,newig2
    print *,'forcing Z descriptors to',newig1,newig2
  enddo

  print *,'u.re_tag_scale '//trim(old_file)//' '//trim(new_file)//' '//trim(the_new_date)
  read(the_new_date,*)new_datev

  print *,'INFO: new date of validity: '//trim(the_new_date),new_datev

  print *,'INFO: opening input file '//trim(old_file)
  fstdin = 0
  i = fnom(fstdin,trim(old_file),'STD+RND+OLD+R/O',0)
  if(i < 0) goto 777
!  print *,'DEBUG: fstdin=',fstdin
  i = fstouv(fstdin,'RND')
  if(i < 0) goto 777
!  print *,'DEBUG: status of fstouv fstdin =',i

  print *,'INFO: opening output file '//trim(new_file)
  fstdout = 0
  i = fnom(fstdout,trim(new_file),'STD+RND',0)
  if(i < 0) goto 777
!  print *,'DEBUG: fstdout=',fstdout
  i = fstouv(fstdout,'RND')
  if(i < 0) goto 777
!  print *,'DEBUG: status of fstouv fstdout =',i

  call fstopc('MSGLVL','ERRORS',0)
  nrec = 0
  allocate(array(1000,1000))
  status = fstinf(fstdin,ni,nj,nk,-1,"",-1,-1,-1,"","")
  do while(status >= 0)
    i = fstluk(array,status,ni,nj,nk)
    nrec = nrec + 1
!
!   transform the record as and if needed
!
    call fstprm(status,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
                typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
                swa,lng,dltf,ubc,extra1,extra2,extra3)
    if(new_datev > 0) then
      deet = 0
      npas = 0
      datev = new_datev
    else
      datev = dateo
    endif
    if(newig1 > 0 .and. newig2 > 0 .and. grtyp == 'Z') then
      ig1 = newig1
      ig2 = newig2
    endif
    if( (mod(datyp,128) == 5 .or. mod(datyp,128) == 1) .and. nbits == 64) then
      call copy84(array,ni,nj,array)
      call printstats(array,ni,nj)
      nbits = 32
    endif
    if(fix_lvl_ip1 >= 1) then  ! set ip1 to zero for some variables
      if(any(nomvar==fix_lvl_vars)) then
        if(ip1 == fix_lvl_ip1) then
          print *,'INFO: ip1 set to zero for ',nomvar
          ip1 = 0
          zeroed = zeroed + 1
        else
          print *,'INFO: record suppressed, nomvar , ip1 :',nomvar,ip1
          suppressed = suppressed + 1
          goto 111
        endif
      endif
    endif
    if (nomvar == 'I2  ' .and. fix_records .and. ip1 == 0) then
      ip1 = 59868832   ! arbitrary code 1
      print *,'INFO: I2 level changed from 0 mb to 1 arbitratry'
    endif
    if (nomvar == 'LG  ' .and. fix_records) goto 111
    if (nomvar == 'GL  ' .and. fix_records) then  ! rename GL to LG
      renamed = renamed + 1
      print *,'INFO: GL renamed LG'
      nomvar = 'LG  '
    endif
    if (nomvar == 'AD  ' .and. fix_records) goto 111
    if (nomvar == 'FI  ' .and. fix_records) then  ! rename GL to LG
      renamed = renamed + 1
      print *,'INFO: FI renamed AD'
      nomvar = 'AD  '
    endif
    if (nomvar == 'N4  ' .and. fix_records) goto 111
    if (nomvar == 'FB  ' .and. fix_records) then  ! rename GL to LG
      renamed = renamed + 1
      print *,'INFO: FB renamed N4'
      nomvar = 'N4  '
    endif
    if (nomvar == 'TM  ' .and. fix_records) then  ! TM converted to Celsius
      rescaled = rescaled + 1
      print *,'INFO: TM converted to Celsius '
      call rescale(array,ni,nj,1.0,-273.16)
    endif
    call fstecr(array,array,-nbits,fstdout,datev,deet,npas,ni,nj,nk,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,datyp,.false.)
111 status = fstsui(fstdin,ni,nj,nk)
  enddo
  print *,'INFO: number of records read and written =',nrec
  if(rescaled > 0) print *,'INFO: number of converted records:',rescaled
  if(renamed > 0)  print *,'INFO: number of renamed records:',renamed
  if(suppressed > 0)  print *,'INFO: number of suppressed records:',suppressed
  if(zeroed > 0)  print *,'INFO: number of records with ip1 set to zero:',zeroed

  call fstfrm(fstdin)
  call fstfrm(fstdout)
777 continue
  stop
end program
subroutine print_usage()
  implicit none
  print *,'USAGE: u.re_tag_date old_standard_file new_standard_file new_date'
  call qqexit(1)
end subroutine
subroutine printstats(z,ni,nj)
  implicit none
  integer, intent(IN) :: ni,nj
  real, dimension(ni,nj), intent(IN) :: Z
  integer :: i,j
  real *8 :: total
  total = 0.0
  do j = 1 , nj
  do i = 1 , ni
    total = total + z(i,j)
  enddo
  enddo
  print *,'Average value=',total/(ni*nj)
end subroutine
subroutine copy84(z,ni,nj,z8)
  implicit none
  integer, intent(IN) :: ni,nj
  real*8, dimension(ni,nj), intent(IN) :: z8
  real, dimension(ni,nj), intent(OUT) :: z
  real, dimension(ni,nj) :: temp
  temp = z8
  z = temp
  return
end subroutine
subroutine rescale(array,ni,nj,factor,offset)
  implicit none
  integer, intent(IN) :: ni, nj
  real, dimension(ni,nj), intent(INOUT) :: array
  real, intent(IN) :: factor, offset
  array = array*factor + offset
  return
end