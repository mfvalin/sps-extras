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
  character(len=64) :: force_z
  integer :: newig1, newig2
  integer :: renamed, rescaled
  logical :: fix_records

  fix_records = .false.
  nargs = command_argument_count()
  if(nargs < 3) call print_usage
  call GET_COMMAND_ARGUMENT(1,old_file)
  call GET_COMMAND_ARGUMENT(2,new_file)
  call GET_COMMAND_ARGUMENT(3,the_new_date)
  newig1 = -1
  newig2 = -1
  renamed = 0
  rescaled = 0
  if(nargs >= 4) then
    call GET_COMMAND_ARGUMENT(4,force_z)
    if(trim(force_z) == '--FIX') then
      fix_records = .true.
    else
      read(force_z,*,err=777)newig1,newig2
      print *,'forcing Z descriptors to',newig1,newig2
    endif
  endif

  print *,'u.re_tag_scale '//trim(old_file)//' '//trim(new_file)//' '//trim(the_new_date)
  read(the_new_date,*)new_datev

  print *,'INFO: new date of validity: '//trim(the_new_date),new_datev

  print *,'INFO: opening input file '//trim(old_file)
  fstdin = 0
  i = fnom(fstdin,trim(old_file),'STD+RND+OLD+R/O',0)
  print *,'DEBUG: fstdin=',fstdin
  i = fstouv(fstdin,'RND')
  print *,'DEBUG: status of fstouv fstdin =',i

  print *,'INFO: opening output file '//trim(new_file)
  fstdout = 0
  i = fnom(fstdout,trim(new_file),'STD+RND',0)
  print *,'DEBUG: fstdout=',fstdout
  i = fstouv(fstdout,'RND')
  print *,'DEBUG: status of fstouv fstdout =',i

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
    if (nomvar == 'GL  ' .and. fix_records) then  ! rename GL to LG
      renamed = renamed + 1
      if(renamed == 1) print *,'INFO: GL will be renamed LG'
      nomvar = 'LG  '
    endif
    if (nomvar == 'TM  ' .and. fix_records) then  ! TM converted to Celsius
      rescaled = rescaled + 1
      if(rescaled == 1)print *,'INFO: TM will be converted to Celsius '
      call rescale(array,ni,nj,1.0,-273.16)
    endif
    call fstecr(array,array,-nbits,fstdout,datev,deet,npas,ni,nj,nk,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,datyp,.false.)
    status = fstsui(fstdin,ni,nj,nk)
  enddo
  print *,'INFO: number of records read and written =',nrec
  if(rescaled > 0) print *,'INFO: number of rescaled TM records:',rescaled
  if(renamed > 0)  print *,'INFO: number of renamed GLrecords:',renamed

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