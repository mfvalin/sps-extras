program fill_levels
  use ISO_C_BINDING
  implicit none
  include 'convert_ip123.inc'
  integer, parameter :: MAX_NAMES=16
  integer :: levels, nvars
  character(len=4096) :: filename
  character(len=4), dimension(MAX_NAMES) :: names
  integer :: i, j, status, length, arb_code, iun, key, lastread, errors
  character(len=16) :: string
  integer, dimension(:), allocatable :: codes
  real*4, dimension(:), pointer :: z
  real*4 :: temp
  real*8 :: hours
  integer :: ni, nj, nk, nrec, ni0, nj0, nk0, arg0
  integer :: nbits,datyp,ip1,ip2,ip3,dateo,deet,npas
  integer :: datev
  integer :: ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,extra1,extra2,extra3
  character(len=1) :: grtyp
  character(len=4) :: nomvar
  character(len=2) :: typvar
  character(len=12) :: etiket
  integer, external :: fstouv, fnom, fstinf, fstluk, fstlir, fst_edit_dir
  logical :: verify_only

  filename = 'DoesNotExistYet'
  verify_only = .false.
  names = "    "
  levels = 0
  errors = 0
  allocate(z(1000000))

  arg0 = 0
  CALL GET_COMMAND_ARGUMENT(arg0 + 1 , filename, length, status)
  if(status .ne. 0) goto 999
  if(trim(filename) == '--help' .or. trim(filename) == '-h') then   ! print usage requested
    call print_usage
    goto 888
  endif
  if(trim(filename) == '--check') then                              ! --check , bump argument base count
    verify_only = .true.
    print 100,"INFO: VERIFY ONLY"
    arg0 = arg0 + 1
  endif

  CALL GET_COMMAND_ARGUMENT(arg0 + 1 , filename, length, status)    ! get file name
  if(status .ne. 0) goto 999
  print 100,"INFO: processing '"//trim(filename)//"'"

  CALL GET_COMMAND_ARGUMENT(arg0 + 2 , string, length, status)      ! get number of levels
  if(status .ne. 0) goto 999

  read(string,*,iostat=status) levels                               ! decode integer
  if(status .ne. 0) goto 999   ! ouch
  print 100,"INFO: number of levels =",levels

  allocate(codes(levels))
  do i = 1, levels
    temp = i
    arb_code = 3
    call CONVIP_plus( codes(i), temp, arb_code, 2, string, .false. )
  enddo
  print *,'INFO: codelist=',codes

  CALL GET_COMMAND_ARGUMENT(arg0 + 3 , string, length, status)
  if(status .ne. 0) goto 999

  read(string,*,iostat=status) nvars
  if(status .ne. 0) goto 999
  print 100,"INFO: number of variables =",nvars

  do i = 1, nvars
    CALL GET_COMMAND_ARGUMENT(arg0 + i + 3 , names(i), length, status)
    if(status .ne. 0) goto 999
    print 100,"INFO: variable",i," = '"//names(i)//"'"
  enddo

  iun = 0
  status = fnom(iun,trim(filename),'STD+RND+OLD',0)
  if(status < 0) then
    print 100,'ERROR: cannot open input file'
    goto 888
  endif
  status = fstouv(iun,'RND')
  if(status < 0) then
    print 100,'ERROR: error opening standard file'
    goto 888
  endif

  do i = 1, nvars
    nomvar = names(i)
    dateo = -1
    datev = -1
!    key = fstinf(iun,ni,nj,nk,datev,"",codes(1),-1,-1,"",nomvar)  ! first level
    key = fstinf(iun,ni,nj,nk,datev,"",-1,-1,-1,"",nomvar)  ! first level
    if(key >= 0) then             ! record found (ip1 == -1)
      if(size(z) < ni*nj) then    ! z is not big enough
        deallocate(z)
        allocate(z(ni*nj+10))
      endif
      call fstprm(key,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
                  typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
                  swa,lng,dltf,ubc,extra1,extra2,extra3)     ! get metadata
      if(ip1 == 0 .and. (.not. verify_only)) then            ! plug 0 mb as codes(1)
        ip1 = codes(1)
        status = fst_edit_dir(key,dateo,deet,npas,ni,nj,nk,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,datyp)
        if(status == 0) then
          print 100,'INFO: forced variable '//trim(nomvar)//' level 0mb to value #1'
        else
          errors = errors + 1
          print 100,'INFO: failed to force variable '//trim(nomvar)//' level 0mb to value #1'
          goto 777
        endif
      endif
      if(.not. verify_only) then
        status = fstluk(z,key,ni,nj,nk)   ! read record
      else
        if(ip1 == codes(1)) then
          print 100,'INFO: found level #',1,' of variable "'//nomvar//'"'
        else
          print 100,'ERROR: level #',1,' of variable "'//nomvar//'" is missing'
        endif
      endif
    else
      print 100,'ERROR: level #',1,' of variable "'//nomvar//'" is missing'
      if(.not. verify_only) then
        errors = errors + 1
        goto 777
      endif
    endif
    lastread = 1
    do j = 2, levels  ! try to read all levels, if a level is not found, write it (using last valid level read)
      hours = deet * npas
      hours = hours / 3600.0
      if(dateo .ne. -1) then   ! no dateo from previous record is available
        call incdatr(datev,dateo,hours)  ! compute date of validity from date of origin for eventual fstecr
      else
        datev = -1
      endif
      key = fstinf(iun,ni0,nj0,nk0,datev,etiket,codes(j),ip2,ip3,typvar,nomvar)   ! look for next
      if(key <0) then   ! arbitrary level not found
        if(verify_only) then
          print 100,'ERROR: level #',j,' of variable "'//nomvar//'" is missing'
          errors = errors + 1
        else   ! write last arbitrary level that was read
          print 100,'INFO: adding level #',lastread,' of variable "'//nomvar//'" as level #',j
          call fstecr(z,z,-nbits,iun,dateo,deet,npas,ni,nj,nk,codes(j),ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,datyp,.false.)
        endif
      else
        if(.not. verify_only) then
          status = fstluk(z,key,ni,nj,nk)  ! read in case next one is missing
          print 100,'INFO: keeping level #',j,' of variable "'//nomvar//'"'
        else
          print 100,'INFO: found level #',j,' of variable "'//nomvar//'"'
        endif
        lastread = j   ! number of last level successfully read
      endif
    enddo
  enddo

  777 continue
  call fstfrm(iun)
  if(errors > 0) then
    print 100,'===== ERRORS IN RUN ====='
    call qqexit(1)
  endif

888 continue
  stop
  100 format(A,I4,1X,A,I4)
999 continue
  print *,"ERROR in arguments, aborting"
  call print_usage
  call qqexit(1)
  stop
end
subroutine print_usage
  print *,"Usage: u.fill_levels [--check] filename nb_levels nvars Var_1 Var_2 ... Var_nvars"
  return
end subroutine print_usage
