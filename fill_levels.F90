program fill_levels
  use ISO_C_BINDING
  implicit none
  include 'convert_ip123.inc'
  integer, parameter :: MAX_NAMES=16
  integer :: levels, nvars
  character(len=4096) :: filename
  character(len=4), dimension(MAX_NAMES) :: names
  integer :: i, j, status, length, arb_code, iun, key
  character(len=16) :: string
  integer, dimension(:), allocatable :: codes
  real*4, dimension(:), pointer :: z
  real*4 :: temp
  integer :: ni, nj, nk, nrec
  integer :: nbits,datyp,ip1,ip2,ip3,dateo,deet,npas
  integer :: datev, new_datev
  integer :: ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,extra1,extra2,extra3
  character(len=1) :: grtyp
  character(len=4) :: nomvar
  character(len=2) :: typvar
  character(len=12) :: etiket
  integer, external :: fstouv, fnom, fstinf, fstluk

  filename = 'DoesNotExistYet'
  names = "    "
  levels = 0
  allocate(z(1000000))

  CALL GET_COMMAND_ARGUMENT(1 , filename, length, status)
  if(status .ne. 0) goto 999
  print 100,"INFO: processing '"//trim(filename)//"'"

  CALL GET_COMMAND_ARGUMENT(2 , string, length, status)
  if(status .ne. 0) goto 999

  read(string,*,iostat=status) levels
  if(status .ne. 0) goto 999
  print 100,"INFO: number of levels =",levels

  allocate(codes(levels))
  do i = 1, levels
    temp = i
    arb_code = 3
    call CONVIP_plus( codes(i), temp, arb_code, 2, string, .false. )
  enddo
  print *,'INFO: codelist=',codes

  CALL GET_COMMAND_ARGUMENT(3 , string, length, status)
  if(status .ne. 0) goto 999

  read(string,*,iostat=status) nvars
  if(status .ne. 0) goto 999
  print 100,"INFO: number of variables =",nvars

  do i = 1, nvars
    CALL GET_COMMAND_ARGUMENT(i+3 , names(i), length, status)
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
    key = fstinf(iun,ni,nj,nk,-1,"",codes(1),-1,-1,"",names(i))  ! first level
    if(size(z) < ni*nj) then
      deallocate(z)
      allocate(z(ni*nj+10))
    endif
    status = fstluk(z,key,ni,nj,nk)
    call fstprm(key,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
                typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
                swa,lng,dltf,ubc,extra1,extra2,extra3)
    do j = 2, levels  ! try to read all levels, if a level is not found, write it
    enddo
  enddo

  call fstfrm(iun)

888 continue
  stop
  100 format(A,I4,1X,A)
999 continue
  print *,"ERROR in arguments, aborting run"
  stop
end
