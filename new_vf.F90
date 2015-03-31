program re_tag_scale
  use ISO_C_BINDING
  implicit none
  include 'convert_ip123.inc'
  integer :: nargs
  character(len=1024) :: old_file, new_file, the_new_date
  integer :: i, j, k, status
  integer :: fstdin, fstdout
  integer, external :: fnom, fstouv, fstinf, fstsui, fstluk, fstlir
  real,    dimension(:,:,:), allocatable :: vf
  real,    dimension(:,:,:), allocatable :: sums
  real,    dimension(:,:), allocatable :: glf, ga, mg, diff
  integer :: ni, nj, nk, nrec
  integer :: nbits,datyp,ip1,ip2,ip3,dateo,deet,npas
  integer :: datev, new_datev
  integer :: ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,extra1,extra2,extra3
  character(len=1) :: grtyp
  character(len=4) :: nomvar
  character(len=2) :: typvar
  character(len=12) :: etiket
  character(len=16) :: dummy
  integer :: the_kind=KIND_ARBITRARY
  real :: p
  integer :: ip, increases, decreases, the_same
  integer, dimension(26) :: stab   ! table for sums
  real :: extra, factor

  nargs = command_argument_count()
  if(nargs /= 2) call exit(1)
  call GET_COMMAND_ARGUMENT(1,old_file)
  call GET_COMMAND_ARGUMENT(2,new_file)

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

  stab     = 1  ! default vegetation
  stab(21) = 2  ! cities
  stab(2)  = 3  ! glacier fraction
  stab(1)  = 4  ! water fraction
  stab(3)  = 4  ! water fraction

  nrec = 0
  status = fstinf(fstdin,ni,nj,nk,-1,"",-1,-1,-1,"","VF")
  if(statuS < 0) THEN
    PRINT *,"ERROR: no vf record found"
    CALL EXIT(1)
  ELSE
    print *,"INFO: ni,nj=",ni,nj
  ENDIF
  call fstprm(status,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
              typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
              swa,lng,dltf,ubc,extra1,extra2,extra3)
  allocate( glf(ni,nj) , ga(ni,nj), mg(ni,nj), diff(ni,nj))
  status = fstlir(glf,fstdin,ni,nj,nk,-1,"",-1,-1,-1,"","GLF")
!
  allocate(sums(ni,nj,0:4))
  sums = 0.0
  allocate(vf(ni,nj,26))
  do i = 1,26
    p = i
    call CONVIP_plus( ip, p, the_kind, +2, dummy, .false. )
    status = fstlir(vf(1,1,i),fstdin,ni,nj,nk,-1,"",ip,-1,-1,"","VF")  ! read VF
    sums(:,:,0) = sums(:,:,0) + vf(:,:,i)                              ! sum of VF fractions
    if(status < 0) THEN
      PRINT *,"ERROR: missing VF for code=",i
      call exit(1)
    endif
  enddo
  print *,'sum of VF (old) max, min',maxval(sums(:,:,0)),minval(sums(:,:,0))
  diff = glf - vf(:,:,2)   ! new VF 2 minus old VF 2
  vf(:,:,2) = glf          ! put glf into VF 2
!
  sums = 0.0
  do i = 1,26    ! redo the sums
    sums(:,:,stab(i)) = sums(:,:,stab(i)) + vf(:,:,i)
  enddo
  sums(:,:,0) = sums(:,:,1) + sums(:,:,2) + sums(:,:,3) + sums(:,:,4)
  print *,'sum of VF (new) max, min',maxval(sums(:,:,0)),minval(sums(:,:,0))
!
! compute new VF
!
  increases = 0
  decreases = 0
  the_same = 0
  do j = 1 , nj
  do i = 1 , ni
    if(diff(i,j) == 0.0) then   ! no change
      the_same = the_same + 1
      cycle
    endif
    if(diff(i,j) > 0.0) then   ! total of VF is too high
!
      decreases = decreases + 1
      extra = diff(i,j)              ! have to reduce VF sum by this amount
      if(extra <= sums(i,j,1) .and. sums(i,j,1) /= 0.0) then  ! can do it by reducing veg fractions
        factor = (sums(i,j,1) - extra) / sums(i,j,1)
      else
        factor = 0.0
      endif
      vf(i,j,4:20) = vf(i,j,4:20) * factor
      vf(i,j,22:26) = vf(i,j,22:26) * factor
      extra = max( 0.0 , extra - sums(i,j,1) )   ! extra left (my be zero)
      if(extra <= sums(i,j,4) .and. sums(i,j,4) /= 0.0) then              ! can do it by reducing veg fractions
        factor = (sums(i,j,4) - extra) / sums(i,j,4)
      else
        factor = 0.0
      endif
      vf(i,j,1) = vf(i,j,1) * factor
      vf(i,j,3) = vf(i,j,3) * factor
      extra = max( 0.0 , extra - sums(i,j,4) )
      vf(i,j,21) =vf(i,j,21) - extra  ! reduce cities
!
    else                             ! total of VF is too low
!
      increases = increases + 1
      if(sums(i,j,1)+sums(i,j,2) == 0.0) then    ! no vegetation nor cities
        vf(i,j,22) = 1.0 - vf(i,j,1) - vf(i,j,2) - vf(i,j,3)
        cycle
      endif
      extra = -diff(i,j)                  ! have to increase VF sum by this value
      if(sums(i,j,1) > 0) then            ! there is vegetation, bump it
        factor =  (sums(i,j,1) + extra) / sums(i,j,1)
        vf(i,j,4:20) = vf(i,j,4:20) * factor
        vf(i,j,22:26) = vf(i,j,22:26) * factor
        cycle
      endif
      vf(i,j,21) = vf(i,j,21) + extra   ! last resort, increase cities
!
    endif
  enddo
  enddo
  print *,"increases  =",increases
  print *,"decreases  =",decreases
  print *,"same value =",the_same
!
  sums = 0.0
  do i = 1,26    ! redo the sums
    sums(:,:,stab(i)) = sums(:,:,stab(i)) + vf(:,:,i)
  enddo
  sums(:,:,0) = sums(:,:,1) + sums(:,:,2) + sums(:,:,3) + sums(:,:,4)
  print *,'sum of VF (adjusted) max, min',maxval(sums(:,:,0)),minval(sums(:,:,0))
!
  mg = max( 0.0 , 1.0 - vf(:,:,1) - vf(:,:,3) )
!
  do j = 1 , nj
  do i = 1 , ni
    if(mg(i,j) == 0.0) then
      ga(i,j) = 0.0
    else
      ga(i,j) = vf(i,j,2) / mg(i,j)
    endif
  enddo
  enddo

  do i = 1,26 ! write new VF
    p = i
    call CONVIP_plus( ip, p, the_kind, +2, dummy, .false. )
    call fstecr(vf(:,:,i),vf(:,:,i),-32,fstdout,dateo,0,0,ni,nj,1,ip,ip2,ip3,typvar,'VF',etiket,grtyp,ig1,ig2,ig3,ig4,133,.false.)
  enddo
  ip = 0
  call fstecr(ga,ga,-32,fstdout,dateo,0,0,ni,nj,1,ip,ip2,ip3,typvar,'MG',etiket,grtyp,ig1,ig2,ig3,ig4,133,.false.)
  call fstecr(mg,mg,-32,fstdout,dateo,0,0,ni,nj,1,ip,ip2,ip3,typvar,'GA',etiket,grtyp,ig1,ig2,ig3,ig4,133,.false.)

  call fstfrm(fstdin)
  call fstfrm(fstdout)
  stop
end program