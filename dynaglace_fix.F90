program fix_ice
  use ISO_C_BINDING
  implicit none
  include 'convert_ip123.inc'
  integer, parameter :: NBINS = 8
  integer, parameter :: ILAYS = 4
  integer, parameter :: SLAYS = ILAYS-1
  real, PARAMETER :: RAYT     =.637122e+7 !  mean radius of the earth
  real *8, parameter :: ONE=1.0
  integer :: ni, nj, nk
  real, dimension(:),     allocatable :: zglmf, zmg, zglacier, zglvol, zgldepth, zglnum
  real, dimension(:,:),   allocatable :: zbfrac, zglfracb, ztskin, zsnoln, zbheight, zblheight, zdxdy
  real, dimension(:,:,:), allocatable :: ztbot, zsnodp, zsnoden
  integer, external :: fnom, fstouv, fstinf, fstluk, fstfrm, fstlir
  character(len=128) :: geofile, outfile
  integer :: statusg, statuso, fstgeo, fstout, keyg, keygla, keyglo
  real, dimension(:), allocatable :: latp, lonp
  integer :: nbits,datyp,ip1,ip2,ip3,dateo,deet,npas
  integer :: ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,extra1,extra2,extra3
  character(len=1) :: grtyp
  character(len=4) :: nomvar
  character(len=2) :: typvar
  character(len=12) :: etiket
  integer :: lni, lnj, lnk
  integer :: i, j, ip, kind
  real :: p
  real *8 :: dlat, dlon, pi, m2, coslat
  character(len=1) :: dummy


  pi = acos(-ONE)

  call GET_COMMAND_ARGUMENT(1,geofile)
  call GET_COMMAND_ARGUMENT(2,outfile)
  fstgeo = 0
  statusg = fnom(fstgeo,trim(geofile),'STD+RND+OLD+R/O',0)
  if(statusg < 0) call exit(1)
  statusg = fstouv(fstgeo,'RND')
  if(statusg < 0) call exit(1)

  fstout = 0
  statuso = fnom(fstout,trim(outfile),'STD+RND',0)
  statuso = fstouv(fstout,'RND')

  keyg = fstinf(fstgeo,ni,nj,nk,-1,"",-1,-1,-1,"","MG")
  call fstprm(keyg,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
              typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
              swa,lng,dltf,ubc,extra1,extra2,extra3)

  allocate(zmg(ni*nj))
  statusg = fstluk(zmg,keyg,ni,nj,nk)

  allocate(lonp(ni),latp(nj))
  keyglo = fstlir(lonp,fstgeo,lni,lnj,lnk,-1,"",ig1,ig2,ig3,"",">>")
  dlon = lonp(2) - lonp(1)
  dlon = dlon * RAYT * pi * 2.0 / 360.0   ! convert to meters
  keygla = fstlir(latp,fstgeo,lni,lnj,lnk,-1,"",ig1,ig2,ig3,"","^^")
  dlat = latp(2) - latp(1)
  dlat = dlat * RAYT * pi * 2.0 / 360.0   ! convert to meters
  m2 = dlat * dlon                        ! nominal grid cell in square meters

  allocate(zdxdy(ni,nj))
  do j = 1 , nj
    coslat = cos(latp(j) * pi / 180.0)
    do i = 1 , ni
      zdxdy(i,j) = m2 * coslat
    enddo
  enddo

  allocate(zglacier(ni*nj))
  keyg = fstlir(zglacier,fstgeo,ni,nj,nk,-1,"",0,0,0,"","GA")

  allocate(zglnum(ni*nj))
  keyg = fstlir(zglnum,fstgeo,ni,nj,nk,-1,"",0,0,0,"","GLNM")

  allocate(zbheight(ni*nj,NBINS))
  do i = 1 , NBINS
    p = 1.0 * i
    kind = KIND_ARBITRARY
    call CONVIP_plus( ip, p, kind, +2, dummy, .false. )
    keyg = fstlir(zbheight(1,i),fstgeo,ni,nj,nk,-1,"",ip,0,0,"","BME")   ! read BME bins
  enddo

  allocate(zbfrac(ni*nj,NBINS))
  do i = 1 , NBINS
    p = 1.0 * i
    kind = KIND_ARBITRARY
    call CONVIP_plus( ip, p, kind, +2, dummy, .false. )
    keyg = fstlir(zbfrac(1,i),fstgeo,ni,nj,nk,-1,"",ip,0,0,"","BFR")   ! read BME bins
  enddo

  allocate(zglmf(ni*nj))
  allocate(zglvol(ni*nj))
  allocate(zgldepth(ni*nj))
  allocate(zglfracb(ni*nj,NBINS))
  allocate(ztskin(ni*nj,NBINS))
  allocate(zsnoln(ni*nj,NBINS))
  allocate(zblheight(ni*nj,NBINS))
  allocate(ztbot(ni*nj,ILAYS,NBINS))
  allocate(zsnodp(ni*nj,SLAYS,NBINS))
  allocate(zsnoden(ni*nj,SLAYS,NBINS))
  zglmf     = 0.0
  zglvol    = 0.0
  zgldepth  = 0.0
  zglfracb  = 0.0
  ztskin    = 0.0
  zsnoln    = 0.0
  zblheight = 0.0
  ztbot     = 0.0
  zsnodp    = 0.0
  zsnoden   = 0.0

  call dynaglace_ini (zglmf, zdxdy, zbfrac, zmg, zglacier, zglfracb, zglvol, zgldepth, zglnum, &
                      ztskin, ztbot, zsnodp, zsnoden, zbheight, zblheight, zsnoln, &
                      ni*nj, NBINS, ILAYS, SLAYS)
  ip1 = 0
  ip2 = 0
  ip2 = 0
  call fstecr(zglmf,zglmf,-32,fstout,dateo,0,0,ni,nj,1,ip1,ip2,ip3,'A','GLMF','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
  call fstecr(zdxdy,zdxdy,-32,fstout,dateo,0,0,ni,nj,1,ip1,ip2,ip3,'A','DXDY','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
  call fstecr(zglvol,zglvol,-32,fstout,dateo,0,0,ni,nj,1,ip1,ip2,ip3,'A','GVOL','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
  call fstecr(zgldepth,zgldepth,-32,fstout,dateo,0,0,ni,nj,1,ip1,ip2,ip3,'A','GLD ','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)

  do i = 1 , NBINS
    p = 1.0 * i
    kind = KIND_ARBITRARY
    call CONVIP_plus( ip, p, kind, +2, dummy, .false. )
    call fstecr(zbfrac(1,i),   zbfrac(1,i),   -32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','BFR ','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
    call fstecr(zglfracb(1,i), zglfracb(1,i), -32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','BGLF','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
    call fstecr(ztskin(1,i),   ztskin(1,i),   -32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','TSG ','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
    call fstecr(zsnoln(1,i),   zsnoln(1,i),   -32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','NSNL','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
    call fstecr(zblheight(1,i),zblheight(1,i),-32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','BLME','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
  enddo

  do i = 1 , NBINS*ILAYS
    p = 1.0 * i
    kind = KIND_ARBITRARY
    call CONVIP_plus( ip, p, kind, +2, dummy, .false. )
    call fstecr(ztbot,    ztbot,    -32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','ITGL','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
  enddo

  do i = 1 , NBINS*SLAYS
    p = 1.0 * i
    kind = KIND_ARBITRARY
    call CONVIP_plus( ip, p, kind, +2, dummy, .false. )
    call fstecr(zsnodp,   zsnodp,   -32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','SDG ','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
    call fstecr(zsnoden,  zsnoden,  -32,fstout,dateo,0,0,ni,nj,1,ip,ip2,ip3,'A','SDN ','DIAGNOSTIQUE','Z',ig1,ig2,ig3,ig4,133,.false.)
  enddo

  call fstprm(keygla,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
              typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
              swa,lng,dltf,ubc,extra1,extra2,extra3)
  call fstecr(latp,  latp,  -nbits,fstout,dateo,0,0,ni,nj,nk,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,datyp,.false.)

  call fstprm(keyglo,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
              typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
              swa,lng,dltf,ubc,extra1,extra2,extra3)
  call fstecr(lonp,  lonp,  -nbits,fstout,dateo,0,0,ni,nj,nk,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,datyp,.false.)

  statuso = fstfrm(fstout)

stop
end program
subroutine dynaglace_ini (zglmf, zdxdy, zbfrac, zmg, zglacier, zglfracb, zglvol, zgldepth, zglnum, &
                          ztskin, ztbot, zsnodp, zsnoden, zbheight, zblheight, zsnoln, &
                          npts, nbins, ilays, slays)
!GEM     (topo,npts,nj,ilays,slays,nbins)


  implicit none

  integer  npts,ilays,slays,nbins

  integer  i, b
  real     glf, glarea
  real     bin_glfrac, bin_height

  real, intent(OUT) :: zglmf      (npts               )  ! Output  GLMF
  real, intent(IN)  :: zdxdy      (npts               )  ! Input   DXDY
  real, intent(IN)  :: zmg        (npts               )  ! Input   MG   (Geophy)   land-sea mask
  real, intent(IN)  :: zglacier   (npts               )  ! Input   GA   (Geophy)   continental ice cover (E)
  real, intent(OUT) :: zbfrac     (npts       ,nbins  )  ! InOut   BFR
  real, intent(OUT) :: zglfracb   (npts       ,nbins  )  ! Output  BGLF
  real, intent(OUT) :: zglvol     (npts               )  ! Output  GVOL
  real, intent(OUT) :: zgldepth   (npts               )  ! Output  GLD
  real, intent(IN)  :: zglnum     (npts               )  ! Input   GLNM (Geophy)   numbers of glaciers per gridbox
  real, intent(OUT) :: ztskin     (npts       ,nbins  )  ! Output  TSG
  real, intent(OUT) :: ztbot      (npts,ilays ,nbins  )  ! Output  ITGL
  real, intent(OUT) :: zsnodp     (npts,slays ,nbins  )  ! Output  SDG
  real, intent(OUT) :: zsnoden    (npts,slays ,nbins  )  ! Output  SDN
  real, intent(OUT) :: zsnoln     (npts       ,nbins  )  ! Output  NSNL
  real, intent(IN)  :: zbheight   (npts       ,nbins  )  ! Input   BME  (Geophy)   height of glacier bins [m]
  real, intent(OUT) :: zblheight  (npts       ,nbins  )  ! Output  BLME

  real, PARAMETER :: critmask  = 0.001
  real, PARAMETER :: gl_c = 0.2055
  real, PARAMETER :: gl_gamma = 1.357


!     =================================================================
!     Initialization
!     ==============


!     Initialization / Calculation of initial fields
!     ============================================

!       Initialization of bin dependent fields
!       =======================================

!       Adjust bin fractions so that sum is 1.
  do i=1,npts
    zbfrac(i,1) = 1.
    do b=2,nbins
      zbfrac(i,1) = zbfrac(i,1) - zbfrac(i,b)
    end do
  end do

!       Glacier fraction of each bin (with respect to grid box)
!       ----------------
  do i=1,npts
!         Fill bins from the top with glacier
    glf = zmg(i) * zglacier(i)
    do b=1,nbins
!           Fully or partially glaciated bin
      if ( glf .ge. critmask ) then
        zglfracb(i,b) = min(glf,zbfrac(i,b))
        glf = glf - zglfracb(i,b)
!           No glacier in bin
      else
        zglfracb(i,b) = 0.
        glf = 0.
      end if
    end do
  end do

!      print *,'Still here CCCCC'
!       Glacier depth - mean glacier depth for all bins [m of ice]
!       -------------   For now the power law is applied to the whole grid box
!                       It could also get applied for each bin seperately
!         Calculate glacier volume [1e6 m^3] according to power law
!SK       ======================================================================
!SK       NEW METHOD: APPLY POWER LAW (V-A-RELATION, SEE DOCUMENTATION DYNAMICE)
!SK       TO TOTAL GRID BOX GLACIER AREA (AT THIS TIME OBTAINED BY MULTIPLYING
!SK       GLACIER FRACTION glacier BY TOTAL GRID CELL AREA zdxdy [m]), THEN DERIVE
!SK       GLACIER DEPTH FROM VOLUME AND AREA. ALSO INITIALIZE GLACIER VOLUME.
!            with c=0.2055, y=1.357, V in [m**3], A in [m**2]
!            V = c * A**y
!         => A = (V/c)**(1/y)
!            H = V / A
!         => H = c * A**(y-1)
!         => A = (H/c)**(1/(y-1))


!      zdxdy=770000000  ! REMOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TODO


!       Rewritten for faster computation !!! =>
!        zgldepth = 0.2055e3 * (glacier*zdxdy*1e-6)**0.357
  do i=1,npts

!         Adjust mean grid point mountain height to bin heights
    zglmf(i) = 0.
    do b=1,nbins
      zglmf(i) = zglmf(i) + zbheight(i,b) * zbfrac(i,b)
    end do

    if ( zmg(i)*zglacier(i) .ge. critmask ) then
      glarea      = zmg(i) * zglacier(i)*zdxdy(i)
      zglvol(i)   = zglnum(i) * gl_c * (glarea/zglnum(i))**gl_gamma
! same as:  zglvol(i)   = gl_c * (glarea)**gl_gamma       * zglnum(i)**(1-gl_gamma)
      zgldepth(i) = zglvol(i) / glarea


!           Mean bin "land" height underneath glacier
      do b=1,nbins
        if ( zglfracb(i,b) .ge. critmask ) then
!               Glacier fraction with respect to bin
          bin_glfrac = zglfracb(i,b) / zbfrac(i,b)

          bin_height = zbheight(i,b)
          zblheight(i,b) =                                     &
                bin_height               * ( 1 - bin_glfrac )  &
            + ( bin_height-zgldepth(i) ) *       bin_glfrac

        else
          zblheight(i,b) = zbheight(i,b)
        end if
      end do

    else
      zglvol(i)   = 0.
      zgldepth(i) = 0.
    end if

!      print *,'zglvol:',zglvol

  end do

!     Initialize glacier fields
!     snodp     : SNOW DEPTH ON GLACIATED LAND FRACTION AT TIME T [m WEQ]
!     tskin     : surface (skin) temperature           over glacier [K]
!     tbotsnow  : temperature at bottom of snow layer  over glacier [K]
!     tbotice   : temperature at bottom of ice  layers over glacier [K]
!     tbot      : temperature at bottom of snow/ice layers over glacier [K]

!       Set interface temperatures for glaciers
  ztskin     = 271.15
  ztbot      = 272.15
  zsnodp     = 0.0
  zsnoden    = 0.0

!       Number of snow layers on glacier
  zsnoln     = 0.0



end subroutine
