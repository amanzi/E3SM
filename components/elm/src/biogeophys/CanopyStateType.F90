module CanopyStateType

  ! !USES:
  use shr_kind_mod    , only : r8 => shr_kind_r8
  use shr_infnan_mod  , only : shr_infnan_isnan,nan => shr_infnan_nan, assignment(=)
  use shr_log_mod     , only : errMsg => shr_log_errMsg
  use abortutils      , only : endrun
  use decompMod       , only : bounds_type
  use landunit_varcon , only : istsoil, istcrop
  use elm_varcon      , only : spval,ispval
  use elm_varpar      , only : nlevcan, nvegwcs
  use elm_varctl      , only : iulog, use_cn, use_fates, use_hydrstress, use_fates_sp
  use elm_varctl      , only : use_polygonal_tundra
  use LandunitType    , only : lun_pp
  use ColumnType      , only : col_pp
  use VegetationType       , only : veg_pp
  !
  implicit none
  save


  !renamed perchroot so as not to confuse with private data variable in
  !SoilMoistStressMod
  ! !PUBLIC DATA MEMBERS:
  ! true => btran is based only on unfrozen soil levels
  logical,  public :: perchroot    = .false.

  ! true  => btran is based on active layer (defined over two years);
  ! false => btran is based on currently unfrozen levels
  logical,  public :: perchroot_alt = .false.
  !------------------------------------------------------------------------------
  !$acc declare create(perchroot )
  !$acc declare create(perchroot_alt)
  !
  ! !PUBLIC TYPES:
  type, public :: CanopyState_type

     integer  , pointer :: frac_veg_nosno_patch     (:)   ! patch fraction of vegetation not covered by snow (0 OR 1) [-]
     integer  , pointer :: frac_veg_nosno_alb_patch (:)   ! patch fraction of vegetation not covered by snow (0 OR 1) [-]

     real(r8) , pointer :: tlai_patch               (:)   ! patch canopy one-sided leaf area index, no burying by snow
     real(r8) , pointer :: tsai_patch               (:)   ! patch canopy one-sided stem area index, no burying by snow
     real(r8) , pointer :: tlai_hist_patch          (:)   ! patch canopy one-sided leaf area index, for SP mode
     real(r8) , pointer :: tsai_hist_patch          (:)   ! patch canopy one-sided stem area index, for SP mode
     real(r8) , pointer :: htop_hist_patch          (:)   ! patch canopy height, for SP mode
     real(r8) , pointer :: elai_patch               (:)   ! patch canopy one-sided leaf area index with burying by snow
     real(r8) , pointer :: esai_patch               (:)   ! patch canopy one-sided stem area index with burying by snow
     real(r8) , pointer :: elai240_patch            (:)   ! patch canopy one-sided leaf area index with burying by snow average over 10days
     real(r8) , pointer :: laisun_patch             (:)   ! patch patch sunlit projected leaf area index
     real(r8) , pointer :: laisha_patch             (:)   ! patch patch shaded projected leaf area index
     real(r8) , pointer :: laisun_z_patch           (:,:) ! patch patch sunlit leaf area for canopy layer
     real(r8) , pointer :: laisha_z_patch           (:,:) ! patch patch shaded leaf area for canopy layer
     real(r8) , pointer :: mlaidiff_patch           (:)   ! patch difference between lai month one and month two (for dry deposition of chemical tracers)
     real(r8) , pointer :: annlai_patch             (:,:) ! patch 12 months of monthly lai from input data set (for dry deposition of chemical tracers)
     real(r8) , pointer :: htop_patch               (:)   ! patch canopy top (m)
     real(r8) , pointer :: hbot_patch               (:)   ! patch canopy bottom (m)
     real(r8) , pointer :: displa_patch             (:)   ! patch displacement height (m)
     real(r8) , pointer :: fsun_patch               (:)   ! patch sunlit fraction of canopy
     real(r8) , pointer :: fsun24_patch             (:)   ! patch 24hr average of sunlit fraction of canopy
     real(r8) , pointer :: fsun240_patch            (:)   ! patch 240hr average of sunlit fraction of canopy

     real(r8) , pointer :: alt_col                  (:)   ! col current depth of thaw
     integer  , pointer :: alt_indx_col             (:)   ! col current depth of thaw
     real(r8) , pointer :: altmax_col               (:)   ! col maximum annual depth of thaw
     real(r8) , pointer :: altmax_lastyear_col      (:)   ! col prior year maximum annual depth of thaw
     integer  , pointer :: altmax_indx_col          (:)   ! col maximum annual depth of thaw
     integer  , pointer :: altmax_lastyear_indx_col (:)   ! col prior year maximum annual depth of thaw
     real(r8) , pointer :: altmax_1989_col          (:)   ! col maximum annual thaw depth in 1989 RF note: temporary for IM1!
     real(r8) , pointer :: altmax_ever_col          (:)   ! col maximum thaw depth from beginning of simulation
     integer  , pointer :: altmax_1989_indx_col     (:)   ! col 1989 maximum thaw depth RF note: temporary for IM1!
     integer  , pointer :: altmax_ever_indx_col     (:)   ! col maximum thaw depth from beginning of simulation

     real(r8) , pointer :: dewmx_patch              (:)   ! patch maximum allowed dew [mm]

     real(r8) , pointer :: dleaf_patch              (:)   ! patch characteristic leaf width (diameter) [m]
                                                          ! for non-ED/FATES this is the same as pftcon%dleaf()
     real(r8),  pointer :: lbl_rsc_h2o_patch        (:)   ! laminar boundary layer resistance for water over dry leaf (s/m)
     real(r8) , pointer :: vegwp_patch              (:,:) ! patch vegetation water matric potential (mm)
   contains

     procedure, public  :: Init
     procedure, public  :: InitAllocate
     procedure, private :: InitHistory
     procedure, private :: InitCold
     procedure, public  :: InitAccBuffer
     procedure, public  :: InitAccVars
     procedure, public  :: UpdateAccVars
     procedure, public  :: Restart


  end type CanopyState_type
  !------------------------------------------------------------------------

contains
  !------------------------------------------------------------------------
  subroutine Init(this, bounds)

    class(canopystate_type) :: this
    type(bounds_type), intent(in) :: bounds

    call this%InitAllocate(bounds)
    call this%InitHistory(bounds)
    call this%InitCold(bounds)

  end subroutine Init

  !------------------------------------------------------------------------
  subroutine InitAllocate(this, bounds )

    ! !ARGUMENTS:
    class(canopystate_type) :: this
    type(bounds_type), intent(in) :: bounds    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp
    integer :: begc, endc
    integer :: begg, endg

    !------------------------------------------------------------------------

    begp = bounds%begp; endp= bounds%endp
    begc = bounds%begc; endc= bounds%endc
    begg = bounds%begg; endg= bounds%endg

    allocate(this%frac_veg_nosno_patch     (begp:endp))           ; this%frac_veg_nosno_patch     (:)   = ispval
    allocate(this%frac_veg_nosno_alb_patch (begp:endp))           ; this%frac_veg_nosno_alb_patch (:)   = 0
    allocate(this%tlai_patch               (begp:endp))           ; this%tlai_patch               (:)   = spval
    allocate(this%tsai_patch               (begp:endp))           ; this%tsai_patch               (:)   = spval
    allocate(this%tlai_hist_patch          (begp:endp))           ; this%tlai_hist_patch          (:)   = spval
    allocate(this%tsai_hist_patch          (begp:endp))           ; this%tsai_hist_patch          (:)   = spval
    allocate(this%htop_hist_patch          (begp:endp))           ; this%htop_hist_patch          (:)   = spval
    allocate(this%elai_patch               (begp:endp))           ; this%elai_patch               (:)   = spval
    allocate(this%elai240_patch            (begp:endp))           ; this%elai240_patch            (:)   = spval
    allocate(this%esai_patch               (begp:endp))           ; this%esai_patch               (:)   = spval
    allocate(this%laisun_patch             (begp:endp))           ; this%laisun_patch             (:)   = spval
    allocate(this%laisha_patch             (begp:endp))           ; this%laisha_patch             (:)   = spval
    allocate(this%laisun_z_patch           (begp:endp,1:nlevcan)) ; this%laisun_z_patch           (:,:) = spval
    allocate(this%laisha_z_patch           (begp:endp,1:nlevcan)) ; this%laisha_z_patch           (:,:) = spval
    allocate(this%mlaidiff_patch           (begp:endp))           ; this%mlaidiff_patch           (:)   = spval
    allocate(this%annlai_patch          (12,begp:endp))           ; this%annlai_patch             (:,:) = spval
    allocate(this%htop_patch               (begp:endp))           ; this%htop_patch               (:)   = spval
    allocate(this%hbot_patch               (begp:endp))           ; this%hbot_patch               (:)   = spval
    allocate(this%displa_patch             (begp:endp))           ; this%displa_patch             (:)   = spval
    allocate(this%fsun_patch               (begp:endp))           ; this%fsun_patch               (:)   = spval
    allocate(this%fsun24_patch             (begp:endp))           ; this%fsun24_patch             (:)   = spval
    allocate(this%fsun240_patch            (begp:endp))           ; this%fsun240_patch            (:)   = spval

    allocate(this%alt_col                  (begc:endc))           ; this%alt_col                  (:)   = spval;
    allocate(this%altmax_col               (begc:endc))           ; this%altmax_col               (:)   = spval
    allocate(this%altmax_lastyear_col      (begc:endc))           ; this%altmax_lastyear_col      (:)   = spval
    allocate(this%altmax_1989_col          (begc:endc))           ; this%altmax_1989_col          (:)   = spval
    allocate(this%altmax_ever_col          (begc:endc))           ; this%altmax_ever_col          (:)   = spval
    allocate(this%alt_indx_col             (begc:endc))           ; this%alt_indx_col             (:)   = huge(1)
    allocate(this%altmax_indx_col          (begc:endc))           ; this%altmax_indx_col          (:)   = huge(1)
    allocate(this%altmax_lastyear_indx_col (begc:endc))           ; this%altmax_lastyear_indx_col (:)   = huge(1)
    allocate(this%altmax_1989_indx_col     (begc:endc))           ; this%altmax_1989_indx_col     (:)   = huge(1)
    allocate(this%altmax_ever_indx_col     (begc:endc))           ; this%altmax_ever_indx_col     (:)   = huge(1)

    allocate(this%dewmx_patch              (begp:endp))           ; this%dewmx_patch              (:)   = spval
    allocate(this%dleaf_patch              (begp:endp))           ; this%dleaf_patch              (:)   = spval
    allocate(this%lbl_rsc_h2o_patch        (begp:endp))           ; this%lbl_rsc_h2o_patch        (:)   = spval
    allocate(this%vegwp_patch              (begp:endp,1:nvegwcs)) ; this%vegwp_patch              (:,:) = spval


  end subroutine InitAllocate

  !-----------------------------------------------------------------------
  subroutine InitHistory(this, bounds)
    !
    ! !USES:
    use shr_infnan_mod, only: nan => shr_infnan_nan, assignment(=)
    use elm_varctl    , only: use_cn
    use elm_varpar    , only: nlevgrnd
    use histFileMod   , only: hist_addfld1d, hist_addfld2d
    !
    ! !ARGUMENTS:
    class(canopystate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begc, endc
    integer :: begp, endp
    real(r8), pointer :: data2dptr(:,:), data1dptr(:) ! temp. pointers for slicing larger arrays
    !---------------------------------------------------------------------

    begp = bounds%begp; endp= bounds%endp
    begc = bounds%begc; endc= bounds%endc

    this%elai_patch(begp:endp) = spval
    call hist_addfld1d (fname='ELAI', units='m^2/m^2', &
        avgflag='A', long_name='exposed one-sided leaf area index', &
         ptr_patch=this%elai_patch)

    this%esai_patch(begp:endp) = spval
    call hist_addfld1d (fname='ESAI', units='m^2/m^2', &
         avgflag='A', long_name='exposed one-sided stem area index', &
         ptr_patch=this%esai_patch)

     if (use_fates_sp) then
        this%tlai_hist_patch(begp:endp) = spval
        call hist_addfld1d (fname='TLAI', units='m', &
             avgflag='A', long_name='TLAI weights for SP mode', &
             ptr_patch=this%tlai_hist_patch)

        this%tsai_hist_patch(begp:endp) = spval
        call hist_addfld1d (fname='TSAI', units='m', &
             avgflag='A', long_name='TSAI weights for SP mode', &
             ptr_patch=this%tsai_hist_patch)
     else
        this%tlai_patch(begp:endp) = spval
        call hist_addfld1d (fname='TLAI', units='1', &
             avgflag='A', long_name='total projected leaf area index', &
             ptr_patch=this%tlai_patch)

        this%tsai_patch(begp:endp) = spval
        call hist_addfld1d (fname='TSAI', units='1', &
             avgflag='A', long_name='total projected stem area index', &
             ptr_patch=this%tsai_patch)
     end if

    if (use_cn .or. use_fates) then
       this%fsun_patch(begp:endp) = spval
       call hist_addfld1d (fname='FSUN', units='proportion', &
            avgflag='A', long_name='sunlit fraction of canopy', &
            ptr_patch=this%fsun_patch, default='inactive')
    end if

    this%laisun_patch(begp:endp) = spval
    call hist_addfld1d (fname='LAISUN', units='1', &
         avgflag='A', long_name='sunlit projected leaf area index', &
         ptr_patch=this%laisun_patch, set_urb=0._r8)

    this%laisha_patch(begp:endp) = spval
    call hist_addfld1d (fname='LAISHA', units='1', &
         avgflag='A', long_name='shaded projected leaf area index', &
         ptr_patch=this%laisha_patch, set_urb=0._r8)

    if (use_cn .or. use_fates) then
       this%dewmx_patch(begp:endp) = spval
       call hist_addfld1d (fname='DEWMX', units='mm', &
            avgflag='A', long_name='Maximum allowed dew', &
            ptr_patch=this%dewmx_patch, default='inactive')
    end if

    if (use_cn .or. use_fates) then

       if (use_fates_sp) then
          this%htop_hist_patch(begp:endp) = spval
          call hist_addfld1d (fname='HTOP', units='m', &
               avgflag='A', long_name='HTOP weights for SP mode', &
               ptr_patch=this%htop_hist_patch)
       else
          this%htop_patch(begp:endp) = spval
          call hist_addfld1d (fname='HTOP', units='m', &
               avgflag='A', long_name='canopy top', &
               ptr_patch=this%htop_patch)
       end if

    end if

    if (use_cn .or. use_fates) then
       this%hbot_patch(begp:endp) = spval
       call hist_addfld1d (fname='HBOT', units='m', &
            avgflag='A', long_name='canopy bottom', &
            ptr_patch=this%hbot_patch, default='inactive')
    end if

    if (use_cn .or. use_fates) then
       this%displa_patch(begp:endp) = spval
       call hist_addfld1d (fname='DISPLA', units='m', &
            avgflag='A', long_name='displacement height', &
            ptr_patch=this%displa_patch, default='inactive')
    end if

    if (use_cn) then
       this%alt_col(begc:endc) = spval
       call hist_addfld1d (fname='ALT', units='m', &
            avgflag='A', long_name='current active layer thickness', &
            ptr_col=this%alt_col)

       this%altmax_col(begc:endc) = spval
       call hist_addfld1d (fname='ALTMAX', units='m', &
            avgflag='A', long_name='maximum annual active layer thickness', &
            ptr_col=this%altmax_col)

       this%altmax_lastyear_col(begc:endc) = spval
       call hist_addfld1d (fname='ALTMAX_LASTYEAR', units='m', &
            avgflag='A', long_name='maximum prior year active layer thickness', &
            ptr_col=this%altmax_lastyear_col)

       if (use_polygonal_tundra) then
          this%altmax_1989_col(begc:endc) = spval
          call hist_addfld1d (fname='ALTMAX_1989', units='m', &
               avgflag='A', long_name='maximum 1989 active layer thickness', &
               ptr_col=this%altmax_1989_col)

          this%altmax_ever_col(begc:endc) = spval
          call hist_addfld1d (fname='ALTMAX_EVER', units='m', &
               avgflag='A', long_name='maximum ever active layer thickness throughout simulation', &
               ptr_col=this%altmax_ever_col)
       end if
    end if

    ! Allow active layer fields to be optionally output even if not running CN

    if (.not. use_cn) then
       this%alt_col(begc:endc) = spval
       call hist_addfld1d (fname='ALT', units='m', &
            avgflag='A', long_name='current active layer thickness', &
            ptr_col=this%alt_col, default='inactive')

       this%altmax_col(begc:endc) = spval
       call hist_addfld1d (fname='ALTMAX', units='m', &
            avgflag='A', long_name='maximum annual active layer thickness', &
            ptr_col=this%altmax_col, default='inactive')

       this%altmax_lastyear_col(begc:endc) = spval
       call hist_addfld1d (fname='ALTMAX_LASTYEAR', units='m', &
            avgflag='A', long_name='maximum prior year active layer thickness', &
            ptr_col=this%altmax_lastyear_col, default='inactive')

       if (use_polygonal_tundra) then
          this%altmax_1989_col(begc:endc) = spval
          call hist_addfld1d (fname='ALTMAX_1989', units='m', &
               avgflag='A', long_name='maximum 1989 active layer thickness', &
               ptr_col=this%altmax_1989_col, default='inactive')

          this%altmax_ever_col(begc:endc) = spval
          call hist_addfld1d (fname='ALTMAX_EVER', units='m', &
               avgflag='A', long_name='maximum ever active layer thickness throughout simulation', &
               ptr_col=this%altmax_ever_col, default='inactive')
       end if
    end if

    ! Accumulated fields
    this%fsun24_patch(begp:endp) = spval
    call hist_addfld1d (fname='FSUN24', units='K',  &
         avgflag='A', long_name='fraction sunlit (last 24hrs)', &
         ptr_patch=this%fsun24_patch, default='inactive')

    this%fsun240_patch(begp:endp) = spval
    call hist_addfld1d (fname='FSUN240', units='K',  &
         avgflag='A', long_name='fraction sunlit (last 240hrs)', &
         ptr_patch=this%fsun240_patch, default='inactive')

    this%elai240_patch(begp:endp) = spval
    call hist_addfld1d (fname='LAI240', units='m2/m2',  &
         avgflag='A', long_name='240hr average of leaf area index', &
         ptr_patch=this%elai240_patch, default='inactive')

    if ( use_hydrstress ) then
       this%vegwp_patch(begp:endp,:) = spval
       call hist_addfld2d (fname='VEGWP',  units='mm', type2d='nvegwcs', &
            avgflag='A', long_name='vegetation water matric potential for sun/sha canopy,xyl,root segments', &
            ptr_patch=this%vegwp_patch)
    end if


  end subroutine InitHistory

  !-----------------------------------------------------------------------
  subroutine InitAccBuffer (this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize accumulation buffer for all required module accumulated fields
    ! This routine set defaults values that are then overwritten by the
    ! restart file for restart or branch runs
    !
    ! !USES
    use accumulMod  , only : init_accum_field
    !
    ! !ARGUMENTS:
    class(canopystate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !---------------------------------------------------------------------

    this%fsun24_patch(bounds%begp:bounds%endp) = spval
    call init_accum_field (name='FSUN24', units='fraction',                                        &
         desc='24hr average of diffuse solar radiation',  accum_type='runmean', accum_period=-1,   &
         subgrid_type='pft', numlev=1, init_value=0._r8)

    this%fsun240_patch(bounds%begp:bounds%endp) = spval
    call init_accum_field (name='FSUN240', units='fraction',                                       &
         desc='240hr average of diffuse solar radiation',  accum_type='runmean', accum_period=-10, &
         subgrid_type='pft', numlev=1, init_value=0._r8)

    this%elai240_patch(bounds%begp:bounds%endp) = spval
    call init_accum_field (name='LAI240', units='m2/m2',                                             &
         desc='240hr average of leaf area index',  accum_type='runmean', accum_period=-10,      &
         subgrid_type='pft', numlev=1, init_value=0._r8)

  end subroutine InitAccBuffer

  !-----------------------------------------------------------------------
  subroutine InitAccVars(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module variables that are associated with
    ! time accumulated fields. This routine is called for both an initial run
    ! and a restart run (and must therefore must be called after the restart file
    ! is read in and the accumulation buffer is obtained)
    !
    ! !USES
    use accumulMod       , only : extract_accum_field
    use elm_time_manager , only : get_nstep
    !
    ! !ARGUMENTS:
    class(canopystate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer  :: begp, endp
    integer  :: nstep
    integer  :: ier
    real(r8), pointer :: rbufslp(:)  ! temporary
    !---------------------------------------------------------------------

    begp = bounds%begp; endp = bounds%endp

    ! Allocate needed dynamic memory for single level pft field
    allocate(rbufslp(begp:endp), stat=ier)
    if (ier/=0) then
       write(iulog,*)' in '
       call endrun(msg="extract_accum_hist allocation error for rbufslp"//&
            errMsg(__FILE__, __LINE__))
    endif

    ! Determine time step
    nstep = get_nstep()

    call extract_accum_field ('FSUN24', rbufslp, nstep)
    this%fsun24_patch(begp:endp) = rbufslp(begp:endp)

    call extract_accum_field ('FSUN240', rbufslp, nstep)
    this%fsun240_patch(begp:endp) = rbufslp(begp:endp)

    call extract_accum_field ('LAI240', rbufslp, nstep)
    this%elai240_patch(begp:endp) = rbufslp(begp:endp)

    call extract_accum_field ('FSUN24', rbufslp, nstep)
    this%fsun24_patch(begp:endp) = rbufslp(begp:endp)

    deallocate(rbufslp)

  end subroutine InitAccVars

  !-----------------------------------------------------------------------
  subroutine UpdateAccVars (this, bounds)
    !
    ! USES
    use elm_time_manager, only : get_nstep
    use accumulMod      , only : update_accum_field, extract_accum_field
    use abortutils      , only : endrun
    !
    ! !ARGUMENTS:
    class(canopystate_type)             :: this
    type(bounds_type)      , intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: g,p                       ! indices
    integer :: dtime                     ! timestep size [seconds]
    integer :: nstep                     ! timestep number
    integer :: ier                       ! error status
    integer :: begp, endp
    real(r8), pointer :: rbufslp(:)      ! temporary single level - pft level
    !---------------------------------------------------------------------
    begp = bounds%begp; endp = bounds%endp
    nstep = get_nstep()
    ! Allocate needed dynamic memory for single level pft field
    allocate(rbufslp(begp:endp), stat=ier)
    if (ier/=0) then
       write(iulog,*)'update_accum_hist allocation error for rbuf1dp'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif
    ! Accumulate and extract fsun24 & fsun240
    do p = begp,endp
       rbufslp(p) = this%fsun_patch(p)
    end do
    call update_accum_field  ('FSUN24' , rbufslp              , nstep)
    call extract_accum_field ('FSUN24' , this%fsun24_patch    , nstep)
    call update_accum_field  ('FSUN240', rbufslp              , nstep)
    call extract_accum_field ('FSUN240', this%fsun240_patch   , nstep)

    ! Accumulate and extract elai240
    do p = begp,endp
       rbufslp(p) = this%elai_patch(p)
    end do
    call update_accum_field  ('LAI240', rbufslp                 , nstep)
    call extract_accum_field ('LAI240', this%elai240_patch       , nstep)

    deallocate(rbufslp)
  end subroutine UpdateAccVars

  !-----------------------------------------------------------------------
  subroutine InitCold(this, bounds)
    !
    ! !ARGUMENTS:
    class(canopystate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer  :: p,l,c,g
    !-----------------------------------------------------------------------

    do p = bounds%begp, bounds%endp
       l = veg_pp%landunit(p)

       this%frac_veg_nosno_patch(p) = 0._r8
       this%tlai_patch(p)       = 0._r8
       this%tsai_patch(p)       = 0._r8
       this%elai_patch(p)       = 0._r8
       this%esai_patch(p)       = 0._r8
       this%htop_patch(p)       = 0._r8
       this%hbot_patch(p)       = 0._r8
       this%dewmx_patch(p)      = 0.1_r8
       this%vegwp_patch(p,:)    = -2.5e4_r8

       if (lun_pp%itype(l) == istsoil .or. lun_pp%itype(l) == istcrop) then
          this%laisun_patch(p) = 0._r8
          this%laisha_patch(p) = 0._r8
       end if

       this%tlai_hist_patch(p)       = 0._r8
       this%tsai_hist_patch(p)       = 0._r8
       this%htop_hist_patch(p)       = 0._r8

       ! needs to be initialized to spval to avoid problems when averaging for the accum
       ! field
       this%fsun_patch(p) = spval
    end do

    do c = bounds%begc, bounds%endc
       l = col_pp%landunit(c)

       if (lun_pp%itype(l) == istsoil .or. lun_pp%itype(l) == istcrop) then
          this%alt_col(c)                  = 0._r8
          this%altmax_col(c)               = 0._r8
          this%altmax_lastyear_col(c)      = 0._r8
          this%altmax_1989_col(c)          = 0._r8
          this%altmax_ever_col(c)          = 0._r8
          this%alt_indx_col(c)             = 0
          this%altmax_indx_col(c)          = 0
          this%altmax_lastyear_indx_col(c) = 0
          this%altmax_1989_indx_col(c)     = 0
          this%altmax_ever_indx_col(c)     = 0
       end if
    end do

  end subroutine InitCold

  !------------------------------------------------------------------------
  subroutine Restart(this, bounds, ncid, flag)
    !
    ! !USES:
    use spmdMod    , only : masterproc
    use ncdio_pio  , only : file_desc_t, ncd_defvar, ncd_io, ncd_double, ncd_int, ncd_inqvdlen
    use restUtilMod
    !
    ! !ARGUMENTS:
    class(canopystate_type) :: this
    type(bounds_type) , intent(in)    :: bounds
    type(file_desc_t) , intent(inout) :: ncid   ! netcdf id
    character(len=*)  , intent(in)    :: flag   ! 'read' or 'write'
    !
    ! !LOCAL VARIABLES:
    integer :: j,p,c,iv ! indices
    logical :: readvar      ! determine if variable is on initial file
    integer :: begp, endp
    !-----------------------------------------------------------------------

    begp = bounds%begp; endp = bounds%endp
    call restartvar(ncid=ncid, flag=flag, varname='FRAC_VEG_NOSNO_ALB', xtype=ncd_int,  &
         dim1name='pft', long_name='fraction of vegetation not covered by snow (0 or 1)', units='', &
         interpinic_flag='interp', readvar=readvar, data=this%frac_veg_nosno_alb_patch)
    call restartvar(ncid=ncid, flag=flag, varname='tlai', xtype=ncd_double,  &
         dim1name='pft', long_name='one-sided leaf area index, no burying by snow', units='', &
         interpinic_flag='interp', readvar=readvar, data=this%tlai_patch)
    call restartvar(ncid=ncid, flag=flag, varname='tsai', xtype=ncd_double,  &
         dim1name='pft', long_name='one-sided stem area index, no burying by snow', units='', &
         interpinic_flag='interp', readvar=readvar, data=this%tsai_patch)
    call restartvar(ncid=ncid, flag=flag, varname='elai', xtype=ncd_double,  &
         dim1name='pft', long_name='one-sided leaf area index, with burying by snow', units='', &
         interpinic_flag='interp', readvar=readvar, data=this%elai_patch)
    call restartvar(ncid=ncid, flag=flag, varname='esai', xtype=ncd_double,  &
         dim1name='pft', long_name='one-sided stem area index, with burying by snow', units='', &
         interpinic_flag='interp', readvar=readvar, data=this%esai_patch)
    call restartvar(ncid=ncid, flag=flag, varname='htop', xtype=ncd_double,  &
         dim1name='pft', long_name='canopy top', units='m', &
         interpinic_flag='interp', readvar=readvar, data=this%htop_patch)
    call restartvar(ncid=ncid, flag=flag, varname='hbot', xtype=ncd_double,  &
         dim1name='pft', long_name='canopy botton', units='m', &
         interpinic_flag='interp', readvar=readvar, data=this%hbot_patch)
    call restartvar(ncid=ncid, flag=flag, varname='mlaidiff', xtype=ncd_double,  &
         dim1name='pft', long_name='difference between lai month one and month two', units='', &
         interpinic_flag='interp', readvar=readvar, data=this%mlaidiff_patch)
    call restartvar(ncid=ncid, flag=flag, varname='fsun', xtype=ncd_double,  &
         dim1name='pft', long_name='sunlit fraction of canopy', units='', &
         interpinic_flag='interp', readvar=readvar, data=this%fsun_patch)
    if (flag=='read' )then
       do p = bounds%begp,bounds%endp
          if (shr_infnan_isnan(this%fsun_patch(p)) ) then
             this%fsun_patch(p) = spval
          end if
       end do
    end if
    if (use_cn .or. use_fates) then
       call restartvar(ncid=ncid, flag=flag, varname='altmax', xtype=ncd_double,  &
            dim1name='column', long_name='', units='', &
            interpinic_flag='interp', readvar=readvar, data=this%altmax_col)
       call restartvar(ncid=ncid, flag=flag, varname='altmax_lastyear', xtype=ncd_double,  &
            dim1name='column', long_name='', units='', &
            interpinic_flag='interp', readvar=readvar, data=this%altmax_lastyear_col)
       call restartvar(ncid=ncid, flag=flag, varname='altmax_indx', xtype=ncd_int,  &
            dim1name='column', long_name='', units='', &
            interpinic_flag='interp', readvar=readvar, data=this%altmax_indx_col)
       call restartvar(ncid=ncid, flag=flag, varname='altmax_lastyear_indx', xtype=ncd_int,  &
            dim1name='column', long_name='', units='', &
            interpinic_flag='interp', readvar=readvar, data=this%altmax_lastyear_indx_col)
       if (use_polygonal_tundra) then
          call restartvar(ncid=ncid, flag=flag, varname='altmax_1989_indx', xtype=ncd_double,  &
               dim1name='column', long_name='', units='', &
               interpinic_flag='interp', readvar=readvar, data=this%altmax_1989_indx_col)
          call restartvar(ncid=ncid, flag=flag, varname='altmax_ever_indx', xtype=ncd_int,  &
               dim1name='column', long_name='', units='', &
               interpinic_flag='interp', readvar=readvar, data=this%altmax_ever_indx_col)
          call restartvar(ncid=ncid, flag=flag, varname='altmax_1989', xtype=ncd_double,  &
               dim1name='column', long_name='', units='', &
               interpinic_flag='interp', readvar=readvar, data=this%altmax_1989_col)
          call restartvar(ncid=ncid, flag=flag, varname='altmax_ever', xtype=ncd_double,  &
               dim1name='column', long_name='', units='', &
               interpinic_flag='interp', readvar=readvar, data=this%altmax_ever_col)
       end if ! polygonal tundra
    end if ! cn or fates
    if ( use_hydrstress ) then
       call restartvar(ncid=ncid, flag=flag, varname='vegwp', xtype=ncd_double, &
            dim1name='pft', dim2name='vegwcs', switchdim=.true., &
            long_name='vegetation water matric potential', units='mm', &
            interpinic_flag='interp', readvar=readvar, data=this%vegwp_patch)
    end if
  end subroutine Restart

end module CanopyStateType
