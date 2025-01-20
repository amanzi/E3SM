module EMI_DataMod
  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This module provides a base class for an array of data to be
  ! passed to an external model.
  !

#define MAX_DIMS 4

  use shr_kind_mod         , only : r8 => shr_kind_r8
  use shr_log_mod          , only : errMsg => shr_log_errMsg
  use abortutils           , only : endrun
  use elm_varctl           , only : iulog
  !
  implicit none
  !

  integer EMI_VALUE_TYPE_UNDEFINED = 0
  integer EMI_VALUE_TYPE_INT       = 1
  integer EMI_VALUE_TYPE_REAL      = 2
  
  type :: emi_dim_data
     !
     ! !DESCRIPTION:
     ! A struct of dimensions, sizes, and self-documenting descriptions
     !
     integer :: beg, end                ! begin and end indices into the array
     integer :: size                    ! size of this dimension, -1 --> unused
     integer :: cumulative_size         ! size up through (and including) this dimension

     character (len=10) :: dim_name     ! name the dimension, e.g. 'column', 'levscpf', etc
     character (len=10) :: beg_name     ! name the beginning range, e.g. '-nlevsno', or '1'
     character (len=10) :: end_name     ! name the end range, e.g. 'nlevsoi', or 'npfts'
     
   contains
     procedure, public :: Init => EMIDimData_Init
  end type emi_dim_data


  type :: emi_dims_data
     !
     ! !DESCRIPTION:
     ! A MAX_DIMS array of emi_dim_data
     !
     type(emi_dim_data) :: dims(MAX_DIMS)       ! the array of dims
     integer :: total_size                      ! total size of the array

   contains
     procedure, public :: Init => EMIDimsData_Init
     procedure, public :: SetDim => EMIDimsData_SetDim
     procedure, public :: ComputeSizes => EMIDimsData_ComputeSizes
  end type emi_dims_data
  

  type :: emi_filter_data
     !
     ! !DESCRIPTION:
     ! A struct for filters
     !

     ! Note that only the left-most dimension may be filtered.
     !
     ! These things could probably be kept directly in emi_data rather
     ! than in their own data type, but there seemed a possibility of
     ! needing multiple filters per dimension.
     !
     logical :: is_compressed           ! does the external model only use the filtered objects?
     integer, pointer :: filter(:)      ! list of indices into ELM's array are used

   contains
     procedure, public :: Init => EMIFilterData_Init
     procedure, public :: Destroy => EMIFilterData_Destroy
  end type emi_filter_data

  
  type :: emi_data
     !
     ! !DESCRIPTION:
     ! A single piece of data plus metadata that may be passed to an
     ! external model.
     !
     integer             :: id                  ! enum of the data type
     character (len=32)  :: name                ! Short name
     character (len=128) :: long_name           ! Long name of data
     character (len=24)  :: units               ! Units

     integer             :: valuetype           ! type of data, EMI_VALUE_TYPE_{INT,REAL}
     logical             :: is_set              ! Is data set

     integer             :: ndim                ! Dimension of the data

     integer, pointer :: values_int(:)            ! flattened data, ints
     real(r8), pointer :: values_real(:)          ! flattened data, reals

   contains

     procedure, public :: Init              => EMIData_Init
     procedure, public :: Setup             => EMIData_Setup
     procedure, public :: SetDim            => EMIData_SetDim
     procedure, public :: AllocateMemory    => EMIData_AllocateMemory
     procedure, public :: Reset             => EMIData_Reset
     procedure, public :: Destroy           => EMIData_Destroy
     procedure, public :: PrintInfo         => EMIData_PrintInfo
     procedure, public :: PrintData         => EMIData_PrintData
     procedure, public :: CopyIn            => EMIData_CopyIn
     procedure, public :: CopyOut           => EMIData_CopyOut
  end type emi_data


contains

  !
  ! emi_dim_data methods
  ! ----------------------------------------
  subroutine EMIDimData_Init(this)
    ! !ARGUMENTS:
    class(emi_dim_data), intent(inout) :: this

    this%beg = -1
    this%beg = -1
    this%size = 0
    this%cumulative_size = -1

    this%dim_name = ''
    this%beg_name = ''
    this%end_name = ''
  end subroutine EMIDimData_Init

  
  !
  ! emi_dims_data methods
  ! ----------------------------------------
  subroutine EMIDimsData_Init(this)
    class(emi_dims_data), intent(inout) :: this
    integer :: d
    
    this%total_size = -1

    do d = 1, MAX_DIMS
       call this%dims(d)%Init()
    end do
  end subroutine EMIDimsData_Init

  
  subroutine EMIDimsData_SetDim(this, dim, beg, end)
    ! !ARGUMENTS:
    class(emi_dims_data), intent(inout) :: this
    integer, intent(in) :: dim
    integer, intent(in) :: beg, end

    this%dims(dim)%beg = beg
    this%dims(dim)%end = end
    this%dims(dim)%size = end - beg + 1
  end subroutine EMIDimsData_SetDim

  
  subroutine EMIDimsData_ComputeSizes(this)
    !
    ! !DESCRIPTION:
    ! Computes the cumulative and total sizes of each dimension.
    !
    class(emi_dims_data), intent(inout) :: this

    integer d
    integer size

    size = 0
    if (this%dims(1)%size > 0) then
       size = this%dims(1)%size
       this%dims(1)%cumulative_size = size
    end if

    do d = 2, MAX_DIMS
       if (this%dims(d)%size > 0) then
          size = size * this%dims(d)%size
          this%dims(d)%cumulative_size = size
       end if
    end do

    this%total_size = size
  end subroutine EMIDimsData_ComputeSizes

  
  !
  ! emi_filter_data methods
  ! ----------------------------------------
  subroutine EMIFilterData_Init(this)
    class(emi_filter_data), intent(inout) :: this

    this%is_compressed = .FALSE.
    nullify(this%filter)
  end subroutine EMIFilterData_Init

  subroutine EMIFilterData_Destroy(this)
    !
    ! !DESCRIPTION:
    !
    class(emi_filter_data), intent(inout) :: this

    this%is_compressed = .FALSE.
    if (associated(this%filter)) then
       nullify(this%filter)
    endif
  end subroutine EMIFilterData_Destroy


  !
  ! emi_data methods
  !------------------------------------------------------------------------
  subroutine EMIData_Init(this)
    !
    ! !DESCRIPTION:
    ! Initializes a EMI data
    !
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    this%id             = -1
    this%name           = ""
    this%long_name      = ""
    this%units          = ""

    this%value_type     = EMI_VALUE_TYPE_UNDEFINED
    this%is_set         = .false.
    this%ndim           = 0

    this%num_em_stages  = 0
    nullify(this%em_stage_ids)

    nullify(this%values_int)
    nullify(this%values_real)
    nullify(this%next)
  end subroutine EMIData_Init


  !------------------------------------------------------------------------
  subroutine EMIData_Setup(this, id, name, long_name, units, value_type)
    !
    ! !DESCRIPTION:
    ! Set value to data members of EMIData object
    !
    !
    ! !ARGUMENTS:
    class(emi_data)  , intent(inout) :: this
    integer          , intent(in)    :: id
    character(len=*) , intent(in)    :: name
    character(len=*) , intent(in)    :: long_name
    character(len=*) , intent(in)    :: units
    integer          , intent(in)    :: value_type
    
    this%id = id       
    this%name = trim(name)
    this%long_name = trim(long_name)
    this%units = trim(units)

    if (value_type /= EMI_VALUE_TYPE_INT .and. data_type /= EMI_VALUE_TYPE_REAL) then
       call endrun(msg='Invalid EMI data_type requested, must be one of EMI_VALUE_TYPE_{INT,REAL}')
    end if
    this%value_type = value_type
  end subroutine EMIData_Setup


  !------------------------------------------------------------------------
  subroutine EMIData_SetDim(this, dim, name, beg, end, beg_name, end_name)
    !
    ! !DESCRIPTION:
    ! Sets beg/end dimension info
    !
    !
    ! !ARGUMENTS:
    class(emi_data) , intent(inout) :: this
    character(len=*), intent(in)    :: name
    integer         , intent(in)    :: dim
    integer         , intent(in)    :: beg
    integer         , intent(in)    :: end
    character(len=*), intent(in)    :: beg_name
    character(len=*), intent(in)    :: end_name

    if (dim > MAX_DIMS) then
       call endrun(msg='Maximum umber of EM data dimensions exceeded.')
    end if

    if (dim > this%ndims) this%ndims = dim
    this%dims(dim)%name = trim(name)
    this%dims(dim)%beg_name = trim(beg_name)
    this%dims(dim)%end_name = trim(end_name)
    call this%dims%SetDim(dim, beg, end)
  end subroutine EMIData_SetDim

  
  !------------------------------------------------------------------------
  subroutine EMIData_AllocateMemory(this)
    !
    ! !DESCRIPTION:
    ! Initializes a EMI data
    !
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    ! locals
    integer ier

    if (this%is_int_type .and. this%is_real_type) then
       call endrun(msg='Data type is defined to be both int and real.')
    endif

    if ((.not.this%is_int_type) .and. (.not.this%is_real_type)) then
       call endrun(msg='Data type is not defined to be either int or real.')
    endif

    ier = 0
    if (this%is_real_type) then
       allocate(this%values_real(this%dims%total_size, stat=ier)
    else
       allocate(this%values_int(this%dims%total_size, stat=ier)
    end if

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif
  end subroutine EMIData_AllocateMemory

  
  !------------------------------------------------------------------------
  subroutine EMIData_Reset(this)
    !
    ! !DESCRIPTION:
    ! Resets values of a EMI data
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    if (this%is_real_type) then
       this%values_real(:) = 0._r8
    else
       this%values_int(:) = 0
    end if
    this%is_set = .false.
  end subroutine EMIData_Reset

  
  !------------------------------------------------------------------------
  subroutine EMIData_PrintInfo(this)
    !
    ! !DESCRIPTION:
    ! Print information about the data
    !
    !
    ! !ARGUMENTS:
    class(emi_data) :: this
    write(iulog,*)trim(this%name)
  end subroutine EMIData_PrintInfo

  
  !------------------------------------------------------------------------
  subroutine EMIData_PrintData(this)
    !
    ! !DESCRIPTION:
    ! Print the data value
    !
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    integer :: i
    if (this%is_real_type) then
       do i = 1, this%em_dims
          write(iulog,*)'     ',this%values_real(i)
       enddo
    else
       do i = 1, this%em_dims
          write(iulog,*)'     ',this%values_int(i)
       enddo
    end if
  end subroutine EMIData_PrintData


  !------------------------------------------------------------------------
  subroutine EMIData_Destroy(this)
    !
    ! !DESCRIPTION:
    ! Destroys a EMI data
    !
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    if (associated(this%values_real)) deallocate(this%values_real)
    if (associated(this%values_int)) deallocate(this%values_int)
    call this%filter%Destroy()
  end subroutine EMIData_Destroy

end module EMI_DataMod
