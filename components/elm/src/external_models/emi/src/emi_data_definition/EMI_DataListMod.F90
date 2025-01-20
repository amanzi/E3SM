module EMI_DataListMod
  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This module provides classes storing emi_data objects
  !
  use EMI_DataMod, only emi_data

  implicit none

  type :: emi_data_list_entry
     integer                            :: index
     type(emi_data), pointer            :: data
     integer                            :: num_em_stages       ! Number of EM stages in which the data is exchanged
     integer, allocatable               :: em_stages(:)        ! ID of EM stages in which the data is exchanged

     type(emi_data_list_entry), pointer :: next                ! used in a linked list

   contains
     procedure, public :: Init          => EMIDataListEntry_Init
     procedure, public :: Destroy       => EMIDataListEntry_Destroy
     procedure, public :: AppendStages  => EMIDataListEntry_AppendStages

  end type emi_data_list_entry

  
  type :: emi_data_list
     !
     ! !DESCRIPTION:
     ! A linked list of emi_data
     !
     type(emi_data_list_entry), pointer :: first
     type(emi_data_list_entry), pointer :: last
     logical :: owning ! does this list own the data entries

   contains
     procedure, public :: Init               => EMIDataList_Init
     procedure, public :: Setup              => EMIDataList_Setup
     procedure, public :: Destroy            => EMIDataList_Destroy
     procedure, public :: Append             => EMIDataList_Append
     procedure, public :: HasIndex           => EMIDataList_HasIndex
     procedure, public :: GetData            => EMIDataList_GetData
     procedure, public :: PrintInfo          => EMIDataList_PrintInfo

     procedure, public :: GetIntValue => EMIDataList_GetIntValue
     procedure, public :: GetPointerToReal1D => EMIDataList_GetPointerToReal1D
     procedure, public :: GetPointerToReal2D => EMIDataList_GetPointerToReal2D
     procedure, public :: GetPointerToReal3D => EMIDataList_GetPointerToReal3D
     procedure, public :: GetPointerToReal4D => EMIDataList_GetPointerToReal4D
     procedure, public :: GetPointerToInt1D => EMIDataList_GetPointerToInt1D
     procedure, public :: GetPointerToInt2D => EMIDataList_GetPointerToInt2D
     procedure, public :: GetPointerToInt3D => EMIDataList_GetPointerToInt3D
     procedure, public :: GetPointerToInt4D => EMIDataList_GetPointerToInt4D
     
  end type emi_data_list

contains

  !
  ! emi_data_list_entry methods
  !------------------------------------------------------------------------
  subroutine EMIDataListEntry_Init(this, index, data)
    class(emi_data_list_entry)  :: this
    integer                     :: index
    type(emi_data), pointer     :: data

    this%index = index
    this%data => data
    this%num_em_stages = 0
    nullify(this%next)
  end subroutine EMIDataListEntry_Init

  
  subroutine EMIDataListEntry_Destroy(this, owning)
    class(emi_data_list_entry) :: this
    logical :: owning

    if (allocated(this%em_stages)) deallocate(this%em_stages)
    this%num_em_stages = 0

    if (owning) then
       call this%data%Destroy()
       deallocate(this%data)
    end if
  end subroutine EMIDataListEntry_Destroy

  
  subroutine EMIDataListEntry_AppendStages(em_stages)
    class(emi_data_list_entry) :: this
    integer :: em_stages(:)

    integer                        :: num_em_stages_combined
    integer, allocatable           :: em_stages_combined(:)
    integer                        :: num_unique_em_stages
    integer, allocatable           :: unique_em_stages(:)
    integer                        :: iem, i
    logical                        :: is_unique

    num_combined = size(em_stages) + size(this%em_stages)
    allocate(em_stages_combined(num_combined))

    if (size(this%em_stages) > 0) then
       do i = 1, size(this%em_stages)
          em_stages_combined(i) = this%em_stages(i)
       enddo
    end if

    num_unique = size(this%em_stages)
    do iem = 1, size(em_stages)
       is_unique = .TRUE.
       do i = 1,num_unique
          if (em_stages_combined(i) == em_stages(iem)) then
             is_unique = .FALSE.
             exit
          end if
       end do

       if (is_unique) then
          num_unique = num_unique + 1
          em_stages_combined(num_unique) = em_stages(iem)
       end if
    enddo

    if (allocated(this%em_stage_ids)) deallocate(this%em_stage_ids)
    allocate(this%em_stage_ids(num_unique))
    this%num_em_stages = num_unique

    iem = 0
    do iem = 1, num_unique
       this%em_stage_ids(iem) = em_stages_combined(iem)
    enddo

    deallocate(em_stages_combined)
  end subroutine EMIDataListEntry_AppendStages


  !
  ! emi_data_list_entry methods
  !------------------------------------------------------------------------
  subroutine EMIDataList_Init(this)
    class(emi_data_list) :: this

    this%num_data = 0
    nullify(this%first)
    nullify(this%last)
  end subroutine EMIDataList_Init

  
  !------------------------------------------------------------------------
  subroutine EMIDataList_Append(this, index, new_data, em_stages)
    !
    ! !DESCRIPTION:
    ! Add a EMIData_ to a list
    !
    ! !ARGUMENTS:
    !
    class(emi_data_list)     :: this
    integer                  :: index
    class(emi_data), pointer :: new_data
    integer, optional :: em_stages(:)

    class(emi_data_list_entry), pointer :: list_entry
    allocate(list_entry)
    call list_entry%Init(index, new_data)

    this%num_data = this%num_data + 1    
    
    if (.not.associated(this%first)) then
       this%first => list_entry
    endif
    
    if (associated(this%last)) then
       this%last%next => list_entry
    endif

    this%last => list_entry

  end subroutine EMIDataList_AddData

  
  !------------------------------------------------------------------------
  subroutine EMIDataList_Setup(this)
    class(emi_data_list) :: this

    class(emi_data_list_entry), pointer :: cur_data

    if (this%owning) then
       cur_data => this%first
       do
          if (.not.associated(cur_data)) exit
          cur_data%data%Setup()
          cur_data => cur_data%next
       enddo
    end if
  end subroutine EMIDataList_Setup

  
  !------------------------------------------------------------------------
  subroutine EMIDataList_Destroy(this)
    !
    ! !DESCRIPTION:
    ! Destroys a EMIData list
    !
    ! !ARGUMENTS:
    class(emi_data_list) :: this

    ! !LOCAL VARIABLES:
    class(emi_data_list_entry)      , pointer    :: cur_data
    class(emi_data_list_entry)      , pointer    :: old_data

    cur_data => this%first
    do
       if (.not.associated(cur_data)) exit
       old_data => cur_data
       cur_data => cur_data%next
       call old_data%Destroy(this%owning)
       deallocate(old_data)
    enddo

    this%num_data = 0
    nullify(this%first)
    nullify(this%last)
  end subroutine EMIDataList_Destroy
  

  !------------------------------------------------------------------------
  subroutine EMIDataList_HasIndex(this, index, is_present)
    class(emi_data_list)     :: this
    integer, intent(in)      :: index
    logical, intent(inout)   :: data_present
    !
    class(emi_data_list_entry), pointer :: cur_data
    data_present = .false.

    cur_data => this%first
    do
       if (.not.associated(cur_data)) exit
       if (cur_data%index == index) then
          data_present = .true.
          exit
       endif
       cur_data => cur_data%next
    enddo
  end subroutine EMIDataList_HasID

  
  !------------------------------------------------------------------------
  subroutine EMIDataList_GetEntry(this, index, list_entry)
    class(emi_data_list)                , intent(in)    :: this
    integer                             , intent(in)    :: index
    class(emi_data_list_entry), pointer , intent(inout) :: list_entry

    list_entry => this%first
    do
       if (.not.associated(list_entry)) exit
       if (list_entry%index == index) then
          data_present = .true.
          exit
       endif
       list_entry => list_entry%next
    enddo
  end subroutine EMIDataList_GetEntry

  
  subroutine EMIDataList_GetData(this, index, data)
    class(emi_data_list)                , intent(in)    :: this
    integer                             , intent(in)    :: index
    class(emi_data), pointer , intent(inout) :: data
    
    class(emi_data_list_entry) :: list_entry
    integer :: values(:)
    
    call this%GetEntry(index, list_entry)
    data => list_entry%data
  end subroutine EMIDataList_GetData


  !------------------------------------------------------------------------
  subroutine EMIDataList_PrintInfo(this)
    class(emi_data_list)     :: this
    !
    class(emi_data)      , pointer    :: cur_data
    integer :: count

    cur_data => this%first
    do
       if (.not.associated(cur_data)) exit
       call cur_data%data%PrintInfo()
       cur_data => cur_data%next
    enddo
  end subroutine EMIDataList_PrintInfo


  !------------------------------------------------------------------------
  subroutine EMIDataList_GetIntValue(this, index, value)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, intent(inout)           :: value

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_INT)
    call data%CheckDim(1)

    if (data%em_size%dims(1)%end /= data%em_size%dims(1)%beg) then
       call endrun(msg='EMIDListGetIntValue: Only extracts values from data ' // &
            'that has 1 value.')
    endif
    value = data%values_int(data%em_size%dims(1)%end)
  end subroutine EMIDataList_GetIntValue


  !------------------------------------------------------------------------
  subroutine EMIDataList_GetPointerToInt1D(this, index, values)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, pointer, intent(inout)  :: values(:)

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_INT)
    call data%CheckDim(1)

    values(data%em_size%dims(1)%beg:data%em_size%dims(1)%end) => data%values_int
  end subroutine EMIDataList_GetPointerToInt1D


  !------------------------------------------------------------------------
  subroutine EMIDataList_GetPointerToInt2D(this, index, values)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, pointer, intent(inout)  :: values(:,:)

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_INT)
    call data%CheckDim(2)

    values(data%em_size%dims(1)%beg:data%em_size%dims(1)%end, &
         data%em_size%dims(2)%beg:data%em_size%dims(2)%end) => data%values_int
  end subroutine EMIDataList_GetPointerToInt2D
  

  !------------------------------------------------------------------------
  subroutine EMIDataList_GetPointerToInt3D(this, index, values)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, pointer, intent(inout)  :: values(:,:,:)

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_INT)
    call data%CheckDim(3)

    values(data%em_size%dims(1)%beg:data%em_size%dims(1)%end, &
         data%em_size%dims(2)%beg:data%em_size%dims(2)%end, &
         data%em_size%dims(3)%beg:data%em_size%dims(3)%end) => data%values_int
  end subroutine EMIDataList_GetPointerToInt3D


  !------------------------------------------------------------------------
  subroutine EMIDataList_GetPointerToReal1D(this, index, values)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, pointer, intent(inout)  :: values(:)

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_REAL)
    call data%CheckDim(1)

    values(data%em_size%dims(1)%beg:data%em_size%dims(1)%end) => data%values_real
  end subroutine EMIDataList_GetPointerToReal1D


  !------------------------------------------------------------------------
  subroutine EMIDataList_GetPointerToReal2D(this, index, values)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, pointer, intent(inout)  :: values(:,:)

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_REAL)
    call data%CheckDim(2)

    values(data%em_size%dims(1)%beg:data%em_size%dims(1)%end, &
         data%em_size%dims(2)%beg:data%em_size%dims(2)%end) => data%values_real
  end subroutine EMIDataList_GetPointerToReal2D
  

  !------------------------------------------------------------------------
  subroutine EMIDataList_GetPointerToReal3D(this, index, values)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, pointer, intent(inout)  :: values(:,:,:)

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_REAL)
    call data%CheckDim(3)

    values(data%em_size%dims(1)%beg:data%em_size%dims(1)%end, &
         data%em_size%dims(2)%beg:data%em_size%dims(2)%end, &
         data%em_size%dims(3)%beg:data%em_size%dims(3)%end) => data%values_real
  end subroutine EMIDataList_GetPointerToReal3D


  !------------------------------------------------------------------------
  subroutine EMIDataList_GetPointerToReal4D(this, index, values)
    class(emi_data_list), intent(in) :: this
    integer, intent(in)              :: index
    integer, pointer, intent(inout)  :: values(:,:,:,:)

    type(emi_data), pointer :: data

    call this%GetData(index, data)
    call data%CheckType(EMI_VALUE_TYPE_REAL)
    call data%CheckDim(4)

    values(data%em_size%dims(1)%beg:data%em_size%dims(1)%end, &
         data%em_size%dims(2)%beg:data%em_size%dims(2)%end, &
         data%em_size%dims(3)%beg:data%em_size%dims(3)%end, &
         data%em_size%dims(4)%beg:data%em_size%dims(4)%end) => data%values_real
  end subroutine EMIDataList_GetPointerToReal4D
    
end module EMI_DataListMod

  
