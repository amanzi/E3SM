module EMI_ColumnType_DataMod

  use EMI_ColumnType_Constants

  implicit none
  public :: EMI_ColumnType_DataInfoByID

contains
  !------------------------------------------------------------------------
  subroutine EMI_ColumnType_DataInfoByID(data_id, id_val, name_val, long_name_val,&
        units_val, is_int_type, is_real_type, ndim, &
        dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
        dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
        data_found)
    !
    ! !DESCRIPTION:
    ! Defines information of data exchanged between ELM and EM
    !
    use EMI_DataDimensionMod, only : dimname_begg
    use EMI_DataDimensionMod, only : dimname_endg
    use EMI_DataDimensionMod, only : dimname_begl
    use EMI_DataDimensionMod, only : dimname_endl
    use EMI_DataDimensionMod, only : dimname_begc
    use EMI_DataDimensionMod, only : dimname_endc
    use EMI_DataDimensionMod, only : dimname_begp
    use EMI_DataDimensionMod, only : dimname_endp
    use EMI_DataDimensionMod, only : dimname_nlevsno
    use EMI_DataDimensionMod, only : dimname_nlevsno_plus_one
    use EMI_DataDimensionMod, only : dimname_nlevsoi
    use EMI_DataDimensionMod, only : dimname_nlevgrnd
    use EMI_DataDimensionMod, only : dimname_zero
    use EMI_DataDimensionMod, only : dimname_one
    use EMI_DataDimensionMod, only : dimname_two
    use EMI_DataDimensionMod, only : dimname_col_one_based_idx

    !
    ! !DESCRIPTION:
    ! Defines information of data exchanged between ELM and EM
    !
    ! !USES: 
    use EMI_DataDimensionMod
    implicit none
    !
    ! !ARGUMENTS:
    integer            , intent(in)  :: data_id
    integer            , intent(out) :: id_val
    character (len=32) , intent(out) :: name_val
    character (len=128), intent(out) :: long_name_val
    character (len=32) , intent(out) :: units_val
    logical            , intent(out) :: is_int_type
    logical            , intent(out) :: is_real_type
    integer            , intent(out) :: ndim
    character (len=32) , intent(out) :: dim1_beg_name
    character (len=32) , intent(out) :: dim1_end_name
    character (len=32) , intent(out) :: dim2_beg_name
    character (len=32) , intent(out) :: dim2_end_name
    character (len=32) , intent(out) :: dim3_beg_name
    character (len=32) , intent(out) :: dim3_end_name
    character (len=32) , intent(out) :: dim4_beg_name
    character (len=32) , intent(out) :: dim4_end_name
    logical            , intent(inout) :: data_found

    is_int_type    = .false.
    is_real_type   = .false.
    dim1_beg_name  = ''
    dim2_beg_name  = ''
    dim3_beg_name  = ''
    dim4_beg_name  = ''
    dim1_end_name  = ''
    dim2_end_name  = ''
    dim3_end_name  = ''
    dim4_end_name  = ''

    select case(data_id)

       ! -------------------------------------------------------------
       ! ELM-to-EM: Filter variables
       ! -------------------------------------------------------------
      case (L2E_FILTER_HYDROLOGYC)
          id_val        = L2E_FILTER_HYDROLOGYC
          name_val      = 'Hydrology filter'
          long_name_val = 'Hydrology filter: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_one
          dim1_end_name = dimname_col_one_based_idx
          data_found    = .true.

       case (L2E_FILTER_NUM_HYDROLOGYC)
          id_val        = L2E_FILTER_NUM_HYDROLOGYC
          name_val      = 'Number of hydrology filter'
          long_name_val = 'Number of hydrology filter: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_one
          dim1_end_name = dimname_one
          data_found    = .true.

      case (L2E_FILTER_SOILC)
         id_val        = L2E_FILTER_SOILC
         name_val      = 'Soil filter'
         long_name_val = 'Soil filter: ELM to External Model'
         units_val     = '[-]'
         is_int_type   = .true.
         ndim          = 1
         dim1_beg_name = dimname_one
         dim1_end_name = dimname_col_one_based_idx
         data_found    = .true.

      case (L2E_FILTER_NUM_SOILC)
         id_val        = L2E_FILTER_NUM_SOILC
         name_val      = 'Number of soil filter'
         long_name_val = 'Number of soil filter: ELM to External Model'
         units_val     = '[-]'
         is_int_type   = .true.
         ndim          = 1
         dim1_beg_name = dimname_one
         dim1_end_name = dimname_one
         data_found    = .true.

       case (L2E_FILTER_NOLAKEC)
          id_val        = L2E_FILTER_HYDROLOGYC
          name_val      = 'Non-lake filter'
          long_name_val = 'Non-lake filter: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_one
          dim1_end_name = dimname_col_one_based_idx
          data_found    = .true.

       case (L2E_FILTER_NUM_NOLAKEC)
          id_val        = L2E_FILTER_NUM_HYDROLOGYC
          name_val      = 'Number of non-lake filter'
          long_name_val = 'Number of non-lake filter: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_one
          dim1_end_name = dimname_one
          data_found    = .true.

       case (L2E_FILTER_NOLAKEC_AND_NOURBANC)
          id_val        = L2E_FILTER_NOLAKEC_AND_NOURBANC
          name_val      = 'Non-lake & non-urban filter'
          long_name_val = 'Non-lake & non-urban filter: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_one
          dim1_end_name = dimname_col_one_based_idx
          data_found    = .true.

       case (L2E_FILTER_NUM_NOLAKEC_AND_NOURBANC)
          id_val        = L2E_FILTER_NUM_NOLAKEC_AND_NOURBANC
          name_val      = 'Number of non-lake & non-urban filter'
          long_name_val = 'Number of non-lake & non-urban filter: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_one
          dim1_end_name = dimname_one
          data_found    = .true.

          ! -------------------------------------------------------------
          ! ELM-to-ELM: Column variables
          ! -------------------------------------------------------------
       case (L2E_COLUMN_ACTIVE)
          id_val        = L2E_COLUMN_ACTIVE
          name_val      = 'Column active'
          long_name_val = 'Column active: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_TYPE)
          id_val        = L2E_COLUMN_TYPE
          name_val      = 'Column type'
          long_name_val = 'Column type: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_LANDUNIT_INDEX)
          id_val        = L2E_COLUMN_LANDUNIT_INDEX
          name_val      = 'Column to landunit index'
          long_name_val = 'Column landunit index: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_ZI)
          id_val        = L2E_COLUMN_ZI
          name_val      = 'Column layer interface depth'
          long_name_val = 'Column layer interface depth: ELM to External Model'
          units_val     = '[m]'
          is_real_type  = .true.
          ndim          = 2
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          dim2_beg_name = dimname_zero
          dim2_end_name = dimname_nlevgrnd
          data_found    = .true.

       case (L2E_COLUMN_DZ)
          id_val        = L2E_COLUMN_DZ
          name_val      = 'Column layer thickness'
          long_name_val = 'Column layer thickness: ELM to External Model'
          units_val     = '[m]'
          is_real_type  = .true.
          ndim          = 2
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          dim2_beg_name = dimname_one
          dim2_end_name = dimname_nlevgrnd
          data_found    = .true.

       case (L2E_COLUMN_Z)
          id_val        = L2E_COLUMN_Z
          name_val      = 'Column layer centroid depth'
          long_name_val = 'Column layer centroid depth: ELM to External Model'
          units_val     = '[m]'
          is_real_type  = .true.
          ndim          = 2
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          dim2_beg_name = dimname_one
          dim2_end_name = dimname_nlevgrnd
          data_found    = .true.

       case (L2E_COLUMN_AREA)
          id_val        = L2E_COLUMN_AREA
          name_val      = 'Column surface area'
          long_name_val = 'Column surface area: ELM to External Model'
          units_val     = '[m2]'
          is_real_type  = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_GRIDCELL_INDEX)
          id_val        = L2E_COLUMN_GRIDCELL_INDEX
          name_val      = 'Column to gridcell index'
          long_name_val = 'Column to gridcell index: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_PATCH_INDEX_BEGIN)
          id_val        = L2E_COLUMN_PATCH_INDEX_BEGIN
          name_val      = 'Beginning column to patch index'
          long_name_val = 'Beginning column to patch index: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_PATCH_INDEX_END)
          id_val        = L2E_COLUMN_PATCH_INDEX_END
          name_val      = 'Ending column to patch index'
          long_name_val = 'Ending column to patch index: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_NUM_SNOW_LAYERS)
          id_val        = L2E_COLUMN_NUM_SNOW_LAYERS
          name_val      = 'Number of snow layers'
          long_name_val = 'Number of snow layers: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

       case (L2E_COLUMN_ZI_SNOW_AND_SOIL)
          id_val        = L2E_COLUMN_ZI_SNOW_AND_SOIL
          name_val      = 'Column layer interface depth'
          long_name_val = 'Column layer interface depth: ELM to External Model'
          units_val     = '[m]'
          is_real_type  = .true.
          ndim          = 2
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          dim2_beg_name = dimname_nlevsno
          dim2_end_name = dimname_nlevgrnd
          data_found    = .true.

       case (L2E_COLUMN_DZ_SNOW_AND_SOIL)
          id_val        = L2E_COLUMN_DZ_SNOW_AND_SOIL
          name_val      = 'Column layer thickness'
          long_name_val = 'Column layer thickness: ELM to External Model'
          units_val     = '[m]'
          is_real_type  = .true.
          ndim          = 2
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          dim2_beg_name = dimname_nlevsno_plus_one
          dim2_end_name = dimname_nlevgrnd
          data_found    = .true.

       case (L2E_COLUMN_Z_SNOW_AND_SOIL)
          id_val        = L2E_COLUMN_Z_SNOW_AND_SOIL
          name_val      = 'Column layer centroid depth'
          long_name_val = 'Column layer centroid depth: ELM to External Model'
          units_val     = '[m]'
          is_real_type  = .true.
          ndim          = 2
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          dim2_beg_name = dimname_nlevsno_plus_one
          dim2_end_name = dimname_nlevgrnd
          data_found    = .true.

       case (L2E_COLUMN_NUM_PATCH)
          id_val        = L2E_COLUMN_NUM_PATCH
          name_val      = 'Number of patches'
          long_name_val = 'Number of patches in column'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.

      case (L2E_COLUMN_PFT_TYPE)
          id_val        = L2E_COLUMN_PFT_TYPE
          name_val      = 'Active PFT type'
          long_name_val = 'Active PFT on column - assumes 1 PFT per column'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begc
          dim1_end_name = dimname_endc
          data_found    = .true.
          ! -------------------------------------------------------------
          ! ELM-to-ELM: Landunit variables
          ! -------------------------------------------------------------
       case (L2E_LANDUNIT_TYPE)
          id_val        = L2E_LANDUNIT_TYPE
          name_val      = 'Landunit type'
          long_name_val = 'Landunit type: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begl
          dim1_end_name = dimname_endl
          data_found    = .true.

       case (L2E_LANDUNIT_LAKEPOINT)
          id_val        = L2E_LANDUNIT_LAKEPOINT
          name_val      = 'Landunit lake point'
          long_name_val = 'Landunit lake point: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begl
          dim1_end_name = dimname_endl
          data_found    = .true.

       case (L2E_LANDUNIT_URBANPOINT)
          id_val        = L2E_LANDUNIT_URBANPOINT
          name_val      = 'Landunit urban point'
          long_name_val = 'Landunit urban point: ELM to External Model'
          units_val     = '[-]'
          is_int_type   = .true.
          ndim          = 1
          dim1_beg_name = dimname_begl
          dim1_end_name = dimname_endl
          data_found    = .true.
       end select
       
   end subroutine EMI_ColumnType_DataInfoByID

 end module EMI_ColumnType_DataMod
