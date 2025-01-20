  !------------------------------------------------------------------------
  subroutine EMIDataList_AddDataByID(this, data_id, num_em_stages_val, em_stage_ids_val, index_of_new_data)
    !
    ! !DESCRIPTION:
    ! Add a EMIData to a list
    !
    ! !USES:
    use ExternalModelConstants
    use EMI_Atm2LndType_DataMod
    use EMI_CanopyStateType_DataMod
    use EMI_ChemStateType_DataMod
    use EMI_EnergyFluxType_DataMod
    use EMI_SoilHydrologyType_DataMod
    use EMI_SoilStateType_DataMod
    use EMI_TemperatureType_DataMod
    use EMI_WaterFluxType_DataMod
    use EMI_WaterStateType_DataMod
    use EMI_TemperatureType_DataMod
    use EMI_ColumnType_Constants
    use EMI_ColumnEnergyStateType_Constants
    use EMI_ColumnEnergyStateType_DataMod
    use EMI_ColumnWaterStateType_Constants
    use EMI_ColumnWaterStateType_DataMod
    use EMI_ColumnWaterFluxType_Constants
    use EMI_ColumnWaterFluxType_DataMod
    use EMI_Filter_Constants
    use EMI_Landunit_Constants
    use EMI_CNCarbonStateType_DataMod
    use EMI_CNCarbonStateType_Constants
    use EMI_CNCarbonFluxType_DataMod
    use EMI_CNCarbonFluxType_Constants
    use EMI_CNNitrogenStateType_DataMod
    use EMI_CNNitrogenStateType_Constants
    use EMI_CNNitrogenFluxType_DataMod
    use EMI_CNNitrogenFluxType_Constants
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
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)                   :: this
    integer         , intent(in)           :: data_id
    integer         , intent(in)           :: num_em_stages_val
    integer         , pointer , intent(in) :: em_stage_ids_val(:)
    integer         , intent(out)          :: index_of_new_data
    !
    class(emi_data) , pointer              :: data
    integer                                :: id_val
    integer                                :: ndim
    character (len=32)                     :: name_val
    character (len=128)                    :: long_name_val
    character (len=32)                     :: units_val
    character (len=32)                     :: dim1_beg_name
    character (len=32)                     :: dim2_beg_name
    character (len=32)                     :: dim3_beg_name
    character (len=32)                     :: dim4_beg_name
    character (len=32)                     :: dim1_end_name
    character (len=32)                     :: dim2_end_name
    character (len=32)                     :: dim3_end_name
    character (len=32)                     :: dim4_end_name
    logical                                :: is_int_type  =.false.
    logical                                :: is_real_type =.false.
    logical                                :: data_present =.false.
    logical                                :: data_found   =.false.
    !-------------------------------------------------------------------------

    call this%IsDataIDPresent(data_id, data_present)
    if (data_present) then
       call this%AppendDataEMStages(data_id, num_em_stages_val, &
                  em_stage_ids_val, index_of_new_data)
       return
    endif

    data_found     = .false.

    if (.not.data_found) then
       call EMI_Atm2LndType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if
    if (.not.data_found) then
       call EMI_CanopyStateType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_ChemStateType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_EnergyFluxType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_SoilHydrologyType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_SoilStateType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_TemperatureType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_WaterFluxType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_WaterStateType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_CNCarbonStateType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if
    
    if (.not.data_found) then
       call EMI_CNNitrogenStateType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if
    
    if (.not.data_found) then
       call EMI_CNCarbonFluxType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if

    if (.not.data_found) then
       call EMI_CNNitrogenFluxType_DataInfoByID(data_id, id_val, &
           name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
           dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
           dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
           data_found)
    end if

    if (.not.data_found) then
      call EMI_ColumnEnergyStateType_DataInfoByID(data_id, id_val, &
           name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
           dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
           dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
           data_found)
   end if

   if (.not.data_found) then
      call EMI_ColumnWaterStateType_DataInfoByID(data_id, id_val, &
           name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
           dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
           dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
           data_found)
   end if

   if (.not.data_found) then
      call EMI_ColumnWaterFluxType_DataInfoByID(data_id, id_val, &
           name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
           dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
           dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
           data_found)
   end if

   if (.not.data_found) then
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
   end if

    if (.not.data_found) then
       write(iulog,*)'Unknown EMIData_ id = ',data_id
       call endrun(msg='EMIDataList_AddDataByID: Unknown EMIData_ id.')
    end if

    allocate(data)
    call data%Init()
    call data%Setup(                        &
         id            = id_val,            &
         name          = name_val,          &
         long_name     = long_name_val,     &
         units         = units_val,         &
         num_em_stages = num_em_stages_val, &
         em_stage_ids  = em_stage_ids_val)
    call data%SetType(is_int_type, is_real_type)
    call data%SetNDimension(ndim)
    call data%SetDimBegEndNames( &
         dim1_beg_name, dim1_end_name, &
         dim2_beg_name, dim2_end_name, &
         dim3_beg_name, dim3_end_name, &
         dim4_beg_name, dim4_end_name)
    call this%AddData(data)
    index_of_new_data = this%num_data

    nullify(data)

  end subroutine EMIDataList_AddDataByID
