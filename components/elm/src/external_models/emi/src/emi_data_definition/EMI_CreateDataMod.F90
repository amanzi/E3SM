module EMI_CreateDataMod

  implicit none

contains

  !------------------------------------------------------------------------
  function CreateDataByID(data_id) result(data)
    !
    ! !DESCRIPTION:
    ! Create an emi_data type
    !
    ! !USES:
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
    use EMI_ColumnEnergyStateType_DataMod
    use EMI_ColumnWaterStateType_DataMod
    use EMI_ColumnWaterFluxType_DataMod
    use EMI_CNCarbonStateType_DataMod
    use EMI_CNCarbonFluxType_DataMod
    use EMI_CNNitrogenStateType_DataMod
    use EMI_CNNitrogenFluxType_DataMod
    use EMI_ColumnData_DataMod

    !
    ! !ARGUMENTS:
    integer         , intent(in)           :: data_id
    type(emi_data), pointer, intent(out)   :: data

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

    if (.not.data_found) then
       call EMI_ColumnType_DataInfoByID(data_id, id_val, &
            name_val, long_name_val, units_val, is_int_type, is_real_type, ndim, &
            dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
            dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
            data_found)
    end if
    
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
       write(iulog,*)'Unknown EMIData ID = ',data_id
       call endrun(msg='EMI_CreateDataByID: Unknown EMIData id.')
    end if

   allocate(data)
   call data%Init()

   if (is_int_type) value_type = EMI_VALUE_TYPE_INT
   if (is_real_type) value_type = EMI_VALUE_TYPE_REAL
   
   call data%Setup(data_id, name_val, long_name_val, units_val, value_type)
   if (ndim >= 1) then
      
      
      call data%SetDim(1, '', 
   

   
 end function CreateDataByID

end module EMI_CreateDataMod
   
