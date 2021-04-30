/* @brief NetCDF utilities.*/
#include <stdio.h>
#include <stdlib.h>
#include "netcdf.h"


/* @brief Crashes if any netCDF errors are detected.*/
void netcdf_catch(int const err)
{
    if (err != NC_NOERR)
    {
        fprintf(stderr, "%s", nc_strerror(err));
        exit(1);
    }
    return;
}


/* @brief Opens netCDF dataset.*/
int open_dataset(char const * path)
{
    int ncid;
    netcdf_catch(nc_open(path, NC_NOWRITE, &ncid));
    return ncid;
}


/* @brief Closes netCDF dataset.*/
void close_dataset(int const ncid)
{
    netcdf_catch(nc_close(ncid));
    return;
}


/* @brief Reads an attribute from netCDF dataset.*/
void read_attribute(int const ncid, char const * variable, char const * name, void * buffer)
{
    int varid;
    netcdf_catch(nc_inq_varid(ncid, variable, &varid));
    netcdf_catch(nc_get_att(ncid, varid, name, buffer));
    return;
}


/* @brief Read the length of a dimension from a netCDF dataset.*/
void read_dimlen(int const ncid, char const * name, int * length)
{
    int dimid;
    netcdf_catch(nc_inq_dimid(ncid, name, &dimid));
    size_t size;
    netcdf_catch(nc_inq_dimlen(ncid, dimid, &size));
    *length = (int)size;
    return;
}


/* @brief Reads variable from netCDF dataset.*/
void read_variable(int const ncid, char const * name, void ** buffer,
                   nc_type const type, size_t const * start, size_t const * count)
{
    int varid;
    netcdf_catch(nc_inq_varid(ncid, name, &varid));
    int ndims;
    netcdf_catch(nc_inq_varndims(ncid, varid, &ndims));
    size_t corner[ndims];
    if (start == NULL)
    {
        int i;
        for (i=0; i<ndims; ++i)
        {
             corner[i] = 0;
        }
    }
    else
    {
        int i;
        for (i=0; i<ndims; ++i)
        {
            corner[i] = start[i];
        }
    }
    size_t sizes[ndims];
    if (count == NULL)
    {
        int dimids[ndims];
        netcdf_catch(nc_inq_vardimid(ncid, varid, dimids));
        int i;
        for (i=0; i<ndims; ++i)
        {
            netcdf_catch(nc_inq_dimlen(ncid, dimids[i], &(sizes[i])));
            sizes[i] -= corner[i];
        }
    }
    else
    {
        int i;
        for (i=0; i<ndims; ++i)
        {
            sizes[i] = count[i];
        }
    }
    size_t total_size = 1;
    int i;
    for (i=0; i<ndims; ++i)
    {
        total_size *= sizes[i];
    }
    size_t num_bytes;
    if (type == NC_INT)
    {
        num_bytes = sizeof(int);
    }
    else if (type == NC_FLOAT)
    {
        num_bytes = sizeof(float);
    }
    else if (type == NC_DOUBLE)
    {
        num_bytes = sizeof(double);
    }
    else
    {
        fprintf(stderr, "non-supported netcdf type.");
        exit(1);
    }
    void * data = malloc(num_bytes*total_size);
    if (type == NC_INT)
    {
        netcdf_catch(nc_get_vara_int(ncid, varid, corner, sizes, (int *)data));
    }
    else if (type == NC_FLOAT)
    {
        netcdf_catch(nc_get_vara_float(ncid, varid, corner, sizes, (float *)data));
    }
    else if (type == NC_DOUBLE)
    {
        netcdf_catch(nc_get_vara_double(ncid, varid, corner, sizes, (double *)data));
    }
    *buffer = data;
    return;
}


#ifdef FOO










/* @brief Add a dimension to a netCDF dataset.*/
int add_dimension(int const ncid, char const * name, size_t const * length)
{
    size_t s;
    if (length == NULL)
    {
        s = NC_UNLIMITED;
    }
    else
    {
        s = *length;
    }
    int dimid;
    netcdf_catch(nc_def_dim(ncid, name, s, &dimid));
    return dimid;
}























!> @brief Add a variable to a netCDF dataset.
function add_variable(ncid, dimid, name, standard_name, units, fill_value, &
                      positive, axis, calendar) result(varid)

  integer, intent(in) :: ncid !< NetCDF id.
  integer, dimension(:), intent(in) :: dimid !< Dimension ids.
  character(len=*), intent(in) :: name !< Variable name.
  character(len=*), intent(in) :: standard_name !< Variable standard name.
  character(len=*), intent(in), optional :: units !< Variable units.
  real(kind=c_double), intent(in), optional :: fill_value !< Fill value.
  character(len=*), intent(in), optional :: positive !< Vertical sense.
  character(len=1), intent(in), optional :: axis !< Axis id.
  character(len=*), intent(in), optional :: calendar !< Calendar.
  integer :: varid

  integer :: err

  err = nf90_def_var(ncid, trim(name), nf90_double, dimid, varid)
  call netcdf_catch(err)
  err = nf90_put_att(ncid, varid, "standard_name", trim(standard_name))
  call netcdf_catch(err)
  if (present(units)) then
    err = nf90_put_att(ncid, varid, "units", trim(units))
    call netcdf_catch(err)
  endif
  if (present(fill_value)) then
    err = nf90_put_att(ncid, varid, "_FillValue", fill_value)
    call netcdf_catch(err)
  endif
  if (present(positive)) then
    err = nf90_put_att(ncid, varid, "positive", trim(positive))
    call netcdf_catch(err)
  endif
  if (present(axis)) then
    err = nf90_put_att(ncid, varid, "axis", trim(axis))
    call netcdf_catch(err)
  endif
  if (present(calendar)) then
    err = nf90_put_att(ncid, varid, "calendar", trim(calendar))
    call netcdf_catch(err)
  endif
end function add_variable


!> @brief Creates netCDF dataset.
function create_dataset(path) result(ncid)

  character(len=*), intent(in) :: path !< File path.
  integer :: ncid !< NetCDF id.

  integer :: err

  err = nf90_create(trim(path), nf90_netcdf4, ncid)
  call netcdf_catch(err)
end function create_dataset


!> @brief Gets the length of a dimension.
function dimension_length(ncid, name) result(length)

  integer, intent(in) :: ncid
  character(len=*), intent(in) :: name
  integer :: length

  integer :: dimid
  integer :: err

  err = nf90_inq_dimid(ncid, trim(name), dimid)
  call netcdf_catch(err)
  err = nf90_inquire_dimension(ncid, dimid, len=length)
  call netcdf_catch(err)
  return
end function dimension_length






!> @brief Reads an attribute from netCDF dataset.
subroutine read_attribute_int(ncid, variable, name, buffer)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: variable !< Variable name.
  character(len=*), intent(in) :: name !< Attribute name.
  integer, intent(out) :: buffer

  integer :: err
  integer :: varid

  err = nf90_inq_varid(ncid, trim(variable), varid)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, trim(name), buffer)
  call netcdf_catch(err)
end subroutine read_attribute_int


!> @brief Reads an attribute from netCDF dataset.
subroutine read_attribute_string(ncid, variable, name, buffer)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: variable !< Variable name.
  character(len=*), intent(in) :: name !< Attribute name.
  character(len=:), allocatable, intent(inout) :: buffer

  integer :: err
  integer :: n
  integer :: varid

  err = nf90_inq_varid(ncid, trim(variable), varid)
  call netcdf_catch(err)
  err = nf90_inquire_attribute(ncid, varid, trim(name), len=n)
  call netcdf_catch(err)
  if (allocated(buffer)) deallocate(buffer)
  allocate(character(len=n) :: buffer)
  err = nf90_get_att(ncid, varid, trim(name), buffer)
  call netcdf_catch(err)
end subroutine read_attribute_string


!> @brief Reads an attribute from netCDF dataset.
subroutine read_attribute_1d_real(ncid, variable, name, buffer)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: variable !< Variable name.
  character(len=*), intent(in) :: name !< Attribute name.
  real(kind=c_double), dimension(:), intent(out) :: buffer

  integer :: err
  integer :: varid

  err = nf90_inq_varid(ncid, trim(variable), varid)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, trim(name), buffer)
  call netcdf_catch(err)
end subroutine read_attribute_1d_real


!> @brief Reads an attribute from netCDF dataset.
subroutine read_global_attribute_real(ncid, name, buffer)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Attribute name.
  real(kind=c_double), intent(out) :: buffer

  integer :: err

  err = nf90_get_att(ncid, nf90_global, trim(name), buffer)
  call netcdf_catch(err)
end subroutine read_global_attribute_real


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_1d_string(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  character(len=*), dimension(:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  integer, dimension(nf90_max_dims) :: sizes
  integer, dimension(nf90_max_dims) :: starts
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(2)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
end subroutine read_variable_1d_string


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_0d_int(ncid, name, buffer, start)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  integer, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices

  integer :: add
  integer :: err
  integer :: n
  integer :: scales
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_get_var(ncid, varid, buffer, start)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0
  else
    call netcdf_catch(err)
  endif
  buffer = buffer*scales + add
end subroutine read_variable_0d_int


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_1d_int(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  integer, dimension(:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer :: add
  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  integer :: scales
  integer, dimension(nf90_max_dims) :: sizes
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(1)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0
  else
    call netcdf_catch(err)
  endif
  buffer(:) = buffer(:)*scales + add
end subroutine read_variable_1d_int


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_2d_int(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  integer, dimension(:,:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer :: add
  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  integer :: scales
  integer, dimension(nf90_max_dims) :: sizes
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(1), sizes(2)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0
  else
    call netcdf_catch(err)
  endif
  buffer(:,:) = buffer(:,:)*scales + add
end subroutine read_variable_2d_int


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_3d_int(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  integer, dimension(:,:,:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer :: add
  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  integer :: scales
  integer, dimension(nf90_max_dims) :: sizes
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(1), sizes(2), sizes(3)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0
  else
    call netcdf_catch(err)
  endif
  buffer(:,:,:) = buffer(:,:,:)*scales + add
end subroutine read_variable_3d_int


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_0d_real(ncid, name, buffer, start)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  real(kind=c_double), intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices

  real(kind=c_double) :: add
  integer :: err
  real(kind=c_double) :: scales
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_get_var(ncid, varid, buffer, start)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1._c_double
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0._c_double
  else
    call netcdf_catch(err)
  endif
  buffer = buffer*scales + add
end subroutine read_variable_0d_real


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_1d_real(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  real(kind=c_double), dimension(:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  real(kind=c_double) :: add
  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  real(kind=c_double) :: scales
  integer, dimension(nf90_max_dims) :: sizes
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(1)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1._c_double
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0._c_double
  else
    call netcdf_catch(err)
  endif
  buffer(:) = buffer(:)*scales + add
end subroutine read_variable_1d_real


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_2d_real(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  real(kind=c_double), dimension(:,:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  real(kind=c_double) :: add
  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  real(kind=c_double) :: scales
  integer, dimension(nf90_max_dims) :: sizes
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(1), sizes(2)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1._c_double
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0._c_double
  else
    call netcdf_catch(err)
  endif
  buffer(:,:) = buffer(:,:)*scales + add
end subroutine read_variable_2d_real


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_3d_real(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  real(kind=c_double), dimension(:,:,:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  real(kind=c_double) :: add
  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  real(kind=c_double) :: scales
  integer, dimension(nf90_max_dims) :: sizes
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(1), sizes(2), sizes(3)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1._c_double
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0._c_double
  else
    call netcdf_catch(err)
  endif
  buffer(:,:,:) = buffer(:,:,:)*scales + add
end subroutine read_variable_3d_real


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_4d_real(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  real(kind=c_double), dimension(:,:,:,:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  real(kind=c_double) :: add
  integer, dimension(nf90_max_dims) :: dimids
  integer :: err
  integer :: i
  integer :: n
  real(kind=c_double) :: scales
  integer, dimension(nf90_max_dims) :: sizes
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  call netcdf_catch(err)
  err = nf90_inquire_variable(ncid, varid, ndims=n)
  call netcdf_catch(err)
  if (present(counts)) then
    sizes(:n) = counts(:n)
  else
    err = nf90_inquire_variable(ncid, varid, dimids=dimids)
    call netcdf_catch(err)
    do i = 1, n
      err = nf90_inquire_dimension(ncid, dimids(i), len=sizes(i))
      call netcdf_catch(err)
    enddo
  endif
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(sizes(1), sizes(2), sizes(3), sizes(4)))
  err = nf90_get_var(ncid, varid, buffer, start, sizes)
  call netcdf_catch(err)
  err = nf90_get_att(ncid, varid, "scale_factor", scales)
  if (err .eq. nf90_enotatt) then
    scales = 1._c_double
  else
    call netcdf_catch(err)
  endif
  err = nf90_get_att(ncid, varid, "add_offset", add)
  if (err .eq. nf90_enotatt) then
    add = 0._c_double
  else
    call netcdf_catch(err)
  endif
  buffer(:,:,:,:) = buffer(:,:,:,:)*scales + add
end subroutine read_variable_4d_real


!> @brief Writes variable to netCDF dataset.
subroutine write_variable_1d_real(ncid, varid, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  integer, intent(in) :: varid !< Variable id.
  real(kind=c_double), dimension(:), intent(in) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer :: err

  err = nf90_put_var(ncid, varid, buffer, start, counts)
  call netcdf_catch(err)
end subroutine write_variable_1d_real


!> @brief Writes variable to netCDF dataset.
subroutine write_variable_2d_real(ncid, varid, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  integer, intent(in) :: varid !< Variable id.
  real(kind=c_double), dimension(:,:), intent(in) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer :: err

  err = nf90_put_var(ncid, varid, buffer, start, counts)
  call netcdf_catch(err)
end subroutine write_variable_2d_real


!> @brief Writes variable to netCDF dataset.
subroutine write_variable_3d_real(ncid, varid, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  integer, intent(in) :: varid !< Variable id.
  real(kind=c_double), dimension(:,:,:), intent(in) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer :: err

  err = nf90_put_var(ncid, varid, buffer, start, counts)
  call netcdf_catch(err)
end subroutine write_variable_3d_real


!> @brief Writes variable to netCDF dataset.
subroutine write_variable_4d_real(ncid, varid, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  integer, intent(in) :: varid !< Variable id.
  real(kind=c_double), dimension(:,:,:,:), intent(in) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer :: err

  err = nf90_put_var(ncid, varid, buffer, start, counts)
  call netcdf_catch(err)
end subroutine write_variable_4d_real


!> @brief Reads variable from netCDF dataset.
subroutine read_variable_1d_int_to_bool(ncid, name, buffer, start, counts)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  logical, dimension(:), allocatable, intent(inout) :: buffer
  integer, dimension(:), intent(in), optional :: start !< Corner indices
  integer, dimension(:), intent(in), optional :: counts !< Edge lengths.

  integer, dimension(:), allocatable :: buffer1d
  integer :: i

  call read_variable(ncid, name, buffer1d, start, counts)
  if (allocated(buffer)) deallocate(buffer)
  allocate(buffer(size(buffer1d)))
  do i = 1, size(buffer1d)
    if (buffer1d(i) .eq. 0) then
      buffer(i) = .false.
    else
      buffer(i) = .true.
    endif
  enddo
  deallocate(buffer1d)
end subroutine read_variable_1d_int_to_bool


!> @brief Determine if a variable is in a netCDF dataset.
function variable_exists(ncid, name) result(has_variable)

  integer, intent(in) :: ncid !< NetCDF id.
  character(len=*), intent(in) :: name !< Variable name.
  logical :: has_variable

  integer :: err
  integer :: varid

  err = nf90_inq_varid(ncid, trim(name), varid)
  if (err .eq. nf90_enotvar) then
    has_variable = .false.
  else
    call netcdf_catch(err)
    has_variable = .true.
  endif
end function variable_exists
#endif
