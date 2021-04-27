module clouds_lib

use iso_c_binding
use hu_stamnes, only: HuStamnes
use ice_cloud_optics, only: IceCloudOptics
use incomplete_beta, only: IncompleteBeta
use optics_utils, only: OpticalProperties
use stochastic_clouds, only: overlap_parameter, TotalWaterPDF

implicit none
private


public :: calculate_overlap, cloud_optics, finalize, initialize


type(IncompleteBeta) :: beta
type(IceCloudOptics) :: ice
type(OpticalProperties) :: ice_optics
type(HuStamnes) :: liquid
type(OpticalProperties) :: liquid_optics
type(TotalWaterPDF):: water_pdf


contains


!> @brief Convert from a c-style string to a fortran-style string.
subroutine c_string_to_fortran_string(cstr, fstr)
  character(kind=c_char, len=1), dimension(*), intent(in) :: cstr !< c-style string.
  character(len=*), intent(inout) :: fstr !< Fortran-style string.

  integer :: i

  fstr = ""
  do i = 1, len(fstr)
    if (cstr(i) .eq. c_null_char) exit
    fstr(i:i) = cstr(i)
  enddo
end subroutine c_string_to_fortran_string


!> @brief Set up library global derived types.
function initialize(beta_path, ice_path, liquid_path, beta_shape) &
  result(error_code) &
  bind(c, name="initialize_clouds_lib")

  character(kind=c_char, len=1), dimension(*), intent(in) :: beta_path !< Path to beta-distribution input file.
  character(kind=c_char, len=1), dimension(*), intent(in) :: ice_path !< Path to ice parameterization input file.
  character(kind=c_char, len=1), dimension(*), intent(in) :: liquid_path !< Path to liquid parameterization input file.
  integer(kind=c_int), intent(in), optional :: beta_shape !< Beta-distribution shape parameter.
  integer(kind=c_int) :: error_code !< Error code.

  integer(kind=c_int) :: beta_shape_
  character(len=1024) :: path

  call c_string_to_fortran_string(beta_path, path)
  call beta%construct(path)
  call c_string_to_fortran_string(ice_path, path)
  call ice%construct(path)
  call ice_optics%construct(ice%bands, ice%band_limits)
  call c_string_to_fortran_string(liquid_path, path)
  call liquid%construct(path)
  call liquid_optics%construct(liquid%bands, liquid%band_limits)
  if (present(beta_shape)) then
    beta_shape_ = beta_shape
  else
    beta_shape_ = 5
  endif
  call water_pdf%construct(beta_shape_, beta_shape_, beta)
  error_code = 0
end function initialize


!> @brief Destroy library global derived types.
function finalize() &
  result(error_code) &
  bind(c, name="finalize_clouds_lib")

  integer(kind=c_int) :: error_code !< Error code.

  call beta%destruct()
  call ice%destruct()
  call ice_optics%destruct()
  call liquid%destruct()
  call liquid_optics%destruct()
  call water_pdf%destruct()
  error_code = 0
end function finalize


!> @brief Sample ice and liquid cloud optical properties for a column.
function cloud_optics(num_bands, band_centers, band_limits, num_layers, &
                      mean_cloud_fraction, mean_liquid_content, mean_ice_content, &
                      overlap, liquid_radius, temperature, beta_liquid, omega_liquid, &
                      g_liquid, beta_ice, omega_ice, g_ice) &
  result(error_code) &
  bind(c, name="cloud_optics")

  integer(kind=c_int), value, intent(in) :: num_bands !< Number of output bands.
  real(kind=c_double), dimension(num_bands), intent(in) :: band_centers !< Center of output bands [cm-1].
  real(kind=c_double), dimension(num_bands + 1), intent(in) :: band_limits !< Limits of output bands [cm-1].
  integer(kind=c_int), value, intent(in) :: num_layers !< Number of layers in the column.
  real(kind=c_double), dimension(num_layers), intent(in) :: mean_cloud_fraction !< Grid box mean cloud fraction.
  real(kind=c_double), dimension(num_layers), intent(in) :: mean_liquid_content !< Grid box mean liquid cloud content.
  real(kind=c_double), dimension(num_layers), intent(in) :: mean_ice_content !< Grid box mean ice cloud content.
  real(kind=c_double), dimension(num_layers - 1), intent(in) :: overlap !< Overlap parameter for adjacent layers.
  real(kind=c_double), value, intent(in) :: liquid_radius !< Liquid droplet radius [microns].
  real(kind=c_double), dimension(num_layers), intent(in) :: temperature !< Layer temperature [K].
  real(kind=c_double), dimension(num_bands, num_layers), intent(inout) :: beta_liquid !< Liquid cloud extinction coefficient [m-1].
  real(kind=c_double), dimension(num_bands, num_layers), intent(inout) :: omega_liquid !< Liquid cloud single-scatter albedo.
  real(kind=c_double), dimension(num_bands, num_layers), intent(inout) :: g_liquid !< Liquid cloud asymmetry factor.
  real(kind=c_double), dimension(num_bands, num_layers), intent(inout) :: beta_ice !< Ice cloud extincion coefficient [m-1].
  real(kind=c_double), dimension(num_bands, num_layers), intent(inout) :: omega_ice !< Ice cloud single-scatter albedo.
  real(kind=c_double), dimension(num_bands, num_layers), intent(inout) :: g_ice !< Ice cloud asymmetry factor.
  integer(kind=c_int) :: error_code !< Error code.

  integer(kind=c_int) :: i
  real(kind=c_double), dimension(num_layers) :: ice_content
  type(OpticalProperties) :: ice_optics
  real(kind=c_double), dimension(num_layers) :: liquid_content
  type(OpticalProperties) :: remapped_ice_optics
  type(OpticalProperties) :: remapped_liquid_optics

  call remapped_liquid_optics%construct(band_centers, band_limits)
  call remapped_ice_optics%construct(band_centers, band_limits)
  call water_pdf%sample_condensate(mean_cloud_fraction, mean_liquid_content, &
                                   mean_ice_content, overlap, liquid_content, ice_content)
  do i = 1, num_layers
    if (liquid_content(i) .gt. 0._c_double) then
      call liquid%optics(liquid_content(i), liquid_radius, liquid_optics)
      call liquid_optics%thick_average(remapped_liquid_optics, &
                                       ending_band=liquid%last_ir_band)
      beta_liquid(:, i) = remapped_liquid_optics%extinction_coefficient(:)
      omega_liquid(:, i) = remapped_liquid_optics%single_scatter_albedo(:)
      g_liquid(:, i) = remapped_liquid_optics%asymmetry_factor(:)
    else
      beta_liquid(:, i) = 0._c_double
      omega_liquid(:, i) = 0._c_double
      g_liquid(:, i) = 0._c_double
    endif
    if (ice_content(i) .gt. 0._c_double) then
      call ice%optics(ice_content(i), -1._c_double, 1._c_double, temperature(i), ice_optics)
      call ice_optics%thick_average(remapped_ice_optics, ending_band=ice%last_ir_band)
      beta_ice(:, i) = remapped_ice_optics%extinction_coefficient(:)
      omega_ice(:, i) = remapped_ice_optics%single_scatter_albedo(:)
      g_ice(:, i) = remapped_ice_optics%asymmetry_factor(:)
    else
      beta_ice(:,i) = 0._c_double
      omega_ice(:,i) = 0._c_double
      g_ice(:,i) = 0._c_double
    endif
  enddo
  call remapped_liquid_optics%destruct()
  call remapped_ice_optics%destruct()
  error_code = 0
end function cloud_optics


!> @brief Calculates the overlap parameter defined in equation 2 from
!!        doi: 10.1029/2004JD005100.
function calculate_overlap(num_layers, altitude, scale_length, alpha) &
  result(error_code) &
  bind(c, name="calculate_overlap")

  integer(kind=c_int), value, intent(in) :: num_layers !< Number of layers.
  real(kind=c_double), dimension(num_layers), intent(in) :: altitude !< Layer altitude [km].
  real(kind=c_double), value, intent(in) :: scale_length !< Scale length [km].
  real(kind=c_double), dimension(num_layers - 1), intent(out) :: alpha !< Overlap parameter between adjacent layers.
  integer(kind=c_int) :: error_code !< Error code.

  call overlap_parameter(altitude, scale_length, alpha)
  error_code = 0
end function calculate_overlap


end module clouds_lib
