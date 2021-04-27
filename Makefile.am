lib_LTLIBRARIES = libclouds.la

AM_CPPFLAGS = -Isrc

#Cloud optics library.
clouds = src
libclouds_la_SOURCES = $(clouds)/c_bindings.f90 \
                       $(clouds)/hu_stamnes.f90 \
                       $(clouds)/ice_cloud_optics.f90 \
                       $(clouds)/incomplete_beta.f90 \
                       $(clouds)/netcdf_utils.f90 \
                       $(clouds)/optics_utils.f90 \
                       $(clouds)/slingo.f90 \
                       $(clouds)/stochastic_clouds.f90
$(clouds)/c_bindings.lo: $(clouds)/hu_stamnes.lo \
                         $(clouds)/ice_cloud_optics.lo \
                         $(clouds)/incomplete_beta.lo \
                         $(clouds)/optics_utils.lo \
                         $(clouds)/stochastic_clouds.lo
$(clouds)/hu_stamnes.lo: $(clouds)/optics_utils.lo \
                         $(clouds)/netcdf_utils.lo
$(clouds)/ice_cloud_optics.lo: $(clouds)/optics_utils.lo \
                               $(clouds)/netcdf_utils.lo
$(clouds)/incomplete_beta.lo: $(clouds)/netcdf_utils.lo
$(clouds)/slingo.lo: $(clouds)/optics_utils.lo \
                     $(clouds)/netcdf_utils.lo
$(clouds)/stochastic_clouds.lo: $(clouds)/incomplete_beta.lo


check_PROGRAMS = ctest
ctest_SOURCES = tests/c-test.c
ctest_LDADD = $(top_builddir)/libclouds.la -lnetcdff


CLEANFILES = *.mod *__genmod.f90
