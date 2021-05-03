lib_LTLIBRARIES = libclouds.la libcloudsc.la

AM_CPPFLAGS = -Isrc -Isrc/c-files

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


check_PROGRAMS = ctest ftest
ctest_SOURCES = tests/c-test.c
ctest_LDADD = $(top_builddir)/libcloudsc.la -lnetcdf
ftest_SOURCES = tests/c-test.c
ftest_LDADD = $(top_builddir)/libclouds.la -lnetcdff


libcloudsc_la_SOURCES = src/c-files/clouds_lib.c \
                        src/c-files/hu_stamnes.c \
                        src/c-files/ice_cloud_optics.c \
                        src/c-files/incomplete_beta.c \
                        src/c-files/netcdf_utils.c \
                        src/c-files/optics_utils.c \
                        src/c-files/stochastic_clouds.c




CLEANFILES = *.mod *__genmod.f90
