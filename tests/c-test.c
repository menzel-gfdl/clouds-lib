#include <math.h>
#include <stdio.h>
#include <string.h>

#include "clouds_lib.h"
#include "netcdf.h"


/*Print out usage message.*/
void usage(char const * name)
{
    fprintf(stderr, "Usage: %s beta_path ice_path liquid_path\n", name);
}

/*Print out help message.*/
void help(char const * name)
{
    usage(name);
    fprintf(stderr, "\nArguments:\n");
    fprintf(stderr, "beta_path:\t\tPath to beta-distribution input file.\n");
    fprintf(stderr, "ice_path:\t\tPath to ice cloud parameterization input file.\n");
    fprintf(stderr, "liquid_path:\t\tPath to liquid cloud parameterization input file.\n");
    fprintf(stderr, "output_path:\t\tPath to output file.\n");
}


int main(int argc, char **argv)
{
    char *pos_args[4] = {NULL, NULL, NULL, NULL};
    int num_args = 0;
    int i;
    for (i=1; i<argc; ++i)
    {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
        {
            help(argv[0]);
            return 0;
        }
        else
        {
            pos_args[num_args] = argv[i];
            num_args++;
        }
    }
    if (num_args != 4)
    {
        usage(argv[0]);
        return 1;
    }
    char const * beta_path = pos_args[0];
    char const * ice_path = pos_args[1];
    char const * liquid_path = pos_args[2];
    char const * output_path = pos_args[3];

    /*Initialize library.*/
    initialize_clouds_lib(beta_path, ice_path, liquid_path, NULL);

    /*CIRC case 6 input data.*/
    int const num_layers = 69;
    double pressure[num_layers] = {
        0.1, 0.17, 0.25, 0.33, 0.43, 0.55, 0.71, 0.91, 1.17, 1.51, 
        1.95, 2.54, 3.37, 4.46, 5.8, 7.6, 10.32, 13.49, 16.91, 21.21, 26.67, 
        32.82, 40.96, 52.26, 66.74, 85.17, 108.54, 132.2, 155.15, 182.12, 213.68, 
        249.84, 279.69, 301.57, 324.74, 349.22, 375.04, 402.3, 431.09, 461.44, 
        493.42, 527.22, 562.95, 600.68, 640.55, 682.63, 727.05, 768.97, 793.01, 
        802.94, 812.95, 823.07, 833.3, 843.65, 854.09, 864.69, 875.36, 886.18, 
        897.17, 906.19, 913.23, 920.4, 927.52, 934.7, 941.96, 949.33, 956.6, 
        967.75, 978.47};
    double temperature[num_layers] = {
        230.92, 241.94, 249.97, 255.58, 260.94, 265.78, 269.37, 
        270.88, 269.01, 264.17, 258.57, 253.03, 247.21, 241.55, 236.24, 231.31, 
        227.74, 225.69, 224.2, 222.78, 221.31, 215.49, 210.85, 210.37, 209.27, 
        210.57, 212.05, 212.39, 214.29, 212, 215.27, 221.41, 225, 228.44, 232.79, 
        237.17, 241.54, 245.11, 248.84, 253.12, 256.46, 258.86, 261.89, 264.28, 
        266.51, 269.39, 271.59, 273.96, 274.28, 274.13, 274.72, 275.3, 275.84, 
        276.36, 276.83, 276.99, 276.66, 276.52, 276.23, 275.14, 274.7, 274.79, 
        275.11, 275.45, 275.84, 276.21, 276.64, 277.57, 278.54};
    double cloud_fraction[num_layers] = {
        1., 1., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1.,
        1., 1., 1., 1., 1., 1., 1., 0., 0.};
    double liquid_content[num_layers] = {
        0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.95, 13.33, 23.36, 26.2, 20.96, 13.17,
        7.71, 8.26, 14.65, 19.98, 26.55, 18.66, 16.72, 14.17, 11.86, 10.1, 8.73, 7.16,
        0.9, 0., 0.};
    double ice_content[num_layers] = {
        2., 3., 4., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
        0., 0., 0., 0., 0., 0., 0., 0., 0.};
    double plev[num_layers + 1] = {
        0.07, 0.13, 0.22, 0.29, 0.37, 0.48, 0.62, 0.8, 1.02, 1.31, 
        1.69, 2.2, 2.87, 3.86, 5.05, 6.53, 8.66, 11.97, 15.02, 18.8, 23.61, 
        29.72, 36.03, 45.91, 58.62, 74.86, 95.48, 121.61, 142.79, 167.49, 196.77, 
        230.61, 269.1, 290.28, 312.8, 336.65, 361.78, 388.28, 416.28, 445.89, 
        476.99, 509.86, 544.63, 581.3, 620.02, 661.02, 704.27, 749.83, 788.1, 
        797.9, 807.9, 817.97, 828.2, 838.5, 848.87, 859.4, 869.92, 880.72, 
        891.64, 902.7, 909.71, 916.79, 924.01, 931.13, 938.24, 945.6, 953.03, 
        960.24, 975.22, 981.7};

/*
    double thickness[num_layers];
    for (i=0; i<num_layers; ++i)
    {
        thickness[i] = (abs(log(plev[i]) - log(plev[i + 1]))*temperature[i]*8.314462)/
                       ((29.9647/1000.)*9.81);
        liquid_content[i] /= thickness[i];
        ice_content[i] /= thickness[i];
    }
*/

    /*Calculate overlap.*/
    double altitude[num_layers];
    double const pressure_scale_height = 7.3; /*[km].*/
    for (i=0; i<num_layers; ++i)
    {
        altitude[i] = log(pressure[i])*pressure_scale_height;
    }
    double const scale_length = 2.;
    double alpha[num_layers - 1];
    calculate_overlap(num_layers, altitude, scale_length, alpha);

    /*Calculate cloud optics.*/
    int const num_bands = 3000.;
    double band_centers[num_bands];
    double band_limits[num_bands + 1];
    for (i=0; i<num_bands; ++i)
    {
        band_centers[i] = (double)(i + 1);
        band_limits[i] = ((double)i) + 0.5;
    }
    band_limits[num_bands] = band_limits[num_bands - 1] + 1.;
    double beta_liquid[num_layers*num_bands];
    double omega_liquid[num_layers*num_bands];
    double g_liquid[num_layers*num_bands];
    double beta_ice[num_layers*num_bands];
    double omega_ice[num_layers*num_bands];
    double g_ice[num_layers*num_bands];
    cloud_optics(num_bands, band_centers, band_limits, num_layers, cloud_fraction,
                 liquid_content, ice_content, alpha, 10., temperature, beta_liquid,
                 omega_liquid, g_liquid, beta_ice, omega_ice, g_ice);

    /*Write output to a file for now.*/
    int ncid;
    nc_create(output_path, NC_CLOBBER|NC_NETCDF4, &ncid);
    int pressure_dimid;
    nc_def_dim(ncid, "pressure", (size_t)num_layers, &pressure_dimid);
    int varid;
    char *units = "mb";
    nc_def_var(ncid, "pressure", NC_DOUBLE, 1, &pressure_dimid, &varid);
/*  nc_put_att_string(ncid, varid, "units", strlen(units), &units);*/
    nc_put_var(ncid, varid, pressure);
    int wavenumber_dimid;
    nc_def_dim(ncid, "wavenumber", (size_t)num_bands, &wavenumber_dimid);
    nc_def_var(ncid, "wavenumber", NC_DOUBLE, 1, &wavenumber_dimid, &varid);
    units = "cm-1";
/*  nc_put_att_string(ncid, varid, "units", strlen(units), &units);*/
    nc_put_var(ncid, varid, band_centers);
    int dimids[2] = {pressure_dimid, wavenumber_dimid};
    nc_def_var(ncid, "liquid_cloud_extinction_coefficient", NC_DOUBLE, 2, dimids, &varid);
    nc_put_var(ncid, varid, beta_liquid);
    nc_def_var(ncid, "liquid_cloud_single_scatter_albedo", NC_DOUBLE, 2, dimids, &varid);
    nc_put_var(ncid, varid, omega_liquid);
    nc_def_var(ncid, "liquid_cloud_asymmetry_factor", NC_DOUBLE, 2, dimids, &varid);
    nc_put_var(ncid, varid, g_liquid);
    nc_def_var(ncid, "ice_cloud_extinction_coefficient", NC_DOUBLE, 2, dimids, &varid);
    nc_put_var(ncid, varid, beta_ice);
    nc_def_var(ncid, "ice_cloud_single_scatter_albedo", NC_DOUBLE, 2, dimids, &varid);
    nc_put_var(ncid, varid, omega_ice);
    nc_def_var(ncid, "ice_cloud_asymmetry_factor", NC_DOUBLE, 2, dimids, &varid);
    nc_put_var(ncid, varid, g_ice);
    nc_close(ncid);

    /*Clean up.*/
    finalize_clouds_lib();
    return 0;
}
