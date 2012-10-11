#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>

#define CS_X(i) (lowx + (double) (i) * (highx - lowx) / (double) viewx)
#define CS_Y(j) (lowy + (double) (j) * (highy - lowy) / (double) viewy)

extern "C" static __global__ void stepN(double lowx, double lowy, double highx, double highy,
                                        int32_t viewx, int32_t viewy,
                                        double* cs_x, double* cs_y,
                                        int32_t cs_dimx, int32_t cs_dimy,
                                        double* zs_x, double* zs_y, int32_t* zs_i,
                                        int32_t zs_dimx, int32_t zs_dimy,
                                        int32_t depth)
{
    int32_t max_i;
    int32_t max_j;
    int gid_x = blockIdx.x * blockDim.x + threadIdx.x;
    int gid_y = blockIdx.y * blockDim.y + threadIdx.y;
    int gwid_x = blockDim.x * gridDim.x;
    int gwid_y = blockDim.y * gridDim.y;

    max_i = cs_dimx > zs_dimx ? zs_dimx : cs_dimx;
    max_j = cs_dimy > zs_dimy ? zs_dimy : cs_dimy;

    for (int32_t j = gid_y; j < max_j; j += gwid_y) {
        for (int32_t i = gid_x; i < max_i; i += gwid_x) {
            int32_t idx;
            double x_temp;
            double y_temp;
            int32_t i_temp;
            double cs_x = CS_X(i);
            double cs_y = CS_Y(j);

            idx  = j * zs_dimx + i;
            x_temp = cs_x;
            y_temp = cs_y;
            i_temp = 0;
            if (depth > 0) {
                for (int k = 0; k < depth; ++k) {
                    double x_new;
                    double y_new;
                    double xx = x_temp * x_temp;
                    double xy = x_temp * y_temp;
                    double yy = y_temp * y_temp;

                    x_new = cs_x + xx - yy;
                    y_new = cs_y + xy + xy;

                    if (x_new * x_new + y_new * y_new > 4.0)
                        break;

                    x_temp = x_new;
                    y_temp = y_new;
                    i_temp = 1 + i_temp;
                }
            }
            zs_x[idx] = x_temp;
            zs_y[idx] = y_temp;
            zs_i[idx] = i_temp;
        }
    }
}

#define DIM_X   64
#define DIM_Y   64
#define BLOCK_X 16
#define BLOCK_Y 16

cudaError_t mandelbrot_cuda_3(double lowx, double lowy, double highx, double highy,
                              int32_t viewx, int32_t viewy, int32_t depth,
                              double* cs_x, double* cs_y,
                              int32_t cs_dimx, int32_t cs_dimy,
                              double* zs_x, double* zs_y,
                              int32_t* zs_i, int32_t zs_dimx,
                              int32_t zs_dimy)
{
    {
        dim3 gridDims;
        dim3 blockDims;

        blockDims.x = DIM_X;
        gridDims.x = BLOCK_X;
        blockDims.y = DIM_Y;
        gridDims.y = BLOCK_Y;
        stepN<<<blockDims, gridDims>>>(lowx, lowy, highx, highy,
                                       viewx, viewy,
                                       cs_x, cs_y,
                                       cs_dimx, cs_dimy,
                                       zs_x, zs_y, zs_i,
                                       zs_dimx, zs_dimy,
                                       depth);
    }

    return cudaGetLastError();
}
