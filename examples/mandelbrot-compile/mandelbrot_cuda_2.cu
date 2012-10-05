#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>
extern "C" static __global__ void genPlane(double* cs_x, double* cs_y,
                                           int32_t cs_dimx, int32_t cs_dimy,
                                           double lowx, double lowy,
                                           double highx, double highy,
                                           int32_t viewx, int32_t viewy)
{
    for (int32_t i = blockIdx.x * blockDim.x + threadIdx.x; i < viewx;
         i += blockDim.x * gridDim.x) {
        for (int32_t j = blockIdx.y * blockDim.y + threadIdx.y; j < viewy;
             j += blockDim.y * gridDim.y) {
            int32_t idx;

            idx = j * cs_dimx + i;
            cs_x[idx] = lowx + (double) i * (highx - lowx) / (double) viewx;
            cs_y[idx] = lowy + (double) j * (highy - lowy) / (double) viewy;
        }
    }
}
extern "C" static __global__ void mkInit(double* cs_x, double* cs_y,
                                         int32_t cs_dimx, int32_t cs_dimy,
                                         double* zs_x, double* zs_y, int32_t* zs_i,
                                         int32_t zs_dimx, int32_t zs_dimy)
{
    for (int32_t i = blockIdx.x * blockDim.x + threadIdx.x; i <
         cs_dimx; i += blockDim.x * gridDim.x) {
        for (int32_t j = blockIdx.y * blockDim.y + threadIdx.y; j <
             cs_dimy; j += blockDim.y * gridDim.y) {
            int32_t idx;
            int32_t idx2;

            idx  = j * zs_dimx + i;
            zs_x[idx] = cs_x[idx];
            zs_y[idx] = cs_y[idx];
            zs_i[idx] = 0;
        }
    }
}
extern "C" static __global__ void stepN(double* cs_x, double* cs_y,
                                        int32_t cs_dimx, int32_t cs_dimy,
                                        double* zs_x, double* zs_y, int32_t* zs_i,
                                        int32_t zs_dimx, int32_t zs_dimy,
                                        int32_t depth)
{
    int32_t max_i;
    int32_t max_j;

    max_i = cs_dimx > zs_dimx ? zs_dimx : cs_dimx;
    max_j = cs_dimy > zs_dimy ? zs_dimy : cs_dimy;

    for (int32_t i = blockIdx.x * blockDim.x + threadIdx.x; i < max_i;
         i += blockDim.x * gridDim.x) {
        for (int32_t j = blockIdx.y * blockDim.y + threadIdx.y; j <
             max_j; j += blockDim.y * gridDim.y) {
            int32_t idx;
            double x_temp;
            double y_temp;
            int32_t i_temp;

            idx  = j * zs_dimx + i;
            x_temp = zs_x[idx];
            y_temp = zs_y[idx];
            i_temp = zs_i[idx];
            if (depth > 0) {
                for (int k = 0; k < depth; ++k) {
                    int32_t idx3;
                    double x_new;
                    double y_new;

                    idx3 = j * cs_dimx + i;
                    x_new = cs_x[idx3] + (x_temp * x_temp - y_temp *
                                                   y_temp);
                    y_new = cs_y[idx3] + x_temp * y_temp + y_temp *
                        x_temp;

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

#define DIM_X   128
#define DIM_Y   128
#define BLOCK_X 16
#define BLOCK_Y 8

cudaError_t mandelbrot_cuda_2(double lowx, double lowy, double highx, double highy,
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
        genPlane<<<blockDims, gridDims>>>(cs_x, cs_y, cs_dimx,
                                          cs_dimy, lowx, lowy, highx, highy,
                                          viewx, viewy);
    }
    {
        dim3 gridDims;
        dim3 blockDims;

        blockDims.x = DIM_X;
        gridDims.x = BLOCK_X;
        blockDims.y = DIM_Y;
        gridDims.y = BLOCK_Y;
        mkInit<<<blockDims, gridDims>>>(cs_x, cs_y, cs_dimx,
                                        cs_dimy, zs_x, zs_y,
                                        zs_i, zs_dimx, zs_dimy);
    }
    {
        dim3 gridDims;
        dim3 blockDims;

        blockDims.x = DIM_X;
        gridDims.x = BLOCK_X;
        blockDims.y = DIM_Y;
        gridDims.y = BLOCK_Y;
        stepN<<<blockDims, gridDims>>>(cs_x, cs_y, cs_dimx,
                                       cs_dimy, zs_x, zs_y,
                                       zs_i, zs_dimx, zs_dimy,
                                       depth);
    }

    return cudaGetLastError();
}
