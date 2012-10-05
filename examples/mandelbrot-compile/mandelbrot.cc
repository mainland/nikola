
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include "cuda.h"
#include "cuda_runtime_api.h"

typedef double Float;

#define NITER 10

#define checkUnix(err) __checkUnix(err, __FILE__, __LINE__)

inline void* __checkUnix(int res, const char *file, const int line)
{
    if (res != 0) {
        fprintf(stderr, "%s(%i): %s\n", file, line, strerror(res));
        exit(-1);
    }

    return 0;
}

#define checkMalloc(err) __checkMalloc(err, __FILE__, __LINE__)

inline void* __checkMalloc(void* ptr, const char *file, const int line)
{
    if (ptr == NULL) {
        fprintf(stderr, "%s(%i): malloc failed\n", file, line);
        exit(-1);
    }

    return ptr;
}

#define checkCuda(err)  __checkCuda(err, __FILE__, __LINE__)

inline void __checkCuda(cudaError err, const char *file, const int line)
{
    if(err != cudaSuccess) {
        fprintf(stderr, "%s(%i): CUDA runtime API error %d: %s.\n",
                file, line, (int)err, cudaGetErrorString(err));
        exit(-1);
    }
}

typedef cudaError_t (*mp)(double lowx, double lowy, double highx, double highy,
                          int32_t viewx, int32_t viewy,
                          int32_t depth,
                          double* cs_x, double* cs_y,
                          int32_t cs_dimx, int32_t cs_dimy,
                          double* zs_x, double* zs_y, int32_t* zs_i,
                          int32_t zs_dimx, int32_t zs_dimy);

cudaError_t mandelbrot_cuda(double lowx, double lowy, double highx, double highy,
                            int32_t viewx, int32_t viewy,
                            int32_t depth,
                            double* cs_x, double* cs_y,
                            int32_t cs_dimx, int32_t cs_dimy,
                            double* zs_x, double* zs_y, int32_t* zs_i,
                            int32_t zs_dimx, int32_t zs_dimy);

cudaError_t mandelbrot_cuda_2(double lowx, double lowy, double highx, double highy,
                              int32_t viewx, int32_t viewy,
                              int32_t depth,
                              double* cs_x, double* cs_y,
                              int32_t cs_dimx, int32_t cs_dimy,
                              double* zs_x, double* zs_y, int32_t* zs_i,
                              int32_t zs_dimx, int32_t zs_dimy);

void blackscholes(Float* callResults,
                  Float* putResults,
                  Float* prices,
                  Float* strikes,
                  Float* years,
                  Float  riskless,
                  Float  volatility,
                  int    n);

const int32_t SIZE_X = 512;
const int32_t SIZE_Y = 512;
const int32_t SIZE   = SIZE_X*SIZE_Y;

const double LEFT  = -0.25;
const double BOT   = -1.0;
const double RIGHT =  0.0;
const double TOP   = -0.75;

const int32_t DEPTH = 256;

void measure_mp(mp f);

int main(int argc, char** argv)
{
    measure_mp(mandelbrot_cuda);
    measure_mp(mandelbrot_cuda_2);

    return 0;
}

void measure_mp(mp f)
{
    double*  cs_x;
    double*  cs_y;
    double*  zs_x;
    double*  zs_y;
    int32_t* zs_i;

    checkCuda(cudaMalloc((void**)&cs_x, SIZE*sizeof(double)));
    checkCuda(cudaMalloc((void**)&cs_y, SIZE*sizeof(double)));
    checkCuda(cudaMalloc((void**)&zs_x, SIZE*sizeof(double)));
    checkCuda(cudaMalloc((void**)&zs_y, SIZE*sizeof(double)));
    checkCuda(cudaMalloc((void**)&zs_i, SIZE*sizeof(int32_t)));

    cudaEvent_t start;
    cudaEvent_t end;
    float       elapsed;
    float       sum_elapsed = 0.0;

    checkCuda(cudaEventCreate(&start));
    checkCuda(cudaEventCreate(&end));

    for (int i = 0; i < NITER; ++i) {
        checkCuda(cudaEventRecord(start));
        checkCuda(f(LEFT, BOT, RIGHT, TOP, SIZE_X, SIZE_Y, DEPTH,
                    cs_x, cs_y, SIZE_X, SIZE_Y,
                    zs_x, zs_y, zs_i, SIZE_X, SIZE_Y));
        checkCuda(cudaGetLastError());
        checkCuda(cudaEventRecord(end));
        checkCuda(cudaDeviceSynchronize());

        checkCuda(cudaEventElapsedTime(&elapsed, start, end));
        sum_elapsed += elapsed;
    }

    checkCuda(cudaEventDestroy(start));
    checkCuda(cudaEventDestroy(end));

    printf("Average elapsed time: %fms\n", sum_elapsed/NITER);

    checkCuda(cudaFree(cs_x));
    checkCuda(cudaFree(cs_y));
    checkCuda(cudaFree(zs_x));
    checkCuda(cudaFree(zs_y));
    checkCuda(cudaFree(zs_i));
}

