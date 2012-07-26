
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

#if defined(CUDA)
#include "cuda.h"
#include "cuda_runtime_api.h"
#endif /* defined(CUDA) */

typedef double Float;

#define N 10000000
#define NSIZE (N*sizeof(Float))

#define NITER 10

#define RISKLESS   0.02f
#define VOLATILITY 0.30f

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

#if defined(CUDA)
#define checkCuda(err)  __checkCuda(err, __FILE__, __LINE__)

inline void __checkCuda(cudaError err, const char *file, const int line)
{
    if(err != cudaSuccess) {
        fprintf(stderr, "%s(%i): CUDA runtime API error %d: %s.\n",
                file, line, (int)err, cudaGetErrorString(err));
        exit(-1);
    }
}
#endif /* defined(CUDA) */

extern "C" int blackscholes_openmp(Float* prices, int32_t nprices_1,
                                   Float* strikes, int32_t nstrikes,
                                   Float* years, int32_t nyears,
                                   Float riskless,
                                   Float volatility,
                                   Float** result,
                                   int32_t* resultsize);

#if defined(CUDA)
cudaError_t blackscholes_cuda(Float* prices, int32_t nprices_1,
                              Float* strikes, int32_t nstrikes,
                              Float* years, int32_t nyears,
                              Float riskless,
                              Float volatility,
                              Float** result,
                              int32_t* resultsize);
#endif /* defined(CUDA) */

void blackscholes(Float* callResults,
                  Float* putResults,
                  Float* prices,
                  Float* strikes,
                  Float* years,
                  Float  riskless,
                  Float  volatility,
                  int    n);

Float randFloat(Float low, Float high)
{
    Float t = (Float)rand() / (Float)RAND_MAX;
    return t*high + (1.0-t)*low;
}

int main(int argc, char** argv)
{
    Float* pricesH    = (Float*) checkMalloc(malloc(NSIZE));
    Float* strikesH   = (Float*) checkMalloc(malloc(NSIZE));
    Float* yearsH     = (Float*) checkMalloc(malloc(NSIZE));
    Float* callGolden = (Float*) checkMalloc(malloc(NSIZE));
    Float* putGolden  = (Float*) checkMalloc(malloc(NSIZE));

    srand(time(NULL));

    for (int i = 0; i < N; i++) {
        pricesH[i]  = randFloat(5.0f, 30.0f);
        strikesH[i] = randFloat(1.0f, 100.0f);
        yearsH[i]   = randFloat(0.25f, 10.0f);
    }

    blackscholes(callGolden, putGolden, pricesH, strikesH, yearsH, RISKLESS, VOLATILITY, N);

#if defined(CUDA)
    Float* pricesD;
    Float* strikesD;
    Float* yearsD;

    checkCuda(cudaMalloc((void**)&pricesD, NSIZE));
    checkCuda(cudaMalloc((void**)&strikesD, NSIZE));
    checkCuda(cudaMalloc((void**)&yearsD, NSIZE));

    checkCuda(cudaMemcpy(pricesD,  pricesH, NSIZE, cudaMemcpyHostToDevice));
    checkCuda(cudaMemcpy(strikesD, strikesH, NSIZE, cudaMemcpyHostToDevice));
    checkCuda(cudaMemcpy(yearsD,   yearsH, NSIZE, cudaMemcpyHostToDevice));

    Float*   callsH = (Float*) checkMalloc(malloc(NSIZE));
    Float*   callsD = NULL;
    int32_t  dummy;

    cudaEvent_t start;
    cudaEvent_t end;
    float       elapsed;
    float       sum_elapsed = 0.0;

    checkCuda(cudaEventCreate(&start));
    checkCuda(cudaEventCreate(&end));

    for (int i = 0; i < NITER; ++i) {
        checkCuda(cudaEventRecord(start));
        checkCuda(blackscholes_cuda(pricesD, N, strikesD, N, yearsD, N, RISKLESS, VOLATILITY, &callsD, &dummy));
        checkCuda(cudaGetLastError());
        checkCuda(cudaEventRecord(end));
        checkCuda(cudaDeviceSynchronize());

        checkCuda(cudaEventElapsedTime(&elapsed, start, end));
        sum_elapsed += elapsed;

        if (i < NITER-1)
            checkCuda(cudaFree(callsD));
    }

    checkCuda(cudaEventDestroy(start));
    checkCuda(cudaEventDestroy(end));

    checkCuda(cudaMemcpy(callsH, callsD, NSIZE, cudaMemcpyDeviceToHost));
#endif /* defined(CUDA) */

#if defined(OPENMP)
    Float* callsH;
    int32_t dummy;

    struct timespec start;
    struct timespec end;
    double          sum_elapsed = 0.0;

    for (int i = 0; i < NITER; ++i) {
        checkUnix(clock_gettime(CLOCK_MONOTONIC, &start));
        blackscholes_openmp(pricesH, N, strikesH, N, yearsH, N, RISKLESS, VOLATILITY, &callsH, &dummy);
        checkUnix(clock_gettime(CLOCK_MONOTONIC, &end));

        sum_elapsed += (end.tv_sec - start.tv_sec)*1e3 + (end.tv_nsec - start.tv_nsec)*1e-6;

        if (i < NITER-1)
            free(callsH);
    }
#endif /* defined(OPENMP) */

    printf("Average elapsed time: %fms\n", sum_elapsed/NITER);

    double sum_delta = 0;
    double sum_ref   = 0;
    double max_delta = 0;

    for(int i = 0; i < N; i++) {
        double delta = fabs(callGolden[i] - callsH[i]);

        //printf("golden = %E\n", callGolden[i]);
        //printf("gpu = %E\n", callsH[i]);

        if(delta > max_delta)
            max_delta = delta;

        sum_delta += delta;
        sum_ref   += fabs(callGolden[i]);
    }

    printf("L1 norm: %E\n", sum_delta / sum_ref);
    printf("Max absolute error: %E\n\n", max_delta);

#if defined(CUDA)
    checkCuda(cudaFree(pricesD));
    checkCuda(cudaFree(strikesD));
    checkCuda(cudaFree(yearsD));
    checkCuda(cudaFree(callsD));
#endif /* defined(CUDA) */

    free(pricesH);
    free(strikesH);
    free(yearsH);
    free(callsH);
    free(callGolden);
    free(putGolden);

    return 0;
}

static double CND(double d){
    const double       A1 = 0.31938153;
    const double       A2 = -0.356563782;
    const double       A3 = 1.781477937;
    const double       A4 = -1.821255978;
    const double       A5 = 1.330274429;
    const double RSQRT2PI = 0.39894228040143267793994605993438;

    double
        K = 1.0 / (1.0 + 0.2316419 * fabs(d));

    double
        cnd = RSQRT2PI * exp(- 0.5 * d * d) *
        (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))));

    if(d > 0)
        cnd = 1.0 - cnd;

    return cnd;
}

static void blackscholes1(Float& callResult,
                          Float& putResult,
                          Float Sf,
                          Float Xf,
                          Float Tf,
                          Float Rf,
                          Float Vf)
{
    double S = Sf, X = Xf, T = Tf, R = Rf, V = Vf;

    double sqrtT = sqrt(T);
    double    d1 = (log(S / X) + (R + 0.5 * V * V) * T) / (V * sqrtT);
    double    d2 = d1 - V * sqrtT;
    double CNDD1 = CND(d1);
    double CNDD2 = CND(d2);

    //Calculate Call and Put simultaneously
    double expRT = exp(- R * T);
    callResult   = (Float)(S * CNDD1 - X * expRT * CNDD2);
    putResult    = (Float)(X * expRT * (1.0 - CNDD2) - S * (1.0 - CNDD1));
}

void blackscholes(Float* callResults,
                  Float* putResults,
                  Float* prices,
                  Float* strikes,
                  Float* years,
                  Float  riskless,
                  Float  volatility,
                  int    n)
{
    for(int i = 0; i < n; i++)
        blackscholes1(callResults[i],
                      putResults[i],
                      prices[i],
                      strikes[i],
                      years[i],
                      riskless,
                      volatility);
}
