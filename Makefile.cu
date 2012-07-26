CUDA_PREFIX=/usr/local/cuda/4.2
CUDA=/usr/local/cuda/4.2/cuda
CUDA_SDK=/usr/local/cuda/4.2/sdk

NVCC = $(CUDA)/bin/nvcc

CFLAGS += -I$(CUDA)/include
