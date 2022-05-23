#ifndef TEST_C_CPP
#define TEST_C_CPP

#ifdef __cplusplus
  #define CX_EXTERN extern "C"
#else
  #define CX_EXTERN
#endif

CX_EXTERN int c_function();
CX_EXTERN int cpp_function();

#endif