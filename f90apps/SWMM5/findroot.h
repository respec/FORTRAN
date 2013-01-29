//-----------------------------------------------------------------------------
//   findroot.h
//
//   Header file for root finding method contained in findroot.c
//-----------------------------------------------------------------------------
int findroot_Newton(double x1, double x2, double* rts, double xacc,
                    void (*func) (double x, double* f, double* df) );
double findroot_Ridder(double x1, double x2, double xacc, double (*func)(double));
