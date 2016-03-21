#ifndef __Vs2dt_h
#define __Vs2dt_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL setup_(long *iold, long *ihydr, long *isorp, char *filen, long len);
extern void STDCALL dotrans_(long *iflag);
extern void STDCALL step_(long *jstp);
extern void STDCALL getnx_(long *nx);
extern void STDCALL getnz_(long *nz);
extern void STDCALL getdx_(double *dx, long *nx);
extern void STDCALL getdz_(double *dz, long *nz);
extern void STDCALL getconc_(double *c, long *nn);
extern void STDCALL getksat_(double *ksat, long *nn);
extern void STDCALL gettex_(int *tex, long *nn);
extern void STDCALL getmoist_(double *moisture, long *nn);
extern void STDCALL getsat_(double *sat, long *nn);
extern void STDCALL getphead_(double *phead, long *nn);
extern void STDCALL getstime_(double *stime);
extern void STDCALL getstep_(long *ktime);
extern void STDCALL getflowmberr_(double *err);
extern void STDCALL gettransmberr_(double *err);
extern void STDCALL getvx_(double *velx, long *nn);
extern void STDCALL getvz_(double *velz, long *nn);
extern void STDCALL closeio_();
extern void STDCALL releasememory_();

#ifdef __cplusplus
}
#endif
#endif
