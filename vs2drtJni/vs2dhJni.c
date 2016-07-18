#if defined(CMAKE_FC)
#include "FC.h"
#endif
#include "vs2_vs2drt.h"

#include <jni.h>
#include <stdlib.h>
#include <string.h>
#if defined(WIN32)
#include <windows.h>
#endif
/* See Java_vs2_vs2drt_getMoistureContent for notes on JNI programming */

// fortran prototypes
void CLOSEIO(void);
void DOHEAT(long *iflag);
void DOSOLUTE(long *iflag);
void DOTRANS(long *iflag);
void GETCOMP(int *i, char buffer[], size_t len);
void GETCOMPCOUNT(long *count);
void GETCONC(int *index, double *c, long *nn);
void GETDX(double *dx, long *nx);
void GETDZ(double *dz, long *nz);
void GETFLOWMBERR(double *err);
void GETHEATTRANSMBERR(double *err);
void GETKSAT(double *ksat, long *nn);
void GETMOIST(double *moist, long *nn);
void GETNX(long *nx);
void GETNZ(long *nz);
void GETPHEAD(double *phead, long *nn);
void GETSAT(double *sat, long *nn);
void GETSOLTRANSMBERR(double *err, int *index);
void GETSTEP(long *ktime);
void GETSTIME(double *stime);
void GETTEMP(double *c, long *nn);
void GETTEX(int *ksat, long *nn);
void GETVX(double *vx, long *nn);
void GETVZ(double *vz, long *nn);
void RELEASEMEMORY(void);
void SETUP(int *iold, int *ihydr, int *isorp, char *filen, size_t filen_len);
void STEP(long *jstop);

JNIEXPORT void JNICALL Java_vs2_vs2drt_start(JNIEnv *env, jclass obj, jint jold, jint jhydr, jint jsorp, jstring jfilen)
{
   int iold, ihydr, isorp;
   size_t len;
   char filen[500];
   char *str = (char *) (*env)->GetStringUTFChars(env, jfilen, 0);
   sprintf(filen, "%s", str);
   len = strlen(filen);
   (*env)->ReleaseStringUTFChars(env, jfilen, str);
   iold = jold;
   ihydr = jhydr;
   isorp = jsorp;
   SETUP(&iold, &ihydr, &isorp, filen, len);
}

JNIEXPORT jboolean JNICALL Java_vs2_vs2drt_getDoTransport(JNIEnv *env, jclass obj)
{
   long iflag;
   DOTRANS(&iflag);
   return (jboolean) iflag;
}

JNIEXPORT jboolean JNICALL Java_vs2_vs2drt_getDoEnergyTransport(JNIEnv *env, jclass obj)
{
   long iflag;
   DOHEAT(&iflag);
   return (jboolean) iflag;
}

JNIEXPORT jboolean JNICALL Java_vs2_vs2drt_getDoSoluteTransport(JNIEnv *env, jclass obj)
{
   long iflag;
   DOSOLUTE(&iflag);
   return (jboolean) iflag;
}

JNIEXPORT jint JNICALL Java_vs2_vs2drt_advanceOneStep(JNIEnv *env, jclass obj)
{
   long jstop;
   STEP(&jstop);
   return (jint) jstop;
}

JNIEXPORT jint JNICALL Java_vs2_vs2drt_getNumCellAlongX(JNIEnv *env, jclass obj)
{
   long nx;
   GETNX(&nx);
   return (jint) nx;
}

JNIEXPORT jint JNICALL Java_vs2_vs2drt_getNumCellAlongZ(JNIEnv *env, jclass obj)
{
   long nz;
   GETNZ(&nz);
   return (jint) nz;
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getCellSizesAlongX(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jdx;
   double *dx;
   long nx;
   int i;

   GETNX(&nx);
   dx = (double *) malloc(nx*sizeof(double));
   GETDX(dx, &nx);
   jdx = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nx; i++)
   {
      jdx[i] = (float) dx[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jdx, 0);
   free(dx);
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getCellSizesAlongZ(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jdz;
   double *dz;
   long nz;
   int i;

   GETNZ(&nz);
   dz = (double *) malloc(nz*sizeof(double));
   GETDZ(dz, &nz);
   jdz = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nz; i++)
   {
      jdz[i] = (float) dz[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jdz, 0);
   free(dz);
}

//JNIEXPORT void JNICALL Java_vs2_vs2drt_getTransport(JNIEnv *env, jclass obj, jfloatArray jvalue)
//{
//   jfloat *jc;
//   double *c;
//   long nx, nz, nn;
//   int i;
//
//   GETNX(&nx);
//   GETNZ(&nz);
//   nn = nx*nz;
//   c = (double *) malloc(nn*sizeof(double));
//   GETCONC(c, &nn);
//   jc = (*env)->GetFloatArrayElements(env, jvalue, NULL);
//   for (i=0; i<nn; i++)
//   {
//      jc[i] = (float) c[i];
//   }
//   (*env)->ReleaseFloatArrayElements(env, jvalue, jc, 0);
//   free(c);
//}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getTemperature(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jc;
   double *c;
   long nx, nz, nn;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   c = (double *) malloc(nn*sizeof(double));
   GETTEMP(c, &nn);
   jc = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jc[i] = (float) c[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jc, 0);
   free(c);
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getConcentration(JNIEnv *env, jclass obj, jint jindex, jfloatArray jvalue)
{
   jfloat *jc;
   double *c;
   long nx, nz, nn;
   int index;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   c = (double *) malloc(nn*sizeof(double));
   index = jindex + 1;  // convert from 0-based to 1-based
   GETCONC(&index, c, &nn);
   jc = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jc[i] = (float) c[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jc, 0);
   free(c);
}

JNIEXPORT jint JNICALL Java_vs2_vs2drt_getComponentCount(JNIEnv *env, jclass obj)
{
   long count;
   GETCOMPCOUNT(&count);
   return (jint) count;
}

JNIEXPORT jobjectArray JNICALL Java_vs2_vs2drt_getComponents(JNIEnv *env, jobject obj)
{
	int i;
	long count;
	jobjectArray arr;
	char buffer[50];

	GETCOMPCOUNT(&count);

	arr = (*env)->NewObjectArray(env, count, (*env)->FindClass(env, "java/lang/String"), (*env)->NewStringUTF(env, ""));
	for (i = 0; i < count; ++i)
	{
		GETCOMP(&i, buffer, sizeof(buffer));
		(*env)->SetObjectArrayElement(env, arr, i, (*env)->NewStringUTF(env, buffer));
	}
	return arr;
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getKSat(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jksat;
   double *ksat;
   long nx, nz, nn;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   ksat = (double *) malloc(nn*sizeof(double));
   GETKSAT(ksat, &nn);
   jksat = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jksat[i] = (float) ksat[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jksat, 0);
   free(ksat);
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getTexturalClassArray(JNIEnv *env, jclass obj, jintArray jvalue)
{
   jint *jtex;
   int *tex;
   long nx, nz, nn;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   tex = (int *) malloc(nn*sizeof(int));
   GETTEX(tex, &nn);
   jtex = (*env)->GetIntArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jtex[i] = tex[i];
   }
   (*env)->ReleaseIntArrayElements(env, jvalue, jtex, 0);
   free(tex);
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getMoistureContent(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jmoist;    /* Pointer for accessing elements of jvalue */
   double *moist;     /* Pointer to the double array containing moisture contents */
   long nx, nz, nn;
   int i;

   GETNX(&nx);     /* Get the number of cells in the x direction */
   GETNZ(&nz);     /* Get the number of cells in the z direction */
   nn = nx*nz;     /* Determine the total number of cells */
   moist = (double *) malloc(nn*sizeof(double));   /* allocate memory */

   /**** Get the moisture content from the model ****/   
   GETMOIST(moist, &nn);

   jmoist = (*env)->GetFloatArrayElements(env, jvalue, NULL);   /* Get a pointer to the elements in jvalue */

   for (i=0; i<nn; i++)
   {
      jmoist[i] = (float) moist[i];  /* Data type conversion */
   }

   (*env)->ReleaseFloatArrayElements(env, jvalue, jmoist, 0);
   free(moist);  /* Free memory */
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getSaturation(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jsat;
   double *sat;
   long nx, nz, nn;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   sat = (double *) malloc(nn*sizeof(double));
   GETSAT(sat, &nn);
   jsat = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jsat[i] = (float) sat[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jsat, 0);
   free(sat);
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getPressureHead(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jphead;
   double *phead;
   long nx, nz, nn;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   phead = (double *) malloc(nn*sizeof(double));
   GETPHEAD(phead, &nn);
   jphead = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jphead[i] = (float) phead[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jphead, 0);
   free(phead);
}

JNIEXPORT jint JNICALL Java_vs2_vs2drt_getTimeStep(JNIEnv *env, jclass obj)
{
   long ktime;
   GETSTEP(&ktime);
   return (jint) ktime;
}

JNIEXPORT jfloat JNICALL Java_vs2_vs2drt_getModelTime(JNIEnv *env, jclass obj)
{
   double stime;
   GETSTIME(&stime);
   return (jfloat) stime;
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_cleanup(JNIEnv *env, jclass obj)
{
   CLOSEIO();
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_releaseMemory(JNIEnv *env, jclass obj)
{
   RELEASEMEMORY();
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getFlowMassBalanceErrors(JNIEnv *env, jclass obj, jdoubleArray jvalue)
{
   jdouble *jerr;
   double *err;

   err = (double *) malloc(2*sizeof(double));
   GETFLOWMBERR(err);
   jerr = (*env)->GetDoubleArrayElements(env, jvalue, NULL);
   jerr[0] = err[0];
   jerr[1] = err[1];
   (*env)->ReleaseDoubleArrayElements(env, jvalue, jerr, 0);
   free(err);
}

//JNIEXPORT void JNICALL Java_vs2_vs2drt_getTransportMassBalanceErrors(JNIEnv *env, jclass obj, jdoubleArray jvalue)
//{
//   jdouble *jerr;
//   double *err;
//
//   err = (double *) malloc(2*sizeof(double));
//   ///gettransmberr_(err);
//   getheattransmberr_(err);
//   jerr = (*env)->GetDoubleArrayElements(env, jvalue, NULL);
//   jerr[0] = err[0];
//   jerr[1] = err[1];
//   (*env)->ReleaseDoubleArrayElements(env, jvalue, jerr, 0);
//   free(err);
//}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getHeatTransportMassBalanceErrors(JNIEnv *env, jclass obj, jdoubleArray jvalue)
{
   jdouble *jerr;
   double *err;

   err = (double *) malloc(2*sizeof(double));
   GETHEATTRANSMBERR(err);
   jerr = (*env)->GetDoubleArrayElements(env, jvalue, NULL);
   jerr[0] = err[0];
   jerr[1] = err[1];
   (*env)->ReleaseDoubleArrayElements(env, jvalue, jerr, 0);
   free(err);
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getSoluteTransportMassBalanceErrors(JNIEnv *env, jclass obj, jint jindex, jdoubleArray jvalue)
{
   jdouble *jerr;
   double *err;
   int index;

   err = (double *) malloc(2*sizeof(double));
   index = jindex + 1;  // convert from 0-based to 1-based
   GETSOLTRANSMBERR(err, &index);
   jerr = (*env)->GetDoubleArrayElements(env, jvalue, NULL);
   jerr[0] = err[0];
   jerr[1] = err[1];
   (*env)->ReleaseDoubleArrayElements(env, jvalue, jerr, 0);
   free(err);
}



JNIEXPORT void JNICALL Java_vs2_vs2drt_getVx(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jvx;
   double *vx;
   long nx, nz, nn;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   vx = (double *) malloc(nn*sizeof(double));
   GETVX(vx, &nn);
   jvx = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jvx[i] = (float) vx[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jvx, 0);
   free(vx);
}

JNIEXPORT void JNICALL Java_vs2_vs2drt_getVz(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jvz;
   double *vz;
   long nx, nz, nn;
   int i;

   GETNX(&nx);
   GETNZ(&nz);
   nn = nx*nz;
   vz = (double *) malloc(nn*sizeof(double));
   GETVZ(vz, &nn);
   jvz = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jvz[i] = (float) vz[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jvz, 0);
   free(vz);
}
