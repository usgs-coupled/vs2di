#include <jni.h>
#include <stdlib.h>
#include "vs2dh.h"

/* See Java_vs2_vs2dh_getMoistureContents for notes on JNI programming */

JNIEXPORT void JNICALL Java_vs2_vs2dh_start(JNIEnv *env, jclass obj, jint jold, jint jhydr, jint jsorp, jstring jfilen)
{
   long len, iold, ihydr, isorp;
   char filen[80];
   char *str = (char *) (*env)->GetStringUTFChars(env, jfilen, 0);
   sprintf(filen, "%s", str);
   len = strlen(filen);
   (*env)->ReleaseStringUTFChars(env, jfilen, str);
   iold = jold;
   ihydr = jhydr;
   isorp = jsorp;
   setup_ (&iold, &ihydr, &isorp, filen, len);
}

JNIEXPORT jboolean JNICALL Java_vs2_vs2dh_getDoTransport(JNIEnv *env, jclass obj)
{
   long iflag;
   dotrans_ (&iflag);
   return (jboolean) iflag;
}

JNIEXPORT jint JNICALL Java_vs2_vs2dh_advanceOneStep(JNIEnv *env, jclass obj)
{
   long jstop;
   step_ (&jstop);
   return (jint) jstop;
}

JNIEXPORT jint JNICALL Java_vs2_vs2dh_getNumCellAlongX(JNIEnv *env, jclass obj)
{
   long nx;
   getnx_ (&nx);
   return (jint) nx;
}

JNIEXPORT jint JNICALL Java_vs2_vs2dh_getNumCellAlongZ(JNIEnv *env, jclass obj)
{
   long nz;
   getnz_ (&nz);
   return (jint) nz;
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getCellSizesAlongX(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jdx;
   double *dx;
   long nx;
   int i;

   getnx_ (&nx);
   dx = (double *) malloc(nx*sizeof(double));
   getdx_ (dx, &nx);
   jdx = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nx; i++)
   {
      jdx[i] = (float) dx[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jdx, 0);
   free(dx);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getCellSizesAlongZ(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jdz;
   double *dz;
   long nz;
   int i;

   getnz_ (&nz);
   dz = (double *) malloc(nz*sizeof(double));
   getdz_ (dz, &nz);
   jdz = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nz; i++)
   {
      jdz[i] = (float) dz[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jdz, 0);
   free(dz);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getTransport(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jc;
   double *c;
   long nx, nz, nn;
   int i;

   getnx_ (&nx);
   getnz_ (&nz);
   nn = nx*nz;
   c = (double *) malloc(nn*sizeof(double));
   getconc_ (c, &nn);
   jc = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jc[i] = (float) c[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jc, 0);
   free(c);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getKSat(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jksat;
   double *ksat;
   long nx, nz, nn;
   int i;

   getnx_ (&nx);
   getnz_ (&nz);
   nn = nx*nz;
   ksat = (double *) malloc(nn*sizeof(double));
   getksat_ (ksat, &nn);
   jksat = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jksat[i] = (float) ksat[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jksat, 0);
   free(ksat);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getTexturalClassArray(JNIEnv *env, jclass obj, jintArray jvalue)
{
   jint *jtex;
   int *tex;
   long nx, nz, nn;
   int i;

   getnx_ (&nx);
   getnz_ (&nz);
   nn = nx*nz;
   tex = (int *) malloc(nn*sizeof(int));
   gettex_ (tex, &nn);
   jtex = (*env)->GetIntArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jtex[i] = tex[i];
   }
   (*env)->ReleaseIntArrayElements(env, jvalue, jtex, 0);
   free(tex);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getMoistureContent(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jmoist;    /* Pointer for accessing elements of jvalue */
   double *moist;     /* Pointer to the double array containing moisture contents */
   long nx, nz, nn;
   int i;

   getnx_ (&nx);     /* Get the number of cells in the x direction */
   getnz_ (&nz);     /* Get the number of cells in the z direction */
   nn = nx*nz;     /* Determine the total number of cells */
   moist = (double *) malloc(nn*sizeof(double));   /* allocate memory */

   /**** Get the moisture content from the model ****/   
   getmoist_ (moist, &nn);

   jmoist = (*env)->GetFloatArrayElements(env, jvalue, NULL);   /* Get a pointer to the elements in jvalue */

   for (i=0; i<nn; i++)
   {
      jmoist[i] = (float) moist[i];  /* Data type conversion */
   }

   (*env)->ReleaseFloatArrayElements(env, jvalue, jmoist, 0);
   free(moist);  /* Free memory */
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getSaturation(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jsat;
   double *sat;
   long nx, nz, nn;
   int i;

   getnx_ (&nx);
   getnz_ (&nz);
   nn = nx*nz;
   sat = (double *) malloc(nn*sizeof(double));
   getsat_ (sat, &nn);
   jsat = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jsat[i] = (float) sat[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jsat, 0);
   free(sat);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getPressureHead(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jphead;
   double *phead;
   long nx, nz, nn;
   int i;

   getnx_ (&nx);
   getnz_ (&nz);
   nn = nx*nz;
   phead = (double *) malloc(nn*sizeof(double));
   getphead_ (phead, &nn);
   jphead = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jphead[i] = (float) phead[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jphead, 0);
   free(phead);
}

JNIEXPORT jint JNICALL Java_vs2_vs2dh_getTimeStep(JNIEnv *env, jclass obj)
{
   long ktime;
   getstep_ (&ktime);
   return (jint) ktime;
}

JNIEXPORT jfloat JNICALL Java_vs2_vs2dh_getModelTime(JNIEnv *env, jclass obj)
{
   double stime;
   getstime_ (&stime);
   return (jfloat) stime;
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_cleanup(JNIEnv *env, jclass obj)
{
   closeio_ ();
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_releaseMemory(JNIEnv *env, jclass obj)
{
   releasememory_ ();
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getFlowMassBalanceErrors(JNIEnv *env, jclass obj, jdoubleArray jvalue)
{
   jdouble *jerr;
   double *err;

   err = (double *) malloc(2*sizeof(double));
   getflowmberr_(err);
   jerr = (*env)->GetDoubleArrayElements(env, jvalue, NULL);
   jerr[0] = err[0];
   jerr[1] = err[1];
   (*env)->ReleaseDoubleArrayElements(env, jvalue, jerr, 0);
   free(err);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getTransportMassBalanceErrors(JNIEnv *env, jclass obj, jdoubleArray jvalue)
{
   jdouble *jerr;
   double *err;

   err = (double *) malloc(2*sizeof(double));
   gettransmberr_(err);
   jerr = (*env)->GetDoubleArrayElements(env, jvalue, NULL);
   jerr[0] = err[0];
   jerr[1] = err[1];
   (*env)->ReleaseDoubleArrayElements(env, jvalue, jerr, 0);
   free(err);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getVx(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jvx;
   double *vx;
   long nx, nz, nn;
   int i;

   getnx_ (&nx);
   getnz_ (&nz);
   nn = nx*nz;
   vx = (double *) malloc(nn*sizeof(double));
   getvx_ (vx, &nn);
   jvx = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jvx[i] = (float) vx[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jvx, 0);
   free(vx);
}

JNIEXPORT void JNICALL Java_vs2_vs2dh_getVz(JNIEnv *env, jclass obj, jfloatArray jvalue)
{
   jfloat *jvz;
   double *vz;
   long nx, nz, nn;
   int i;

   getnx_ (&nx);
   getnz_ (&nz);
   nn = nx*nz;
   vz = (double *) malloc(nn*sizeof(double));
   getvz_ (vz, &nn);
   jvz = (*env)->GetFloatArrayElements(env, jvalue, NULL);
   for (i=0; i<nn; i++)
   {
      jvz[i] = (float) vz[i];
   }
   (*env)->ReleaseFloatArrayElements(env, jvalue, jvz, 0);
   free(vz);
}
