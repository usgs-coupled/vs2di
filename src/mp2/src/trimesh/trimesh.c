#ifdef SINGLE
#define REAL float
#else /* not SINGLE */
#define REAL double
#endif /* not SINGLE */

#define TRIMESH_DLL

#include <malloc.h>
#include <stdio.h>
#include <jni.h>
#ifdef _WINDOWS
#include <direct.h>
#endif
#include "triangle.h"
#include "trimesh.h"

JNIEXPORT void JNICALL Java_mp2_mp2Math_trimesh(
        JNIEnv *env, jclass obj, 
        jint jquality, 
        jdoubleArray jinputPointList, 
        jdoubleArray jinputPointAttributeList, 
        jintArray jsegmentList,
        jobjectArray jreturnObjectList)
{

    int i;
    jsize numberOfInputPoints;
    jsize numberOfSegments;
    jsize returnObjectSize;
    double *inputPointList;
    double *inputPointAttributeList;
    int *segmentList;
    jdouble *jpt;
    jdouble *jattr;
    jint *jseg;
    jint *tri;

    jintArray joutputTriangleList;
    jdoubleArray joutputPointList;
    jdoubleArray joutputPointAttributeList;
    jintArray joutputSegmentList;

    struct triangulateio in, out;

    numberOfInputPoints = (*env)->GetArrayLength(env, jinputPointList)/2;
    numberOfSegments = (*env)->GetArrayLength(env, jsegmentList)/2;
    jpt = (*env)->GetDoubleArrayElements(env, jinputPointList, 0);
    jattr = (*env)->GetDoubleArrayElements(env, jinputPointAttributeList, 0);
    jseg = (*env)->GetIntArrayElements(env, jsegmentList, 0);
    inputPointList = (double *) malloc(numberOfInputPoints * 2 * sizeof(double));
    inputPointAttributeList = (double *) malloc(numberOfInputPoints * sizeof(double));
    segmentList = (int *) malloc(numberOfSegments * 2* sizeof(int));

    for (i=0; i<2*numberOfInputPoints; i++)
    {
        inputPointList[i] = (double) jpt[i];
    }

    for (i=0; i<numberOfInputPoints; i++)
    {
        inputPointAttributeList[i] = (double) jattr[i];
    }

    for (i=0; i<2*numberOfSegments; i++)
    {
        segmentList[i] = (int) jseg[i];
    }

    in.numberofpoints = numberOfInputPoints;
    in.pointlist = inputPointList,
    in.numberofpointattributes = 1;
    in.pointattributelist = inputPointAttributeList;
    in.pointmarkerlist = (int *) NULL;
    in.trianglelist = (int *) NULL;
    in.triangleattributelist = (REAL *) NULL;
    in.trianglearealist = (REAL *) NULL;
    in.neighborlist = (int *) NULL;
    in.numberoftriangles = 0;
    in.numberofcorners = 0;
    in.numberoftriangleattributes = 0;
    in.numberofsegments = numberOfSegments;
    in.segmentlist = segmentList;
    in.segmentmarkerlist = (int *) NULL;
    in.numberofholes = 0;
    in.holelist = (REAL *) NULL;
    in.numberofregions = 0;
    in.regionlist = (REAL *) NULL;
    in.numberofedges = 0;
    in.edgelist = (int *) NULL;
    in.edgemarkerlist = (int *) NULL;
    in.normlist = (REAL *) NULL;
    
    out.pointlist = (REAL *) NULL;
    out.pointattributelist = (REAL *) NULL;
    out.pointmarkerlist = (int *) NULL;
    out.trianglelist = (int *) NULL;
    out.triangleattributelist = (REAL *) NULL;
    out.neighborlist = (int *) NULL;
    out.segmentlist = (int *) NULL;
    out.segmentmarkerlist = (int *) NULL;
    out.edgelist = (int *) NULL;
    out.edgemarkerlist = (int *) NULL;

    if (((int) jquality) == 1)
    {
        /* quality triangulation--add points so that no triangle has 
           an angle less than 20 degrees */
        triangulate("pqcOQz", &in, &out, (struct triangulateio *) NULL);
    }
    else
    {
        /* constrained triangulation, no criterion on 
           triangle shape */
        triangulate("pcOQz", &in, &out, (struct triangulateio *) NULL);
    }

    returnObjectSize = (*env)->GetArrayLength(env, jreturnObjectList);

    if (returnObjectSize > 0)
    {
        joutputTriangleList = (*env)->NewIntArray(env, (jsize) (out.numberoftriangles*3));
        tri = (*env)->GetIntArrayElements(env, joutputTriangleList, 0);
        for (i=0; i<out.numberoftriangles*3; i++)
        {
            tri[i] = (jint) out.trianglelist[i];
        }
		(*env)->ReleaseIntArrayElements(env, joutputTriangleList, tri, 0);
        (*env)->SetObjectArrayElement(env, jreturnObjectList, 
                            0, joutputTriangleList);
    }

    if (returnObjectSize > 1 && out.numberofpoints > 0)
    {
        joutputPointList = (*env)->NewDoubleArray(env, (jsize) (out.numberofpoints*2));
        jpt = (*env)->GetDoubleArrayElements(env, joutputPointList, 0);
        for (i=0; i<out.numberofpoints*2; i++)
        {
            jpt[i] = (jdouble) out.pointlist[i];
        }
		(*env)->ReleaseDoubleArrayElements(env, joutputPointList, jpt, 0);
        (*env)->SetObjectArrayElement(env, jreturnObjectList, 
                            1, joutputPointList);
    }

    if (returnObjectSize > 2 && out.numberofpoints > 0)
    {
        joutputPointAttributeList = (*env)->NewDoubleArray(env, (jsize) out.numberofpoints);
        jattr = (*env)->GetDoubleArrayElements(env, joutputPointAttributeList, 0);
        for (i=0; i<out.numberofpoints; i++) {
            jattr[i] = (jdouble) out.pointattributelist[i];
        }
		(*env)->ReleaseDoubleArrayElements(env, joutputPointAttributeList, jattr, 0);
        (*env)->SetObjectArrayElement(env, jreturnObjectList, 
                            2, joutputPointAttributeList);
    }

    if (returnObjectSize > 3 && out.numberofsegments > 0)
    {
        joutputSegmentList = (*env)->NewIntArray(env, (jsize) out.numberofsegments);
        jseg = (*env)->GetIntArrayElements(env, joutputSegmentList, 0);
        for (i=0; i<out.numberofsegments; i++) {
            jseg[i] = (jint) out.segmentlist[i];
        }
		(*env)->ReleaseIntArrayElements(env, joutputSegmentList, jseg, 0);
        (*env)->SetObjectArrayElement(env, jreturnObjectList, 
                            3, joutputSegmentList);
    }

    free(inputPointList);
    free(inputPointAttributeList);
    free(segmentList);
    if (out.pointlist != NULL)
    {
        free(out.pointlist);
    }
    if (out.pointattributelist != NULL)
    {
        free(out.pointattributelist);
    }
    if (out.trianglelist != NULL)
    {
        free(out.trianglelist);
    }
    if (out.triangleattributelist != NULL)
    {
        free(out.triangleattributelist);
    }

}

JNIEXPORT void JNICALL Java_mp2_mp2Math_changeDirectory(
        JNIEnv *env, jclass obj, 
        jstring directory)
{
    char *str = (char *) (*env)->GetStringUTFChars(env, directory, 0);
#ifdef _WINDOWS
    _chdir(str);
#else
    chdir(str);
#endif
    (*env)->ReleaseStringUTFChars(env, directory, str);
}
