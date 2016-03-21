#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT void JNICALL Java_mp2_mp2Math_trimesh(
        JNIEnv *env, jclass obj, 
        jint jquality, 
        jdoubleArray jinputPointList, 
        jdoubleArray jinputPointAttributeList, 
        jintArray jsegmentList,
        jobjectArray jreturnObjectList);

JNIEXPORT void JNICALL Java_mp2_mp2Math_changeDirectory(
        JNIEnv *env, jclass obj, 
        jstring directory);

#ifdef __cplusplus
}
#endif
