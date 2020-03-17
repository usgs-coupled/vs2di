#define VS2_VER_MAJOR      1
#define VS2_VER_MINOR      1
#define VS2_VER_PATCH      0  
#define VS2_VER_REVISION   0

#define RELEASE_DATE           "@RELEASE_DATE@"

#define APR_STRINGIFY(n) APR_STRINGIFY_HELPER(n)
#define APR_STRINGIFY_HELPER(n) #n

/** Version number */
#define VS2_VER_NUM            APR_STRINGIFY(VS2_VER_MAJOR) \
                           "." APR_STRINGIFY(VS2_VER_MINOR) \
                           "." APR_STRINGIFY(VS2_VER_PATCH) \
                           "." APR_STRINGIFY(VS2_VER_REVISION)



#define PRODUCT_NAME       "VS2POST" \
                       "-" APR_STRINGIFY(VS2_VER_MAJOR) \
                       "." APR_STRINGIFY(VS2_VER_MINOR)
